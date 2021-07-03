package lila.forum

import actorApi._
import scala.concurrent.duration._

import lila.common.Bus
import lila.common.paginator._
import lila.common.String.noShouting
import lila.db.dsl._
import lila.db.paginator._
import lila.hub.actorApi.timeline.{ ForumPost, Propagate }
import lila.memo.CacheApi
import lila.security.{ Granter => MasterGranter }
import lila.user.{ Holder, User }

final private[forum] class TopicApi(
    env: Env,
    indexer: lila.hub.actors.ForumSearch,
    maxPerPage: lila.common.config.MaxPerPage,
    spam: lila.security.Spam,
    promotion: lila.security.PromotionApi,
    timeline: lila.hub.actors.Timeline,
    shutup: lila.hub.actors.Shutup,
    detectLanguage: DetectLanguage,
    cacheApi: CacheApi
)(implicit ec: scala.concurrent.ExecutionContext) {

  import BSONHandlers._

  def show(
      categSlug: String,
      slug: String,
      page: Int,
      forUser: Option[User]
  ): Fu[Option[(Categ, Topic, Paginator[Post])]] =
    for {
      data <- env.categRepo bySlug categSlug flatMap {
        _ ?? { categ =>
          env.topicRepo.forUser(forUser).byTree(categSlug, slug) dmap {
            _ map (categ -> _)
          }
        }
      }
      res <- data ?? { case (categ, topic) =>
        lila.mon.forum.topic.view.increment()
        env.postApi.paginator(topic, page, forUser) map { (categ, topic, _).some }
      }
    } yield res

  object findDuplicate {
    private val cache = cacheApi.notLoadingSync[(User.ID, String), Topic.ID](64, "forum.topic.duplicate") {
      _.expireAfterWrite(1 hour).build()
    }
    def apply(topic: Topic): Fu[Option[Topic]] = {
      val key = (~topic.userId, topic.name)
      cache.getIfPresent(key) ?? env.topicRepo.coll.byId[Topic] orElse {
        cache.put(key, topic.id)
        fuccess(none)
      }
    }
  }

  def makeTopic(
      categ: Categ,
      data: ForumForm.TopicData,
      me: User
  ): Fu[Topic] =
    env.topicRepo.nextSlug(categ, data.name) zip detectLanguage(data.post.text) flatMap { case (slug, lang) =>
      val topic = Topic.make(
        categId = categ.slug,
        slug = slug,
        name = noShouting(data.name),
        userId = me.id,
        troll = me.marks.troll,
        hidden = categ.quiet || data.looksLikeVenting
      )
      val post = Post.make(
        topicId = topic.id,
        author = none,
        userId = me.id,
        troll = me.marks.troll,
        hidden = topic.hidden,
        text = spam.replace(data.post.text),
        lang = lang map (_.language),
        number = 1,
        categId = categ.id,
        modIcon = (~data.post.modIcon && MasterGranter(_.PublicMod)(me)).option(true)
      )
      findDuplicate(topic) flatMap {
        case Some(dup) => fuccess(dup)
        case None =>
          env.postRepo.coll.insert.one(post) >>
            env.topicRepo.coll.insert.one(topic withPost post) >>
            env.categRepo.coll.update.one($id(categ.id), categ.withPost(topic, post)) >>- {
              !categ.quiet ?? (indexer ! InsertPost(post))
              !categ.quiet ?? env.recent.invalidate()
              promotion.save(me, post.text)
              shutup ! {
                val text = s"${topic.name} ${post.text}"
                if (post.isTeam) lila.hub.actorApi.shutup.RecordTeamForumMessage(me.id, text)
                else lila.hub.actorApi.shutup.RecordPublicForumMessage(me.id, text)
              }
              if (!post.troll && !categ.quiet)
                timeline ! Propagate(ForumPost(me.id, topic.id.some, topic.name, post.id))
                  .toFollowersOf(me.id)
              lila.mon.forum.post.create.increment()
              env.mentionNotifier.notifyMentionedUsers(post, topic)
              Bus.publish(actorApi.CreatePost(post), "forumPost")
            } inject topic
      }
    }

  def makeBlogDiscuss(categ: Categ, slug: String, name: String, url: String): Funit = {
    val topic = Topic.make(
      categId = categ.slug,
      slug = slug,
      name = name,
      troll = false,
      userId = User.lichessId,
      hidden = false
    )
    val post = Post.make(
      topicId = topic.id,
      author = none,
      userId = User.lichessId,
      troll = false,
      hidden = false,
      text = s"Comments on $url",
      lang = none,
      number = 1,
      categId = categ.id,
      modIcon = true.some
    )
    env.postRepo.coll.insert.one(post) >>
      env.topicRepo.coll.insert.one(topic withPost post) >>
      env.categRepo.coll.update.one($id(categ.id), categ.withPost(topic, post)) >>-
      (indexer ! InsertPost(post)) >>-
      env.recent.invalidate() >>-
      Bus.publish(actorApi.CreatePost(post), "forumPost") void
  }

  def paginator(categ: Categ, page: Int, forUser: Option[User]): Fu[Paginator[TopicView]] = {
    val adapter = new Adapter[Topic](
      collection = env.topicRepo.coll,
      selector = env.topicRepo.forUser(forUser) byCategNotStickyQuery categ,
      projection = none,
      sort = $sort.updatedDesc
    ) mapFutureList { topics =>
      env.postRepo.coll.optionsByOrderedIds[Post, String](topics.map(_ lastPostId forUser))(_.id) map {
        posts =>
          topics zip posts map { case (topic, post) =>
            TopicView(categ, topic, post, env.postApi lastPageOf topic, forUser)
          }
      }
    }
    val cachedAdapter =
      if (categ.isTeam) adapter
      else new CachedAdapter(adapter, nbResults = fuccess(1000))
    Paginator(
      adapter = cachedAdapter,
      currentPage = page,
      maxPerPage = maxPerPage
    )
  }

  def getSticky(categ: Categ, forUser: Option[User]): Fu[List[TopicView]] =
    env.topicRepo.stickyByCateg(categ) flatMap { topics =>
      topics.map { topic =>
        env.postRepo.coll.byId[Post](topic lastPostId forUser) map { post =>
          TopicView(categ, topic, post, env.postApi lastPageOf topic, forUser)
        }
      }.sequenceFu
    }

  def denormalize(topic: Topic): Funit =
    for {
      nbPosts       <- env.postRepo countByTopic topic
      lastPost      <- env.postRepo lastByTopic topic
      nbPostsTroll  <- env.postRepo.unsafe countByTopic topic
      lastPostTroll <- env.postRepo.unsafe lastByTopic topic
      _ <-
        env.topicRepo.coll.update
          .one(
            $id(topic.id),
            topic.copy(
              nbPosts = nbPosts,
              lastPostId = lastPost ?? (_.id),
              updatedAt = lastPost.fold(topic.updatedAt)(_.createdAt),
              nbPostsTroll = nbPostsTroll,
              lastPostIdTroll = lastPostTroll ?? (_.id),
              updatedAtTroll = lastPostTroll.fold(topic.updatedAtTroll)(_.createdAt)
            )
          )
          .void
    } yield ()
}

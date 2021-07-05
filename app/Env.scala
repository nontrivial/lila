package lila.app

import akka.actor._
import com.softwaremill.macwire._
import play.api.libs.ws.StandaloneWSClient
import play.api.mvc.{ ControllerComponents, SessionCookieBaker }
import play.api.{ Configuration, Environment }
import scala.concurrent.duration._
import scala.concurrent.{ ExecutionContext, Future }

import lila.common.config._
import lila.common.{ Bus, Strings, UserIds }
import lila.memo.SettingStore.Strings._
import lila.memo.SettingStore.UserIds._
import lila.security.Granter
import lila.user.{ Holder, User }

final class Env(
    val config: Configuration,
    val imageRepo: lila.db.ImageRepo,
    val api: lila.api.Env,
    val user: lila.user.Env,
    val mailer: lila.mailer.Env,
    val security: lila.security.Env,
    val hub: lila.hub.Env,
    val socket: lila.socket.Env,
    val memo: lila.memo.Env,
    val msg: lila.msg.Env,
    val game: lila.game.Env,
    val bookmark: lila.bookmark.Env,
    val search: lila.search.Env,
    val gameSearch: lila.gameSearch.Env,
    val timeline: lila.timeline.Env,
    val notifyM: lila.notify.Env,
    val round: lila.round.Env,
    val lobby: lila.lobby.Env,
    val setup: lila.setup.Env,
    val importer: lila.importer.Env,
    val tournament: lila.tournament.Env,
    val relation: lila.relation.Env,
    val pref: lila.pref.Env,
    val chat: lila.chat.Env,
    val coordinate: lila.coordinate.Env,
    val history: lila.history.Env,
    val playban: lila.playban.Env,
    val shutup: lila.shutup.Env,
    val push: lila.push.Env,
    val perfStat: lila.perfStat.Env,
    val irc: lila.irc.Env,
    val challenge: lila.challenge.Env,
    val explorer: lila.explorer.Env,
    val plan: lila.plan.Env,
    val event: lila.event.Env,
    val pool: lila.pool.Env,
    val oAuth: lila.oauth.Env,
    val rating: lila.rating.Env,
    val lilaCookie: lila.common.LilaCookie,
    val net: NetConfig,
    val controllerComponents: ControllerComponents
)(implicit
    val system: ActorSystem,
    val executionContext: ExecutionContext,
    val mode: play.api.Mode
) {

  val explorerEndpoint  = config.get[String]("explorer.endpoint")
  val tablebaseEndpoint = config.get[String]("explorer.tablebase.endpoint")

  val appVersionDate    = config.getOptional[String]("app.version.date")
  val appVersionCommit  = config.getOptional[String]("app.version.commit")
  val appVersionMessage = config.getOptional[String]("app.version.message")

  lazy val apiTimelineSetting = memo.settingStore[Int](
    "apiTimelineEntries",
    default = 10,
    text = "API timeline entries to serve".some
  )
  lazy val noDelaySecretSetting = memo.settingStore[Strings](
    "noDelaySecrets",
    default = Strings(Nil),
    text =
      "Secret tokens that allows fetching ongoing games without the 3-moves delay. Separated by commas.".some
  )
  lazy val featuredTeamsSetting = memo.settingStore[Strings](
    "featuredTeams",
    default = Strings(Nil),
    text = "Team IDs that always get their tournaments visible on /tournament. Separated by commas.".some
  )
  lazy val prizeTournamentMakers = memo.settingStore[UserIds](
    "prizeTournamentMakers ",
    default = UserIds(Nil),
    text =
      "User IDs who can make prize tournaments (arena & swiss) without a warning. Separated by commas.".some
  )

  lazy val preloader     = wire[mashup.Preload]
  lazy val socialInfo    = wire[mashup.UserInfo.SocialApi]
  lazy val userNbGames   = wire[mashup.UserInfo.NbGamesApi]
  lazy val userInfo      = wire[mashup.UserInfo.UserInfoApi]
  lazy val gamePaginator = wire[mashup.GameFilterMenu.PaginatorBuilder]
  lazy val pageCache     = wire[http.PageCache]

  def scheduler = system.scheduler

  def closeAccount(u: User, by: Holder): Funit =
    for {
      playbanned <- playban.api.hasCurrentBan(u.id)
      selfClose = u.id == by.id
      modClose  = !selfClose && Granter(_.CloseAccount)(by.user)
      badApple  = u.lameOrTrollOrAlt || modClose
      _       <- user.repo.disable(u, keepEmail = badApple || playbanned)
      _       <- relation.api.unfollowAll(u.id)
      _       <- user.rankingApi.remove(u.id)
      _       <- challenge.api.removeByUserId(u.id)
      _       <- tournament.api.withdrawAll(u)
      _       <- plan.api.cancel(u).nevermind
      _       <- lobby.seekApi.removeByUser(u)
      _       <- security.store.closeAllSessionsOf(u.id)
      _       <- push.webSubscriptionApi.unsubscribeByUser(u)
    } yield Bus.publish(lila.hub.actorApi.security.CloseAccount(u.id), "accountClose")

  Bus.subscribeFun("garbageCollect") { case lila.hub.actorApi.security.GarbageCollect(userId) =>
    // GC can be aborted by reverting the initial SB mark
    user.repo.isTroll(userId) foreach { troll =>
      if (troll) scheduler.scheduleOnce(1.second) {
        lichessClose(userId).unit
      }
    }
  }
  Bus.subscribeFun("rageSitClose") { case lila.hub.actorApi.playban.RageSitClose(userId) =>
    lichessClose(userId).unit
  }
  private def lichessClose(userId: User.ID) =
    user.repo.lichessAnd(userId) flatMap {
      _ ?? { case (lichess, user) =>
        closeAccount(user, lichess)
      }
    }
}

final class EnvBoot(
    config: Configuration,
    environment: Environment,
    controllerComponents: ControllerComponents,
    cookieBacker: SessionCookieBaker,
    shutdown: CoordinatedShutdown
)(implicit
    ec: ExecutionContext,
    system: ActorSystem,
    ws: StandaloneWSClient
) {

  implicit def scheduler   = system.scheduler
  implicit def mode        = environment.mode
  def appPath              = AppPath(environment.rootPath)
  val netConfig            = config.get[NetConfig]("net")
  def netDomain            = netConfig.domain
  def baseUrl              = netConfig.baseUrl
  implicit def idGenerator = game.idGenerator

  lazy val mainDb: lila.db.Db = mongo.blockingDb("main", config.get[String]("mongodb.uri"))
  lazy val imageRepo          = new lila.db.ImageRepo(mainDb(CollName("image")))

  // wire all the lila modules
  lazy val memo: lila.memo.Env               = wire[lila.memo.Env]
  lazy val mongo: lila.db.Env                = wire[lila.db.Env]
  lazy val user: lila.user.Env               = wire[lila.user.Env]
  lazy val mailer: lila.mailer.Env           = wire[lila.mailer.Env]
  lazy val security: lila.security.Env       = wire[lila.security.Env]
  lazy val hub: lila.hub.Env                 = wire[lila.hub.Env]
  lazy val socket: lila.socket.Env           = wire[lila.socket.Env]
  lazy val msg: lila.msg.Env                 = wire[lila.msg.Env]
  lazy val game: lila.game.Env               = wire[lila.game.Env]
  lazy val bookmark: lila.bookmark.Env       = wire[lila.bookmark.Env]
  lazy val search: lila.search.Env           = wire[lila.search.Env]
  lazy val gameSearch: lila.gameSearch.Env   = wire[lila.gameSearch.Env]
  lazy val timeline: lila.timeline.Env       = wire[lila.timeline.Env]
  lazy val notifyM: lila.notify.Env          = wire[lila.notify.Env]
  lazy val round: lila.round.Env             = wire[lila.round.Env]
  lazy val lobby: lila.lobby.Env             = wire[lila.lobby.Env]
  lazy val setup: lila.setup.Env             = wire[lila.setup.Env]
  lazy val importer: lila.importer.Env       = wire[lila.importer.Env]
  lazy val tournament: lila.tournament.Env   = wire[lila.tournament.Env]
  lazy val relation: lila.relation.Env       = wire[lila.relation.Env]
  lazy val pref: lila.pref.Env               = wire[lila.pref.Env]
  lazy val chat: lila.chat.Env               = wire[lila.chat.Env]
  lazy val coordinate: lila.coordinate.Env   = wire[lila.coordinate.Env]
  lazy val history: lila.history.Env         = wire[lila.history.Env]
  lazy val playban: lila.playban.Env         = wire[lila.playban.Env]
  lazy val shutup: lila.shutup.Env           = wire[lila.shutup.Env]
  lazy val push: lila.push.Env               = wire[lila.push.Env]
  lazy val perfStat: lila.perfStat.Env       = wire[lila.perfStat.Env]
  lazy val irc: lila.irc.Env                 = wire[lila.irc.Env]
  lazy val challenge: lila.challenge.Env     = wire[lila.challenge.Env]
  lazy val explorer: lila.explorer.Env       = wire[lila.explorer.Env]
  lazy val plan: lila.plan.Env               = wire[lila.plan.Env]
  lazy val event: lila.event.Env             = wire[lila.event.Env]
  lazy val pool: lila.pool.Env               = wire[lila.pool.Env]
  lazy val oAuth: lila.oauth.Env             = wire[lila.oauth.Env]
  lazy val rating: lila.rating.Env           = wire[lila.rating.Env]
  lazy val api: lila.api.Env                 = wire[lila.api.Env]
  lazy val lilaCookie                        = wire[lila.common.LilaCookie]

  val env: lila.app.Env = {
    val c = lila.common.Chronometer.sync(wire[lila.app.Env])
    lila.log("boot").info(s"Loaded lila modules in ${c.showDuration}")
    c.result
  }

  templating.Environment setEnv env
}

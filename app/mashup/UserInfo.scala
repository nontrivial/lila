package lila.app
package mashup

import play.api.data.Form

import lila.api.Context
import lila.bookmark.BookmarkApi
import lila.game.Crosstable
import lila.relation.RelationApi
import lila.security.Granter
import lila.user.{ Trophies, TrophyApi, User }

case class UserInfo(
    user: User,
    ranks: lila.rating.UserRankMap,
    ratingChart: Option[String],
    nbs: UserInfo.NbGames,
    nbFollowers: Int,
    nbPosts: Int,
    trophies: Trophies,
    completionRate: Option[Double]
) {

  def crosstable = nbs.crosstable

  def completionRatePercent =
    completionRate.map { cr =>
      math.round(cr * 100)
    }

  def countTrophiesAndPerfCups = trophies.size + ranks.count(_._2 <= 100)
}

object UserInfo {

  sealed abstract class Angle(val key: String)
  object Angle {
    case class Games(searchForm: Option[Form[_]]) extends Angle("games")
    case object Other                             extends Angle("other")
  }

  case class Social(
      relation: Option[lila.relation.Relation],
      notes: List[lila.user.Note],
      followable: Boolean,
      blocked: Boolean
  )

  final class SocialApi(
      relationApi: RelationApi,
      noteApi: lila.user.NoteApi,
      prefApi: lila.pref.PrefApi
  ) {
    def apply(u: User, ctx: Context): Fu[Social] =
      ctx.userId.?? {
        relationApi.fetchRelation(_, u.id).mon(_.user segment "relation")
      } zip
        ctx.me.?? { me =>
          noteApi
            .get(u, me, Granter(_.ModNote)(me))
            .mon(_.user segment "notes")
        } zip
        ctx.isAuth.?? {
          prefApi.followable(u.id).mon(_.user segment "followable")
        } zip
        ctx.userId.?? { myId =>
          relationApi.fetchBlocks(u.id, myId).mon(_.user segment "blocks")
        } dmap { case (((relation, notes), followable), blocked) =>
          Social(relation, notes, followable, blocked)
        }
  }

  case class NbGames(
      crosstable: Option[Crosstable.WithMatchup],
      playing: Int,
      imported: Int,
      bookmark: Int
  ) {
    def withMe: Option[Int] = crosstable.map(_.crosstable.nbGames)
  }

  final class NbGamesApi(
      bookmarkApi: BookmarkApi,
      gameCached: lila.game.Cached,
      crosstableApi: lila.game.CrosstableApi
  ) {
    def apply(u: User, ctx: Context, withCrosstable: Boolean): Fu[NbGames] =
      (withCrosstable ?? ctx.me.filter(u.!=) ?? { me =>
        crosstableApi.withMatchup(me.id, u.id) dmap some
      }).mon(_.user segment "crosstable") zip
        gameCached.nbPlaying(u.id).mon(_.user segment "nbPlaying") zip
        gameCached.nbImportedBy(u.id).mon(_.user segment "nbImported") zip
        bookmarkApi.countByUser(u).mon(_.user segment "nbBookmarks") dmap {
          case (((crosstable, playing), imported), bookmark) =>
            NbGames(
              crosstable,
              playing = playing,
              imported = imported,
              bookmark = bookmark
            )
        }
  }

  final class UserInfoApi(
      relationApi: RelationApi,
      trophyApi: TrophyApi,
      ratingChartApi: lila.history.RatingChartApi,
      userCached: lila.user.Cached,
      playbanApi: lila.playban.PlaybanApi
  )(implicit ec: scala.concurrent.ExecutionContext) {
    def apply(user: User, nbs: NbGames, ctx: Context): Fu[UserInfo] =
      (ctx.noBlind ?? ratingChartApi(user)).mon(_.user segment "ratingChart") zip
        relationApi.countFollowers(user.id).mon(_.user segment "nbFollowers") zip
        !(user.is(User.lichessId) || user.isBot) ??
        trophyApi.findByUser(user).mon(_.user segment "trophy") zip
        playbanApi.completionRate(user.id).mon(_.user segment "completion") zip
        userCached.rankingsOf(user.id) map {
          // format: off
          case (((((((((((((ratingChart, nbFollowers))), trophies))))))), completionRate)), ranks) =>
          // format: on
          new UserInfo(
            user = user,
            ranks = ranks,
            nbs = nbs,
            ratingChart = ratingChart,
            nbFollowers = nbFollowers,
            trophies = trophies ::: trophyApi.roleBasedTrophies(
              user,
              Granter(_.PublicMod)(user),
              Granter(_.Developer)(user),
              Granter(_.Verified)(user)
            ),
            completionRate = completionRate
          )
        }
  }
}

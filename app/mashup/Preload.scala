package lila.app
package mashup

import lila.api.Context
import lila.event.Event
import lila.forum.MiniForumPost
import lila.game.{ Game, Pov }
import lila.playban.TempBan
import lila.timeline.Entry
import lila.tournament.{ Tournament, Winner }
import lila.user.LightUserApi
import lila.user.User
import play.api.libs.json._

final class Preload(
    gameRepo: lila.game.GameRepo,
    userCached: lila.user.Cached,
    tourWinners: lila.tournament.WinnersApi,
    timelineApi: lila.timeline.EntryApi,
    lobbyApi: lila.api.LobbyApi,
    lobbySocket: lila.lobby.LobbySocket,
    playbanApi: lila.playban.PlaybanApi,
    lightUserApi: LightUserApi,
    roundProxy: lila.round.GameProxyRepo,
)(implicit ec: scala.concurrent.ExecutionContext) {

  import Preload._

  def apply(
      posts: Fu[List[MiniForumPost]],
      tours: Fu[List[Tournament]],
      events: Fu[List[Event]]
  )(implicit ctx: Context): Fu[Homepage] =
    lobbyApi(ctx).mon(_.lobby segment "lobbyApi") zip
      posts.mon(_.lobby segment "posts") zip
      tours.mon(_.lobby segment "tours") zip
      events.mon(_.lobby segment "events") zip
      (ctx.userId ?? timelineApi.userEntries).mon(_.lobby segment "timeline") zip
      userCached.topWeek.mon(_.lobby segment "userTopWeek") zip
      tourWinners.all.dmap(_.top).mon(_.lobby segment "tourWinners") zip
      (ctx.userId ?? playbanApi.currentBan).mon(_.lobby segment "playban") zip
      (ctx.blind ?? ctx.me ?? roundProxy.urgentGames) flatMap {
        // format: off
        case (((((((((((((data, povs), posts), tours))), feat), entries), lead), tWinners))), playban), _) =>
        // format: on
        (ctx.me ?? currentGameMyTurn(povs, lightUserApi.sync))
          .mon(_.lobby segment "currentGame") zip
          lightUserApi
            .preloadMany {
              tWinners.map(_.userId) ::: posts.flatMap(_.userId) ::: entries.flatMap(_.userIds).toList
            }
            .mon(_.lobby segment "lightUsers") map { case (currentGame, _) =>
            Homepage(
              data,
              entries,
              tours,
              feat,
              lead,
              tWinners,
              playban,
              currentGame,
              lobbySocket.counters
            )
          }
      }

  def currentGameMyTurn(user: User): Fu[Option[CurrentGame]] =
    gameRepo.playingRealtimeNoAi(user).flatMap {
      _.map { roundProxy.pov(_, user) }.sequenceFu.dmap(_.flatten)
    } flatMap {
      currentGameMyTurn(_, lightUserApi.sync)(user)
    }

  private def currentGameMyTurn(povs: List[Pov], lightUser: lila.common.LightUser.GetterSync)(
      user: User
  ): Fu[Option[CurrentGame]] =
    ~povs.collectFirst {
      case p1 if p1.game.nonAi && p1.game.hasClock && p1.isMyTurn =>
        roundProxy.pov(p1.gameId, user) dmap (_ | p1) map { pov =>
          val opponent = lila.game.Namer.playerTextBlocking(pov.opponent)(lightUser)
          CurrentGame(pov = pov, opponent = opponent).some
        }
    }
}

object Preload {

  case class Homepage(
      data: JsObject,
      userTimeline: Vector[Entry],
      tours: List[Tournament],
      featured: Option[Game],
      leaderboard: List[User.LightPerf],
      tournamentWinners: List[Winner],
      playban: Option[TempBan],
      currentGame: Option[Preload.CurrentGame],
      counters: lila.lobby.LobbyCounters
  )

  case class CurrentGame(pov: Pov, opponent: String)
}

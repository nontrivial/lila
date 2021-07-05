package lila.api

import chess.format.FEN
import org.joda.time.DateTime
import play.api.libs.json._
import reactivemongo.api.bson._
import reactivemongo.api.ReadPreference

import lila.common.config._
import lila.common.Json.jodaWrites
import lila.common.paginator.{ Paginator, PaginatorJson }
import lila.db.dsl._
import lila.db.paginator.{ Adapter, CachedAdapter }
import lila.game.BSONHandlers._
import lila.game.Game.{ BSONFields => G }
import lila.game.JsonView._
import lila.game.{ CrosstableApi, Game, PerfPicker }
import lila.user.User

final private[api] class GameApi(
    net: NetConfig,
    apiToken: Secret,
    gameRepo: lila.game.GameRepo,
    gameCache: lila.game.Cached,
    crosstableApi: CrosstableApi
)(implicit ec: scala.concurrent.ExecutionContext) {

  import GameApi.WithFlags

  private def makeUrl(game: Game) = s"${net.baseUrl}/${game.id}/${game.naturalOrientation.name}"

  private def checkToken(withFlags: WithFlags) = withFlags applyToken apiToken.value

  private def gameToJson(
      g: Game,
      initialFen: Option[FEN],
      withFlags: WithFlags
  ) =
    Json
      .obj(
        "id"         -> g.id,
        "initialFen" -> initialFen,
        "rated"      -> g.rated,
        "variant"    -> g.variant.key,
        "speed"      -> g.speed.key,
        "perf"       -> PerfPicker.key(g),
        "createdAt"  -> g.createdAt,
        "lastMoveAt" -> g.movedAt,
        "turns"      -> g.turns,
        "color"      -> g.turnColor.name,
        "status"     -> g.status.name,
        "clock" -> g.clock.map { clock =>
          Json.obj(
            "initial"   -> clock.limitSeconds,
            "increment" -> clock.incrementSeconds,
            "totalTime" -> clock.estimateTotalSeconds
          )
        },
        "daysPerTurn" -> g.daysPerTurn,
        "players" -> JsObject(g.players map { p =>
          p.color.name -> Json
            .obj(
              "userId"     -> p.userId,
              "rating"     -> p.rating,
              "ratingDiff" -> p.ratingDiff
            )
            .add("name", p.name)
            .add("provisional" -> p.provisional)
            .add("moveCentis" -> withFlags.moveTimes ?? g.moveTimes(p.color).map(_.map(_.centis)))
            .add("blurs" -> withFlags.blurs.option(p.blurs.nb))
        }),
        "moves"    -> withFlags.moves.option(g.pgnMoves mkString " "),
        "opening"  -> withFlags.opening.??(g.opening),
        "fens" -> (withFlags.fens && g.finished) ?? {
          chess.Replay
            .boards(
              moveStrs = g.pgnMoves,
              initialFen = initialFen,
              variant = g.variant
            )
            .toOption map { boards =>
            JsArray(boards map chess.format.Forsyth.exportBoard map JsString.apply)
          }
        },
        "winner" -> g.winnerColor.map(_.name),
        "url"    -> makeUrl(g)
      )
      .noNull
}

object GameApi {

  case class WithFlags(
      moves: Boolean = false,
      fens: Boolean = false,
      opening: Boolean = false,
      moveTimes: Boolean = false,
      blurs: Boolean = false,
      token: Option[String] = none
  ) {

    def applyToken(validToken: String) =
      copy(
        blurs = token has validToken
      )
  }
}

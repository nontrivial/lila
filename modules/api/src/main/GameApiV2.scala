package lila.api

import akka.stream.scaladsl._
import chess.format.FEN
import chess.format.pgn.Tag
import org.joda.time.DateTime
import play.api.libs.json._
import scala.concurrent.duration._

import lila.common.config.MaxPerSecond
import lila.common.Json.jodaWrites
import lila.common.{ HTTPRequest, LightUser }
import lila.db.dsl._
import lila.game.JsonView._
import lila.game.PgnDump.WithFlags
import lila.game.{ Game, PerfPicker, Query }
import lila.user.User
import lila.round.GameProxyRepo

final class GameApiV2(
    gameRepo: lila.game.GameRepo,
    getLightUser: LightUser.Getter,
    realPlayerApi: RealPlayerApi,
    gameProxy: GameProxyRepo
)(implicit
    ec: scala.concurrent.ExecutionContext,
    system: akka.actor.ActorSystem
) {

  import GameApiV2._

  private val keepAliveInterval = 70.seconds // play's idleTimeout = 75s

  private val fileR = """[\s,]""".r

  def filename(game: Game, format: Format): Fu[String] =
    gameLightUsers(game) map { case (_, _) =>
      fileR.replaceAllIn(
        "lichess_pgn_%s_%s_vs_%s.%s.%s".format(
          Tag.UTCDate.format.print(game.createdAt),
          game.id,
          format.toString.toLowerCase
        ),
        "_"
      )
    }

  def filename(tour: Tournament, format: String): String =
    fileR.replaceAllIn(
      "lichess_tournament_%s_%s_%s.%s".format(
        Tag.UTCDate.format.print(tour.startsAt),
        tour.id,
        lila.common.String.slugify(tour.name),
        format
      ),
      "_"
    )

  private val upgradeOngoingGame =
    Flow[Game].mapAsync(4)(gameProxy.upgradeIfPresent)

  private def formatterFor(config: Config) =
    config.format match {
      case Format.JSON => jsonFormatter(config.flags)
    }

  private def emptyMsgFor(config: Config) =
    config.format match {
      case Format.JSON => "{}\n"
    }

  private def jsonFormatter(flags: WithFlags) =
    (
        game: Game,
        initialFen: Option[FEN],
        realPlayers: Option[RealPlayers]
    ) =>
      toJson(game, initialFen, flags, realPlayers) dmap { json =>
        s"${Json.stringify(json)}\n"
      }

  private def toJson(
      g: Game,
      initialFen: Option[FEN],
      withFlags: WithFlags,
      realPlayers: Option[RealPlayers] = None
  ): Fu[JsObject] =
    for {
      lightUsers <- gameLightUsers(g) dmap { case (wu, bu) => List(wu, bu) }
    } yield Json
      .obj(
        "id"         -> g.id,
        "rated"      -> g.rated,
        "variant"    -> g.variant.key,
        "speed"      -> g.speed.key,
        "perf"       -> PerfPicker.key(g),
        "createdAt"  -> g.createdAt,
        "lastMoveAt" -> g.movedAt,
        "status"     -> g.status.name,
        "players" -> JsObject(g.players zip lightUsers map { case (p, user) =>
          p.color.name -> Json
            .obj()
            .add("user", user)
            .add("rating", p.rating)
            .add("ratingDiff", p.ratingDiff)
            .add("name", p.name)
            .add("provisional" -> p.provisional)
            .add("aiLevel" -> p.aiLevel)
        // .add("moveCentis" -> withFlags.moveTimes ?? g.moveTimes(p.color).map(_.map(_.centis)))
        })
      )
      .add("initialFen" -> initialFen)
      .add("winner" -> g.winnerColor.map(_.name))
      .add("opening" -> g.opening.ifTrue(withFlags.opening))
      .add("moves" -> withFlags.moves.option {
        withFlags keepDelayIf g.playable applyDelay g.pgnMoves mkString " "
      })
      .add("daysPerTurn" -> g.daysPerTurn)
      .add("tournament" -> g.tournamentId)
      .add("clock" -> g.clock.map { clock =>
        Json.obj(
          "initial"   -> clock.limitSeconds,
          "increment" -> clock.incrementSeconds,
          "totalTime" -> clock.estimateTotalSeconds
        )
      })

  private def gameLightUsers(game: Game): Fu[(Option[LightUser], Option[LightUser])] =
    (game.whitePlayer.userId ?? getLightUser) zip (game.blackPlayer.userId ?? getLightUser)
}

object GameApiV2 {

  sealed trait Format
  object Format {
    case object JSON extends Format
    def byRequest(req: play.api.mvc.RequestHeader) = if (HTTPRequest acceptsNdJson req) JSON
  }

  sealed trait Config {
    val format: Format
    val flags: WithFlags
  }

  case class OneConfig(
      format: Format,
      imported: Boolean,
      flags: WithFlags,
      playerFile: Option[String]
  ) extends Config

  case class ByUserConfig(
      user: User,
      vs: Option[User],
      format: Format,
      since: Option[DateTime] = None,
      until: Option[DateTime] = None,
      max: Option[Int] = None,
      rated: Option[Boolean] = None,
      perfType: Set[lila.rating.PerfType],
      analysed: Option[Boolean] = None,
      color: Option[chess.Color],
      flags: WithFlags,
      perSecond: MaxPerSecond,
      playerFile: Option[String]
  ) extends Config {
    def postFilter(g: Game) =
      rated.fold(true)(g.rated ==) && {
        perfType.isEmpty || g.perfType.exists(perfType.contains)
      } && color.fold(true) { c =>
        g.player(c).userId has user.id
      } && analysed.fold(true)(g.metadata.analysed ==)
  }

  case class ByIdsConfig(
      ids: Seq[Game.ID],
      format: Format,
      flags: WithFlags,
      perSecond: MaxPerSecond,
      playerFile: Option[String] = None
  ) extends Config

  case class ByTournamentConfig(
      format: Format,
      flags: WithFlags,
      perSecond: MaxPerSecond
  ) extends Config
}

package controllers

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import play.api.mvc._
import scala.util.chaining._

import lila.api.GameApiV2
import lila.app._
import lila.common.config.MaxPerSecond
import lila.common.HTTPRequest
import lila.game.{ Game => GameModel }

final class Game(
    env: Env,
    apiC: => Api
) extends LilaController(env) {

  def bookmark(gameId: String) =
    Auth { implicit ctx => me =>
      env.bookmark.api.toggle(gameId, me.id)
    }

  def delete(gameId: String) =
    Auth { implicit ctx => me =>
      OptionFuResult(env.game.gameRepo game gameId) { game =>
        if (game.pgnImport.flatMap(_.user) ?? (me.id.==)) {
          env.hub.bookmark ! lila.hub.actorApi.bookmark.Remove(game.id)
          (env.game.gameRepo remove game.id) >>
            env.game.cached.clearNbImportedByCache(me.id) inject
            Redirect(routes.User.show(me.username))
        } else
          fuccess {
            Redirect(routes.Round.watcher(game.id, game.naturalOrientation.name))
          }
      }
    }

  private def WithVs(req: RequestHeader)(f: Option[lila.user.User] => Fu[Result]): Fu[Result] =
    get("vs", req) match {
      case None => f(none)
      case Some(name) =>
        env.user.repo named name flatMap {
          case None       => notFoundJson(s"No such opponent: $name")
          case Some(user) => f(user.some)
        }
    }

  private[controllers] def requestPgnFlags(req: RequestHeader, extended: Boolean) =
    lila.game.PgnDump.WithFlags(
      moves = getBoolOpt("moves", req) | true,
      tags = getBoolOpt("tags", req) | true,
      clocks = getBoolOpt("clocks", req) | extended,
      evals = getBoolOpt("evals", req) | extended,
      opening = getBoolOpt("opening", req) | extended,
      literate = getBoolOpt("literate", req) | false,
      pgnInJson = getBoolOpt("pgnInJson", req) | false,
      delayMoves = !get("key", req).exists(env.noDelaySecretSetting.get().value.contains)
    )

  private[controllers] def gameContentType(config: GameApiV2.Config) =
    config.format match {
      case GameApiV2.Format.JSON =>
        config match {
          case _: GameApiV2.OneConfig => JSON
          case _                      => ndJsonContentType
        }
    }

  private[controllers] def preloadUsers(game: GameModel): Funit =
    env.user.lightUserApi preloadMany game.userIds
}

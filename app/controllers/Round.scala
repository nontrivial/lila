package controllers

import play.api.libs.json._
import play.api.mvc._
import scala.concurrent.duration._
import views._

import lila.api.Context
import lila.app._
import lila.chat.Chat
import lila.common.HTTPRequest
import lila.game.{ Pov, Game => GameModel, PgnDump }
import lila.user.{ User => UserModel }

final class Round(
    env: Env,
    gameC: => Game,
    challengeC: => Challenge,
    userC: => User
) extends LilaController(env)
    with TheftPrevention {

  private def otherPovs(game: GameModel)(implicit ctx: Context) =
    ctx.me ?? { user =>
      env.round.proxyRepo urgentGames user map {
        _ filter { pov =>
          pov.gameId != game.id && pov.game.isSwitchable
        }
      }
    }

  private def getNext(currentGame: GameModel)(povs: List[Pov]) =
    povs find { pov =>
      pov.isMyTurn && (pov.game.hasClock || !currentGame.hasClock)
    }

  def whatsNext(fullId: String) =
    Open { implicit ctx =>
      OptionFuResult(env.round.proxyRepo.pov(fullId)) { currentPov =>
        if (currentPov.isMyTurn) fuccess {
          Ok(Json.obj("nope" -> true))
        }
        else
          otherPovs(currentPov.game) map getNext(currentPov.game) map { next =>
            Ok(Json.obj("next" -> next.map(_.fullId)))
          }
      }
    }

  private def proxyPov(gameId: String, color: String): Fu[Option[Pov]] =
    chess.Color.fromName(color) ?? {
      env.round.proxyRepo.pov(gameId, _)
    }

  private[controllers] def getPlayerChat(game: GameModel)(implicit
      ctx: Context
  ): Fu[Option[Chat.GameOrEvent]] =
    ctx.noKid ?? {
      def toEventChat(resource: String)(c: lila.chat.UserChat.Mine) =
        Chat
          .GameOrEvent(
            Right(
              (
                c truncate 100,
                lila.chat.Chat.ResourceId(resource)
              )
            )
          )
          .some
      (game.tournamentId) match {
        case _ =>
          game.hasChat ?? {
            env.chat.api.playerChat.findIf(Chat.Id(game.id), !game.justCreated) map { chat =>
              Chat
                .GameOrEvent(
                  Left(
                    Chat.Restricted(
                      chat,
                      restricted = game.fromLobby && ctx.isAnon
                    )
                  )
                )
                .some
            }
          }
      }
    }

  def sides(gameId: String, color: String) =
    Open { implicit ctx =>
      OptionFuResult(proxyPov(gameId, color)) { pov =>
          env.game.gameRepo.initialFen(pov.game) zip
          env.game.crosstableApi.withMatchup(pov.game) zip
          env.bookmark.api.exists(pov.game, ctx.me) map {
            case (((initialFen), crosstable), bookmarked) =>
              Ok(html.game.bits.sides(pov, initialFen, crosstable, bookmarked = bookmarked))
          }
      }
    }

  def writeNote(gameId: String) =
    AuthBody { implicit ctx => me =>
      import play.api.data.Forms._
      import play.api.data._
      implicit val req = ctx.body
      Form(single("text" -> text))
        .bindFromRequest()
        .fold(
          _ => fuccess(BadRequest),
          text => env.round.noteApi.set(gameId, me.id, text.trim take 10000)
        )
    }

  def readNote(gameId: String) =
    Auth { _ => me =>
      env.round.noteApi.get(gameId, me.id) dmap { Ok(_) }
    }

  def continue(id: String, mode: String) =
    Open { implicit ctx =>
      OptionResult(env.game.gameRepo game id) { game =>
        Redirect(
          "%s?fen=%s#%s".format(
            routes.Lobby.home,
            get("fen") | (chess.format.Forsyth >> game.chess).value,
            mode
          )
        )
      }
    }

  def resign(fullId: String) =
    Open { implicit ctx =>
      OptionFuRedirect(env.round.proxyRepo.pov(fullId)) { pov =>
        if (isTheft(pov)) {
          lila.log("round").warn(s"theft resign $fullId ${HTTPRequest.ipAddress(ctx.req)}")
          fuccess(routes.Lobby.home)
        } else {
          env.round resign pov
          import scala.concurrent.duration._
          akka.pattern.after(500.millis, env.system.scheduler)(fuccess(routes.Lobby.home))
        }
      }
    }

  def mini(gameId: String, color: String) =
    Open { implicit ctx =>
      OptionOk(
        chess.Color.fromName(color).??(env.round.proxyRepo.povIfPresent(gameId, _)) orElse env.game.gameRepo
          .pov(gameId, color)
      )(html.game.mini(_))
    }

  def miniFullId(fullId: String) =
    Open { implicit ctx =>
      OptionOk(env.round.proxyRepo.povIfPresent(fullId) orElse env.game.gameRepo.pov(fullId))(
        html.game.mini(_)
      )
    }

  def apiAddTime(anyId: String, seconds: Int) =
    Scoped(_.Challenge.Write) { implicit req => me =>
      import lila.round.actorApi.round.Moretime
      if (seconds < 1 || seconds > 86400) BadRequest.fuccess
      else
        env.round.proxyRepo.game(lila.game.Game takeGameId anyId) map {
          _.flatMap { Pov(_, me) }.?? { pov =>
            env.round.tellRound(pov.gameId, Moretime(pov.typedPlayerId, seconds.seconds))
            jsonOkResult
          }
        }
    }
}

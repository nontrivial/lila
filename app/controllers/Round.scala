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

  private def renderPlayer(pov: Pov)(implicit ctx: Context): Fu[Result] =
    negotiate(
      html =
        if (!pov.game.started) notFound
        else
          PreventTheft(pov) {
            env.tournament.api.gameView.player(pov) flatMap { tour =>
              gameC.preloadUsers(pov.game) zip
                getPlayerChat(pov.game) zip
                (ctx.noBlind ?? env.game.crosstableApi
                  .withMatchup(pov.game)) zip
                (pov.game.isSwitchable ?? otherPovs(pov.game)) zip
                env.bookmark.api.exists(pov.game, ctx.me) zip
                env.api.roundApi.player(pov, lila.api.Mobile.Api.currentVersion) map {
                  case ((((((_), chatOption), crosstable), playing), bookmarked), data) =>
                    Ok(
                      html.round.player(
                        pov,
                        data,
                        cross = crosstable,
                        playing = playing,
                        chatOption = chatOption,
                        bookmarked = bookmarked
                      )
                    )
                }
            }
          },
      api = apiVersion => {
        if (isTheft(pov)) fuccess(theftResponse)
        else
          env.tournament.api.gameView.mobile(pov.game) flatMap { tour =>
            gameC.preloadUsers(pov.game) zip
              env.api.roundApi.player(pov, apiVersion) zip
              getPlayerChat(pov.game) map { case ((_, data), chat) =>
                Ok {
                  data.add("chat", chat.flatMap(_.game).map(c => lila.chat.JsonView(c.chat)))
                }
              }
          }
      }
    ) dmap NoCache

  def player(fullId: String) =
    Open { implicit ctx =>
      env.round.proxyRepo.pov(fullId) flatMap {
        case Some(pov) => renderPlayer(pov)
        case None      => userC.tryRedirect(fullId) getOrElse notFound
      }
    }

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

  def next(gameId: String) =
    Auth { implicit ctx => me =>
      OptionFuResult(env.round.proxyRepo game gameId) { currentGame =>
        otherPovs(currentGame) map getNext(currentGame) map {
          _ orElse Pov(currentGame, me)
        } flatMap {
          case Some(next) => renderPlayer(next)
          case None =>
            fuccess(Redirect(currentGame.simulId match {
              case None          => routes.Round.watcher(gameId, "white")
            }))
        }
      }
    }

  def watcher(gameId: String, color: String) =
    Open { implicit ctx =>
      proxyPov(gameId, color) flatMap {
        case Some(pov) =>
          get("pov") match {
            case Some(requestedPov) =>
              (pov.player.userId, pov.opponent.userId) match {
                case (Some(_), Some(opponent)) if opponent == requestedPov =>
                  Redirect(routes.Round.watcher(gameId, (!pov.color).name)).fuccess
                case (Some(player), Some(_)) if player == requestedPov =>
                  Redirect(routes.Round.watcher(gameId, pov.color.name)).fuccess
                case _ =>
                  Redirect(routes.Round.watcher(gameId, "white")).fuccess
              }
            case None =>
              watch(pov)
          }
        case None => userC.tryRedirect(gameId) getOrElse challengeC.showId(gameId)
      }
    }

  private def proxyPov(gameId: String, color: String): Fu[Option[Pov]] =
    chess.Color.fromName(color) ?? {
      env.round.proxyRepo.pov(gameId, _)
    }

  private[controllers] def watch(pov: Pov, userTv: Option[UserModel] = None)(implicit
      ctx: Context
  ): Fu[Result] =
    playablePovForReq(pov.game) match {
      case Some(player) if userTv.isEmpty => renderPlayer(pov withColor player.color)
      case _ if pov.game.variant == chess.variant.RacingKings && pov.color.black =>
        if (userTv.isDefined) watch(!pov, userTv)
        else Redirect(routes.Round.watcher(pov.gameId, "white")).fuccess
      case _ =>
        negotiate(
          html = {
            if (HTTPRequest.isHuman(ctx.req))
                getWatcherChat(pov.game) zip
                (ctx.noBlind ?? env.game.crosstableApi.withMatchup(pov.game)) zip
                env.bookmark.api.exists(pov.game, ctx.me) flatMap {
                  case ((chat, crosstable), bookmarked) =>
                    env.api.roundApi.watcher(
                      pov,
                      lila.api.Mobile.Api.currentVersion
                    ) map { data =>
                      Ok(
                        html.round.watcher(
                          pov,
                          data,
                          crosstable,
                          chatOption = chat,
                          bookmarked = bookmarked
                        )
                      )
                    }
                }
            else
              for { // web crawlers don't need the full thing
                initialFen <- env.game.gameRepo.initialFen(pov.gameId)
                pgn        <- env.api.pgnDump(pov.game, initialFen, PgnDump.WithFlags(clocks = false))
              } yield Ok(html.round.watcher.crawler(pov, initialFen, pgn))
          },
          api = apiVersion =>
            for {
              data     <- env.api.roundApi.watcher(pov, apiVersion)
              chat     <- getWatcherChat(pov.game)
            } yield Ok {
              data
                .add("chat" -> chat.map(c => lila.chat.JsonView(c.chat)))
            }
        ) map { NoCache(_) }
    }

  private[controllers] def getWatcherChat(
      game: GameModel
  )(implicit ctx: Context): Fu[Option[lila.chat.UserChat.Mine]] = {
    ctx.noKid && ctx.me.fold(true)(env.chat.panic.allowed) && {
      game.finishedOrAborted || !ctx.userId.exists(game.userIds.contains)
    }
  } ?? {
    val id = Chat.Id(s"${game.id}/w")
    env.chat.api.userChat.findMineIf(id, ctx.me, !game.justCreated) flatMap { chat =>
      env.user.lightUserApi.preloadMany(chat.chat.userIds) inject chat.some
    }
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

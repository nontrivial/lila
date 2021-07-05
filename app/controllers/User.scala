package controllers

import akka.stream.scaladsl._
import play.api.data.Form
import play.api.http.ContentTypes
import play.api.libs.EventSource
import play.api.libs.json._
import play.api.mvc._
import scala.concurrent.duration._
import scala.language.existentials
import scala.util.chaining._
import scalatags.Text.Frag
import views._

import lila.api.{ BodyContext, Context }
import lila.app._
import lila.app.mashup.{ GameFilter, GameFilterMenu }
import lila.common.paginator.Paginator
import lila.common.{ HTTPRequest, IpAddress }
import lila.game.{ Pov, Game => GameModel }
import lila.rating.PerfType
import lila.security.UserLogins
import lila.socket.UserLagCache
import lila.user.{ User => UserModel, Holder }
import lila.security.Granter

final class User(
    env: Env,
    roundC: => Round,
    gameC: => Game
) extends LilaController(env) {

  private def relationApi    = env.relation.api
  private def userGameSearch = env.gameSearch.userGameSearch

  private def apiGames(u: UserModel, filter: String, page: Int)(implicit ctx: BodyContext[_]) = {
    userGames(u, filter, page) flatMap env.api.userGameApi.jsPaginator map { res =>
      Ok(res ++ Json.obj("filter" -> GameFilter.All.name))
    }
  }

  def show(username: String) =
    OpenBody { implicit ctx =>
      EnabledUser(username) { u =>
        negotiate(
          html = renderShow(u),
          api = _ => apiGames(u, GameFilter.All.name, 1)
        )
      }
    }
  private def renderShow(u: UserModel, status: Results.Status = Results.Ok)(implicit ctx: Context) =
    if (HTTPRequest.isSynchronousHttp(ctx.req)) {
      for {
        nbs    <- env.userNbGames(u, ctx, withCrosstable = false)
        info   <- env.userInfo(u, nbs, ctx)
        social <- env.socialInfo(u, ctx)
      } yield status {
        lila.mon.chronoSync(_.user segment "renderSync") {
          html.user.show.page.activity(u, info, social)
        }
      }
    } else
      env.activity.read.recent(u) map { as =>
        status(html.activity(u, as))
      }

  def download(username: String) = OpenBody { implicit ctx =>
    OptionOk(env.user.repo named username) { user =>
      html.user.download(user)
    }
  }

  def gamesAll(username: String, page: Int) = games(username, GameFilter.All.name, page)

  def games(username: String, filter: String, page: Int) =
    OpenBody { implicit ctx =>
      Reasonable(page) {
        EnabledUser(username) { u =>
          if (filter == "search" && ctx.isAnon)
            negotiate(
              html = Unauthorized(html.search.login(u.count.game)).fuccess,
              api = _ => Unauthorized(jsonError("Login required")).fuccess
            )
          else
            negotiate(
              html = for {
                nbs <- env.userNbGames(u, ctx, withCrosstable = true)
                filters = GameFilterMenu(u, nbs, filter, ctx.isAuth)
                pag <- env.gamePaginator(
                  user = u,
                  nbs = nbs.some,
                  filter = filters.current,
                  me = ctx.me,
                  page = page
                )(ctx.body, formBinding)
                notes <- ctx.me ?? { me =>
                  env.round.noteApi.byGameIds(pag.currentPageResults.map(_.id), me.id)
                }
                res <-
                  if (HTTPRequest isSynchronousHttp ctx.req) for {
                    info   <- env.userInfo(u, nbs, ctx)
                    social <- env.socialInfo(u, ctx)
                    searchForm =
                      (filters.current == GameFilter.Search) option
                        GameFilterMenu.searchForm(userGameSearch, filters.current)(ctx.body, formBinding)
                  } yield html.user.show.page.games(u, info, pag, filters, searchForm, social, notes)
                  else fuccess(html.user.show.gamesContent(u, nbs, pag, filters, filter, notes))
              } yield res,
              api = _ => apiGames(u, filter, page)
            )
        }
      }
    }

  private def EnabledUser(username: String)(f: UserModel => Fu[Result])(implicit ctx: Context): Fu[Result] =
    if (UserModel.isGhost(username))
      negotiate(
        html = Ok(html.site.bits.ghost).fuccess,
        api = _ => notFoundJson("Deleted user")
      )
    else
      env.user.repo named username flatMap {
        case None                                             => notFound
        case Some(u) if u.enabled || isGranted(_.UserModView) => f(u)
        case Some(u) =>
          negotiate(
            html = env.user.repo isErased u flatMap { erased =>
              if (erased.value) notFound
              else NotFound(html.user.show.page.disabled(u)).fuccess
            },
            api = _ => fuccess(NotFound(jsonError("No such user, or account closed")))
          )
      }
  def showMini(username: String) =
    Open { implicit ctx =>
      OptionFuResult(env.user.repo named username) { user =>
        if (user.enabled || isGranted(_.UserModView))
          ctx.userId.?? { relationApi.fetchBlocks(user.id, _) } zip
            ctx.userId.?? { env.game.crosstableApi.fetchOrEmpty(user.id, _) dmap some } zip
            ctx.isAuth.?? { env.pref.api.followable(user.id) } zip
            ctx.userId.?? { relationApi.fetchRelation(_, user.id) } flatMap {
              case (((blocked, crosstable), followable), relation) =>
                val ping = env.socket.isOnline(user.id) ?? UserLagCache.getLagRating(user.id)
                negotiate(
                  html = !ctx.is(user) ?? currentlyPlaying(user) map { pov =>
                    Ok(html.user.mini(user, pov, blocked, followable, relation, ping, crosstable))
                      .withHeaders(CACHE_CONTROL -> "max-age=5")
                  },
                  api = _ => {
                    import lila.game.JsonView.crosstableWrites
                    fuccess(
                      Ok(
                        Json.obj(
                          "crosstable" -> crosstable,
                          "perfs"      -> lila.user.JsonView.perfs(user, user.best8Perfs)
                        )
                      )
                    )
                  }
                )
            }
        else fuccess(Ok(html.user.bits.miniClosed(user)))
      }
    }

  def online =
    Action.async { implicit req =>
      val max = 50
      negotiate(
        html = notFoundJson(),
        api = _ =>
          env.user.cached.getTop50Online map { users =>
            Ok(
              Json.toJson(
                users
                  .take(getInt("nb", req).fold(10)(_ min max))
                  .map(env.user.jsonView(_))
              )
            )
          }
      )
    }

  def ratingHistory(username: String) =
    OpenBody { implicit ctx =>
      EnabledUser(username) { u =>
        env.history
          .ratingChartApi(u)
          .dmap(_ | "[]") // send an empty JSON array if no history JSON is available
          .dmap(jsonStr => Ok(jsonStr) as JSON)
      }
    }

  private def currentlyPlaying(user: UserModel): Fu[Option[Pov]] =
    env.game.cached.lastPlayedPlayingId(user.id) flatMap {
      _ ?? { env.round.proxyRepo.pov(_, user) }
    }

  private def lastPlayed(user: UserModel): Fu[Option[Pov]] =
    env.game.gameRepo
      .lastPlayed(user)
      .flatMap(_ ?? { p =>
        env.round.proxyRepo.upgradeIfPresent(p) dmap some
      })

  private val UserGamesRateLimitPerIP = new lila.memo.RateLimit[IpAddress](
    credits = 500,
    duration = 10.minutes,
    key = "user_games.web.ip"
  )

  private def userGames(
      u: UserModel,
      filterName: String,
      page: Int
  )(implicit ctx: BodyContext[_]): Fu[Paginator[GameModel]] = {
    UserGamesRateLimitPerIP(HTTPRequest ipAddress ctx.req, cost = page, msg = s"on ${u.username}") {
      lila.mon.http.userGamesCost.increment(page.toLong)
      for {
        pagFromDb <- env.gamePaginator(
          user = u,
          nbs = none,
          filter = GameFilterMenu.currentOf(GameFilterMenu.all, filterName),
          me = ctx.me,
          page = page
        )(ctx.body, formBinding)
        pag <- pagFromDb.mapFutureResults(env.round.proxyRepo.upgradeIfPresent)
        _ <- env.user.lightUserApi preloadMany pag.currentPageResults.flatMap(_.userIds)
      } yield pag
    }(fuccess(Paginator.empty[GameModel]))
  }

  def list =
    Open { implicit ctx =>
      env.user.cached.top10.get {} flatMap { leaderboards =>
        negotiate(
          html =
            for {
              nbAllTime      <- env.user.cached.top10NbGame.get {}
              topOnline      <- env.user.cached.getTop50Online
            } yield Ok(
              html.user.list(
                online = topOnline,
                leaderboards = leaderboards,
                nbAllTime = nbAllTime
              )
            ),
          api = _ =>
            fuccess {
              implicit val lpWrites = OWrites[UserModel.LightPerf](env.user.jsonView.lightPerfIsOnline)
              Ok(
                Json.obj(
                  "bullet"        -> leaderboards.bullet,
                  "blitz"         -> leaderboards.blitz,
                  "rapid"         -> leaderboards.rapid,
                  "classical"     -> leaderboards.classical,
                  "ultraBullet"   -> leaderboards.ultraBullet,
                  "crazyhouse"    -> leaderboards.crazyhouse,
                  "chess960"      -> leaderboards.chess960,
                  "kingOfTheHill" -> leaderboards.kingOfTheHill,
                  "threeCheck"    -> leaderboards.threeCheck,
                  "antichess"     -> leaderboards.antichess,
                  "atomic"        -> leaderboards.atomic,
                  "horde"         -> leaderboards.horde,
                  "racingKings"   -> leaderboards.racingKings
                )
              )
            }
        )
      }
    }

  def topNb(nb: Int, perfKey: String) =
    Open { implicit ctx =>
      PerfType(perfKey) ?? { perfType =>
        env.user.cached.top200Perf get perfType.id dmap { _ take (nb atLeast 1 atMost 200) } flatMap {
          users =>
            negotiate(
              html = (nb == 200) ?? Ok(html.user.top(perfType, users)).fuccess,
              api = _ =>
                fuccess {
                  implicit val lpWrites = OWrites[UserModel.LightPerf](env.user.jsonView.lightPerfIsOnline)
                  Ok(Json.obj("users" -> users))
                }
            )
        }
      }
    }

  def topWeek =
    Open { implicit ctx =>
      negotiate(
        html = notFound,
        api = _ =>
          env.user.cached.topWeek.map { users =>
            Ok(Json toJson users.map(env.user.jsonView.lightPerfIsOnline))
          }
      )
    }

  protected[controllers] def loginsTableData(user: UserModel, userLogins: UserLogins, max: Int)(implicit
      ctx: Context
  ): Fu[UserLogins.TableData] = {
    val familyUserIds = user.id :: userLogins.otherUserIds
    (isGranted(_.ModNote) ?? env.user.noteApi
      .forMod(familyUserIds)
      .logTimeIfGt(s"${user.username} noteApi.forMod", 2 seconds)) zip
      env.playban.api.bans(familyUserIds).logTimeIfGt(s"${user.username} playban.bans", 2 seconds) zip
      lila.security.UserLogins.withMeSortedWithEmails(env.user.repo, user, userLogins) map {
        case ((notes, bans), othersWithEmail) =>
          UserLogins.TableData(userLogins, othersWithEmail, notes, bans, max)
      }
  }

  def writeNote(username: String) =
    AuthBody { implicit ctx => me =>
      doWriteNote(username, me)(
        _ => user => renderShow(user, Results.BadRequest),
        Redirect(routes.User.show(username)).flashSuccess
      )(ctx.body)
    }

  def apiWriteNote(username: String) =
    ScopedBody() { implicit req => me =>
      doWriteNote(username, me)(
        err = err => _ => jsonFormErrorDefaultLang(err),
        suc = jsonOkResult
      )
    }

  private def doWriteNote(
      username: String,
      me: UserModel
  )(err: Form[_] => UserModel => Fu[Result], suc: => Result)(implicit req: Request[_]) =
    env.user.repo named username flatMap {
      _ ?? { user =>
        env.user.forms.note
          .bindFromRequest()
          .fold(
            e => err(e)(user),
            data =>
              {
                val isMod = data.mod && isGranted(_.ModNote, me)
                env.user.noteApi.write(user, data.text, me, isMod, isMod && ~data.dox)
              } inject suc
          )
      }
    }

  def deleteNote(id: String) =
    Auth { implicit ctx => me =>
      OptionFuResult(env.user.noteApi.byId(id)) { note =>
        (note.isFrom(me) && !note.mod) ?? {
          env.user.noteApi.delete(note._id) inject Redirect(routes.User.show(note.to).url + "?note")
        }
      }
    }

  def opponents =
    Auth { implicit ctx => me =>
      for {
        ops         <- env.game.favoriteOpponents(me.id)
        followables <- env.pref.api.followables(ops map (_._1.id))
        relateds <-
          ops
            .zip(followables)
            .map { case ((u, nb), followable) =>
              relationApi.fetchRelation(me.id, u.id) map {
                lila.relation.Related(u, nb.some, followable, _)
              }
            }
            .sequenceFu
      } yield html.user.opponents(me, relateds)
    }

  def perfStat(username: String, perfKey: String) =
    Open { implicit ctx =>
      env.perfStat.api.data(username, perfKey, ctx.me) flatMap {
        _ ?? { data =>
          negotiate(
            html = env.history.ratingChartApi(data.user) map { chart =>
              Ok(html.user.perfStat(data, chart))
            },
            api = _ =>
              JsonOk {
                getBool("graph").?? {
                  env.history.ratingChartApi.singlePerf(data.user, data.stat.perfType) map some
                } map { graph =>
                  env.perfStat.jsonView(data).add("graph", graph)
                }
              }
          )
        }
      }
    }

  def autocomplete =
    Open { implicit ctx =>
      get("term", ctx.req).filter(_.nonEmpty).filter(lila.user.User.couldBeUsername) match {
        case None => BadRequest("No search term provided").fuccess
        case Some(term) if getBool("exists") =>
          env.user.repo nameExists term map { r =>
            Ok(JsBoolean(r))
          }
        case Some(term) =>
          {
            (get("tour"), get("swiss")) match {
              case _ =>
                ctx.me.ifTrue(getBool("friend")) match {
                  case Some(follower) =>
                    env.relation.api.searchFollowedBy(follower, term, 10) flatMap {
                      case Nil     => env.user.cached userIdsLike term
                      case userIds => fuccess(userIds)
                    }
                  case None if getBool("teacher") =>
                    env.user.repo.userIdsLikeWithRole(term, lila.security.Permission.Teacher.dbKey)
                  case None => env.user.cached userIdsLike term
                }
            }
          } flatMap { userIds =>
            if (getBool("names")) env.user.lightUserApi.asyncMany(userIds) map { users =>
              Json toJson users.flatMap(_.map(_.name))
            }
            else if (getBool("object")) env.user.lightUserApi.asyncMany(userIds) map { users =>
              Json.obj(
                "result" -> JsArray(users.flatten.map { u =>
                  lila.common.LightUser.lightUserWrites.writes(u).add("online" -> env.socket.isOnline(u.id))
                })
              )
            }
            else fuccess(Json toJson userIds)
          } map JsonOk
      }
    }

  def myself =
    Auth { _ => me =>
      fuccess(Redirect(routes.User.show(me.username)))
    }

  def redirect(username: String) =
    Open { implicit ctx =>
      tryRedirect(username) getOrElse notFound
    }

  def tryRedirect(username: String)(implicit ctx: Context): Fu[Option[Result]] =
    env.user.repo enabledNamed username map2 { user =>
      Redirect(routes.User.show(user.username))
    }
}

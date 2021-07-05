package lila.api

import chess.format.FEN
import play.api.i18n.Lang
import play.api.libs.json._

import lila.common.ApiVersion
import lila.game.{ Game, Pov }
import lila.pref.Pref
import lila.round.JsonView.WithFlags
import lila.round.{ Forecast, JsonView }
import lila.security.Granter
import lila.tree.Node.partitionTreeJsonWriter
import lila.user.User

final private[api] class RoundApi(
    jsonView: JsonView,
    noteApi: lila.round.NoteApi,
    forecastApi: lila.round.ForecastApi,
    bookmarkApi: lila.bookmark.BookmarkApi,
    gameRepo: lila.game.GameRepo,
    getLightUser: lila.common.LightUser.GetterSync
)(implicit ec: scala.concurrent.ExecutionContext) {

  def player(pov: Pov, apiVersion: ApiVersion)(implicit
      ctx: Context
  ): Fu[JsObject] =
    gameRepo
      .initialFen(pov.game)
      .flatMap { initialFen =>
        implicit val lang = ctx.lang
        jsonView.playerJson(
          pov,
          ctx.pref,
          apiVersion,
          ctx.me,
          withFlags = WithFlags(blurs = ctx.me ?? Granter(_.ViewBlurs)),
          initialFen = initialFen,
          nvui = ctx.blind
        ) zip
          (ctx.me.ifTrue(ctx.isMobileApi) ?? (me => noteApi.get(pov.gameId, me.id))) zip
          forecastApi.loadForDisplay(pov) zip
          bookmarkApi.exists(pov.game, ctx.me) map {
            case (((((json)), note), forecast), bookmarked) =>
              (
                  withSteps(pov, initialFen) compose
                  withNote(note) compose
                  withBookmark(bookmarked) compose
                  withForecastCount(forecast.map(_.steps.size))
              )(json)
          }
      }
      .mon(_.round.api.player)

  def watcher(
      pov: Pov,
      apiVersion: ApiVersion,
      tv: Option[lila.round.OnTv],
      initialFenO: Option[Option[FEN]] = None
  )(implicit ctx: Context): Fu[JsObject] =
    initialFenO
      .fold(gameRepo initialFen pov.game)(fuccess)
      .flatMap { initialFen =>
        implicit val lang = ctx.lang
        jsonView.watcherJson(
          pov,
          ctx.pref,
          apiVersion,
          ctx.me,
          tv,
          initialFen = initialFen,
          withFlags = WithFlags(blurs = ctx.me ?? Granter(_.ViewBlurs))
        ) zip
          (ctx.me.ifTrue(ctx.isMobileApi) ?? (me => noteApi.get(pov.gameId, me.id))) zip
          bookmarkApi.exists(pov.game, ctx.me) map { case ((((json)), note), bookmarked) =>
            (
                withNote(note) compose
                withBookmark(bookmarked) compose
                withSteps(pov, initialFen)
            )(json)
          }
      }
      .mon(_.round.api.watcher)

  def review(
      pov: Pov,
      apiVersion: ApiVersion,
      tv: Option[lila.round.OnTv] = None,
      initialFenO: Option[Option[FEN]] = None,
      withFlags: WithFlags,
      owner: Boolean = false
  )(implicit ctx: Context): Fu[JsObject] =
    initialFenO
      .fold(gameRepo initialFen pov.game)(fuccess)
      .flatMap { initialFen =>
        implicit val lang = ctx.lang
        jsonView.watcherJson(
          pov,
          ctx.pref,
          apiVersion,
          ctx.me,
          tv,
          initialFen = initialFen,
          withFlags = withFlags.copy(blurs = ctx.me ?? Granter(_.ViewBlurs))
        ) zip
          ctx.userId.ifTrue(ctx.isMobileApi).?? {
            noteApi.get(pov.gameId, _)
          } zip
          (owner.??(forecastApi loadForDisplay pov)) zip
          bookmarkApi.exists(pov.game, ctx.me) map {
            case ((((((json))), note), fco), bookmarked) =>
              (
                  withNote(note) compose
                  withBookmark(bookmarked) compose
                  withForecast(pov, owner, fco)
              )(json)
          }
      }
      .mon(_.round.api.watcher)

  private def withSteps(pov: Pov, initialFen: Option[FEN])(obj: JsObject) =
    obj + ("steps" -> lila.round.StepBuilder(
      id = pov.gameId,
      pgnMoves = pov.game.pgnMoves,
      variant = pov.game.variant,
      initialFen = initialFen | pov.game.variant.initialFen
    ))

  private def withNote(note: String)(json: JsObject) =
    if (note.isEmpty) json else json + ("note" -> JsString(note))

  private def withBookmark(v: Boolean)(json: JsObject) =
    json.add("bookmarked" -> v)

  private def withForecastCount(count: Option[Int])(json: JsObject) =
    count.filter(0 !=).fold(json) { c =>
      json + ("forecastCount" -> JsNumber(c))
    }

  private def withForecast(pov: Pov, owner: Boolean, fco: Option[Forecast])(json: JsObject) =
    if (pov.game.forecastable && owner)
      json + (
        "forecast" -> {
          if (pov.forecastable) fco.fold[JsValue](Json.obj("none" -> true)) { fc =>
            import Forecast.forecastJsonWriter
            Json toJson fc
          }
          else Json.obj("onMyTurn" -> true)
        }
      )
    else json

}

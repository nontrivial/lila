package views.html.analyse

import bits.dataPanel
import chess.variant.Crazyhouse
import controllers.routes
import play.api.i18n.Lang
import play.api.libs.json.Json

import lila.api.Context
import lila.app.templating.Environment._
import lila.app.ui.ScalatagsTemplate._
import lila.common.String.html.safeJsonValue
import lila.game.Pov

object replay {

  private[analyse] def titleOf(pov: Pov)(implicit lang: Lang) =
    s"${playerText(pov.game.whitePlayer)} vs ${playerText(pov.game.blackPlayer)}: ${pov.game.opening
      .fold(trans.analysis.txt())(_.opening.ecoName)}"

  def apply(
      pov: Pov,
      data: play.api.libs.json.JsObject,
      initialFen: Option[chess.format.FEN],
      pgn: String,
      analysis: Option[lila.analyse.Analysis],
      analysisStarted: Boolean,
      cross: Option[lila.game.Crosstable.WithMatchup],
      userTv: Option[lila.user.User],
      chatOption: Option[lila.chat.UserChat.Mine],
      bookmarked: Boolean
  )(implicit ctx: Context) = {

    import pov._

    val chatJson = chatOption map { c =>
      views.html.chat.json(
        c.chat,
        name = trans.spectatorRoom.txt(),
        timeout = c.timeout,
        withNoteAge = ctx.isAuth option game.secondsSinceCreation,
        public = true,
        resourceId = lila.chat.Chat.ResourceId(s"game/${c.chat.id}"),
        palantir = ctx.me.exists(_.canPalantir)
      )
    }

    bits.layout(
      title = titleOf(pov),
      moreCss = frag(
        cssTag("analyse.round"),
        pov.game.variant == Crazyhouse option cssTag("analyse.zh"),
        ctx.blind option cssTag("round.nvui")
      ),
      moreJs = frag(
        analyseTag,
        analyseNvuiTag,
        embedJsUnsafeLoadThen(s"""LichessAnalyse.boot(${safeJsonValue(
          Json
            .obj(
              "data"   -> data,
              "i18n"   -> jsI18n(),
              "userId" -> ctx.userId,
              "chat"   -> chatJson,
              "explorer" -> Json.obj(
                "endpoint"          -> explorerEndpoint,
                "tablebaseEndpoint" -> tablebaseEndpoint
              )
            )
            .add("hunter" -> isGranted(_.Hunter))
        )})""")
      ),
      openGraph = povOpenGraph(pov).some
    )(
      frag(
        main(cls := "analyse")(
          st.aside(cls := "analyse__side")(
            views.html.game
              .side(
                pov,
                initialFen,
                bookmarked = bookmarked
              )
          ),
          chatOption.map(_ => views.html.chat.frag)
        ),
        if (ctx.blind)
          div(cls := "blind-content none")(
            h2("PGN downloads"),
            input(tpe := "hidden", value := pgn, cls := "game-pgn"),
            button(cls := "copy-pgn", dataRel := "game-pgn")(
              "Copy PGN to clipboard"
            )
          )
      )
    )
  }
}

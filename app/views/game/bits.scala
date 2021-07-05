package views.html.game

import play.api.i18n.Lang

import lila.api.Context
import lila.app.templating.Environment._
import lila.app.ui.ScalatagsTemplate._
import lila.game.{ Game, Pov }
import lila.rating.PerfType.Correspondence

import controllers.routes

object bits {

  def gameIcon(game: Game): Char =
    game.perfType match {
      case _ if game.fromPosition         => ''
      case _ if game.imported             => ''
      case Some(p) if game.variant.exotic => p.iconChar
      case _ if game.hasAi                => ''
      case Some(p)                        => p.iconChar
      case _                              => ''
    }

  def sides(
      pov: Pov,
      initialFen: Option[chess.format.FEN],
      cross: Option[lila.game.Crosstable.WithMatchup],
      bookmarked: Boolean
  )(implicit ctx: Context) =
    div(
      side.meta(pov, initialFen, bookmarked = bookmarked),
      cross.map { c =>
        div(cls := "crosstable")(crosstable(ctx.userId.fold(c)(c.fromPov), pov.gameId.some))
      }
    )

}

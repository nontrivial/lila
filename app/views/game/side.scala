package views.html
package game

import lila.api.Context
import lila.app.templating.Environment._
import lila.app.ui.ScalatagsTemplate._

import controllers.routes

object side {

  private val separator  = " • "
  private val dataUserTv = attr("data-user-tv")
  private val dataTime   = attr("data-time")

  def apply(
      pov: lila.game.Pov,
      initialFen: Option[chess.format.FEN],
      bookmarked: Boolean
  )(implicit ctx: Context): Option[Frag] =
    ctx.noBlind option frag(
      meta(pov, initialFen, bookmarked)
    )

  def meta(
      pov: lila.game.Pov,
      initialFen: Option[chess.format.FEN],
      bookmarked: Boolean
  )(implicit ctx: Context): Option[Frag] =
    ctx.noBlind option {
      import pov._
      div(cls := "game__meta")(
        st.section(
          div(cls := "game__meta__infos", dataIcon := bits.gameIcon(game))(
            div(
              div(cls := "header")(
                div(cls := "setup")(
                  views.html.bookmark.toggle(game, bookmarked),
                  if (game.imported)
                    div(
                      a(href := routes.Importer.importGame, title := trans.importGame.txt())("IMPORT"),
                      separator,
                      bits.variantLink(game.variant, initialFen = initialFen, shortName = true)
                    )
                  else
                    frag(
                      widgets showClock game,
                      separator,
                      (if (game.rated) trans.rated else trans.casual).txt(),
                      separator,
                      bits.variantLink(game.variant, game.perfType, initialFen, shortName = true)
                    )
                ),
                game.pgnImport.flatMap(_.date).map(frag(_)) getOrElse {
                  frag(
                    if (game.isBeingPlayed) trans.playingRightNow()
                    else momentFromNowWithPreload(game.createdAt)
                  )
                }
              ),
              game.pgnImport.exists(_.date.isDefined) option small(
                "Imported ",
                game.pgnImport.flatMap(_.user).map { user =>
                  trans.by(userIdLink(user.some, None, withOnline = false))
                }
              )
            )
          ),
          div(cls := "game__meta__players")(
            game.players.map { p =>
              frag(
                div(cls := s"player color-icon is ${p.color.name} text")(
                  playerLink(p, withOnline = false, withDiff = true, withBerserk = true)
                )
              )
            }
          )
        ),
        game.finishedOrAborted option {
          st.section(cls := "status")(
            gameEndStatus(game),
            game.winner.map { winner =>
              frag(
                separator,
                winner.color.fold(trans.whiteIsVictorious, trans.blackIsVictorious)()
              )
            }
          )
        },
        game.variant.chess960 ??
          chess.variant.Chess960
            .positionNumber(initialFen | chess.format.Forsyth.initial)
            .map { number =>
              st.section(
                "Chess960 start position: ",
                strong(number)
              )
            }
      )
    }
}

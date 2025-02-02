package views.html.user.show

import lila.app.templating.Environment._
import lila.app.ui.ScalatagsTemplate._
import lila.user.User

import controllers.routes

object newPlayer {

  def apply(u: User) =
    div(cls := "new-player")(
      h2("Welcome to lichess.org!"),
      p(
        "This is your profile page.",
        u.profile.isEmpty option frag(
          br,
          "Would you like to ",
          a(href := routes.Account.profile)("improve it"),
          "?"
        )
      ),
      p(
        "What now? Here are a few suggestions:"
      ),
      ul(
        li(a(href := s"${routes.Lobby.home}#ai")("Play the artificial intelligence")),
        li(a(href := s"${routes.Lobby.home}#hook")("Play opponents from around the world")),
        li(a(href := routes.User.list)("Follow your friends on Lichess")),
        li(a(href := routes.Pref.form("game-display"))("Configure Lichess to your liking")),
        li("Explore the site and have fun :)")
      )
    )
}

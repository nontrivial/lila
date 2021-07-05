package views.html.base

import controllers.routes

import lila.api.Context
import lila.app.templating.Environment._
import lila.app.ui.ScalatagsTemplate._

object topnav {

  private def linkTitle(url: String, name: Frag)(implicit ctx: Context) =
    if (ctx.blind) h3(name) else a(href := url)(name)

  def apply()(implicit ctx: Context) =
    st.nav(id := "topnav", cls := "hover")(
      st.section(
        linkTitle(
          "/",
          frag(
            span(cls := "play")(trans.play()),
            span(cls := "home")("lichess.org")
          )
        ),
        div(role := "group")(
          if (ctx.noBot) a(href := "/?any#hook")(trans.createAGame())
          else a(href := "/?any#friend")(trans.playWithAFriend()),
        )
      ),
      st.section(
        linkTitle(routes.User.list.path, trans.community()),
        div(role := "group")(
          a(href := routes.User.list)(trans.players())
        )
      )
    )
}

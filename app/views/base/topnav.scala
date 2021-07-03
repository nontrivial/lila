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
          ctx.noBot option frag(
            a(href := routes.Tournament.home)(trans.arena.arenaTournaments()),
            ctx.pref.hasDgt option a(href := routes.DgtCtrl.index)("DGT board")
          )
        )
      ),
      st.section(
        linkTitle(routes.User.list.path, trans.community()),
        div(role := "group")(
          a(href := routes.User.list)(trans.players()),
          ctx.noKid option a(href := routes.ForumCateg.index)(trans.forum()),
          ctx.me.exists(!_.kid) option a(href := routes.Plan.index)(trans.patron.donate())
        )
      )
    )
}

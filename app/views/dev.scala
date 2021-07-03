package views.html

import play.api.data.Form

import lila.api.Context
import lila.app.templating.Environment._
import lila.app.ui.ScalatagsTemplate._

import controllers.routes

object dev {

  def settings(settings: List[lila.memo.SettingStore[_]])(implicit ctx: Context) = {
    val title = "Settings"
    views.html.base.layout(
      title = title,
      moreCss = cssTag("mod.misc")
    )(
      main(cls := "page-menu")(
        div(id := "settings", cls := "page-menu__content box box-pad")(
          h1(title),
          p("Tread lightly."),
          settings.map { s =>
            postForm(action := routes.Dev.settingsPost(s.id))(
              p(s.text | s.id),
              s.form.value match {
                case Some(v: Boolean) =>
                  div(
                    span(cls := "form-check-input")(form3.cmnToggle(s.id, "v", v))
                  )
                case v =>
                  input(
                    name := "v",
                    value := (v match {
                      case None    => ""
                      case Some(x) => x.toString
                      case x       => x.toString
                    })
                  )
              },
              submitButton(cls := "button button-empty", dataIcon := "")
            )
          }
        )
      )
    )
  }

}

package views.html
package account

import lila.api.Context
import lila.app.templating.Environment._
import lila.app.ui.ScalatagsTemplate._

import controllers.routes

object close {

  import trans.settings._

  def apply(u: lila.user.User, form: play.api.data.Form[_])(implicit ctx: Context) =
    account.layout(
      title = s"${u.username} - ${closeAccount.txt()}",
      active = "close"
    ) {
      div(cls := "account box box-pad")(
      h1(dataIcon := "", cls := "text")(closeAccount()),
        postForm(cls := "form3", action := routes.Account.closeConfirm)(
          div(cls := "form-group")(closeAccountExplanation()),
          div(cls := "form-group")(cantOpenSimilarAccount()),
          form3.passwordModified(form("passwd"), trans.password())(autofocus, autocomplete := "off"),
          form3.actions(
            frag(
              a(href := routes.User.show(u.username))(changedMindDoNotCloseAccount()),
              form3.submit(
                closeAccount(),
                icon = "".some,
                confirm = closingIsDefinitive.txt().some
              )(cls := "button-red")
            )
          )
        )
      )
    }
}

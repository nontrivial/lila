package views.html

import controllers.routes
import play.api.i18n.Lang

import lila.api.Context
import lila.app.templating.Environment._
import lila.app.ui.ScalatagsTemplate._
import lila.hub.actorApi.timeline._

object timeline {

  def entries(entries: Vector[lila.timeline.Entry])(implicit ctx: Context) =
    div(cls := "entries")(
      filterEntries(entries) map { entry =>
        div(cls := "entry")(timeline.entry(entry))
      }
    )

  def more(entries: Vector[lila.timeline.Entry])(implicit ctx: Context) =
    views.html.base.layout(
      title = trans.timeline.txt(),
      moreCss = cssTag("slist")
    )(
      main(cls := "timeline page-small box")(
        h1(trans.timeline()),
        table(cls := "slist slist-pad")(
          tbody(
            filterEntries(entries) map { e =>
              tr(td(entry(e)))
            }
          )
        )
      )
    )

  private def filterEntries(entries: Vector[lila.timeline.Entry])(implicit ctx: Context) =
    if (ctx.noKid) entries
    else entries.filter(e => e.okForKid)

  private def userLink(userId: lila.user.User.ID)(implicit ctx: Context) =
    ctx.me match {
      case Some(me) if me.is(userId) => lightUserLink(me.light, withOnline = true)(ctx.lang)(cls := "online")
      case _                         => userIdLink(userId.some, withOnline = true)
    }

  private def entry(e: lila.timeline.Entry)(implicit ctx: Context) =
    frag(
      e.decode.map[Frag] {
        case Follow(u1, u2) =>
          trans.xStartedFollowingY(
            userLink(u1),
            userLink(u2)
          )
        case ForumPost(userId, _, topicName, postId) =>
          trans.xPostedInForumY(
            userLink(userId),
          )
        case TourJoin(userId, tourId, tourName) =>
          trans.xCompetesInY(
            userLink(userId),
            a(href := routes.Tournament.show(tourId))(tourName)
          )
        case GameEnd(playerId, opponent, win, perfKey) =>
          for {
            opponentId <- opponent
            perf       <- lila.rating.PerfType(perfKey)
          } yield (win match {
            case Some(true)  => trans.victoryVsYInZ
            case Some(false) => trans.defeatVsYInZ
            case None        => trans.drawVsYInZ
          })(
            a(
              href := routes.Round.player(playerId),
              dataIcon := perf.iconChar,
              cls := "text glpt"
            )(win match {
              case Some(true)  => trans.victory()
              case Some(false) => trans.defeat()
              case None        => trans.draw()
            }),
            userLink(opponentId),
            perf.trans
          )
        case PlanStart(userId) =>
          a(href := routes.Plan.index)(
            trans.patron.xBecamePatron(userLink(userId))
          )
        case PlanRenew(userId, months) =>
          a(href := routes.Plan.index)(
            trans.patron.xIsPatronForNbMonths
              .plural(months, userLink(userId), months)
          )
      },
      " ",
      momentFromNowWithPreload(e.date)
    )
}

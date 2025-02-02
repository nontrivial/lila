package views.html.user.show

import controllers.routes
import play.api.i18n.Lang

import lila.api.Context
import lila.app.templating.Environment._
import lila.app.ui.ScalatagsTemplate._
import lila.rating.PerfType
import lila.user.User

object side {

  def apply(
      u: User,
      rankMap: lila.rating.UserRankMap,
      active: Option[lila.rating.PerfType]
  )(implicit ctx: Context) = {

    def showNonEmptyPerf(perf: lila.rating.Perf, perfType: PerfType) =
      perf.nonEmpty option showPerf(perf, perfType)

    def showPerf(perf: lila.rating.Perf, perfType: PerfType) = {
      val isPuzzle = perfType == lila.rating.PerfType.Puzzle
      a(
        dataIcon := perfType.iconChar,
        title := perfType.desc,
        cls := List(
          "empty"  -> perf.isEmpty,
          "active" -> active.has(perfType)
        ),
        span(
          h3(perfType.trans),
          if (isPuzzle && u.perfs.dubiousPuzzle && !ctx.is(u)) st.rating("?")
          else
            st.rating(
              if (perf.glicko.clueless) strong("?")
              else
                strong(
                  perf.glicko.intRating,
                  perf.provisional option "?"
                ),
              " ",
              ratingProgress(perf.progress),
              " ",
              span(
                if (perfType.key == "puzzle") trans.nbPuzzles(perf.nb, perf.nb.localize)
                else trans.nbGames(perf.nb, perf.nb.localize)
              )
            ),
          rankMap get perfType map { rank =>
            span(cls := "rank", title := trans.rankIsUpdatedEveryNbMinutes.pluralSameTxt(15))(
              trans.rankX(rank.localize)
            )
          }
        ),
        iconTag("")
      )
    }

    div(cls := "side sub-ratings")(
      (!u.lame || ctx.is(u) || isGranted(_.UserModView)) option frag(
        showNonEmptyPerf(u.perfs.ultraBullet, PerfType.UltraBullet),
        showPerf(u.perfs.bullet, PerfType.Bullet),
        showPerf(u.perfs.blitz, PerfType.Blitz),
        showPerf(u.perfs.rapid, PerfType.Rapid),
        showPerf(u.perfs.classical, PerfType.Classical),
        showPerf(u.perfs.correspondence, PerfType.Correspondence),
        u.hasVariantRating option hr,
        showNonEmptyPerf(u.perfs.crazyhouse, PerfType.Crazyhouse),
        showNonEmptyPerf(u.perfs.chess960, PerfType.Chess960),
        showNonEmptyPerf(u.perfs.kingOfTheHill, PerfType.KingOfTheHill),
        showNonEmptyPerf(u.perfs.threeCheck, PerfType.ThreeCheck),
        showNonEmptyPerf(u.perfs.antichess, PerfType.Antichess),
        showNonEmptyPerf(u.perfs.atomic, PerfType.Atomic),
        showNonEmptyPerf(u.perfs.horde, PerfType.Horde),
        showNonEmptyPerf(u.perfs.racingKings, PerfType.RacingKings)
      )
    )
  }

}

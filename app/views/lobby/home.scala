package views.html.lobby

import controllers.routes
import play.api.libs.json.Json

import lila.api.Context
import lila.app.mashup.Preload.Homepage
import lila.app.templating.Environment._
import lila.app.ui.ScalatagsTemplate._
import lila.common.String.html.safeJsonValue
import lila.game.Pov

object home {

  def apply(homepage: Homepage)(implicit ctx: Context) = {
    import homepage._
    views.html.base.layout(
      title = "",
      fullTitle = Some {
        s"lichess.${if (netConfig.isProd) "org" else "dev"} • ${trans.freeOnlineChess.txt()}"
      },
      moreJs = frag(
        jsModule("lobby"),
        embedJsUnsafeLoadThen(
          s"""LichessLobby(${safeJsonValue(
            Json.obj(
              "data" -> data,
              "playban" -> playban.map { pb =>
                Json.obj(
                  "minutes"          -> pb.mins,
                  "remainingSeconds" -> (pb.remainingSeconds + 3)
                )
              },
              "i18n" -> i18nJsObject(i18nKeys)
            )
          )})"""
        )
      ),
      moreCss = cssTag("lobby"),
      chessground = false,
      openGraph = lila.app.ui
        .OpenGraph(
          image = assetUrl("logo/lichess-tile-wide.png").some,
          twitterImage = assetUrl("logo/lichess-tile.png").some,
          title = "The best free, adless Chess server",
          url = netBaseUrl,
          description = trans.siteDescription.txt()
        )
        .some
    ) {
      main(
        cls := List(
          "lobby"            -> true,
          "lobby-nope"       -> (playban.isDefined || currentGame.isDefined),
        )
      )(
        div(cls := "lobby__table")(
          div(cls := "bg-switch", title := "Dark mode")(
            div(cls := "bg-switch__track"),
            div(cls := "bg-switch__thumb")
          ),
          div(cls := "lobby__start")(
            ctx.blind option h2("Play"),
            a(
              href := routes.Setup.hookForm,
              cls := List(
                "button button-metal config_hook" -> true,
                "disabled"                        -> (playban.isDefined || currentGame.isDefined || ctx.isBot)
              ),
              trans.createAGame()
            ),
            a(
              href := routes.Setup.friendForm(none),
              cls := List(
                "button button-metal config_friend" -> true,
                "disabled"                          -> currentGame.isDefined
              ),
              trans.playWithAFriend()
            )
          ),
          div(cls := "lobby__counters")(
            ctx.blind option h2("Counters"),
            a(
              id := "nb_connected_players",
              href := ctx.noBlind.option(routes.User.list.url)
            )(
              trans.nbPlayers(
                strong(dataCount := homepage.counters.members)(homepage.counters.members.localize)
              )
            )
          )
        ),
        currentGame.map(bits.currentGameInfo) orElse
          playban.map(bits.playbanInfo) getOrElse {
            bits.lobbyApp
          },
        div(cls := "lobby__side")(
          ctx.blind option h2("Highlights"),
          if (ctx.isAuth)
            div(cls := "timeline")(
              ctx.blind option h2("Timeline"),
              views.html.timeline entries userTimeline,
              userTimeline.nonEmpty option a(cls := "more", href := routes.Timeline.home)(
                trans.more(),
                " »"
              )
            )
        ),
        featured map { g =>
          div(cls := "lobby__tv")(
            views.html.game.mini(Pov naturalOrientation g, tv = true)
          )
        },
        ctx.noBot option bits.underboards(leaderboard),
      )
    }
  }

  private val i18nKeys = List(
    trans.realTime,
    trans.correspondence,
    trans.nbGamesInPlay,
    trans.player,
    trans.time,
    trans.joinTheGame,
    trans.cancel,
    trans.casual,
    trans.rated,
    trans.variant,
    trans.mode,
    trans.list,
    trans.graph,
    trans.filterGames,
    trans.youNeedAnAccountToDoThat,
    trans.oneDay,
    trans.nbDays,
    trans.aiNameLevelAiLevel,
    trans.yourTurn,
    trans.rating,
    trans.createAGame,
    trans.quickPairing,
    trans.lobby,
    trans.custom,
    trans.anonymous
  ).map(_.key)
}

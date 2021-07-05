package lila.api

import akka.actor._
import com.softwaremill.macwire._
import play.api.libs.ws.StandaloneWSClient
import play.api.{ Configuration, Mode }
import scala.concurrent.duration._

import lila.chat.GetLinkCheck
import lila.common.Bus
import lila.common.config._
import lila.hub.actorApi.Announce
import lila.user.User

@Module
final class Env(
    appConfig: Configuration,
    net: NetConfig,
    securityEnv: lila.security.Env,
    mailerEnv: lila.mailer.Env,
    explorerEnv: lila.explorer.Env,
    planEnv: lila.plan.Env,
    gameEnv: lila.game.Env,
    chatEnv: lila.chat.Env,
    roundEnv: lila.round.Env,
    bookmarkApi: lila.bookmark.BookmarkApi,
    prefApi: lila.pref.PrefApi,
    playBanApi: lila.playban.PlaybanApi,
    userEnv: lila.user.Env,
    relationEnv: lila.relation.Env,
    lobbyEnv: lila.lobby.Env,
    tourEnv: lila.tournament.Env,
    challengeEnv: lila.challenge.Env,
    socketEnv: lila.socket.Env,
    msgEnv: lila.msg.Env,
    cacheApi: lila.memo.CacheApi,
    mongoCacheApi: lila.memo.MongoCache.Api,
    ws: StandaloneWSClient,
    val mode: Mode
)(implicit
    ec: scala.concurrent.ExecutionContext,
    system: ActorSystem
) {

  val config = ApiConfig loadFrom appConfig
  import config.{ apiToken, pagerDuty => pagerDutyConfig }
  import net.{ baseUrl, domain }

  lazy val pgnDump: PgnDump = wire[PgnDump]

  lazy val userApi = wire[UserApi]

  lazy val gameApi = wire[GameApi]

  lazy val realPlayers = wire[RealPlayerApi]

  lazy val gameApiV2 = wire[GameApiV2]

  lazy val userGameApi = wire[UserGameApi]

  lazy val roundApi = wire[RoundApi]

  lazy val lobbyApi = wire[LobbyApi]

  lazy val eventStream = wire[EventStream]

  lazy val personalDataExport = wire[PersonalDataExport]

  lazy val referrerRedirect = wire[ReferrerRedirect]

  private lazy val influxEvent = new InfluxEvent(
    ws = ws,
    endpoint = config.influxEventEndpoint,
    env = config.influxEventEnv
  )
  if (mode == Mode.Prod) system.scheduler.scheduleOnce(5 seconds)(influxEvent.start())

  private lazy val pagerDuty = wire[PagerDuty]

  system.scheduler.scheduleWithFixedDelay(1 minute, 1 minute) { () =>
    lila.mon.bus.classifiers.update(lila.common.Bus.size)
    // ensure the Lichess user is online
    socketEnv.remoteSocket.onlineUserIds.getAndUpdate(_ + User.lichessId)
    userEnv.repo.setSeenAt(User.lichessId)
  }
}

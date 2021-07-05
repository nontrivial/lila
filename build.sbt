import com.typesafe.sbt.packager.Keys.scriptClasspath

import BuildSettings._
import Dependencies._

lazy val root = Project("lila", file("."))
  .enablePlugins(PlayScala, if (useEpoll) PlayNettyServer else PlayAkkaHttpServer)
  .disablePlugins(if (useEpoll) PlayAkkaHttpServer else PlayNettyServer)
  .dependsOn(api)
  .aggregate(api)
  .settings(buildSettings)

// shorter prod classpath
scriptClasspath := Seq("*")
maintainer := "contact@lichess.org"
Compile / resourceDirectory := baseDirectory.value / "conf"

// format: off
libraryDependencies ++= akka.bundle ++ playWs.bundle ++ Seq(
  macwire.macros, macwire.util, play.json, jodaForms,
  chess, compression, scalalib, hasher,
  reactivemongo.driver, reactivemongo.kamon, maxmind, prismic, scalatags,
  kamon.core, kamon.influxdb, kamon.metrics, kamon.prometheus,
  scrimage, scaffeine, lettuce, uaparser
) ++ {
  if (useEpoll) Seq(epoll, reactivemongo.epoll)
  else Seq.empty
}

lazy val modules = Seq(
  common, db, rating, user, security, hub, socket,
  msg, notifyModule, i18n, game, bookmark, search,
  gameSearch, timeline,
  round, pool, lobby, setup,
  importer, relation, pref,
  chat, coordinate,
  history, shutup, push, mailer,
  playban, perfStat, irc, quote, challenge,
  explorer,
  event
)

lazy val moduleRefs = modules map projectToRef
lazy val moduleCPDeps = moduleRefs map { sbt.ClasspathDependency(_, None) }

lazy val api = module("api",
  moduleCPDeps,
  Seq(play.api, play.json, hasher, kamon.core, kamon.influxdb, lettuce, specs2) ++ reactivemongo.bundle
).settings(
  Runtime / aggregate := false,
  Test / aggregate := true  // Test <: Runtime
) aggregate (moduleRefs: _*)

lazy val i18n = smallModule("i18n",
  Seq(common, db, hub),
  Seq(scalatags, specs2)
).settings(
  Compile / sourceGenerators += Def.task {
    MessageCompiler(
      sourceDir = new File("translation/source"),
      destDir = new File("translation/dest"),
      dbs = "site arena emails coordinates contact patron broadcast tfa settings preferences perfStat search lag challenge".split(' ').toList,
      compileTo = (Compile / sourceManaged).value
    )
  }.taskValue
)

lazy val quote = smallModule("quote",
  Seq(),
  Seq(play.json)
)

lazy val coordinate = smallModule("coordinate",
  Seq(common, db, user),
  Seq(autoconfig) ++ reactivemongo.bundle ++ macwire.bundle
)

lazy val common = smallModule("common",
  Seq(),
  Seq(
    scalalib, scalaUri, chess, autoconfig,
    kamon.core, scalatags, jodaForms, scaffeine, specs2, apacheText
  ) ++ reactivemongo.bundle ++ flexmark.bundle
)

lazy val rating = module("rating",
  Seq(common, db, memo, i18n),
  reactivemongo.bundle
)

lazy val perfStat = module("perfStat",
  Seq(common, db, user, game, rating),
  reactivemongo.bundle
)

lazy val history = module("history",
  Seq(common, db, memo, game, user),
  Seq(scalatags) ++ reactivemongo.bundle
)

lazy val db = smallModule("db",
  Seq(common),
  Seq(hasher, scrimage) ++ reactivemongo.bundle
)

lazy val memo = smallModule("memo",
  Seq(common, db),
  Seq(scaffeine, autoconfig, scalatest, akka.testkit) ++ reactivemongo.bundle ++ macwire.bundle
)

lazy val search = smallModule("search",
  Seq(common, hub),
  playWs.bundle ++ Seq(autoconfig) ++ macwire.bundle
)

lazy val chat = module("chat",
  Seq(common, db, user, security, i18n, socket),
  Seq(scalatags) ++ reactivemongo.bundle
)

lazy val room = module("room",
  Seq(common, socket, chat),
  Seq(lettuce) ++ reactivemongo.bundle
)

lazy val timeline = module("timeline",
  Seq(common, db, game, user, hub, security, relation),
  reactivemongo.bundle
)

lazy val event = module("event",
  Seq(common, db, memo, i18n, user),
  Seq(scalatags) ++ reactivemongo.bundle
)

lazy val user = smallModule("user",
  Seq(common, memo, db, hub, rating, socket),
  Seq(hasher, specs2, autoconfig, scalaUri) ++ playWs.bundle ++ reactivemongo.bundle ++ macwire.bundle
)

lazy val game = module("game",
  Seq(common, memo, db, hub, user, chat),
  Seq(compression, specs2) ++ reactivemongo.bundle
)

lazy val gameSearch = module("gameSearch",
  Seq(common, hub, search, game),
  reactivemongo.bundle
)

lazy val round = module("round",
  Seq(common, db, memo, hub, socket, game, user, i18n, pref, chat, history, playban, room, irc),
  Seq(scalatags, hasher, kamon.core, lettuce) ++ reactivemongo.bundle
)

lazy val pool = module("pool",
  Seq(common, game, user, playban),
  reactivemongo.bundle
)

lazy val lobby = module("lobby",
  Seq(common, db, memo, hub, socket, game, user, round, timeline, relation, playban, security, pool),
  Seq(lettuce) ++ reactivemongo.bundle
)

lazy val setup = module("setup",
  Seq(common, db, memo, hub, socket, game, user, lobby, pref, relation, oauth),
  reactivemongo.bundle
)

lazy val importer = module("importer",
  Seq(common, game, round),
  reactivemongo.bundle
)

lazy val oauth = smallModule("oauth",
  Seq(common, db, user),
  Seq(autoconfig) ++ reactivemongo.bundle ++ macwire.bundle
)

lazy val security = module("security",
  Seq(common, hub, db, user, i18n, irc, oauth, mailer),
  Seq(maxmind, hasher, uaparser, specs2) ++ reactivemongo.bundle
)

lazy val shutup = module("shutup",
  Seq(common, db, hub, game, relation),
  Seq(specs2) ++ reactivemongo.bundle
)

lazy val challenge = module("challenge",
  Seq(common, db, hub, setup, game, relation, pref, socket, room, msg),
  Seq(scalatags, lettuce, specs2) ++ reactivemongo.bundle
)

lazy val playban = module("playban",
  Seq(common, db, game, msg, chat),
  reactivemongo.bundle
)

lazy val push = module("push",
  Seq(common, db, user, game, challenge, msg),
  Seq(googleOAuth) ++ reactivemongo.bundle
)

lazy val irc = smallModule("irc",
  Seq(common, hub, user),
  Seq(autoconfig) ++ reactivemongo.bundle ++ macwire.bundle
)

lazy val mailer = module("mailer",
  Seq(common, user),
  reactivemongo.bundle ++ Seq(scalatags, hasher, play.mailer)
)

lazy val relation = module("relation",
  Seq(common, db, memo, hub, user, game, pref),
  reactivemongo.bundle
)

lazy val pref = module("pref",
  Seq(common, db, user),
  Seq(macwire.util) ++ reactivemongo.bundle
)

lazy val msg = module("msg",
  Seq(common, db, user, hub, relation, security, shutup, notifyModule, chat),
  reactivemongo.bundle
)

lazy val bookmark = module("bookmark",
  Seq(common, memo, db, hub, user, game),
  reactivemongo.bundle
)

lazy val report = module("report",
  Seq(common, db, user, game, security, playban),
  reactivemongo.bundle
)

lazy val explorer = module("explorer",
  Seq(common, db, game, importer),
  reactivemongo.bundle
)

lazy val notifyModule = module("notify",
  Seq(common, db, game, user, hub, relation),
  reactivemongo.bundle
)

lazy val tree = smallModule("tree",
  Seq(common),
  Seq()
)

lazy val socket = smallModule("socket",
  Seq(common, hub, memo, tree),
  Seq(lettuce) ++ macwire.bundle
)

lazy val hub = smallModule("hub",
  Seq(common),
  Seq(scaffeine, macwire.util)
)

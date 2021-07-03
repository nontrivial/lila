package lila.activity

import akka.actor._
import com.softwaremill.macwire._
import scala.concurrent.duration._

import lila.common.config._
import lila.hub.actorApi.round.CorresMoveEvent

final class Env(
    db: lila.db.Db,
    gameRepo: lila.game.GameRepo,
    postApi: lila.forum.PostApi,
    tourLeaderApi: lila.tournament.LeaderboardApi,
    getTourName: lila.tournament.GetTourName
)(implicit
    ec: scala.concurrent.ExecutionContext,
    system: ActorSystem
) {

  private lazy val coll = db(CollName("activity"))

  lazy val write: ActivityWriteApi = wire[ActivityWriteApi]

  lazy val read: ActivityReadApi = wire[ActivityReadApi]

  lazy val jsonView = wire[JsonView]

  lila.common.Bus.subscribeFuns(
    "finishGame" -> {
      case lila.game.actorApi.FinishGame(game, _, _) if !game.aborted => write.game(game).unit
    },
    "stormRun" -> { case lila.hub.actorApi.puzzle.StormRun(userId, score) =>
      write.storm(userId, score).unit
    },
    "racerRun" -> { case lila.hub.actorApi.puzzle.RacerRun(userId, score) =>
      write.racer(userId, score).unit
    },
    "streakRun" -> { case lila.hub.actorApi.puzzle.StreakRun(userId, score) =>
      write.streak(userId, score).unit
    }
  )

  lila.common.Bus.subscribeFun(
    "forumPost",
    "finishPractice",
    "team",
    "startSimul",
    "moveEventCorres",
    "plan",
    "relation",
    "startStudy",
    "streamStart",
    "swissFinish"
  ) {
    case lila.forum.actorApi.CreatePost(post)             => write.forumPost(post).unit
    case CorresMoveEvent(move, Some(userId), _, _, false) => write.corresMove(move.gameId, userId).unit
    case lila.hub.actorApi.plan.MonthInc(userId, months)  => write.plan(userId, months).unit
    case lila.hub.actorApi.relation.Follow(from, to)      => write.follow(from, to).unit
    case lila.hub.actorApi.team.CreateTeam(id, _, userId) => write.team(id, userId).unit
    case lila.hub.actorApi.team.JoinTeam(id, userId)      => write.team(id, userId).unit
    case lila.hub.actorApi.streamer.StreamStart(userId)   => write.streamStart(userId).unit
  }
}

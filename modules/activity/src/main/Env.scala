package lila.activity

import akka.actor._
import com.softwaremill.macwire._
import scala.concurrent.duration._

import lila.common.config._
import lila.hub.actorApi.round.CorresMoveEvent

final class Env(
    db: lila.db.Db,
    gameRepo: lila.game.GameRepo
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
    "streakRun" -> { case lila.hub.actorApi.puzzle.StreakRun(userId, score) =>
      write.streak(userId, score).unit
    }
  )

  lila.common.Bus.subscribeFun(
    "moveEventCorres",
    "relation"
  ) {
    case CorresMoveEvent(move, Some(userId), _, _, false) => write.corresMove(move.gameId, userId).unit
    case lila.hub.actorApi.relation.Follow(from, to)      => write.follow(from, to).unit
  }
}

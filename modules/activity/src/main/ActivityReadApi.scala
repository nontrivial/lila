package lila.activity

import org.joda.time.{ DateTime, Interval }
import reactivemongo.api.ReadPreference

import lila.common.Heapsort
import lila.db.dsl._
import lila.game.LightPov
import lila.user.User
import lila.tournament.LeaderboardApi

final class ActivityReadApi(
    coll: Coll,
    gameRepo: lila.game.GameRepo,
    tourLeaderApi: lila.tournament.LeaderboardApi,
)(implicit ec: scala.concurrent.ExecutionContext) {

  import BSONHandlers._
  import model._

  implicit private val ordering = scala.math.Ordering.Double.TotalOrdering

  private val recentNb = 7

  def recent(u: User, nb: Int = recentNb): Fu[Vector[ActivityView]] =
    for {
      activities <-
        coll
          .find(regexId(u.id))
          .sort($sort desc "_id")
          .cursor[Activity](ReadPreference.secondaryPreferred)
          .vector(nb)
          .dmap(_.filterNot(_.isEmpty))
          .mon(_.user segment "activity.raws")
      views <- activities.map { a =>
        one(a).mon(_.user segment "activity.view")
      }.sequenceFu
    } yield addSignup(u.createdAt, views)

  private def one(a: Activity): Fu[ActivityView] =
    for {
      corresMoves <- a.corres ?? { corres =>
        getLightPovs(a.id.userId, corres.movesIn) dmap {
          _.map(corres.moves -> _)
        }
      }
      corresEnds <- a.corres ?? { corres =>
        getLightPovs(a.id.userId, corres.end) dmap {
          _.map { povs =>
            Score.make(povs) -> povs
          }
        }
      }
      tours <- a.games.exists(_.hasNonCorres) ?? {
        val dateRange = a.date -> a.date.plusDays(1)
        tourLeaderApi
          .timeRange(a.id.userId, dateRange)
          .dmap { entries =>
            entries.nonEmpty option ActivityView.Tours(
              nb = entries.size,
              best = Heapsort.topN(
                entries,
                activities.maxSubEntries,
                Ordering.by[LeaderboardApi.Entry, Double](-_.rankRatio.value)
              )
            )
          }
          .mon(_.user segment "activity.tours")
      }

    } yield ActivityView(
      interval = a.interval,
      games = a.games,
      puzzles = a.puzzles,
      storm = a.storm,
      racer = a.racer,
      streak = a.streak,
      patron = a.patron,
      corresMoves = corresMoves,
      corresEnds = corresEnds,
      follows = a.follows,
      tours = tours
    )

  private def addSignup(at: DateTime, recent: Vector[ActivityView]) = {
    val (found, views) = recent.foldLeft(false -> Vector.empty[ActivityView]) {
      case ((false, as), a) if a.interval contains at => (true, as :+ a.copy(signup = true))
      case ((found, as), a)                           => (found, as :+ a)
    }
    if (!found && views.sizeIs < recentNb && DateTime.now.minusDays(8).isBefore(at))
      views :+ ActivityView(
        interval = new Interval(at.withTimeAtStartOfDay, at.withTimeAtStartOfDay plusDays 1),
        signup = true
      )
    else views
  }

  private def getLightPovs(userId: User.ID, gameIds: List[GameId]): Fu[Option[List[LightPov]]] =
    gameIds.nonEmpty ?? {
      gameRepo.light.gamesFromSecondary(gameIds.map(_.value)).dmap {
        _.flatMap { LightPov.ofUserId(_, userId) }.some.filter(_.nonEmpty)
      }
    }
}

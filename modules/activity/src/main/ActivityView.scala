package lila.activity

import org.joda.time.Interval

import lila.game.LightPov
import lila.practice.PracticeStudy
import lila.simul.Simul
import lila.study.Study
import lila.tournament.LeaderboardApi.{ Entry => TourEntry }

import activities._
import model._
import lila.swiss.Swiss

case class ActivityView(
    interval: Interval,
    games: Option[Games] = None,
    puzzles: Option[Puzzles] = None,
    storm: Option[Storm] = None,
    racer: Option[Racer] = None,
    streak: Option[Streak] = None,
    patron: Option[Patron] = None,
    corresMoves: Option[(Int, List[LightPov])] = None,
    corresEnds: Option[(Score, List[LightPov])] = None,
    follows: Option[Follows] = None,
    teams: Option[Teams] = None,
    tours: Option[ActivityView.Tours] = None,
    stream: Boolean = false,
    signup: Boolean = false
)

object ActivityView {

  case class Tours(
      nb: Int,
      best: List[TourEntry]
  )
}

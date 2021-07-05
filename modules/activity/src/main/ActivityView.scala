package lila.activity

import org.joda.time.Interval

import lila.game.LightPov

import activities._
import model._

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
    stream: Boolean = false,
    signup: Boolean = false
)

object ActivityView {


}

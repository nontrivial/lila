package lila.activity

import model._
import ornicar.scalalib.Zero

import lila.rating.PerfType
import lila.user.User

object activities {

  val maxSubEntries = 15

  case class Games(value: Map[PerfType, Score]) extends AnyVal {
    def add(pt: PerfType, score: Score) =
      copy(
        value = value + (pt -> value.get(pt).fold(score)(_ add score))
      )
    def hasNonCorres = value.exists(_._1 != PerfType.Correspondence)
  }
  implicit val GamesZero = Zero.instance(Games(Map.empty))
  case class Streak(runs: Int, score: Int) {
    def +(s: Int) = Streak(runs = runs + 1, score = score atLeast s)
  }
  implicit val StreakZero = Zero.instance(Streak(0, 0))

  case class Corres(moves: Int, movesIn: List[GameId], end: List[GameId]) {
    def add(gameId: GameId, moved: Boolean, ended: Boolean) =
      Corres(
        moves = moves + (moved ?? 1),
        movesIn = if (moved) (gameId :: movesIn).distinct.take(maxSubEntries) else movesIn,
        end = if (ended) (gameId :: end).take(maxSubEntries) else end
      )
  }
  implicit val CorresZero = Zero.instance(Corres(0, Nil, Nil))

  case class FollowList(ids: List[User.ID], nb: Option[Int]) {
    def actualNb = nb | ids.size
    def +(id: User.ID) =
      if (ids contains id) this
      else {
        val newIds = (id :: ids).distinct
        copy(
          ids = newIds take maxSubEntries,
          nb = nb.map(1 +).orElse(newIds.size > maxSubEntries option newIds.size)
        )
      }
    def isEmpty = ids.isEmpty
  }
  implicit val FollowListZero = Zero.instance(FollowList(Nil, None))
  implicit val FollowsZero    = Zero.instance(Follows(None, None))

  case class Follows(in: Option[FollowList], out: Option[FollowList]) {
    def addIn(id: User.ID)  = copy(in = Some(~in + id))
    def addOut(id: User.ID) = copy(out = Some(~out + id))
    def isEmpty             = in.fold(true)(_.isEmpty) && out.fold(true)(_.isEmpty)
  }


}

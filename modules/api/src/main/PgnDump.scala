package lila.api

import chess.format.FEN
import chess.format.pgn.Pgn
import lila.game.Game
import lila.game.PgnDump.WithFlags

final class PgnDump(
    val dumper: lila.game.PgnDump,
    getTournamentName: lila.tournament.GetTourName
)(implicit ec: scala.concurrent.ExecutionContext) {

  implicit private val lang = lila.i18n.defaultLang

  def apply(
      game: Game,
      initialFen: Option[FEN],
      flags: WithFlags,
      realPlayers: Option[RealPlayers] = None
  ): Fu[Pgn] =
    dumper(game, initialFen, flags) flatMap { pgn =>
      fuccess(pgn)
    } map { pgn =>
      realPlayers.fold(pgn)(_.update(game, pgn))
    }

  def formatter(flags: WithFlags) =
    (
        game: Game,
        initialFen: Option[FEN],
        realPlayers: Option[RealPlayers]
    ) => apply(game, initialFen, flags, realPlayers) dmap toPgnString

  def toPgnString(pgn: Pgn) = {
    // merge analysis & eval comments
    // 1. e4 { [%eval 0.17] } { [%clk 0:00:30] }
    // 1. e4 { [%eval 0.17] [%clk 0:00:30] }
    s"$pgn\n\n\n".replaceIf("] } { [", "] [")
  }
}

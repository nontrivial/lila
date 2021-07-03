package lila.api

import chess.format.FEN
import chess.format.pgn.Pgn
import lila.analyse.{ Analysis, Annotator }
import lila.game.Game
import lila.game.PgnDump.WithFlags

final class PgnDump(
    val dumper: lila.game.PgnDump,
    annotator: Annotator,
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
      val evaled = analysis.ifTrue(flags.evals).fold(pgn)(addEvals(pgn, _))
      if (flags.literate) annotator(evaled, game, analysis)
      else evaled
    } map { pgn =>
      realPlayers.fold(pgn)(_.update(game, pgn))
    }

  private def addEvals(p: Pgn, analysis: Analysis): Pgn =
    analysis.infos.foldLeft(p) { case (pgn, info) =>
      pgn.updateTurn(
        info.turn,
        turn =>
          turn.update(
            info.color,
            move => {
              val comment = info.cp
                .map(_.pawns.toString)
                .orElse(info.mate.map(m => s"#${m.value}"))
              move.copy(
                comments = comment.map(c => s"[%eval $c]").toList ::: move.comments
              )
            }
          )
      )
    }

  def formatter(flags: WithFlags) =
    (
        game: Game,
        initialFen: Option[FEN],
        analysis: Option[Analysis],
        realPlayers: Option[RealPlayers]
    ) => apply(game, initialFen, analysis, flags, realPlayers) dmap toPgnString

  def toPgnString(pgn: Pgn) = {
    // merge analysis & eval comments
    // 1. e4 { [%eval 0.17] } { [%clk 0:00:30] }
    // 1. e4 { [%eval 0.17] [%clk 0:00:30] }
    s"$pgn\n\n\n".replaceIf("] } { [", "] [")
  }
}

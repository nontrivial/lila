package lila.api

import cats.implicits._
import scala.concurrent.ExecutionContext

import lila.chat.UserLine
import lila.common.config.NetDomain
import lila.hub.actorApi.shutup.PublicSource
import lila.tournament.Tournament
import lila.tournament.TournamentRepo
import lila.user.User

/* Determine if a link to a lichess resource
 * can be posted from another lichess resource.
 * Owners of a resource can post any link on it (but not to it).
 * Links to a team resource can be posted from another resource of the same team.
 * Links to official resources can be posted from anywhere.
 * */
final private class LinkCheck(
    domain: NetDomain,
    tournamentRepo: TournamentRepo,
)(implicit ec: ExecutionContext) {

  import LinkCheck._

  def apply(line: UserLine, source: PublicSource): Fu[Boolean] =
    if (multipleLinks find line.text) fuFalse
    else
      line.text match {
        case tournamentLinkR(id) => withSource(source, tourLink)(id, line)
        case _                   => fuTrue
      }

  private def withSource(
      source: PublicSource,
      f: (String, FullSource) => Fu[Boolean]
  )(id: String, line: UserLine): Fu[Boolean] = {
    source match {
      case PublicSource.Tournament(id) => tournamentRepo byId id map2 FullSource.TournamentSource
      case _                           => fuccess(none)
    }
  } flatMap {
    _ ?? { source =>
      // the owners of a chat can post whichever link they like
      if (source.owners(line.userId)) fuTrue
      else f(id, source)
    }
  }

  private def tourLink(tourId: Tournament.ID, source: FullSource) =
    tournamentRepo byId tourId flatMap {
      _ ?? { tour =>
        fuccess(tour.isScheduled) >>| {
          source.teamId ?? { sourceTeamId =>
            fuccess(tour.conditions.teamMember.exists(_.teamId == sourceTeamId)) >>|
              tournamentRepo.isForTeam(tour.id, sourceTeamId)
          }
        }
      }
    }

  private def studyLink(studyId: String, source: FullSource) = fuFalse

  private def teamLink(teamId: String, source: FullSource) = fuFalse

  private val multipleLinks   = s"(?i)$domain.+$domain".r.unanchored
  private val tournamentLinkR = s"(?i)$domain/tournament/(\\w+)".r.unanchored
}

private object LinkCheck {

  sealed trait FullSource {
    def owners: Set[User.ID]
  }

  object FullSource {
    case class TournamentSource(value: Tournament) extends FullSource {
      def owners = Set(value.createdBy)
    }
  }
}

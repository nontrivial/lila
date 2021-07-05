package lila.activity

import org.joda.time.Interval
import play.api.i18n.Lang
import play.api.libs.json._

import lila.common.Iso
import lila.common.Json._
import lila.game.JsonView.colorWrites
import lila.game.LightPov
import lila.rating.PerfType
import lila.user.User

import activities._
import model._

final class JsonView(
) {

  private object Writers {
    implicit val intervalWrites = OWrites[Interval] { i =>
      Json.obj("start" -> i.getStart, "end" -> i.getEnd)
    }
    implicit val perfTypeWrites   = Writes[PerfType](pt => JsString(pt.key))
    implicit val ratingWrites     = intIsoWriter(Iso.int[Rating](Rating.apply, _.value))
    implicit val ratingProgWrites = Json.writes[RatingProg]
    implicit val scoreWrites      = Json.writes[Score]
    implicit val gamesWrites = OWrites[Games] { games =>
      JsObject {
        games.value.toList.sortBy(-_._2.size).map { case (pt, score) =>
          pt.key -> scoreWrites.writes(score)
        }
      }
    }
    implicit val variantWrites: Writes[chess.variant.Variant] = Writes { v =>
      JsString(v.key)
    }

    implicit val streakWrites                     = Json.writes[Streak]
    implicit val playerWrites = OWrites[lila.game.Player] { p =>
      Json
        .obj()
        .add("user" -> p.userId)
        .add("rating" -> p.rating)
    }
    implicit val lightPovWrites = OWrites[LightPov] { p =>
      Json.obj(
        "id"       -> p.game.id,
        "color"    -> p.color,
        "url"      -> s"/${p.game.id}/${p.color.name}",
        "opponent" -> p.opponent
      )
    }
    implicit val followListWrites = Json.writes[FollowList]
    implicit val followsWrites    = Json.writes[Follows]
    implicit val patronWrites = Json.writes[Patron]
  }
  import Writers._

  def apply(a: ActivityView, user: User)(implicit lang: Lang): Fu[JsObject] =
    fuccess {
      Json
        .obj("interval" -> a.interval)
        .add("games", a.games)
        .add(
          "correspondenceMoves",
          a.corresMoves.map { case (nb, povs) =>
            Json.obj("nb" -> nb, "games" -> povs)
          }
        )
        .add(
          "correspondenceEnds",
          a.corresEnds.map { case (score, povs) =>
            Json.obj("score" -> score, "games" -> povs)
          }
        )
        .add("follows" -> a.follows)
    }
}

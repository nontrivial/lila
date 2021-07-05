package views.html.user

import controllers.routes
import play.api.i18n.Lang

import lila.api.Context
import lila.app.templating.Environment._
import lila.app.ui.ScalatagsTemplate._
import lila.playban.RageSit
import lila.security.Granter
import lila.security.{ Permission, UserLogins }
import lila.user.{ Holder, User }

object mod {
  private def mzSection(key: String) =
    div(cls := s"mz-section mz-section--$key", dataRel := key, id := s"mz_$key")

  def menu =
    mzSection("menu")(
      a(href := "#mz_actions")("Overview"),
      a(href := "#mz_irwin")("Irwin"),
      a(href := "#mz_assessments")("Evaluation"),
      a(href := "#mz_mod_log")("Mod log"),
      a(href := "#mz_reports")("Reports"),
      a(href := "#identification_screen")("Identification")
    )

  def actions(u: User, emails: User.Emails, erased: User.Erased)(implicit
      ctx: Context
  ): Frag =
    mzSection("actions")(
      div(cls := "btn-rack")(
        isGranted(_.ModMessage) option {
          postForm(action := routes.Mod.spontaneousInquiry(u.username), title := "Start an inquiry")(
            submitButton(cls := "btn-rack__btn inquiry", title := "Hotkey: i")(i)
          )
        },
        isGranted(_.UserEvaluate) option {
          postForm(
            action := routes.Mod.refreshUserAssess(u.username),
            title := "Collect data and ask irwin",
            cls := "xhr"
          )(
            submitButton(cls := "btn-rack__btn")("Evaluate")
          )
        },
        isGranted(_.Hunter) option {
          a(
            cls := "btn-rack__btn",
            href := routes.GameMod.index(u.username),
            title := "View games"
          )("Games")
        },
        isGranted(_.Shadowban) option {
          a(
            cls := "btn-rack__btn",
            href := routes.Mod.communicationPublic(u.id),
            title := "View communications"
          )("Comms")
        },
        isGranted(_.NotifySlack) option {
          postForm(action := routes.Mod.notifySlack(u.id), title := "Notify #tavern", cls := "xhr")(
            submitButton(cls := "btn-rack__btn")("Slack")
          )
        }
      ),
      div(cls := "btn-rack")(
        isGranted(_.CloseAccount) option {
          postForm(
            action := routes.Mod.alt(u.username, !u.marks.alt),
            title := "Preemptively close unauthorized alt.",
            cls := "xhr"
          )(
            submitButton(cls := List("btn-rack__btn" -> true, "active" -> u.marks.alt))("Alt")
          )
        },
        isGranted(_.MarkEngine) option {
          postForm(
            action := routes.Mod.engine(u.username, !u.marks.engine),
            title := "This user is clearly cheating.",
            cls := "xhr"
          )(
            submitButton(cls := List("btn-rack__btn" -> true, "active" -> u.marks.engine))("Engine")
          )
        },
        isGranted(_.MarkBooster) option {
          postForm(
            action := routes.Mod.booster(u.username, !u.marks.boost),
            title := "Marks the user as a booster or sandbagger.",
            cls := "xhr"
          )(
            submitButton(cls := List("btn-rack__btn" -> true, "active" -> u.marks.boost))("Booster")
          )
        },
        isGranted(_.Shadowban) option {
          postForm(
            action := routes.Mod.troll(u.username, !u.marks.troll),
            title := "Enable/disable communication features for this user.",
            cls := "xhr"
          )(
            submitButton(cls := List("btn-rack__btn" -> true, "active" -> u.marks.troll))("Shadowban")
          )
        },
        (u.marks.troll && isGranted(_.Shadowban)) option {
          postForm(
            action := routes.Mod.deletePmsAndChats(u.username),
            title := "Delete all PMs and public chat messages",
            cls := "xhr"
          )(
            submitButton(cls := "btn-rack__btn confirm")("Clear PMs & chats")
          )
        },
        isGranted(_.SetKidMode) option {
          postForm(
            action := routes.Mod.kid(u.username),
            title := "Activate kid mode if not already the case",
            cls := "xhr"
          )(
            submitButton(cls := "btn-rack__btn confirm", cls := u.kid.option("active"))("Kid")
          )
        },
        isGranted(_.RemoveRanking) option {
          postForm(
            action := routes.Mod.rankban(u.username, !u.marks.rankban),
            title := "Include/exclude this user from the rankings.",
            cls := "xhr"
          )(
            submitButton(cls := List("btn-rack__btn" -> true, "active" -> u.marks.rankban))("Rankban")
          )
        },
        isGranted(_.ReportBan) option {
          postForm(
            action := routes.Mod.reportban(u.username, !u.marks.reportban),
            title := "Enable/disable the report feature for this user.",
            cls := "xhr"
          )(
            submitButton(cls := List("btn-rack__btn" -> true, "active" -> u.marks.reportban))("Reportban")
          )
        }
      ),
      div(cls := "btn-rack")(
        if (u.enabled) {
          isGranted(_.CloseAccount) option {
            postForm(
              action := routes.Mod.closeAccount(u.username),
              title := "Disables this account.",
              cls := "xhr"
            )(
              submitButton(cls := "btn-rack__btn")("Close")
            )
          }
        } else if (erased.value) {
          "Erased"
        } else {
          isGranted(_.CloseAccount) option {
            postForm(
              action := routes.Mod.reopenAccount(u.username),
              title := "Re-activates this account.",
              cls := "xhr"
            )(
              submitButton(cls := "btn-rack__btn active")("Closed")
            )
          }
        }
      ),
      div(cls := "btn-rack")(
        (u.totpSecret.isDefined && isGranted(_.DisableTwoFactor)) option {
          postForm(
            action := routes.Mod.disableTwoFactor(u.username),
            title := "Disables two-factor authentication for this account.",
            cls := "xhr"
          )(
            submitButton(cls := "btn-rack__btn confirm")("Disable 2FA")
          )
        },
        (isGranted(_.Impersonate) || (isGranted(_.Admin) && u.id == "lichess")) option {
          postForm(action := routes.Mod.impersonate(u.username))(
            submitButton(cls := "btn-rack__btn")("Impersonate")
          )
        }
      ),
      isGranted(_.SetTitle) option {
        postForm(cls := "fide-title", action := routes.Mod.setTitle(u.username))(
          form3.select(
            lila.user.UserForm.title.fill(u.title.map(_.value))("title"),
            lila.user.Title.acronyms.map(t => t -> t),
            "No title".some
          )
        )
      },
      (isGranted(_.Admin) && isGranted(_.SetEmail)) ?? frag(
        postForm(cls := "email", action := routes.Mod.setEmail(u.username))(
          st.input(
            tpe := "email",
            value := emails.current.??(_.value),
            name := "email",
            placeholder := "Email address"
          ),
          submitButton(cls := "button", dataIcon := "")
        ),
        emails.previous.map { email =>
          s"Previously $email"
        }
      )
    )

  def prefs(u: User)(pref: lila.pref.Pref)(implicit ctx: Context) =
    frag(
      canViewRoles(u) option mzSection("roles")(
        (if (isGranted(_.ChangePermission)) a(href := routes.Mod.permissions(u.username)) else span)(
          strong(cls := "text inline", dataIcon := " ")("Permissions: "),
          if (u.roles.isEmpty) "Add some" else Permission(u.roles).map(_.name).mkString(", ")
        )
      ),
      mzSection("preferences")(
        strong(cls := "text inline", dataIcon := "")("Notable preferences"),
        ul(
          (pref.keyboardMove != lila.pref.Pref.KeyboardMove.NO) option li("keyboard moves"),
          pref.botCompatible option li(
            strong(
              a(
                cls := "text",
                dataIcon := "",
                href := lila.common.String.base64
                  .decode("aHR0cDovL2NoZXNzLWNoZWF0LmNvbS9ob3dfdG9fY2hlYXRfYXRfbGljaGVzcy5odG1s")
              )("BOT-COMPATIBLE SETTINGS")
            )
          )
        )
      )
    )

  def showRageSit(rageSit: RageSit) =
    mzSection("sitdccounter")(
      strong(cls := "text inline")("Ragesit counter"),
      strong(cls := "fat")(rageSit.counterView)
    )

  private val sortNumberTh    = th(attr("data-sort-method") := "number")
  private val dataSort        = attr("data-sort")
  private val dataTags        = attr("data-tags")
  private val playban         = iconTag("")
  private val alt: Frag       = i("A")
  private val shadowban: Frag = iconTag("")
  private val boosting: Frag  = iconTag("")
  private val engine: Frag    = iconTag("")
  private val closed: Frag    = iconTag("")
  private val clean: Frag     = iconTag("")
  private val reportban       = iconTag("")
  private val notesText       = iconTag("")
  private def markTd(nb: Int, content: => Frag) =
    if (nb > 0) td(cls := "i", dataSort := nb)(content)
    else td

  def otherUsers(mod: Holder, u: User, data: UserLogins.TableData)(implicit
      ctx: Context
  ): Tag = {
    import data._
    mzSection("others")(
      table(cls := "slist")(
        thead(
          tr(
            th(
              pluralize("linked user", userLogins.otherUsers.size),
              (max < 1000 && true || othersWithEmail.others.sizeIs >= max) option frag(
                nbsp,
                a(cls := "more-others")("Load more")
              )
            ),
            isGranted(_.Admin) option th("Email"),
            sortNumberTh(dataSortDefault)("Same"),
            th("Games"),
            sortNumberTh(playban)(cls := "i", title := "Playban"),
            sortNumberTh(alt)(cls := "i", title := "Alt"),
            sortNumberTh(shadowban)(cls := "i", title := "Shadowban"),
            sortNumberTh(boosting)(cls := "i", title := "Boosting"),
            sortNumberTh(engine)(cls := "i", title := "Engine"),
            sortNumberTh(closed)(cls := "i", title := "Closed"),
            sortNumberTh(reportban)(cls := "i", title := "Reportban"),
            sortNumberTh(notesText)(cls := "i", title := "Notes"),
            sortNumberTh("Created"),
            sortNumberTh("Active"),
            isGranted(_.CloseAccount) option th
          )
        ),
        tbody(
          othersWithEmail.others.map { case other @ UserLogins.OtherUser(o, _, _) =>
            val userNotes =
              notes.filter(n => n.to == o.id && (ctx.me.exists(n.isFrom) || isGranted(_.Admin)))
            tr(
              cls := (o == u) option "same"
            )(
              if (o == u || Granter.canViewAltUsername(mod, o))
                td(dataSort := o.id)(userLink(o, withBestRating = true, params = "?mod"))
              else td,
              isGranted(_.Admin) option td(othersWithEmail emailValueOf o),
              td(
                // show prints and ips separately
                dataSort := other.score + (other.ips.nonEmpty ?? 1000000) + (other.fps.nonEmpty ?? 3000000)
              )(
                List(other.ips.size -> "IP", other.fps.size -> "Print")
                  .collect {
                    case (nb, name) if nb > 0 => s"$nb $name"
                  }
                  .mkString(", ")
              ),
              td(dataSort := o.count.game)(o.count.game.localize),
              markTd(~bans.get(o.id), playban(cls := "text")(~bans.get(o.id))),
              markTd(o.marks.alt ?? 1, alt),
              markTd(o.marks.troll ?? 1, shadowban),
              markTd(o.marks.boost ?? 1, boosting),
              markTd(o.marks.engine ?? 1, engine),
              markTd(o.disabled ?? 1, closed),
              markTd(o.marks.reportban ?? 1, reportban),
              userNotes.nonEmpty option {
                td(dataSort := userNotes.size)(
                  a(href := s"${routes.User.show(o.username)}?notes")(
                    notesText(
                      title := s"Notes from ${userNotes.map(_.from).map(usernameOrId).mkString(", ")}",
                      cls := "is-green"
                    ),
                    userNotes.size
                  )
                )
              } getOrElse td(dataSort := 0),
              td(dataSort := o.createdAt.getMillis)(momentFromNowServer(o.createdAt)),
              td(dataSort := o.seenAt.map(_.getMillis.toString))(o.seenAt.map(momentFromNowServer)),
              isGranted(_.CloseAccount) option td(
                !o.marks.alt option button(
                  cls := "button button-empty button-thin button-red mark-alt",
                  href := routes.Mod.alt(o.id, !o.marks.alt)
                )("ALT")
              )
            )
          }
        )
      )
    )
  }

  def identification(mod: Holder, user: User, logins: UserLogins)(implicit
      ctx: Context
  ): Frag = {
    val canIpBan  = isGranted(_.IpBan)
    val canFpBan  = isGranted(_.PrintBan)
    val canLocate = isGranted(_.Admin) || user.lameOrTrollOrAlt
    mzSection("identification")(
      canLocate option div(cls := "spy_locs")(
        table(cls := "slist slist--sort")(
          thead(
            tr(
              th("Country"),
              th("Region"),
              th("City"),
              sortNumberTh("Date")
            )
          ),
          tbody(
            logins.distinctLocations.toList
              .sortBy(-_.seconds)
              .map { loc =>
                tr(
                  td(loc.value.country),
                  td(loc.value.region),
                  td(loc.value.city),
                  td(dataSort := loc.date.getMillis)(momentFromNowServer(loc.date))
                )
              }
              .toList
          )
        )
      ),
      canLocate option div(cls := "spy_uas")(
        table(cls := "slist slist--sort")(
          thead(
            tr(
              th(pluralize("Device", logins.uas.size)),
              th("OS"),
              th("Client"),
              sortNumberTh("Date"),
              th("Type")
            )
          ),
          tbody(
            logins.uas
              .sortBy(-_.seconds)
              .map { ua =>
                val parsed = ua.value.parse
                tr(
                  td(title := ua.value.value)(
                    if (parsed.device.family == "Other") "Computer" else parsed.device.family
                  ),
                  td(parts(parsed.os.family.some, parsed.os.major)),
                  td(parts(parsed.userAgent.family.some, parsed.userAgent.major)),
                  td(dataSort := ua.date.getMillis)(momentFromNowServer(ua.date)),
                  td(ua.value.client.toString)
                )
              }
          )
        )
      ),
      div(id := "identification_screen", cls := "spy_ips")(
        table(cls := "slist spy_filter slist--sort")(
          thead(
            tr(
              th(pluralize("IP", logins.prints.size)),
              sortNumberTh("Alts"),
              th,
              th("Client"),
              sortNumberTh("Date"),
              canIpBan option sortNumberTh
            )
          )
        )
      ),
      div(cls := "spy_fps")(
        table(cls := "slist spy_filter slist--sort")(
          thead(
            tr(
              th(pluralize("Print", logins.prints.size)),
              sortNumberTh("Alts"),
              th("Client"),
              sortNumberTh("Date"),
              canFpBan option sortNumberTh
            )
          ),
          tbody(
            logins.prints.sortBy(fp => (-fp.alts.score, -fp.fp.seconds)).map { fp =>
              tr(cls := fp.banned option "blocked")(
                td(a(href := routes.Mod.print(fp.fp.value.value))(fp.fp.value)),
                td(dataSort := fp.alts.score)(altMarks(fp.alts)),
                td(fp.client.toString),
                td(dataSort := fp.fp.date.getMillis)(momentFromNowServer(fp.fp.date)),
                canFpBan option td(dataSort := (9999 - fp.alts.cleans))(
                  button(
                    cls := List(
                      "button button-empty" -> true,
                      "button-discouraging" -> (fp.alts.cleans > 0)
                    ),
                    href := routes.Mod.printBan(!fp.banned, fp.fp.value.value)
                  )("BAN")
                )
              )
            }
          )
        )
      )
    )
  }

  private def parts(ps: Option[String]*) = ps.flatten.distinct mkString " "

  private def altMarks(alts: UserLogins.Alts) =
    List[(Int, Frag)](
      alts.boosters -> boosting,
      alts.engines  -> engine,
      alts.trolls   -> shadowban,
      alts.alts     -> alt,
      alts.closed   -> closed,
      alts.cleans   -> clean
    ) collect {
      case (nb, tag) if nb > 4 => frag(List.fill(3)(tag), "+", nb - 3)
      case (nb, tag) if nb > 0 => frag(List.fill(nb)(tag))
    }

  def userMarks(o: User, playbans: Option[Int]) =
    div(cls := "user_marks")(
      playbans.map { nb =>
        playban(nb)
      },
      o.marks.troll option shadowban,
      o.marks.boost option boosting,
      o.marks.engine option engine,
      o.disabled option closed,
      o.marks.reportban option reportban
    )
}

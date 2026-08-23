package cellar.cli

import cats.effect.std.Console
import cats.effect.{ExitCode, IO}
import cellar.Config
import com.monovore.decline.Opts
import com.typesafe.config.{ConfigFactory, ConfigRenderOptions, ConfigValueFactory}
import fs2.io.file.{Files, Flags, Path}

object TelemetrySubcommand:

  private[cli] val userCellarDir     = Path(sys.props("user.home")) / ".cellar"
  private[cli] val confFile          = userCellarDir / "cellar.conf"
  private[cli] val projectConfFile   = Config.defaultProjectPath
  private[cli] val globalSeenMarker  = userCellarDir / ".telemetry-seen"
  private[cli] val projectSeenMarker = Path(".cellar") / ".telemetry-seen"

  private val privacyPolicyUrl = "https://github.com/VirtusLab/cellar/blob/main/PRIVACY_POLICY.md"

  private val globalFlag: Opts[Boolean] =
    Opts.flag("global", "Apply to all projects (default: current project only)").orFalse

  /** Human-readable scope for status messages, matching the target config file. */
  private def scopeLabel(global: Boolean): String =
    if global then "globally" else "for this project"

  def opts: Opts[IO[ExitCode]] =
    Opts.subcommand("telemetry", "Manage anonymous usage telemetry") {
      enableCmd orElse disableCmd orElse statusCmd orElse resetIdCmd
    }

  /** Persist the telemetry choice and record that consent was answered at the same scope. */
  private def record(enabled: Boolean, global: Boolean): IO[Unit] =
    setEnabled(enabled, global) *> markAnswered(global)

  private def enableCmd: Opts[IO[ExitCode]] =
    Opts.subcommand("enable", "Enable anonymous usage telemetry") {
      globalFlag.map { global =>
        record(enabled = true, global) *>
          InstallationId.ensure.flatMap(id =>
            IO.println(
              s"Telemetry enabled ${scopeLabel(global)}. Installation ID: $id\n" +
                s"Privacy Policy: $privacyPolicyUrl"
            )
          ).as(ExitCode.Success)
      }
    }

  private def disableCmd: Opts[IO[ExitCode]] =
    Opts.subcommand("disable", "Disable anonymous usage telemetry") {
      globalFlag.map { global =>
        record(enabled = false, global) *>
          IO.blocking(Config.loadFresh()).flatMap { fresh =>
            IO.whenA(fresh.otel.enabled)(
              IO(System.err.println(
                "Note: a project-level .cellar/cellar.conf enables telemetry and overrides this setting. " +
                "Edit it directly to disable telemetry for that project."
              ))
            )
          } *>
          IO.println(s"Telemetry disabled ${scopeLabel(global)}.").as(ExitCode.Success)
      }
    }

  private[cli] def markAnswered(global: Boolean): IO[Unit] =
    val marker = if global then globalSeenMarker else projectSeenMarker
    Files[IO].createDirectories(marker.parent.get) *>
      Files[IO].open(marker, Flags.Write).use_

  /**
   * Pure decision for the first-run consent gate: whether to withhold the command and show the
   * consent prompt. We gate until the user has answered — either via config or a "seen" marker.
   * There is no give-up fallthrough: an unanswered prompt keeps gating on every run.
   */
  private[cli] def shouldGateTelemetry(
      alreadyAnswered: Boolean,
      markerAnswered: Boolean
  ): Boolean =
    !alreadyAnswered && !markerAnswered

  private[cli] enum TelemetryChoice:
    case EnableProject, EnableGlobal, DisableProject, DisableGlobal

  /** Parse a line of user input into a consent choice; empty input defaults to disabling. */
  private[cli] def parseChoice(line: String): Option[TelemetryChoice] =
    line.trim.toLowerCase match
      case "1" | "e" | "enable"               => Some(TelemetryChoice.EnableProject)
      case "2" | "enable-global"              => Some(TelemetryChoice.EnableGlobal)
      case "" | "3" | "d" | "disable"         => Some(TelemetryChoice.DisableProject)
      case "4" | "g" | "global"               => Some(TelemetryChoice.DisableGlobal)
      case _                                  => None

  private def applyChoice(choice: TelemetryChoice): IO[Unit] =
    val (enabled, global) = choice match
      case TelemetryChoice.EnableProject  => (true, false)
      case TelemetryChoice.EnableGlobal   => (true, true)
      case TelemetryChoice.DisableProject => (false, false)
      case TelemetryChoice.DisableGlobal  => (false, true)
    record(enabled, global) *> {
      if enabled then
        InstallationId.ensure.flatMap(id =>
          Console[IO].errorln(
            s"Telemetry enabled ${scopeLabel(global)}. Installation ID: $id\n" +
              s"Privacy Policy: $privacyPolicyUrl"
          )
        )
      else Console[IO].errorln(s"Telemetry disabled ${scopeLabel(global)}.")
    }

  /**
   * Interactive first-run consent prompt for human (TTY) sessions. Reads a single choice from
   * stdin and records it, so the original command can proceed in the same invocation. All prompt
   * text is written to stderr so it never contaminates a piped stdout.
   */
  private[cli] def interactivePrompt: IO[Unit] =
    val intro =
      "cellar collects anonymous usage stats — command names and success/failure only, no " +
        "coordinates, symbols, or user data. See https://github.com/VirtusLab/cellar#telemetry"
    def ask: IO[Unit] =
      Console[IO].errorln(intro) *>
        Console[IO].error(
          "Enable telemetry? [1] enable  [2] enable globally  [3] disable (default)  [4] disable globally: "
        ) *>
        Console[IO].readLine.attempt.flatMap {
          case Left(_) => applyChoice(TelemetryChoice.DisableProject) // EOF (e.g. Ctrl-D) → privacy-preserving default
          case Right(line) =>
            parseChoice(line) match
              case Some(choice) => applyChoice(choice)
              case None         => Console[IO].errorln("Please answer 1, 2, 3, or 4.") *> ask
        }
    ask

  private def statusCmd: Opts[IO[ExitCode]] =
    Opts.subcommand("status", "Show telemetry status") {
      Opts.unit.map { _ =>
        val cfg = Config.global.otel
        val statusLine =
          if cfg.enabled then s"Telemetry: enabled\nEndpoint:  ${cfg.endpoint}"
          else "Telemetry: disabled"
        InstallationId.read.flatMap { idOpt =>
          val idLine = idOpt.fold("")(id => s"\nInstallation ID: $id")
          IO.println(statusLine + idLine).as(ExitCode.Success)
        }
      }
    }

  private def resetIdCmd: Opts[IO[ExitCode]] =
    Opts.subcommand("reset-id", "Generate a new anonymous installation ID") {
      Opts.unit.map { _ =>
        InstallationId.reset.flatMap(id =>
          IO.println(s"New installation ID: $id")
        ).as(ExitCode.Success)
      }
    }

  private[cli] def setEnabled(enabled: Boolean, global: Boolean): IO[Unit] =
    val target = if global then confFile else projectConfFile
    Files[IO].createDirectories(target.parent.get) *>
      Files[IO].readUtf8(target).compile.string
        .recover { case _: java.nio.file.NoSuchFileException => "" }
        .flatMap { existing =>
          val updated  = ConfigFactory.parseString(existing)
            .withValue("otel.enabled", ConfigValueFactory.fromAnyRef(enabled))
          val rendered = updated.root().render(
            ConfigRenderOptions.defaults().setOriginComments(false).setComments(false).setJson(false)
          )
          fs2.Stream.emit(rendered).through(Files[IO].writeUtf8(target)).compile.drain
        }

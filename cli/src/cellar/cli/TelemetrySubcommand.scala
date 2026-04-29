package cellar.cli

import cats.effect.{ExitCode, IO}
import cellar.Config
import com.monovore.decline.Opts
import fs2.io.file.{Files, Flags, Path}

object TelemetrySubcommand:

  private[cli] val confFile         = Path(sys.props("user.home")) / ".cellar" / "cellar.conf"
  private[cli] val globalSeenMarker = Path(sys.props("user.home")) / ".cellar" / ".telemetry-seen"

  def opts: Opts[IO[ExitCode]] =
    Opts.subcommand("telemetry", "Manage anonymous usage telemetry") {
      enableCmd orElse disableCmd orElse statusCmd orElse resetIdCmd
    }

  private def enableCmd: Opts[IO[ExitCode]] =
    Opts.subcommand("enable", "Enable anonymous usage telemetry") {
      Opts.unit.map { _ =>
        setEnabled(true) *> markAnswered *>
          InstallationId.ensure.flatMap(id =>
            IO.println(s"Telemetry enabled. Installation ID: $id")
          ).as(ExitCode.Success)
      }
    }

  private def disableCmd: Opts[IO[ExitCode]] =
    Opts.subcommand("disable", "Disable anonymous usage telemetry") {
      val globalFlag = Opts.flag("global", "Disable for all projects and never prompt again").orFalse
      globalFlag.map { global =>
        setEnabled(false) *>
          IO.whenA(global)(markAnswered) *>
          IO.blocking(Config.loadFresh()).flatMap { fresh =>
            IO.whenA(fresh.telemetry.enabled)(
              IO(System.err.println(
                "Note: a project-level .cellar/cellar.conf enables telemetry and overrides this setting. " +
                "Edit it directly to disable telemetry for that project."
              ))
            )
          } *>
          IO.println("Telemetry disabled.").as(ExitCode.Success)
      }
    }

  private def markAnswered: IO[Unit] =
    Files[IO].createDirectories(globalSeenMarker.parent.get) *>
      Files[IO].open(globalSeenMarker, Flags.Write).use_

  private def statusCmd: Opts[IO[ExitCode]] =
    Opts.subcommand("status", "Show telemetry status") {
      Opts.unit.map { _ =>
        val cfg = Config.global.telemetry
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

  private[cli] def setEnabled(enabled: Boolean): IO[Unit] =
    val dir = confFile.parent.get
    Files[IO].createDirectories(dir) *>
      Files[IO].readUtf8(confFile).compile.string
        .recover { case _: java.nio.file.NoSuchFileException => "" }
        .flatMap { content =>
          val updated = replaceTelemetryBlock(content, enabled)
          fs2.Stream.emit(updated).through(Files[IO].writeUtf8(confFile)).compile.drain
        }

  private def replaceTelemetryBlock(content: String, enabled: Boolean): String =
    val stripped = content.replaceAll("""(?s)telemetry\s*\{[^}]*\}\n?""", "").strip
    val block    = s"telemetry {\n  enabled = $enabled\n}"
    if stripped.isEmpty then block + "\n"
    else stripped + "\n\n" + block + "\n"

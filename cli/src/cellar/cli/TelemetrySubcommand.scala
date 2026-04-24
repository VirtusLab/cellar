package cellar.cli

import cats.effect.{ExitCode, IO}
import cellar.Config
import com.monovore.decline.Opts
import fs2.io.file.{Files, Path}

object TelemetrySubcommand:

  private val confFile = Path(sys.props("user.home")) / ".cellar" / "cellar.conf"

  def opts: Opts[IO[ExitCode]] =
    Opts.subcommand("telemetry", "Manage anonymous usage telemetry") {
      enableCmd orElse disableCmd orElse statusCmd orElse resetIdCmd
    }

  private def enableCmd: Opts[IO[ExitCode]] =
    Opts.subcommand("enable", "Enable anonymous usage telemetry") {
      Opts.unit.map { _ =>
        setEnabled(true) *>
          InstallationId.ensure.flatMap(id =>
            IO.println(s"Telemetry enabled. Installation ID: $id")
          ).as(ExitCode.Success)
      }
    }

  private def disableCmd: Opts[IO[ExitCode]] =
    Opts.subcommand("disable", "Disable anonymous usage telemetry") {
      Opts.unit.map { _ =>
        setEnabled(false) *>
          IO.println("Telemetry disabled.").as(ExitCode.Success)
      }
    }

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
        val idPath = Path(sys.props("user.home")) / ".cellar" / "installation_id"
        Files[IO].deleteIfExists(idPath) *>
          InstallationId.ensure.flatMap(id =>
            IO.println(s"New installation ID: $id")
          ).as(ExitCode.Success)
      }
    }

  private def setEnabled(enabled: Boolean): IO[Unit] =
    val dir = confFile.parent.get
    Files[IO].createDirectories(dir) *>
      Files[IO].exists(confFile).flatMap { exists =>
        val readContent = if exists then Files[IO].readUtf8(confFile).compile.string else IO.pure("")
        readContent.flatMap { content =>
          val updated = replaceTelemetryBlock(content, enabled)
          fs2.Stream.emit(updated).through(Files[IO].writeUtf8(confFile)).compile.drain
        }
      }

  private def replaceTelemetryBlock(content: String, enabled: Boolean): String =
    val stripped = content.replaceAll("""(?s)telemetry\s*\{[^}]*\}\n?""", "").strip
    val block    = s"telemetry {\n  enabled = $enabled\n}"
    if stripped.isEmpty then block + "\n"
    else stripped + "\n\n" + block + "\n"

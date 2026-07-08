package cellar.cli

import cats.effect.{ExitCode, IO}
import cellar.Config
import com.monovore.decline.Opts
import com.typesafe.config.{ConfigFactory, ConfigRenderOptions, ConfigValueFactory}
import fs2.io.file.{Files, Flags, Path}

object TelemetrySubcommand:

  private[cli] val userCellarDir    = Path(sys.props("user.home")) / ".cellar"
  private[cli] val confFile         = userCellarDir / "cellar.conf"
  private[cli] val globalSeenMarker = userCellarDir / ".telemetry-seen"

  def opts: Opts[IO[ExitCode]] =
    Opts.subcommand("telemetry", "Manage anonymous usage telemetry") {
      enableCmd orElse disableCmd orElse statusCmd orElse resetIdCmd
    }

  private def enableCmd: Opts[IO[ExitCode]] =
    Opts.subcommand("enable", "Enable anonymous usage telemetry") {
      Opts.unit.map { _ =>
        setEnabled(true) *> markAnswered *>
          InstallationId.ensure.flatMap(id =>
            IO.println(
              s"Telemetry enabled. Installation ID: $id\n" +
                "Privacy Policy: https://github.com/VirtusLab/cellar/blob/main/PRIVACY_POLICY.md"
            )
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
            IO.whenA(fresh.otel.enabled)(
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

  private[cli] def setEnabled(enabled: Boolean): IO[Unit] =
    Files[IO].createDirectories(confFile.parent.get) *>
      IO.blocking {
        val nio    = confFile.toNioPath
        val parsed =
          if java.nio.file.Files.exists(nio) then ConfigFactory.parseFile(nio.toFile)
          else ConfigFactory.empty()
        val updated  = parsed.withValue("otel.enabled", ConfigValueFactory.fromAnyRef(enabled))
        val rendered = updated.root().render(
          ConfigRenderOptions.defaults().setOriginComments(false).setComments(false).setJson(false)
        )
        java.nio.file.Files.writeString(nio, rendered)
        ()
      }

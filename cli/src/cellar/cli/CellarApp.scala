package cellar.cli

import cats.effect.{ExitCode, IO, Resource}
import cats.effect.unsafe.IORuntimeConfig
import cats.syntax.all.*
import cellar.*
import cellar.handlers.{DepsHandler, GetHandler, GetSourceHandler, ListHandler, MetaHandler, ProjectGetHandler, ProjectListHandler, ProjectSearchHandler, SearchHandler}
import cellar.profiling.{ProfilingIOApp, PyroscopeSetup, TracingRuntime}
import com.monovore.decline.*
import coursierapi.{MavenRepository, Repository}
import fs2.io.file.{Files, Path}
import org.typelevel.otel4s.trace.Tracer

import scala.concurrent.duration.Duration

private def runtimeLabel: String =
  if System.getProperty("org.graalvm.nativeimage.imagecode") == "runtime" then "native-image"
  else s"JVM ${System.getProperty("java.version")}"

private def platformLabel: String =
  val os = System.getProperty("os.name").toLowerCase match
    case n if n.contains("mac")   => "macos"
    case n if n.contains("linux") => "linux"
    case n if n.contains("win")   => "windows"
    case other                    => other
  val arch = System.getProperty("os.arch") match
    case "aarch64"          => "arm64"
    case "x86_64" | "amd64" => "x86_64"
    case other              => other
  s"$os-$arch"

object CellarApp extends ProfilingIOApp:

  override def profilingEnabled: Boolean = Config.global.profiling.enabled

  override def runtimeConfig: IORuntimeConfig =
    val base = super.runtimeConfig
    if Config.global.starvationChecks.enabled then base
    else base.copy(cpuStarvationCheckInitialDelay = Duration.Inf)

  private lazy val tracingConfig = TracingConfigBridge.fromCellarConfig(Config.global)

  private val pyroscopeResource: Resource[IO, Unit] =
    tracingConfig.pyroscopeEndpoint.fold(Resource.unit[IO])(PyroscopeSetup.resource(_, "cellar"))

  private def traced(commandName: String)(body: Tracer[IO] ?=> IO[ExitCode]): IO[ExitCode] =
    val maybeInstallationId =
      if tracingConfig.otlpEndpoint.isDefined then InstallationId.read else IO.pure(None)
    pyroscopeResource.use { _ =>
      maybeInstallationId.flatMap { installationId =>
        TracingRuntime.tracedCommand(
          tracingConfig,
          sharedIOLocal,
          BuildInfo.version,
          commandName,
          installationId = installationId,
          classifyUserError = _.isInstanceOf[CellarError]
        )(body)
      }
    }

  /** Exit code returned when a command is withheld pending the user's telemetry choice. */
  private val needsInputExitCode = ExitCode(2)

  /**
   * Gate the first-run telemetry consent decision. Returns `Some(exitCode)` when the command must
   * NOT run yet (the consent prompt was emitted and the user needs to choose); `None` when the
   * command is clear to proceed. The gate persists until the user answers — there is no give-up
   * fallthrough, so a command stays withheld on every run until a choice is recorded.
   */
  private val firstRunNotice: IO[Option[ExitCode]] =
    val alreadyAnswered =
      Config.global.otel.enabled || Config.global.profiling.enabled
    val shouldGate: IO[Boolean] =
      // Short-circuit when already answered via config: skip the marker-file stats entirely, since
      // this runs before every command and the marker result would be discarded anyway.
      if alreadyAnswered then IO.pure(false)
      else
        (
          Files[IO].exists(TelemetrySubcommand.projectSeenMarker),
          Files[IO].exists(TelemetrySubcommand.globalSeenMarker)
        ).mapN { (projectSeen, globalSeen) =>
          TelemetrySubcommand.shouldGateTelemetry(alreadyAnswered, projectSeen || globalSeen)
        }
    shouldGate.flatMap {
      case false => IO.pure(None)
      case true  =>
        // A real terminal (`System.console()` non-null) means a human is present: prompt
        // interactively and let the command proceed once answered. Otherwise (pipe / agent / CI)
        // emit the machine-readable consent request and withhold the command until answered.
        IO(System.console() != null).flatMap {
          case true  => TelemetrySubcommand.interactivePrompt.as(None)
          case false =>
            val json =
              """|{
                 |  "status": "needs_input",
                 |  "ui": "AskUserQuestion",
                 |  "instruction": "IMPORTANT: This is a consent prompt for the human user, not for you. Present these options to the user through your interactive selection UI (e.g. the AskUserQuestion tool) so the user can pick one. Do NOT render them as plain text, do NOT choose an option on the user's behalf, and do NOT proceed with the original command until the user has chosen.",
                 |  "question": "IMPORTANT — cellar needs your consent before continuing. Enable anonymous cellar usage stats? (No coordinates, symbols, or user data — just command names and success/failure.)",
                 |  "options": [
                 |    { "label": "Enable",          "description": "Opt in for this project — helps improve cellar.", "command": "cellar telemetry enable" },
                 |    { "label": "Enable globally", "description": "Opt in for all projects.",                       "command": "cellar telemetry enable --global" },
                 |    { "label": "Disable",          "description": "Opt out for this project.",                     "command": "cellar telemetry disable" },
                 |    { "label": "Disable globally", "description": "Never ask again across any project.",           "command": "cellar telemetry disable --global" }
                 |  ],
                 |  "details": "Telemetry overview: https://github.com/VirtusLab/cellar#telemetry — Privacy Policy: https://github.com/VirtusLab/cellar/blob/main/PRIVACY_POLICY.md"
                 |}""".stripMargin
            IO(System.out.println(json)).as(Some(needsInputExitCode))
        }
    }

  def main: Opts[IO[ExitCode]] =
    val regularCmds =
      getSubcmd orElse getExternalSubcmd orElse
        getSourceSubcmd orElse
        listSubcmd orElse listExternalSubcmd orElse
        searchSubcmd orElse searchExternalSubcmd orElse
        depsSubcmd orElse metaSubcmd
    regularCmds.map(cmd =>
      firstRunNotice.flatMap {
        case Some(code) => IO.pure(code)
        case None       => cmd
      }
    ) orElse TelemetrySubcommand.opts

  override def run(args: List[String]): IO[ExitCode] =
    val versionOpt = Opts
      .flag("version", "Print the version number and exit", short = "V")
      .map(_ => IO.println(s"${BuildInfo.version} ($runtimeLabel, $platformLabel)").as(ExitCode.Success))
    val command = Command("cellar", "Inspect Maven-published JVM dependency APIs")(main <+> versionOpt)
    command.parse(args, sys.env) match
      case Left(help) if help.errors.nonEmpty =>
        traced("parse-error")(IO.pure(ExitCode.Error)).void *>
          IO.blocking(System.err.println(help)).as(ExitCode.Error)
      case Left(help) =>
        IO.blocking(System.out.println(help)).as(ExitCode.Success)
      case Right(action) =>
        action

  private given Argument[Path] = Argument[java.nio.file.Path].map(Path.fromNioPath)

  private val coordArg: Opts[String] =
    Opts.argument[String]("coordinate")

  private val symbolArg: Opts[String] =
    Opts.argument[String]("fully-qualified-symbol")

  private val javaHomeOpt: Opts[Option[Path]] =
    Opts.option[Path]("java-home", "Use a specific JDK for JRE classpath").orNone

  private val extraReposOpt: Opts[List[Repository]] =
    Opts.options[String]("repository", "Extra Maven repository URL (repeatable)", short = "r", metavar = "url")
      .orEmpty
      .map(_.map(MavenRepository.of(_)))

  private val limitOpt: Opts[Int] =
    Opts
      .option[Int]("limit", "Maximum number of results to return", short = "l", metavar = "N")
      .withDefault(50)

  private val memberLimitOpt: Opts[Option[Int]] =
    Opts
      .option[Int]("limit", "Maximum number of members to display", short = "l", metavar = "N")
      .orNone

  private val moduleOpt: Opts[Option[String]] =
    Opts.option[String]("module", "Build module name (required for Mill/sbt)", short = "m", metavar = "name").orNone

  private val noCacheOpt: Opts[Boolean] =
    Opts.flag("no-cache", "Skip classpath cache (re-extract from build tool)").orFalse

  private val hideInheritedOpt: Opts[Boolean] =
    Opts.flag("hide-inherited", "Show only members declared on the type itself").orFalse

  private val groupInheritedOpt: Opts[Boolean] =
    Opts.flag("group-inherited", "Group members by declaring type with section headers").orFalse

  private def parseAndResolve(raw: String, extraRepos: List[Repository]): IO[Either[String, MavenCoordinate]] =
    MavenCoordinate.parse(raw) match
      case Left(err)    => IO.pure(Left(err))
      case Right(coord) => coord.resolveLatest(extraRepos).map(Right(_))

  private val getSubcmd: Opts[IO[ExitCode]] =
    Opts.subcommand("get", "Fetch symbol info from the current project") {
      (symbolArg, moduleOpt, memberLimitOpt, hideInheritedOpt, groupInheritedOpt, javaHomeOpt, noCacheOpt).mapN {
        (fqn, module, limit, hideInherited, groupInherited, javaHome, noCache) =>
          traced("get") {
            ProjectGetHandler.run(fqn, module, javaHome, noCache, limit, hideInherited, groupInherited)
          }
      }
    }

  private val listSubcmd: Opts[IO[ExitCode]] =
    Opts.subcommand("list", "List symbols in a package or class from the current project") {
      (symbolArg, moduleOpt, limitOpt, javaHomeOpt, noCacheOpt).mapN { (fqn, module, limit, javaHome, noCache) =>
        traced("list") {
          ProjectListHandler.run(fqn, module, limit, javaHome, noCache)
        }
      }
    }

  private val searchSubcmd: Opts[IO[ExitCode]] =
    Opts.subcommand("search", "Substring search for symbol names in the current project") {
      (Opts.argument[String]("query"), moduleOpt, limitOpt, javaHomeOpt, noCacheOpt).mapN {
        (query, module, limit, javaHome, noCache) =>
          traced("search") {
            ProjectSearchHandler.run(query, module, limit, javaHome, noCache)
          }
      }
    }

  private val getExternalSubcmd: Opts[IO[ExitCode]] =
    Opts.subcommand("get-external", "Fetch symbol info from a Maven coordinate") {
      (coordArg, symbolArg, memberLimitOpt, hideInheritedOpt, groupInheritedOpt, javaHomeOpt, extraReposOpt).mapN {
        (rawCoord, fqn, limit, hideInherited, groupInherited, javaHome, extraRepos) =>
          traced("get-external") {
            parseAndResolve(rawCoord, extraRepos).flatMap {
              case Left(err)    => IO.blocking(System.err.println(err)).as(ExitCode.Error)
              case Right(coord) =>
                GetHandler.run(coord, fqn, javaHome, extraRepos, limit, hideInherited, groupInherited)
            }
          }
      }
    }

  private val getSourceSubcmd: Opts[IO[ExitCode]] =
    Opts.subcommand("get-source", "Fetch the source code of a named symbol") {
      (coordArg, symbolArg, javaHomeOpt, extraReposOpt).mapN { (rawCoord, fqn, javaHome, extraRepos) =>
        traced("get-source") {
          parseAndResolve(rawCoord, extraRepos).flatMap {
            case Left(err)    => IO.blocking(System.err.println(err)).as(ExitCode.Error)
            case Right(coord) => GetSourceHandler.run(coord, fqn, javaHome, extraRepos)
          }
        }
      }
    }

  private val listExternalSubcmd: Opts[IO[ExitCode]] =
    Opts.subcommand("list-external", "List symbols from a Maven coordinate") {
      (coordArg, symbolArg, limitOpt, javaHomeOpt, extraReposOpt).mapN { (rawCoord, fqn, limit, javaHome, extraRepos) =>
        traced("list-external") {
          parseAndResolve(rawCoord, extraRepos).flatMap {
            case Left(err)    => IO.blocking(System.err.println(err)).as(ExitCode.Error)
            case Right(coord) => ListHandler.run(coord, fqn, limit, javaHome, extraRepos)
          }
        }
      }
    }

  private val searchExternalSubcmd: Opts[IO[ExitCode]] =
    Opts.subcommand("search-external", "Substring search for symbol names from a Maven coordinate") {
      (coordArg, Opts.argument[String]("query"), limitOpt, javaHomeOpt, extraReposOpt).mapN {
        (rawCoord, query, limit, javaHome, extraRepos) =>
          traced("search-external") {
            parseAndResolve(rawCoord, extraRepos).flatMap {
              case Left(err)    => IO.blocking(System.err.println(err)).as(ExitCode.Error)
              case Right(coord) => SearchHandler.run(coord, query, limit, javaHome, extraRepos)
            }
          }
      }
    }

  private val depsSubcmd: Opts[IO[ExitCode]] =
    Opts.subcommand("deps", "Print the transitive dependency list") {
      (coordArg, extraReposOpt).mapN { (rawCoord, extraRepos) =>
        traced("deps") {
          parseAndResolve(rawCoord, extraRepos).flatMap {
            case Left(err)    => IO.blocking(System.err.println(err)).as(ExitCode.Error)
            case Right(coord) => DepsHandler.run(coord, extraRepositories = extraRepos)
          }
        }
      }
    }

  private val metaSubcmd: Opts[IO[ExitCode]] =
    Opts.subcommand("meta", "Print POM metadata (name, description, license, SCM, developers)") {
      (coordArg, extraReposOpt).mapN { (rawCoord, extraRepos) =>
        traced("meta") {
          parseAndResolve(rawCoord, extraRepos).flatMap {
            case Left(err)    => IO.blocking(System.err.println(err)).as(ExitCode.Error)
            case Right(coord) => MetaHandler.run(coord, extraRepositories = extraRepos)
          }
        }
      }
    }

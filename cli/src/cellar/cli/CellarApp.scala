package cellar.cli

import cats.effect.{ExitCode, IO}
import cats.syntax.all.*
import cellar.*
import cellar.handlers.{DepsHandler, GetHandler, GetSourceHandler, ListHandler, MetaHandler, ProjectGetHandler, ProjectListHandler, ProjectSearchHandler, SearchHandler}
import com.monovore.decline.*
import com.monovore.decline.effect.*
import coursierapi.{MavenRepository, Repository}
import fs2.io.file.Path

object CellarApp
    extends CommandIOApp(
      name = "cellar",
      header = "Inspect Maven-published JVM dependency APIs",
      version = BuildInfo.version
    ):

  override def main: Opts[IO[ExitCode]] =
    getSubcmd orElse getExternalSubcmd orElse
      getSourceSubcmd orElse
      listSubcmd orElse listExternalSubcmd orElse
      searchSubcmd orElse searchExternalSubcmd orElse
      depsSubcmd orElse metaSubcmd

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

  private val moduleOpt: Opts[Option[String]] =
    Opts.option[String]("module", "Build module name (required for Mill/sbt)", short = "m", metavar = "name").orNone

  private val noCacheOpt: Opts[Boolean] =
    Opts.flag("no-cache", "Skip classpath cache (re-extract from build tool)").orFalse

  private val configOpt: Opts[IO[Config]] =
    Opts.option[Path]("config", "Path to config file", "c").orNone.map(Config.load)

  private def parseAndResolve(raw: String, extraRepos: List[Repository]): IO[Either[String, MavenCoordinate]] =
    MavenCoordinate.parse(raw) match
      case Left(err)    => IO.pure(Left(err))
      case Right(coord) => coord.resolveLatest(extraRepos).map(Right(_))

  private val getSubcmd: Opts[IO[ExitCode]] =
    Opts.subcommand("get", "Fetch symbol info from the current project") {
      (symbolArg, moduleOpt, configOpt, javaHomeOpt, noCacheOpt).mapN { (fqn, module, configIO, javaHome, noCache) =>
        configIO.flatMap(ProjectGetHandler.run(fqn, module, _, javaHome, noCache))
      }
    }

  private val listSubcmd: Opts[IO[ExitCode]] =
    Opts.subcommand("list", "List symbols in a package or class from the current project") {
      (symbolArg, moduleOpt, limitOpt, configOpt, javaHomeOpt, noCacheOpt).mapN { (fqn, module, limit, configIO, javaHome, noCache) =>
        configIO.flatMap(ProjectListHandler.run(fqn, module, limit, _, javaHome, noCache))
      }
    }

  private val searchSubcmd: Opts[IO[ExitCode]] =
    Opts.subcommand("search", "Substring search for symbol names in the current project") {
      (Opts.argument[String]("query"), moduleOpt, limitOpt, configOpt, javaHomeOpt, noCacheOpt).mapN {
        (query, module, limit, configIO, javaHome, noCache) =>
          configIO.flatMap(ProjectSearchHandler.run(query, module, limit, _, javaHome, noCache))
      }
    }

  private val getExternalSubcmd: Opts[IO[ExitCode]] =
    Opts.subcommand("get-external", "Fetch symbol info from a Maven coordinate") {
      (coordArg, symbolArg, javaHomeOpt, extraReposOpt).mapN { (rawCoord, fqn, javaHome, extraRepos) =>
        parseAndResolve(rawCoord, extraRepos).flatMap {
          case Left(err)    => IO.blocking(System.err.println(err)).as(ExitCode.Error)
          case Right(coord) => GetHandler.run(coord, fqn, javaHome, extraRepos)
        }
      }
    }

  private val getSourceSubcmd: Opts[IO[ExitCode]] =
    Opts.subcommand("get-source", "Fetch the source code of a named symbol") {
      (coordArg, symbolArg, javaHomeOpt, extraReposOpt).mapN { (rawCoord, fqn, javaHome, extraRepos) =>
        parseAndResolve(rawCoord, extraRepos).flatMap {
          case Left(err)    => IO.blocking(System.err.println(err)).as(ExitCode.Error)
          case Right(coord) => GetSourceHandler.run(coord, fqn, javaHome, extraRepos)
        }
      }
    }

  private val listExternalSubcmd: Opts[IO[ExitCode]] =
    Opts.subcommand("list-external", "List symbols from a Maven coordinate") {
      (coordArg, symbolArg, limitOpt, javaHomeOpt, extraReposOpt).mapN { (rawCoord, fqn, limit, javaHome, extraRepos) =>
        parseAndResolve(rawCoord, extraRepos).flatMap {
          case Left(err)    => IO.blocking(System.err.println(err)).as(ExitCode.Error)
          case Right(coord) => ListHandler.run(coord, fqn, limit, javaHome, extraRepos)
        }
      }
    }

  private val searchExternalSubcmd: Opts[IO[ExitCode]] =
    Opts.subcommand("search-external", "Substring search for symbol names from a Maven coordinate") {
      (coordArg, Opts.argument[String]("query"), limitOpt, javaHomeOpt, extraReposOpt).mapN {
        (rawCoord, query, limit, javaHome, extraRepos) =>
          parseAndResolve(rawCoord, extraRepos).flatMap {
            case Left(err)    => IO.blocking(System.err.println(err)).as(ExitCode.Error)
            case Right(coord) => SearchHandler.run(coord, query, limit, javaHome, extraRepos)
          }
      }
    }

  private val depsSubcmd: Opts[IO[ExitCode]] =
    Opts.subcommand("deps", "Print the transitive dependency list") {
      (coordArg, extraReposOpt).mapN { (rawCoord, extraRepos) =>
        parseAndResolve(rawCoord, extraRepos).flatMap {
          case Left(err)    => IO.blocking(System.err.println(err)).as(ExitCode.Error)
          case Right(coord) => DepsHandler.run(coord, extraRepositories = extraRepos)
        }
      }
    }

  private val metaSubcmd: Opts[IO[ExitCode]] =
    Opts.subcommand("meta", "Print POM metadata (name, description, license, SCM, developers)") {
      (coordArg, extraReposOpt).mapN { (rawCoord, extraRepos) =>
        parseAndResolve(rawCoord, extraRepos).flatMap {
          case Left(err)    => IO.blocking(System.err.println(err)).as(ExitCode.Error)
          case Right(coord) => MetaHandler.run(coord, extraRepositories = extraRepos)
        }
      }
    }

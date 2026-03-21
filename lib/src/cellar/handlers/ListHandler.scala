package cellar.handlers

import cats.effect.{ExitCode, IO}
import cats.effect.std.Console
import cats.syntax.all.*
import cellar.*
import coursierapi.Repository
import java.nio.file.Path

object ListHandler:
  def run(
      coord: MavenCoordinate,
      fqn: String,
      limit: Int,
      javaHome: Option[Path] = None,
      extraRepositories: Seq[Repository] = Seq.empty
  )(using Console[IO]): IO[ExitCode] =
    val program =
      for
        jreClasspath <- javaHome.fold(JreClasspath.jrtPath())(JreClasspath.jrtPath)
        result   <- ContextResource.makeFromCoord(coord, jreClasspath, extraRepositories).use { (ctx, _) =>
          given tastyquery.Contexts.Context = ctx
          runCore(fqn, limit, Some(coord))
        }
      yield result

    program.handleErrorWith {
      case e: CellarError => Console[IO].errorln(e.getMessage).as(ExitCode.Error)
      case e: Throwable   => Console[IO].errorln(e.getMessage).as(ExitCode.Error)
    }

  /** Core list logic shared by coordinate-based and project-aware flows. */
  def runCore(
      fqn: String,
      limit: Int,
      coord: Option[MavenCoordinate]
  )(using tastyquery.Contexts.Context, Console[IO]): IO[ExitCode] =
    SymbolLister.resolve(fqn).flatMap {
      case ListResolveResult.NotFound =>
        val ctx = coord.fold(s"'$fqn' not found.")(c => s"'$fqn' not found in '${c.render}'.")
        Console[IO].errorln(ctx).as(ExitCode.Error)
      case ListResolveResult.PartialMatch(resolvedFqn, missingMember) =>
        val msg = coord match
          case Some(c) => CellarError.PartialResolution(fqn, c, resolvedFqn, missingMember).getMessage
          case None    => s"Symbol '$fqn' not found. Resolved up to '$resolvedFqn' but member '$missingMember' was not found."
        Console[IO].errorln(msg).as(ExitCode.Error)
      case ListResolveResult.Found(target) =>
        val memberStream = SymbolLister
          .listMembers(target)
          .evalMap(sym => IO.blocking(LineFormatter.formatLine(sym)))
        StreamOps.bounded(memberStream, limit).flatMap { lines =>
          lines.traverse_(Console[IO].println).as(ExitCode.Success)
        }
    }

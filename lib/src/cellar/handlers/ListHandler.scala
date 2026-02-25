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
        jrePaths <- javaHome.fold(JreClasspath.jrtPath())(JreClasspath.jrtPath)
        result   <- ContextResource.makeFromCoord(coord, jrePaths, extraRepositories).use { (ctx, _) =>
          given tastyquery.Contexts.Context = ctx
          SymbolLister.resolve(fqn).flatMap {
            case ListResolveResult.NotFound =>
              Console[IO]
                .errorln(s"'$fqn' not found in '${coord.render}'.")
                .as(ExitCode.Error)
            case ListResolveResult.PartialMatch(resolvedFqn, missingMember) =>
              Console[IO]
                .errorln(CellarError.PartialResolution(fqn, coord, resolvedFqn, missingMember).getMessage)
                .as(ExitCode.Error)
            case ListResolveResult.Found(target) =>
              val memberStream = SymbolLister
                .listMembers(target)
                .evalMap(sym => IO.blocking(LineFormatter.formatLine(sym)))
              StreamOps.bounded(memberStream, limit).flatMap { lines =>
                lines.traverse_(Console[IO].println).as(ExitCode.Success)
              }
          }
        }
      yield result

    program.handleErrorWith {
      case e: CellarError => Console[IO].errorln(e.getMessage).as(ExitCode.Error)
      case e: Throwable   => Console[IO].errorln(e.getMessage).as(ExitCode.Error)
    }

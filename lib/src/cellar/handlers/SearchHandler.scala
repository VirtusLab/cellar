package cellar.handlers

import cats.effect.{ExitCode, IO}
import cats.effect.std.Console
import cats.syntax.all.*
import cellar.*
import coursierapi.Repository
import fs2.io.file.Path
import org.typelevel.otel4s.trace.Tracer

object SearchHandler:
  def run(
      coord: MavenCoordinate,
      query: String,
      limit: Int,
      javaHome: Option[Path] = None,
      extraRepositories: Seq[Repository] = Seq.empty
  )(using Console[IO], Tracer[IO]): IO[ExitCode] =
    val program =
      for
        jreClasspath <- javaHome.fold(JreClasspath.jrtPath())(JreClasspath.jrtPath)
        result   <- ContextResource.makeFromCoord(coord, jreClasspath, extraRepositories).use { (ctx, classpath) =>
          given tastyquery.Contexts.Context = ctx
          runCore(query, limit, classpath, jreClasspath)
        }
      yield result

    program.handleErrorWith { case e: Throwable =>
      Console[IO].errorln(e.getMessage).as(ExitCode.Error)
    }

  def runCore(
      query: String,
      limit: Int,
      classpath: tastyquery.Classpaths.Classpath,
      jreClasspath: tastyquery.Classpaths.Classpath
  )(using tastyquery.Contexts.Context, Console[IO], Tracer[IO]): IO[ExitCode] =
    val lowerQuery = query.toLowerCase
    val matchingStream = AllSymbolsStream
      .stream(classpath, jreClasspath)
      .filter(sym => sym.name.toString.toLowerCase.contains(lowerQuery))
    // Rank exact-name matches first, then prefix matches, then by length — so
    // `Monad` beats `Bimonad`/`MonadError` regardless of declaration order.
    matchingStream.compile.toList.flatMap { allMatches =>
      val sorted  = allMatches.sortBy(sym => rankKey(lowerQuery, sym.name.toString))
      val limited = sorted.take(limit)
      val note    = if sorted.length > limit then
        Console[IO].errorln(s"Note: results truncated at $limit. Use --limit to increase.")
      else IO.unit
      limited.traverse_ { sym =>
        IO.blocking {
          val fqn = sym.displayFullName
          val sig = LineFormatter.formatLine(sym)
          s"$fqn — $sig"
        }.flatMap(Console[IO].println)
      } >> note.as(ExitCode.Success)
    }

  /**
   * Sort key for search results. `lowerQuery` must already be lower-cased.
   * Order: exact name match → prefix match → shorter name → alphabetical.
   */
  private[cellar] def rankKey(lowerQuery: String, name: String): (Int, Int, Int, String) =
    val lowerName = name.toLowerCase
    val exact     = if lowerName == lowerQuery then 0 else 1
    val prefix    = if lowerName.startsWith(lowerQuery) then 0 else 1
    (exact, prefix, name.length, name)

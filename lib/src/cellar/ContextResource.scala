package cellar

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import cellar.CoursierFetchClient.ResolvedClasspath
import coursierapi.Repository
import fs2.io.file.Path
import org.typelevel.log4cats.Logger
import org.typelevel.otel4s.trace.Tracer
import tastyquery.Classpaths.Classpath
import tastyquery.Contexts.Context
import tastyquery.jdk.ClasspathLoaders

object ContextResource:
  def make(jars: Seq[Path], jreClasspath: Classpath)(using
      tracer: Tracer[IO],
      logger: Logger[IO] = StderrLogger.off
  ): Resource[IO, (Context, Classpath)] =
    makeWithSources(ResolvedClasspath(jars, Map.empty), jreClasspath).map((ctx, cp, _) => (ctx, cp))

  /** Adds the Scala stdlib when `resolved` lacks one (Java-only artifacts and projects), since
    * tasty-query reads Java's `int` and `T[]` as `scala.Int` and `scala.Array`. `extraRepositories`
    * are only used to fetch it.
    */
  def makeWithSources(
      resolved: ResolvedClasspath,
      jreClasspath: Classpath,
      extraRepositories: Seq[Repository] = Seq.empty
  )(using
      tracer: Tracer[IO],
      logger: Logger[IO] = StderrLogger.off
  ): Resource[IO, (Context, Classpath, SourceJars)] =
    Resource.eval {
      tracer.span("tasty.context.init").surround {
        for
          full         <- withScalaLibrary(resolved, extraRepositories)
          jars          = full.jars
          _            <- logger.debug(s"loading ${jars.size} jar(s)")
          _            <- jars.traverse_(j => logger.debug(s"  $j"))
          loaded       <- IO.blocking(readClasspathRobust(jars.toList)).adaptError { case e =>
                            new RuntimeException(
                              s"Failed to load classpath (${e.getClass.getSimpleName}: ${e.getMessage}). " +
                                "If JRE paths are invalid, set JAVA_HOME or use --java-home.",
                              e
                            )
                          }
          (kept, jarClasspath, dropped) = loaded
          _            <- dropped.traverse_(p =>
                            logger.warn(s"dropped unreadable classpath entry (tasty-query MatchError): $p")
                          )
          classpath    = jreClasspath ++ jarClasspath
          ctx          <- IO.blocking(Context.initialize(classpath))
          _             = JavaParamNames.register(ctx, classpath)
          sourceJars   <- IO(SourceJars.pair(kept, jarClasspath, full.sourcesJars)).flatTap {
                            case Some(_) => IO.unit
                            case None    => logger.warn("classpath entries do not line up with jars; sources unavailable")
                          }
        yield (ctx, classpath, sourceJars.getOrElse(SourceJars.empty))
      }
    }

  private def withScalaLibrary(resolved: ResolvedClasspath, extraRepositories: Seq[Repository])(using
      Tracer[IO],
      Logger[IO]
  ): IO[ResolvedClasspath] =
    if resolved.jars.exists(j => DocstringExtractor.isStdlib(j.fileName.toString)) then IO.pure(resolved)
    else
      val stdlib = MavenCoordinate("org.scala-lang", "scala-library", TastyQueryStdlib.version)
      CoursierFetchClient
        .fetchClasspathWithSources(stdlib, extraRepositories)
        .map(s => ResolvedClasspath(resolved.jars ++ s.jars, resolved.sourcesJars ++ s.sourcesJars))
        .handleErrorWith(e =>
          Logger[IO].warn(s"could not fetch ${stdlib.render}; Java members may fail to resolve: ${e.getMessage}").as(resolved)
        )

  /** Reads the classpath, excluding paths that cause `MatchError` in tasty-query
    * (e.g. vendor-injected JRT modules such as the Azul CRS client). Returns the excluded paths
    * alongside the classpath so the caller can report them — dropping an entry silently can turn a
    * present symbol into a "not found".
    */
  private def readClasspathRobust(paths: List[Path], dropped: List[Path] = Nil): (List[Path], Classpath, List[Path]) =
    try (paths, ClasspathLoaders.read(paths.map(_.toNioPath)), dropped)
    catch
      case e: MatchError =>
        val bad = paths.find { p =>
          try { ClasspathLoaders.read(List(p.toNioPath)): Unit; false }
          catch case _: MatchError => true
        }
        bad match
          case Some(offender) => readClasspathRobust(paths.filterNot(_ == offender), offender :: dropped)
          case None           => throw e

  def makeFromCoord(
      coord: MavenCoordinate,
      jreClasspath: Classpath,
      extraRepositories: Seq[Repository] = Seq.empty
  )(using
      tracer: Tracer[IO],
      logger: Logger[IO] = StderrLogger.off
  ): Resource[IO, (Context, Classpath, SourceJars)] =
    Resource.eval(CoursierFetchClient.fetchClasspathWithSources(coord, extraRepositories)).flatMap { resolved =>
      makeWithSources(resolved, jreClasspath, extraRepositories).evalMap { (ctx, classpath, sourceJars) =>
        IO.blocking {
          if resolved.jars.nonEmpty then
            // Only the artifact's own jars: the stdlib `makeWithSources` may add always has symbols.
            val artifactJars = resolved.jars.map(_.toString).toSet
            val jarEntries   = classpath.filter(e => artifactJars(e.toString))
            val hasSymbols = jarEntries.exists { entry =>
              try ctx.findSymbolsByClasspathEntry(entry).nonEmpty
              catch case _: Exception => false
            }
            if !hasSymbols then throw CellarError.EmptyArtifact(coord)
          (ctx, classpath, sourceJars)
        }
      }
    }

package cellar

import cats.effect.{IO, Resource}
import cats.syntax.monadError.*
import cellar.build.BuildTool
import coursierapi.Repository
import tastyquery.Classpaths
import tastyquery.Classpaths.Classpath
import tastyquery.Contexts.Context
import tastyquery.jdk.ClasspathLoaders
import java.nio.file.Path

object ContextResource:
  def make(jars: Seq[Path], jreClasspath: Classpath): Resource[IO, (Context, Classpath)] =
    Resource.eval {
      for
        jarClasspath <- IO.blocking(readClasspathRobust(jars.toList)).adaptError { case e =>
                          new RuntimeException(
                            s"Failed to load classpath (${e.getClass.getSimpleName}: ${e.getMessage}). " +
                              "If JRE paths are invalid, set JAVA_HOME or use --java-home.",
                            e
                          )
                        }
        classpath    = jreClasspath ++ jarClasspath
        ctx          <- IO.blocking(Context.initialize(classpath))
      yield (ctx, classpath)
    }

  /** Reads the classpath, excluding paths that cause `MatchError` in tasty-query
    * (e.g. vendor-injected JRT modules such as the Azul CRS client).
    */
  private def readClasspathRobust(paths: List[Path]): Classpath =
    try ClasspathLoaders.read(paths)
    catch
      case e: MatchError =>
        val bad = paths.find { p =>
          try { ClasspathLoaders.read(List(p)); false }
          catch case _: MatchError => true
        }
        bad match
          case Some(offender) => readClasspathRobust(paths.filterNot(_ == offender))
          case None           => throw e

  def makeFromBuildTool(
      buildTool: BuildTool,
      module: Option[String],
      jreClasspath: Classpath
  ): Resource[IO, (Context, Classpath)] =
    Resource.eval(buildTool.extractClasspath(module)).flatMap { paths =>
      make(paths, jreClasspath)
    }

  def makeFromCoord(
      coord: MavenCoordinate,
      jreClasspath: Classpath,
      extraRepositories: Seq[Repository] = Seq.empty
  ): Resource[IO, (Context, Classpath)] =
    Resource.eval(CoursierFetchClient.fetchClasspath(coord, extraRepositories)).flatMap { jars =>
      make(jars, jreClasspath).evalMap { (ctx, classpath) =>
        IO.blocking {
          if jars.nonEmpty then
            val jarEntries = classpath.filter(_.toString.endsWith(".jar"))
            val hasSymbols = jarEntries.exists { entry =>
              try ctx.findSymbolsByClasspathEntry(entry).nonEmpty
              catch case _: Exception => false
            }
            if !hasSymbols then throw CellarError.EmptyArtifact(coord)
          (ctx, classpath)
        }
      }
    }

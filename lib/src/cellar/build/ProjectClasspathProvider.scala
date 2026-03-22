package cellar.build

import cats.effect.{IO, Resource}
import cellar.ContextResource
import java.nio.file.Path
import tastyquery.Classpaths.Classpath
import tastyquery.Contexts.Context

object ProjectClasspathProvider:
  def provide(
      cwd: Path,
      module: Option[String],
      jreClasspath: Classpath,
      noCache: Boolean,
      millBinary: String = "./mill"
  ): Resource[IO, (Context, Classpath)] =
    Resource.eval(resolveClasspath(cwd, module, noCache, millBinary)).flatMap { paths =>
      ContextResource.make(paths, jreClasspath)
    }

  private def resolveClasspath(cwd: Path, module: Option[String], noCache: Boolean, millBinary: String): IO[List[Path]] =
    BuildToolDetector.detectKind(cwd).flatMap { kind =>
      val buildTool = instantiate(kind, cwd, millBinary)
      val useCache = kind != BuildToolKind.ScalaCli && !noCache

      if useCache then cachedFlow(buildTool, module, cwd)
      else buildTool.extractClasspath(module)
    }

  private def cachedFlow(buildTool: BuildTool, module: Option[String], cwd: Path): IO[List[Path]] =
    val cache = ClasspathCache(cwd)
    val moduleKey = module.getOrElse("")

    for
      fingerFiles <- buildTool.fingerprintFiles()
      hash        <- BuildFingerprint.compute(fingerFiles, moduleKey)
      cached      <- cache.get(hash)
      paths <- cached match
        case Some(paths) => buildTool.compile(module).as(paths)
        case None        => buildTool.extractClasspath(module).flatTap(paths => cache.put(hash, paths))
    yield paths

  private def instantiate(kind: BuildToolKind, cwd: Path, millBinary: String): BuildTool = kind match
    case BuildToolKind.Mill     => MillBuildTool(cwd, millBinary)
    case BuildToolKind.Sbt      => SbtBuildTool(cwd)
    case BuildToolKind.ScalaCli => ScalaCliBuildTool(cwd)

package cellar

import fs2.io.file.Path
import pureconfig.*

case class MillConfig(binary: String) derives ConfigReader

case class SbtConfig(binary: String, extraArgs: String) derives ConfigReader {
  def effectiveExtraArgs: List[String] = extraArgs.split("\\s+").filter(_.nonEmpty).toList
}

case class StarvationChecksConfig(enabled: Boolean) derives ConfigReader

case class ProfilingConfig(enabled: Boolean, pyroscopeEndpoint: String) derives ConfigReader

case class OtelConfig(enabled: Boolean, endpoint: String) derives ConfigReader

/** Extra Maven repository URLs added to Coursier's defaults for every external command. */
case class MavenConfig(repositories: List[String]) derives ConfigReader

case class Config(
    maven: MavenConfig,
    mill: MillConfig,
    sbt: SbtConfig,
    starvationChecks: StarvationChecksConfig,
    profiling: ProfilingConfig,
    otel: OtelConfig
) derives ConfigReader

object Config {
  private val defaultUserPath: Option[Path] =
    sys.props.get("user.home").map(Path(_).resolve(".cellar").resolve("cellar.conf"))
  private[cellar] val defaultProjectPath: Path = Path(".cellar").resolve("cellar.conf")

  /** Loads with explicit file locations so tests never touch the real user configuration. */
  private[cellar] def loadFrom(userPath: Option[Path], projectPath: Option[Path]): Config = {
    val paths = userPath.toList ++ projectPath.toList
    paths
      .foldLeft(ConfigSource.default)((cs, p) => ConfigSource.file(p.toNioPath).optional.withFallback(cs))
      .loadOrThrow[Config]
  }

  private def load(): Config = loadFrom(defaultUserPath, Some(defaultProjectPath))

  lazy val global: Config = load()

  def loadFresh(): Config = load()
}

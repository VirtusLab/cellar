package cellar

import cats.syntax.all.*
import fs2.io.file.Path
import pureconfig.*

case class MillConfig(binary: String) derives ConfigReader

case class SbtConfig(binary: String, extraArgs: String) derives ConfigReader {
  def effectiveExtraArgs: List[String] = extraArgs.split("\\s+").filter(_.nonEmpty).toList
}

case class StarvationChecksConfig(enabled: Boolean) derives ConfigReader

case class ProfilingConfig(enabled: Boolean, pyroscopeEndpoint: String) derives ConfigReader

case class OtelConfig(enabled: Boolean, endpoint: String) derives ConfigReader

case class UpdateCheckConfig(enabled: Boolean) derives ConfigReader

/** Extra Maven repository URLs added to Coursier's defaults for every external command. */
case class MavenConfig(repositories: List[String]) derives ConfigReader

/**
 * JVM system properties as `key=value` entries, the format scala-cli's `java.properties` uses. The native image
 * ignores `JAVA_TOOL_OPTIONS`, so this is how a custom trust store reaches it.
 */
case class JavaConfig(properties: List[String]) derives ConfigReader {
  def parsed: Either[String, List[(String, String)]] =
    properties.traverse { entry =>
      entry.split("=", 2).map(_.trim) match {
        case Array(key, value) if key.nonEmpty => Right(key -> value)
        case _                                 => Left(s"Invalid java.properties entry '$entry': expected key=value")
      }
    }
}

case class Config(
    maven: MavenConfig,
    java: JavaConfig,
    mill: MillConfig,
    sbt: SbtConfig,
    starvationChecks: StarvationChecksConfig,
    profiling: ProfilingConfig,
    otel: OtelConfig,
    updateCheck: UpdateCheckConfig
) derives ConfigReader

object Config {
  private val defaultUserPath: Option[Path] =
    sys.props.get("user.home").map(Path(_).resolve(".cellar").resolve("cellar.conf"))
  private[cellar] val defaultProjectPath: Path = Path(".cellar").resolve("cellar.conf")

  /** Loads with explicit file locations so tests never touch the real user configuration. */
  private[cellar] def loadFrom(userPath: Option[Path], projectPath: Option[Path]): Config = {
    def layered(paths: List[Path]): ConfigSource =
      paths.foldLeft(ConfigSource.default)((cs, p) => ConfigSource.file(p.toNioPath).optional.withFallback(cs))

    val merged = layered(userPath.toList ++ projectPath.toList).loadOrThrow[Config]
    // A checked-out repo must not choose the trust store or proxy that downloads and repository credentials go through.
    merged.copy(java = layered(userPath.toList).at("java").loadOrThrow[JavaConfig])
  }

  private def load(): Config = loadFrom(defaultUserPath, Some(defaultProjectPath))

  lazy val global: Config = load()

  def loadFresh(): Config = load()
}

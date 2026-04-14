package cellar

import fs2.io.file.Path
import pureconfig.*

case class MillConfig(binary: String) derives ConfigReader

case class SbtConfig(binary: String, extraArgs: String) derives ConfigReader {
  def effectiveExtraArgs: List[String] = extraArgs.split("\\s+").filter(_.nonEmpty).toList
}

case class StarvationChecksConfig(enabled: Boolean) derives ConfigReader

case class Config(mill: MillConfig, sbt: SbtConfig, starvationChecks: StarvationChecksConfig) derives ConfigReader

object Config {
  val defaultUserPath: Option[Path] =
    sys.props.get("user.home").map(Path(_).resolve(".cellar").resolve("cellar.conf"))
  val defaultProjectPath: Path = Path(".cellar").resolve("cellar.conf")

  /** Memoized bootstrap config loaded from default locations. Accessed before
    * the IO runtime starts (from `IOApp.runtimeConfig`), so it must be
    * synchronous. Throws `ConfigReaderException` on malformed config.
    */
  lazy val bootstrap: Config = loadSync(None)

  def loadSync(path: Option[Path]): Config = {
    val paths = path match
      case Some(p) => List(p)
      case None =>
        (defaultUserPath.toList ++ List(defaultProjectPath))
          .filter(p => java.nio.file.Files.exists(p.toNioPath))
    paths
      .foldLeft(ConfigSource.default)((cs, p) => ConfigSource.file(p.toNioPath).withFallback(cs))
      .loadOrThrow[Config]
  }
}

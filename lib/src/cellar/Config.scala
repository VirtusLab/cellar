package cellar

import cats.effect.IO
import cats.syntax.all.*
import fs2.io.file.{Files, Path}
import pureconfig.*

case class MillConfig(binary: String) derives ConfigReader

case class SbtConfig(binary: String, extraArgs: String) derives ConfigReader {
  def effectiveExtraArgs: List[String] = extraArgs.split("\\s+").filter(_.nonEmpty).toList
}

case class Config(mill: MillConfig, sbt: SbtConfig, suppressStarvationWarnings: Boolean = true) derives ConfigReader

object Config {
  lazy val default: IO[Config] = load(None)

  val defaultUserPath: Option[Path] =
    sys.props.get("user.home").map(Path(_).resolve(".cellar").resolve("cellar.conf"))
  val defaultProjectPath: Path = Path(".cellar").resolve("cellar.conf")

  def load(path: Option[Path]): IO[Config] = {
    def load0(path: List[Path]) =
      IO.blocking {
        path.foldLeft(ConfigSource.default)((cs, p) => ConfigSource.file(p.toNioPath).withFallback(cs)).loadOrThrow[Config]
      }

    path match
      case sp: Some[_] => load0(sp.toList)
      case None =>
        val relevantPaths = defaultUserPath.toList ++ List(defaultProjectPath)
        relevantPaths.filterA(p => Files[IO].exists(p)).flatMap(load0)
  }
}

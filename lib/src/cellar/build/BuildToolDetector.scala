package cellar.build

import cats.effect.IO
import java.nio.file.{Files, Path}

enum BuildToolKind:
  case Mill, Sbt, ScalaCli

object BuildToolDetector:
  private val millMarkers = List("build.mill", "build.sc", "build.mill.yaml", "build.yaml")

  /** Detect the build tool kind from marker files only (no binary check). */
  def detectKind(dir: Path): IO[BuildToolKind] =
    IO.blocking {
      val millMarker = millMarkers.find(m => Files.exists(dir.resolve(m)))
      val hasSbt = Files.exists(dir.resolve("build.sbt"))
      val hasScalaBuild = Files.isDirectory(dir.resolve(".scala-build"))

      if millMarker.isDefined then BuildToolKind.Mill
      else if hasSbt then BuildToolKind.Sbt
      else if hasScalaBuild then BuildToolKind.ScalaCli
      else BuildToolKind.ScalaCli // fallback
    }


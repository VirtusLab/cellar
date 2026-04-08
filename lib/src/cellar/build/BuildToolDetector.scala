package cellar.build

import cats.effect.IO
import cats.syntax.all.*
import fs2.io.file.{Files, Path}

enum BuildToolKind:
  case Mill, Sbt, ScalaCli

object BuildToolDetector:
  private val millMarkers = List("build.mill", "build.sc", "build.mill.yaml", "build.yaml")

  /** Detect the build tool kind from marker files only (no binary check). */
  def detectKind(dir: Path): IO[BuildToolKind] = {
    val hasMill = millMarkers.existsM(m => Files[IO].exists(dir.resolve(m)))
    val hasSbt = Files[IO].exists(dir.resolve("build.sbt"))
    val hasScalaCli = Files[IO].isDirectory(dir.resolve(".scala-build"))

    BuildToolKind.values.toList.findM { // sequentially check each until one is true
      case BuildToolKind.Mill => hasMill
      case BuildToolKind.Sbt => hasSbt
      case BuildToolKind.ScalaCli => hasScalaCli
    }.map(_.getOrElse(BuildToolKind.ScalaCli)) // default to Scala CLI if none was found
  }


package cellar.build

import java.nio.file.{Files, Path}

object ClasspathOutputParser:

  /** Parse Mill's JSON array output: `["ref:hash:/path", "/path2"]` */
  def parseJsonArray(input: String, checkExists: Boolean = true): Either[String, List[Path]] =
    val trimmed = input.trim
    if !trimmed.startsWith("[") || !trimmed.endsWith("]") then
      return Left(s"Expected JSON array but got: ${trimmed.take(100)}")

    val inner = trimmed.substring(1, trimmed.length - 1).trim
    if inner.isEmpty then return Left("Build tool produced an empty classpath.")

    val entries = inner.split(",").map(_.trim.stripPrefix("\"").stripSuffix("\"")).toList
    val paths = entries.map { entry =>
      // Mill outputs paths as plain strings, "ref:<hash>:<path>", or "qref:v1:<hash>:<path>"
      // Extract the absolute path portion by finding ":/" which precedes the absolute path
      val path = entry.lastIndexOf(":/") match
        case -1  => entry // plain path
        case idx => entry.substring(idx + 1) // strip prefix up to the path
      Path.of(path)
    }

    val filtered = if checkExists then paths.filter(p => Files.exists(p)) else paths
    if filtered.isEmpty then Left("Build tool produced an empty classpath (all paths filtered).")
    else Right(filtered)

  /** Parse colon-separated classpath output (sbt, scala-cli) */
  def parseColonSeparated(input: String): Either[String, List[Path]] =
    val entries = input.trim
      .split(":")
      .map(_.trim)
      .filter(_.nonEmpty)
      .toList

    if entries.isEmpty then Left("Build tool produced an empty classpath.")
    else Right(entries.map(Path.of(_)))

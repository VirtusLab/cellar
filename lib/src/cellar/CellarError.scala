package cellar

import java.nio.file.Path

sealed trait CellarError extends Throwable

object CellarError:
  final case class CoordinateNotFound(
      coord: MavenCoordinate,
      cause: Throwable,
      suggestions: List[String] = List.empty
  ) extends CellarError:
    override def getMessage: String =
      val base = s"Could not resolve '${coord.render}'."
      if suggestions.isEmpty then
        s"$base Check that the group ID, artifact ID, and version are correct."
      else if suggestions.head.startsWith("Artifact exists.") then
        s"$base\n\n${suggestions.head}"
      else
        val hint = suggestions.map(s => s"  $s").mkString("\n")
        s"$base\n\nDid you mean?\n$hint"
    override def getCause: Throwable = cause

  final case class SymbolNotFound(fqn: String, coord: MavenCoordinate, nearMatches: List[String])
      extends CellarError:
    override def getMessage: String =
      val base = s"Symbol '$fqn' not found in '${coord.render}'."
      if nearMatches.isEmpty then base
      else s"$base Did you mean one of: ${nearMatches.mkString(", ")}?"

  final case class PartialResolution(fqn: String, coord: MavenCoordinate, resolvedFqn: String, missingMember: String)
      extends CellarError:
    override def getMessage: String =
      s"Symbol '$fqn' not found in '${coord.render}'. Resolved up to '$resolvedFqn' but member '$missingMember' was not found."

  final case class PackageGivenToGet(fqn: String) extends CellarError:
    override def getMessage: String =
      s"'$fqn' is a package, not a symbol. Use 'cellar list ${fqn}' to explore package contents."

  final case class EmptyArtifact(coord: MavenCoordinate) extends CellarError:
    override def getMessage: String =
      s"Artifact '${coord.render}' resolved but contains no extractable symbols. No .tasty, pickle, or .class files were found."

  final case class ShadedDuplicate(fqn: String, firstJar: Path, duplicateJar: Path)
      extends CellarError:
    override def getMessage: String =
      s"Symbol '$fqn' exists in multiple JARs on the classpath: '$firstJar' and '$duplicateJar'."

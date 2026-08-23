package cellar.cli

import coursierapi.MavenRepository

import java.net.URI
import scala.util.Try

/** `MavenRepository.of` accepts any string verbatim, so a typo such as a missing scheme survives
  * until resolution fails — and it then fails as a coordinate error, pointing at the coordinate
  * rather than at the repository. Rejecting the URL up front keeps the blame where it belongs.
  */
object RepositoryUrl:

  private val supportedSchemes = List("http", "https", "file")

  private def expected = supportedSchemes.map(s => s"$s://").mkString(", ")

  /** Left carries the reason only; the caller prefixes it with where the URL came from. */
  def parse(raw: String): Either[String, MavenRepository] =
    val trimmed = raw.trim
    if trimmed.isEmpty then Left("the URL is empty")
    else
      Try(URI(trimmed)).toEither.left
        .map(_ => s"'$raw' is not a valid URL")
        .flatMap { uri =>
          Option(uri.getScheme).map(_.toLowerCase) match
            case None =>
              Left(s"'$raw' has no scheme (expected one of $expected)")
            case Some(scheme) if !supportedSchemes.contains(scheme) =>
              Left(s"'$raw' has unsupported scheme '$scheme' (expected one of $expected)")
            case Some(scheme) if scheme != "file" && Option(uri.getHost).forall(_.isEmpty) =>
              Left(s"'$raw' has no host")
            case Some(_) =>
              Right(MavenRepository.of(trimmed))
        }

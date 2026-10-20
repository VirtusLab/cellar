package cellar.cli

import coursier.MavenRepository
import coursier.parse.RepositoryParser

import java.net.URI
import scala.util.Try

/** `MavenRepository(...)` accepts any string verbatim, so a typo such as a missing scheme survives
  * until resolution fails — and it then fails as a coordinate error, pointing at the coordinate
  * rather than at the repository. Rejecting the URL up front keeps the blame where it belongs.
  *
  * Credentials in the URL (`https://user:token@host/...`) become the repository's own and are sent
  * with the first request; error messages mask the password.
  */
object RepositoryUrl:

  private val supportedSchemes = List("http", "https", "file")

  private def expected = supportedSchemes.map(s => s"$s://").mkString(", ")

  /** Left carries the reason only; the caller prefixes it with where the URL came from. */
  def parse(raw: String): Either[String, MavenRepository] =
    val trimmed = raw.trim
    val shown   = masked(trimmed)
    if trimmed.isEmpty then Left("the URL is empty")
    else
      Try(URI(trimmed)).toEither.left
        .map(_ => s"'$shown' is not a valid URL")
        .flatMap { uri =>
          Option(uri.getScheme).map(_.toLowerCase) match
            case None =>
              Left(s"'$shown' has no scheme (expected one of $expected)")
            case Some(scheme) if !supportedSchemes.contains(scheme) =>
              Left(s"'$shown' has unsupported scheme '$scheme' (expected one of $expected)")
            case Some(scheme) if scheme != "file" && Option(uri.getHost).forall(_.isEmpty) =>
              Left(s"'$shown' has no host")
            case Some(_) =>
              RepositoryParser.repository(trimmed).left.map(_ => s"'$shown' is not a valid URL").flatMap {
                case maven: MavenRepository => Right(maven)
                case _                      => Left(s"'$shown' is not a Maven repository URL")
              }
        }

  private def masked(url: String): String =
    url.replaceFirst("^([a-zA-Z][a-zA-Z0-9+.-]*://[^:/@]*):[^@/]*@", "$1:****@")

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
  * with the first request; error messages mask them.
  */
object RepositoryUrl:

  private val supportedSchemes = List("http", "https", "file")

  private def expected = supportedSchemes.map(s => s"$s://").mkString(", ")

  /** Left carries the reason only; the caller prefixes it with where the URL came from. */
  def parse(raw: String): Either[String, MavenRepository] =
    val trimmed = raw.trim
    val shown   = masked(trimmed)
    // coursier does not URL-decode credentials, so these characters cannot be written in any form.
    val credentialsHint =
      if shown == trimmed then ""
      else "; credentials in a URL cannot contain '/', '?', '#' or '@', use COURSIER_CREDENTIALS instead"
    if trimmed.isEmpty then Left("the URL is empty")
    else
      Try(URI(trimmed)).toEither.left
        .map(_ => s"'$shown' is not a valid URL$credentialsHint")
        .flatMap { uri =>
          Option(uri.getScheme).map(_.toLowerCase) match
            case None =>
              Left(s"'$shown' has no scheme (expected one of $expected)")
            case Some(scheme) if !supportedSchemes.contains(scheme) =>
              Left(s"'$shown' has unsupported scheme '$scheme' (expected one of $expected)")
            case Some(scheme) if scheme != "file" && Option(uri.getHost).forall(_.isEmpty) =>
              Left(s"'$shown' has no host$credentialsHint")
            case Some(_) =>
              // coursier's own message would echo the password; a scheme checked above always
              // parses to a Maven repository.
              RepositoryParser.repository(trimmed).left
                .map(_ => s"'$shown' is not a valid URL$credentialsHint")
                .map { case maven: MavenRepository => maven }
        }

  /** Hides everything between the scheme and the last `@`. A token may itself contain `/`, `@` or
    * `#`, so the usual delimiters cannot tell where the credentials end.
    */
  private def masked(url: String): String =
    val schemeEnd = url.indexOf("://")
    val at        = url.lastIndexOf('@')
    if schemeEnd >= 0 && at > schemeEnd then url.take(schemeEnd + 3) + "****" + url.drop(at) else url

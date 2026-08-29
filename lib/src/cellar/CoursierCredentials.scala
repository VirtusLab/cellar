package cellar

import cats.effect.IO
import coursierapi.{Credentials, MavenRepository, Repository}
import fs2.io.file.{Files, Path}

import java.io.StringReader
import java.net.URI
import java.util.Properties
import scala.jdk.CollectionConverters.*

/** Reads the credentials coursier itself would use (`COURSIER_CREDENTIALS`, then
  * `credentials.properties` in the coursier config dir) and pins them onto each repository.
  *
  * coursier-interface already attaches these to its cache, but only the metadata (POM) requests
  * see them: the artifact download path is built with a separate default cache and never retries
  * a 401 with credentials. Repository-level credentials travel with every artifact request, so
  * they are the only way to reach the JAR download.
  */
object CoursierCredentials:
  def load(
      env: Map[String, String] = sys.env,
      home: Path = Path(sys.props("user.home"))
  ): IO[List[Credentials]] =
    env.get("COURSIER_CREDENTIALS").filter(_.trim.nonEmpty) match
      case Some(value) if value.startsWith("/")     => fromFile(Path(value))
      case Some(value) if value.startsWith("file:") => fromFile(Path(URI.create(value).getPath))
      case Some(value)                              => IO.pure(parseInline(value))
      case None =>
        val configDir = env.get("COURSIER_CONFIG_DIR").map(Path(_)).getOrElse {
          val xdg = env.get("XDG_CONFIG_HOME").map(Path(_)).getOrElse(home / ".config")
          if sys.props("os.name").toLowerCase.contains("mac") then home / "Library" / "Application Support" / "Coursier"
          else xdg / "coursier"
        }
        fromFile(configDir / "credentials.properties")

  def applyTo(repo: Repository, credentials: List[Credentials]): Repository = repo match
    case mvn: MavenRepository if mvn.getCredentials == null =>
      val uri = URI.create(mvn.getBase)
      credentials
        .find(c => c.getHost == uri.getHost && (!c.isHttpsOnly || uri.getScheme == "https"))
        .fold(repo)(mvn.withCredentials)
    case other => other

  def parseInline(content: String): List[Credentials] =
    content.linesIterator.map(_.trim).filter(_.nonEmpty).toList.flatMap {
      case s"$host($realm) $user:$password" => Some(Credentials.of(host, user, password, realm))
      case s"$host $user:$password"         => Some(Credentials.of(host, user, password))
      case _                                => None
    }

  def parseProperties(props: Properties): List[Credentials] =
    props.stringPropertyNames.asScala.toList.filter(_.endsWith(".username")).sorted.flatMap { userKey =>
      val prefix = userKey.stripSuffix(".username")
      for
        host     <- Option(props.getProperty(s"$prefix.host"))
        password <- Option(props.getProperty(s"$prefix.password"))
      yield
        val base = Credentials.of(host, props.getProperty(userKey), password, props.getProperty(s"$prefix.realm"))
        Option(props.getProperty(s"$prefix.https-only")).map(_.toBoolean).fold(base.withHttpsOnly(true))(base.withHttpsOnly)
    }

  private def fromFile(path: Path): IO[List[Credentials]] =
    Files[IO].isRegularFile(path).flatMap {
      case false => IO.pure(Nil)
      case true  =>
        Files[IO].readUtf8(path).compile.string.map { content =>
          val props = new Properties
          props.load(new StringReader(content))
          parseProperties(props)
        }.handleError(_ => Nil)
    }

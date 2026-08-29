package cellar

import coursierapi.MavenRepository
import fs2.io.file.Files

import cats.effect.IO
import cats.effect.unsafe.implicits.global

import java.util.Properties

class CoursierCredentialsTest extends munit.FunSuite:

  private def props(pairs: (String, String)*): Properties =
    val p = new Properties
    pairs.foreach((k, v) => p.setProperty(k, v))
    p

  test("parseProperties reads a coursier credentials.properties entry"):
    val creds = CoursierCredentials.parseProperties(
      props("art.host" -> "artifactory.example.com", "art.username" -> "alice", "art.password" -> "s3cret", "art.realm" -> "Artifactory Realm")
    )
    assertEquals(creds.map(c => (c.getHost, c.getUser, c.getPassword, c.getRealm, c.isHttpsOnly)),
      List(("artifactory.example.com", "alice", "s3cret", "Artifactory Realm", true)))

  test("parseProperties honours https-only=false and skips entries without host"):
    val creds = CoursierCredentials.parseProperties(
      props("a.host" -> "h", "a.username" -> "u", "a.password" -> "p", "a.https-only" -> "false", "b.username" -> "u", "b.password" -> "p")
    )
    assertEquals(creds.map(c => (c.getHost, c.isHttpsOnly)), List(("h", false)))

  test("parseInline reads COURSIER_CREDENTIALS lines with and without realm"):
    val creds = CoursierCredentials.parseInline("  h1(My Realm) u1:p1\n\nh2 u2:p2\n")
    assertEquals(creds.map(c => (c.getHost, c.getUser, c.getPassword, Option(c.getRealm))),
      List(("h1", "u1", "p1", Some("My Realm")), ("h2", "u2", "p2", None)))

  test("applyTo attaches credentials only to the repository whose host matches"):
    val creds = CoursierCredentials.parseInline("artifactory.example.com alice:s3cret")
    val hit   = CoursierCredentials.applyTo(MavenRepository.of("https://artifactory.example.com/maven"), creds)
    val miss  = CoursierCredentials.applyTo(MavenRepository.of("https://repo1.maven.org/maven2"), creds)
    assertEquals(hit.asInstanceOf[MavenRepository].getCredentials.getUser, "alice")
    assertEquals(miss.asInstanceOf[MavenRepository].getCredentials, null)

  test("applyTo skips https-only credentials for an http repository"):
    val creds = CoursierCredentials.parseProperties(props("a.host" -> "h", "a.username" -> "u", "a.password" -> "p"))
    val repo  = CoursierCredentials.applyTo(MavenRepository.of("http://h/maven"), creds)
    assertEquals(repo.asInstanceOf[MavenRepository].getCredentials, null)

  test("load prefers COURSIER_CREDENTIALS and falls back to the config dir file"):
    val dir = Files[IO].createTempDirectory.unsafeRunSync()
    fs2.Stream.emit("a.host=filehost\na.username=u\na.password=p\n").through(Files[IO].writeUtf8(dir / "credentials.properties")).compile.drain.unsafeRunSync()
    val fromFile = CoursierCredentials.load(Map("COURSIER_CONFIG_DIR" -> dir.toString), dir).unsafeRunSync()
    val fromEnv  = CoursierCredentials.load(Map("COURSIER_CREDENTIALS" -> "envhost u:p", "COURSIER_CONFIG_DIR" -> dir.toString), dir).unsafeRunSync()
    val missing  = CoursierCredentials.load(Map.empty, dir).unsafeRunSync()
    assertEquals(fromFile.map(_.getHost), List("filehost"))
    assertEquals(fromEnv.map(_.getHost), List("envhost"))
    assertEquals(missing, Nil)

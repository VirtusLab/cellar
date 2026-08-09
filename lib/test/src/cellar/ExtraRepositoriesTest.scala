package cellar

import cats.effect.IO
import cats.syntax.all.*
import coursierapi.MavenRepository
import fs2.io.file.{Files => Fs2Files, Path}
import munit.CatsEffectSuite
import org.typelevel.otel4s.trace.Tracer.Implicits.noop

class ExtraRepositoriesTest extends CatsEffectSuite:

  private def withConfigFiles(userConfig: Option[String], projectConfig: Option[String])(
      test: Config => IO[Unit]
  ): IO[Unit] =
    Fs2Files[IO].tempDirectory.use { dir =>
      def write(name: String, contents: String): IO[Path] =
        val file = dir.resolve(name)
        fs2.Stream.emit(contents).through(Fs2Files[IO].writeUtf8(file)).compile.drain.as(file)

      for
        userPath    <- userConfig.traverse(write("user.conf", _))
        projectPath <- projectConfig.traverse(write("project.conf", _))
        _           <- test(Config.loadFrom(userPath, projectPath))
      yield ()
    }

  private def bases(configured: List[String], commandLine: List[String]): List[String] =
    ExtraRepositories.effective(configured.map(MavenRepository.of), commandLine.map(MavenRepository.of)).map {
      case maven: MavenRepository => maven.getBase
      case other                  => fail(s"Expected a MavenRepository, got $other")
    }

  test("configured repositories come first, command-line values append"):
    assertEquals(
      bases(List("https://configured.example/maven"), List("https://cli.example/maven")),
      List("https://configured.example/maven", "https://cli.example/maven")
    )

  test("duplicates collapse to their first occurrence, ignoring a trailing slash"):
    assertEquals(
      bases(List("https://repo.example/maven"), List("https://repo.example/maven/", "https://other.example/maven")),
      List("https://repo.example/maven", "https://other.example/maven")
    )

  test("no configured repositories leaves command-line values untouched"):
    assertEquals(bases(Nil, List("https://cli.example/maven")), List("https://cli.example/maven"))

  test("default configuration has no extra repositories"):
    withConfigFiles(None, None)(config => IO(assertEquals(config.maven.repositories, Nil)))

  test("empty project list clears repositories inherited from the user config"):
    withConfigFiles(
      userConfig = Some("""maven.repositories = ["https://configured.example/maven"]"""),
      projectConfig = Some("maven.repositories = []")
    )(config => IO(assertEquals(config.maven.repositories, Nil)))

  test("configured repository resolves a fixture artifact without a command-line repository"):
    TestFixtures.assumeFixturesAvailable()
    withConfigFiles(
      userConfig = Some(s"""maven.repositories = ["file://${TestFixtures.localM2}"]"""),
      projectConfig = None
    ) { config =>
      val repositories = ExtraRepositories.effective(config.maven.repositories.map(MavenRepository.of), Nil)
      CoursierFetchClient
        .fetchClasspath(TestFixtures.scala3Coord, repositories)
        .map(paths => assert(paths.nonEmpty, "Expected the fixture JAR to resolve through the configured repository"))
    }

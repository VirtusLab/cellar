package cellar

import cats.effect.IO
import cats.syntax.all.*
import coursier.MavenRepository
import fs2.io.file.{Files => Fs2Files, Path}
import org.typelevel.otel4s.trace.Tracer.Implicits.noop
import weaver.{Expectations, SimpleIOSuite}

object ExtraRepositoriesTest extends SimpleIOSuite:

  private def withConfigFiles(userConfig: Option[String], projectConfig: Option[String])(
      test: Config => IO[Expectations]
  ): IO[Expectations] =
    Fs2Files[IO].tempDirectory.use { dir =>
      def write(name: String, contents: String): IO[Path] =
        val file = dir.resolve(name)
        fs2.Stream.emit(contents).through(Fs2Files[IO].writeUtf8(file)).compile.drain.as(file)

      for
        userPath    <- userConfig.traverse(write("user.conf", _))
        projectPath <- projectConfig.traverse(write("project.conf", _))
        result      <- test(Config.loadFrom(userPath, projectPath))
      yield result
    }

  private def bases(configured: List[String], commandLine: List[String]): List[String] =
    ExtraRepositories.effective(configured.map(MavenRepository(_)), commandLine.map(MavenRepository(_))).map {
      case maven: MavenRepository => maven.root
      case other                  => s"Expected a MavenRepository, got $other"
    }

  pureTest("configured repositories come first, command-line values append"):
    expect.eql(
      List("https://configured.example/maven", "https://cli.example/maven"),
      bases(List("https://configured.example/maven"), List("https://cli.example/maven"))
    )

  pureTest("duplicates collapse to their first occurrence, ignoring a trailing slash"):
    expect.eql(
      List("https://repo.example/maven", "https://other.example/maven"),
      bases(List("https://repo.example/maven"), List("https://repo.example/maven/", "https://other.example/maven"))
    )

  pureTest("a duplicate carrying credentials wins over the configured one without"):
    val withCredentials = MavenRepository("https://repo.example/maven", Some(coursier.core.Authentication("u", "p")))
    expect.eql(
      List(Some("u")),
      ExtraRepositories.effective(List(MavenRepository("https://repo.example/maven")), List(withCredentials)).map {
        case maven: MavenRepository => maven.authentication.flatMap(_.userOpt)
        case _                      => None
      }
    )

  pureTest("no configured repositories leaves command-line values untouched"):
    expect.eql(List("https://cli.example/maven"), bases(Nil, List("https://cli.example/maven")))

  test("default configuration has no extra repositories"):
    withConfigFiles(None, None)(config => IO(expect.eql(List.empty[String], config.maven.repositories)))

  test("empty project list clears repositories inherited from the user config"):
    withConfigFiles(
      userConfig = Some("""maven.repositories = ["https://configured.example/maven"]"""),
      projectConfig = Some("maven.repositories = []")
    )(config => IO(expect.eql(List.empty[String], config.maven.repositories)))

  test("configured repository resolves a fixture artifact without a command-line repository"):
    TestFixtures.assumeFixturesAvailable *>
      withConfigFiles(
        userConfig = Some(s"""maven.repositories = ["file://${TestFixtures.localM2}"]"""),
        projectConfig = None
      ) { config =>
        val repositories = ExtraRepositories.effective(config.maven.repositories.map(MavenRepository(_)), Nil)
        CoursierFetchClient
          .fetchClasspath(TestFixtures.scala3Coord, repositories)
          .map(paths => expect(paths.nonEmpty, "Expected the fixture JAR to resolve through the configured repository"))
      }

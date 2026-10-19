package cellar

import cats.effect.{ExitCode, IO}
import cats.effect.std.Console
import cats.syntax.all.*
import cellar.process.ProcessRunner
import org.typelevel.otel4s.trace.Tracer.Implicits.noop
import weaver.Expectations

import java.nio.file.Files
import fs2.io.file.{Files => Fs2Files, Path}

/** Integration tests for project-aware commands.
  *
  * Tests require build tools (scala-cli, mill) on PATH.
  * Tests that require unavailable tools are ignored via `requireTool`.
  */
object ProjectAwareIntegrationTest extends SequentialIOSuite:

  private def safeRun(f: IO[ExitCode])(using Console[IO]): IO[ExitCode] =
    f.handleErrorWith(e => Console[IO].errorln(e.getMessage).as(ExitCode.Error))

  private lazy val millBinary: String =
    Option(System.getProperty("cellar.test.millBinary")).getOrElse("mill")

  private def isOnPath(binary: String): IO[Boolean] =
    ProcessRunner.run("which", List(binary)).map(_.exitCode == 0).handleError(_ => false)

  private def isBinaryAvailable(binary: String): IO[Boolean] =
    (Fs2Files[IO].isRegularFile(Path(binary)), isOnPath(binary)).mapN(_ || _)

  private def requireTool(available: IO[Boolean], reason: String): IO[Unit] =
    available.flatMap(IO.unlessA(_)(ignore[IO](reason)))

  private def withTempDir(test: Path => IO[Expectations]): IO[Expectations] =
    Fs2Files[IO].tempDirectory.use(test)

  // --- ProcessRunner tests ---

  test("ProcessRunner: echo captures stdout"):
    process.ProcessRunner.run("echo", List("hello")).map { result =>
      expect.eql(0, result.exitCode) and
        expect.eql("hello", result.stdout.trim) and
        expect(result.stderr.isEmpty)
    }

  test("ProcessRunner: non-zero exit captures stderr"):
    process.ProcessRunner.run("sh", List("-c", "echo err >&2; exit 1")).map { result =>
      expect.eql(1, result.exitCode) and
        expect(result.stderr.contains("err"))
    }

  test("ProcessRunner: command not found"):
    process.ProcessRunner.run("nonexistent-command-xyz", Nil).attempt.map { result =>
      expect(result.isLeft) and
        expect(result.left.exists(_.getMessage.contains("nonexistent-command-xyz")))
    }

  test("ProcessRunner: working directory"):
    process.ProcessRunner.run("pwd", Nil, Some(Path("/tmp"))).map { result =>
      expect.eql(0, result.exitCode) and
        // /tmp may be a symlink to /private/tmp on macOS
        expect(result.stdout.trim.endsWith("/tmp"))
    }

  // --- BuildToolDetector tests ---

  test("BuildToolDetector: detects Mill from build.mill"):
    withTempDir { dir =>
      Fs2Files[IO].createFile(dir.resolve("build.mill")) >>
        build.BuildToolDetector.detectKind(dir).map { kind =>
          expect(clue(kind) == build.BuildToolKind.Mill)
        }
    }

  test("BuildToolDetector: detects Mill from build.sc"):
    withTempDir { dir =>
      Fs2Files[IO].createFile(dir.resolve("build.sc")) >>
        build.BuildToolDetector.detectKind(dir).map { kind =>
          expect(clue(kind) == build.BuildToolKind.Mill)
        }
    }

  test("BuildToolDetector: detects sbt from build.sbt"):
    withTempDir { dir =>
      Fs2Files[IO].createFile(dir.resolve("build.sbt")) >>
        build.BuildToolDetector.detectKind(dir).map { kind =>
          expect(clue(kind) == build.BuildToolKind.Sbt)
        }
    }

  test("BuildToolDetector: detects scala-cli from .scala-build"):
    withTempDir { dir =>
      Fs2Files[IO].createDirectories(dir.resolve(".scala-build")) >>
        build.BuildToolDetector.detectKind(dir).map { kind =>
          expect(clue(kind) == build.BuildToolKind.ScalaCli)
        }
    }

  test("BuildToolDetector: fallback to scala-cli on empty dir"):
    withTempDir { dir =>
      build.BuildToolDetector.detectKind(dir).map { kind =>
        expect(clue(kind) == build.BuildToolKind.ScalaCli)
      }
    }

  test("BuildToolDetector: Mill takes priority over sbt"):
    withTempDir { dir =>
      Fs2Files[IO].createFile(dir.resolve("build.mill")) >>
      Fs2Files[IO].createFile(dir.resolve("build.sbt")) >>
      build.BuildToolDetector.detectKind(dir).map { kind =>
        expect(clue(kind) == build.BuildToolKind.Mill)
      }
    }

  test("BuildToolDetector: sbt takes priority over scala-cli"):
    withTempDir { dir =>
      Fs2Files[IO].createFile(dir.resolve("build.sbt")) >>
      Fs2Files[IO].createDirectories(dir.resolve(".scala-build")) >>
      build.BuildToolDetector.detectKind(dir).map { kind =>
        expect(clue(kind) == build.BuildToolKind.Sbt)
      }
    }

  // --- ClasspathOutputParser tests ---

  test("ClasspathOutputParser: JSON array with refs"):
    val input = """["ref:abc123:/path/to/classes", "/path/to/dep.jar"]"""
    build.ClasspathOutputParser.parseJsonArray(input, checkExists = false) map {
      case Right(paths) =>
        expect.eql(List("/path/to/classes", "/path/to/dep.jar"), paths.map(_.toString))
      case Left(err) => failure(err)
    }

  test("ClasspathOutputParser: JSON empty array"):
    build.ClasspathOutputParser.parseJsonArray("[]", checkExists = false) map {
      case Left(err) => expect(err.contains("empty"))
      case Right(_)  => failure("Should fail on empty array")
    }

  test("ClasspathOutputParser: JSON malformed"):
    build.ClasspathOutputParser.parseJsonArray("not json", checkExists = false) map {
      case Left(_)  => success // expected
      case Right(_) => failure("Should fail on malformed input")
    }

  pureTest("ClasspathOutputParser: colon-separated"):
    build.ClasspathOutputParser.parseColonSeparated("/a/b.jar:/c/d.jar\n") match
      case Right(paths) => expect.eql(List("/a/b.jar", "/c/d.jar"), paths.map(_.toString))
      case Left(err)    => failure(err)

  pureTest("ClasspathOutputParser: colon-separated single entry"):
    build.ClasspathOutputParser.parseColonSeparated("/a/b.jar\n") match
      case Right(paths) => expect.eql(1, paths.size)
      case Left(err)    => failure(err)

  pureTest("ClasspathOutputParser: colon-separated empty"):
    build.ClasspathOutputParser.parseColonSeparated("\n") match
      case Left(_)  => success // expected
      case Right(_) => failure("Should fail on empty")

  pureTest("ClasspathOutputParser: colon-separated with whitespace"):
    build.ClasspathOutputParser.parseColonSeparated(" /a/b.jar : /c/d.jar ") match
      case Right(paths) => expect.eql(List("/a/b.jar", "/c/d.jar"), paths.map(_.toString))
      case Left(err)    => failure(err)

  // --- ScalaCliBuildTool tests ---

  test("ScalaCliBuildTool: rejects --module"):
    build.ScalaCliBuildTool(Path(".")).extractClasspath(Some("foo"), testScope = false).attempt.map { result =>
      expect(result.isLeft) and
        expect(result.left.exists(_.getMessage.contains("--module is not supported")))
    }

  test("ScalaCliBuildTool: fingerprintFiles returns empty"):
    build.ScalaCliBuildTool(Path(".")).fingerprintFiles.map { files =>
      expect(files.isEmpty)
    }

  // --- MillBuildTool tests ---

  test("MillBuildTool: rejects missing --module"):
    build.MillBuildTool(Path("."), Config.global.mill)
      .extractClasspath(None, testScope = false).attempt.map { result =>
        expect(result.isLeft) and
          expect(result.left.exists(_.getMessage.contains("--module is required for Mill")))
    }

  test("MillBuildTool: rejects --test scope"):
    build.MillBuildTool(Path("."), Config.global.mill)
      .validateTestScope(testScope = true).attempt.map { result =>
        expect(result.isLeft) and
          expect(result.left.exists(_.getMessage.contains("--test is not supported for Mill")))
    }

  // --- SbtBuildTool tests ---

  test("SbtBuildTool: rejects missing --module"):
    build.SbtBuildTool(Path("."), Config.global.sbt).extractClasspath(None, testScope = false)
      .attempt.map { result =>
        expect(result.isLeft) and
          expect(result.left.exists(_.getMessage.contains("--module is required for sbt")))
    }

  // --- BuildFingerprint tests ---

  test("BuildFingerprint: deterministic"):
    withTempDir { dir =>
      val file = dir.resolve("test.txt")
      IO.blocking(Files.writeString(file.toNioPath, "hello")) >>
        (for
          h1 <- build.BuildFingerprint.compute(List(file), "mod")
          h2 <- build.BuildFingerprint.compute(List(file), "mod")
        yield expect.eql(h1, h2))
    }

  test("BuildFingerprint: content sensitivity"):
    withTempDir { dir =>
      val file = dir.resolve("test.txt")
      for
        _  <- IO.blocking(Files.writeString(file.toNioPath, "hello"))
        h1 <- build.BuildFingerprint.compute(List(file), "mod")
        _  <- IO.blocking(Files.writeString(file.toNioPath, "world"))
        h2 <- build.BuildFingerprint.compute(List(file), "mod")
      yield expect(clue(h1) != clue(h2))
    }

  test("BuildFingerprint: module sensitivity"):
    withTempDir { dir =>
      val file = dir.resolve("test.txt")
      for
        _  <- IO.blocking(Files.writeString(file.toNioPath, "hello"))
        h1 <- build.BuildFingerprint.compute(List(file), "modA")
        h2 <- build.BuildFingerprint.compute(List(file), "modB")
      yield expect(clue(h1) != clue(h2))
    }

  test("BuildFingerprint: file order independence"):
    withTempDir { dir =>
      val a = dir.resolve("a.txt")
      val b = dir.resolve("b.txt")
      for
        _ <- IO.blocking { Files.writeString(a.toNioPath, "aaa") }
        _ <- IO.blocking { Files.writeString(b.toNioPath, "bbb") }
        h1 <- build.BuildFingerprint.compute(List(a, b), "mod")
        h2 <- build.BuildFingerprint.compute(List(b, a), "mod")
      yield expect.eql(h1, h2)
    }

  test("BuildFingerprint: missing file skipped"):
    withTempDir { dir =>
      val existing = dir.resolve("exists.txt")
      val missing = dir.resolve("nope.txt")
      IO.blocking(Files.writeString(existing.toNioPath, "data")) >>
        build.BuildFingerprint.compute(List(existing, missing), "mod").map { hash =>
          expect.eql(64, hash.length) // valid SHA-256 hex
        }
    }

  test("BuildFingerprint: empty file list"):
    build.BuildFingerprint.compute(Nil, "mod").map { hash =>
      expect.eql(64, hash.length)
    }

  // --- ClasspathCache tests ---

  test("ClasspathCache: round-trip"):
    withTempDir { dir =>
      val cache = build.ClasspathCache(dir)
      // Create dummy paths that exist
      val p1 = dir.resolve("a.jar")
      val p2 = dir.resolve("b.jar")
      for
        _ <- Fs2Files[IO].createFile(p1)
        _ <- Fs2Files[IO].createFile(p2)
        _ <- cache.put("testhash", List(p1, p2))
        r <- cache.get("testhash")
      yield expect(clue(r) == Some(List(p1, p2)))
    }

  test("ClasspathCache: miss"):
    withTempDir { dir =>
      build.ClasspathCache(dir).get("nonexistent").map { r =>
        expect(clue(r) == None)
      }
    }

  test("ClasspathCache: invalidates on missing path"):
    withTempDir { dir =>
      val cache = build.ClasspathCache(dir)
      val p1 = dir.resolve("a.jar")
      for
        _ <- Fs2Files[IO].createFile(p1)
        _ <- cache.put("hash2", List(p1))
        _ <- Fs2Files[IO].delete(p1)
        r <- cache.get("hash2")
      yield expect(clue(r) == None)
    }

  // --- End-to-end scala-cli tests (requires scala-cli on PATH) ---

  test("E2E scala-cli: get resolves project symbol"):
    requireTool(isOnPath("scala-cli"), "scala-cli not on PATH") >>
    withTempDir { dir =>
      val console = CapturingConsole()
      given Console[IO] = console
      IO.blocking(Files.writeString(dir.resolve("Main.scala").toNioPath,
        """package example
          |
          |class MyClass:
          |  def hello: String = "world"
          |""".stripMargin
      )) >>
        handlers.ProjectGetHandler.run("example.MyClass", module = None, cwd = Some(dir)).map { code =>
          expect(clue(code) == ExitCode.Success, s"Stderr: ${console.errBuf}") and
            expect(console.outBuf.toString.contains("MyClass"), s"Output: ${console.outBuf}") and
            expect(console.outBuf.toString.contains("hello"), s"Output: ${console.outBuf}")
        }
    }

  test("E2E scala-cli: get resolves symbol from dependency"):
    requireTool(isOnPath("scala-cli"), "scala-cli not on PATH") >>
    withTempDir { dir =>
      val console = CapturingConsole()
      given Console[IO] = console
      IO.blocking(Files.writeString(dir.resolve("Main.scala").toNioPath,
        """//> using dep org.typelevel::cats-core:2.10.0
          |
          |package example
          |
          |class Wrapper:
          |  def run: Unit = ()
          |""".stripMargin
      )) >>
        handlers.ProjectGetHandler.run("cats.Monad", module = None, cwd = Some(dir)).map { code =>
          expect(clue(code) == ExitCode.Success) and
            expect(console.outBuf.toString.contains("Monad"), s"Output: ${console.outBuf}")
        }
    }

  test("E2E scala-cli: get shows a dependency's default values and docs from its sources jar"):
    requireTool(isOnPath("scala-cli"), "scala-cli not on PATH") >>
    withTempDir { dir =>
      val console = CapturingConsole()
      given Console[IO] = console
      IO.blocking(Files.writeString(dir.resolve("Main.scala").toNioPath,
        """//> using dep org.scalameta::munit:1.0.4
          |object Main:
          |  def run: Unit = ()
          |""".stripMargin
      )) >>
        handlers.ProjectGetHandler.run("munit.Assertions.assertEquals", module = None, cwd = Some(dir)).map { code =>
          val out = console.outBuf.toString
          expect(clue(code) == ExitCode.Success, s"Stderr: ${console.errBuf}") and
            expect(out.contains("clue: => Any = \"values are not the same\""), s"Output: $out") and
            expect(out.contains("Asserts that two elements are equal"), s"Output: $out")
        }
    }

  test("E2E scala-cli: --test resolves symbol from test-only dependency"):
    requireTool(isOnPath("scala-cli"), "scala-cli not on PATH") >>
    withTempDir { dir =>
      val console = CapturingConsole()
      given Console[IO] = console
      IO.blocking(Files.writeString(dir.resolve("Main.scala").toNioPath,
        """//> using test.dep org.scalameta::munit:1.0.4
          |
          |package example
          |
          |class Wrapper:
          |  def run: Unit = ()
          |""".stripMargin
      )) >>
        (
          safeRun(handlers.ProjectGetHandler.run("munit.FunSuite", module = None, cwd = Some(dir))).map { code =>
            expect(clue(code) == ExitCode.Error, "test-only dep must not be on the compile classpath")
          },
          handlers.ProjectGetHandler.run("munit.FunSuite", module = None, cwd = Some(dir), testScope = true).map { code =>
            expect(clue(code) == ExitCode.Success, s"Stderr: ${console.errBuf}") and
              expect(console.outBuf.toString.contains("FunSuite"), s"Output: ${console.outBuf}")
          }
        ).mapN(_ and _)
    }

  test("E2E scala-cli: list lists members of project class"):
    requireTool(isOnPath("scala-cli"), "scala-cli not on PATH") >>
    withTempDir { dir =>
      val console = CapturingConsole()
      given Console[IO] = console
      IO.blocking(Files.writeString(dir.resolve("Main.scala").toNioPath,
        """package example
          |
          |class MyClass:
          |  def hello: String = "world"
          |  def goodbye: Int = 42
          |""".stripMargin
      )) >>
        handlers.ProjectListHandler.run("example.MyClass", module = None, limit = 50, cwd = Some(dir)).map { code =>
          val out = console.outBuf.toString
          expect(clue(code) == ExitCode.Success) and
            expect(out.contains("hello"), s"Output: $out") and
            expect(out.contains("goodbye"), s"Output: $out")
        }
    }

  test("E2E scala-cli: search finds symbols across project"):
    requireTool(isOnPath("scala-cli"), "scala-cli not on PATH") >>
    withTempDir { dir =>
      val console = CapturingConsole()
      given Console[IO] = console
      IO.blocking(Files.writeString(dir.resolve("Main.scala").toNioPath,
        """package example
          |
          |class UniqueTestClassName123:
          |  def run: Unit = ()
          |""".stripMargin
      )) >>
        handlers.ProjectSearchHandler.run("UniqueTestClassName123", module = None, limit = 50, cwd = Some(dir)).map { code =>
          expect(clue(code) == ExitCode.Success) and
            expect(console.outBuf.toString.contains("UniqueTestClassName123"), s"Output: ${console.outBuf}")
        }
    }

  test("E2E scala-cli: --module produces error"):
    requireTool(isOnPath("scala-cli"), "scala-cli not on PATH") >>
    withTempDir { dir =>
      val console = CapturingConsole()
      given Console[IO] = console
      IO.blocking(Files.writeString(dir.resolve("Main.scala").toNioPath,
        """package example
          |class Foo
          |""".stripMargin
      )) >>
        handlers.ProjectGetHandler.run("example.Foo", module = Some("bar"), cwd = Some(dir))
          .handleErrorWith { e => Console[IO].errorln(e.getMessage).as(ExitCode.Error) }
          .map { code =>
            expect(clue(code) == ExitCode.Error) and
              expect(console.errBuf.toString.contains("--module is not supported"), s"Stderr: ${console.errBuf}")
          }
    }

  test("E2E scala-cli: compilation failure surfaces build tool error"):
    requireTool(isOnPath("scala-cli"), "scala-cli not on PATH") >>
    withTempDir { dir =>
      val console = CapturingConsole()
      given Console[IO] = console
      IO.blocking(Files.writeString(dir.resolve("Bad.scala").toNioPath,
        """package example
          |class Bad {
          |  val x: String = 42  // type error
          |}
          |""".stripMargin
      )) >>
        safeRun(handlers.ProjectGetHandler.run("example.Bad", module = None, cwd = Some(dir))).map { code =>
          expect(clue(code) == ExitCode.Error) and
            expect(console.errBuf.toString.contains("Compilation failed"), s"Stderr: ${console.errBuf}")
        }
    }

  // --- End-to-end Mill tests (requires mill on PATH) ---

  // No `mill` needed: --test must be rejected before the build tool is invoked.
  test("E2E Mill: --test is rejected without invoking the build tool"):
    withTempDir { dir =>
      val console = CapturingConsole()
      given Console[IO] = console
      IO.blocking(Files.writeString(dir.resolve("build.mill").toNioPath, "package build\n")) >>
        safeRun(
          handlers.ProjectGetHandler.run(
            "example.Foo",
            module = Some("app"),
            cwd = Some(dir),
            config = Config.global.copy(mill = MillConfig("nonexistent-mill-binary-xyz")),
            testScope = true
          )
        ).map { code =>
          expect(clue(code) == ExitCode.Error) and
            expect(console.errBuf.toString.contains("--test is not supported for Mill"), s"Stderr: ${console.errBuf}")
        }
    }

  test("E2E Mill: get resolves project symbol"):
    requireTool(isBinaryAvailable(millBinary), s"$millBinary not found") >>
    withTempDir { dir =>
      val console = CapturingConsole()
      given Console[IO] = console
      IO.blocking {
        Files.writeString(dir.resolve("build.mill").toNioPath,
          """package build
            |import mill._, scalalib._
            |
            |object app extends ScalaModule {
            |  def scalaVersion = "3.8.1"
            |}
            |""".stripMargin
        )
        Files.createDirectories(dir.resolve("app/src").toNioPath)
        Files.writeString(dir.resolve("app/src/Main.scala").toNioPath,
          """package example
            |
            |class MillClass:
            |  def greet: String = "hello from mill"
            |""".stripMargin
        )
      } >>
        handlers.ProjectGetHandler.run("example.MillClass", module = Some("app"), cwd = Some(dir), config = Config.global.copy(mill = MillConfig(millBinary))).map { code =>
          expect(clue(code) == ExitCode.Success, s"Stderr: ${console.errBuf}\nStdout: ${console.outBuf}") and
            expect(console.outBuf.toString.contains("MillClass"), s"Output: ${console.outBuf}") and
            expect(console.outBuf.toString.contains("greet"), s"Output: ${console.outBuf}")
        }
    }

  test("E2E Mill: --module required"):
    requireTool(isBinaryAvailable(millBinary), s"$millBinary not found") >>
    withTempDir { dir =>
      val console = CapturingConsole()
      given Console[IO] = console
      IO.blocking(Files.writeString(dir.resolve("build.mill").toNioPath, "")) >>
        handlers.ProjectGetHandler.run("example.Foo", module = None, cwd = Some(dir), config = Config.global.copy(mill = MillConfig(millBinary)))
          .handleErrorWith { e => Console[IO].errorln(e.getMessage).as(ExitCode.Error) }
          .map { code =>
            expect(clue(code) == ExitCode.Error) and
              expect(console.errBuf.toString.contains("--module is required for Mill"), s"Stderr: ${console.errBuf}")
          }
    }

  // --- End-to-end sbt tests (requires sbt on PATH) ---

  test("E2E sbt: get resolves project symbol"):
    requireTool(isOnPath("sbt"), "sbt not on PATH") >>
    withTempDir { dir =>
      val console = CapturingConsole()
      given Console[IO] = console
      IO.blocking {
        Files.writeString(dir.resolve("build.sbt").toNioPath,
          """lazy val `cellar-test` = (project in file("."))
            |  .settings(scalaVersion := "3.8.1")
            |""".stripMargin
        )
        Files.createDirectories(dir.resolve("project").toNioPath)
        Files.writeString(dir.resolve("project/build.properties").toNioPath, "sbt.version=1.10.11\n")
        Files.createDirectories(dir.resolve("src/main/scala/example").toNioPath)
        Files.writeString(dir.resolve("src/main/scala/example/Main.scala").toNioPath,
          """package example
            |
            |class SbtClass:
            |  def greet: String = "hello from sbt"
            |""".stripMargin
        )
      } >>
        handlers.ProjectGetHandler.run("example.SbtClass", module = Some("cellar-test"), cwd = Some(dir)).map { code =>
          expect(clue(code) == ExitCode.Success, s"Stderr: ${console.errBuf}") and
            expect(console.outBuf.toString.contains("SbtClass"), s"Output: ${console.outBuf}")
        }
    }

  test("E2E sbt: --module required"):
    requireTool(isOnPath("sbt"), "sbt not on PATH") >>
    withTempDir { dir =>
      val console = CapturingConsole()
      given Console[IO] = console
      IO.blocking(Files.writeString(dir.resolve("build.sbt").toNioPath, "")) >>
        handlers.ProjectGetHandler.run("example.Foo", module = None, cwd = Some(dir))
          .handleErrorWith { e => Console[IO].errorln(e.getMessage).as(ExitCode.Error) }
          .map { code =>
            expect(clue(code) == ExitCode.Error) and
              expect(console.errBuf.toString.contains("--module is required for sbt"), s"Stderr: ${console.errBuf}")
          }
    }

package cellar

import cats.effect.{ExitCode, IO}
import cats.effect.std.Console
import cats.syntax.all.*
import cellar.process.ProcessRunner
import munit.CatsEffectSuite

import java.nio.file.Files
import fs2.io.file.{Files => Fs2Files, Path}

/** Integration tests for project-aware commands.
  *
  * Tests require build tools (scala-cli, mill) on PATH.
  * Tests that require unavailable tools are skipped via `assume`.
  */
class ProjectAwareIntegrationTest extends CatsEffectSuite:

  override def munitIOTimeout = scala.concurrent.duration.Duration(300, "s")

  private lazy val millBinary: String =
    Option(System.getProperty("cellar.test.millBinary")).getOrElse("mill")

  private def isOnPath(binary: String): IO[Boolean] =
    ProcessRunner.run("which", List(binary)).map(_.exitCode == 0).handleError(_ => false)

  private def isBinaryAvailable(binary: String): IO[Boolean] =
    (Fs2Files[IO].isRegularFile(Path(binary)), isOnPath(binary)).mapN(_ || _)

  private def withTempDir(test: Path => IO[Unit]): IO[Unit] =
    Fs2Files[IO].tempDirectory.use(test)

  // --- ProcessRunner tests ---

  test("ProcessRunner: echo captures stdout"):
    process.ProcessRunner.run("echo", List("hello")).map { result =>
      assertEquals(result.exitCode, 0)
      assertEquals(result.stdout.trim, "hello")
      assert(result.stderr.isEmpty)
    }

  test("ProcessRunner: non-zero exit captures stderr"):
    process.ProcessRunner.run("sh", List("-c", "echo err >&2; exit 1")).map { result =>
      assertEquals(result.exitCode, 1)
      assert(result.stderr.contains("err"))
    }

  test("ProcessRunner: command not found"):
    process.ProcessRunner.run("nonexistent-command-xyz", Nil).attempt.map { result =>
      assert(result.isLeft)
      assert(result.left.exists(_.getMessage.contains("nonexistent-command-xyz")))
    }

  test("ProcessRunner: working directory"):
    process.ProcessRunner.run("pwd", Nil, Some(Path("/tmp"))).map { result =>
      assertEquals(result.exitCode, 0)
      // /tmp may be a symlink to /private/tmp on macOS
      assert(result.stdout.trim.endsWith("/tmp"))
    }

  // --- BuildToolDetector tests ---

  test("BuildToolDetector: detects Mill from build.mill"):
    withTempDir { dir =>
      Fs2Files[IO].createFile(dir.resolve("build.mill")) >>
        build.BuildToolDetector.detectKind(dir).map { kind =>
          assertEquals(kind, build.BuildToolKind.Mill)
        }
    }

  test("BuildToolDetector: detects Mill from build.sc"):
    withTempDir { dir =>
      Fs2Files[IO].createFile(dir.resolve("build.sc")) >>
        build.BuildToolDetector.detectKind(dir).map { kind =>
          assertEquals(kind, build.BuildToolKind.Mill)
        }
    }

  test("BuildToolDetector: detects sbt from build.sbt"):
    withTempDir { dir =>
      Fs2Files[IO].createFile(dir.resolve("build.sbt")) >>
        build.BuildToolDetector.detectKind(dir).map { kind =>
          assertEquals(kind, build.BuildToolKind.Sbt)
        }
    }

  test("BuildToolDetector: detects scala-cli from .scala-build"):
    withTempDir { dir =>
      Fs2Files[IO].createDirectories(dir.resolve(".scala-build")) >>
        build.BuildToolDetector.detectKind(dir).map { kind =>
          assertEquals(kind, build.BuildToolKind.ScalaCli)
        }
    }

  test("BuildToolDetector: fallback to scala-cli on empty dir"):
    withTempDir { dir =>
      build.BuildToolDetector.detectKind(dir).map { kind =>
        assertEquals(kind, build.BuildToolKind.ScalaCli)
      }
    }

  test("BuildToolDetector: Mill takes priority over sbt"):
    withTempDir { dir =>
      Fs2Files[IO].createFile(dir.resolve("build.mill")) >>
      Fs2Files[IO].createFile(dir.resolve("build.sbt")) >>
      build.BuildToolDetector.detectKind(dir).map { kind =>
        assertEquals(kind, build.BuildToolKind.Mill)
      }
    }

  test("BuildToolDetector: sbt takes priority over scala-cli"):
    withTempDir { dir =>
      Fs2Files[IO].createFile(dir.resolve("build.sbt")) >>
      Fs2Files[IO].createDirectories(dir.resolve(".scala-build")) >>
      build.BuildToolDetector.detectKind(dir).map { kind =>
        assertEquals(kind, build.BuildToolKind.Sbt)
      }
    }

  // --- ClasspathOutputParser tests ---

  test("ClasspathOutputParser: JSON array with refs"):
    val input = """["ref:abc123:/path/to/classes", "/path/to/dep.jar"]"""
    build.ClasspathOutputParser.parseJsonArray(input, checkExists = false) map {
      case Right(paths) =>
        assertEquals(paths.map(_.toString), List("/path/to/classes", "/path/to/dep.jar"))
      case Left(err) => fail(err)
    }

  test("ClasspathOutputParser: JSON empty array"):
    build.ClasspathOutputParser.parseJsonArray("[]", checkExists = false) map {
      case Left(err) => assert(err.contains("empty"))
      case Right(_)  => fail("Should fail on empty array")
    }

  test("ClasspathOutputParser: JSON malformed"):
    build.ClasspathOutputParser.parseJsonArray("not json", checkExists = false) map {
      case Left(_)  => () // expected
      case Right(_) => fail("Should fail on malformed input")
    }

  test("ClasspathOutputParser: colon-separated"):
    build.ClasspathOutputParser.parseColonSeparated("/a/b.jar:/c/d.jar\n") match
      case Right(paths) => assertEquals(paths.map(_.toString), List("/a/b.jar", "/c/d.jar"))
      case Left(err)    => fail(err)

  test("ClasspathOutputParser: colon-separated single entry"):
    build.ClasspathOutputParser.parseColonSeparated("/a/b.jar\n") match
      case Right(paths) => assertEquals(paths.size, 1)
      case Left(err)    => fail(err)

  test("ClasspathOutputParser: colon-separated empty"):
    build.ClasspathOutputParser.parseColonSeparated("\n") match
      case Left(_)  => () // expected
      case Right(_) => fail("Should fail on empty")

  test("ClasspathOutputParser: colon-separated with whitespace"):
    build.ClasspathOutputParser.parseColonSeparated(" /a/b.jar : /c/d.jar ") match
      case Right(paths) => assertEquals(paths.map(_.toString), List("/a/b.jar", "/c/d.jar"))
      case Left(err)    => fail(err)

  // --- ScalaCliBuildTool tests ---

  test("ScalaCliBuildTool: rejects --module"):
    build.ScalaCliBuildTool(Path(".")).extractClasspath(Some("foo")).attempt.map { result =>
      assert(result.isLeft)
      assert(result.left.exists(_.getMessage.contains("--module is not supported")))
    }

  test("ScalaCliBuildTool: fingerprintFiles returns empty"):
    build.ScalaCliBuildTool(Path(".")).fingerprintFiles.map { files =>
      assert(files.isEmpty)
    }

  // --- MillBuildTool tests ---

  test("MillBuildTool: rejects missing --module"):
    build.MillBuildTool(Path("."), Config.global.mill)
      .extractClasspath(None).attempt.map { result =>
        assert(result.isLeft)
        assert(result.left.exists(_.getMessage.contains("--module is required for Mill")))
    }

  // --- SbtBuildTool tests ---

  test("SbtBuildTool: rejects missing --module"):
    build.SbtBuildTool(Path("."), Config.global.sbt).extractClasspath(None)
      .attempt.map { result =>
        assert(result.isLeft)
        assert(result.left.exists(_.getMessage.contains("--module is required for sbt")))
    }

  // --- BuildFingerprint tests ---

  test("BuildFingerprint: deterministic"):
    withTempDir { dir =>
      val file = dir.resolve("test.txt")
      IO.blocking(Files.writeString(file.toNioPath, "hello")) >>
        (for
          h1 <- build.BuildFingerprint.compute(List(file), "mod")
          h2 <- build.BuildFingerprint.compute(List(file), "mod")
        yield assertEquals(h1, h2))
    }

  test("BuildFingerprint: content sensitivity"):
    withTempDir { dir =>
      val file = dir.resolve("test.txt")
      for
        _  <- IO.blocking(Files.writeString(file.toNioPath, "hello"))
        h1 <- build.BuildFingerprint.compute(List(file), "mod")
        _  <- IO.blocking(Files.writeString(file.toNioPath, "world"))
        h2 <- build.BuildFingerprint.compute(List(file), "mod")
      yield assertNotEquals(h1, h2)
    }

  test("BuildFingerprint: module sensitivity"):
    withTempDir { dir =>
      val file = dir.resolve("test.txt")
      for
        _  <- IO.blocking(Files.writeString(file.toNioPath, "hello"))
        h1 <- build.BuildFingerprint.compute(List(file), "modA")
        h2 <- build.BuildFingerprint.compute(List(file), "modB")
      yield assertNotEquals(h1, h2)
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
      yield assertEquals(h1, h2)
    }

  test("BuildFingerprint: missing file skipped"):
    withTempDir { dir =>
      val existing = dir.resolve("exists.txt")
      val missing = dir.resolve("nope.txt")
      IO.blocking(Files.writeString(existing.toNioPath, "data")) >>
        build.BuildFingerprint.compute(List(existing, missing), "mod").map { hash =>
          assertEquals(hash.length, 64) // valid SHA-256 hex
        }
    }

  test("BuildFingerprint: empty file list"):
    build.BuildFingerprint.compute(Nil, "mod").map { hash =>
      assertEquals(hash.length, 64)
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
      yield assertEquals(r, Some(List(p1, p2)))
    }

  test("ClasspathCache: miss"):
    withTempDir { dir =>
      build.ClasspathCache(dir).get("nonexistent").map { r =>
        assertEquals(r, None)
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
      yield assertEquals(r, None)
    }

  // --- End-to-end scala-cli tests (requires scala-cli on PATH) ---

  test("E2E scala-cli: get resolves project symbol"):
    isOnPath("scala-cli").map(assume(_, "scala-cli not on PATH")) >>
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
        handlers.ProjectGetHandler.run("example.MyClass", module = None, Config.global, cwd = Some(dir)).map { code =>
          assertEquals(code, ExitCode.Success, s"Stderr: ${console.errBuf}")
          assert(console.outBuf.toString.contains("MyClass"), s"Output: ${console.outBuf}")
          assert(console.outBuf.toString.contains("hello"), s"Output: ${console.outBuf}")
        }
    }

  test("E2E scala-cli: get resolves symbol from dependency"):
    isOnPath("scala-cli").map(assume(_, "scala-cli not on PATH")) >>
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
        handlers.ProjectGetHandler.run("cats.Monad", module = None, Config.global, cwd = Some(dir)).map { code =>
          assertEquals(code, ExitCode.Success)
          assert(console.outBuf.toString.contains("Monad"), s"Output: ${console.outBuf}")
        }
    }

  test("E2E scala-cli: list lists members of project class"):
    isOnPath("scala-cli").map(assume(_, "scala-cli not on PATH")) >>
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
        handlers.ProjectListHandler.run("example.MyClass", module = None, limit = 50, Config.global, cwd = Some(dir)).map { code =>
          assertEquals(code, ExitCode.Success)
          val out = console.outBuf.toString
          assert(out.contains("hello"), s"Output: $out")
          assert(out.contains("goodbye"), s"Output: $out")
        }
    }

  test("E2E scala-cli: search finds symbols across project"):
    isOnPath("scala-cli").map(assume(_, "scala-cli not on PATH")) >>
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
        handlers.ProjectSearchHandler.run("UniqueTestClassName123", module = None, limit = 50, Config.global, cwd = Some(dir)).map { code =>
          assertEquals(code, ExitCode.Success)
          assert(console.outBuf.toString.contains("UniqueTestClassName123"), s"Output: ${console.outBuf}")
        }
    }

  test("E2E scala-cli: --module produces error"):
    isOnPath("scala-cli").map(assume(_, "scala-cli not on PATH")) >>
    withTempDir { dir =>
      val console = CapturingConsole()
      given Console[IO] = console
      IO.blocking(Files.writeString(dir.resolve("Main.scala").toNioPath,
        """package example
          |class Foo
          |""".stripMargin
      )) >>
        handlers.ProjectGetHandler.run("example.Foo", module = Some("bar"), Config.global, cwd = Some(dir)).map { code =>
          assertEquals(code, ExitCode.Error)
          assert(console.errBuf.toString.contains("--module is not supported"), s"Stderr: ${console.errBuf}")
        }
    }

  test("E2E scala-cli: compilation failure surfaces build tool error"):
    isOnPath("scala-cli").map(assume(_, "scala-cli not on PATH")) >>
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
        handlers.ProjectGetHandler.run("example.Bad", module = None, Config.global, cwd = Some(dir)).map { code =>
          assertEquals(code, ExitCode.Error)
          assert(console.errBuf.toString.contains("Compilation failed"), s"Stderr: ${console.errBuf}")
        }
    }

  // --- End-to-end Mill tests (requires mill on PATH) ---

  test("E2E Mill: get resolves project symbol"):
    isBinaryAvailable(millBinary).map(assume(_, s"$millBinary not found")) >>
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
          assertEquals(code, ExitCode.Success, s"Stderr: ${console.errBuf}\nStdout: ${console.outBuf}")
          assert(console.outBuf.toString.contains("MillClass"), s"Output: ${console.outBuf}")
          assert(console.outBuf.toString.contains("greet"), s"Output: ${console.outBuf}")
        }
    }

  test("E2E Mill: --module required"):
    isBinaryAvailable(millBinary).map(assume(_, s"$millBinary not found")) >>
    withTempDir { dir =>
      val console = CapturingConsole()
      given Console[IO] = console
      IO.blocking(Files.writeString(dir.resolve("build.mill").toNioPath, "")) >>
        handlers.ProjectGetHandler.run("example.Foo", module = None, cwd = Some(dir), config = Config.global.copy(mill = MillConfig(millBinary))).map { code =>
          assertEquals(code, ExitCode.Error)
          assert(console.errBuf.toString.contains("--module is required for Mill"), s"Stderr: ${console.errBuf}")
        }
    }

  // --- End-to-end sbt tests (requires sbt on PATH) ---

  test("E2E sbt: get resolves project symbol"):
    isOnPath("sbt").map(assume(_, "sbt not on PATH")) >>
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
        handlers.ProjectGetHandler.run("example.SbtClass", module = Some("cellar-test"), Config.global, cwd = Some(dir)).map { code =>
          assertEquals(code, ExitCode.Success, s"Stderr: ${console.errBuf}")
          assert(console.outBuf.toString.contains("SbtClass"), s"Output: ${console.outBuf}")
        }
    }

  test("E2E sbt: --module required"):
    isOnPath("sbt").map(assume(_, "sbt not on PATH")) >>
    withTempDir { dir =>
      val console = CapturingConsole()
      given Console[IO] = console
      IO.blocking(Files.writeString(dir.resolve("build.sbt").toNioPath, "")) >>
        handlers.ProjectGetHandler.run("example.Foo", module = None, Config.global, cwd = Some(dir)).map { code =>
          assertEquals(code, ExitCode.Error)
          assert(console.errBuf.toString.contains("--module is required for sbt"), s"Stderr: ${console.errBuf}")
        }
    }

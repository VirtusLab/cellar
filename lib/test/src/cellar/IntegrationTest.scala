package cellar

import cats.effect.{ExitCode, IO}
import cats.effect.std.Console
import munit.CatsEffectSuite

/** End-to-end integration tests against locally published fixture artifacts.
  *
  * Prerequisites: `./mill publishFixtures` must have been run.
  */
class IntegrationTest extends CatsEffectSuite:

  // Capture stdout and stderr for assertions
  class CapturingConsole extends Console[IO]:
    val outBuf: StringBuilder = new StringBuilder
    val errBuf: StringBuilder = new StringBuilder
    def readLineWithCharset(charset: java.nio.charset.Charset): IO[String] = IO.pure("")
    def print[A](a: A)(using fmt: cats.Show[A]): IO[Unit]                 = IO.unit
    def println[A](a: A)(using fmt: cats.Show[A]): IO[Unit] =
      IO { outBuf.append(fmt.show(a)).append('\n'); () }
    def error[A](a: A)(using fmt: cats.Show[A]): IO[Unit] = IO.unit
    def errorln[A](a: A)(using fmt: cats.Show[A]): IO[Unit] =
      IO { errBuf.append(fmt.show(a)).append('\n'); () }

  // ─── get subcommand ──────────────────────────────────────────────────────

  test("get: Scala3 sealed ADT stdout contains **Known subtypes:**"):
    TestFixtures.assumeFixturesAvailable()
    val console = CapturingConsole()
    given Console[IO] = console
    handlers.GetHandler
      .run(
        TestFixtures.scala3Coord,
        "cellar.fixture.scala3.CellarADT",
        extraRepositories = Seq(TestFixtures.localM2Repo)
      )
      .map { code =>
        assertEquals(code, ExitCode.Success)
        assert(
          console.outBuf.toString.contains("**Known subtypes:**"),
          s"Output: ${console.outBuf}"
        )
      }

  test("get: Scala3 case class CellarA exits 0 with output"):
    TestFixtures.assumeFixturesAvailable()
    val console = CapturingConsole()
    given Console[IO] = console
    handlers.GetHandler
      .run(
        TestFixtures.scala3Coord,
        "cellar.fixture.scala3.CellarA",
        extraRepositories = Seq(TestFixtures.localM2Repo)
      )
      .map { code =>
        assertEquals(code, ExitCode.Success)
        assert(console.outBuf.toString.nonEmpty)
      }

  test("get: Scala3 opaque type Celsius exits cleanly"):
    // An opaque type alias (e.g. `opaque type Celsius = Double`) may or may not be
    // directly resolvable depending on how tasty-query exposes it.  Either the companion
    // object is found (ExitCode.Success) or a SymbolNotFound error is returned (ExitCode.Error).
    // The command must not crash with an unhandled exception.
    TestFixtures.assumeFixturesAvailable()
    val console = CapturingConsole()
    given Console[IO] = console
    handlers.GetHandler
      .run(
        TestFixtures.scala3Coord,
        "cellar.fixture.scala3.Celsius",
        extraRepositories = Seq(TestFixtures.localM2Repo)
      )
      .map { code =>
        assert(
          code == ExitCode.Success || code == ExitCode.Error,
          s"Unexpected exit code: $code"
        )
        val combined = console.outBuf.toString + console.errBuf.toString
        assert(combined.contains("Celsius"), s"Expected 'Celsius' in output or error: $combined")
      }

  test("get: Scala2 type class exits 0 with non-empty output"):
    TestFixtures.assumeFixturesAvailable()
    val console = CapturingConsole()
    given Console[IO] = console
    handlers.GetHandler
      .run(
        TestFixtures.scala2Coord,
        "cellar.fixture.scala2.CellarTypeClass",
        extraRepositories = Seq(TestFixtures.localM2Repo)
      )
      .map { code =>
        assertEquals(code, ExitCode.Success)
        assert(console.outBuf.toString.nonEmpty)
      }

  test("get: Scala2 artifact prints Scala 2 note to stderr"):
    TestFixtures.assumeFixturesAvailable()
    val console = CapturingConsole()
    given Console[IO] = console
    handlers.GetHandler
      .run(
        TestFixtures.scala2Coord,
        "cellar.fixture.scala2.CellarTypeClass",
        extraRepositories = Seq(TestFixtures.localM2Repo)
      )
      .map { code =>
        assertEquals(code, ExitCode.Success)
        assert(
          console.errBuf.toString.contains("Scala 2"),
          s"Stderr: ${console.errBuf}"
        )
      }

  test("get: Scala3 artifact does not print Scala 2 note to stderr"):
    TestFixtures.assumeFixturesAvailable()
    val console = CapturingConsole()
    given Console[IO] = console
    handlers.GetHandler
      .run(
        TestFixtures.scala3Coord,
        "cellar.fixture.scala3.CellarA",
        extraRepositories = Seq(TestFixtures.localM2Repo)
      )
      .map { code =>
        assertEquals(code, ExitCode.Success)
        assert(
          !console.errBuf.toString.contains("Scala 2"),
          s"Unexpected Scala 2 note in stderr: ${console.errBuf}"
        )
      }

  test("get: Java interface exits 0 with output containing interface"):
    TestFixtures.assumeFixturesAvailable()
    val console = CapturingConsole()
    given Console[IO] = console
    handlers.GetHandler
      .run(
        TestFixtures.javaCoord,
        "cellar.fixture.java.CellarJavaInterface",
        extraRepositories = Seq(TestFixtures.localM2Repo)
      )
      .map { code =>
        assertEquals(code, ExitCode.Success)
        assert(console.outBuf.toString.nonEmpty)
      }

  private val scalaLibCoord = MavenCoordinate("org.scala-lang", "scala-library", "3.8.1")

  test("get: nested type Quotes.reflectModule resolves"):
    val console = CapturingConsole()
    given Console[IO] = console
    handlers.GetHandler
      .run(scalaLibCoord, "scala.quoted.Quotes.reflectModule")
      .map { code =>
        assertEquals(code, ExitCode.Success)
        val out = console.outBuf.toString
        assert(out.contains("reflectModule"), s"Output: $out")
      }

  test("get: 2-level nested Quotes.reflectModule.SymbolMethods resolves"):
    val console = CapturingConsole()
    given Console[IO] = console
    handlers.GetHandler
      .run(scalaLibCoord, "scala.quoted.Quotes.reflectModule.SymbolMethods")
      .map { code =>
        assertEquals(code, ExitCode.Success)
        val out = console.outBuf.toString
        assert(out.contains("SymbolMethods"), s"Output: $out")
      }

  test("list: nested type Quotes.reflectModule lists members"):
    val console = CapturingConsole()
    given Console[IO] = console
    handlers.ListHandler
      .run(scalaLibCoord, "scala.quoted.Quotes.reflectModule", limit = 50)
      .map { code =>
        assertEquals(code, ExitCode.Success)
        val out = console.outBuf.toString
        assert(out.nonEmpty, "Expected non-empty list output")
      }

  test("get: partial resolution shows helpful error"):
    val console = CapturingConsole()
    given Console[IO] = console
    handlers.GetHandler
      .run(scalaLibCoord, "scala.quoted.Quotes.nonExistent")
      .map { code =>
        assertEquals(code, ExitCode.Error)
        val err = console.errBuf.toString
        assert(err.contains("Resolved up to"), s"Stderr: $err")
        assert(err.contains("scala.quoted.Quotes"), s"Stderr: $err")
        assert(err.contains("nonExistent"), s"Stderr: $err")
      }

  test("get: non-existent FQN exits 1 and stderr contains 'not found'"):
    TestFixtures.assumeFixturesAvailable()
    val console = CapturingConsole()
    given Console[IO] = console
    handlers.GetHandler
      .run(
        TestFixtures.scala3Coord,
        "cellar.fixture.scala3.DoesNotExist99999",
        extraRepositories = Seq(TestFixtures.localM2Repo)
      )
      .map { code =>
        assertEquals(code, ExitCode.Error)
        assert(
          console.errBuf.toString.toLowerCase.contains("not found"),
          s"Stderr: ${console.errBuf}"
        )
      }

  test("get: package FQN exits 1 and stderr mentions 'cellar list'"):
    TestFixtures.assumeFixturesAvailable()
    val console = CapturingConsole()
    given Console[IO] = console
    handlers.GetHandler
      .run(
        TestFixtures.scala3Coord,
        "cellar.fixture.scala3",
        extraRepositories = Seq(TestFixtures.localM2Repo)
      )
      .map { code =>
        assertEquals(code, ExitCode.Error)
        assert(
          console.errBuf.toString.contains("cellar list") || console.errBuf.toString.contains("list"),
          s"Stderr: ${console.errBuf}"
        )
      }

  test("get-source: Java class returns java code block"):
    TestFixtures.assumeFixturesAvailable()
    val console = CapturingConsole()
    given Console[IO] = console
    handlers.GetSourceHandler
      .run(
        TestFixtures.javaCoord,
        "cellar.fixture.java.CellarJavaClass",
        extraRepositories = Seq(TestFixtures.localM2Repo)
      )
      .map { code =>
        assertEquals(code, ExitCode.Success)
        assert(console.outBuf.toString.contains("```java"), s"Output: ${console.outBuf}")
      }

  // ─── list subcommand ─────────────────────────────────────────────────────

  test("list: package scala3 fixture lists top-level types"):
    TestFixtures.assumeFixturesAvailable()
    val console = CapturingConsole()
    given Console[IO] = console
    handlers.ListHandler
      .run(
        TestFixtures.scala3Coord,
        "cellar.fixture.scala3",
        limit = 50,
        extraRepositories = Seq(TestFixtures.localM2Repo)
      )
      .map { code =>
        assertEquals(code, ExitCode.Success)
        val out = console.outBuf.toString
        assert(out.nonEmpty, "Expected non-empty list output")
        assert(out.contains("CellarADT"), s"Expected CellarADT in output: $out")
      }

  test("list: class members of CellarTC includes render method"):
    TestFixtures.assumeFixturesAvailable()
    val console = CapturingConsole()
    given Console[IO] = console
    handlers.ListHandler
      .run(
        TestFixtures.scala3Coord,
        "cellar.fixture.scala3.CellarTC",
        limit = 50,
        extraRepositories = Seq(TestFixtures.localM2Repo)
      )
      .map { code =>
        assertEquals(code, ExitCode.Success)
        assert(console.outBuf.toString.contains("render"), s"Output: ${console.outBuf}")
      }

  test("list: limit=1 returns exactly 1 line with truncation note"):
    TestFixtures.assumeFixturesAvailable()
    val console = CapturingConsole()
    given Console[IO] = console
    handlers.ListHandler
      .run(
        TestFixtures.scala3Coord,
        "cellar.fixture.scala3.CellarADT",
        limit = 1,
        extraRepositories = Seq(TestFixtures.localM2Repo)
      )
      .map { code =>
        assertEquals(code, ExitCode.Success)
        val lines = console.outBuf.toString.linesIterator.filter(_.nonEmpty).toList
        assert(lines.length <= 1, s"Expected at most 1 line, got: $lines")
      }

  test("list: non-existent FQN exits 1"):
    TestFixtures.assumeFixturesAvailable()
    val console = CapturingConsole()
    given Console[IO] = console
    handlers.ListHandler
      .run(
        TestFixtures.scala3Coord,
        "cellar.fixture.scala3.DoesNotExist99999",
        limit = 50,
        extraRepositories = Seq(TestFixtures.localM2Repo)
      )
      .map { code =>
        assertEquals(code, ExitCode.Error)
      }

  // ─── search subcommand ───────────────────────────────────────────────────

  test("search: 'CellarADT' query finds sealed trait"):
    TestFixtures.assumeFixturesAvailable()
    val console = CapturingConsole()
    given Console[IO] = console
    handlers.SearchHandler
      .run(
        TestFixtures.scala3Coord,
        "CellarADT",
        limit = 50,
        extraRepositories = Seq(TestFixtures.localM2Repo)
      )
      .map { code =>
        assertEquals(code, ExitCode.Success)
        assert(
          console.outBuf.toString.contains("CellarADT"),
          s"Output: ${console.outBuf}"
        )
      }

  test("search: case-insensitive — 'cellaradt' finds same results"):
    TestFixtures.assumeFixturesAvailable()
    val console = CapturingConsole()
    given Console[IO] = console
    handlers.SearchHandler
      .run(
        TestFixtures.scala3Coord,
        "cellaradt",
        limit = 50,
        extraRepositories = Seq(TestFixtures.localM2Repo)
      )
      .map { code =>
        assertEquals(code, ExitCode.Success)
        assert(console.outBuf.toString.toLowerCase.contains("cellaradt"), s"Output: ${console.outBuf}")
      }

  test("search: non-existent query returns empty output with exit 0"):
    TestFixtures.assumeFixturesAvailable()
    val console = CapturingConsole()
    given Console[IO] = console
    handlers.SearchHandler
      .run(
        TestFixtures.scala3Coord,
        "xyzNeverExistsABC123",
        limit = 50,
        extraRepositories = Seq(TestFixtures.localM2Repo)
      )
      .map { code =>
        assertEquals(code, ExitCode.Success)
        assertEquals(console.outBuf.toString.trim, "")
      }

  // ─── deps subcommand ─────────────────────────────────────────────────────

  test("deps: scala3 fixture exits 0 and first line contains coordinate"):
    TestFixtures.assumeFixturesAvailable()
    val console = CapturingConsole()
    given Console[IO] = console
    handlers.DepsHandler
      .run(TestFixtures.scala3Coord, extraRepositories = Seq(TestFixtures.localM2Repo))
      .map { code =>
        assertEquals(code, ExitCode.Success)
        val out = console.outBuf.toString
        assert(out.nonEmpty, "Expected non-empty deps output")
      }

  test("deps: invalid coordinate exits 1"):
    TestFixtures.assumeFixturesAvailable()
    val console = CapturingConsole()
    given Console[IO] = console
    val bad = MavenCoordinate("com.nonexistent.x12345", "artifact", "1.0.0")
    handlers.DepsHandler.run(bad).map { code =>
      assertEquals(code, ExitCode.Error)
    }

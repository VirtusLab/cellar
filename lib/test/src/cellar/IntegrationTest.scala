package cellar

import cats.effect.{ExitCode, IO}
import cats.effect.std.Console
import munit.CatsEffectSuite
import org.typelevel.otel4s.trace.Tracer.Implicits.noop

/** End-to-end integration tests against locally published fixture artifacts.
  *
  * Prerequisites: `./mill publishFixtures` must have been run.
  */
class IntegrationTest extends CatsEffectSuite:

  private def safeRun(f: IO[ExitCode])(using Console[IO]): IO[ExitCode] =
    f.handleErrorWith(e => Console[IO].errorln(e.getMessage).as(ExitCode.Error))

  // Must precede every `test(...)` below: those register bodies that capture `this`,
  // and -Wsafe-init rejects capturing a suite whose fields are not yet assigned.
  private val scalaLibCoord = MavenCoordinate("org.scala-lang", "scala-library", "3.8.1")

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
    safeRun(handlers.GetHandler
      .run(
        TestFixtures.scala3Coord,
        "cellar.fixture.scala3.Celsius",
        extraRepositories = Seq(TestFixtures.localM2Repo)
      ))
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
    safeRun(handlers.GetHandler
      .run(
        TestFixtures.scala3Coord,
        "cellar.fixture.scala3.DoesNotExist99999",
        extraRepositories = Seq(TestFixtures.localM2Repo)
      ))
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

  test("get-source: Java class returns java code block with source body"):
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
        val out = console.outBuf.toString
        assert(out.contains("```java"), s"Output: $out")
        assert(out.contains("getDefault"), s"Expected source body in: $out")
      }

  test("get-source: trait with same-file companion returns both"):
    TestFixtures.assumeFixturesAvailable()
    val console = CapturingConsole()
    given Console[IO] = console
    handlers.GetSourceHandler
      .run(
        TestFixtures.scala3Coord,
        "cellar.fixture.scala3.CellarTC",
        extraRepositories = Seq(TestFixtures.localM2Repo)
      )
      .map { code =>
        assertEquals(code, ExitCode.Success)
        val out = console.outBuf.toString
        assert(out.contains("trait CellarTC"), s"Expected 'trait CellarTC' in: $out")
        assert(out.contains("object CellarTC"), s"Expected 'object CellarTC' in: $out")
      }

  test("get-source: trailing-$ FQN returns companion source"):
    TestFixtures.assumeFixturesAvailable()
    val console = CapturingConsole()
    given Console[IO] = console
    handlers.GetSourceHandler
      .run(
        TestFixtures.scala3Coord,
        "cellar.fixture.scala3.CellarTC$",
        extraRepositories = Seq(TestFixtures.localM2Repo)
      )
      .map { code =>
        assertEquals(code, ExitCode.Success)
        val out = console.outBuf.toString
        assert(out.contains("object CellarTC"), s"Expected 'object CellarTC' in: $out")
      }

  test("get-source: standalone object returns the whole object body"):
    TestFixtures.assumeFixturesAvailable()
    val console = CapturingConsole()
    given Console[IO] = console
    handlers.GetSourceHandler
      .run(
        TestFixtures.scala3Coord,
        "cellar.fixture.scala3.Celsius",
        extraRepositories = Seq(TestFixtures.localM2Repo)
      )
      .map { code =>
        assertEquals(code, ExitCode.Success)
        val out = console.outBuf.toString
        // The module val's ValDef has a zero-extent span; only the module class
        // carries the object body, so the resolver must hand over the class.
        assert(out.contains("object Celsius"), s"Expected 'object Celsius' in: $out")
        assert(out.contains("toFahrenheit"), s"Expected the object body in: $out")
      }

  // ─── docstrings ──────────────────────────────────────────────────────────

  private def assertNoCompilerCrash(console: CapturingConsole): Unit =
    assert(
      !console.outBuf.toString.contains("Exception while compiling"),
      s"Compiler crash report leaked into stdout: ${console.outBuf}"
    )

  test("get: Scala3 sealed trait docstring reaches stdout"):
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
          console.outBuf.toString.contains("Sealed ADT hierarchy for testing sealedChildren extraction."),
          s"Output: ${console.outBuf}"
        )
        assertNoCompilerCrash(console)
      }

  test("get: Scala3 type class trait docstring reaches stdout"):
    TestFixtures.assumeFixturesAvailable()
    val console = CapturingConsole()
    given Console[IO] = console
    handlers.GetHandler
      .run(
        TestFixtures.scala3Coord,
        "cellar.fixture.scala3.CellarTC",
        extraRepositories = Seq(TestFixtures.localM2Repo)
      )
      .map { code =>
        assertEquals(code, ExitCode.Success)
        assert(
          console.outBuf.toString.contains("Scala 3 type class with given instances."),
          s"Output: ${console.outBuf}"
        )
        assertNoCompilerCrash(console)
      }

  test("get: trait with companion keeps its docstring after companion collapse"):
    TestFixtures.assumeFixturesAvailable()
    val console = CapturingConsole()
    given Console[IO] = console
    handlers.GetHandler
      .run(
        TestFixtures.scala3Coord,
        "cellar.fixture.scala3.CellarWithCompanion",
        extraRepositories = Seq(TestFixtures.localM2Repo)
      )
      .map { code =>
        assertEquals(code, ExitCode.Success)
        assert(
          console.outBuf.toString.contains("Fixture for testing resolution of members declared on the companion."),
          s"Output: ${console.outBuf}"
        )
        assertNoCompilerCrash(console)
      }

  test("get: method-level docstring reaches stdout"):
    TestFixtures.assumeFixturesAvailable()
    val console = CapturingConsole()
    given Console[IO] = console
    handlers.GetHandler
      .run(
        TestFixtures.scala3Coord,
        "cellar.fixture.scala3.CellarTC.render",
        extraRepositories = Seq(TestFixtures.localM2Repo)
      )
      .map { code =>
        assertEquals(code, ExitCode.Success)
        assert(
          console.outBuf.toString.contains("Renders the value as a human-readable string."),
          s"Output: ${console.outBuf}"
        )
        assertNoCompilerCrash(console)
      }

  test("get: undocumented symbol renders cleanly without docstring artifacts"):
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
        val out = console.outBuf.toString
        assert(out.contains("## cellar.fixture.scala3.CellarA"), s"Output: $out")
        assert(!out.contains("/**"), s"Raw docstring markers leaked: $out")
        assertNoCompilerCrash(console)
      }

  test("get: Java class renders signature without compiler crash"):
    TestFixtures.assumeFixturesAvailable()
    val console = CapturingConsole()
    given Console[IO] = console
    handlers.GetHandler
      .run(
        TestFixtures.javaCoord,
        "cellar.fixture.java.CellarJavaClass",
        extraRepositories = Seq(TestFixtures.localM2Repo)
      )
      .map { code =>
        assertEquals(code, ExitCode.Success)
        val out = console.outBuf.toString
        assert(out.contains("class CellarJavaClass"), s"Output: $out")
        assertNoCompilerCrash(console)
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
    safeRun(handlers.DepsHandler.run(bad)).map { code =>
      assertEquals(code, ExitCode.Error)
    }

  // ─── coordinate suggestions ────────────────────────────────────────────────

  test("get: wrong version shows 'Artifact exists' with latest version"):
    val console = CapturingConsole()
    given Console[IO] = console
    val bad = MavenCoordinate("org.typelevel", "cats-core_3", "9.9.9")
    safeRun(handlers.GetHandler.run(bad, "cats.Monad")).map { code =>
      assertEquals(code, ExitCode.Error)
      val err = console.errBuf.toString
      assert(err.contains("Artifact exists."), s"Stderr: $err")
      assert(err.contains("Latest version:"), s"Stderr: $err")
    }

  test("get: wrong artifact shows 'Did you mean?' suggestions"):
    val console = CapturingConsole()
    given Console[IO] = console
    val bad = MavenCoordinate("com.lihaoyi", "mill-scalalib_3", "1.1.1")
    safeRun(handlers.GetHandler.run(bad, "mill.javalib.NativeImageModule")).map { code =>
      assertEquals(code, ExitCode.Error)
      val err = console.errBuf.toString
      assert(err.contains("Could not resolve"), s"Stderr: $err")
    }

  test("get: completely wrong coordinate shows generic error without suggestions"):
    val console = CapturingConsole()
    given Console[IO] = console
    val bad = MavenCoordinate("com.nonexistent.x12345", "foo", "1.0.0")
    safeRun(handlers.GetHandler.run(bad, "bar.Baz")).map { code =>
      assertEquals(code, ExitCode.Error)
      val err = console.errBuf.toString
      assert(err.contains("Check that the group ID"), s"Stderr: $err")
      assert(!err.contains("Did you mean?"), s"Stderr: $err")
    }

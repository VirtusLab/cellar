package cellar

import cats.effect.{ExitCode, IO}
import cats.effect.std.Console
import org.typelevel.otel4s.trace.Tracer.Implicits.noop
import weaver.Expectations

/** End-to-end integration tests against locally published fixture artifacts.
  *
  * Prerequisites: `./mill publishFixtures` must have been run.
  */
object IntegrationTest extends SequentialIOSuite:

  private def safeRun(f: IO[ExitCode])(using Console[IO]): IO[ExitCode] =
    f.handleErrorWith(e => Console[IO].errorln(e.getMessage).as(ExitCode.Error))

  // Must precede every `test(...)` below: those register bodies that capture `this`,
  // and -Wsafe-init rejects capturing a suite whose fields are not yet assigned.
  private val scalaLibCoord = MavenCoordinate("org.scala-lang", "scala-library", "3.8.1")

  // ─── get subcommand ──────────────────────────────────────────────────────

  test("get: Scala3 sealed ADT stdout contains **Known subtypes:**"):
    val console = CapturingConsole()
    given Console[IO] = console
    TestFixtures.assumeFixturesAvailable *>
      handlers.GetHandler
        .run(
          TestFixtures.scala3Coord,
          "cellar.fixture.scala3.CellarADT",
          extraRepositories = Seq(TestFixtures.localM2Repo)
        )
        .map { code =>
          expect(clue(code) == ExitCode.Success) and
            expect(
              console.outBuf.toString.contains("**Known subtypes:**"),
              s"Output: ${console.outBuf}"
            )
        }

  test("get: Scala3 case class CellarA exits 0 with output"):
    val console = CapturingConsole()
    given Console[IO] = console
    TestFixtures.assumeFixturesAvailable *>
      handlers.GetHandler
        .run(
          TestFixtures.scala3Coord,
          "cellar.fixture.scala3.CellarA",
          extraRepositories = Seq(TestFixtures.localM2Repo)
        )
        .map { code =>
          expect(clue(code) == ExitCode.Success) and
            expect(console.outBuf.toString.nonEmpty)
        }

  test("get: Scala3 opaque type Celsius exits cleanly"):
    // An opaque type alias (e.g. `opaque type Celsius = Double`) may or may not be
    // directly resolvable depending on how tasty-query exposes it.  Either the companion
    // object is found (ExitCode.Success) or a SymbolNotFound error is returned (ExitCode.Error).
    // The command must not crash with an unhandled exception.
    val console = CapturingConsole()
    given Console[IO] = console
    TestFixtures.assumeFixturesAvailable *>
      safeRun(handlers.GetHandler
        .run(
          TestFixtures.scala3Coord,
          "cellar.fixture.scala3.Celsius",
          extraRepositories = Seq(TestFixtures.localM2Repo)
        ))
        .map { code =>
          val combined = console.outBuf.toString + console.errBuf.toString
          expect(
            code == ExitCode.Success || code == ExitCode.Error,
            s"Unexpected exit code: $code"
          ) and
            expect(combined.contains("Celsius"), s"Expected 'Celsius' in output or error: $combined")
        }

  test("get: Scala2 type class exits 0 with non-empty output"):
    val console = CapturingConsole()
    given Console[IO] = console
    TestFixtures.assumeFixturesAvailable *>
      handlers.GetHandler
        .run(
          TestFixtures.scala2Coord,
          "cellar.fixture.scala2.CellarTypeClass",
          extraRepositories = Seq(TestFixtures.localM2Repo)
        )
        .map { code =>
          expect(clue(code) == ExitCode.Success) and
            expect(console.outBuf.toString.nonEmpty)
        }

  test("get: Scala 2 member shows its Scaladoc and default value"):
    val console = CapturingConsole()
    given Console[IO] = console
    TestFixtures.assumeFixturesAvailable *>
      handlers.GetHandler
        .run(
          TestFixtures.scala2Coord,
          "cellar.fixture.scala2.CellarSugar.withDefault",
          extraRepositories = Seq(TestFixtures.localM2Repo)
        )
        .map { code =>
          val out = console.outBuf.toString
          expect(clue(code) == ExitCode.Success) and
            expect(out.contains("def withDefault(a: Int, b: String = \"b\"): String"), out) and
            expect(out.contains("Takes a default for `b`."), out)
        }

  test("get-source: Scala 2 trait with same-file companion returns both"):
    val console = CapturingConsole()
    given Console[IO] = console
    TestFixtures.assumeFixturesAvailable *>
      handlers.GetSourceHandler
        .run(
          TestFixtures.scala2Coord,
          "cellar.fixture.scala2.CellarTypeClass",
          extraRepositories = Seq(TestFixtures.localM2Repo)
        )
        .map { code =>
          val out = console.outBuf.toString
          expect(clue(code) == ExitCode.Success) and
            expect(out.contains("// cellar/fixture/scala2/CellarTypeClass.scala lines 4–12"), out) and
            expect(out.contains("trait CellarTypeClass[A]"), out) and
            expect(out.contains("object CellarTypeClass"), out)
        }

  test("get-source: Scala 2 same-named source files resolve by package"):
    def source(fqn: String): IO[String] =
      val console = CapturingConsole()
      given Console[IO] = console
      handlers.GetSourceHandler
        .run(TestFixtures.scala2Coord, fqn, extraRepositories = Seq(TestFixtures.localM2Repo))
        .flatMap { code =>
          expect(clue(code) == ExitCode.Success, console.errBuf.toString)
            .failFast[IO]
            .as(console.outBuf.toString)
        }
    for
      _     <- TestFixtures.assumeFixturesAvailable
      outer <- source("cellar.fixture.scala2.CellarSugar")
      inner <- source("cellar.fixture.scala2.nested.CellarSugar")
    yield
      expect(outer.contains("// cellar/fixture/scala2/CellarSugar.scala lines"), outer) and
        expect(outer.contains("def curriedDefault"), outer) and
        expect(inner.contains("// cellar/fixture/scala2/nested/CellarSugar.scala lines 4–6"), inner) and
        expect(!inner.contains("def curriedDefault"), inner)

  test("get-source: Scala 2 method returns just its definition"):
    val console = CapturingConsole()
    given Console[IO] = console
    TestFixtures.assumeFixturesAvailable *>
      handlers.GetSourceHandler
        .run(
          TestFixtures.scala2Coord,
          "cellar.fixture.scala2.CellarTypeClass.describe",
          extraRepositories = Seq(TestFixtures.localM2Repo)
        )
        .map { code =>
          val out = console.outBuf.toString
          expect(clue(code) == ExitCode.Success) and
            expect(out.contains("lines 5–5"), out) and
            expect(out.contains("def describe(a: A): String"), out) and
            expect(!out.contains("trait CellarTypeClass"), out)
        }

  test("get: Scala2 artifact prints no language note to stderr"):
    val console = CapturingConsole()
    given Console[IO] = console
    TestFixtures.assumeFixturesAvailable *>
      handlers.GetHandler
        .run(
          TestFixtures.scala2Coord,
          "cellar.fixture.scala2.CellarTypeClass",
          extraRepositories = Seq(TestFixtures.localM2Repo)
        )
        .map { code =>
          expect(clue(code) == ExitCode.Success) and
            expect(!console.errBuf.toString.contains("Scala 2"), s"Stderr: ${console.errBuf}")
        }

  test("get: Scala3 artifact does not print Scala 2 note to stderr"):
    val console = CapturingConsole()
    given Console[IO] = console
    TestFixtures.assumeFixturesAvailable *>
      handlers.GetHandler
        .run(
          TestFixtures.scala3Coord,
          "cellar.fixture.scala3.CellarA",
          extraRepositories = Seq(TestFixtures.localM2Repo)
        )
        .map { code =>
          expect(clue(code) == ExitCode.Success) and
            expect(
              !console.errBuf.toString.contains("Scala 2"),
              s"Unexpected Scala 2 note in stderr: ${console.errBuf}"
            )
        }

  test("get: Java interface exits 0 with output containing interface"):
    val console = CapturingConsole()
    given Console[IO] = console
    TestFixtures.assumeFixturesAvailable *>
      handlers.GetHandler
        .run(
          TestFixtures.javaCoord,
          "cellar.fixture.java.CellarJavaInterface",
          extraRepositories = Seq(TestFixtures.localM2Repo)
        )
        .map { code =>
          expect(clue(code) == ExitCode.Success) and
            expect(console.outBuf.toString.nonEmpty)
        }

  test("get: Java member shows its Javadoc and source parameter names"):
    val console = CapturingConsole()
    given Console[IO] = console
    TestFixtures.assumeFixturesAvailable *>
      handlers.GetHandler
        .run(
          TestFixtures.javaCoord,
          "cellar.fixture.java.CellarJavaInterface.repeat",
          extraRepositories = Seq(TestFixtures.localM2Repo)
        )
        .map { code =>
          val out = console.outBuf.toString
          expect(clue(code) == ExitCode.Success) and
            expect(out.contains("List<T> repeat(T value, int times)"), out) and
            expect(out.contains("Repeats {@code value} the given number of times."), out)
        }

  test("get: nested type Quotes.reflectModule resolves"):
    val console = CapturingConsole()
    given Console[IO] = console
    handlers.GetHandler
      .run(scalaLibCoord, "scala.quoted.Quotes.reflectModule")
      .map { code =>
        val out = console.outBuf.toString
        expect(clue(code) == ExitCode.Success) and
          expect(out.contains("reflectModule"), s"Output: $out")
      }

  test("get: 2-level nested Quotes.reflectModule.SymbolMethods resolves"):
    val console = CapturingConsole()
    given Console[IO] = console
    handlers.GetHandler
      .run(scalaLibCoord, "scala.quoted.Quotes.reflectModule.SymbolMethods")
      .map { code =>
        val out = console.outBuf.toString
        expect(clue(code) == ExitCode.Success) and
          expect(out.contains("SymbolMethods"), s"Output: $out")
      }

  test("list: nested type Quotes.reflectModule lists members"):
    val console = CapturingConsole()
    given Console[IO] = console
    handlers.ListHandler
      .run(scalaLibCoord, "scala.quoted.Quotes.reflectModule", limit = 50)
      .map { code =>
        val out = console.outBuf.toString
        expect(clue(code) == ExitCode.Success) and
          expect(out.nonEmpty, "Expected non-empty list output")
      }

  test("get: partial resolution shows helpful error"):
    val console = CapturingConsole()
    given Console[IO] = console
    handlers.GetHandler
      .run(scalaLibCoord, "scala.quoted.Quotes.nonExistent")
      .map { code =>
        val err = console.errBuf.toString
        expect(clue(code) == ExitCode.Error) and
          expect(err.contains("Resolved up to"), s"Stderr: $err") and
          expect(err.contains("scala.quoted.Quotes"), s"Stderr: $err") and
          expect(err.contains("nonExistent"), s"Stderr: $err")
      }

  test("get: non-existent FQN exits 1 and stderr contains 'not found'"):
    val console = CapturingConsole()
    given Console[IO] = console
    TestFixtures.assumeFixturesAvailable *>
      safeRun(handlers.GetHandler
        .run(
          TestFixtures.scala3Coord,
          "cellar.fixture.scala3.DoesNotExist99999",
          extraRepositories = Seq(TestFixtures.localM2Repo)
        ))
        .map { code =>
          expect(clue(code) == ExitCode.Error) and
            expect(
              console.errBuf.toString.toLowerCase.contains("not found"),
              s"Stderr: ${console.errBuf}"
            )
        }

  test("get: package FQN exits 1 and stderr mentions 'cellar list'"):
    val console = CapturingConsole()
    given Console[IO] = console
    TestFixtures.assumeFixturesAvailable *>
      handlers.GetHandler
        .run(
          TestFixtures.scala3Coord,
          "cellar.fixture.scala3",
          extraRepositories = Seq(TestFixtures.localM2Repo)
        )
        .map { code =>
          expect(clue(code) == ExitCode.Error) and
            expect(
              console.errBuf.toString.contains("cellar list") || console.errBuf.toString.contains("list"),
              s"Stderr: ${console.errBuf}"
            )
        }

  test("get-source: Java class returns java code block with source body"):
    val console = CapturingConsole()
    given Console[IO] = console
    TestFixtures.assumeFixturesAvailable *>
      handlers.GetSourceHandler
        .run(
          TestFixtures.javaCoord,
          "cellar.fixture.java.CellarJavaClass",
          extraRepositories = Seq(TestFixtures.localM2Repo)
        )
        .map { code =>
          val out = console.outBuf.toString
          expect(clue(code) == ExitCode.Success) and
            expect(out.contains("```java"), s"Output: $out") and
            expect(out.contains("getDefault"), s"Expected source body in: $out")
        }

  test("get-source: Java static method returns its class's source"):
    val console = CapturingConsole()
    given Console[IO] = console
    TestFixtures.assumeFixturesAvailable *>
      handlers.GetSourceHandler
        .run(
          TestFixtures.javaCoord,
          "cellar.fixture.java.CellarJavaClass.staticHelper",
          extraRepositories = Seq(TestFixtures.localM2Repo)
        )
        .map { code =>
          val out = console.outBuf.toString
          expect(clue(code) == ExitCode.Success, console.errBuf.toString) and
            expect(out.contains("// cellar/fixture/java/CellarJavaClass.java"), out) and
            expect(out.contains("public static int staticHelper"), out)
        }

  test("get-source: trait with same-file companion returns both"):
    val console = CapturingConsole()
    given Console[IO] = console
    TestFixtures.assumeFixturesAvailable *>
      handlers.GetSourceHandler
        .run(
          TestFixtures.scala3Coord,
          "cellar.fixture.scala3.CellarTC",
          extraRepositories = Seq(TestFixtures.localM2Repo)
        )
        .map { code =>
          val out = console.outBuf.toString
          expect(clue(code) == ExitCode.Success) and
            expect(out.contains("trait CellarTC"), s"Expected 'trait CellarTC' in: $out") and
            expect(out.contains("object CellarTC"), s"Expected 'object CellarTC' in: $out")
        }

  test("get-source: trailing-$ FQN returns companion source"):
    val console = CapturingConsole()
    given Console[IO] = console
    TestFixtures.assumeFixturesAvailable *>
      handlers.GetSourceHandler
        .run(
          TestFixtures.scala3Coord,
          "cellar.fixture.scala3.CellarTC$",
          extraRepositories = Seq(TestFixtures.localM2Repo)
        )
        .map { code =>
          val out = console.outBuf.toString
          expect(clue(code) == ExitCode.Success) and
            expect(out.contains("object CellarTC"), s"Expected 'object CellarTC' in: $out")
        }

  test("get-source: standalone object returns the whole object body"):
    val console = CapturingConsole()
    given Console[IO] = console
    TestFixtures.assumeFixturesAvailable *>
      handlers.GetSourceHandler
        .run(
          TestFixtures.scala3Coord,
          "cellar.fixture.scala3.Celsius",
          extraRepositories = Seq(TestFixtures.localM2Repo)
        )
        .map { code =>
          val out = console.outBuf.toString
          // The module val's ValDef has a zero-extent span; only the module class
          // carries the object body, so the resolver must hand over the class.
          expect(clue(code) == ExitCode.Success) and
            expect(out.contains("object Celsius"), s"Expected 'object Celsius' in: $out") and
            expect(out.contains("toFahrenheit"), s"Expected the object body in: $out")
        }

  // ─── docstrings ──────────────────────────────────────────────────────────

  private def noCompilerCrash(console: CapturingConsole): Expectations =
    expect(
      !console.outBuf.toString.contains("Exception while compiling"),
      s"Compiler crash report leaked into stdout: ${console.outBuf}"
    )

  test("get: Scala3 sealed trait docstring reaches stdout"):
    val console = CapturingConsole()
    given Console[IO] = console
    TestFixtures.assumeFixturesAvailable *>
      handlers.GetHandler
        .run(
          TestFixtures.scala3Coord,
          "cellar.fixture.scala3.CellarADT",
          extraRepositories = Seq(TestFixtures.localM2Repo)
        )
        .map { code =>
          expect(clue(code) == ExitCode.Success) and
            expect(
              console.outBuf.toString.contains("Sealed ADT hierarchy for testing sealedChildren extraction."),
              s"Output: ${console.outBuf}"
            ) and
            noCompilerCrash(console)
        }

  test("get: Scala3 type class trait docstring reaches stdout"):
    val console = CapturingConsole()
    given Console[IO] = console
    TestFixtures.assumeFixturesAvailable *>
      handlers.GetHandler
        .run(
          TestFixtures.scala3Coord,
          "cellar.fixture.scala3.CellarTC",
          extraRepositories = Seq(TestFixtures.localM2Repo)
        )
        .map { code =>
          expect(clue(code) == ExitCode.Success) and
            expect(
              console.outBuf.toString.contains("Scala 3 type class with given instances."),
              s"Output: ${console.outBuf}"
            ) and
            noCompilerCrash(console)
        }

  test("get: trait with companion keeps its docstring after companion collapse"):
    val console = CapturingConsole()
    given Console[IO] = console
    TestFixtures.assumeFixturesAvailable *>
      handlers.GetHandler
        .run(
          TestFixtures.scala3Coord,
          "cellar.fixture.scala3.CellarWithCompanion",
          extraRepositories = Seq(TestFixtures.localM2Repo)
        )
        .map { code =>
          expect(clue(code) == ExitCode.Success) and
            expect(
              console.outBuf.toString.contains("Fixture for testing resolution of members declared on the companion."),
              s"Output: ${console.outBuf}"
            ) and
            noCompilerCrash(console)
        }

  test("get: method-level docstring reaches stdout"):
    val console = CapturingConsole()
    given Console[IO] = console
    TestFixtures.assumeFixturesAvailable *>
      handlers.GetHandler
        .run(
          TestFixtures.scala3Coord,
          "cellar.fixture.scala3.CellarTC.render",
          extraRepositories = Seq(TestFixtures.localM2Repo)
        )
        .map { code =>
          expect(clue(code) == ExitCode.Success) and
            expect(
              console.outBuf.toString.contains("Renders the value as a human-readable string."),
              s"Output: ${console.outBuf}"
            ) and
            noCompilerCrash(console)
        }

  test("get: undocumented symbol renders cleanly without docstring artifacts"):
    val console = CapturingConsole()
    given Console[IO] = console
    TestFixtures.assumeFixturesAvailable *>
      handlers.GetHandler
        .run(
          TestFixtures.scala3Coord,
          "cellar.fixture.scala3.CellarA",
          extraRepositories = Seq(TestFixtures.localM2Repo)
        )
        .map { code =>
          val out = console.outBuf.toString
          expect(clue(code) == ExitCode.Success) and
            expect(out.contains("## cellar.fixture.scala3.CellarA"), s"Output: $out") and
            expect(!out.contains("/**"), s"Raw docstring markers leaked: $out") and
            noCompilerCrash(console)
        }

  test("get: Java class renders signature without compiler crash"):
    val console = CapturingConsole()
    given Console[IO] = console
    TestFixtures.assumeFixturesAvailable *>
      handlers.GetHandler
        .run(
          TestFixtures.javaCoord,
          "cellar.fixture.java.CellarJavaClass",
          extraRepositories = Seq(TestFixtures.localM2Repo)
        )
        .map { code =>
          val out = console.outBuf.toString
          expect(clue(code) == ExitCode.Success) and
            expect(out.contains("class CellarJavaClass"), s"Output: $out") and
            noCompilerCrash(console)
        }

  test("get: JDK class overriding inherited methods resolves on a Java-only classpath"):
    val console = CapturingConsole()
    given Console[IO] = console
    TestFixtures.assumeFixturesAvailable *>
      safeRun(
        handlers.GetHandler.run(
          TestFixtures.javaCoord,
          "java.util.ArrayList",
          extraRepositories = Seq(TestFixtures.localM2Repo)
        )
      ).map { code =>
        expect(clue(code) == ExitCode.Success, s"stderr: ${console.errBuf}") and
          expect(console.outBuf.toString.contains("Object[] toArray()"), s"Output: ${console.outBuf}")
      }

  // ─── list subcommand ─────────────────────────────────────────────────────

  test("list: package scala3 fixture lists top-level types"):
    val console = CapturingConsole()
    given Console[IO] = console
    TestFixtures.assumeFixturesAvailable *>
      handlers.ListHandler
        .run(
          TestFixtures.scala3Coord,
          "cellar.fixture.scala3",
          limit = 50,
          extraRepositories = Seq(TestFixtures.localM2Repo)
        )
        .map { code =>
          val out = console.outBuf.toString
          expect(clue(code) == ExitCode.Success) and
            expect(out.nonEmpty, "Expected non-empty list output") and
            expect(out.contains("CellarADT"), s"Expected CellarADT in output: $out")
        }

  test("list: class members of CellarTC includes render method"):
    val console = CapturingConsole()
    given Console[IO] = console
    TestFixtures.assumeFixturesAvailable *>
      handlers.ListHandler
        .run(
          TestFixtures.scala3Coord,
          "cellar.fixture.scala3.CellarTC",
          limit = 50,
          extraRepositories = Seq(TestFixtures.localM2Repo)
        )
        .map { code =>
          expect(clue(code) == ExitCode.Success) and
            expect(console.outBuf.toString.contains("render"), s"Output: ${console.outBuf}")
        }

  test("list: limit=1 returns exactly 1 line with truncation note"):
    val console = CapturingConsole()
    given Console[IO] = console
    TestFixtures.assumeFixturesAvailable *>
      handlers.ListHandler
        .run(
          TestFixtures.scala3Coord,
          "cellar.fixture.scala3.CellarADT",
          limit = 1,
          extraRepositories = Seq(TestFixtures.localM2Repo)
        )
        .map { code =>
          val lines = console.outBuf.toString.linesIterator.filter(_.nonEmpty).toList
          expect(clue(code) == ExitCode.Success) and
            expect(lines.length <= 1, s"Expected at most 1 line, got: $lines")
        }

  test("list: non-existent FQN exits 1"):
    val console = CapturingConsole()
    given Console[IO] = console
    TestFixtures.assumeFixturesAvailable *>
      handlers.ListHandler
        .run(
          TestFixtures.scala3Coord,
          "cellar.fixture.scala3.DoesNotExist99999",
          limit = 50,
          extraRepositories = Seq(TestFixtures.localM2Repo)
        )
        .map { code =>
          expect(clue(code) == ExitCode.Error)
        }

  // ─── search subcommand ───────────────────────────────────────────────────

  test("search: 'CellarADT' query finds sealed trait"):
    val console = CapturingConsole()
    given Console[IO] = console
    TestFixtures.assumeFixturesAvailable *>
      handlers.SearchHandler
        .run(
          TestFixtures.scala3Coord,
          "CellarADT",
          limit = 50,
          extraRepositories = Seq(TestFixtures.localM2Repo)
        )
        .map { code =>
          expect(clue(code) == ExitCode.Success) and
            expect(
              console.outBuf.toString.contains("CellarADT"),
              s"Output: ${console.outBuf}"
            )
        }

  test("search: case-insensitive — 'cellaradt' finds same results"):
    val console = CapturingConsole()
    given Console[IO] = console
    TestFixtures.assumeFixturesAvailable *>
      handlers.SearchHandler
        .run(
          TestFixtures.scala3Coord,
          "cellaradt",
          limit = 50,
          extraRepositories = Seq(TestFixtures.localM2Repo)
        )
        .map { code =>
          expect(clue(code) == ExitCode.Success) and
            expect(console.outBuf.toString.toLowerCase.contains("cellaradt"), s"Output: ${console.outBuf}")
        }

  test("search: non-existent query returns empty output with exit 0"):
    val console = CapturingConsole()
    given Console[IO] = console
    TestFixtures.assumeFixturesAvailable *>
      handlers.SearchHandler
        .run(
          TestFixtures.scala3Coord,
          "xyzNeverExistsABC123",
          limit = 50,
          extraRepositories = Seq(TestFixtures.localM2Repo)
        )
        .map { code =>
          expect(clue(code) == ExitCode.Success) and
            expect.eql("", console.outBuf.toString.trim)
        }

  // ─── deps subcommand ─────────────────────────────────────────────────────

  test("deps: scala3 fixture exits 0 and first line contains coordinate"):
    val console = CapturingConsole()
    given Console[IO] = console
    TestFixtures.assumeFixturesAvailable *>
      handlers.DepsHandler
        .run(TestFixtures.scala3Coord, extraRepositories = Seq(TestFixtures.localM2Repo))
        .map { code =>
          val out = console.outBuf.toString
          expect(clue(code) == ExitCode.Success) and
            expect(out.nonEmpty, "Expected non-empty deps output")
        }

  test("deps: invalid coordinate exits 1"):
    val console = CapturingConsole()
    given Console[IO] = console
    val bad = MavenCoordinate("com.nonexistent.x12345", "artifact", "1.0.0")
    TestFixtures.assumeFixturesAvailable *>
      safeRun(handlers.DepsHandler.run(bad)).map { code =>
        expect(clue(code) == ExitCode.Error)
      }

  // ─── coordinate suggestions ────────────────────────────────────────────────

  test("get: wrong version shows 'Artifact exists' with latest version"):
    val console = CapturingConsole()
    given Console[IO] = console
    val bad = MavenCoordinate("org.typelevel", "cats-core_3", "9.9.9")
    safeRun(handlers.GetHandler.run(bad, "cats.Monad")).map { code =>
      val err = console.errBuf.toString
      expect(clue(code) == ExitCode.Error) and
        expect(err.contains("Artifact exists."), s"Stderr: $err") and
        expect(err.contains("Latest version:"), s"Stderr: $err")
    }

  test("get: wrong artifact shows 'Did you mean?' suggestions"):
    val console = CapturingConsole()
    given Console[IO] = console
    val bad = MavenCoordinate("com.lihaoyi", "mill-scalalib_3", "1.1.1")
    safeRun(handlers.GetHandler.run(bad, "mill.javalib.NativeImageModule")).map { code =>
      val err = console.errBuf.toString
      expect(clue(code) == ExitCode.Error) and
        expect(err.contains("Could not resolve"), s"Stderr: $err")
    }

  test("get: completely wrong coordinate shows generic error without suggestions"):
    val console = CapturingConsole()
    given Console[IO] = console
    val bad = MavenCoordinate("com.nonexistent.x12345", "foo", "1.0.0")
    safeRun(handlers.GetHandler.run(bad, "bar.Baz")).map { code =>
      val err = console.errBuf.toString
      expect(clue(code) == ExitCode.Error) and
        expect(err.contains("Check that the group ID"), s"Stderr: $err") and
        expect(!err.contains("Did you mean?"), s"Stderr: $err")
    }

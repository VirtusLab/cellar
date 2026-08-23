package cellar

import cats.effect.{ExitCode, IO}
import cats.effect.std.Console
import cellar.handlers.MetaHandler
import munit.CatsEffectSuite

class MetaHandlerTest extends CatsEffectSuite:

  private def run(coord: MavenCoordinate): IO[(ExitCode, String)] =
    TestFixtures.assumeFixturesAvailable()
    val console = CapturingConsole()
    given Console[IO] = console
    MetaHandler.run(coord, extraRepositories = Seq(TestFixtures.localM2Repo))
      .map(code => (code, console.outBuf.toString))

  test("meta: scala3 fixture exits 0"):
    run(TestFixtures.scala3Coord).map { (code, _) =>
      assertEquals(code, ExitCode.Success)
    }

  test("meta: scala3 fixture shows description"):
    run(TestFixtures.scala3Coord).map { (_, out) =>
      assert(out.contains("Cellar Scala 3 test fixture"), s"Output: $out")
    }

  test("meta: scala3 fixture shows project URL"):
    run(TestFixtures.scala3Coord).map { (_, out) =>
      assert(out.contains("https://github.com/example/cellar"), s"Output: $out")
    }

  test("meta: scala3 fixture shows Apache-2.0 license"):
    run(TestFixtures.scala3Coord).map { (_, out) =>
      assert(out.contains("Apache-2.0"), s"Output: $out")
    }

  test("meta: scala3 fixture shows SCM URL"):
    run(TestFixtures.scala3Coord).map { (_, out) =>
      assert(out.contains("github.com/example/cellar"), s"Output: $out")
    }

  test("meta: scala3 fixture shows developer"):
    run(TestFixtures.scala3Coord).map { (_, out) =>
      assert(out.contains("Test Developer"), s"Output: $out")
    }

  test("meta: java fixture shows description"):
    run(TestFixtures.javaCoord).map { (_, out) =>
      assert(out.contains("Cellar Java test fixture"), s"Output: $out")
    }

  test("meta: scala2 fixture shows description"):
    run(TestFixtures.scala2Coord).map { (_, out) =>
      assert(out.contains("Cellar Scala 2.13 test fixture"), s"Output: $out")
    }

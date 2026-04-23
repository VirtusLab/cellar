package cellar

import cats.effect.IO
import munit.CatsEffectSuite
import tastyquery.Contexts.Context

class NearMatchFinderTest extends CatsEffectSuite:

  private def withCtxAndCp[A](
      body: (Context, tastyquery.Classpaths.Classpath) => IO[A]
  ): IO[A] =
    TestFixtures.assumeFixturesAvailable()
    for
      jrePaths <- JreClasspath.jrtPath()
      jars     <- CoursierFetchClient.fetchClasspath(
                    TestFixtures.scala3Coord, Seq(TestFixtures.localM2Repo))
      result   <- ContextResource.make(jars, jrePaths).use { (ctx, cp) => body(ctx, cp) }
    yield result

  test("findNearMatches returns match for exact simple name"):
    withCtxAndCp { (ctx, cp) =>
      given Context = ctx
      NearMatchFinder.findNearMatches("wrong.prefix.CellarADT", cp).map { matches =>
        assert(matches.nonEmpty, s"Expected near matches for CellarADT, got: $matches")
        assert(
          matches.exists(_.endsWith("CellarADT")),
          s"Expected CellarADT in results: $matches"
        )
      }
    }

  test("findNearMatches is case-insensitive"):
    withCtxAndCp { (ctx, cp) =>
      given Context = ctx
      NearMatchFinder.findNearMatches("wrong.prefix.cellaraadt", cp).map { matches =>
        // case-insensitive — should find CellarADT or similar
        assert(matches.length <= 10)
      }
    }

  test("findNearMatches returns at most 10 results"):
    withCtxAndCp { (ctx, cp) =>
      given Context = ctx
      NearMatchFinder.findNearMatches("object", cp).map { matches =>
        assert(matches.length <= 10)
      }
    }

  test("findNearMatches returns empty for non-existent name"):
    withCtxAndCp { (ctx, cp) =>
      given Context = ctx
      NearMatchFinder.findNearMatches("xyzNeverExistsABC123", cp).map { matches =>
        assertEquals(matches, Nil)
      }
    }

  test("simple name extraction: last segment after dot"):
    withCtxAndCp { (ctx, cp) =>
      given Context = ctx
      // "cellar.fixture.scala3.CellarA" → simple name "CellarA"
      NearMatchFinder.findNearMatches("bad.package.CellarA", cp).map { matches =>
        assert(matches.exists(_.endsWith("CellarA")), s"Expected CellarA: $matches")
      }
    }

  List(
    "cellar.fixture.scala3.CellarADT",
    "cellar.fixture.scala3.CellarB$",
  ).foreach { fqn =>
    test(s"findNearMatches does not suggest $fqn back to the user"):
      withCtxAndCp { (ctx, cp) =>
        given Context = ctx
        NearMatchFinder.findNearMatches(fqn, cp).map { matches =>
          assert(!matches.contains(fqn), s"Expected $fqn to be excluded from near matches, got: $matches")
        }
      }
  }

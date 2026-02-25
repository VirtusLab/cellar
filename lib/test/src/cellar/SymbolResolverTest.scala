package cellar

import cats.effect.IO
import munit.CatsEffectSuite
import tastyquery.Contexts.Context

class SymbolResolverTest extends CatsEffectSuite:

  private def withCtx[A](body: Context => IO[A]): IO[A] =
    TestFixtures.assumeFixturesAvailable()
    for
      jrePaths <- JreClasspath.jrtPath()
      jars     <- CoursierFetchClient.fetchClasspath(
                    TestFixtures.scala3Coord, Seq(TestFixtures.localM2Repo))
      result   <- ContextResource.make(jars, jrePaths).use { (ctx, _) => body(ctx) }
    yield result

  test("resolve class FQN returns Found with ClassSymbol"):
    withCtx { ctx =>
      given Context = ctx
      SymbolResolver.resolve("cellar.fixture.scala3.CellarADT").map {
        case LookupResult.Found(syms) => assert(syms.nonEmpty)
        case other                    => fail(s"Expected Found, got $other")
      }
    }

  test("resolve package FQN returns IsPackage"):
    withCtx { ctx =>
      given Context = ctx
      SymbolResolver.resolve("cellar.fixture.scala3").map {
        case LookupResult.IsPackage => ()
        case other                  => fail(s"Expected IsPackage, got $other")
      }
    }

  test("resolve non-existent FQN returns NotFound"):
    withCtx { ctx =>
      given Context = ctx
      SymbolResolver.resolve("cellar.fixture.scala3.DoesNotExist99999").map {
        case LookupResult.NotFound => ()
        case other                 => fail(s"Expected NotFound, got $other")
      }
    }

  test("resolve case class FQN returns Found"):
    withCtx { ctx =>
      given Context = ctx
      SymbolResolver.resolve("cellar.fixture.scala3.CellarA").map {
        case LookupResult.Found(syms) => assert(syms.nonEmpty)
        case other                    => fail(s"Expected Found, got $other")
      }
    }

  test("resolve member method returns Found with TermSymbols"):
    withCtx { ctx =>
      given Context = ctx
      // CellarTC has a `render` method
      SymbolResolver.resolve("cellar.fixture.scala3.CellarTC.render").map {
        case LookupResult.Found(syms) =>
          assert(syms.nonEmpty)
          assert(syms.forall(_.name.toString == "render"))
        case other => fail(s"Expected Found for render, got $other")
      }
    }

  test("resolve non-existent method returns PartialMatch"):
    withCtx { ctx =>
      given Context = ctx
      SymbolResolver.resolve("cellar.fixture.scala3.CellarADT.nonExistentXYZ").map {
        case LookupResult.PartialMatch(resolved, missing) =>
          assert(resolved.contains("CellarADT"), s"Expected resolved to contain CellarADT, got $resolved")
          assertEquals(missing, "nonExistentXYZ")
        case other => fail(s"Expected PartialMatch, got $other")
      }
    }

  test("resolve nested type returns Found"):
    withCtx { ctx =>
      given Context = ctx
      SymbolResolver.resolve("cellar.fixture.scala3.CellarOuter.InnerTrait").map {
        case LookupResult.Found(syms) =>
          assert(syms.nonEmpty)
        case other => fail(s"Expected Found for nested type, got $other")
      }
    }

  test("resolve 2-level nested type returns Found"):
    withCtx { ctx =>
      given Context = ctx
      SymbolResolver.resolve("cellar.fixture.scala3.CellarOuter.InnerTrait.innerMethod").map {
        case LookupResult.Found(syms) =>
          assert(syms.nonEmpty)
          assert(syms.exists(_.name.toString == "innerMethod"))
        case other => fail(s"Expected Found for 2-level nested member, got $other")
      }
    }

  test("resolve inherited method returns Found"):
    withCtx { ctx =>
      given Context = ctx
      // midMethod is declared on CellarMid, should be found via CellarLeaf's linearization
      SymbolResolver.resolve("cellar.fixture.scala3.CellarLeaf.midMethod").map {
        case LookupResult.Found(syms) =>
          assert(syms.nonEmpty)
          assert(syms.exists(_.name.toString == "midMethod"))
        case other => fail(s"Expected Found for inherited method, got $other")
      }
    }

  test("resolve inherited nested type returns Found"):
    withCtx { ctx =>
      given Context = ctx
      // InnerTrait is declared on CellarOuter, should be found via CellarLeaf's linearization
      SymbolResolver.resolve("cellar.fixture.scala3.CellarLeaf.InnerTrait").map {
        case LookupResult.Found(syms) =>
          assert(syms.nonEmpty)
        case other => fail(s"Expected Found for inherited nested type, got $other")
      }
    }

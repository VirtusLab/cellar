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

  test("resolve trailing-$ FQN returns the companion module class"):
    withCtx { ctx =>
      given Context = ctx
      // `cellar.fixture.scala3.CellarTC$` should resolve to `object CellarTC`,
      // not the trait of the same name.
      SymbolResolver.resolve("cellar.fixture.scala3.CellarTC$").map {
        case LookupResult.Found(syms) =>
          assert(syms.nonEmpty)
          syms.head match
            case cls: tastyquery.Symbols.ClassSymbol =>
              assert(cls.isModuleClass, s"Expected module class, got $cls")
            case other => fail(s"Expected ClassSymbol, got $other")
        case other => fail(s"Expected Found for trailing-\\$$ FQN, got $other")
      }
    }

  test("resolve companion term member via <Trait>.<member>"):
    withCtx { ctx =>
      given Context = ctx
      // `apply` lives on `object CellarWithCompanion`, not on the trait.
      SymbolResolver.resolve("cellar.fixture.scala3.CellarWithCompanion.apply").map {
        case LookupResult.Found(syms) =>
          assert(syms.nonEmpty)
          assert(syms.exists(_.name.toString == "apply"))
        case other => fail(s"Expected Found for companion apply, got $other")
      }
    }

  test("resolve companion-nested type via <Trait>.<NestedType>"):
    withCtx { ctx =>
      given Context = ctx
      // `CompanionNested` is a trait declared inside `object CellarWithCompanion`.
      SymbolResolver.resolve("cellar.fixture.scala3.CellarWithCompanion.CompanionNested").map {
        case LookupResult.Found(syms) =>
          assert(syms.nonEmpty)
        case other => fail(s"Expected Found for companion-nested type, got $other")
      }
    }

  test("resolve member of companion-nested type via <Trait>.<NestedType>.<member>"):
    withCtx { ctx =>
      given Context = ctx
      // Exercises the intermediate companion fallback in findClassMember.
      SymbolResolver.resolve("cellar.fixture.scala3.CellarWithCompanion.CompanionNested.nestedMethod").map {
        case LookupResult.Found(syms) =>
          assert(syms.nonEmpty)
          assert(syms.exists(_.name.toString == "nestedMethod"))
        case other => fail(s"Expected Found for nested member via companion, got $other")
      }
    }

  test("instance-side resolution still wins over companion"):
    withCtx { ctx =>
      given Context = ctx
      // `instanceMethod` is on the trait, not the companion — must still resolve to the trait's decl.
      SymbolResolver.resolve("cellar.fixture.scala3.CellarWithCompanion.instanceMethod").map {
        case LookupResult.Found(syms) =>
          assert(syms.exists(_.name.toString == "instanceMethod"))
        case other => fail(s"Expected Found for instance member, got $other")
      }
    }

  test("resolve non-existent member on class with companion still returns PartialMatch"):
    withCtx { ctx =>
      given Context = ctx
      SymbolResolver.resolve("cellar.fixture.scala3.CellarWithCompanion.nonExistentXYZ").map {
        case LookupResult.PartialMatch(resolved, missing) =>
          assert(resolved.contains("CellarWithCompanion"), s"Expected resolved to contain CellarWithCompanion, got $resolved")
          assertEquals(missing, "nonExistentXYZ")
        case other => fail(s"Expected PartialMatch, got $other")
      }
    }

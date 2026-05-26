package cellar

import cats.effect.IO
import munit.CatsEffectSuite
import org.typelevel.otel4s.trace.Tracer.Implicits.noop
import tastyquery.Contexts.Context
import tastyquery.Symbols.{ClassSymbol, TermSymbol}

class SymbolResolverTest extends CatsEffectSuite:

  private def withCtx[A](body: Context => IO[A]): IO[A] =
    TestFixtures.assumeFixturesAvailable()
    for
      jrePaths <- JreClasspath.jrtPath()
      jars     <- CoursierFetchClient.fetchClasspath(
                    TestFixtures.scala3Coord, Seq(TestFixtures.localM2Repo))
      result   <- ContextResource.make(jars, jrePaths).use { (ctx, _) => body(ctx) }
    yield result

  private def withScala2Ctx[A](body: Context => IO[A]): IO[A] =
    TestFixtures.assumeFixturesAvailable()
    for
      jrePaths <- JreClasspath.jrtPath()
      jars     <- CoursierFetchClient.fetchClasspath(
                    TestFixtures.scala2Coord, Seq(TestFixtures.localM2Repo))
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

  // Scala 3 top-level defs/types/objects live in a synthetic `<filename>$package$`
  // wrapper class. SymbolResolver fans out into those wrappers so users can refer
  // to top-level symbols by their natural FQN.
  // Fixture: fixtureScala3/src/myapp/Hello.scala
  //   package myapp
  //   @main def hello = println(42)
  //   opaque type Hello = Int
  //   object Hello:
  //     def fromInt(a: Int): Hello = a

  test("top-level @main def myapp.hello resolves to both the def and the synthetic wrapper class"):
    withCtx { ctx =>
      given Context = ctx
      SymbolResolver.resolve("myapp.hello").map {
        case LookupResult.Found(syms) =>
          assert(
            syms.exists(s => s.isInstanceOf[tastyquery.Symbols.TermSymbol] && s.name.toString == "hello"),
            s"Expected to find the top-level def, got $syms"
          )
        case other => fail(s"Expected Found, got $other")
      }
    }

  test("top-level opaque type myapp.Hello resolves"):
    withCtx { ctx =>
      given Context = ctx
      SymbolResolver.resolve("myapp.Hello").map {
        case LookupResult.Found(syms) =>
          assert(syms.exists(_.name.toString == "Hello"), s"Expected to find Hello, got $syms")
        case other => fail(s"Expected Found, got $other")
      }
    }

  test("top-level companion member myapp.Hello.fromInt resolves"):
    withCtx { ctx =>
      given Context = ctx
      SymbolResolver.resolve("myapp.Hello.fromInt").map {
        case LookupResult.Found(syms) =>
          assert(syms.exists(_.name.toString == "fromInt"), s"Expected to find fromInt, got $syms")
        case other => fail(s"Expected Found, got $other")
      }
    }

  // Scala 2 ADT: sealed trait with subtypes nested inside the companion object.
  // Fixture: fixtureScala2/src/cellar/fixture/scala2/CellarADT.scala
  //   sealed trait CellarADT
  //   object CellarADT {
  //     case object CellarAA extends CellarADT
  //     final case class CellarAB(value: Int) extends CellarADT
  //   }

  test("Scala 2: resolve nested case object inside companion returns Found"):
    withScala2Ctx { ctx =>
      given Context = ctx
      SymbolResolver.resolve("cellar.fixture.scala2.CellarADT.CellarAA").map {
        case LookupResult.Found(syms) =>
          assert(syms.nonEmpty, s"Expected non-empty Found, got $syms")
          assert(syms.exists(_.name.toString.stripSuffix("$") == "CellarAA"), s"Expected CellarAA symbol, got $syms")
        case other => fail(s"Expected Found for CellarAA, got $other")
      }
    }

  test("Scala 2: resolve nested case class inside companion returns Found"):
    withScala2Ctx { ctx =>
      given Context = ctx
      SymbolResolver.resolve("cellar.fixture.scala2.CellarADT.CellarAB").map {
        case LookupResult.Found(syms) =>
          assert(syms.nonEmpty, s"Expected non-empty Found, got $syms")
          assert(syms.exists(_.name.toString == "CellarAB"), s"Expected CellarAB symbol, got $syms")
        case other => fail(s"Expected Found for CellarAB, got $other")
      }
    }

  test("Scala 2: resolving sealed trait returns the trait and one symbol for the companion"):
    withScala2Ctx { ctx =>
      given Context = ctx
      SymbolResolver.resolve("cellar.fixture.scala2.CellarADT").map {
        case LookupResult.Found(syms) =>
          // The companion is represented once, by its module class -- not also by
          // the module val that findStaticTerm returns for the same object.
          assertEquals(syms.size, 2, s"Expected trait + companion, got $syms")
          assert(!syms.exists { case t: TermSymbol => t.isModuleVal; case _ => false },
            s"Module val should have been widened to its module class: $syms")
          val moduleClasses = syms.collect { case c: ClassSymbol if c.isModuleClass => c }
          assertEquals(moduleClasses.size, 1, s"Expected exactly one module class, got $syms")
        case other => fail(s"Expected Found, got $other")
      }
    }

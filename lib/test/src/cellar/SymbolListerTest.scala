package cellar

import cats.effect.IO
import munit.CatsEffectSuite
import tastyquery.Contexts.Context

class SymbolListerTest extends CatsEffectSuite:

  private def withCtx[A](body: Context => IO[A]): IO[A] =
    TestFixtures.assumeFixturesAvailable()
    for
      jrePaths <- JreClasspath.jrtPath()
      jars     <- CoursierFetchClient.fetchClasspath(
                    TestFixtures.scala3Coord, Seq(TestFixtures.localM2Repo))
      result   <- ContextResource.make(jars, jrePaths).use { (ctx, _) => body(ctx) }
    yield result

  test("resolve returns Package for package FQN"):
    withCtx { ctx =>
      given Context = ctx
      SymbolLister.resolve("cellar.fixture.scala3").map {
        case ListResolveResult.Found(ListTarget.Package(_)) => ()
        case other                                          => fail(s"Expected Package, got $other")
      }
    }

  test("resolve returns Cls for class FQN"):
    withCtx { ctx =>
      given Context = ctx
      SymbolLister.resolve("cellar.fixture.scala3.CellarADT").map {
        case ListResolveResult.Found(ListTarget.Cls(_)) => ()
        case other                                      => fail(s"Expected Cls, got $other")
      }
    }

  test("resolve returns None for non-existent FQN"):
    withCtx { ctx =>
      given Context = ctx
      SymbolLister.resolve("cellar.fixture.scala3.DoesNotExist99999").map {
        case ListResolveResult.NotFound => ()
        case other                      => fail(s"Expected NotFound, got $other")
      }
    }

  test("listMembers for Package includes known top-level types"):
    withCtx { ctx =>
      given Context = ctx
      SymbolLister.resolve("cellar.fixture.scala3").flatMap {
        case ListResolveResult.Found(target) =>
          SymbolLister.listMembers(target).compile.toList.map { syms =>
            val names = syms.map(_.name.toString)
            assert(names.exists(_.contains("CellarADT")), s"Expected CellarADT in: $names")
          }
        case other => fail(s"Package not found, got $other")
      }
    }

  test("listMembers for Cls includes declared members"):
    withCtx { ctx =>
      given Context = ctx
      SymbolLister.resolve("cellar.fixture.scala3.CellarTC").flatMap {
        case ListResolveResult.Found(target) =>
          SymbolLister.listMembers(target).compile.toList.map { syms =>
            val names = syms.map(_.name.toString)
            assert(names.contains("render"), s"Expected render in: $names")
          }
        case other => fail(s"CellarTC not found, got $other")
      }
    }

  test("listMembers does not include private members"):
    withCtx { ctx =>
      given Context = ctx
      SymbolLister.resolve("cellar.fixture.scala3.CellarA").flatMap {
        case ListResolveResult.Found(target) =>
          SymbolLister.listMembers(target).compile.toList.map { syms =>
            syms.foreach { sym =>
              sym match
                case ts: tastyquery.Symbols.TermSymbol =>
                  assert(!ts.isPrivate, s"Private symbol leaked: ${sym.name}")
                case _ => ()
            }
          }
        case other => fail(s"CellarA not found, got $other")
      }
    }

package cellar

import cats.effect.IO
import munit.CatsEffectSuite
import tastyquery.Contexts.Context

class GetFormatterTest extends CatsEffectSuite:

  private def withCtx[A](body: Context => IO[A]): IO[A] =
    TestFixtures.assumeFixturesAvailable()
    for
      jrePaths <- JreClasspath.jrtPath()
      jars     <- CoursierFetchClient.fetchClasspath(
                    TestFixtures.scala3Coord, Seq(TestFixtures.localM2Repo))
      result   <- ContextResource.make(jars, jrePaths).use { (ctx, _) => body(ctx) }
    yield result

  test("formatSymbol for trait produces ## heading and trait keyword"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarADT")
        val output = GetFormatter.formatSymbol(cls)
        assert(output.contains("## "), s"Expected heading in: $output")
        assert(output.contains("trait"), s"Expected 'trait' in: $output")
      }
    }

  test("formatSymbol for sealed trait produces Known subtypes line"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarADT")
        val output = GetFormatter.formatSymbol(cls)
        assert(output.contains("**Known subtypes:**"), s"Expected subtypes in: $output")
      }
    }

  test("formatSymbol for sealed trait lists CellarA in subtypes"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarADT")
        val output = GetFormatter.formatSymbol(cls)
        assert(output.contains("CellarA"), s"Expected CellarA in subtypes: $output")
      }
    }

  test("formatSymbol for non-sealed class has no Known subtypes line"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarA")
        val output = GetFormatter.formatSymbol(cls)
        assert(!output.contains("**Known subtypes:**"), s"Unexpected subtypes: $output")
      }
    }

  test("formatSymbol output contains a scala code fence"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarADT")
        val output = GetFormatter.formatSymbol(cls)
        assert(output.contains("```scala"), s"Expected code fence in: $output")
      }
    }

  test("formatSymbol members includes declared method"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarTC")
        val output = GetFormatter.formatSymbol(cls)
        assert(output.contains("render"), s"Expected 'render' in members: $output")
      }
    }

  test("formatSymbol members does not include Object methods"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarTC")
        val output = GetFormatter.formatSymbol(cls)
        assert(!output.contains("notify"), s"Unexpected Object method in: $output")
        assert(!output.contains("finalize"), s"Unexpected Object method in: $output")
      }
    }

  test("formatGetResult separates multiple symbols with ---"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls1   = ctx.findStaticClass("cellar.fixture.scala3.CellarA")
        val cls2   = ctx.findStaticClass("cellar.fixture.scala3.CellarADT")
        val output = GetFormatter.formatGetResult("test", List(cls1, cls2))
        assert(output.contains("---"), s"Expected separator in: $output")
      }
    }

  test("formatGetResult for single symbol has no --- separator"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarA")
        val output = GetFormatter.formatGetResult("test", List(cls))
        assert(!output.contains("---"), s"Unexpected separator: $output")
      }
    }

  test("formatSymbol members includes all overloaded methods"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarOverloaded")
        val output = GetFormatter.formatSymbol(cls)
        val processCount = output.linesIterator.count(_.contains("def process("))
        assertEquals(processCount, 3, s"Expected 3 process overloads in:\n$output")
      }
    }

  test("formatSymbol members includes inherited overloaded methods"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarOverloadedChild")
        val output = GetFormatter.formatSymbol(cls)
        val actionCount = output.linesIterator.count(_.contains("def action("))
        assertEquals(actionCount, 2, s"Expected 2 action overloads in:\n$output")
      }
    }

  private def withJavaCtx[A](body: Context => IO[A]): IO[A] =
    TestFixtures.assumeFixturesAvailable()
    for
      jrePaths <- JreClasspath.jrtPath()
      jars     <- CoursierFetchClient.fetchClasspath(
                    TestFixtures.javaCoord, Seq(TestFixtures.localM2Repo))
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

  test("formatSymbol members includes all overloaded methods (Java)"):
    withJavaCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.java.CellarJavaClass")
        val output = GetFormatter.formatSymbol(cls)
        val formatCount = output.linesIterator.count(_.contains("def format("))
        assertEquals(formatCount, 3, s"Expected 3 format overloads in:\n$output")
      }
    }

  test("formatSymbol companion members include full signatures"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala3.CellarTC")
        val output = GetFormatter.formatSymbol(cls)
        assert(output.contains("**Companion members:**"), s"Expected companion section in: $output")
        assert(output.contains("def apply"), s"Expected 'def apply' signature in companion: $output")
        assert(output.contains("CellarTC[A]"), s"Expected return type in companion signature: $output")
      }
    }

  test("formatSymbol members includes all overloaded methods (Scala 2)"):
    withScala2Ctx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls    = ctx.findStaticClass("cellar.fixture.scala2.CellarOverloaded")
        val output = GetFormatter.formatSymbol(cls)
        val processCount = output.linesIterator.count(_.contains("def process("))
        assertEquals(processCount, 3, s"Expected 3 process overloads in:\n$output")
      }
    }

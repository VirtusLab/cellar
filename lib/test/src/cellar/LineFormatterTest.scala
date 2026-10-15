package cellar

import cats.effect.IO
import munit.CatsEffectSuite
import org.typelevel.otel4s.trace.Tracer.Implicits.noop
import tastyquery.Contexts.Context

class LineFormatterTest extends CatsEffectSuite:

  private def withCtx[A](body: Context => IO[A]): IO[A] =
    TestFixtures.assumeFixturesAvailable()
    for
      jrePaths <- JreClasspath.jrtPath()
      jars     <- CoursierFetchClient.fetchClasspath(
                    TestFixtures.scala3Coord, Seq(TestFixtures.localM2Repo))
      result   <- ContextResource.make(jars, jrePaths).use { (ctx, _) => body(ctx) }
    yield result

  test("formatLine for a sealed trait starts with 'sealed trait'"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls  = ctx.findStaticClass("cellar.fixture.scala3.CellarADT")
        val line = LineFormatter.formatLine(cls)
        assert(line.startsWith("sealed trait"), s"Expected 'sealed trait' prefix in: $line")
      }
    }

  test("formatLine output contains no embedded newlines"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls  = ctx.findStaticClass("cellar.fixture.scala3.CellarA")
        val line = LineFormatter.formatLine(cls)
        assert(!line.contains('\n'), s"Embedded newline in: $line")
      }
    }

  test("formatLine for an abstract method starts with 'abstract def'"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls     = ctx.findStaticClass("cellar.fixture.scala3.CellarTC")
        val members = cls.declarations
        val renderSym = members.find(_.name.toString == "render")
        renderSym.foreach { sym =>
          val line = LineFormatter.formatLine(sym)
          assert(line.startsWith("abstract def"), s"Expected 'abstract def' prefix: $line")
        }
      }
    }

  test("formatLine does not throw for Java symbol"):
    withCtx { ctx =>
      IO.blocking {
        given Context = ctx
        val cls  = ctx.findStaticClass("java.lang.String")
        val line = LineFormatter.formatLine(cls)
        assert(line.nonEmpty)
      }
    }

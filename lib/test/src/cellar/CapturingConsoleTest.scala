package cellar

import cats.effect.IO
import cats.effect.std.Console
import cats.syntax.all.*
import munit.CatsEffectSuite

class CapturingConsoleTest extends CatsEffectSuite:

  test("print and error capture, without adding newlines"):
    CapturingConsole
      .capture(Console[IO].print("a") *> Console[IO].print("b") *> Console[IO].error("x"))
      .map { (_, out, err) =>
        assertEquals(out, "ab")
        assertEquals(err, "x")
      }

  test("println and errorln terminate each write with a newline"):
    CapturingConsole.capture(Console[IO].println("a") *> Console[IO].errorln("x")).map { (_, out, err) =>
      assertEquals(out, "a\n")
      assertEquals(err, "x\n")
    }

  test("readLine raises EOF, so a read-until-valid prompt cannot spin"):
    CapturingConsole.capture(Console[IO].readLine.attempt).map { (result, _, _) =>
      assert(result.isLeft, s"expected EOF, got $result")
    }

  test("concurrent writes are all recorded"):
    CapturingConsole.capture(List.range(0, 200).parTraverse_(i => Console[IO].println(i))).map { (_, out, _) =>
      assertEquals(out.linesIterator.size, 200)
    }

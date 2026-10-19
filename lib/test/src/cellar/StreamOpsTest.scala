package cellar

import cats.effect.IO
import cats.effect.std.Console
import fs2.Stream
import weaver.SimpleIOSuite

object StreamOpsTest extends SimpleIOSuite:

  // Test console that captures stderr lines
  class CapturingConsole extends Console[IO]:
    var errLines: List[String]        = Nil
    var outLines: List[String]        = Nil
    def readLineWithCharset(charset: java.nio.charset.Charset): IO[String] = IO.pure("")
    def print[A](a: A)(using fmt: cats.Show[A]): IO[Unit]                 = IO.unit
    def println[A](a: A)(using fmt: cats.Show[A]): IO[Unit] =
      IO { outLines = fmt.show(a) :: outLines }
    def error[A](a: A)(using fmt: cats.Show[A]): IO[Unit]      = IO.unit
    def errorln[A](a: A)(using fmt: cats.Show[A]): IO[Unit] =
      IO { errLines = fmt.show(a) :: errLines }

  test("bounded: stream shorter than limit returns full list without truncation"):
    val console = CapturingConsole()
    given Console[IO] = console
    val stream = Stream.emits(1 to 30)
    StreamOps.bounded(stream, 50).map { result =>
      expect.eql(30, result.length) and
        expect(console.errLines.isEmpty, "no truncation note expected")
    }

  test("bounded: stream exactly at limit returns full list without truncation"):
    val console = CapturingConsole()
    given Console[IO] = console
    val stream = Stream.emits(1 to 50)
    StreamOps.bounded(stream, 50).map { result =>
      expect.eql(50, result.length) and
        expect(console.errLines.isEmpty)
    }

  test("bounded: stream one over limit returns limit elements with truncation note"):
    val console = CapturingConsole()
    given Console[IO] = console
    val stream = Stream.emits(1 to 51)
    StreamOps.bounded(stream, 50).map { result =>
      expect.eql(50, result.length) and
        expect(console.errLines.nonEmpty) and
        expect(console.errLines.exists(_.contains("truncated")))
    }

  test("bounded: large stream returns only limit elements"):
    val console = CapturingConsole()
    given Console[IO] = console
    val stream = Stream.emits(1 to 10000)
    StreamOps.bounded(stream, 50).map { result =>
      expect.eql(50, result.length) and
        expect.eql(1, console.errLines.length) // exactly one note
    }

  test("bounded: limit=0 on non-empty stream returns empty list with truncation"):
    val console = CapturingConsole()
    given Console[IO] = console
    val stream = Stream.emits(1 to 5)
    StreamOps.bounded(stream, 0).map { result =>
      expect.eql(Nil, result) and
        expect(console.errLines.nonEmpty)
    }

  test("bounded: truncation note contains the limit value"):
    val console = CapturingConsole()
    given Console[IO] = console
    val stream = Stream.emits(1 to 100)
    StreamOps.bounded(stream, 7).map { _ =>
      expect(console.errLines.exists(_.contains("7")))
    }

  test("boundedWithFlag: returns false when not truncated"):
    val stream = Stream.emits(1 to 10)
    StreamOps.boundedWithFlag(stream, 50).map { (results, truncated) =>
      expect.eql(10, results.length) and
        expect.eql(false, truncated)
    }

  test("boundedWithFlag: returns true when truncated"):
    val stream = Stream.emits(1 to 100)
    StreamOps.boundedWithFlag(stream, 50).map { (results, truncated) =>
      expect.eql(50, results.length) and
        expect.eql(true, truncated)
    }

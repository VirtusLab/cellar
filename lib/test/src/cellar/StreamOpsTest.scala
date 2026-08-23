package cellar

import fs2.Stream
import munit.CatsEffectSuite

class StreamOpsTest extends CatsEffectSuite:

  test("bounded: stream shorter than limit returns full list without truncation"):
    CapturingConsole.capture(StreamOps.bounded(Stream.emits(1 to 30), 50)).map { (result, _, err) =>
      assertEquals(result.length, 30)
      assert(err.isEmpty, "no truncation note expected")
    }

  test("bounded: stream exactly at limit returns full list without truncation"):
    CapturingConsole.capture(StreamOps.bounded(Stream.emits(1 to 50), 50)).map { (result, _, err) =>
      assertEquals(result.length, 50)
      assert(err.isEmpty)
    }

  test("bounded: stream one over limit returns limit elements with truncation note"):
    CapturingConsole.capture(StreamOps.bounded(Stream.emits(1 to 51), 50)).map { (result, _, err) =>
      assertEquals(result.length, 50)
      assert(err.nonEmpty)
      assert(err.contains("truncated"))
    }

  test("bounded: large stream returns only limit elements"):
    CapturingConsole.capture(StreamOps.bounded(Stream.emits(1 to 10000), 50)).map { (result, _, err) =>
      assertEquals(result.length, 50)
      assertEquals(err.linesIterator.size, 1) // exactly one note
    }

  test("bounded: limit=0 on non-empty stream returns empty list with truncation"):
    CapturingConsole.capture(StreamOps.bounded(Stream.emits(1 to 5), 0)).map { (result, _, err) =>
      assertEquals(result, Nil)
      assert(err.nonEmpty)
    }

  test("bounded: truncation note contains the limit value"):
    CapturingConsole.capture(StreamOps.bounded(Stream.emits(1 to 100), 7)).map { (_, _, err) =>
      assert(err.contains("7"))
    }

  test("boundedWithFlag: returns false when not truncated"):
    val stream = Stream.emits(1 to 10)
    StreamOps.boundedWithFlag(stream, 50).map { (results, truncated) =>
      assertEquals(results.length, 10)
      assertEquals(truncated, false)
    }

  test("boundedWithFlag: returns true when truncated"):
    val stream = Stream.emits(1 to 100)
    StreamOps.boundedWithFlag(stream, 50).map { (results, truncated) =>
      assertEquals(results.length, 50)
      assertEquals(truncated, true)
    }

package cellar

import weaver.FunSuite

object ScalaSourcesTest extends FunSuite:

  test("Scala 2 sources written with -Xsource:3 parse"):
    expect(ScalaSources.parseSource("package p\nopen class Props(val a: Int)\n", scala2 = true).isDefined)

  test("Scala 3 sources using capture checking parse"):
    val text = "package p\ntrait It[A]:\n  def sliding(size: Int, step: Int = 1): It[A]^{this}\n"
    expect(ScalaSources.parseSource(text, scala2 = false).isDefined)

  test("a source too deeply nested for the parser yields nothing instead of failing"):
    val deep = "object Deep { def f(x: Int): Int = " + ("if (x == 0) 0 else " * 20000) + "1 }"
    expect(ScalaSources.parseSource(deep, scala2 = true).isEmpty)

package cellar.bench

import munit.FunSuite

class ScalaSourceDeclsTest extends FunSuite:

  private def decls(source: String, scala2: Boolean = false): SourceSide =
    ScalaSourceDecls(scala2).read(Iterator("p/Api.scala" -> source))

  private def text(source: String, key: String): String =
    decls(source).decls.find(_.key == key).map(_.text).getOrElse(fail(s"no $key in ${decls(source).decls}"))

  test("a generic extension's type parameters lead its method"):
    val source = "package p\nobject O:\n  extension [A](fa: List[A]) def foo[B](x: B): Int = 0\n"
    assertEquals(text(source, "term:foo(fa)(x)"), "def foo[A](fa: List[A])[B](x: B): Int")

  test("interleaved type parameter clauses keep their order"):
    val source = "package p\nobject O:\n  def foo[A](a: A)[B](b: B): Int = 0\n"
    assertEquals(text(source, "term:foo(a)(b)"), "def foo[A](a: A)[B](b: B): Int")

  test("a private named given is neither compared nor counted as a gap"):
    val side = decls("package p\nobject O:\n  private given foo: Ordering[Int] = ???\n")
    assertEquals(side.decls.map(_.key), List("class"))
    assertEquals(side.knownGaps, Map.empty)

  test("an anonymous given is counted as a gap"):
    val side = decls("package p\nobject O:\n  given Ordering[Int] = ???\n")
    assertEquals(side.knownGaps, Map("anonymous or template givens (not compared)" -> 1))

  test("a field from an implicit clause is an implicit val"):
    val source = "package p\ncase class C[O](o: O)(implicit val ct: Ordering[O])\n"
    assertEquals(decls(source).decls.find(_.key == "term:ct").map(_.text), Some("implicit val ct: Ordering[O]"))

  test("a field from a Scala 2 implicit clause is an implicit val"):
    val source = "package p\ncase class C[O](o: O)(implicit val ct: Ordering[O])\n"
    assertEquals(decls(source, scala2 = true).decls.find(_.key == "term:ct").map(_.text), Some("implicit val ct: Ordering[O]"))

package cellar.bench

import weaver.{Expectations, FunSuite}

object ScalaSourceDeclsTest extends FunSuite:

  private def decls(source: String, scala2: Boolean = false): SourceSide =
    ScalaSourceDecls(scala2).read(Iterator("p/Api.scala" -> source))

  private def text(source: String, key: String)(check: String => Expectations): Expectations =
    decls(source).decls.find(_.key == key).map(_.text).fold(failure(s"no $key in ${decls(source).decls}"))(check)

  test("a generic extension's type parameters lead its method"):
    val source = "package p\nobject O:\n  extension [A](fa: List[A]) def foo[B](x: B): Int = 0\n"
    text(source, "term:foo(fa)(x)")(expect.eql("def foo[A](fa: List[A])[B](x: B): Int", _))

  test("interleaved type parameter clauses keep their order"):
    val source = "package p\nobject O:\n  def foo[A](a: A)[B](b: B): Int = 0\n"
    text(source, "term:foo(a)(b)")(expect.eql("def foo[A](a: A)[B](b: B): Int", _))

  test("a private named given is neither compared nor counted as a gap"):
    val side = decls("package p\nobject O:\n  private given foo: Ordering[Int] = ???\n")
    expect.eql(List("class"), side.decls.map(_.key)) and
      expect.eql(Map.empty[String, Int], side.knownGaps)

  test("an anonymous given is counted as a gap"):
    val side = decls("package p\nobject O:\n  given Ordering[Int] = ???\n")
    expect.eql(Map("anonymous or template givens (not compared)" -> 1), side.knownGaps)

  test("a field from an implicit clause is an implicit val"):
    val source = "package p\ncase class C[O](o: O)(implicit val ct: Ordering[O])\n"
    expect.eql(Some("implicit val ct: Ordering[O]"), decls(source).decls.find(_.key == "term:ct").map(_.text))

  test("a field from a Scala 2 implicit clause is an implicit val"):
    val source = "package p\ncase class C[O](o: O)(implicit val ct: Ordering[O])\n"
    expect.eql(Some("implicit val ct: Ordering[O]"), decls(source, scala2 = true).decls.find(_.key == "term:ct").map(_.text))

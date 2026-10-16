package cellar.fixture.scala3

/** Scala 3 type class with given instances. */
trait CellarTC[A]:
  /** Renders the value as a human-readable string. */
  def render(a: A): String

object CellarTC:
  def apply[A](using tc: CellarTC[A]): CellarTC[A] = tc

  def renderAll[A: CellarTC, B](as: List[A], b: B)(using sep: String): String = as.map(CellarTC[A].render).mkString(sep) + b.toString

  extension [A: CellarTC](a: A) def maxWith[B](b: B)(other: A): A = if CellarTC[A].render(a) < b.toString then a else other

given CellarTC[Int] with
  def render(i: Int): String = i.toString

given CellarTC[String] with
  def render(s: String): String = s

given [A: CellarTC]: CellarTC[List[A]] with
  def render(as: List[A]): String =
    as.map(CellarTC[A].render).mkString("[", ", ", "]")

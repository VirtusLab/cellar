package cellar.fixture.scala3

/** Scala 3 type class with given instances. */
trait CellarTC[A]:
  /** Renders the value as a human-readable string. */
  def render(a: A): String

object CellarTC:
  def apply[A](using tc: CellarTC[A]): CellarTC[A] = tc

given CellarTC[Int] with
  def render(i: Int): String = i.toString

given CellarTC[String] with
  def render(s: String): String = s

given [A: CellarTC]: CellarTC[List[A]] with
  def render(as: List[A]): String =
    as.map(CellarTC[A].render).mkString("[", ", ", "]")

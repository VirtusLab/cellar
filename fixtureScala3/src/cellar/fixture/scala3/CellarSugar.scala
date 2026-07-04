package cellar.fixture.scala3

/** Fixture for signature rendering: function-type sugar + implicit/using lists. */
trait CellarShow[A]:
  def show(a: A): A

/** Symbolic binary type constructor, to exercise infix rendering (e.g. `F ~> G`). */
trait ~>[F[_], G[_]]:
  def apply[A](fa: F[A]): G[A]

trait CellarSugar:
  def mapK[F[_], G[_]](f: F ~> G): Unit
  def wildcard: List[?]
  def boundedWildcard: List[? <: AnyRef]

  def transform[A, B](f: A => B): B
  def zip[A, B, C](f: (A, B) => C): C
  def nested[A, B, C](f: (A => B) => C): C
  def thunk[A](f: () => A): A
  def ctx[A, B](f: A ?=> B): B
  def pair[A, B](t: (A, B)): (A, B)
  def triple[A, B, C](t: (A, B, C)): (A, B, C)
  def tupleArg[A, B](f: ((A, B)) => Boolean): Boolean
  def withImplicit[A](a: A)(implicit s: CellarShow[A]): A
  def withUsing[A](a: A)(using s: CellarShow[A]): A

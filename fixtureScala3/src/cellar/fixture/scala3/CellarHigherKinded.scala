package cellar.fixture.scala3

/** Fixture for signature rendering: higher-kinded type parameters. */
trait CellarBox[F[_]]

trait CellarBoundedBox[F[_ <: AnyRef]]

/** Passing a type constructor where `F[_]` is expected: TASTy stores the eta-expansion
  * `[A] =>> CellarSelfBox[A]`, which should print as the constructor a reader would write.
  */
class CellarSelfBox[A] extends CellarBox[CellarSelfBox]

trait CellarHigherKinded:
  /** A hand-written eta-expansion, structurally identical to what the compiler generates when a
    * type constructor is passed where `F[_]` is expected.
    */
  type HandWrittenEta = [A] =>> List[A]

  def wrap[F[_], A](fa: F[A]): F[A]
  def compose[F[_], G[_]](bf: CellarBox[F], bg: CellarBox[G]): CellarBox[[A] =>> F[G[A]]]
  def composeBounded[F[_]](bf: CellarBox[F]): CellarBoundedBox[[A <: AnyRef] =>> F[A]]
  def bounded[F[X <: AnyRef]](fa: F[String]): F[String]
  def upper[F[A] <: Iterable[A]](fa: F[Int]): F[Int]
  def selfBounded[F[A <: Comparable[A]]](fa: F[String]): F[String]
  def bimap[G[_, _], A, B](g: G[A, B]): G[A, B]

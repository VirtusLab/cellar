package cellar.fixture.scala2

/** Fixture for modifiers printed on member lines. */
class CellarModifiers {
  lazy val lazyValue: Int = 1
  var mutable: Int = 0
  implicit object ImplicitObject
  protected def guarded: Int = 0
  private[scala2] def internal: Int = 0
}

/** Fixture for the parents scalac adds to objects: the companion of a serializable class gets
  * `java.io.Serializable`, a case class's synthetic companion `AbstractFunctionN`, and a written
  * `Serializable` or `AbstractFunctionN` stays.
  */
trait CellarSerialized extends Serializable
object CellarSerialized

final case class CellarPair(a: Int, b: String)

final case class CellarWrapped(a: Int)
object CellarWrapped extends scala.runtime.AbstractFunction1[Int, CellarWrapped]

object CellarSerializable extends Serializable

package cellar.fixture.scala2

/** Fixture for modifiers printed on member lines. */
class CellarModifiers {
  lazy val lazyValue: Int = 1
  var mutable: Int = 0
  implicit object ImplicitObject
  protected def guarded: Int = 0
  private[scala2] def internal: Int = 0
}

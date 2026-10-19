package cellar.fixture.scala3

/** Fixture for modifiers printed on member lines. */
class CellarModifiers:
  lazy val lazyValue: Int = 1
  var mutable: Int = 0
  given Int = 0
  protected def guarded: Int = 0
  private[scala3] def internal: Int = 0

/** Fixture for an access qualifier that names an object. */
object CellarScope:
  private[CellarScope] def origin: Int = 0

/** Fixture for how an enum and its cases are printed. */
enum CellarShape:
  case Circle, Square
  case Polygon(sides: Int)

package cellar.fixture.scala3

/** Fixture for testing nested type resolution and inherited member lookup. */
trait CellarOuter:
  trait InnerTrait:
    def innerMethod: String

  trait DeepNested:
    def deepMethod: Int

trait CellarMid extends CellarOuter:
  def midMethod: String

trait CellarLeaf extends CellarMid:
  def leafMethod: Boolean

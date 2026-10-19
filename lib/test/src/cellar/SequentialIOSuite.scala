package cellar

import weaver.SimpleIOSuite

/** For suites whose tests each boot a tasty-query Context or a build tool: run concurrently, as
  * weaver does by default, they exhaust the heap of a CI runner.
  */
abstract class SequentialIOSuite extends SimpleIOSuite:
  override def maxParallelism: Int = 1

package cellar.profiling

import scala.concurrent.ExecutionContext

/** Hook for wrapping an [[ExecutionContext]] so tasks running on the compute
  * pool can be labeled for Pyroscope profile↔span correlation. Pass-through
  * for now; the actual label propagation is deferred until Block 2.9
  * confirms what's needed on top of the JFR profiler's built-in context
  * tracking.
  */
object ProfilingExecutionContext:
  def wrap(ec: ExecutionContext): ExecutionContext = ec

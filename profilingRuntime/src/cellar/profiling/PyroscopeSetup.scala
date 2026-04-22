package cellar.profiling

import cats.effect.{IO, Resource}
import io.pyroscope.javaagent.PyroscopeAgent
import io.pyroscope.javaagent.config.Config
import scala.concurrent.ExecutionContext

object PyroscopeSetup:

  /** Starts the Pyroscope JVM agent and stops it on resource release. The
    * agent runs in a daemon thread; multiple `start` calls are no-ops.
    */
  def resource(serverAddress: String, applicationName: String): Resource[IO, Unit] =
    Resource.make {
      IO.blocking {
        val cfg = new Config.Builder()
          .setApplicationName(applicationName)
          .setServerAddress(serverAddress)
          .build()
        PyroscopeAgent.start(cfg)
      }
    }(_ => IO.blocking(PyroscopeAgent.stop()).attempt.void)

  /** Compute-pool transform for Pyroscope profile↔span correlation. Callers
    * pass this to [[cats.effect.unsafe.IORuntimeBuilder.transformCompute]]
    * when wiring a custom [[cats.effect.unsafe.IORuntime]].
    */
  val computeTransform: ExecutionContext => ExecutionContext =
    ProfilingExecutionContext.wrap

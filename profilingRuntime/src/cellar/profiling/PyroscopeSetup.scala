package cellar.profiling

import cats.effect.{IO, Resource}
import io.pyroscope.http.Format
import io.pyroscope.javaagent.{EventType, PyroscopeAgent}
import io.pyroscope.javaagent.api.Logger
import io.pyroscope.javaagent.config.Config
import org.http4s.Uri

import java.net.{InetSocketAddress, Socket}

object PyroscopeSetup:

  /** Starts the Pyroscope JVM agent using ITIMER sampling with JFR output format
    * (same as SLS; works on macOS without elevated privileges). Stops on release.
    * Skips silently if the server is unreachable.
    */
  def resource(serverAddress: String, applicationName: String): Resource[IO, Unit] =
    if TracingRuntime.NativeImage then Resource.unit
    else Resource.eval(reachable(serverAddress)).flatMap {
      case false => Resource.unit
      case true  =>
        Resource.make {
          IO.blocking {
            val cfg = new Config.Builder()
              .setApplicationName(applicationName)
              .setServerAddress(serverAddress)
              .setProfilingEvent(EventType.ITIMER)
              .setFormat(Format.JFR)
              .setLogLevel(Logger.Level.ERROR)
              .build()
            PyroscopeAgent.start(cfg)
          }
        }(_ => IO.blocking(PyroscopeAgent.stop()).attempt.void)
    }

  private def reachable(serverAddress: String): IO[Boolean] =
    IO.fromEither(Uri.fromString(serverAddress)).flatMap { uri =>
      uri.host match
        case None    => IO.pure(false)
        case Some(h) =>
          val port = uri.port.getOrElse(4040)
          IO.blocking {
            val socket = new Socket()
            try
              socket.connect(new InetSocketAddress(h.value, port), 500)
              true
            catch case _: Exception => false
            finally socket.close()
          }
    }.handleError(_ => false)

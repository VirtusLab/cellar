package cellar.profiling

import cats.effect.{IO, Resource}
import io.pyroscope.http.Format
import io.pyroscope.javaagent.{EventType, PyroscopeAgent}
import io.pyroscope.javaagent.api.Logger
import io.pyroscope.javaagent.config.Config

import java.time.Duration

object PyroscopeSetup:

  // Pyroscope's default logger writes upload errors to stderr; silence it
  // so a misconfigured remote doesn't paint the CLI's output.
  private val silentLogger: Logger =
    new Logger:
      override def log(level: Logger.Level, message: String, args: Object*): Unit = ()

  /** Starts the Pyroscope JVM agent using ITIMER sampling with JFR output format
    * (same as SLS; works on macOS without elevated privileges). Stops on release.
    */
  def resource(serverAddress: String, applicationName: String): Resource[IO, Unit] =
    if TracingRuntime.NativeImage then Resource.unit
    else
      Resource.make {
        IO.blocking {
          val cfg = new Config.Builder()
            .setApplicationName(applicationName)
            .setServerAddress(serverAddress)
            // WALL sampling drops span_id labels — breaks the trace↔profile link in Grafana.
            .setProfilingEvent(EventType.ITIMER)
            .setFormat(Format.JFR)
            // Cellar commands are short-lived. Defaults (10s sampling/upload window,
            // 10s export timeout) mean sub-10s runs only ever flush at shutdown, and
            // the final POST may not complete before the JVM exits. Tighten so that
            // a snapshot is uploaded every 1s during the run, and the shutdown drain
            // has up to 10s to finish.
            .setSamplingDuration(Duration.ofSeconds(1))
            .setUploadInterval(Duration.ofSeconds(1))
            .setProfileExportTimeout(Duration.ofSeconds(10))
            .build()
          val opts = new PyroscopeAgent.Options.Builder(cfg)
            .setLogger(silentLogger)
            .build()
          PyroscopeAgent.start(opts)
        }
      }(_ => IO.blocking(PyroscopeAgent.stop()).attempt.void)

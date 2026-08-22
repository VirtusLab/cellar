package cellar

import cats.effect.IO
import coursierapi.Cache
import org.typelevel.log4cats.Logger

enum LogLevel:
  case Off, Verbose, Debug

object LogLevel:
  def parse(raw: String): Option[LogLevel] =
    raw.trim.toLowerCase match
      case "off"     => Some(Off)
      case "verbose" => Some(Verbose)
      case "debug"   => Some(Debug)
      case _         => None

  /** `CELLAR_LOG=verbose|debug` sets the level without a CLI flag, so a released native binary can
    * be debugged in place. An explicit `--verbose` / `--debug` overrides it.
    */
  def fromEnv(env: Map[String, String]): LogLevel =
    env.get("CELLAR_LOG").flatMap(parse).getOrElse(Off)

/**
 * A `Logger[IO]` writing to stderr — stdout carries the Markdown payload and must stay clean enough
 * to pipe into a prompt.
 *
 * Deliberately not backed by slf4j: `slf4j-nop` is on the CLI classpath to silence coursier, and a
 * real slf4j backend would both un-silence it and add native-image weight.
 */
final class StderrLogger(val level: LogLevel) extends Logger[IO]:
  private def enabled(min: LogLevel): Boolean = level.ordinal >= min.ordinal

  private def emit(min: LogLevel, msg: => String, err: Option[Throwable]): IO[Unit] =
    if !enabled(min) then IO.unit
    else
      IO.blocking {
        System.err.println(s"[cellar] $msg")
        // A stack trace is noise unless you are actually digging: debug only.
        err.foreach(t =>
          if level == LogLevel.Debug then t.printStackTrace()
          else System.err.println(s"[cellar]   ${t.getClass.getName}: ${t.getMessage}")
        )
      }

  def error(message: => String): IO[Unit] = emit(LogLevel.Verbose, message, None)
  def warn(message: => String): IO[Unit]  = emit(LogLevel.Verbose, message, None)
  def info(message: => String): IO[Unit]  = emit(LogLevel.Verbose, message, None)
  def debug(message: => String): IO[Unit] = emit(LogLevel.Debug, message, None)
  def trace(message: => String): IO[Unit] = emit(LogLevel.Debug, message, None)

  def error(t: Throwable)(message: => String): IO[Unit] = emit(LogLevel.Verbose, message, Some(t))
  def warn(t: Throwable)(message: => String): IO[Unit]  = emit(LogLevel.Verbose, message, Some(t))
  def info(t: Throwable)(message: => String): IO[Unit]  = emit(LogLevel.Verbose, message, Some(t))
  def debug(t: Throwable)(message: => String): IO[Unit] = emit(LogLevel.Debug, message, Some(t))
  def trace(t: Throwable)(message: => String): IO[Unit] = emit(LogLevel.Debug, message, Some(t))

object StderrLogger:
  /** The silent default, so library code and tests need no wiring. */
  val off: Logger[IO] = new StderrLogger(LogLevel.Off)

object CoursierLogging:
  /**
   * A coursier cache that reports download activity to stderr when diagnostics are on, so a slow or
   * failing fetch shows which URLs are being tried.
   *
   * `coursierapi.Logger` exposes only the `nop()` and `progressBars()` factories — there are no
   * overridable callbacks to route events into log4cats — so progress bars pointed at stderr are
   * the only way to surface this through the interface API.
   *
   * Those bars emit ANSI cursor control unconditionally, which corrupts the stream when stderr is
   * a pipe rather than a terminal, so they are attached only for an interactive run. Piped runs
   * still get the URLs coursier lists in its failure message.
   */
  def cache(logger: Logger[IO]): Cache =
    val base = Cache.create()
    logger match
      case s: StderrLogger if s.level != LogLevel.Off && System.console() != null =>
        base.withLogger(coursierapi.Logger.progressBars(System.err))
      case _ => base

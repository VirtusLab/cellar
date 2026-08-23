package cellar

import cats.Show
import cats.data.Chain
import cats.effect.{IO, Ref}
import cats.effect.std.Console
import cats.syntax.all.*

/** Captures what an effect writes to the console instead of hitting the real one.
  *
  * The buffers are `Ref`s rather than `StringBuilder`s so that reads are themselves effects:
  * a caller cannot observe a buffer before the effect under test has run and silently assert
  * against an empty string. It also keeps writes consistent if a handler ever fans out.
  */
final class CapturingConsole private (
    outRef: Ref[IO, Chain[String]],
    errRef: Ref[IO, Chain[String]]
) extends Console[IO]:

  val out: IO[String] = outRef.get.map(_.toList.mkString)
  val err: IO[String] = errRef.get.map(_.toList.mkString)

  // The real Console raises on end-of-input. Returning "" instead would spin any
  // read-until-valid loop forever — see TelemetrySubcommand's prompt.
  def readLineWithCharset(charset: java.nio.charset.Charset): IO[String] =
    IO.raiseError(new java.io.EOFException)

  def print[A](a: A)(using fmt: Show[A]): IO[Unit]   = outRef.update(_ :+ fmt.show(a))
  def println[A](a: A)(using fmt: Show[A]): IO[Unit] = outRef.update(_ :+ s"${fmt.show(a)}\n")
  def error[A](a: A)(using fmt: Show[A]): IO[Unit]   = errRef.update(_ :+ fmt.show(a))
  def errorln[A](a: A)(using fmt: Show[A]): IO[Unit] = errRef.update(_ :+ s"${fmt.show(a)}\n")

object CapturingConsole:

  def make: IO[CapturingConsole] =
    (IO.ref(Chain.empty[String]), IO.ref(Chain.empty[String])).mapN(new CapturingConsole(_, _))

  /** Runs `f` against a fresh capturing console, pairing its result with all it wrote. */
  def capture[A](f: Console[IO] ?=> IO[A]): IO[(A, String, String)] =
    make.flatMap { console =>
      given Console[IO] = console
      for
        a         <- f
        (o, e)    <- (console.out, console.err).tupled
      yield (a, o, e)
    }

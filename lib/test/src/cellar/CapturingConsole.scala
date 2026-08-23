package cellar

import cats.effect.IO
import cats.effect.std.Console

class CapturingConsole extends Console[IO]:
  val outBuf: StringBuilder = new StringBuilder
  val errBuf: StringBuilder = new StringBuilder
  def readLineWithCharset(charset: java.nio.charset.Charset): IO[String] = IO.pure("")
  def print[A](a: A)(using fmt: cats.Show[A]): IO[Unit]                 = IO.unit
  def println[A](a: A)(using fmt: cats.Show[A]): IO[Unit] =
    IO { outBuf.append(fmt.show(a)).append('\n'): Unit }
  def error[A](a: A)(using fmt: cats.Show[A]): IO[Unit] = IO.unit
  def errorln[A](a: A)(using fmt: cats.Show[A]): IO[Unit] =
    IO { errBuf.append(fmt.show(a)).append('\n'): Unit }

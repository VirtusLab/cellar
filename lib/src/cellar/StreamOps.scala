package cellar

import cats.effect.IO
import cats.effect.std.Console
import fs2.Stream

object StreamOps:
  def bounded[A](stream: Stream[IO, A], limit: Int)(using Console[IO]): IO[List[A]] =
    boundedWithFlag(stream, limit).flatMap { (results, truncated) =>
      (if truncated then
         Console[IO].errorln(s"Note: results truncated at $limit. Use --limit to increase.")
       else IO.unit).as(results)
    }

  def boundedWithFlag[A](stream: Stream[IO, A], limit: Int): IO[(results: List[A], truncated: Boolean)] =
    stream.take(limit.toLong + 1L).compile.toList.map { all =>
      val truncated = all.length > limit
      (all.take(limit), truncated)
    }

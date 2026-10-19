package cellar.cli

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import fs2.io.file.{Files, Path}
import weaver.SimpleIOSuite

import java.time.Instant
import scala.concurrent.duration.*

object UpdateCheckTest extends SimpleIOSuite:

  import UpdateCheck.*

  private val tempCache = Files[IO].tempDirectory.map(_ / "update-check")

  /** A fetch returning `result` that counts how often it ran. */
  private def countingFetch(result: IO[Option[String]]): IO[(Ref[IO, Int], IO[Option[String]])] =
    Ref.of[IO, Int](0).map(calls => (calls, calls.update(_ + 1) *> result))

  private def writeCheckedAt(cache: Path, at: Instant, latest: String): IO[Unit] =
    fs2.Stream.emit(renderCache(Cached(at, Some(latest)))).through(Files[IO].writeUtf8(cache)).compile.drain

  private def staleCache(cache: Path, latest: String): IO[Unit] =
    IO.realTimeInstant.flatMap(now => writeCheckedAt(cache, now.minusSeconds(25.hours.toSeconds), latest))

  pureTest("versionFromLocation extracts the tag from the releases/latest redirect"):
    expect.all(
      versionFromLocation("https://github.com/VirtusLab/cellar/releases/tag/v0.5.0") == Some("0.5.0"),
      versionFromLocation("https://github.com/VirtusLab/cellar/releases/tag/0.1.0-M14") == Some("0.1.0-M14"),
      versionFromLocation("https://github.com/VirtusLab/cellar/releases").isEmpty
    )

  pureTest("isNewer orders milestones before their final release"):
    expect.all(
      isNewer("0.1.0-M13", "0.1.0-M14"),
      isNewer("0.1.0-M13", "0.1.0"),
      isNewer("0.4.0", "0.10.0"),
      !isNewer("0.5.0", "0.5.0"),
      !isNewer("0.5.0", "0.4.0")
    )

  pureTest("updateCommand matches the install channel"):
    expect.all(
      updateCommand(Some("/nix/store/abc-cellar/bin/cellar")) == "nix profile upgrade cellar",
      updateCommand(Some("/home/u/.local/share/coursier/bin/cellar")) == "cs update cellar",
      updateCommand(Some("/Users/u/Library/Application Support/Coursier/bin/cellar")) == "cs update cellar",
      updateCommand(Some("/usr/local/bin/cellar")) == s"download it from $releasesPage",
      updateCommand(None) == s"download it from $releasesPage"
    )

  pureTest("cache round-trips, including a failed lookup"):
    val at = Instant.ofEpochMilli(1_700_000_000_000L)
    expect.all(
      parseCache(renderCache(Cached(at, Some("0.5.0")))) == Some(Cached(at, Some("0.5.0"))),
      parseCache(renderCache(Cached(at, None))) == Some(Cached(at, None)),
      parseCache("garbage").isEmpty
    )

  pureTest("cache goes stale after a day"):
    val at = Instant.parse("2026-01-01T00:00:00Z")
    expect.all(
      !isStale(Cached(at, None), Instant.parse("2026-01-01T23:00:00Z")),
      isStale(Cached(at, None), Instant.parse("2026-01-02T01:00:00Z"))
    )

  test("repeated runs within a day look up the latest release once"):
    tempCache.use { cache =>
      for
        (calls, fetch) <- countingFetch(IO.pure(Some("0.5.0")))
        results        <- latestRelease(cache, fetch).replicateA(3)
        n              <- calls.get
      yield expect.all(results == List.fill(3)(Some("0.5.0")), n == 1)
    }

  test("a cache older than a day triggers a new lookup"):
    tempCache.use { cache =>
      for
        _              <- staleCache(cache, "0.4.0")
        (calls, fetch) <- countingFetch(IO.pure(Some("0.5.0")))
        latest         <- latestRelease(cache, fetch)
        n              <- calls.get
      yield expect.all(latest == Some("0.5.0"), n == 1)
    }

  test("a failed lookup keeps the previous answer and is not retried within a day"):
    tempCache.use { cache =>
      for
        _              <- staleCache(cache, "0.4.0")
        (calls, fetch) <- countingFetch(IO.raiseError(new java.io.IOException("offline")))
        results        <- latestRelease(cache, fetch).replicateA(2)
        n              <- calls.get
      yield expect.all(results == List.fill(2)(Some("0.4.0")), n == 1)
    }

  test("a hanging lookup times out and is not retried within a day"):
    tempCache.use { cache =>
      for
        (calls, fetch) <- countingFetch(IO.never)
        first          <- latestRelease(cache, fetch).timeout(5.seconds)
        second         <- latestRelease(cache, fetch)
        n              <- calls.get
      yield expect.all(first.isEmpty, second.isEmpty, n == 1)
    }

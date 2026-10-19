package cellar.cli

import cats.effect.IO
import fs2.io.file.Files
import weaver.SimpleIOSuite

object SeenMarkerTest extends SimpleIOSuite:

  import TelemetrySubcommand.touchMarker

  private val tempDir = Files[IO].tempDirectory

  test("creates the marker and its parent directory"):
    tempDir.use { dir =>
      val marker = dir / ".cellar" / ".telemetry-seen"
      (touchMarker(marker) *> Files[IO].exists(marker)).map(expect(_))
    }

  test("leaves an existing marker untouched"):
    tempDir.use { dir =>
      val marker = dir / ".telemetry-seen"
      Files[IO].writeUtf8(marker)(fs2.Stream.emit("keep")).compile.drain *>
        touchMarker(marker) *>
        Files[IO].readUtf8(marker).compile.string.map(expect.eql("keep", _))
    }

  test("does not truncate the target of a symlinked marker"):
    tempDir.use { dir =>
      val target = dir / "victim.txt"
      val marker = dir / ".telemetry-seen"
      Files[IO].writeUtf8(target)(fs2.Stream.emit("precious")).compile.drain *>
        Files[IO].createSymbolicLink(marker, target) *>
        touchMarker(marker) *>
        Files[IO].readUtf8(target).compile.string.map(expect.eql("precious", _))
    }

  test("does not create the target of a dangling symlinked marker"):
    tempDir.use { dir =>
      val target = dir / "planted.txt"
      val marker = dir / ".telemetry-seen"
      Files[IO].createSymbolicLink(marker, target) *>
        touchMarker(marker) *>
        Files[IO].exists(target).map(exists => expect(!exists))
    }

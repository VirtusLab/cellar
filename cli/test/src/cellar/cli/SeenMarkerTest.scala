package cellar.cli

import cats.effect.IO
import fs2.io.file.Files
import munit.CatsEffectSuite

class SeenMarkerTest extends CatsEffectSuite:

  import TelemetrySubcommand.touchMarker

  private val tempDir = ResourceFunFixture(Files[IO].tempDirectory)

  tempDir.test("creates the marker and its parent directory"):
    dir =>
      val marker = dir / ".cellar" / ".telemetry-seen"
      touchMarker(marker) *> Files[IO].exists(marker).assert

  tempDir.test("leaves an existing marker untouched"):
    dir =>
      val marker = dir / ".telemetry-seen"
      Files[IO].writeUtf8(marker)(fs2.Stream.emit("keep")).compile.drain *>
        touchMarker(marker) *>
        Files[IO].readUtf8(marker).compile.string.assertEquals("keep")

  tempDir.test("does not truncate the target of a symlinked marker"):
    dir =>
      val target = dir / "victim.txt"
      val marker = dir / ".telemetry-seen"
      Files[IO].writeUtf8(target)(fs2.Stream.emit("precious")).compile.drain *>
        Files[IO].createSymbolicLink(marker, target) *>
        touchMarker(marker) *>
        Files[IO].readUtf8(target).compile.string.assertEquals("precious")

  tempDir.test("does not create the target of a dangling symlinked marker"):
    dir =>
      val target = dir / "planted.txt"
      val marker = dir / ".telemetry-seen"
      Files[IO].createSymbolicLink(marker, target) *>
        touchMarker(marker) *>
        Files[IO].exists(target).assertEquals(false)

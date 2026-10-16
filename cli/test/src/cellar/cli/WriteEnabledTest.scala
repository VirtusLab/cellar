package cellar.cli

import cats.effect.IO
import fs2.io.file.{Files, Path}
import munit.CatsEffectSuite

class WriteEnabledTest extends CatsEffectSuite:

  import TelemetrySubcommand.writeEnabled

  private val tempDir = ResourceFunFixture(Files[IO].tempDirectory)

  private def write(p: Path, s: String) = fs2.Stream.emit(s).through(Files[IO].writeUtf8(p)).compile.drain
  private def read(p: Path)             = Files[IO].readUtf8(p).compile.string

  tempDir.test("keeps comments, includes and other keys while flipping otel.enabled"):
    dir =>
      val conf = dir / "cellar.conf"
      val original =
        """# my repositories
          |repositories = ["https://repo.example"] # corp mirror
          |include "creds.conf"
          |otel {
          |  # off for now
          |  enabled = false
          |}
          |""".stripMargin
      write(conf, original) *> writeEnabled(conf, enabled = true) *>
        read(conf).assertEquals(original.replace("enabled = false", "enabled = true"))

  tempDir.test("creates the file and its directory when absent"):
    dir =>
      val conf = dir / ".cellar" / "cellar.conf"
      writeEnabled(conf, enabled = false) *>
        read(conf).map(com.typesafe.config.ConfigFactory.parseString(_).getBoolean("otel.enabled"))
          .assertEquals(false)

  tempDir.test("updates the file behind a symlink and keeps the link"):
    dir =>
      val real = dir / "dotfiles.conf"
      val conf = dir / "cellar.conf"
      write(real, "# mine\notel.enabled = true\n") *>
        Files[IO].createSymbolicLink(conf, real) *>
        writeEnabled(conf, enabled = false) *>
        Files[IO].isSymbolicLink(conf).assert *>
        read(real).assertEquals("# mine\notel.enabled = false\n")

  tempDir.test("leaves no temp files behind"):
    dir =>
      val conf = dir / "cellar.conf"
      writeEnabled(conf, enabled = true) *>
        Files[IO].list(dir).map(_.fileName.toString).compile.toList.assertEquals(List("cellar.conf"))

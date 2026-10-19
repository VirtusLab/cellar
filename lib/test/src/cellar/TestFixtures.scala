package cellar

import cats.effect.IO
import coursier.{MavenRepository, Repository}
import weaver.Expectations.Helpers.ignore

/** Test fixture coordinates and local-m2 repository accessor.
  *
  * System properties are injected by the Mill `forkArgs` in build.mill.
  * Run `./mill publishFixtures` before executing integration tests.
  */
object TestFixtures:
  private def require(key: String): String =
    val v = System.getProperty(key)
    assert(v != null, s"System property '$key' is not set. Run './mill publishFixtures' first.")
    v

  lazy val localM2: String = require("cellar.test.localM2")

  lazy val localM2Repo: Repository =
    MavenRepository(s"file://$localM2")

  lazy val javaCoord: MavenCoordinate = MavenCoordinate(
    group    = require("cellar.test.fixtureJavaGroup"),
    artifact = require("cellar.test.fixtureJavaArtifact"),
    version  = require("cellar.test.fixtureJavaVersion")
  )

  lazy val scala2Coord: MavenCoordinate = MavenCoordinate(
    group    = require("cellar.test.fixtureScala2Group"),
    artifact = require("cellar.test.fixtureScala2Artifact"),
    version  = require("cellar.test.fixtureScala2Version")
  )

  lazy val scala3Coord: MavenCoordinate = MavenCoordinate(
    group    = require("cellar.test.fixtureScala3Group"),
    artifact = require("cellar.test.fixtureScala3Artifact"),
    version  = require("cellar.test.fixtureScala3Version")
  )

  /** Skip a test if fixture system properties are not configured. */
  val assumeFixturesAvailable: IO[Unit] =
    IO.whenA(System.getProperty("cellar.test.localM2") == null)(
      ignore[IO]("Fixture system properties not set — run './mill publishFixtures' first")
    )

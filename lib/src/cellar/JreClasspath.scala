package cellar

import cats.effect.IO
import cats.syntax.monadError.*
import java.net.{URI, URLClassLoader}
import java.nio.file.{Files, FileSystems, Path}
import scala.jdk.CollectionConverters.*

object JreClasspath:
  /** Returns a path for each module in the running JVM's JRT filesystem.
    *
    * Tasty-query requires individual module paths (e.g. `modules/java.base`),
    * not the JRT root `/`, so that relative class paths resolve correctly.
    */
  def jrtPath(): IO[Seq[Path]] =
    sys.env.get("JAVA_HOME").map(h => jrtPath(Path.of(h))).getOrElse {
      IO.blocking {
        val fs = FileSystems.getFileSystem(URI.create("jrt:/"))
        Files.list(fs.getPath("modules")).iterator().asScala.toSeq
      }.adaptError { case _ =>
        new RuntimeException(
          "Could not locate JRE classpath. Set JAVA_HOME or pass --java-home pointing to a JDK installation."
        )
      }
    }

  def jrtPath(javaHome: Path): IO[Seq[Path]] =
    IO.blocking {
      val jrtFsJar = javaHome.resolve("lib/jrt-fs.jar")
      if !Files.exists(jrtFsJar) then
        throw new IllegalArgumentException(
          s"Not a valid JDK home (missing lib/jrt-fs.jar): $javaHome"
        )
      val env         = java.util.Map.of("java.home", javaHome.toString)
      val classLoader = new URLClassLoader(Array(jrtFsJar.toUri.toURL))
      val fs          = FileSystems.newFileSystem(URI.create("jrt:/"), env, classLoader)
      Files.list(fs.getPath("modules")).iterator().asScala.toSeq
    }

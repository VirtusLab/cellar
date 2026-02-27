package cellar

import cats.effect.IO
import cats.syntax.monadError.*
import java.net.{URI, URLClassLoader}
import java.nio.file.{Files, FileSystems, Path}
import scala.jdk.CollectionConverters.*
import tastyquery.Classpaths
import tastyquery.jdk.ClasspathLoaders

object JreClasspath:
  def jrtPath(): IO[Classpaths.Classpath] =
    sys.env.get("JAVA_HOME").map(h => jrtPath(Path.of(h))).getOrElse {
      IO.blocking {
        val fs    = FileSystems.getFileSystem(URI.create("jrt:/"))
        val paths = Files.list(fs.getPath("modules")).iterator().asScala.toList
        ClasspathLoaders.read(paths)
      }.adaptError { case _ =>
        new RuntimeException(
          "Could not locate JRE classpath. Set JAVA_HOME or pass --java-home pointing to a JDK installation."
        )
      }
    }

  def jrtPath(javaHome: Path): IO[Classpaths.Classpath] =
    IO.blocking {
      val jrtFsJar = javaHome.resolve("lib/jrt-fs.jar")
      if !Files.exists(jrtFsJar) then
        throw new IllegalArgumentException(
          s"Not a valid JDK home (missing lib/jrt-fs.jar): $javaHome"
        )
      val env = java.util.Map.of("java.home", javaHome.toString)
      val fs =
        try
          // Bypasses GraalVM's FileSystems substitution in native image
          val cls      = Class.forName("jdk.internal.jrtfs.JrtFileSystemProvider")
          val ctor     = cls.getDeclaredConstructor()
          ctor.setAccessible(true)
          val provider = ctor.newInstance().asInstanceOf[java.nio.file.spi.FileSystemProvider]
          provider.newFileSystem(URI.create("jrt:/"), env)
        catch case _: java.lang.reflect.InaccessibleObjectException =>
          // JVM without --add-opens: fall back to URLClassLoader
          val cl = new URLClassLoader(Array(jrtFsJar.toUri.toURL))
          FileSystems.newFileSystem(URI.create("jrt:/"), env, cl)
      ClasspathLoaders.read(Files.list(fs.getPath("modules")).iterator().asScala.toList)
    }

package cellar

import cats.effect.IO
import java.io.ByteArrayInputStream
import java.net.{URI, URLClassLoader}
import java.nio.file.{Files, FileSystems, Path}
import java.util.zip.ZipInputStream
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import tastyquery.Classpaths
import tastyquery.Classpaths.InMemory
import tastyquery.jdk.ClasspathLoaders

object JreClasspath:
  // True when running as a GraalVM native image binary
  private val isNativeImage: Boolean =
    sys.props.get("org.graalvm.nativeimage.imagecode").contains("runtime")

  def jrtPath(): IO[Classpaths.Classpath] =
    if isNativeImage then loadBundledJre()
    else
      sys.env.get("JAVA_HOME") match
        case Some(h) => jrtPath(Path.of(h))
        case None =>
          IO.raiseError(new RuntimeException(
            "Could not locate JRE classpath. Set JAVA_HOME or pass --java-home pointing to a JDK installation."
          ))

  def jrtPath(javaHome: Path): IO[Classpaths.Classpath] =
    // JRT filesystem is not reliably accessible in GraalVM native image:
    // https://github.com/oracle/graal/issues/10013
    if isNativeImage then loadBundledJre()
    else
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

  private def loadBundledJre(): IO[Classpaths.Classpath] =
    IO.blocking {
      val stream = Thread.currentThread().getContextClassLoader.getResourceAsStream("jre.bin")
      if stream == null then
        throw new RuntimeException(
          "Bundled JRE not found. This is a build error — please report it."
        )
      try parseJarToClasspath(stream.readAllBytes())
      finally stream.close()
    }

  private def parseJarToClasspath(jarBytes: Array[Byte]): Classpaths.Classpath =
    val pkgMap = mutable.LinkedHashMap[String, mutable.LinkedHashMap[String, (Option[IArray[Byte]], Option[IArray[Byte]])]]()

    val zis = new ZipInputStream(new ByteArrayInputStream(jarBytes))
    var entry = zis.getNextEntry()
    while entry != null do
      val name = entry.getName
      if !entry.isDirectory then
        val isClass = name.endsWith(".class")
        val isTasty = name.endsWith(".tasty")
        if isClass || isTasty then
          val bytes     = IArray.unsafeFromArray(zis.readAllBytes())
          val base      = name.stripSuffix(".class").stripSuffix(".tasty")
          val lastSlash = base.lastIndexOf('/')
          val (pkg, simpleName) =
            if lastSlash < 0 then ("", base)
            else (base.substring(0, lastSlash).replace('/', '.'), base.substring(lastSlash + 1))
          if simpleName != "module-info" then
            val classMap                       = pkgMap.getOrElseUpdate(pkg, mutable.LinkedHashMap())
            val (existingClass, existingTasty) = classMap.getOrElse(simpleName, (None, None))
            classMap(simpleName) =
              if isClass then (Some(bytes), existingTasty)
              else (existingClass, Some(bytes))
      entry = zis.getNextEntry()
    zis.close()

    val packageDatas = pkgMap.toList.map { (pkgName, classMap) =>
      val classDatas = classMap.toList.map { case (simpleName, (classBytes, tastyBytes)) =>
        val debug = if pkgName.isEmpty then simpleName else s"$pkgName.$simpleName"
        InMemory.ClassData(debug, simpleName, tastyBytes, classBytes)
      }
      InMemory.PackageData(pkgName, pkgName, classDatas)
    }
    List(InMemory.ClasspathEntry("bundled-jre", packageDatas))

package cellar

import coursier.cache.FileCache
import coursier.util.Artifact
import coursier.{Classifier, Fetch, Repository}
import fs2.io.file.Path

import java.util.Properties
import java.util.zip.ZipFile
import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.control.NonFatal

/** Finds the `-sources.jar` published with a jar the build tool handed us, for classpaths that
  * were not resolved with the `sources` classifier. Answers `None` when there is none, offline, or
  * for a jar that is not a published artifact (a project's own output).
  */
object SourcesResolver:
  def forJar(jar: Path, repositories: Seq[Repository]): Option[Path] =
    try sibling(jar).orElse(fromCoursierCache(jar)).orElse(fromPomProperties(jar, repositories))
    catch case NonFatal(_) => None

  private val SourcesSuffix = "-sources.jar"

  private def sourcesName(jar: Path): String = jar.fileName.toString.stripSuffix(".jar") + SourcesSuffix

  // Maven, Ivy and coursier caches keep the sources next to the jar once anything fetched them.
  private def sibling(jar: Path): Option[Path] =
    jar.parent.map(_ / sourcesName(jar)).filter(_.toNioPath.toFile.isFile)

  /** A coursier cache path mirrors its URL (`<cache>/https/host/path/a-1.0.jar`), so the sources
    * jar is the same URL with the `-sources.jar` suffix, whichever repository served it.
    */
  private def fromCoursierCache(jar: Path): Option[Path] =
    val cache    = FileCache()
    val root     = Path.fromNioPath(cache.location.toPath)
    val relative = root.toNioPath.relativize(jar.toNioPath)
    Option.when(jar.toNioPath.startsWith(root.toNioPath) && relative.getNameCount > 2) {
      val protocol = relative.getName(0).toString
      val rest     = relative.subpath(1, relative.getNameCount).toString.replace('\\', '/')
      s"$protocol://${rest.stripSuffix(jar.fileName.toString)}${sourcesName(jar)}"
    }.flatMap { url =>
      cache.file(Artifact(url)).run.unsafeRun()(using cache.ec).toOption.map(f => Path.fromNioPath(f.toPath))
    }

  // Maven-built jars name their own coordinate; the one matching the file name is the jar's own,
  // the others are shaded dependencies.
  private def fromPomProperties(jar: Path, repositories: Seq[Repository]): Option[Path] =
    coordinate(jar).flatMap { (group, artifact, version) =>
      val dependency = MavenCoordinate(group, artifact, version).toCoursierDependency.copy(transitive = false)
      Fetch()
        .addDependencies(dependency)
        .addRepositories(repositories*)
        .addClassifiers(Classifier.sources)
        .withMainArtifacts(false)
        .run()
        .map(f => Path.fromNioPath(f.toPath))
        .find(_.fileName.toString.endsWith(SourcesSuffix))
    }

  private def coordinate(jar: Path): Option[(String, String, String)] =
    Using.resource(ZipFile(jar.toNioPath.toFile)) { zip =>
      zip.entries().asScala
        .filter(e => e.getName.startsWith("META-INF/maven/") && e.getName.endsWith("/pom.properties"))
        .map { e =>
          val props = Properties()
          Using.resource(zip.getInputStream(e))(props.load)
          (props.getProperty("groupId"), props.getProperty("artifactId"), props.getProperty("version"))
        }
        .find { (group, artifact, version) =>
          group != null && artifact != null && version != null &&
          jar.fileName.toString == s"$artifact-$version.jar"
        }
    }

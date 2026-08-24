package cellar

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import coursierapi.{Cache, Fetch}
import fs2.io.file.{Files, Path}
import org.typelevel.log4cats.Logger

import java.util.zip.ZipFile
import scala.jdk.CollectionConverters.*
import scala.quoted.*
import scala.tasty.inspector.*

object DocstringExtractor:
  private def isStdlib(name: String): Boolean =
    name.startsWith("scala3-library") || name.startsWith("scala-library")

  /** Fetches the scala stdlib jars matching the compiler's own version via coursier. */
  private lazy val compilerStdlibJars: Seq[Path] =
    // The stdlib's own version, not `dotty.tools.dotc.config.Properties`: that reads
    // `compiler.properties` out of the compiler jar, which native-image does not embed, so it
    // yields "" and the fetch below asks for `scala3-library_3:` with no version at all.
    // Requires the Scala 3 stdlib (3.8+), where `library.properties` carries a 3.x version.
    val scalaVersion = scala.util.Properties.versionNumberString
    val deps = Seq(
      coursierapi.Dependency.of("org.scala-lang", "scala3-library_3", scalaVersion)
    )
    val fetch = Fetch.create().withCache(Cache.create())
    deps.foreach(fetch.addDependencies(_))
    fetch.fetch().asScala.toSeq.map(file => Path.fromNioPath(file.toPath))

  def extract(jars: Seq[Path], coord: MavenCoordinate, fqn: String)(using logger: Logger[IO]): IO[Option[String]] =
    findPrimaryJar(jars, coord) match
      case None =>
        logger.debug(s"no primary jar for ${coord.artifact} among ${jars.size} fetched jars").as(None)
      case Some(primaryJar) =>
        bundledJre.use { jre =>
          candidateTastyEntries(fqn).collectFirstSomeM(extractAndInspect(primaryJar, _, jars, fqn, jre))
        }

  /** The JDK classes dotc needs, for native-image only.
    *
    * There the compiler has no `jrt:/` to read `java.lang.*` from (see [[JreClasspath]]), and a
    * TASTy run without them dies on `asTerm called on not-a-Term val <none>` while building
    * `Definitions`. The image already carries the build JDK's classes as the `jre.bin` resource —
    * itself a jar — so writing it out gives dotc an ordinary classpath entry.
    *
    * `memoizedAcquire` keeps that write off the artifacts that never reach the compiler — a Java
    * jar has no `.tasty` entry to inspect — while still writing at most once per lookup, and a
    * failure degrades to "no docstring" rather than failing the command around it.
    */
  private def bundledJre(using logger: Logger[IO]): Resource[IO, IO[Seq[Path]]] =
    if !JreClasspath.isNativeImage then Resource.pure(IO.pure(Nil))
    else
      Files[IO].tempFile(None, "cellar-jre-", ".jar", None)
        .evalMap(unpackBundledJre)
        .handleErrorWith { (t: Throwable) =>
          Resource.eval(logger.warn(t)("could not unpack the bundled JRE; no docstrings").as(Seq.empty[Path]))
        }
        .memoizedAcquire

  private def unpackBundledJre(dest: Path)(using logger: Logger[IO]): IO[Seq[Path]] =
    IO.blocking(Option(Thread.currentThread().getContextClassLoader).flatMap(cl => Option(cl.getResourceAsStream("jre.bin")))).flatMap {
      case None =>
        logger.warn("bundled JRE resource missing; no docstrings").as(Seq.empty[Path])
      case Some(stream) =>
        fs2.io.readInputStream(IO.pure(stream), 64 * 1024)
          .through(Files[IO].writeAll(dest))
          .compile.drain
          .as(Seq(dest))
    }

  /** Returns the .tasty zip entry names to try, most specific first. */
  private def candidateTastyEntries(fqn: String): List[String] =
    val direct = fqn.replace('.', '/') + ".tasty"
    val lastDot = fqn.lastIndexOf('.')
    if lastDot <= 0 then List(direct)
    else List(direct, fqn.substring(0, lastDot).replace('.', '/') + ".tasty")

  /** The outcome of one inspector run, kept so the (blocking, non-IO) inspector callback can report
    * what happened without logging from inside the compiler run.
    */
  private case class InspectionOutcome(callbackFired: Boolean, classpathSize: Int, docstring: Option[String])

  private def extractAndInspect(jar: Path, tastyEntry: String, allJars: Seq[Path], fqn: String, jre: IO[Seq[Path]])(using
      logger: Logger[IO]
  ): IO[Option[String]] =
    Files[IO].tempFile(None, "cellar-", ".tasty", None).use { tmp =>
      copyTastyEntry(jar, tastyEntry, tmp).flatMap {
        case false =>
          logger.debug(s"$tastyEntry not present in ${jar.fileName}").as(None)
        case true =>
          jre.flatMap(runInspector(tmp, tastyEntry, allJars, fqn, _))
      }
    }

  private def runInspector(tmp: Path, tastyEntry: String, allJars: Seq[Path], fqn: String, jreJars: Seq[Path])(using
      logger: Logger[IO]
  ): IO[Option[String]] =
    // The failure this diagnostic exists for — a compiler that cannot start under native-image
    // — arrives as a LinkageError, which is not NonFatal. cats-effect routes those to
    // `onFatalFailure` and tears down the runtime without ever reaching `.attempt`, so the
    // catch has to live inside the blocking thunk to keep the error reportable.
    val run = IO.blocking[Either[Throwable, InspectionOutcome]] {
      try
        var docstring: Option[String] = None
        var fired                     = false
        val cp = allJars.filterNot(p => isStdlib(p.fileName.toString)) ++ compilerStdlibJars ++ jreJars
        // `Driver.doCompile` reports a failed run with a bare `println`, so a compiler
        // problem would otherwise land in the middle of the Markdown cellar writes to stdout.
        val _ = Console.withOut(Console.err) {
          TastyInspector.inspectAllTastyFiles(
            List(tmp.toString),
            Nil,
            cp.map(_.toString).toList
          )(new Inspector:
            def inspect(using q: Quotes)(tastys: List[Tasty[q.type]]): Unit =
              fired = true
              docstring = lookupDocstring(fqn)(using q)
          )
        }
        Right(InspectionOutcome(fired, cp.size, docstring))
      catch case t: Throwable => Left(t)
    }
    logger.debug(s"inspecting $tastyEntry for $fqn") *> run.flatMap {
      case Left(t) =>
        logger.warn(t)(s"TASTy inspection of $tastyEntry failed; no docstring for $fqn").as(None)
      case Right(InspectionOutcome(false, cpSize, _)) =>
        logger.warn(s"TASTy inspector never ran for $tastyEntry ($cpSize classpath entries)").as(None)
      case Right(InspectionOutcome(true, cpSize, None)) =>
        logger.debug(s"inspected $tastyEntry ($cpSize classpath entries), no docstring for $fqn").as(None)
      case Right(InspectionOutcome(true, _, found)) =>
        IO.pure(found)
    }

  /** Copies `tastyEntry` out of `jar` into `dest`; false if the jar has no such entry. */
  private def copyTastyEntry(jar: Path, tastyEntry: String, dest: Path): IO[Boolean] =
    Resource.fromAutoCloseable(IO.blocking(new ZipFile(jar.toNioPath.toFile))).use { zip =>
      Option(zip.getEntry(tastyEntry)) match
        case None => IO.pure(false)
        case Some(entry) =>
          fs2.io.readInputStream(IO.blocking(zip.getInputStream(entry)), 64 * 1024)
            .through(Files[IO].writeAll(dest))
            .compile.drain.as(true)
    }

  private def lookupDocstring(fqn: String)(using q: Quotes): Option[String] =
    import q.reflect.*
    val direct =
      try Some(Symbol.requiredClass(fqn))
      catch
        case _ =>
          try Some(Symbol.requiredModule(fqn))
          catch case _ => None
    direct.filterNot(_.isNoSymbol).flatMap(_.docstring).orElse(lookupMemberDocstring(fqn))

  private def lookupMemberDocstring(fqn: String)(using q: Quotes): Option[String] =
    import q.reflect.*
    val lastDot = fqn.lastIndexOf('.')
    if lastDot <= 0 then None
    else
      val ownerFqn   = fqn.substring(0, lastDot)
      val memberName = fqn.substring(lastDot + 1)
      val owner =
        try Symbol.requiredClass(ownerFqn)
        catch
          case _ =>
            try Symbol.requiredModule(ownerFqn)
            catch case _ => Symbol.noSymbol
      if owner.isNoSymbol then None
      else
        val methods = owner.methodMember(memberName)
        val field   = owner.fieldMember(memberName)
        val sym     = methods.headOption.getOrElse(field)
        if sym.isNoSymbol then None else sym.docstring

  private def findPrimaryJar(jars: Seq[Path], coord: MavenCoordinate): Option[Path] =
    val expectedName = s"${coord.artifact}-${coord.version}.jar"
    jars.find(_.fileName.toString == expectedName).orElse(jars.headOption)

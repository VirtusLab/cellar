package cellar

import java.nio.file.{Files, Path, StandardCopyOption}
import java.util.zip.ZipFile
import scala.jdk.CollectionConverters.*
import scala.quoted.*
import scala.tasty.inspector.*
import cats.effect.IO
import cats.syntax.all.*
import coursierapi.{Cache, Fetch}
import org.typelevel.log4cats.Logger

object DocstringExtractor:
  private def isStdlib(name: String): Boolean =
    name.startsWith("scala3-library") || name.startsWith("scala-library")

  /** Fetches the scala stdlib jars matching the compiler's own version via coursier. */
  private lazy val compilerStdlibJars: Seq[Path] =
    val scalaVersion = dotty.tools.dotc.config.Properties.versionNumberString
    val deps = Seq(
      coursierapi.Dependency.of("org.scala-lang", "scala3-library_3", scalaVersion)
    )
    val fetch = Fetch.create().withCache(Cache.create())
    deps.foreach(fetch.addDependencies(_))
    fetch.fetch().asScala.toSeq.map(_.toPath)

  def extract(jars: Seq[Path], coord: MavenCoordinate, fqn: String)(using logger: Logger[IO]): IO[Option[String]] =
    findPrimaryJar(jars, coord) match
      case None =>
        logger.debug(s"no primary jar for ${coord.artifact} among ${jars.size} fetched jars").as(None)
      case Some(primaryJar) =>
        candidateTastyEntries(fqn).collectFirstSomeM(extractAndInspect(primaryJar, _, jars, fqn))

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

  private def extractAndInspect(jar: Path, tastyEntry: String, allJars: Seq[Path], fqn: String)(using
      logger: Logger[IO]
  ): IO[Option[String]] =
    IO.blocking(copyTastyEntry(jar, tastyEntry)).flatMap {
      case None =>
        logger.debug(s"$tastyEntry not present in ${jar.getFileName}").as(None)
      case Some(tmp) =>
        val run = IO.blocking {
          var docstring: Option[String] = None
          var fired                     = false
          val cp = allJars.filterNot(p => isStdlib(p.getFileName.toString)) ++ compilerStdlibJars
          TastyInspector.inspectAllTastyFiles(
            List(tmp.toString),
            Nil,
            cp.map(_.toString).toList
          )(new Inspector:
            def inspect(using q: Quotes)(tastys: List[Tasty[q.type]]): Unit =
              fired = true
              docstring = lookupDocstring(fqn)(using q)
          )
          InspectionOutcome(fired, cp.size, docstring)
        }
        // `attempt` catches Throwable, not just Exception: a compiler that cannot start under
        // native-image fails with an Error (NoClassDefFoundError and friends), which the previous
        // `catch case _: Exception` let escape the diagnostic entirely.
        val inspected = logger.debug(s"inspecting $tastyEntry for $fqn") *> run.attempt.flatMap {
          case Left(t) =>
            logger.warn(t)(s"TASTy inspection of $tastyEntry failed; no docstring for $fqn").as(None)
          case Right(InspectionOutcome(false, cpSize, _)) =>
            logger.warn(s"TASTy inspector never ran for $tastyEntry ($cpSize classpath entries)").as(None)
          case Right(InspectionOutcome(true, cpSize, None)) =>
            logger.debug(s"inspected $tastyEntry ($cpSize classpath entries), no docstring for $fqn").as(None)
          case Right(InspectionOutcome(true, _, found)) =>
            IO.pure(found)
        }
        inspected.guarantee(IO.blocking(Files.deleteIfExists(tmp)).void)
    }

  /** Copies `tastyEntry` out of `jar` into a temp file, or `None` if the jar has no such entry. */
  private def copyTastyEntry(jar: Path, tastyEntry: String): Option[Path] =
    val zip = new ZipFile(jar.toFile)
    try
      Option(zip.getEntry(tastyEntry)).map { entry =>
        val tmp = Files.createTempFile("cellar-", ".tasty")
        val in  = zip.getInputStream(entry)
        try Files.copy(in, tmp, StandardCopyOption.REPLACE_EXISTING)
        finally in.close()
        tmp
      }
    finally
      zip.close()

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
    jars.find(_.getFileName.toString == expectedName).orElse(jars.headOption)

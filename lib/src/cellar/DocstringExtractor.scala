package cellar

import java.nio.file.{Files, Path, StandardCopyOption}
import java.util.zip.ZipFile
import scala.quoted.*
import scala.tasty.inspector.*

object DocstringExtractor:
  def extract(jars: Seq[Path], coord: MavenCoordinate, fqn: String): Option[String] =
    findPrimaryJar(jars, coord).flatMap { primaryJar =>
      candidateTastyEntries(fqn).iterator
        .flatMap(entry => extractAndInspect(primaryJar, entry, jars, fqn))
        .nextOption()
    }

  /** Returns the .tasty zip entry names to try, most specific first. */
  private def candidateTastyEntries(fqn: String): List[String] =
    val direct = fqn.replace('.', '/') + ".tasty"
    val lastDot = fqn.lastIndexOf('.')
    if lastDot <= 0 then List(direct)
    else List(direct, fqn.substring(0, lastDot).replace('.', '/') + ".tasty")

  private def extractAndInspect(jar: Path, tastyEntry: String, allJars: Seq[Path], fqn: String): Option[String] =
    val zip = new ZipFile(jar.toFile)
    try
      Option(zip.getEntry(tastyEntry)).flatMap { entry =>
        val tmp = Files.createTempFile("cellar-", ".tasty")
        try
          val in = zip.getInputStream(entry)
          try Files.copy(in, tmp, StandardCopyOption.REPLACE_EXISTING)
          finally in.close()

          var result: Option[String] = None
          try
            TastyInspector.inspectAllTastyFiles(
              List(tmp.toString),
              Nil,
              allJars.map(_.toString).toList
            )(new Inspector:
              def inspect(using q: Quotes)(tastys: List[Tasty[q.type]]): Unit =
                result = lookupDocstring(fqn)(using q)
            )
          catch case _: Exception => ()
          result
        finally
          Files.deleteIfExists(tmp)
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

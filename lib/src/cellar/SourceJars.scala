package cellar

import fs2.io.file.Path
import tastyquery.Classpaths.{Classpath, ClasspathEntry}
import tastyquery.Contexts.Context
import tastyquery.Symbols.{ClassSymbol, PackageSymbol, Symbol, TermOrTypeSymbol, TermSymbol}

import java.util.zip.ZipFile
import scala.reflect.NameTransformer
import scala.util.Using

/** Which `-sources.jar` holds the source of a symbol: the one published next to the classpath
  * entry the symbol was loaded from, which for an inherited or transitive member is not the jar
  * the user named.
  */
final class SourceJars private (byEntry: List[(ClasspathEntry, Path)]):
  // Classpath order, because the first entry defining a class is the one tasty-query loaded it from.
  def forSymbol(sym: Symbol): Option[Path] =
    for
      top <- SourceJars.topLevelOwner(sym)
      pkg <- top.owner match
               case p: PackageSymbol => Some(SourceJars.packageName(p))
               case _                => None
      jar <- byEntry.collectFirst {
               case (entry, jar) if entry.listAllPackages().exists { data =>
                     data.dotSeparatedName == pkg &&
                     data.getClassDataByBinaryName(SourceJars.encode(top.name.toString)).isDefined
                   } => jar
             }
    yield jar

object SourceJars:
  val empty: SourceJars = new SourceJars(Nil)

  /** `ClasspathLoaders.read` yields one entry per path, in order; anything else means the pairing
    * would be a guess, so the caller gets `None` rather than wrong sources.
    */
  def pair(jars: List[Path], entries: Classpath, sourcesJars: Map[Path, Path]): Option[SourceJars] =
    Option.when(jars.length == entries.length) {
      new SourceJars(jars.zip(entries).flatMap((jar, entry) => sourcesJars.get(jar).map(entry -> _)))
    }

  /** The dotted package name as the classpath spells it, `""` for the root and empty packages. */
  private[cellar] def packageName(pkg: PackageSymbol): String =
    val name = pkg.fullName.toString
    if pkg.isRootPackage || name == "<empty>" then "" else name

  /** A symbol name as it appears in a classfile name, `::` as `$colon$colon`. */
  private[cellar] def encode(name: String): String = NameTransformer.encode(name)

  /** The symbol directly owned by a package that `sym` is nested in, or `sym` itself. */
  private[cellar] def topLevelOwner(sym: Symbol): Option[TermOrTypeSymbol] =
    sym match
      case s: TermOrTypeSymbol =>
        s.owner match
          case _: PackageSymbol        => Some(s)
          case owner: TermOrTypeSymbol => topLevelOwner(owner)
      case _ => None

  /** Like [[topLevelOwner]], but an `object` is answered with its module class. */
  private[cellar] def topLevelClass(sym: Symbol)(using Context): Option[ClassSymbol] =
    topLevelOwner(sym).flatMap {
      case cls: ClassSymbol                    => Some(cls)
      case term: TermSymbol if term.isModuleVal => term.moduleClass
      case _                                    => None
    }

  private[cellar] def readText(jar: Path, entry: String): Option[String] =
    Using.resource(ZipFile(jar.toNioPath.toFile)) { zip =>
      Option(zip.getEntry(entry)).map(e => String(zip.getInputStream(e).readAllBytes(), "UTF-8"))
    }

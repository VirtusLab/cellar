package cellar

import fs2.io.file.Path
import tastyquery.Classpaths.{Classpath, ClasspathEntry}
import tastyquery.Contexts.Context
import tastyquery.Symbols.{ClassSymbol, PackageSymbol, Symbol, TermOrTypeSymbol, TermSymbol}

import java.util.zip.ZipFile
import scala.collection.concurrent.TrieMap
import scala.reflect.NameTransformer
import scala.util.Using

/** Which `-sources.jar` holds the source of a symbol: the one published next to the classpath
  * entry the symbol was loaded from, which for an inherited or transitive member is not the jar
  * the user named.
  */
final class SourceJars private (
    entries: List[(ClasspathEntry, Path)],
    paired: Map[Path, Path],
    resolve: Path => Option[Path]
):
  private val resolved = TrieMap.empty[Path, Option[Path]]

  def forSymbol(sym: Symbol): Option[Path] =
    jarFor(sym).flatMap(jar => paired.get(jar).orElse(resolved.getOrElseUpdate(jar, resolve(jar))))

  /** The classpath jar (or directory) the symbol's top-level class was loaded from. */
  def jarFor(sym: Symbol): Option[Path] =
    for
      top <- SourceJars.topLevelOwner(sym)
      pkg <- top.owner match
               case p: PackageSymbol => Some(SourceJars.packageName(p))
               case _                => None
      // Java statics live on a synthetic module class `Foo$` that has no classfile of its own.
      binaries = List(SourceJars.encode(top.name.toString)).flatMap(n => List(n, n.stripSuffix("$"))).distinct
      // Classpath order, because the first entry defining a class is the one tasty-query loaded it from.
      jar <- entries.collectFirst {
               case (entry, jar) if entry.listAllPackages().exists { data =>
                     data.dotSeparatedName == pkg && binaries.exists(data.getClassDataByBinaryName(_).isDefined)
                   } => jar
             }
    yield jar

object SourceJars:
  val empty: SourceJars = new SourceJars(Nil, Map.empty, _ => None)

  /** `ClasspathLoaders.read` yields one entry per path, in order; anything else means the pairing
    * would be a guess, so the caller gets `None` rather than wrong sources. A jar missing from
    * `sourcesJars` is looked up with `resolve` the first time a symbol needs it.
    */
  def pair(
      jars: List[Path],
      entries: Classpath,
      sourcesJars: Map[Path, Path],
      resolve: Path => Option[Path] = _ => None
  ): Option[SourceJars] =
    Option.when(jars.length == entries.length)(new SourceJars(entries.zip(jars), sourcesJars, resolve))

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

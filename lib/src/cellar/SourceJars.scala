package cellar

import fs2.io.file.Path
import tastyquery.Classpaths.{Classpath, ClasspathEntry}
import tastyquery.Symbols.{PackageSymbol, Symbol, TermOrTypeSymbol}

/** Which `-sources.jar` holds the source of a symbol: the one published next to the classpath
  * entry the symbol was loaded from, which for an inherited or transitive member is not the jar
  * the user named.
  */
final class SourceJars private (byEntry: Map[ClasspathEntry, Path]):
  def forSymbol(sym: Symbol): Option[Path] =
    for
      top     <- topLevelClass(sym)
      pkg     <- top.owner match
                   case p: PackageSymbol => Some(p.fullName.toString)
                   case _                => None
      (entry, _) <- byEntry.find { (entry, _) =>
                      entry.listAllPackages().exists { data =>
                        data.dotSeparatedName == pkg && data.getClassDataByBinaryName(top.name.toString).isDefined
                      }
                    }
    yield byEntry(entry)

  private def topLevelClass(sym: Symbol): Option[TermOrTypeSymbol] =
    sym match
      case s: TermOrTypeSymbol =>
        s.owner match
          case _: PackageSymbol        => Some(s)
          case owner: TermOrTypeSymbol => topLevelClass(owner)
      case _ => None

object SourceJars:
  val empty: SourceJars = new SourceJars(Map.empty)

  /** `ClasspathLoaders.read` yields one entry per path, in order; anything else means the pairing
    * would be a guess, so the caller gets `None` rather than wrong sources.
    */
  def pair(jars: List[Path], entries: Classpath, sourcesJars: Map[Path, Path]): Option[SourceJars] =
    Option.when(jars.length == entries.length) {
      new SourceJars(jars.zip(entries).flatMap((jar, entry) => sourcesJars.get(jar).map(entry -> _)).toMap)
    }

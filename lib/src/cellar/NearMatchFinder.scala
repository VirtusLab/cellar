package cellar

import cats.effect.IO
import tastyquery.Classpaths.Classpath
import tastyquery.Contexts.Context

object NearMatchFinder:
  def findNearMatches(fqn: String, classpath: Classpath)(using ctx: Context): IO[List[String]] =
    IO.blocking {
      val simpleName = fqn.lastIndexOf('.') match
        case -1  => fqn
        case idx => fqn.substring(idx + 1)
      val lowerName = simpleName.toLowerCase

      classpath.to(LazyList)
        .flatMap(entry => try ctx.findSymbolsByClasspathEntry(entry).toList catch case _: Exception => Nil)
        .filter(sym => PublicApiFilter.isPublic(sym) && sym.name.toString.toLowerCase == lowerName)
        .map(_.displayFullName)
        .take(10)
        .toList
    }

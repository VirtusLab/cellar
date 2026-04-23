package cellar

import cats.effect.IO
import tastyquery.Classpaths.Classpath
import tastyquery.Contexts.Context

object NearMatchFinder:
  private val log = java.util.logging.Logger.getLogger("cellar.NearMatchFinder")
  def findNearMatches(fqn: String, classpath: Classpath)(using ctx: Context): IO[List[String]] =
    IO.blocking {
      val simpleName = fqn.lastIndexOf('.') match
        case -1  => fqn
        case idx => fqn.substring(idx + 1)
      val lowerName = simpleName.toLowerCase

      classpath.to(LazyList)
        .flatMap(entry => try ctx.findSymbolsByClasspathEntry(entry).toList catch
          case e: Throwable =>
            if log.isLoggable(java.util.logging.Level.FINE) then
              log.log(java.util.logging.Level.FINE, s"Unexpected exception scanning classpath entry: $entry", e)
            Nil
        )
        .filter(sym => PublicApiFilter.isPublic(sym) && sym.name.toString.toLowerCase == lowerName)
        .map(_.displayFullName)
        .filter(_ != fqn)
        .take(10)
        .toList
    }

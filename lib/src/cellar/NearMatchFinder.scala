package cellar

import cats.effect.IO
import cats.syntax.all.*
import org.typelevel.log4cats.Logger
import tastyquery.Classpaths.Classpath
import tastyquery.Contexts.Context

object NearMatchFinder:
  def findNearMatches(fqn: String, classpath: Classpath)(using
      ctx: Context,
      logger: Logger[IO] = StderrLogger.off
  ): IO[List[String]] =
    val scan = IO.blocking {
      val simpleName = fqn.lastIndexOf('.') match
        case -1  => fqn
        case idx => fqn.substring(idx + 1)
      val lowerName = simpleName.toLowerCase

      // Entries that blow up while being scanned are skipped, but collected so the caller can
      // report them — a near-match search that silently skipped half the classpath looks
      // identical to one that genuinely found nothing.
      val skipped = List.newBuilder[(String, Throwable)]
      val matches = classpath.to(LazyList)
        .flatMap(entry =>
          try ctx.findSymbolsByClasspathEntry(entry).toList
          catch
            case e: Throwable =>
              skipped += ((entry.toString, e))
              Nil
        )
        .filter(sym => PublicApiFilter.isPublic(sym) && sym.name.toString.toLowerCase == lowerName)
        .map(_.displayFullName)
        .filter(_ != fqn)
        .take(10)
        .toList
      (matches, skipped.result())
    }

    scan.flatMap { (matches, skipped) =>
      // Unreadable entries are routine — the JRE alone contributes dozens every run — so only the
      // count is worth verbose, and the per-entry detail is kept for debug.
      val skippedSummary =
        if skipped.isEmpty then IO.unit
        else logger.info(s"near-match scan skipped ${skipped.size} unreadable classpath entries")
      logger.debug(s"near-match scan for '$fqn': ${matches.size} candidate(s)") *>
        skippedSummary *>
        skipped.traverse_((entry, e) => logger.debug(s"  skipped $entry: ${e.getClass.getName}: ${e.getMessage}")) *>
        IO.pure(matches)
    }

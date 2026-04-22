package cellar

import cats.effect.IO
import fs2.Stream
import tastyquery.Classpaths.{Classpath, ClasspathEntry}
import tastyquery.Contexts.Context
import tastyquery.Symbols.TermOrTypeSymbol

object AllSymbolsStream:
  private val log = java.util.logging.Logger.getLogger("cellar.AllSymbolsStream")
  /** Streams all public symbols from the classpath, excluding the given JRE entries. */
  def stream(classpath: Classpath, jreClasspath: Classpath)(using ctx: Context): Stream[IO, TermOrTypeSymbol] =
    val jreEntries = jreClasspath.toSet
    val libEntries: List[ClasspathEntry] = classpath.filterNot(jreEntries.contains)
    Stream
      .emits(libEntries)
      .flatMap { entry =>
        Stream
          .eval(IO.blocking {
            try ctx.findSymbolsByClasspathEntry(entry).toList
            catch
              case e: Throwable =>
                if log.isLoggable(java.util.logging.Level.FINE) then
                  log.fine(s"Unexpected exception scanning classpath entry: ${e.getClass.getName}: ${e.getMessage}")
                Nil
          })
          .flatMap(syms => Stream.emits(syms))
      }
      .evalFilter(sym => IO.blocking(PublicApiFilter.isPublic(sym)))

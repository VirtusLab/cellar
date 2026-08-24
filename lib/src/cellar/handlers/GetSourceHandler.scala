package cellar.handlers

import cats.effect.std.Console
import cats.effect.{ExitCode, IO}
import cellar.*
import coursierapi.Repository
import fs2.io.file.Path
import org.typelevel.log4cats.Logger
import org.typelevel.otel4s.trace.Tracer
import tastyquery.Contexts.Context
import tastyquery.SourceLanguage
import tastyquery.Symbols.{ClassSymbol, Symbol, TermOrTypeSymbol}
import tastyquery.Trees.Tree

object GetSourceHandler:
  private type SourceRef = (filePath: String, startLine: Int, endLine: Int, language: String)
  def run(
      coord: MavenCoordinate,
      fqn: String,
      javaHome: Option[Path] = None,
      extraRepositories: Seq[Repository] = Seq.empty,
      logger: Logger[IO] = StderrLogger.off
  )(using Console[IO], Tracer[IO]): IO[ExitCode] =
    given Logger[IO] = logger
    val program =
      for
        jreClasspath <- javaHome.fold(JreClasspath.jrtPath())(JreClasspath.jrtPath)
        result   <- ContextResource.makeFromCoord(coord, jreClasspath, extraRepositories).use { (ctx, classpath) =>
          given Context = ctx
          SymbolResolver.resolve(fqn).flatMap {
            case LookupResult.IsPackage =>
              Console[IO].errorln(s"'$fqn' is a package, not a symbol.").as(ExitCode.Error)
            case LookupResult.NotFound =>
              NearMatchFinder.findNearMatches(fqn, classpath).flatMap { nearMatches =>
                IO.raiseError(CellarError.SymbolNotFound(fqn, coord, nearMatches))
              }
            case LookupResult.PartialMatch(resolvedFqn, missingMember) =>
              IO.raiseError(CellarError.PartialResolution(fqn, Some(coord), resolvedFqn, missingMember))
            case LookupResult.LookupFailed(cause) =>
              IO.raiseError(CellarError.SymbolLookupFailed(fqn, cause))
            case LookupResult.Found(symbols) =>
              IO.blocking(combinedSourceRef(symbols.head)(using ctx)).flatMap {
                case None =>
                  Console[IO].errorln(
                    s"No source position for '$fqn'."
                  ).as(ExitCode.Error)
                case Some(ref) =>
                  SourceFetcher.fetch(coord, ref.filePath, ref.startLine, ref.endLine, extraRepositories).flatMap {
                    case Left(err) =>
                      Console[IO].errorln(err).as(ExitCode.Error)
                    case Right(result) =>
                      val lineInfo = if ref.endLine == Int.MaxValue then "" else s" lines ${ref.startLine + 1}–${ref.endLine + 1}"
                      val header = s"// ${result.entryPath}$lineInfo"
                      Console[IO].println(s"```${ref.language}\n$header\n${result.lines.mkString("\n")}\n```")
                        .as(ExitCode.Success)
                  }
              }
          }
        }
      yield result

    program

  /**
   * Resolve the source range for `sym`. When `sym` is a ClassSymbol whose
   * companion lives in the same source file, widen the range to cover both —
   * so `get-source cats.Monad` returns the trait *and* `object Monad` in one
   * slice, which is where `apply`, type-class summoners, etc. actually live.
   */
  private def combinedSourceRef(sym: Symbol)(using Context): Option[SourceRef] =
    val primary = sourceRef(sym)
    val companion = sym match
      case cls: ClassSymbol => cls.companionClass.flatMap(sourceRef)
      case _                => None
    (primary, companion) match
      case (Some(p), Some(c)) if p.filePath == c.filePath && p.language == c.language =>
        Some((filePath = p.filePath, startLine = math.min(p.startLine, c.startLine), endLine = math.max(p.endLine, c.endLine), language = p.language))
      case _ => primary

  private def sourceRef(sym: Symbol): Option[SourceRef] =
    sym.tree.flatMap { t =>
      val pos = t.asInstanceOf[Tree].pos
      if pos.isUnknown || pos.isSynthetic || pos.sourceFile == tastyquery.SourceFile.NoSource then None
      else Some((filePath = pos.sourceFile.path, startLine = pos.startLine, endLine = pos.endLine, language = "scala"))
    }.orElse {
      sym match
        case s: TermOrTypeSymbol if s.sourceLanguage == SourceLanguage.Java =>
          Some((filePath = wholeFilePath(s, "java"), startLine = 0, endLine = Int.MaxValue, language = "java"))
        case s: TermOrTypeSymbol if s.sourceLanguage == SourceLanguage.Scala2 =>
          Some((filePath = wholeFilePath(s, "scala"), startLine = 0, endLine = Int.MaxValue, language = "scala"))
        case _ => None
    }

  /**
   * Scala 2 pickles and Java classfiles carry no positions, so the best we can do is the
   * whole file, guessed from the top-level name. Scala 2 files often don't match their
   * class name; SourceFetcher falls back to scanning the package directory for that case.
   */
  private def wholeFilePath(sym: TermOrTypeSymbol, ext: String): String =
    def topLevel(s: TermOrTypeSymbol): TermOrTypeSymbol = s.owner match
      case p: TermOrTypeSymbol if !p.isPackage => topLevel(p)
      case _                                   => s
    topLevel(sym).displayFullName.stripSuffix("$").replace('.', '/') + "." + ext

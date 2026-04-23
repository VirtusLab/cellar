package cellar.handlers

import cats.effect.{ExitCode, IO}
import cats.effect.std.Console
import cellar.*
import coursierapi.Repository
import fs2.io.file.Path
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
      extraRepositories: Seq[Repository] = Seq.empty
  )(using Console[IO]): IO[ExitCode] =
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
                    s"No source position for '$fqn'. Only Scala 3 (TASTy) and Java symbols are supported."
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

    program.handleErrorWith { case e: Throwable =>
      Console[IO].errorln(e.getMessage).as(ExitCode.Error)
    }

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
          Some((filePath = javaSourcePath(s), startLine = 0, endLine = Int.MaxValue, language = "java"))
        case _ => None
    }

  private def javaSourcePath(sym: TermOrTypeSymbol): String =
    def topLevel(s: TermOrTypeSymbol): TermOrTypeSymbol = s.owner match
      case p: TermOrTypeSymbol if !p.isPackage => topLevel(p)
      case _                                   => s
    topLevel(sym).displayFullName.replace('.', '/') + ".java"

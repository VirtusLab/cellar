package cellar.handlers

import cats.effect.{ExitCode, IO}
import cats.effect.std.Console
import cellar.*
import coursierapi.Repository
import fs2.io.file.Path
import tastyquery.Contexts.Context
import tastyquery.SourceLanguage
import tastyquery.Symbols.TermOrTypeSymbol
import tastyquery.Trees.Tree

object GetSourceHandler:
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
            case LookupResult.Found(symbols) =>
              IO.blocking(sourceRef(symbols.head)).flatMap {
                case None =>
                  Console[IO].errorln(
                    s"No source position for '$fqn'. Only Scala 3 (TASTy) and Java symbols are supported."
                  ).as(ExitCode.Error)
                case Some((filePath, startLine, endLine, language)) =>
                  SourceFetcher.fetch(coord, filePath, startLine, endLine, extraRepositories).flatMap {
                    case Left(err) =>
                      Console[IO].errorln(err).as(ExitCode.Error)
                    case Right(result) =>
                      val lineInfo = if endLine == Int.MaxValue then "" else s" lines ${startLine + 1}–${endLine + 1}"
                      val header = s"// ${result.entryPath}$lineInfo"
                      Console[IO].println(s"```$language\n$header\n${result.lines.mkString("\n")}\n```")
                        .as(ExitCode.Success)
                  }
              }
          }
        }
      yield result

    program.handleErrorWith {
      case e: CellarError => Console[IO].errorln(e.getMessage).as(ExitCode.Error)
      case e: Throwable   => Console[IO].errorln(e.getMessage).as(ExitCode.Error)
    }

  private def sourceRef(sym: tastyquery.Symbols.Symbol): Option[(String, Int, Int, String)] =
    sym.tree.flatMap { t =>
      val pos = t.asInstanceOf[Tree].pos
      if pos.isUnknown || pos.isSynthetic || pos.sourceFile == tastyquery.SourceFile.NoSource then None
      else Some((pos.sourceFile.path, pos.startLine, pos.endLine, "scala"))
    }.orElse {
      sym match
        case s: TermOrTypeSymbol if s.sourceLanguage == SourceLanguage.Java =>
          Some((javaSourcePath(s), 0, Int.MaxValue, "java"))
        case _ => None
    }

  private def javaSourcePath(sym: TermOrTypeSymbol): String =
    def topLevel(s: TermOrTypeSymbol): TermOrTypeSymbol = s.owner match
      case p: TermOrTypeSymbol if !p.isPackage => topLevel(p)
      case _                                   => s
    topLevel(sym).displayFullName.replace('.', '/') + ".java"

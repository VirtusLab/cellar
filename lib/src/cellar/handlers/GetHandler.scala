package cellar.handlers

import cats.effect.std.Console
import cats.effect.{ExitCode, IO}
import cellar.*
import coursier.Repository
import fs2.io.file.Path
import org.typelevel.log4cats.Logger
import org.typelevel.otel4s.trace.Tracer
import tastyquery.Classpaths.Classpath
import tastyquery.Contexts.Context
import tastyquery.Symbols.Symbol

object GetHandler:
  def run(
      coord: MavenCoordinate,
      fqn: String,
      javaHome: Option[Path] = None,
      extraRepositories: Seq[Repository] = Seq.empty,
      limit: Option[Int] = None,
      hideInherited: Boolean = false,
      groupInherited: Boolean = false,
      logger: Logger[IO] = StderrLogger.off
  )(using Console[IO], Tracer[IO]): IO[ExitCode] =
    given Logger[IO] = logger
    val program =
      for
        jreClasspath <- javaHome.fold(JreClasspath.jrtPath())(JreClasspath.jrtPath)
        result   <- ContextResource.makeFromCoord(coord, jreClasspath, extraRepositories).use { (ctx, classpath, sourceJars) =>
          given Context = ctx
          runCore(fqn, classpath, Some(coord), sourceJars, limit, hideInherited, groupInherited, logger)
        }
      yield result

    program

  def runCore(
      fqn: String,
      classpath: Classpath,
      coord: Option[MavenCoordinate],
      sourceJars: SourceJars,
      limit: Option[Int] = None,
      hideInherited: Boolean = false,
      groupInherited: Boolean = false,
      logger: Logger[IO] = StderrLogger.off
  )(using Context, Console[IO], Tracer[IO]): IO[ExitCode] =
    given Logger[IO] = logger
    SymbolResolver.resolve(fqn).flatMap {
      case LookupResult.Found(symbols) =>
        val jars = classpath.filter(_.toString.endsWith(".jar")).map(e => Path(e.toString)).toSeq
        for
          _         <- warnShadedDuplicate(fqn, classpath)
          docstring <- tastyDoc(fqn, symbols, jars, coord, sourceJars)
                         .map(_.orElse(symbols.headOption.flatMap(sourceDoc)))
          formatted <- IO.blocking(GetFormatter.formatGetResult(fqn, symbols, docstring, limit, hideInherited, groupInherited))
          _         <- Console[IO].println(formatted)
        yield ExitCode.Success
      case LookupResult.IsPackage =>
        Console[IO].errorln(
          s"'$fqn' is a package. Use 'cellar list $fqn' to explore package contents."
        ).as(ExitCode.Error)
      case LookupResult.PartialMatch(resolvedFqn, missingMember) =>
        Console[IO].errorln(CellarError.PartialResolution(fqn, coord, resolvedFqn, missingMember).getMessage).as(ExitCode.Error)
      case LookupResult.NotFound =>
        coord match
          case Some(c) =>
            NearMatchFinder.findNearMatches(fqn, classpath).flatMap { nearMatches =>
              IO.raiseError(CellarError.SymbolNotFound(fqn, c, nearMatches))
            }
          case None =>
            NearMatchFinder.findNearMatches(fqn, classpath).flatMap { nearMatches =>
              val base = s"Symbol '$fqn' not found."
              val msg = if nearMatches.isEmpty then base
                else s"$base Did you mean one of: ${nearMatches.mkString(", ")}?"
              Console[IO].errorln(msg).as(ExitCode.Error)
            }
      case LookupResult.LookupFailed(cause) =>
        IO.raiseError(CellarError.SymbolLookupFailed(fqn, cause))
    }

  /** Warns to stderr if the target FQN exists in more than one JAR on the classpath. */
  private def warnShadedDuplicate(fqn: String, classpath: Classpath)(using Console[IO]): IO[Unit] =
    IO.blocking(findShadedDuplicate(fqn, classpath)).flatMap {
      case Some(err) => Console[IO].errorln(s"Warning: ${err.getMessage}")
      case None      => IO.unit
    }

  private def findShadedDuplicate(fqn: String, classpath: Classpath): Option[CellarError.ShadedDuplicate] =
    val lastDot = fqn.lastIndexOf('.')
    if lastDot <= 0 then return None
    val pkgName    = fqn.substring(0, lastDot)
    val simpleName = fqn.substring(lastDot + 1)
    val jarEntries = classpath.filter(_.toString.endsWith(".jar"))
    val containing = jarEntries.filter { entry =>
      try
        entry
          .listAllPackages()
          .exists(pkg =>
            pkg.dotSeparatedName == pkgName &&
              pkg.listAllClassDatas().exists(_.binaryName == simpleName)
          )
      catch case _: Exception => false
    }
    if containing.size > 1 then
      try
        val first = Path(containing(0).toString)
        val dup   = Path(containing(1).toString)
        Some(CellarError.ShadedDuplicate(fqn, first, dup))
      catch case _: Exception => None
    else None

  // The jar the symbol was loaded from, rather than the coordinate's own jar, so that a project
  // classpath (no coordinate) and transitive symbols get their docstring too.
  private def tastyDoc(
      fqn: String,
      symbols: List[Symbol],
      jars: Seq[Path],
      coord: Option[MavenCoordinate],
      sourceJars: SourceJars
  )(using Logger[IO]): IO[Option[String]] =
    symbols.headOption.flatMap(sourceJars.jarFor).filter(_.fileName.toString.endsWith(".jar")) match
      case Some(jar) => DocstringExtractor.extractFrom(jar, jars, fqn)
      case None      => coord.fold(IO.pure(Option.empty[String]))(DocstringExtractor.extract(jars, _, fqn))

  // Scala 3 docs come from TASTy, which DocstringExtractor has already read.
  private def sourceDoc(sym: Symbol)(using Context): Option[String] =
    TypePrinter.detectLanguage(sym) match
      case DetectedLanguage.Java   => JavaSources.docFor(sym)
      case DetectedLanguage.Scala2 => ScalaSources.docFor(sym)
      case DetectedLanguage.Scala3 => None

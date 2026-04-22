package cellar.handlers

import cats.effect.{ExitCode, IO}
import cats.effect.std.Console
import cellar.*
import coursierapi.Repository
import fs2.io.file.Path
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
      groupInherited: Boolean = false
  )(using Console[IO]): IO[ExitCode] =
    val program =
      for
        jreClasspath <- javaHome.fold(JreClasspath.jrtPath())(JreClasspath.jrtPath)
        result   <- ContextResource.makeFromCoord(coord, jreClasspath, extraRepositories).use { (ctx, classpath) =>
          given Context = ctx
          runCore(fqn, classpath, Some(coord), limit, hideInherited, groupInherited)
        }
      yield result

    program.handleErrorWith { case e: Throwable =>
      Console[IO].errorln(e.getMessage).as(ExitCode.Error)
    }

  def runCore(
      fqn: String,
      classpath: Classpath,
      coord: Option[MavenCoordinate],
      limit: Option[Int] = None,
      hideInherited: Boolean = false,
      groupInherited: Boolean = false
  )(using Context, Console[IO]): IO[ExitCode] =
    SymbolResolver.resolve(fqn).flatMap {
      case LookupResult.Found(symbols) =>
        val jars = classpath.filter(_.toString.endsWith(".jar")).map(e => Path(e.toString)).toSeq
        for
          _         <- warnShadedDuplicate(fqn, classpath)
          docstring <- coord.fold(IO.pure(Option.empty[String]))(c => IO.blocking(DocstringExtractor.extract(jars.map(_.toNioPath), c, fqn)))
          formatted <- IO.blocking(GetFormatter.formatGetResult(fqn, symbols, docstring, limit, hideInherited, groupInherited))
          _         <- Console[IO].println(formatted)
          _         <- warnScala2(symbols)
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

  /** Warns to stderr when any resolved symbol is from a Scala 2 artifact. */
  private def warnScala2(symbols: List[Symbol])(using Console[IO]): IO[Unit] =
    val isScala2 = symbols.exists(s => TypePrinter.detectLanguage(s) == DetectedLanguage.Scala2)
    if isScala2 then
      Console[IO].errorln("Note: Scala 2 artifact — type information may be incomplete.")
    else IO.unit

package cellar

import cats.effect.IO
import cats.syntax.all.*
import org.typelevel.log4cats.Logger
import org.typelevel.otel4s.trace.Tracer
import tastyquery.Contexts.Context
import tastyquery.Exceptions.MemberNotFoundException
import tastyquery.Names.{termName, typeName}
import tastyquery.Symbols.{ClassSymbol, Symbol, TermOrTypeSymbol}

sealed trait LookupResult
object LookupResult:
  final case class Found(symbols: List[Symbol])                              extends LookupResult
  case object IsPackage                                                      extends LookupResult
  case object NotFound                                                       extends LookupResult
  final case class PartialMatch(resolvedFqn: String, missingMember: String)  extends LookupResult
  final case class LookupFailed(cause: Throwable)                            extends LookupResult

/**
 * Records which lookup strategies a resolution attempt tried. The lookups are synchronous, so they
 * cannot write to a `Logger[IO]` as they go; `SymbolResolver.resolve` drains this afterwards and
 * logs it. Without it, a failed lookup is indistinguishable from a symbol that is genuinely absent.
 */
final class ResolutionTrace:
  private val entriesB = List.newBuilder[String]
  def add(msg: => String): Unit    = entriesB += msg
  def entries: List[String]        = entriesB.result()

object SymbolResolver:
  def resolve(
      fqn: String
  )(using ctx: Context, tracer: Tracer[IO], logger: Logger[IO] = StderrLogger.off): IO[LookupResult] =
    tracer.span("symbol.resolve").surround {
      val trace = ResolutionTrace()
      val lookup =
        IO.blocking(tryTopLevel(fqn, trace)).flatMap {
          case Some(result) => IO.pure(result)
          case None         => IO.blocking(tryNestedLookup(fqn, trace)).map(_.getOrElse(LookupResult.NotFound))
        }
      lookup.flatTap(result => logTrace(fqn, result, trace))
    }

  /** A miss is what people actually debug, so report the trail at verbose there and reserve debug
    * for the successful case.
    */
  private def logTrace(fqn: String, result: LookupResult, trace: ResolutionTrace)(using
      logger: Logger[IO]
  ): IO[Unit] =
    val summary = result match
      case LookupResult.Found(symbols)          => s"resolved $fqn to ${symbols.size} symbol(s)"
      case LookupResult.IsPackage               => s"$fqn is a package"
      case LookupResult.NotFound                => s"could not resolve $fqn"
      case LookupResult.PartialMatch(r, m)      => s"resolved $fqn only as far as $r (no member '$m')"
      case LookupResult.LookupFailed(cause)     => s"lookup of $fqn failed: ${cause.getClass.getName}: ${cause.getMessage}"
    val failed = result match
      case _: LookupResult.Found | LookupResult.IsPackage => false
      case _                                              => true
    val emit: String => IO[Unit] = if failed then logger.info(_) else logger.debug(_)
    emit(summary) *> trace.entries.traverse_(step => emit(s"  $step"))

  /**
   * Try to resolve as a top-level class, module, term, type, package, or
   * Scala-3 top-level decl (which lives in a synthetic `<filename>$package$`
   * module class). All public hits with the same FQN are returned.
   */
  private def tryTopLevel(fqn: String, trace: ResolutionTrace)(using ctx: Context): Option[LookupResult] =
    var caught: Option[Throwable] = None
    // ClassCastException is a known tastyquery quirk: thrown (instead of MemberNotFoundException)
    // when a lookup crosses a package boundary for a non-existent symbol.
    def t[A](label: String)(thunk: => A): Option[A] =
      try
        val a = thunk
        trace.add(s"$label($fqn): hit")
        Some(a)
      catch
        case _: MemberNotFoundException =>
          trace.add(s"$label($fqn): miss")
          None
        case _: ClassCastException =>
          trace.add(s"$label($fqn): miss (ClassCastException — tastyquery package-boundary quirk)")
          None
        case scala.util.control.NonFatal(e) =>
          trace.add(s"$label($fqn): ${e.getClass.getName}: ${e.getMessage}")
          if caught.isEmpty then caught = Some(e)
          None

    // As `t`, but without recording into `caught` — mirrors `tryOrNone`, whose failures must not
    // promote a NotFound into a LookupFailed.
    def tq[A](label: String)(thunk: => A): Option[A] =
      try
        val a = thunk
        trace.add(s"$label($fqn): hit")
        Some(a)
      catch
        case _: MemberNotFoundException =>
          trace.add(s"$label($fqn): miss")
          None
        case e: Exception =>
          trace.add(s"$label($fqn): ${e.getClass.getName}: ${e.getMessage}")
          None

    // A trailing `$` is the JVM-level name of a companion object — treat it as
    // an explicit request for the module class, not the class of the same name.
    if fqn.endsWith("$") then
      val stripped = fqn.stripSuffix("$")
      trace.add(s"trailing '$$' — treating as a module class request for $stripped")
      t("findStaticModuleClass")(ctx.findStaticModuleClass(stripped)).map(s => LookupResult.Found(List(s)))
        .orElse(caught.map(LookupResult.LookupFailed(_)))
    else
      val direct = List(
        t("findStaticClass")(ctx.findStaticClass(fqn)),
        t("findStaticModuleClass")(ctx.findStaticModuleClass(fqn)),
        tq("findStaticTerm")(ctx.findStaticTerm(fqn)),
        tq("findStaticType")(ctx.findStaticType(fqn))
      ).flatten
      val wrapped = packageWrapperMembers(fqn)
      if wrapped.nonEmpty then trace.add(s"package wrapper: ${wrapped.size} member(s)")
      val all = (direct ++ wrapped).distinct
      if all.nonEmpty then Some(LookupResult.Found(all))
      else
        tq("findPackage")(ctx.findPackage(fqn)).map(_ => LookupResult.IsPackage)
          .orElse(caught.map(LookupResult.LookupFailed(_)))

  /**
   * Multi-segment nested member walk.
   * Splits the FQN into segments, finds the longest top-level prefix,
   * then walks remaining segments as member lookups.
   */
  private def tryNestedLookup(fqn: String, trace: ResolutionTrace)(using ctx: Context): Option[LookupResult] =
    val segments = fqn.split('.')
    if segments.length < 2 then
      trace.add(s"no nested lookup: $fqn is a single segment")
      return None

    trace.add(s"nested lookup over ${segments.length} segments")
    val (root, firstError) = findTopLevelRoot(segments, trace)
    root match
      case None =>
        trace.add("no top-level root matched any prefix")
        firstError.map(LookupResult.LookupFailed(_))
      case Some((cls, rootIdx)) =>
        val resolvedSoFar = segments.take(rootIdx).mkString(".")
        trace.add(s"root ${cls.displayFullName}, walking ${segments.length - rootIdx} more segment(s)")
        walkMembers(cls, segments, rootIdx, resolvedSoFar)

  /**
   * Walk remaining segments as nested member lookups on the given ClassSymbol.
   * At the final segment, collect both term overloads and type members.
   * At intermediate segments, resolve to a ClassSymbol to continue walking.
   */
  private def walkMembers(
      owner: ClassSymbol,
      segments: Array[String],
      fromIdx: Int,
      resolvedSoFar: String
  )(using ctx: Context): Option[LookupResult] =
    if fromIdx >= segments.length then
      return Some(LookupResult.Found(List(owner)))

    var current: ClassSymbol = owner
    var currentResolved      = resolvedSoFar
    var i                    = fromIdx

    while i < segments.length - 1 do
      val seg = segments(i)
      findClassMember(current, seg) match
        case Some(cls) =>
          currentResolved = s"$currentResolved.$seg"
          current = cls
          i += 1
        case None =>
          return Some(LookupResult.PartialMatch(currentResolved, seg))

    // Final segment: walk linearization for term overloads + type members.
    // If the instance side has nothing, fall through to the companion module
    // so `Foo.apply` resolves to `object Foo`'s `apply`.
    val finalSeg = segments(segments.length - 1)
    val tName    = termName(finalSeg)
    val tyName   = typeName(finalSeg)
    val direct   = collectFinalMembers(current, tName, tyName)
    val all =
      if direct.nonEmpty then direct
      else current.companionClass.toList.flatMap(collectFinalMembers(_, tName, tyName))

    if all.isEmpty then Some(LookupResult.PartialMatch(currentResolved, finalSeg))
    else Some(LookupResult.Found(all))

  private def collectFinalMembers(
      cls: ClassSymbol,
      tName: tastyquery.Names.UnsignedTermName,
      tyName: tastyquery.Names.TypeName
  )(using ctx: Context): List[Symbol] =
    val linearization =
      try cls.linearization
      catch case _: Exception => List(cls)
    val seen    = scala.collection.mutable.Set.empty[Symbol]
    val results = List.newBuilder[Symbol]
    for klass <- linearization do
      for sym <- klass.getAllOverloadedDecls(tName) if !seen.contains(sym) do
        seen += sym
        results += sym
      for sym <- klass.getDecl(tyName) if !seen.contains(sym) do
        seen += sym
        results += sym
    results.result().filter(PublicApiFilter.isPublic)

  /**
   * Find a nested class member by name, walking the linearization.
   * Tries type members first (traits, classes), then term members (objects).
   * Falls through to the companion class so `Foo.Bar` resolves when `Bar` is
   * declared inside `object Foo` rather than on the `Foo` trait/class itself.
   */
  private[cellar] def findClassMember(owner: ClassSymbol, name: String)(using ctx: Context): Option[ClassSymbol] =
    directClassMember(owner, name).orElse {
      owner.companionClass.flatMap(directClassMember(_, name))
    }

  private def directClassMember(owner: ClassSymbol, name: String)(using ctx: Context): Option[ClassSymbol] =
    val byType = owner.getMember(typeName(name)).collect { case cs: ClassSymbol => cs }
    byType.orElse {
      owner.getMember(termName(name)).flatMap(_.moduleClass)
    }

  /**
   * Resolve an FQN to a ClassSymbol, supporting nested types.
   * Shared helper used by both SymbolResolver and SymbolLister.
   */
  def resolveToClass(fqn: String)(using ctx: Context): Either[Option[LookupResult.PartialMatch], ClassSymbol] =
    tryOrNone(ctx.findStaticClass(fqn)).orElse(tryOrNone(ctx.findStaticModuleClass(fqn))) match
      case Some(cls) => Right(cls)
      case None =>
        val segments = fqn.split('.')
        if segments.length < 2 then return Left(None)

        findTopLevelRoot(segments)._1 match
          case None => Left(None)
          case Some((root, rootIdx)) =>
            var current: ClassSymbol = root
            var currentResolved = segments.take(rootIdx).mkString(".")
            var i = rootIdx
            while i < segments.length do
              val seg = segments(i)
              findClassMember(current, seg) match
                case Some(cls) =>
                  currentResolved = s"$currentResolved.$seg"
                  current = cls
                  i += 1
                case None =>
                  return Left(Some(LookupResult.PartialMatch(currentResolved, seg)))
            Right(current)

  /**
   * Try progressively longer prefixes of segments as a top-level class/module.
   * Returns the longest matching ClassSymbol, the index past the root segments,
   * and the first unexpected exception encountered (if any).
   */
  private def findTopLevelRoot(
      segments: Array[String],
      trace: ResolutionTrace = ResolutionTrace()
  )(using ctx: Context): (Option[(ClassSymbol, Int)], Option[Throwable]) =
    var best: Option[(ClassSymbol, Int)] = None
    var firstError: Option[Throwable] = None
    var i = 1
    while i < segments.length do
      val prefix = segments.take(i + 1).mkString(".")
      def t[A](thunk: => A): Option[A] =
        try Some(thunk)
        catch
          case _: MemberNotFoundException => None
          case _: ClassCastException      => None
          case scala.util.control.NonFatal(e) =>
            if firstError.isEmpty then firstError = Some(e)
            None
      val found = t(ctx.findStaticClass(prefix))
        .orElse(t(ctx.findStaticModuleClass(prefix)))
        .orElse(packageWrapperClass(prefix))
      trace.add(s"prefix '$prefix': ${if found.isDefined then "hit" else "miss"}")
      if found.isDefined then best = Some((found.get, i + 1))
      i += 1
    (best, firstError)

  /**
   * Scala 3 top-level defs/types/objects in `pkg` live inside a synthetic
   * `<filename>$package$` module class. Walks every such wrapper in `pkg` and
   * applies `f` with the member name to be looked up.
   */
  private def withPackageWrappers[A](fqn: String)(f: (ClassSymbol, String) => IterableOnce[A])(using ctx: Context): List[A] =
    val idx = fqn.lastIndexOf('.')
    if idx < 0 then return Nil
    val name = fqn.substring(idx + 1)
    tryOrNone(ctx.findPackage(fqn.substring(0, idx))).toList.flatMap { pkg =>
      tryOrNone(pkg.declarations).getOrElse(Nil).flatMap {
        case c: ClassSymbol if c.isModuleClass && c.name.toString.endsWith("$package$") =>
          f(c, name).iterator.toList
        case _ => Nil
      }
    }

  private def packageWrapperMembers(fqn: String)(using ctx: Context): List[Symbol] =
    withPackageWrappers[Symbol](fqn) { (w, name) =>
      w.getAllOverloadedDecls(termName(name)) ++ w.getDecl(typeName(name))
    }.filter(PublicApiFilter.isPublic)

  private def packageWrapperClass(fqn: String)(using ctx: Context): Option[ClassSymbol] =
    withPackageWrappers[ClassSymbol](fqn)((w, name) => directClassMember(w, name)).headOption

  private[cellar] val universalBaseClasses = Set("scala.Any", "scala.AnyRef", "java.lang.Object")

  /**
   * Walk the linearization (MRO) of a class, collecting all declarations.
   * Uses tastyquery's `overridingSymbol` to skip declarations that are
   * overridden by a more-derived class, while preserving distinct overloads.
   */
  def collectClassMembers(cls: ClassSymbol)(using ctx: Context): List[TermOrTypeSymbol] =
    val seen   = scala.collection.mutable.Set.empty[TermOrTypeSymbol]
    val result = List.newBuilder[TermOrTypeSymbol]
    val linearization =
      try cls.linearization
      catch case _: Exception => List(cls)
    linearization.filterNot(k => universalBaseClasses.contains(k.displayFullName)).foreach { klass =>
      val decls =
        try klass.declarations
        catch case _: Exception => Nil
      decls.foreach { decl =>
        if !seen.contains(decl) then
          val dominated = decl.overridingSymbol(cls).exists(_ != decl)
          if !dominated then
            seen += decl
            result += decl
      }
    }
    result.result()

  private def tryOrNone[A](thunk: => A): Option[A] =
    try Some(thunk)
    catch
      case _: MemberNotFoundException => None
      case _: Exception               => None

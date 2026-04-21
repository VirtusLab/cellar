package cellar

import cats.effect.IO
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

object SymbolResolver:
  def resolve(fqn: String)(using ctx: Context): IO[LookupResult] =
    IO.blocking(tryTopLevel(fqn)).flatMap {
      case Some(result) => IO.pure(result)
      case None         => IO.blocking(tryNestedLookup(fqn)).map(_.getOrElse(LookupResult.NotFound))
    }

  /** Try to resolve as a top-level class, module, term, type, or package. */
  private def tryTopLevel(fqn: String)(using ctx: Context): Option[LookupResult] =
    // A trailing `$` is the JVM-level name of a companion object — treat it as
    // an explicit request for the module class, not the class of the same name.
    if fqn.endsWith("$") then
      val stripped = fqn.stripSuffix("$")
      tryOrNone(ctx.findStaticModuleClass(stripped)).map(s => LookupResult.Found(List(s)))
    else
      tryOrNone(ctx.findStaticClass(fqn)).map(s => LookupResult.Found(List(s)))
        .orElse(tryOrNone(ctx.findStaticModuleClass(fqn)).map(s => LookupResult.Found(List(s))))
        .orElse(tryOrNone(ctx.findStaticTerm(fqn)).map(s => LookupResult.Found(List(s))))
        .orElse(tryOrNone(ctx.findStaticType(fqn)).map(s => LookupResult.Found(List(s))))
        .orElse(tryOrNone(ctx.findPackage(fqn)).map(_ => LookupResult.IsPackage))

  /**
   * Multi-segment nested member walk.
   * Splits the FQN into segments, finds the longest top-level prefix,
   * then walks remaining segments as member lookups.
   */
  private def tryNestedLookup(fqn: String)(using ctx: Context): Option[LookupResult] =
    val segments = fqn.split('.')
    if segments.length < 2 then return None

    findTopLevelRoot(segments) match
      case None => None
      case Some((root, rootIdx)) =>
        val resolvedSoFar = segments.take(rootIdx).mkString(".")
        walkMembers(root, segments, rootIdx, resolvedSoFar)

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

        findTopLevelRoot(segments) match
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
   * Returns the longest matching ClassSymbol and the index past the root segments.
   */
  private def findTopLevelRoot(segments: Array[String])(using ctx: Context): Option[(ClassSymbol, Int)] =
    var best: Option[(ClassSymbol, Int)] = None
    var i = 1
    while i < segments.length do
      val prefix = segments.take(i + 1).mkString(".")
      val found = tryOrNone(ctx.findStaticClass(prefix))
        .orElse(tryOrNone(ctx.findStaticModuleClass(prefix)))
      if found.isDefined then best = Some((found.get, i + 1))
      i += 1
    best

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

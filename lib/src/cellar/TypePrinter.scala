package cellar

import tastyquery.Contexts.Context
import tastyquery.Symbols.{
  ClassSymbol,
  ClassTypeParamSymbol,
  Symbol,
  TermOrTypeSymbol,
  TermSymbol,
  TypeMemberDefinition,
  TypeMemberSymbol
}
import tastyquery.Types.*

enum DetectedLanguage:
  case Scala3, Scala2, Java

object TypePrinter:
  def detectLanguage(sym: Symbol): DetectedLanguage =
    val lang = sym match
      case s: TermOrTypeSymbol => s.sourceLanguage.productPrefix
      case _                   => "Scala3"
    lang match
      case "Scala2" => DetectedLanguage.Scala2
      case "Java"   => DetectedLanguage.Java
      case _        => DetectedLanguage.Scala3


  def printType(tpe: Type)(using ctx: Context): String =
    tpe match
      case t: TypeRef =>
        val name = t.name.toString
        t.prefix match
          case NoPrefix                          => name
          case _: ThisType                       => name
          case p: Type if isElidedPrefix(p)      => name
          case p: Type                           => s"${printType(p)}.$name"
          case _                                 => name

      case t: AppliedType =>
        asFunction(t) match
          case Some((contextual, params, result)) =>
            val arrow = if contextual then " ?=> " else " => "
            val lhs = params match
              case single :: Nil if !functionArgNeedsParens(single) => printTypeOrWildcard(single)
              case _ => params.map(printTypeOrWildcard).mkString("(", ", ", ")")
            s"$lhs$arrow${printTypeOrWildcard(result)}"
          case None =>
            asTuple(t) match
              case Some(elems) => elems.map(printTypeOrWildcard).mkString("(", ", ", ")")
              case None =>
                asInfix(t) match
                  case Some((lhs, op, rhs)) =>
                    s"${printTypeOrWildcard(lhs)} $op ${printTypeOrWildcard(rhs)}"
                  case None =>
                    val args = t.args.map(printTypeOrWildcard).mkString(", ")
                    s"${printType(t.tycon)}[$args]"

      case t: ByNameType     => s"=> ${printType(t.resultType)}"
      case t: AndType        => s"${printType(t.first)} & ${printType(t.second)}"
      case t: OrType         => s"${printType(t.first)} | ${printType(t.second)}"
      case t: AnnotatedType  => printType(t.typ)
      case t: ThisType       => s"${t.tref.name}.this"
      case t: TermRef        => t.name.toString
      case t: TermParamRef   => t.binder.paramNames(t.paramNum).toString
      case t: TypeParamRef   => t.binder.paramNames(t.paramNum).toString
      case t: RepeatedType   => s"${printType(t.elemType)}*"
      case t: TypeRefinement => printType(t.parent)
      case t: TermRefinement => printType(t.parent)
      case t: RecType        => printType(t.parent)
      case t: ConstantType   => t.value.value.toString
      case t: MatchType      => s"${printType(t.scrutinee)} match { ... }"
      case t: FlexibleType   => printType(t.nonNullableType)
      case t: TypeLambda =>
        val params = t.paramNames.zip(t.paramTypeBounds).map(printTypeParam)
        s"[${params.mkString(", ")}] =>> ${printType(t.resultType)}"
      // Without this, Nothing falls through to the class-name fallback below and prints as
      // "NothingType" — which also defeats the unbounded-lower-bound elision in printTypeParam,
      // since that compares against "Nothing".
      case t: NothingType    => "Nothing"
      case _                 => tpe.getClass.getSimpleName

  def printMethodic(tpe: TypeOrMethodic)(using ctx: Context): String =
    tpe match
      case t: MethodType =>
        val prefix =
          if t.isContextual then "using "
          else if t.isImplicit then "implicit "
          else ""
        val params = t.paramNames.zip(t.paramTypes).map { (n, tp) =>
          s"$n: ${printType(tp)}"
        }
        val paramStr = s"($prefix${params.mkString(", ")})"
        val rest = t.resultType match
          case _: MethodType | _: PolyType => printMethodic(t.resultType)
          case r                           => s": ${printMethodic(r)}"
        s"$paramStr$rest"

      case t: PolyType =>
        val typeParams = t.paramNames.zip(t.paramTypeBounds).map(printTypeParam)
        s"[${typeParams.mkString(", ")}]${printMethodic(t.resultType)}"

      case t: Type => printType(t)

  def printSymbolSignatureSafe(sym: Symbol)(using ctx: Context): String =
    val lang = detectLanguage(sym)
    val sig =
      try printSymbolSignature(sym)
      catch case _: Exception => s"${sym.name} // [signature unavailable]"
    lang match
      case DetectedLanguage.Scala2 => s"$sig // [Scala 2 — limited type information]"
      case _                       => sig

  def printSymbolSignature(sym: tastyquery.Symbols.Symbol)(using ctx: Context): String =
    sym match
      case cls: ClassSymbol =>
        val kind       = if cls.isTrait then "trait" else if cls.isModuleClass then "object" else "class"
        // A module class is named `Foo$` on the JVM; print the source-level name.
        val name       = if cls.isModuleClass then cls.name.toString.stripSuffix("$") else cls.name.toString
        val typeParams = printClassTypeParams(cls.typeParams)
        // `AnyRef` is how the universal parent prints for a Scala 2 / Java symbol,
        // the same way `Object` is for a Scala 3 one -- neither carries information.
        val parents    = cls.parents.map(printParent).filterNot(universalParents.contains)
        val extendsStr = if parents.isEmpty then "" else s" extends ${parents.mkString(" with ")}"
        s"$kind $name$typeParams$extendsStr"

      case term: TermSymbol =>
        val keyword = termKeyword(term)
        if term.isModuleVal then s"$keyword ${term.name}"
        else s"$keyword ${term.name}${printTopLevelMethodic(term.declaredType)}"

      case tm: TypeMemberSymbol =>
        tm.typeDef match
          case TypeMemberDefinition.OpaqueTypeAlias(_, alias) =>
            s"opaque type ${tm.name} = ${printType(alias)}"
          case TypeMemberDefinition.TypeAlias(alias) =>
            s"type ${tm.name} = ${printType(alias)}"
          case TypeMemberDefinition.AbstractType(bounds) =>
            s"type ${tm.name}${printBoundsSuffix(bounds)}"

      case other => other.toString

  /** ` >: L <: H`, omitting either half when it is the trivial bound. */
  private def printBoundsSuffix(bounds: TypeBounds)(using Context): String =
    bounds match
      case b: AbstractTypeBounds =>
        // compare the rendered form: `Type.toString` is a structural dump, never "Nothing"
        val low  = printType(b.low)
        val high = printType(b.high)
        val lo   = if low == "Nothing" then "" else s" >: $low"
        val hi   = if high == "Any" then "" else s" <: $high"
        s"$lo$hi"
      // TypeAlias is the other TypeBounds subtype; low == high == the aliased type
      case b: TypeAlias => s" = ${printType(b.low)}"

  private def termKeyword(sym: TermSymbol): String =
    if sym.isGivenOrUsing then "given"
    else if sym.isInline && sym.isMethod then "inline def"
    else if sym.isMethod then "def"
    else if sym.isModuleVal then "object"
    else "val"

  private def printTopLevelMethodic(tpe: TypeOrMethodic)(using ctx: Context): String =
    tpe match
      case t: Type => s": ${printType(t)}"
      case t: PolyType =>
        val typeParams = t.paramNames.zip(t.paramTypeBounds).map(printTypeParam)
        s"[${typeParams.mkString(", ")}]${printTopLevelMethodic(t.resultType)}"
      case t: MethodType => printMethodic(t)

  private def printClassTypeParams(params: List[ClassTypeParamSymbol])(using ctx: Context): String =
    if params.isEmpty then ""
    else
      val rendered = params.map(printClassTypeParam)
      s"[${rendered.mkString(", ")}]"

  private def printClassTypeParam(param: ClassTypeParamSymbol)(using ctx: Context): String =
    val variance = param.declaredVariance.productPrefix match
      case "Covariant"     => "+"
      case "Contravariant" => "-"
      case _               => ""
    s"$variance${printTypeParam(param.name, param.declaredBounds)}"

  private def printTypeParam(name: tastyquery.Names.TypeName, bounds: TypeBounds)(using ctx: Context): String =
    bounds match
      case b: AbstractTypeBounds =>
        b.high match
          case tl: TypeLambda => s"$name${printHkParams(tl)}"
          case _ =>
            val lo = if printType(b.low) == "Nothing" then "" else s" >: ${printType(b.low)}"
            val hi = if printType(b.high) == "Any" then "" else s" <: ${printType(b.high)}"
            s"$name$lo$hi"
      case _ => name.toString

  private def printHkParams(tl: TypeLambda)(using ctx: Context): String =
    val rendered = tl.paramNames.zip(tl.paramTypeBounds).map { (name, bounds) =>
      val (lo, hi) = bounds match
        case b: AbstractTypeBounds =>
          val lo = if printType(b.low) == "Nothing" then "" else s" >: ${printType(b.low)}"
          val hi = if printType(b.high) == "Any" then "" else s" <: ${printType(b.high)}"
          (lo, hi)
        case _ => ("", "")
      (name.toString, lo, hi)
    }
    // A parameter must be named (not `_`) when it is referenced elsewhere in the lambda:
    // in a bound (self-referential, e.g. `A <: Comparable[A]`) or in the constructor
    // bound (the lambda's result, e.g. `F[A] <: Iterable[A]`).
    val resultStr = printType(tl.resultType)
    val context   = (resultStr :: rendered.flatMap((_, lo, hi) => List(lo, hi))).mkString(" ")
    val params = rendered.map { (name, lo, hi) =>
      val head = if isMentioned(context, name) then name else "_"
      s"$head$lo$hi"
    }
    val constructorBound = if resultStr == "Any" then "" else s" <: $resultStr"
    s"[${params.mkString(", ")}]$constructorBound"

  private def isMentioned(context: String, name: String): Boolean =
    context.matches(s"(?s).*\\b${java.util.regex.Pattern.quote(name)}\\b.*")

  private def printTypeOrWildcard(tow: TypeOrWildcard)(using ctx: Context): String =
    tow match
      case w: WildcardTypeArg =>
        w.bounds match
          case b: AbstractTypeBounds =>
            val lo = if printType(b.low) == "Nothing" then "" else s" >: ${printType(b.low)}"
            val hi = if printType(b.high) == "Any" then "" else s" <: ${printType(b.high)}"
            if lo.isEmpty && hi.isEmpty then "?" else s"?$lo$hi"
          case _ => "?"
      case t: Type => printType(t)

  /** Render a class parent, parenthesising function-arrow sugar so it is valid in `extends` position. */
  private val universalParents = Set("Object", "Any", "AnyRef")

  private def printParent(tpe: Type)(using ctx: Context): String =
    val rendered = printType(tpe)
    tpe match
      case t: AppliedType if asFunction(t).isDefined => s"($rendered)"
      case _                                         => rendered

  private def isElidedPrefix(prefix: Type): Boolean =
    prefix match
      case _: ThisType => true
      case t: TermRef  => t.name.toString == "Predef" || t.name.toString == "package"
      case _           => false

  /** Decompose `scala.FunctionN` / `scala.ContextFunctionN` into (isContextual, params, result). */
  private def asFunction(t: AppliedType): Option[(Boolean, List[TypeOrWildcard], TypeOrWildcard)] =
    t.tycon match
      case tycon: TypeRef if isScalaPackage(tycon.prefix) =>
        val name = tycon.name.toString
        val decoded =
          if name.startsWith("ContextFunction") then Some((true, name.stripPrefix("ContextFunction")))
          else if name.startsWith("Function") then Some((false, name.stripPrefix("Function")))
          else None
        decoded.flatMap { (contextual, digits) =>
          digits.toIntOption
            .filter(arity => arity >= 0 && t.args.sizeIs == arity + 1)
            .map(_ => (contextual, t.args.init, t.args.last))
        }
      case _ => None

  /** Decompose `scala.TupleN` (arity >= 2) into its element types. */
  private def asTuple(t: AppliedType): Option[List[TypeOrWildcard]] =
    t.tycon match
      case tycon: TypeRef if isScalaPackage(tycon.prefix) =>
        tycon.name.toString.stripPrefix("Tuple").toIntOption
          .filter(arity => arity >= 2 && t.args.sizeIs == arity)
          .map(_ => t.args)
      case _ => None

  /** Decompose a binary applied type whose tycon is a symbolic operator (e.g. `F ~> G`). */
  private def asInfix(t: AppliedType): Option[(TypeOrWildcard, String, TypeOrWildcard)] =
    t.tycon match
      case tycon: TypeRef if t.args.sizeIs == 2 && isOperatorName(tycon.name.toString) =>
        Some((t.args.head, tycon.name.toString, t.args(1)))
      case _ => None

  /** An identifier composed solely of operator characters (no letters/digits/`_`/`$`). */
  private def isOperatorName(name: String): Boolean =
    name.nonEmpty && name.forall(c => !c.isLetterOrDigit && c != '_' && c != '$')

  private def isScalaPackage(prefix: Prefix): Boolean =
    prefix match
      case p: PackageRef => p.fullyQualifiedName.toString == "scala"
      case _             => false

  /** A function- or tuple-typed left operand of `=>` must be parenthesised: `(A => B) => C`, `((A, B)) => C`. */
  private def functionArgNeedsParens(tow: TypeOrWildcard): Boolean =
    tow match
      case t: AppliedType => asFunction(t).isDefined || asTuple(t).isDefined
      case _              => false

package cellar

import tastyquery.Contexts.Context
import tastyquery.Symbols.{ClassSymbol, ClassTypeParamSymbol, Symbol, TermOrTypeSymbol, TermSymbol}
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
          case p: Type if isPackageOrNone(p)    => name
          case p: Type                           => s"${printType(p)}.$name"
          case _                                 => name

      case t: AppliedType =>
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
      case _                 => tpe.getClass.getSimpleName

  def printMethodic(tpe: TypeOrMethodic)(using ctx: Context): String =
    tpe match
      case t: MethodType =>
        val prefix = if t.isContextual then "using " else ""
        val params = t.paramNames.zip(t.paramTypes).map { (n, tp) =>
          s"$n: ${printType(tp)}"
        }
        val paramStr = s"($prefix${params.mkString(", ")})"
        s"$paramStr: ${printMethodic(t.resultType)}"

      case t: PolyType =>
        val typeParams = t.paramNames.zip(t.paramTypeBounds).map { (n, bounds) =>
          bounds match
            case b: AbstractTypeBounds =>
              val lo = if b.low.toString == "Nothing" then "" else s" >: ${printType(b.low)}"
              val hi = if b.high.toString == "Any" then "" else s" <: ${printType(b.high)}"
              s"$n$lo$hi"
            case _ => n.toString
        }
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
        val typeParams = printClassTypeParams(cls.typeParams)
        val parents    = cls.parents.map(printType).filter(p => p != "Object" && p != "Any")
        val extendsStr = if parents.isEmpty then "" else s" extends ${parents.mkString(" with ")}"
        s"$kind ${cls.name}$typeParams$extendsStr"

      case term: TermSymbol =>
        val keyword = termKeyword(term)
        if term.isModuleVal then s"$keyword ${term.name}"
        else
          val sig = printMethodic(term.declaredType)
          s"$keyword ${term.name}$sig"

      case other => other.toString

  private def termKeyword(sym: TermSymbol): String =
    if sym.isGivenOrUsing then "given"
    else if sym.isInline && sym.isMethod then "inline def"
    else if sym.isMethod then "def"
    else if sym.isModuleVal then "object"
    else "val"

  private def printClassTypeParams(params: List[ClassTypeParamSymbol]): String =
    if params.isEmpty then ""
    else
      val rendered = params.map { p =>
        val variance = p.declaredVariance.productPrefix match
          case "Covariant"     => "+"
          case "Contravariant" => "-"
          case _               => ""
        s"$variance${p.name}"
      }
      s"[${rendered.mkString(", ")}]"

  private def printTypeOrWildcard(tow: TypeOrWildcard)(using ctx: Context): String =
    tow match
      case w: WildcardTypeArg =>
        w.bounds match
          case b: AbstractTypeBounds =>
            val lo = if b.low.toString == "Nothing" then "" else s" >: ${printType(b.low)}"
            val hi = if b.high.toString == "Any" then "" else s" <: ${printType(b.high)}"
            if lo.isEmpty && hi.isEmpty then "?" else s"?$lo$hi"
          case _ => "?"
      case t: Type => printType(t)

  private def isPackageOrNone(prefix: Type): Boolean =
    prefix match
      case _: ThisType => true
      case _           => false

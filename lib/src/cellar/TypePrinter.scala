package cellar

import tastyquery.Contexts.Context
import tastyquery.Modifiers.{OpenLevel, TermSymbolKind, Visibility}
import tastyquery.Names.nme
import tastyquery.Symbols.{
  ClassSymbol,
  ClassTypeParamSymbol,
  ParamSymbolsClause,
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
      case t: TypeRef if isFromJavaObject(t) => "Object"

      case t: TypeRef =>
        val name = t.name.toString
        t.prefix match
          case NoPrefix                          => name
          case _: ThisType                       => name
          case p: Type if isElidedPrefix(p)      => name
          // A path prefix (`Tracer.Type`), not the singleton type a TermRef is on its own.
          case p: TermRef                        => s"${p.name}.$name"
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
                    val args = t.args.map(printTypeArgument).mkString(", ")
                    s"${printType(t.tycon)}[$args]"

      case t: ByNameType     => s"=> ${printType(t.resultType)}"
      case t: AndType        => s"${printType(t.first)} & ${printType(t.second)}"
      case t: OrType         => s"${printType(t.first)} | ${printType(t.second)}"
      case t: AnnotatedType  => printType(t.typ)
      case t: ThisType       => s"${t.tref.name}.this"
      case t: TermRef        => s"${t.name}.type"
      case t: TermParamRef   => t.binder.paramNames(t.paramNum).toString
      case t: TypeParamRef   => t.binder.paramNames(t.paramNum).toString
      case t: RepeatedType   => s"${printType(t.elemType)}*"
      case t: (TypeRefinement | TermRefinement) =>
        val (base, members) = refinements(t)
        s"${printType(base)} { ${members.mkString("; ")} }"
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
      case _: NothingType    => "Nothing"
      case _                 => tpe.getClass.getSimpleName

  /** `paramSymss` runs alongside the type: the type alone has no notion of a default argument,
   *  only the parameter symbol does, and a method's clauses line up one-to-one with them.
   */
  def printMethodic(
      tpe: TypeOrMethodic,
      paramSymss: List[ParamSymbolsClause] = Nil,
      javaNames: Option[List[String]] = None,
      defaultValues: Map[String, String] = Map.empty,
      elided: Set[String] = Set.empty
  )(using ctx: Context): String =
    tpe match
      case t: MethodType if t.paramNames.nonEmpty && t.paramNames.forall(n => elided(n.toString)) =>
        printMethodic(t.resultType, paramSymss.drop(1), None, defaultValues, elided) match
          case rest if t.resultType.isInstanceOf[Type] => s": $rest"
          case rest                                    => rest
      case t: MethodType =>
        val prefix =
          if t.isContextual then "using "
          else if t.isImplicit then "implicit "
          else ""
        val defaults = paramSymss.headOption match
          case Some(Left(syms)) => syms.map(_.isParamWithDefault)
          case _                => Nil
        val names = javaNames.filter(_.length == t.paramNames.length).getOrElse(t.paramNames.map(_.toString))
        val params = names.zip(t.paramTypes).zipWithIndex.collect { case ((n, tp), i) if !elided(n) =>
          val default =
            if defaults.lift(i).contains(true) then s" = ${defaultValues.get(n).map(printDefault).getOrElse("...")}" else ""
          s"$n: ${printType(tp)}$default"
        }
        val paramStr = s"($prefix${params.mkString(", ")})"
        val rest = t.resultType match
          case _: MethodType | _: PolyType => printMethodic(t.resultType, paramSymss.drop(1), None, defaultValues, elided)
          case r                           => s": ${printMethodic(r, Nil, None, Map.empty, Set.empty)}"
        s"$paramStr$rest"

      case t: PolyType =>
        val (typeParams, evidence) = printPolyParams(t)
        s"$typeParams${printMethodic(t.resultType, paramSymss.drop(1), javaNames, defaultValues, elided ++ evidence)}"

      case t: Type => printType(t)

  private val Evidence = """evidence\$\d+""".r

  /** `[A: Monoid, F[_]]` and the evidence parameters those context bounds stand for. The compilers
   *  name a context bound's evidence `evidence$N` and pass it in an implicit or using clause, typed
   *  by the bound applied to the one type parameter.
   */
  private def printPolyParams(poly: PolyType)(using Context): (String, Set[String]) =
    def evidenceIn(tpe: TypeOrMethodic): List[(String, Int, String)] =
      tpe match
        case m: MethodType =>
          val here =
            if !(m.isImplicit || m.isContextual) then Nil
            else
              m.paramNames.zip(m.paramTypes).collect {
                case (n, app: AppliedType) if Evidence.matches(n.toString) =>
                  app.args match
                    case List(ref: TypeParamRef) if ref.binder eq poly => Some((n.toString, ref.paramNum, printType(app.tycon)))
                    case _                                             => None
              }.flatten
          here ++ evidenceIn(m.resultType)
        case _ => Nil
    val evidence = evidenceIn(poly.resultType)
    val bounds   = evidence.groupMap(_._2)(_._3)
    val params = poly.paramNames.zip(poly.paramTypeBounds).zipWithIndex.map { case ((n, b), i) =>
      printTypeParam(n, b) + bounds.getOrElse(i, Nil).map(tc => s": $tc").mkString
    }
    (s"[${params.mkString(", ")}]", evidence.map(_._1).toSet)

  /** `<FromJavaObject>` is the internal name for Java's Object as seen from Scala. It is a real
   *  TypeRef, not a fallback, so it would otherwise print verbatim, angle brackets and all.
   *  Matched by name rather than tasty-query's `isFromJavaObject`, which resolves the symbol and
   *  throws on primitive parameter types — that took out every signature containing an int.
   */
  private[cellar] def isFromJavaObject(t: TypeRef): Boolean =
    t.name.toString == "<FromJavaObject>"

  private[cellar] def javaParamNames(term: TermSymbol)(using Context): Option[List[String]] =
    if detectLanguage(term) == DetectedLanguage.Java && term.isMethod then
      JavaClassfiles.namesFor(term).orElse(JavaSources.paramNames(term))
    else None

  private val MaxDefaultLength = 40

  /** A default is shown as written when it is one short line; joining lines could swallow the
    * rest of the signature into a `//` comment or run statements together.
    */
  private def printDefault(expr: String): String =
    val printable = !expr.contains('\n') && !expr.contains("//") && !expr.contains("/*")
    if printable && expr.length <= MaxDefaultLength then expr else "..."

  def printSymbolSignatureSafe(sym: Symbol)(using ctx: Context): String =
    printSymbolSignatureSafe(sym, detectLanguage(sym))

  /** `syntax` is set by a listing to its own language, so that members inherited from the other
   *  language do not switch syntax mid-block.
   */
  def printSymbolSignatureSafe(sym: Symbol, syntax: DetectedLanguage)(using ctx: Context): String =
    try printSymbolSignature(sym, syntax)
    catch case _: Exception => s"${sym.name} // [signature unavailable]"

  def printSymbolSignature(sym: Symbol)(using ctx: Context): String =
    printSymbolSignature(sym, detectLanguage(sym))

  def printSymbolSignature(sym: Symbol, syntax: DetectedLanguage)(using ctx: Context): String =
    if syntax == DetectedLanguage.Java then JavaTypePrinter.printSymbolSignature(sym)
    else printScalaSignature(sym)

  private[cellar] def printScalaSignature(sym: Symbol)(using ctx: Context): String =
    sym match
      case cls: ClassSymbol =>
        val kind =
          if cls.isTrait then "trait"
          else if cls.isModuleClass then "object"
          else if cls.isEnum then (if cls.isCaseClass then "case" else "enum")
          else "class"
        // A module class is named `Foo$` on the JVM; print the source-level name.
        val name       = if cls.isModuleClass then cls.name.toString.stripSuffix("$") else cls.name.toString
        val typeParams = printClassTypeParams(cls.typeParams)
        // `AnyRef` is how the universal parent prints for a Scala 2 / Java symbol,
        // the same way `Object` is for a Scala 3 one -- neither carries information.
        val parents =
          cls.parents.filterNot(p => cls.isEnum && isEnumBase(p) || isCompilerAddedParent(cls, p))
            .map(printParent).filterNot(universalParents.contains)
        val extendsStr = if parents.isEmpty then "" else s" extends ${parents.mkString(" with ")}"
        val implicitness = if isImplicitClass(cls) then "implicit " else ""
        s"${visibilityModifier(cls)}$implicitness${classModifiers(cls)}$kind $name$typeParams$extendsStr"

      case term: TermSymbol =>
        val keyword = visibilityModifier(term) + termModifiers(term) + termKeyword(term)
        if term.isModuleVal then s"$keyword ${term.name}"
        else if term.isEnumCase then s"case ${term.name} extends ${printMethodic(term.declaredType)}"
        else
          val javaNames = javaParamNames(term)
          val defaults  = if term.hasParamWithDefault then ScalaSources.defaults(term) else Map.empty
          s"$keyword ${term.name}${printTopLevelMethodic(term.declaredType, term.paramSymss, javaNames, defaults)}"

      case tm: TypeMemberSymbol =>
        val visibility = visibilityModifier(tm)
        tm.typeDef match
          case TypeMemberDefinition.OpaqueTypeAlias(_, alias) =>
            s"${visibility}opaque type ${tm.name}${printAliasRhs(alias)}"
          case TypeMemberDefinition.TypeAlias(alias) =>
            s"${visibility}type ${tm.name}${printAliasRhs(alias)}"
          case TypeMemberDefinition.AbstractType(bounds) =>
            s"${visibility}type ${printTypeParam(tm.name, bounds)}"

      case other => other.toString

  /**
   * Renders one type argument, contracting a compiler-generated eta-expansion back to the
   * constructor it expanded.
   *
   * Passing `C` where `F[_]` is expected makes the compiler store `[X] =>> C[X]` in TASTy, so a
   * Scala 3 artifact renders `NonEmptyCollection[A, List, [A] =>> NonEmptyList[A]]` where the
   * Scala 2 build of the same class renders the `NonEmptyList` a reader would write.
   *
   * Deliberately limited to argument position, because position is the only signal there is. A
   * hand-written `type F = [A] =>> C[A]` is a different declaration from `type F = C`, but the two
   * are indistinguishable in the data: measured on a covariant class, the compiler-generated
   * expansion reports an *invariant* parameter, exactly like a hand-written one, so comparing the
   * lambda's variance against the constructor's does not separate them (and would refuse the very
   * case this exists to fix). Reaching the variance at all needs reflection, and it would not help.
   *
   * So: in argument position the expansion is the compiler's doing, and contracting reads back as
   * the source did; as the whole right-hand side of an alias it is the author's, and is left alone.
   */
  private def printTypeArgument(arg: TypeOrWildcard)(using ctx: Context): String =
    arg match
      case tl: TypeLambda => etaContracted(tl).getOrElse(printTypeOrWildcard(tl))
      case other          => printTypeOrWildcard(other)

  /**
   * `C` for a lambda that just passes its parameters straight through to `C`, in order, with no
   * bounds of their own. `[A <: AnyRef] =>> F[A]` says something the bare `F` does not, and
   * `[A] =>> F[G[A]]` is not a plain constructor at all — neither contracts.
   */
  private def etaContracted(t: TypeLambda)(using ctx: Context): Option[String] =
    val trivialBounds = t.paramTypeBounds.forall {
      case b: AbstractTypeBounds => printType(b.low) == "Nothing" && printType(b.high) == "Any"
      case _                     => false
    }
    t.resultType match
      case app: AppliedType if trivialBounds && app.args.sizeIs == t.paramNames.size =>
        val passesThrough = app.args.zipWithIndex.forall {
          case (ref: TypeParamRef, i) => ref.binder == t && ref.paramNum == i
          case _                      => false
        }
        Option.when(passesThrough)(printType(app.tycon))
      case _ => None

  /** The refined type and its members, outermost refinement last as it is declared last. */
  private def refinements(tpe: Type)(using Context): (Type, List[String]) =
    tpe match
      case t: TypeRefinement =>
        val (base, members) = refinements(t.parent)
        val member = t.refinedBounds match
          case alias: TypeAlias => s"type ${t.refinedName}${printAliasRhs(alias.alias)}"
          case bounds           => s"type ${printTypeParam(t.refinedName, bounds)}"
        (base, members :+ member)
      case t: TermRefinement =>
        val (base, members) = refinements(t.parent)
        val member = t.refinedType match
          case tpe: Type if t.isStable => s"val ${t.refinedName}: ${printType(tpe)}"
          case tpe: Type               => s"def ${t.refinedName}: ${printType(tpe)}"
          case methodic                => s"def ${t.refinedName}${printMethodic(methodic)}"
        (base, members :+ member)
      case other => (other, Nil)

  /** TASTy stores `type F[A] = G[A]` and `type F = [A] =>> G[A]` alike; the former is how
   *  aliases are almost always written, so a lambda's parameters print as the alias's.
   */
  private def printAliasRhs(alias: Type)(using Context): String =
    alias match
      case tl: TypeLambda =>
        val params = tl.paramNames.zip(tl.paramTypeBounds).map(printTypeParam)
        s"[${params.mkString(", ")}] = ${printType(tl.resultType)}"
      case other => s" = ${printType(other)}"

  /** tasty-query exposes no implicit flag on a class, but an implicit class always comes with the
   *  implicit conversion of the same name the compiler writes next to it.
   */
  private def isImplicitClass(cls: ClassSymbol)(using Context): Boolean =
    !cls.isModuleClass && (cls.owner match
      case owner: ClassSymbol =>
        owner.declarations.exists {
          case t: TermSymbol => t.isImplicit && t.isMethod && t.name.toString == cls.name.toString
          case _             => false
        }
      case _ => false)

  /** Space-terminated, so it prepends to the kind keyword as-is; empty when there is nothing to say. */
  private def classModifiers(cls: ClassSymbol): String =
    val caseness = if cls.isCaseClass then List("case") else Nil
    // Every object is final, and the statics module tasty-query synthesizes for a Java abstract
    // class inherits its abstractness; only `case` tells objects apart.
    if cls.isModuleClass then caseness.map(_ + " ").mkString
    // `sealed abstract` / `final case` are how an enum and its cases are encoded, not declared.
    else if cls.isEnum then ""
    else
      val openness = cls.openLevel match
        case OpenLevel.Sealed => List("sealed")
        case OpenLevel.Final  => List("final")
        case _                => Nil
      // A trait is abstract by definition.
      val abstractness = if cls.isAbstractClass && !cls.isTrait then List("abstract") else Nil
      (openness ++ abstractness ++ caseness).map(_ + " ").mkString

  /** Space-terminated. Listings drop plain `private` members, so only the qualified and protected
   *  forms, which leave a member callable from somewhere outside its class, reach here.
   */
  private def visibilityModifier(sym: TermOrTypeSymbol): String =
    sym.visibility match
      case Visibility.Protected              => "protected "
      case Visibility.ProtectedThis          => "protected[this] "
      case Visibility.ScopedProtected(scope) => s"protected[${scopeName(scope)}] "
      case Visibility.ScopedPrivate(scope)   => s"private[${scopeName(scope)}] "
      case _                                 => ""

  // A scope that is an object is its module class, named `Foo$` on the JVM.
  private def scopeName(scope: tastyquery.Symbols.DeclaringSymbol): String =
    scope.name.toString.stripSuffix("$")

  /** Space-terminated, like [[classModifiers]]. */
  private def termModifiers(sym: TermSymbol)(using Context): String =
    val implicitness = if sym.isImplicit then "implicit " else ""
    if sym.isModuleVal then
      // `implicit` sits on the val that holds an object; `case` on its module class.
      implicitness + sym.moduleClass.map(classModifiers).getOrElse("")
    else
      List(
        // tasty-query reports every Scala 3 constructor as abstract.
        Option.when(sym.isAbstractMember && sym.name != nme.Constructor)("abstract "),
        Option.when(sym.isFinalMember)("final ")
      ).flatten.mkString + implicitness

  private def termKeyword(sym: TermSymbol): String =
    if sym.isGivenOrUsing then "given"
    else if sym.isInline && sym.isMethod then "inline def"
    else if sym.isMethod then "def"
    else if sym.isModuleVal then "object"
    else sym.kind match
      case TermSymbolKind.LazyVal => "lazy val"
      case TermSymbolKind.Var     => "var"
      case _                      => "val"

  private def printTopLevelMethodic(
      tpe: TypeOrMethodic,
      paramSymss: List[ParamSymbolsClause],
      javaNames: Option[List[String]],
      defaultValues: Map[String, String],
      elided: Set[String] = Set.empty
  )(using ctx: Context): String =
    tpe match
      case t: Type => s": ${printType(t)}"
      case t: PolyType =>
        val (typeParams, evidence) = printPolyParams(t)
        s"$typeParams${printTopLevelMethodic(t.resultType, paramSymss.drop(1), javaNames, defaultValues, elided ++ evidence)}"
      case t: MethodType => printMethodic(t, paramSymss, javaNames, defaultValues, elided)

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
          case tl: TypeLambda =>
            b.low match
              // `F2[x] >: F[x]`: a lambda lower bound names the parameters it mentions.
              case low: TypeLambda if low.paramNames.sizeIs == tl.paramNames.size =>
                val lowStr = printType(low.resultType)
                if lowStr == "Nothing" then s"$name${printHkParams(tl)}"
                else s"$name${printHkParams(tl, low.paramNames.map(_.toString), lowStr)} >: $lowStr"
              case _ => s"$name${printHkParams(tl)}"
          case _ =>
            val lo = if printType(b.low) == "Nothing" then "" else s" >: ${printType(b.low)}"
            val hi = if printType(b.high) == "Any" then "" else s" <: ${printType(b.high)}"
            s"$name$lo$hi"
      case _ => name.toString

  private def printHkParams(tl: TypeLambda, names: List[String] = Nil, mentionedIn: String = "")(using ctx: Context): String =
    val paramNames = if names.nonEmpty then names else tl.paramNames.map(_.toString)
    val rendered = paramNames.zip(tl.paramTypeBounds).map { (name, bounds) =>
      val (lo, hi) = bounds match
        case b: AbstractTypeBounds =>
          val lo = if printType(b.low) == "Nothing" then "" else s" >: ${printType(b.low)}"
          val hi = if printType(b.high) == "Any" then "" else s" <: ${printType(b.high)}"
          (lo, hi)
        case _ => ("", "")
      (name, lo, hi)
    }
    // A parameter must be named (not `_`) when it is referenced elsewhere in the lambda:
    // in a bound (self-referential, e.g. `A <: Comparable[A]`) or in the constructor
    // bound (the lambda's result, e.g. `F[A] <: Iterable[A]`).
    val resultStr = printType(tl.resultType)
    val context   = (mentionedIn :: resultStr :: rendered.flatMap((_, lo, hi) => List(lo, hi))).mkString(" ")
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

  /** Parents the compilers add that no declaration names: Scala 3's `Mirror` for derivation, and
   *  the `Serializable` Scala 2 gives every object; a case object's stays, as its case class's does.
   */
  private def isCompilerAddedParent(cls: ClassSymbol, parent: Type): Boolean =
    parent match
      case t: TypeRef =>
        def inDeriving(prefix: Prefix) = prefix match
          case p: PackageRef => p.fullyQualifiedName.toString == "scala.deriving"
          case _             => false
        val isMirror = t.prefix match
          case m: TermRef  => m.name.toString == "Mirror" && inDeriving(m.prefix)
          case m: ThisType => m.tref.name.toString == "Mirror$" && inDeriving(m.tref.prefix)
          case _           => false
        val isObjectSerializable =
          cls.isModuleClass && !cls.isCaseClass && t.name.toString == "Serializable" && (t.prefix match
            case p: PackageRef => Set("scala", "java.io").contains(p.fullyQualifiedName.toString)
            case _             => false)
        isMirror || isObjectSerializable
      case _ => false

  /** The supertype every enum gets implicitly, which its declaration never names. */
  private def isEnumBase(parent: Type): Boolean =
    parent match
      case t: AppliedType => isEnumBase(t.tycon)
      case t: TypeRef =>
        t.name.toString == "Enum" && (t.prefix match
          case p: PackageRef => Set("scala.reflect", "java.lang").contains(p.fullyQualifiedName.toString)
          case _             => false)
      case _ => false

  /** Render a class parent, parenthesising function-arrow sugar so it is valid in `extends` position. */
  private[cellar] val universalParents = Set("Object", "Any", "AnyRef")

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

  private[cellar] def isScalaPackage(prefix: Prefix): Boolean =
    prefix match
      case p: PackageRef => p.fullyQualifiedName.toString == "scala"
      case _             => false

  /** A function- or tuple-typed left operand of `=>` must be parenthesised: `(A => B) => C`, `((A, B)) => C`. */
  private def functionArgNeedsParens(tow: TypeOrWildcard): Boolean =
    tow match
      case t: AppliedType => asFunction(t).isDefined || asTuple(t).isDefined
      case _              => false

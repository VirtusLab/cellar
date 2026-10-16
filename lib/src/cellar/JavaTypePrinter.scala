package cellar

import tastyquery.Contexts.Context
import tastyquery.Symbols.{ClassSymbol, Symbol, TermSymbol}
import tastyquery.Types.*

/** Signatures in Java syntax. `public` is left implicit, since `PublicApiFilter` lists only public
  * and protected members.
  */
object JavaTypePrinter:
  def printSymbolSignature(sym: Symbol)(using ctx: Context): String =
    sym match
      // The module class tasty-query makes for a Java class's statics is no Java declaration of its
      // own; the class it belongs to is.
      case cls: ClassSymbol  => printClass(if cls.isModuleClass then cls.companionClass.getOrElse(cls) else cls)
      case term: TermSymbol  => if term.isMethod then printMethod(term) else printField(term)
      case other             => TypePrinter.printScalaSignature(other)

  def printType(tpe: Type)(using ctx: Context): String =
    tpe match
      case t: TypeRef if TypePrinter.isFromJavaObject(t) => "Object"
      case t: TypeRef if TypePrinter.isScalaPackage(t.prefix) && TypePrinter.universalParents.contains(t.name.toString) => "Object"
      case t: TypeRef if TypePrinter.isScalaPackage(t.prefix) && JavaClassfiles.primitives.contains(s"scala.${t.name}") =>
        JavaClassfiles.primitives(s"scala.${t.name}")
      case t: TypeRef => printPrefix(t.prefix).fold(t.name.toString)(p => s"$p.${t.name}")
      case t: AppliedType =>
        t.tycon match
          case tycon: TypeRef if TypePrinter.isScalaPackage(tycon.prefix) && tycon.name.toString == "Array" =>
            s"${printArg(t.args.head)}[]"
          case tycon => s"${printType(tycon)}<${t.args.map(printArg).mkString(", ")}>"
      case t: RepeatedType  => s"${printType(t.elemType)}..."
      case t: AndType       => s"${printType(t.first)} & ${printType(t.second)}"
      case t: TypeParamRef  => t.binder.paramNames(t.paramNum).toString
      case t: AnnotatedType => printType(t.typ)
      case t: FlexibleType  => printType(t.nonNullableType)
      case other            => TypePrinter.printType(other)

  // A static nested class hangs off its outer class's statics, i.e. the module term, so the
  // outer classes of a static class are TermRefs and those of an inner class TypeRefs.
  private def printPrefix(prefix: Prefix)(using Context): Option[String] =
    prefix match
      case p: TypeRef => Some(printType(p))
      case p: TermRef => Some(printPrefix(p.prefix).fold(p.name.toString)(o => s"$o.${p.name}"))
      case _          => None

  private def printArg(arg: TypeOrWildcard)(using Context): String =
    arg match
      case w: WildcardTypeArg =>
        w.bounds match
          case b: AbstractTypeBounds if !isNothing(b.low) => s"? super ${printType(b.low)}"
          case b: AbstractTypeBounds if !isUniversal(b.high) => s"? extends ${printType(b.high)}"
          case _ => "?"
      case t: Type => printType(t)

  private def printClass(cls: ClassSymbol)(using Context): String =
    val parents = cls.parents
    val kind =
      if cls.isTrait && parents.exists(isNamed(_, "java.lang.annotation.Annotation")) then "@interface"
      else if cls.isTrait then "interface"
      else if parents.headOption.exists(isNamed(_, "java.lang.Enum")) then "enum"
      else "class"
    val modifiers = List(
      Option.when(isProtected(cls) || JavaClassfiles.isProtectedNested(cls))("protected"),
      Option.when(isStatic(cls))("static"),
      Option.when(kind == "class" && cls.isAbstractClass)("abstract"),
      Option.when(kind == "class" && cls.openLevel.productPrefix == "Final")("final")
    ).flatten
    val name       = cls.name.toString.stripSuffix("$")
    val typeParams = printTypeParams(cls.typeParams.map(p => (p.name.toString, p.declaredBounds)))
    // A classfile lists its superclass first and its interfaces after, and an interface's
    // superclass is always Object, so position alone separates `extends` from `implements`.
    val superclass = parents.headOption.filterNot(p => isUniversal(p) || isNamed(p, "java.lang.Enum"))
    val interfaces = parents.drop(1).filterNot(isNamed(_, "java.lang.annotation.Annotation"))
    val inheritance =
      if cls.isTrait then clause("extends", interfaces)
      else clause("extends", superclass.toList) + clause("implements", interfaces)
    (modifiers :+ kind :+ s"$name$typeParams$inheritance").mkString(" ")

  private def printMethod(term: TermSymbol)(using Context): String =
    val owner         = term.owner.asInstanceOf[ClassSymbol]
    val isConstructor = term.name.toString == "<init>"
    val modifiers = List(
      Option.when(isProtected(term))("protected"),
      Option.when(isStatic(term))("static"),
      Option.when(term.isAbstractMember && !owner.isTrait)("abstract"),
      Option.when(owner.isTrait && !term.isAbstractMember && !isStatic(term))("default"),
      Option.when(term.isFinalMember)("final")
    ).flatten
    // A constructor also carries its class's type parameters, which Java writes on the class; a
    // generic constructor of a generic class nests its own after them.
    def unwrap(tpe: TypeOrMethodic): (List[(String, TypeBounds)], TypeOrMethodic) =
      tpe match
        case p: PolyType =>
          val (rest, method) = unwrap(p.resultType)
          (p.paramNames.map(_.toString).zip(p.paramTypeBounds) ++ rest, method)
        case other => (Nil, other)
    val (allTypeParams, method) = unwrap(term.declaredType)
    val typeParams = printTypeParams(if isConstructor then allTypeParams.drop(owner.typeParams.size) else allTypeParams)
    method match
      case m: MethodType =>
        val dropped = if JavaClassfiles.leadsWithOuterInstance(term) then 1 else 0
        val types   = m.paramTypes.drop(dropped)
        val names = TypePrinter.javaParamNames(term)
          .map(ns => if ns.length == m.paramTypes.length then ns.drop(dropped) else ns)
          .filter(_.length == types.length)
          .getOrElse(m.paramNames.drop(dropped).map(_.toString))
        // `x$N` is tasty-query's stand-in when no name survived compilation; like javap, print
        // the bare type rather than a name nobody wrote.
        val varargs = isConstructor && JavaClassfiles.isVarargs(term)
        val params = names.zip(types).zipWithIndex.map { case ((n, tpe), i) =>
          val rendered = if varargs && i == types.length - 1 then printVarargs(tpe) else printType(tpe)
          if n.startsWith("x$") then rendered else s"$rendered $n"
        }
        val head =
          if isConstructor then owner.name.toString.stripSuffix("$")
          else s"${returnType(m.resultType)} ${term.name}"
        (modifiers ++ Option.when(typeParams.nonEmpty)(typeParams) :+ s"$head(${params.mkString(", ")})").mkString(" ")
      case _ => TypePrinter.printScalaSignature(term)

  // tasty-query turns a varargs method's last parameter into a RepeatedType, but leaves a
  // constructor's as the array it is in the descriptor.
  private def printVarargs(tpe: Type)(using Context): String =
    tpe match
      case t: AppliedType if t.args.sizeIs == 1 => s"${printArg(t.args.head)}..."
      case t                                     => printType(t)

  private def printField(term: TermSymbol)(using Context): String =
    val modifiers = List(
      Option.when(isProtected(term))("protected"),
      Option.when(isStatic(term))("static"),
      Option.when(term.isFinalMember)("final")
    ).flatten
    (modifiers :+ s"${returnType(term.declaredType)} ${term.name}").mkString(" ")

  private def returnType(tpe: TypeOrMethodic)(using Context): String =
    tpe match
      case t: Type => printType(t)
      case other   => TypePrinter.printMethodic(other)

  private def printTypeParams(params: List[(String, TypeBounds)])(using Context): String =
    if params.isEmpty then ""
    else
      val rendered = params.map {
        case (name, b: AbstractTypeBounds) if !isUniversal(b.high) => s"$name extends ${printType(b.high)}"
        case (name, _)                                              => name
      }
      rendered.mkString("<", ", ", ">")

  private def clause(keyword: String, types: List[Type])(using Context): String =
    if types.isEmpty then "" else types.map(printType).mkString(s" $keyword ", ", ", "")

  // Java statics live on tasty-query's synthetic module class, so the owner is what marks them.
  private def isStatic(sym: Symbol): Boolean =
    sym.owner match
      case c: ClassSymbol => c.isModuleClass
      case _              => false

  private def isProtected(sym: TermSymbol | ClassSymbol): Boolean =
    sym.visibility.productPrefix.contains("Protected")

  private def isUniversal(tpe: Type)(using Context): Boolean =
    printType(tpe) == "Object"

  private def isNothing(tpe: Type)(using Context): Boolean =
    tpe.isInstanceOf[NothingType] || TypePrinter.printType(tpe) == "Nothing"

  // Compared by name, not by resolving the symbol, which throws for a type missing from the classpath.
  private def isNamed(tpe: Type, fqn: String): Boolean =
    val tref = tpe match
      case t: AppliedType => t.tycon
      case t              => t
    tref match
      case t: TypeRef =>
        t.prefix match
          case p: PackageRef => s"${p.fullyQualifiedName}.${t.name}" == fqn
          case _             => false
      case _ => false

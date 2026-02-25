package cellar

import tastyquery.Contexts.Context
import tastyquery.Symbols.{ClassSymbol, Symbol, TermOrTypeSymbol, TermSymbol, TypeSymbol}

object GetFormatter:
  def formatSymbol(sym: Symbol, docstring: Option[String] = None)(using ctx: Context): String =
    val fqn       = sym.displayFullName
    val signature = TypePrinter.printSymbolSignature(sym)
    val flags     = renderFlags(sym)
    val origin    = renderOrigin(sym)
    val members   = renderMembers(sym)
    val companion = renderCompanion(sym)
    val subtypes  = renderSubtypes(sym)

    val sb = new StringBuilder
    sb.append(s"## $fqn\n")
    sb.append(s"```scala\n$signature\n```\n")
    docstring.map(cleanDocstring).filter(_.nonEmpty).foreach { doc =>
      sb.append(s"$doc\n\n")
    }
    if flags.nonEmpty then sb.append(s"**Flags:** $flags\n")
    sb.append(s"**Origin:** $origin\n")
    members.foreach(m => sb.append(s"**Members:**\n```scala\n$m\n```\n"))
    companion.foreach(c => sb.append(s"**Companion members:** $c\n"))
    subtypes.foreach(s => sb.append(s"**Known subtypes:** $s\n"))
    sb.toString

  def formatGetResult(@annotation.unused fqn: String, symbols: List[Symbol], docstring: Option[String] = None)(using ctx: Context): String =
    symbols.zipWithIndex.map { (sym, i) =>
      formatSymbol(sym, if i == 0 then docstring else None)
    }.mkString("\n\n---\n\n")

  private def cleanDocstring(raw: String): String =
    raw.stripPrefix("/**").stripSuffix("*/").trim
      .linesIterator
      .map(_.replaceAll("""^\s*\*\s?""", "").stripTrailing())
      .filter(_.nonEmpty)
      .mkString("\n")
      .trim

  private def renderFlags(sym: Symbol)(using Context): String =
    val flags = sym match
      case cls: ClassSymbol =>
        List(
          if cls.isAbstractClass then Some("abstract") else None,
          if cls.isTrait then None else None, // trait is implied by kind keyword
          cls.openLevel match
            case ol if ol.productPrefix == "Sealed" => Some("sealed")
            case ol if ol.productPrefix == "Final"  => Some("final")
            case ol if ol.productPrefix == "Open"   => Some("open")
            case _                                  => None
          ,
          if cls.isCaseClass then Some("case") else None
        ).flatten
      case term: TermSymbol =>
        List(
          if term.isAbstractMember then Some("abstract") else None,
          if term.isFinalMember then Some("final") else None,
          if term.isInline then Some("inline") else None,
          if term.isGivenOrUsing then Some("given") else None,
          if term.isImplicit then Some("implicit") else None
        ).flatten
      case _: TypeSymbol => Nil
      case _             => Nil
    flags.mkString(", ")

  private def renderOrigin(sym: Symbol): String =
    sym.owner match
      case owner: ClassSymbol => owner.displayFullName
      case _                  => sym.displayFullName

  private def renderMembers(sym: Symbol)(using ctx: Context): Option[String] =
    sym match
      case cls: ClassSymbol =>
        val members = collectClassMembers(cls)
          .filter(PublicApiFilter.isPublic)
          .map(m => TypePrinter.printSymbolSignatureSafe(m).linesIterator.mkString(" ").trim)
        if members.isEmpty then None else Some(members.mkString("\n"))
      case _ => None

  private val universalBaseClasses = Set("scala.Any", "scala.AnyRef", "java.lang.Object")

  private def collectClassMembers(cls: ClassSymbol)(using ctx: Context): List[TermOrTypeSymbol] =
    val seen   = scala.collection.mutable.Set.empty[String]
    val result = List.newBuilder[TermOrTypeSymbol]
    val linearization =
      try cls.linearization
      catch case _: Exception => List(cls)
    for
      klass <- linearization if !universalBaseClasses.contains(klass.displayFullName)
      decl  <-
        try klass.declarations
        catch case _: Exception => Nil
    do
      val key = decl.name.toString
      if !seen.contains(key) then
        seen += key
        result += decl
    result.result()

  private def renderCompanion(sym: Symbol)(using ctx: Context): Option[String] =
    sym match
      case cls: ClassSymbol =>
        cls.companionClass.flatMap { companion =>
          val members = companion.declarations
            .filter(m => PublicApiFilter.isPublic(m))
            .map(_.name.toString)
          if members.isEmpty then None else Some(members.mkString(", "))
        }
      case _ => None

  private def renderSubtypes(sym: Symbol)(using ctx: Context): Option[String] =
    sym match
      case cls: ClassSymbol =>
        val children = cls.sealedChildren
        if children.isEmpty then None
        else Some(children.map(_.displayFullName).mkString(", "))
      case _ => None

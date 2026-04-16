package cellar

import tastyquery.Contexts.Context
import tastyquery.Symbols.{ClassSymbol, Symbol, TermOrTypeSymbol, TermSymbol, TypeSymbol}

object GetFormatter:
  def formatSymbol(
      sym: Symbol,
      docstring: Option[String] = None,
      limit: Option[Int] = None,
      hideInherited: Boolean = false,
      groupInherited: Boolean = false
  )(using ctx: Context): String =
    val fqn       = sym.displayFullName
    val signature = TypePrinter.printSymbolSignature(sym)
    val flags     = renderFlags(sym)
    val origin    = renderOrigin(sym)
    val members   = renderMembers(sym, limit, hideInherited, groupInherited)
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

  def formatGetResult(
      @annotation.unused fqn: String,
      symbols: List[Symbol],
      docstring: Option[String] = None,
      limit: Option[Int] = None,
      hideInherited: Boolean = false,
      groupInherited: Boolean = false
  )(using ctx: Context): String =
    symbols.zipWithIndex.map { (sym, i) =>
      formatSymbol(sym, if i == 0 then docstring else None, limit, hideInherited, groupInherited)
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

  private def renderMembers(
      sym: Symbol,
      limit: Option[Int],
      hideInherited: Boolean,
      groupInherited: Boolean
  )(using ctx: Context): Option[String] =
    sym match
      case cls: ClassSymbol =>
        // --hide-inherited silently wins over --group-inherited
        if hideInherited then renderFlatMembers(cls.declarations.filter(PublicApiFilter.isPublic).toList, limit)
        else if groupInherited then renderGroupedMembers(cls, limit)
        else renderFlatMembers(SymbolResolver.collectClassMembers(cls).filter(PublicApiFilter.isPublic), limit)
      case _ => None

  private def formatMember(m: TermOrTypeSymbol)(using Context): String =
    TypePrinter.printSymbolSignatureSafe(m).linesIterator.mkString(" ").trim

  private def renderFlatMembers(raw: List[TermOrTypeSymbol], limit: Option[Int])(using Context): Option[String] =
    val members = raw.map(formatMember)
    if members.isEmpty then None
    else limit match
      case Some(n) if members.length > n =>
        Some(members.take(n).mkString("\n") + s"\n// … ${members.length - n} more members")
      case _ =>
        Some(members.mkString("\n"))

  private val universalBaseClasses = Set("scala.Any", "scala.AnyRef", "java.lang.Object")

  private def renderGroupedMembers(cls: ClassSymbol, limit: Option[Int])(using ctx: Context): Option[String] =
    val linearization =
      try cls.linearization
      catch case _: Exception => List(cls)
    val seen     = scala.collection.mutable.Set.empty[TermOrTypeSymbol]
    val sections = List.newBuilder[(String, List[String])]
    linearization.filterNot(k => universalBaseClasses.contains(k.displayFullName)).foreach { klass =>
      val decls =
        try klass.declarations
        catch case _: Exception => Nil
      val members = decls.flatMap {
        case decl: TermOrTypeSymbol if PublicApiFilter.isPublic(decl) && !seen.contains(decl) =>
          val dominated = decl.overridingSymbol(cls).exists(_ != decl)
          if !dominated then
            seen += decl
            Some(formatMember(decl))
          else None
        case _ => None
      }.toList
      if members.nonEmpty then
        val label =
          if klass == cls then s"Declared on ${klass.name}"
          else s"Inherited from ${klass.name}"
        sections += ((label, members))
    }
    val all = sections.result()
    if all.isEmpty then None
    else
      val totalMembers = all.map(_._2.length).sum
      val needsTruncation = limit.exists(totalMembers > _)
      val sb = new StringBuilder
      var remaining = limit.getOrElse(Int.MaxValue)
      for (header, members) <- all if remaining > 0 do
        val take = members.take(remaining)
        sb.append(s"// $header\n")
        sb.append(take.mkString("\n")).append('\n')
        remaining -= take.length
      if needsTruncation then
        sb.append(s"// … ${totalMembers - limit.get} more members\n")
      Some(sb.toString.trim)

  private def renderCompanion(sym: Symbol)(using ctx: Context): Option[String] =
    sym match
      case cls: ClassSymbol =>
        cls.companionClass.flatMap { companion =>
          val members = companion.declarations
            .filter(m => PublicApiFilter.isPublic(m))
            .map(m => TypePrinter.printSymbolSignatureSafe(m).linesIterator.mkString(" ").trim)
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

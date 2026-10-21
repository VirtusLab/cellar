package cellar

import tastyquery.Contexts.Context
import tastyquery.Symbols.{ClassSymbol, Symbol, TermOrTypeSymbol}

object GetFormatter:
  def formatSymbol(
      sym: Symbol,
      docstring: Option[String] = None,
      hideInherited: Boolean = false,
      groupInherited: Boolean = false
  )(using ctx: Context): String =
    val fqn       = displayFqn(sym)
    val signature = TypePrinter.printSymbolSignature(sym)
    val origin    = renderOrigin(sym)
    val members   = renderMembers(sym, hideInherited, groupInherited)
    val companion = renderCompanion(sym, groupInherited && !hideInherited)
    val subtypes  = renderSubtypes(sym)
    val supertypes = renderSupertypes(sym)
    val isJava    = TypePrinter.detectLanguage(sym) == DetectedLanguage.Java
    val fence     = if isJava then "java" else "scala"
    // Java statics are what tasty-query models as the companion's members.
    val companionLabel = if isJava then "Static members" else "Companion members"

    val sb = new StringBuilder
    sb.append(s"## $fqn\n")
    sb.append(s"```$fence\n$signature\n```\n")
    // A blank line, so the docstring after it is a paragraph of its own.
    supertypes.foreach(s => sb.append(s"**Supertypes:** $s\n\n"))
    docstring.map(cleanDocstring).filter(_.nonEmpty).foreach { doc =>
      sb.append(s"$doc\n\n")
    }
    sb.append(s"**Origin:** $origin\n")
    members.foreach(m => sb.append(s"**Members:**\n```$fence\n$m\n```\n"))
    companion.foreach(c => sb.append(s"**$companionLabel:**\n```$fence\n$c\n```\n"))
    subtypes.foreach(s => sb.append(s"**Known subtypes:** $s\n"))
    sb.toString

  def formatGetResult(
      @annotation.unused fqn: String,
      symbols: List[Symbol],
      docstring: Option[String] = None,
      hideInherited: Boolean = false,
      groupInherited: Boolean = false
  )(using ctx: Context): String =
    dropRedundantCompanions(symbols).zipWithIndex.map { (sym, i) =>
      formatSymbol(sym, if i == 0 then docstring else None, hideInherited, groupInherited)
    }.mkString("\n\n---\n\n")

  /**
   * Drops a module class whose own companion class is also being rendered.
   *
   * One FQN legitimately resolves to both — for a Java class, tasty-query synthesizes the module
   * that holds the statics. Since every section also prints its companion's members, rendering both
   * symbols emits the whole API twice, mirrored: the statics appear as the class's "Companion
   * members" and again as the module's "Members". The class section alone carries both halves.
   *
   * Only companions of each other are collapsed; an FQN resolving to genuinely unrelated symbols
   * still gets a section each.
   */
  private def dropRedundantCompanions(symbols: List[Symbol])(using ctx: Context): List[Symbol] =
    val classes = symbols.collect { case c: ClassSymbol => c }.toSet
    symbols.filterNot {
      case c: ClassSymbol if c.isModuleClass => c.companionClass.exists(classes.contains)
      case _                                 => false
    }


  private def cleanDocstring(raw: String): String =
    raw.stripPrefix("/**").stripSuffix("*/").trim
      .linesIterator
      .map(_.replaceAll("""^\s*\*\s?""", "").stripTrailing())
      .filter(_.nonEmpty)
      .mkString("\n")
      .trim

  private def renderOrigin(sym: Symbol): String =
    sym.owner match
      case owner: ClassSymbol => displayFqn(owner)
      case _                  => displayFqn(sym)

  private def renderMembers(
      sym: Symbol,
      hideInherited: Boolean,
      groupInherited: Boolean
  )(using ctx: Context): Option[String] =
    sym match
      case cls: ClassSymbol =>
        // --hide-inherited silently wins over --group-inherited
        val syntax = TypePrinter.detectLanguage(cls)
        if hideInherited then renderFlatMembers(cls.declarations.filter(PublicApiFilter.isPublic).toList, syntax)
        else if groupInherited then renderGroupedMembers(cls)
        else renderFlatMembers(SymbolResolver.collectClassMembers(cls).filter(PublicApiFilter.isPublic), syntax)
      case _ => None

  private def formatMember(m: TermOrTypeSymbol, syntax: DetectedLanguage)(using Context): String =
    TypePrinter.printSymbolSignatureSafe(m, syntax).linesIterator.mkString(" ").trim

  private def renderFlatMembers(raw: List[TermOrTypeSymbol], syntax: DetectedLanguage)(using
      Context
  ): Option[String] =
    val members = raw.map(formatMember(_, syntax))
    if members.isEmpty then None
    else Some(members.mkString("\n"))

  private def renderGroupedMembers(cls: ClassSymbol)(using ctx: Context): Option[String] =
    val syntax = TypePrinter.detectLanguage(cls)
    val linearization =
      try cls.linearization
      catch case _: Exception => List(cls)
    val seen     = scala.collection.mutable.Set.empty[TermOrTypeSymbol]
    val sections = List.newBuilder[(String, List[String])]
    linearization.filterNot(k => SymbolResolver.universalBaseClasses.contains(k.displayFullName)).foreach { klass =>
      val decls =
        try klass.declarations
        catch case _: Exception => Nil
      val members = decls.flatMap {
        case decl: TermOrTypeSymbol if PublicApiFilter.isPublic(decl) && !seen.contains(decl) =>
          val dominated = decl.overridingSymbol(cls).exists(_ != decl)
          if !dominated then
            seen += decl
            Some(formatMember(decl, syntax))
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
    else Some(all.map((header, members) => s"// $header\n${members.mkString("\n")}").mkString("\n"))

  private def renderCompanion(sym: Symbol, groupInherited: Boolean)(using
      ctx: Context
  ): Option[String] =
    sym match
      case cls: ClassSymbol =>
        cls.companionClass.flatMap { companion =>
          if groupInherited then renderGroupedMembers(companion)
          else renderFlatMembers(companion.declarations.filter(PublicApiFilter.isPublic).toList, TypePrinter.detectLanguage(companion))
        }
      case _ => None

  /** Every class `sym` inherits from, nearest first, including the parents its signature leaves
   *  out because no declaration names them.
   */
  private def renderSupertypes(sym: Symbol)(using ctx: Context): Option[String] =
    sym match
      case cls: ClassSymbol =>
        val supers =
          try cls.linearization.drop(1)
          catch case _: Exception => Nil
        // tasty-query puts `Matchable` in every linearization, a Java class's too.
        val shown = supers.filterNot(k =>
          SymbolResolver.universalBaseClasses.contains(k.displayFullName) || k.displayFullName == "scala.Matchable")
        Option.when(shown.nonEmpty)(shown.map(displayFqn).mkString(", "))
      case _ => None

  private def renderSubtypes(sym: Symbol)(using ctx: Context): Option[String] =
    sym match
      case cls: ClassSymbol =>
        val children = cls.sealedChildren
        if children.isEmpty then None
        else Some(children.map(displayFqn).mkString(", "))
      case _ => None

  /** `displayFullName` uses the JVM module-class encoding: `Foo$` for an object,
   *  and `Outer$.Inner` for anything nested inside one. Render both at source level.
   *  The trailing marker is stripped only for an actual module class, so a name that
   *  genuinely ends in `$` (a `$` member) is left alone.
   *
   *  Scala 3 top-level defs/types/objects live in a synthetic `<file>$package`
   *  class that is not part of the name a user would write, so that segment is
   *  dropped too: `pkg.Hello$package$.Hello$.fromInt` becomes `pkg.Hello.fromInt`.
   */
  private[cellar] def displayFqn(sym: Symbol): String =
    val fqn = sym.displayFullName.replace("$.", ".")
    val unmangled = sym match
      case cls: ClassSymbol if cls.isModuleClass => fqn.stripSuffix("$")
      case _                                     => fqn
    unmangled.split('.').iterator.filterNot(_.endsWith("$package")).mkString(".")

package cellar.bench

import cellar.ScalaSources
import fs2.io.file.Path

import java.util.zip.ZipFile
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.meta.prettyprinters.XtensionSyntax
import scala.meta.{Ctor, Decl as MDecl, Defn, Init, Mod, Name, Pat, Pkg, Stat, Term, Tree, Type}
import scala.util.Using

/** The expected rendering of every non-private declaration in a Scala sources jar, written as
  * cellar's Scala printer aims to print it, but keeping what the source says where the two could
  * differ (context bounds, `protected`, type member parameters), so those differences surface.
  *
  * Owners are named as tasty-query names classes: `pkg.A`, `pkg.A$` for an object, `pkg.A$.B` for
  * a class inside one, and `pkg.File$package$` for Scala 3 top-level definitions.
  */
final class ScalaSourceDecls(scala2: Boolean):
  private val gaps = mutable.LinkedHashMap.empty[String, Int].withDefaultValue(0)

  def read(sourcesJar: Path): SourceSide =
    val out = List.newBuilder[Decl]
    Using.resource(ZipFile(sourcesJar.toNioPath.toFile)) { zip =>
      zip.entries.asScala.filter(_.getName.endsWith(".scala")).foreach { entry =>
        val text = String(zip.getInputStream(entry).readAllBytes(), "UTF-8")
        ScalaSources.parseSource(text, scala2) match
          case Some(source) =>
            val stem = entry.getName.substring(entry.getName.lastIndexOf('/') + 1).stripSuffix(".scala")
            Visitor(out, stem).stats(Scope("", None), source.stats)
          case None => gaps("files that did not parse") += 1
      }
    }
    SourceSide(out.result(), gaps.toMap)

  private final case class Scope(pkg: String, owner: Option[String])

  private final class Visitor(out: mutable.Builder[Decl, List[Decl]], stem: String):
    def stats(scope: Scope, trees: List[Tree]): Unit = trees.foreach(stat(scope, _))

    private def memberOwner(scope: Scope): String = scope.owner.getOrElse(s"${scope.pkg}$stem$$package$$")
    private def classPath(scope: Scope, name: String): String = scope.owner.fold(s"${scope.pkg}$name")(o => s"$o.$name")

    private def stat(scope: Scope, tree: Tree): Unit =
      tree match
        case p: Pkg        => stats(Scope(s"${scope.pkg}${p.ref.syntax}.", None), p.body.stats)
        case p: Pkg.Object => body(Scope(s"${scope.pkg}${p.name.value}.", Some(s"${scope.pkg}${p.name.value}.package$$")), p.templ.body.stats)
        case c: Defn.Class if visible(c.mods) =>
          val isCase = c.mods.exists(_.isInstanceOf[Mod.Case])
          val path   = classPath(scope, c.name.value)
          header(path, c.mods, "class", c.name.value, c.tparamClause.syntax, c.templ.inits, isCase)
          fields(path, c.ctor, isCase)
          body(scope.copy(owner = Some(path)), c.templ.body.stats)
        case t: Defn.Trait if visible(t.mods) =>
          val path = classPath(scope, t.name.value)
          header(path, t.mods, "trait", t.name.value, t.tparamClause.syntax, t.templ.inits, isCase = false)
          body(scope.copy(owner = Some(path)), t.templ.body.stats)
        case o: Defn.Object if visible(o.mods) =>
          val path = classPath(scope, s"${o.name.value}$$")
          header(path, o.mods, "object", o.name.value, "", o.templ.inits, o.mods.exists(_.isInstanceOf[Mod.Case]))
          body(scope.copy(owner = Some(path)), o.templ.body.stats)
        case e: Defn.Enum if visible(e.mods) =>
          val path = classPath(scope, e.name.value)
          header(path, e.mods, "enum", e.name.value, e.tparamClause.syntax, e.templ.inits, isCase = false)
          body(scope.copy(owner = Some(path)), e.templ.body.stats.filterNot(_.isInstanceOf[Defn.EnumCase]))
        case d: Defn.Def if visible(d.mods)   => method(scope, d.mods, d.name.value, d.paramClauseGroups, d.decltpe, abstractly = false, Nil)
        case d: MDecl.Def if visible(d.mods)  => method(scope, d.mods, d.name.value, d.paramClauseGroups, Some(d.decltpe), abstractly = true, Nil)
        case d: Defn.Macro if visible(d.mods) => method(scope, d.mods, d.name.value, d.paramClauseGroups, d.decltpe, abstractly = false, Nil)
        case e: Defn.ExtensionGroup =>
          val receiver = e.paramClauseGroup.toList.flatMap(_.paramClauses)
          val defs = e.body match
            case d: Defn.Def   => List(d)
            case b: Term.Block => b.stats.collect { case d: Defn.Def => d }
            case _             => Nil
          defs.filter(d => visible(d.mods)).foreach { d =>
            method(scope, d.mods, d.name.value, d.paramClauseGroups, d.decltpe, abstractly = false, receiver)
          }
        case g: Defn.GivenAlias if visible(g.mods) && g.name.value.nonEmpty =>
          given_(scope, g)
        case _: (Defn.GivenAlias | Defn.Given) => gaps("anonymous or template givens (not compared)") += 1
        case v: Defn.Val if visible(v.mods)  => value(scope, v.mods, if v.mods.exists(_.isInstanceOf[Mod.Lazy]) then "lazy val" else "val", v.pats, v.decltpe, abstractly = false)
        case v: Defn.Var if visible(v.mods)  => value(scope, v.mods, "var", v.pats, v.decltpe, abstractly = false)
        case v: MDecl.Val if visible(v.mods) => value(scope, v.mods, "val", v.pats, Some(v.decltpe), abstractly = true)
        case v: MDecl.Var if visible(v.mods) => value(scope, v.mods, "var", v.pats, Some(v.decltpe), abstractly = true)
        case t: Defn.Type if visible(t.mods) =>
          emit(memberOwner(scope), s"type:${t.name.value}", s"${visibility(t.mods)}type ${t.name.value}${t.tparamClause.syntax} = ${t.body.syntax}")
        case t: MDecl.Type if visible(t.mods) =>
          val bounds = t.bounds.syntax
          emit(memberOwner(scope), s"type:${t.name.value}", s"${visibility(t.mods)}type ${t.name.value}${t.tparamClause.syntax}${if bounds.isEmpty then "" else s" $bounds"}")
        case _ => ()

    private def body(scope: Scope, trees: List[Stat]): Unit = stats(scope, trees)

    private def header(path: String, mods: List[Mod], kind: String, name: String, tparams: String, inits: List[Init], isCase: Boolean): Unit =
      val universal = Set("AnyRef", "Any", "Object", "java.lang.Object", "scala.AnyRef", "scala.Any")
      val written   = inits.map(_.tpe).filterNot(t => universal(t.syntax)).map {
        case f: Type.Function => s"(${f.syntax})"
        case t                => t.syntax
      }
      // The compiler mixes these into every case class and case object.
      val parents   = if isCase then written ++ List("Product", "Serializable").filterNot(written.contains) else written
      val modifiers =
        if kind == "object" then if isCase then "case " else ""
        else
          List(
            Option.when(has[Mod.Sealed](mods))("sealed"),
            // A value class is final whether or not it says so.
            Option.when(has[Mod.Final](mods) || written.contains("AnyVal"))("final"),
            Option.when(has[Mod.Abstract](mods) && kind != "trait")("abstract"),
            Option.when(isCase)("case")
          ).flatten.map(_ + " ").mkString
      val extendsStr = if parents.isEmpty then "" else parents.mkString(" extends ", " with ", "")
      emit(path, "class", s"${visibility(mods)}${implicitness(mods)}$modifiers$kind $name$tparams$extendsStr")

    // A case class's first parameter list and `val`/`var` parameters are fields.
    private def fields(path: String, ctor: Ctor.Primary, isCase: Boolean): Unit =
      ctor.paramClauses.toList.zipWithIndex.foreach { (clause, i) =>
        clause.values.foreach { p =>
          val isVar = p.mods.exists(_.isInstanceOf[Mod.VarParam])
          val isVal = p.mods.exists(_.isInstanceOf[Mod.ValParam]) || (isCase && i == 0 && clause.mod.isEmpty)
          if (isVal || isVar) && visible(p.mods) then
            p.decltpe.foreach { tpe =>
              emit(path, s"term:${p.name.value}", s"${visibility(p.mods)}${if isVar then "var" else "val"} ${p.name.value}: ${tpe.syntax}")
            }
        }
      }

    private def method(
        scope: Scope,
        mods: List[Mod],
        name: String,
        groups: List[scala.meta.Member.ParamClauseGroup],
        decltpe: Option[Type],
        abstractly: Boolean,
        receiver: List[Term.ParamClause]
    ): Unit =
      decltpe match
        case None => gaps("declarations with an inferred type (not compared)") += 1
        case Some(tpe) =>
          val tparams = groups.map(_.tparamClause.syntax).mkString
          val clauses = receiver ++ groups.flatMap(_.paramClauses)
          val keyword = if has[Mod.Inline](mods) then "inline def" else "def"
          val text    = s"${termModifiers(mods, abstractly)}$keyword $name$tparams${clauses.map(renderClause).mkString}: ${tpe.syntax}"
          emit(memberOwner(scope), termKey(name, clauses), text)

    private def given_(scope: Scope, g: Defn.GivenAlias): Unit =
      val tparams = g.paramClauseGroups.map(_.tparamClause.syntax).mkString
      val clauses = g.paramClauseGroups.flatMap(_.paramClauses)
      val text    = s"${visibility(g.mods)}given ${g.name.value}$tparams${clauses.map(renderClause).mkString}: ${g.decltpe.syntax}"
      emit(memberOwner(scope), termKey(g.name.value, clauses), text)

    private def value(scope: Scope, mods: List[Mod], keyword: String, pats: List[Pat], decltpe: Option[Type], abstractly: Boolean): Unit =
      (pats, decltpe) match
        case (List(Pat.Var(name)), Some(tpe)) =>
          emit(memberOwner(scope), s"term:${name.value}", s"${termModifiers(mods, abstractly)}$keyword ${name.value}: ${tpe.syntax}")
        case (List(Pat.Var(_)), None) => gaps("declarations with an inferred type (not compared)") += 1
        case _                         => gaps("pattern definitions (not compared)") += 1

    private def renderClause(clause: Term.ParamClause): String =
      // Scala 2 syntax marks a clause implicit on its first parameter rather than on the clause.
      val firstMods = clause.values.headOption.toList.flatMap(_.mods)
      val prefix = clause.mod match
        case Some(_: Mod.Using)                               => "using "
        case Some(_: Mod.Implicit)                            => "implicit "
        case _ if firstMods.exists(_.isInstanceOf[Mod.Implicit]) => "implicit "
        case _ if firstMods.exists(_.isInstanceOf[Mod.Using])    => "using "
        case _                                                => ""
      val params = clause.values.map { p =>
        val tpe     = p.decltpe.map(_.syntax).getOrElse("")
        val default = p.default.map(d => s" = ${printableDefault(d.pos.text)}").getOrElse("")
        if p.name.value.isEmpty then tpe else s"${p.name.value}: $tpe$default"
      }
      params.mkString(s"($prefix", ", ", ")")

    // Mirrors the printer: a default is shown when it is one short line, otherwise as `...`.
    private def printableDefault(expr: String): String =
      val printable = !expr.contains('\n') && !expr.contains("//") && !expr.contains("/*")
      if printable && expr.length <= 40 then expr else "..."

    private def termModifiers(mods: List[Mod], abstractly: Boolean): String =
      visibility(mods) + List(
        Option.when(abstractly)("abstract "),
        Option.when(has[Mod.Final](mods))("final ")
      ).flatten.mkString + implicitness(mods)

    private def implicitness(mods: List[Mod]): String = if has[Mod.Implicit](mods) then "implicit " else ""

    private def visibility(mods: List[Mod]): String =
      mods.collectFirst {
        case Mod.Protected(within) => qualified("protected", within)
        case Mod.Private(within)   => qualified("private", within)
      }.getOrElse("")

    private def qualified(word: String, within: scala.meta.Ref): String =
      within match
        case _: Name.Anonymous => s"$word "
        case n                 => s"$word[${n.syntax}] "

    private def emit(owner: String, key: String, text: String): Unit = out += Decl(owner, key, text): Unit

  /** Pairs by name plus each clause's parameter names; anonymous `using` clauses name nothing. */
  private def termKey(name: String, clauses: List[Term.ParamClause]): String =
    val named = clauses.map(_.values.map(_.name.value).filter(_.nonEmpty)).filter(_.nonEmpty)
    s"term:$name${named.map(_.mkString("(", ",", ")")).mkString}"

  private def has[M <: Mod](mods: List[Mod])(using ct: reflect.ClassTag[M]): Boolean = mods.exists(ct.runtimeClass.isInstance)

  // Plain `private` and `private[this]` members are not API; `private[pkg]` ones are listed by cellar.
  private def visible(mods: List[Mod]): Boolean =
    !mods.exists {
      case Mod.Private(_: Name.Anonymous | _: Term.This) => true
      case _                                              => false
    }

package cellar

import fs2.io.file.Path
import org.objectweb.asm.{ClassReader, ClassVisitor, Opcodes}
import tastyquery.Classpaths.Classpath
import tastyquery.Contexts.Context
import tastyquery.SourceLanguage
import tastyquery.Symbols.{ClassSymbol, PackageSymbol, Symbol, TermOrTypeSymbol, TermSymbol, TypeMemberSymbol}

import java.util.zip.ZipFile
import java.util.{Collections, WeakHashMap}
import scala.collection.concurrent.TrieMap
import scala.jdk.CollectionConverters.*
import scala.meta.inputs.Input
import scala.meta.parsers.Parse
import scala.meta.tokens.Token
import scala.meta.{Decl, Defn, Dialect, Mod, Pat, Pkg, Term, Tree, dialects}
import scala.util.Using
import scala.util.control.NonFatal

/** Default argument values, Scaladoc and source positions read from the `-sources.jar`, for
  * Scala symbols whose binary carries none of them: a Scala 2 pickle has no positions, docs or
  * default expressions, and TASTy has no default expressions either.
  *
  * A Scala source file is found through the classfile's `SourceFile` attribute, which names the
  * file but not its directory, so candidates under the package directory are parsed until one
  * declares the toplevel symbol. Parsed files are cached per [[Context]].
  */
object ScalaSources:
  final case class Location(entry: String, startLine: Int, endLine: Int)

  private final case class Declared(location: Location, doc: Option[String], defaults: Map[String, String])
  private final case class Parsed(entry: String, declared: Map[String, Declared])

  private final class Registered(val classpath: Classpath, val sourceJars: SourceJars):
    val entries  = TrieMap.empty[Path, List[String]]
    val files    = TrieMap.empty[(Path, String), Option[Parsed]]
    val toplevel = TrieMap.empty[String, Option[Parsed]]

  private val registry = Collections.synchronizedMap(new WeakHashMap[Context, Registered])

  def register(ctx: Context, classpath: Classpath, sourceJars: SourceJars): Unit =
    registry.put(ctx, Registered(classpath, sourceJars)): Unit

  /** Default expressions by parameter name, as written in the source. */
  def defaults(method: TermSymbol)(using Context): Map[String, String] =
    declared(method).map(_.defaults).getOrElse(Map.empty)

  def docFor(sym: Symbol)(using Context): Option[String] =
    declared(sym).flatMap(_.doc)

  def location(sym: Symbol)(using Context): Option[Location] =
    declared(sym).map(_.location)

  private def declared(sym: Symbol)(using ctx: Context): Option[Declared] =
    for
      registered <- Option(registry.get(ctx))
      top        <- toplevelClass(sym)
      key        <- symbolKey(sym)
      parsed     <- registered.toplevel.getOrElseUpdate(classPath(top), locate(registered, top))
      found      <- parsed.declared.get(key)
    yield found

  private def toplevelClass(sym: Symbol): Option[ClassSymbol] =
    sym match
      case cls: ClassSymbol if cls.owner.isInstanceOf[PackageSymbol] => Some(cls)
      case s: TermOrTypeSymbol                                       => toplevelClass(s.owner)
      case _                                                         => None

  /** `pkg.Outer$.Inner`: module classes keep their `$`, which is how objects are keyed on the
    * source side too, so a class and its companion never collide.
    */
  private def classPath(cls: ClassSymbol): String =
    cls.owner match
      case pkg: PackageSymbol => s"${pkg.fullName}.${cls.name}"
      case outer: ClassSymbol => s"${classPath(outer)}.${cls.name}"
      case _                  => cls.name.toString

  private def symbolKey(sym: Symbol): Option[String] =
    sym match
      case cls: ClassSymbol => Some(classPath(cls))
      case term: TermSymbol =>
        term.owner match
          case owner: ClassSymbol =>
            val params =
              if term.isMethod then term.paramSymss.collect { case Left(syms) => syms.map(_.name.toString) }.flatten
              else Nil
            Some(memberKey(classPath(owner), term.name.toString, params, term.isMethod))
          case _ => None
      case tm: TypeMemberSymbol =>
        tm.owner match
          case owner: ClassSymbol => Some(memberKey(classPath(owner), tm.name.toString, Nil, false))
          case _                  => None
      case _ => None

  private def memberKey(owner: String, name: String, params: List[String], isMethod: Boolean): String =
    if isMethod then s"$owner#$name(${params.mkString(",")})" else s"$owner#$name"

  private def locate(registered: Registered, top: ClassSymbol): Option[Parsed] =
    for
      jar    <- registered.sourceJars.forSymbol(top)
      pkg     = top.owner.asInstanceOf[PackageSymbol].fullName.toString
      pkgDir  = if pkg.isEmpty then "" else pkg.replace('.', '/') + "/"
      dialect = if top.sourceLanguage == SourceLanguage.Scala2 then dialects.Scala213 else dialects.Scala3
      entries = registered.entries.getOrElseUpdate(jar, listEntries(jar))
      wanted  = classPath(top)
      parsed <- candidates(entries, pkgDir, sourceFileName(registered.classpath, top)).iterator
                  .flatMap(entry => registered.files.getOrElseUpdate((jar, entry), parseSafe(jar, entry, dialect)))
                  .find(_.declared.contains(wanted))
    yield parsed

  /** Files named by the `SourceFile` attribute first, those under the package directory before
    * the rest; without the attribute, every file under the package directory.
    */
  private def candidates(entries: List[String], pkgDir: String, fileName: Option[String]): List[String] =
    val scala = entries.filter(_.endsWith(".scala"))
    val named = fileName.toList.flatMap(name => scala.filter(e => e == name || e.endsWith(s"/$name")))
    val (inPkg, elsewhere) = named.partition(_.startsWith(pkgDir))
    (inPkg ++ elsewhere ++ scala.filter(e => e.startsWith(pkgDir) && !e.drop(pkgDir.length).contains('/'))).distinct

  private def sourceFileName(classpath: Classpath, top: ClassSymbol): Option[String] =
    val binary = s"${top.owner.asInstanceOf[PackageSymbol].fullName}.${top.name.toString.stripSuffix("$")}"
    LazyList(binary, s"$binary$$").flatMap(JavaParamNames.classBytes(classpath, _)).headOption.flatMap { bytes =>
      var name: Option[String] = None
      val visitor = new ClassVisitor(Opcodes.ASM9):
        override def visitSource(source: String, debug: String): Unit = name = Option(source)
      try new ClassReader(bytes).accept(visitor, ClassReader.SKIP_CODE | ClassReader.SKIP_FRAMES)
      catch case NonFatal(_) => ()
      name
    }

  private def listEntries(jar: Path): List[String] =
    try Using.resource(ZipFile(jar.toNioPath.toFile))(_.entries().asScala.filterNot(_.isDirectory).map(_.getName).toList)
    catch case NonFatal(_) => Nil

  // Sources are a best-effort extra; an unreadable jar or unparsable file must only cost what they carry.
  private def parseSafe(jar: Path, entry: String, dialect: Dialect): Option[Parsed] =
    try
      Using.resource(ZipFile(jar.toNioPath.toFile)) { zip =>
        Option(zip.getEntry(entry)).map { e =>
          val text = String(zip.getInputStream(e).readAllBytes(), "UTF-8")
          parse(entry, text, dialect)
        }
      }
    catch case NonFatal(_) => None

  private def parse(entry: String, text: String, dialect: Dialect): Parsed =
    val source   = Parse.parseSource(Input.String(text), dialect).get
    val docs     = source.tokens.collect { case c: Token.Comment if c.text.startsWith("/**") => c }.toList
    val declared = Map.newBuilder[String, Declared]

    // A Scaladoc belongs to the definition that directly follows it, modifiers included.
    def doc(tree: Tree): Option[String] =
      docs.filter(_.end <= tree.pos.start).lastOption
        .filter(c => text.substring(c.end, tree.pos.start).trim.isEmpty)
        .map(_.text)

    def add(key: String, tree: Tree, docOf: Option[String], defaults: Map[String, String]): Unit =
      declared += key -> Declared(Location(entry, tree.pos.startLine, tree.pos.endLine), docOf, defaults)

    def paramNames(clauses: List[Term.ParamClause]): List[String] = clauses.flatMap(_.values).map(_.name.value)
    def paramDefaults(clauses: List[Term.ParamClause]): Map[String, String] =
      clauses.flatMap(_.values).flatMap(p => p.default.map(d => p.name.value -> d.pos.text)).toMap

    def stats(owner: String, trees: List[Tree]): Unit = trees.foreach(stat(owner, _))

    def stat(owner: String, tree: Tree): Unit =
      tree match
        case p: Pkg        => stats(s"$owner${refName(p.ref)}.", p.body.stats)
        case p: Pkg.Object => add(s"$owner${p.name.value}.package$$", p, doc(p), Map.empty); stats(s"$owner${p.name.value}.package$$.", p.templ.body.stats)
        case c: Defn.Class =>
          val path = s"$owner${c.name.value}"
          add(path, c, doc(c), Map.empty)
          val ctorDefaults = paramDefaults(c.ctor.paramClauses.toList)
          add(memberKey(path, "<init>", paramNames(c.ctor.paramClauses.toList), true), c, None, ctorDefaults)
          // the synthetic apply and copy of a case class take the constructor's defaults
          if c.mods.exists(_.isInstanceOf[Mod.Case]) then
            val names = paramNames(c.ctor.paramClauses.toList)
            add(memberKey(s"$path$$", "apply", names, true), c, None, ctorDefaults)
            add(memberKey(path, "copy", names, true), c, None, ctorDefaults)
          stats(s"$path.", c.templ.body.stats)
        case t: Defn.Trait  => add(s"$owner${t.name.value}", t, doc(t), Map.empty); stats(s"$owner${t.name.value}.", t.templ.body.stats)
        case e: Defn.Enum   => add(s"$owner${e.name.value}", e, doc(e), Map.empty); stats(s"$owner${e.name.value}.", e.templ.body.stats)
        case o: Defn.Object => add(s"$owner${o.name.value}$$", o, doc(o), Map.empty); stats(s"$owner${o.name.value}$$.", o.templ.body.stats)
        case d: Defn.Def =>
          val clauses = d.paramClauseGroups.flatMap(_.paramClauses)
          add(memberKey(owner.stripSuffix("."), d.name.value, paramNames(clauses), true), d, doc(d), paramDefaults(clauses))
        case d: Decl.Def =>
          val clauses = d.paramClauseGroups.flatMap(_.paramClauses)
          add(memberKey(owner.stripSuffix("."), d.name.value, paramNames(clauses), true), d, doc(d), paramDefaults(clauses))
        case v: Defn.Val  => patNames(v.pats).foreach(n => add(memberKey(owner.stripSuffix("."), n, Nil, false), v, doc(v), Map.empty))
        case v: Defn.Var  => patNames(v.pats).foreach(n => add(memberKey(owner.stripSuffix("."), n, Nil, false), v, doc(v), Map.empty))
        case v: Decl.Val  => patNames(v.pats).foreach(n => add(memberKey(owner.stripSuffix("."), n, Nil, false), v, doc(v), Map.empty))
        case v: Decl.Var  => patNames(v.pats).foreach(n => add(memberKey(owner.stripSuffix("."), n, Nil, false), v, doc(v), Map.empty))
        case t: Defn.Type => add(memberKey(owner.stripSuffix("."), t.name.value, Nil, false), t, doc(t), Map.empty)
        case t: Decl.Type => add(memberKey(owner.stripSuffix("."), t.name.value, Nil, false), t, doc(t), Map.empty)
        case _            => ()

    stats("", source.stats)
    Parsed(entry, declared.result())

  private def refName(ref: Term.Ref): String =
    ref match
      case Term.Select(qual: Term.Ref, name) => s"${refName(qual)}.${name.value}"
      case name: Term.Name                   => name.value
      case other                             => other.pos.text

  private def patNames(pats: List[Pat]): List[String] =
    def names(tree: Tree): List[String] =
      tree match
        case Pat.Var(name) => List(name.value)
        case other         => other.children.flatMap(names)
    pats.flatMap(names)

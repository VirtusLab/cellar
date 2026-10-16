package cellar

import fs2.io.file.Path
import org.objectweb.asm.{ClassReader, ClassVisitor, Opcodes}
import tastyquery.Classpaths.Classpath
import tastyquery.Contexts.Context
import tastyquery.SourceLanguage
import tastyquery.Symbols.{ClassSymbol, PackageSymbol, Symbol, TermSymbol, TypeMemberSymbol}
import tastyquery.Types.*

import java.util.zip.ZipFile
import java.util.{Collections, WeakHashMap}
import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.meta.inputs.Input
import scala.meta.parsers.Parse
import scala.meta.tokens.Token
import scala.meta.{Ctor, Decl, Defn, Dialect, Mod, Pat, Pkg, Source, Stat, Term, Tree, Type as MType, dialects}
import scala.util.Using
import scala.util.control.NonFatal

/** Default argument values, Scaladoc and source positions of Scala symbols, read from the
  * `-sources.jar` because the binary does not carry them: a Scala 2 pickle has none of the three,
  * and TASTy has no default expressions. Every lookup answers `None` (or no defaults) when the
  * source cannot be found, parsed, or matched to exactly one declaration.
  */
object ScalaSources:
  final case class Location(entry: String, startLine: Int, endLine: Int)

  /** A parameter as both sides can name it: its name and the simple name of its type's head,
    * `""` when that is unknown, which matches anything.
    */
  private final case class Param(name: String, tpe: String)

  private final case class Declared(
      location: Location,
      doc: Option[String],
      params: List[Param],
      defaults: Map[String, String],
      synthetic: Boolean
  )

  /** Classes and objects by path (`pkg.Outer$.Inner`); members by owner path, name and namespace. */
  private final case class Parsed(
      classes: Map[String, Declared],
      members: Map[(String, String, Boolean), List[Declared]]
  )

  private final class Registered(val classpath: Classpath, val sourceJars: SourceJars):
    val entries  = TrieMap.empty[Path, List[String]]
    val files    = TrieMap.empty[(Path, String), Option[Parsed]]
    val toplevel = TrieMap.empty[String, Option[Parsed]]

  private val registry = Collections.synchronizedMap(new WeakHashMap[Context, Registered])

  def register(ctx: Context, classpath: Classpath, sourceJars: SourceJars): Unit =
    registry.put(ctx, Registered(classpath, sourceJars)): Unit

  /** Default expressions by parameter name, as written in the source. */
  def defaults(method: TermSymbol)(using Context): Map[String, String] =
    candidates(method) match
      case List(one) => one.defaults
      // Only one alternative of an overloaded method may declare defaults.
      case many =>
        many.filter(_.defaults.nonEmpty) match
          case List(one) => one.defaults
          case _         => Map.empty

  def docFor(sym: Symbol)(using Context): Option[String] =
    unique(sym).flatMap(_.doc)

  def location(sym: Symbol)(using Context): Option[Location] =
    unique(sym).map(_.location)

  private def unique(sym: Symbol)(using Context): Option[Declared] =
    candidates(sym) match
      case List(one) => Some(one)
      case _         => None

  private def candidates(sym: Symbol)(using ctx: Context): List[Declared] =
    val found =
      for
        registered <- Option(registry.get(ctx))
        top        <- SourceJars.topLevelClass(sym)
        parsed     <- registered.toplevel.getOrElseUpdate(classPath(top), locate(registered, top))
      yield matching(parsed, sym)
    found.getOrElse(Nil)

  private def matching(parsed: Parsed, sym: Symbol)(using Context): List[Declared] =
    sym match
      case cls: ClassSymbol => parsed.classes.get(classPath(cls)).toList
      case term: TermSymbol if term.isModuleVal =>
        term.moduleClass.flatMap(c => parsed.classes.get(classPath(c))).toList
      case term: TermSymbol =>
        term.owner match
          case owner: ClassSymbol =>
            val params  = symbolParams(term)
            val shaped  = parsed.members.getOrElse((classPath(owner), term.name.toString, false), Nil)
                            .filter(d => sameParams(d.params, params))
            val written = shaped.filterNot(_.synthetic)
            if written.nonEmpty then written else shaped
          case _ => Nil
      case tm: TypeMemberSymbol =>
        tm.owner match
          case owner: ClassSymbol => parsed.members.getOrElse((classPath(owner), tm.name.toString, true), Nil)
          case _                  => Nil
      case _ => Nil

  /** `pkg.Outer$.Inner`: module classes keep their `$`, as objects do on the source side. */
  private def classPath(cls: ClassSymbol): String =
    cls.owner match
      case pkg: PackageSymbol =>
        val pkgName = SourceJars.packageName(pkg)
        if pkgName.isEmpty then cls.name.toString else s"$pkgName.${cls.name}"
      case outer: ClassSymbol => s"${classPath(outer)}.${cls.name}"
      case _                  => cls.name.toString

  // Context-bound evidence and anonymous `using` parameters are compiler-named and never written.
  private val CompilerNamed = """(evidence|x)\$\d+""".r

  private def symbolParams(term: TermSymbol): List[Param] =
    term.paramSymss.collect { case Left(syms) => syms }.flatten
      .filterNot(p => p.isSynthetic || CompilerNamed.matches(p.name.toString))
      .map { p =>
        val tpe = p.declaredType match
          case t: Type => simpleName(t)
          case _       => ""
        Param(p.name.toString, tpe)
      }

  private def sameParams(written: List[Param], binary: List[Param]): Boolean =
    written.length == binary.length && written.zip(binary).forall { (w, b) =>
      w.name == b.name && (w.tpe.isEmpty || b.tpe.isEmpty || w.tpe == b.tpe)
    }

  private def simpleName(tpe: Type): String =
    tpe match
      case t: AppliedType =>
        t.tycon match
          case tycon: TypeRef if tycon.name.toString == "<repeated>" =>
            t.args.headOption.collect { case a: Type => simpleName(a) + "*" }.getOrElse("")
          case tycon => simpleName(tycon)
      case t: TypeRef       => t.name.toString
      case t: TypeParamRef  => t.paramName.toString
      case t: ByNameType    => simpleName(t.resultType)
      case t: RepeatedType  => simpleName(t.elemType) + "*"
      case t: AnnotatedType => simpleName(t.typ)
      case _                => ""

  private def writtenName(tpe: MType): String =
    tpe match
      case MType.Name(name)         => name
      case MType.Select(_, name)    => name.value
      case MType.Project(_, name)   => name.value
      case t: MType.Apply           => writtenName(t.tpe)
      case MType.ByName(t)          => writtenName(t)
      case MType.Repeated(t)        => writtenName(t) + "*"
      case MType.Annotate(t, _)     => writtenName(t)
      case MType.Tuple(args)        => s"Tuple${args.size}"
      case f: MType.Function        => s"Function${f.paramClause.values.size}"
      case f: MType.ContextFunction => s"ContextFunction${f.paramClause.values.size}"
      case _                        => ""

  private def locate(registered: Registered, top: ClassSymbol): Option[Parsed] =
    for
      jar    <- registered.sourceJars.forSymbol(top)
      pkg     = top.owner match
                  case p: PackageSymbol => SourceJars.packageName(p)
                  case _                => ""
      pkgDir  = if pkg.isEmpty then "" else pkg.replace('.', '/') + "/"
      scala2  = top.sourceLanguage == SourceLanguage.Scala2
      entries = registered.entries.getOrElseUpdate(jar, listEntries(jar))
      // A companion the compiler synthesized is declared nowhere; its class's file is the answer.
      wanted  = Set(classPath(top), classPath(top).stripSuffix("$"))
      parsed <- candidateFiles(entries, pkgDir, sourceFileName(registered.classpath, top)).iterator
                  .flatMap(entry => registered.files.getOrElseUpdate((jar, entry), parseSafe(jar, entry, scala2)))
                  .find(p => wanted.exists(p.classes.contains))
    yield parsed

  /** Files named by the `SourceFile` attribute, those under the package directory first, then the
    * rest of the package directory, for a jar whose classfiles name a file it does not contain.
    */
  private def candidateFiles(entries: List[String], pkgDir: String, fileName: Option[String]): LazyList[String] =
    val scala = entries.filter(_.endsWith(".scala"))
    val named = fileName.toList.flatMap(name => scala.filter(e => e == name || e.endsWith(s"/$name")))
    val (inPkg, elsewhere) = named.partition(_.startsWith(pkgDir))
    val rest  = LazyList.from(scala).filter(e => e.startsWith(pkgDir) && !e.drop(pkgDir.length).contains('/'))
    (LazyList.from(inPkg ++ elsewhere) ++ rest).distinct

  private def sourceFileName(classpath: Classpath, top: ClassSymbol): Option[String] =
    JavaClassfiles.binaryName(top).flatMap { binary =>
      LazyList(binary, s"$binary$$").flatMap(JavaClassfiles.classBytes(classpath, _)).headOption.flatMap { bytes =>
        var name: Option[String] = None
        val visitor = new ClassVisitor(Opcodes.ASM9):
          override def visitSource(source: String, debug: String): Unit = name = Option(source)
        try new ClassReader(bytes).accept(visitor, ClassReader.SKIP_CODE | ClassReader.SKIP_FRAMES)
        catch case NonFatal(_) => ()
        name
      }
    }

  private def listEntries(jar: Path): List[String] =
    try Using.resource(ZipFile(jar.toNioPath.toFile))(_.entries().asScala.filterNot(_.isDirectory).map(_.getName).toList)
    catch case NonFatal(_) => Nil

  // Sources are a best-effort extra; an unreadable jar or unparsable file must only cost what they carry.
  // A recursive-descent parser can overflow on deeply nested code, which is not `NonFatal`.
  private def parseSafe(jar: Path, entry: String, scala2: Boolean): Option[Parsed] =
    try SourceJars.readText(jar, entry).flatMap(text => parseSource(text, scala2).map(index(entry, _)))
    catch case NonFatal(_) | _: StackOverflowError => None

  /** Parses a source file of the given language, or `None` when no dialect accepts it. */
  private[cellar] def parseSource(text: String, scala2: Boolean): Option[Source] =
    // Scala 2 code may use `-Xsource:3` syntax and Scala 3 code capture checking; the plain
    // dialects are the fallback for what those reject, such as `?` as an identifier.
    val tried: List[Dialect] =
      if scala2 then List(dialects.Scala213Source3, dialects.Scala213) else List(dialects.Scala3Future, dialects.Scala3)
    try tried.iterator.map(d => Parse.parseSource(Input.String(text), d).toOption).collectFirst { case Some(s) => s }
    catch case NonFatal(_) | _: StackOverflowError => None

  private def index(entry: String, source: Source): Parsed =
    val tokens  = source.tokens
    val byStart = mutable.HashMap.empty[Int, Int]
    tokens.indices.foreach(i => byStart.getOrElseUpdate(tokens(i).start, i): Unit)

    val classes = mutable.LinkedHashMap.empty[String, Declared]
    val members = mutable.LinkedHashMap.empty[(String, String, Boolean), List[Declared]]
    val stem    = entry.substring(entry.lastIndexOf('/') + 1).stripSuffix(".scala")

    // A Scaladoc belongs to the definition it precedes, past ordinary comments and whitespace.
    def doc(tree: Tree): Option[String] =
      byStart.get(tree.pos.start).flatMap { start =>
        def skippable(t: Token): Boolean =
          t match
            case c: Token.Comment                          => !c.text.startsWith("/**")
            case _: Token.Trivia | _: Token.Indentation    => true
            case _: Token.BOF                              => true
            case _                                         => false
        (start - 1 to 0 by -1).find(i => !skippable(tokens(i))).map(tokens(_)).collect { case c: Token.Comment => c.text }
      }

    def declared(tree: Tree, docOf: Option[String], params: List[Term.Param], synthetic: Boolean = false): Declared =
      val named = params.filter(_.name.value.nonEmpty)
      Declared(
        Location(entry, tree.pos.startLine, tree.pos.endLine),
        docOf,
        named.map(p => Param(p.name.value, p.decltpe.map(writtenName).getOrElse(""))),
        named.flatMap(p => p.default.map(d => p.name.value -> d.pos.text)).toMap,
        synthetic
      )

    def addMember(owner: String, name: String, isType: Boolean, d: Declared): Unit =
      members.updateWith((owner, name, isType))(old => Some(old.getOrElse(Nil) :+ d)): Unit

    def flatParams(clauses: Seq[Term.ParamClause]): List[Term.Param] = clauses.toList.flatMap(_.values)

    /** `owner` is the enclosing class path, or `None` directly in package `pkg` (a `pkg.` prefix). */
    final case class Scope(pkg: String, owner: Option[String]):
      def classPath(name: String): String = owner.fold(s"$pkg$name")(o => s"$o.$name")
      // Scala 3 top-level definitions belong to the synthetic `File$package` object.
      def memberOwner: String = owner.getOrElse {
        val holder = s"$pkg$stem$$package$$"
        classes.getOrElseUpdate(holder, Declared(Location(entry, 0, Int.MaxValue), None, Nil, Map.empty, true)): Unit
        holder
      }

    def classLike(scope: Scope, tree: Tree, name: String, ctor: Option[Ctor.Primary], isCase: Boolean, body: List[Stat]): Unit =
      val path = scope.classPath(name)
      classes(path) = declared(tree, doc(tree), Nil)
      ctor.foreach { c =>
        val clauses = c.paramClauses.toList
        addMember(path, "<init>", false, declared(tree, None, flatParams(clauses)))
        clauses.zipWithIndex.foreach { (clause, i) =>
          clause.values.foreach { p =>
            val isField = (isCase && i == 0) || p.mods.exists(m => m.isInstanceOf[Mod.ValParam] || m.isInstanceOf[Mod.VarParam])
            if isField then addMember(path, p.name.value, false, declared(p, doc(p), Nil))
          }
        }
        // The compiler's `apply` takes the constructor's defaults; a written one wins in `matching`.
        if isCase then addMember(s"$path$$", "apply", false, declared(tree, None, flatParams(clauses), synthetic = true))
      }
      stats(Scope(scope.pkg, Some(path)), body)

    def method(scope: Scope, tree: Tree, name: String, params: List[Term.Param]): Unit =
      addMember(scope.memberOwner, name, false, declared(tree, doc(tree), params))

    def value(scope: Scope, tree: Tree, pats: List[Pat]): Unit =
      patNames(pats).foreach(n => addMember(scope.memberOwner, n, false, declared(tree, doc(tree), Nil)))

    def enumCases(scope: Scope, e: Defn.Enum): Unit =
      val companion = Scope(scope.pkg, Some(scope.classPath(s"${e.name.value}$$")))
      e.templ.body.stats.foreach {
        case c: Defn.EnumCase if c.ctor.paramClauses.nonEmpty =>
          classLike(companion, c, c.name.value, Some(c.ctor), true, Nil)
        case c: Defn.EnumCase =>
          addMember(companion.memberOwner, c.name.value, false, declared(c, doc(c), Nil))
        case c: Defn.RepeatedEnumCase =>
          c.cases.foreach(n => addMember(companion.memberOwner, n.value, false, declared(c, doc(c), Nil)))
        case _ => ()
      }

    def stats(scope: Scope, trees: List[Tree]): Unit = trees.foreach(stat(scope, _))

    def stat(scope: Scope, tree: Tree): Unit =
      tree match
        case p: Pkg => stats(Scope(s"${scope.pkg}${refName(p.ref)}.", None), p.body.stats)
        case p: Pkg.Object =>
          classLike(Scope(s"${scope.pkg}${p.name.value}.", None), p, "package$", None, false, p.templ.body.stats)
        case c: Defn.Class =>
          classLike(scope, c, c.name.value, Some(c.ctor), c.mods.exists(_.isInstanceOf[Mod.Case]), c.templ.body.stats)
        case t: Defn.Trait  => classLike(scope, t, t.name.value, Some(t.ctor), false, t.templ.body.stats)
        case o: Defn.Object => classLike(scope, o, s"${o.name.value}$$", None, false, o.templ.body.stats)
        case e: Defn.Enum =>
          classLike(scope, e, e.name.value, Some(e.ctor), false, e.templ.body.stats.filterNot(isEnumCase))
          enumCases(scope, e)
        case d: Defn.Def   => method(scope, d, d.name.value, flatParams(d.paramClauseGroups.flatMap(_.paramClauses)))
        case d: Decl.Def   => method(scope, d, d.name.value, flatParams(d.paramClauseGroups.flatMap(_.paramClauses)))
        case d: Defn.Macro => method(scope, d, d.name.value, flatParams(d.paramClauseGroups.flatMap(_.paramClauses)))
        case c: Ctor.Secondary =>
          scope.owner.foreach(o => addMember(o, "<init>", false, declared(c, doc(c), flatParams(c.paramClauses))))
        case e: Defn.ExtensionGroup =>
          val receiver = flatParams(e.paramClauseGroup.toList.flatMap(_.paramClauses))
          extensionDefs(e.body).foreach { d =>
            method(scope, d, d.name.value, receiver ++ flatParams(d.paramClauseGroups.flatMap(_.paramClauses)))
          }
        case g: Defn.GivenAlias if g.name.value.nonEmpty =>
          method(scope, g, g.name.value, flatParams(g.paramClauseGroups.flatMap(_.paramClauses)))
        case v: Defn.Val  => value(scope, v, v.pats)
        case v: Defn.Var  => value(scope, v, v.pats)
        case v: Decl.Val  => value(scope, v, v.pats)
        case v: Decl.Var  => value(scope, v, v.pats)
        case t: Defn.Type => addMember(scope.memberOwner, t.name.value, true, declared(t, doc(t), Nil))
        case t: Decl.Type => addMember(scope.memberOwner, t.name.value, true, declared(t, doc(t), Nil))
        case _            => ()

    stats(Scope("", None), source.stats)
    Parsed(classes.toMap, members.toMap)

  private def isEnumCase(tree: Tree): Boolean =
    tree.isInstanceOf[Defn.EnumCase] || tree.isInstanceOf[Defn.RepeatedEnumCase]

  private def extensionDefs(body: Tree): List[Defn.Def] =
    body match
      case d: Defn.Def   => List(d)
      case b: Term.Block => b.stats.collect { case d: Defn.Def => d }
      case _             => Nil

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

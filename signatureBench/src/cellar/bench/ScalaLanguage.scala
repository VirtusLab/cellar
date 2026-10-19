package cellar.bench

import cellar.{DetectedLanguage, PublicApiFilter, TypePrinter}
import fs2.io.file.Path
import tastyquery.Contexts.Context
import tastyquery.Modifiers.TermSymbolKind
import tastyquery.Symbols.{ClassSymbol, PackageSymbol, TermOrTypeSymbol, TermSymbol, TypeMemberSymbol}

import scala.util.Try

/** Scala 2 and Scala 3 differ only in the dialect their sources are parsed with. */
final class ScalaLanguage(val name: String, scala2: Boolean, val defaultLibraries: List[String]) extends Language:
  def sourceDecls(sourcesJar: Path): SourceSide = ScalaSourceDecls(scala2).read(sourcesJar)

  def printedDecls(owners: Set[String])(using ctx: Context): List[Decl] =
    val classes = owners.flatMap(packageOf).flatMap(pkg => nestedClasses(pkg.declarations)).map(c => classPath(c) -> c).toMap
    owners.toList.flatMap { owner =>
      classes.get(owner).toList.flatMap { cls =>
        val vars    = cls.declarations.collect { case t: TermSymbol if t.kind == TermSymbolKind.Var => t.name.toString }.toSet
        val members = cls.declarations.collect {
          case t: TermSymbol if PublicApiFilter.isPublic(t) && !t.isModuleVal && !isAccessorNoise(t, vars) =>
            Decl(owner, termKey(t), print(t))
          case t: TypeMemberSymbol if PublicApiFilter.isPublic(t) =>
            Decl(owner, s"type:${t.name}", print(t))
        }
        Decl(owner, "class", print(cls)) :: members
      }
    }

  override def isGenerated(decl: Decl): Boolean =
    val name = decl.key.stripPrefix("term:").takeWhile(_ != '(')
    // Anonymous givens are left out on the source side, so their compiler names are not extra.
    ScalaLanguage.generated(name) || ScalaLanguage.productElement.matches(name) || name.startsWith("given_")

  private def print(sym: TermOrTypeSymbol)(using Context): String =
    TypePrinter.printSymbolSignatureSafe(sym, DetectedLanguage.Scala3)

  // A constructor is the class's parameters, and a `var`'s setter is the `var` itself; a setter
  // the source writes as a `def` has no `var` beside it, so it stays.
  private def isAccessorNoise(t: TermSymbol, vars: Set[String]): Boolean =
    val name = t.name.toString
    name == "<init>" || (name.endsWith("_=") && vars(name.stripSuffix("_=")))

  private def termKey(t: TermSymbol): String =
    // Context-bound evidence is compiler-named, and may share a clause with written parameters.
    val named = t.paramSymss.collect { case Left(params) => params.map(_.name.toString).filterNot(ScalaLanguage.compilerNamed.matches) }
      .filter(_.nonEmpty)
    s"term:${t.name}${named.map(_.mkString("(", ",", ")")).mkString}"

  /** The longest dotted prefix of `owner` that names a package. */
  private def packageOf(owner: String)(using ctx: Context): Option[PackageSymbol] =
    val segments = owner.split('.').toList
    (segments.length - 1 to 1 by -1).iterator
      .flatMap(n => Try(ctx.findPackage(segments.take(n).mkString("."))).toOption)
      .nextOption()

  private def nestedClasses(decls: Iterable[tastyquery.Symbols.Symbol])(using Context): Iterable[ClassSymbol] =
    decls.collect { case c: ClassSymbol => c }.flatMap(c => c +: nestedClasses(Try(c.declarations).getOrElse(Nil)).toSeq)

  /** `pkg.Outer$.Inner`, as `ScalaSourceDecls` names owners. */
  private def classPath(cls: ClassSymbol): String =
    cls.owner match
      case pkg: PackageSymbol => s"${pkg.fullName}.${cls.name}"
      case outer: ClassSymbol => s"${classPath(outer)}.${cls.name}"
      case _                  => cls.name.toString

  private val qualifier      = """\b_root_\.|\b[A-Za-z][\w$]*\.(?=[A-Za-z_])(?!type\b)""".r
  // The names the compilers give placeholder parameters: `_$1` (Scala 3), `β$0$` (kind-projector).
  private val placeholderName = """_\$\d+|[^\[\]\s,]+\$\d+\$"""
  private val placeholderLam  = s"""\\[((?:$placeholderName)(?:, (?:$placeholderName))*)\\] =>> """.r
  private val placeholder    = """(?<=[\[( ,])[*?](?=[\],)])""".r

  // Spellings of one type that differ only by syntax: kind-projector's `λ[α => F[α]]` as
  // `[α] =>> F[α]`; placeholders (`F[A, *]`, `F[A, ?]`, and the printer's `[_$1] =>> F[A, _$1]`)
  // as `F[A, _]`, which also makes `?` and `_` wildcards alike; `with` as `&`; and qualifiers,
  // since imports decide how much of a name the source spells out.
  def normalize(text: String): String =
    val unlambda = contractPlaceholders(kindProjectorAsLambda(text))
    val unqualified = qualifier.replaceAllIn(unlambda, "")
    placeholder.replaceAllIn(unqualified, "_").replace(" with ", " & ").replaceAll("\\s+", " ").trim

  /** `[_$1] =>> F[A, _$1]` as `F[A, _]`: the lambda's body runs to the first unbalanced `]` or `,`. */
  private def contractPlaceholders(text: String): String =
    placeholderLam.findFirstMatchIn(text) match
      case None => text
      case Some(m) =>
        val from  = m.end
        var depth = 0
        var end   = from
        while end < text.length && !(depth == 0 && (text(end) == ']' || text(end) == ',')) do
          if "[(".contains(text(end)) then depth += 1
          else if "])".contains(text(end)) then depth -= 1
          end += 1
        val body = m.group(1).split(", ").foldLeft(text.substring(from, end))(_.replace(_, "_"))
        contractPlaceholders(text.substring(0, m.start) + body + text.substring(end))

  private def kindProjectorAsLambda(text: String): String =
    val start = text.indexOf("λ[")
    if start < 0 then text
    else
      val open  = start + 1
      val close = (open + 1 until text.length).scanLeft((open, 1)) { case ((_, depth), i) =>
        (i, depth + (if text(i) == '[' then 1 else if text(i) == ']' then -1 else 0))
      }.collectFirst { case (i, 0) => i }
      close match
        case None => text
        case Some(end) =>
          val inner        = text.substring(open + 1, end)
          val arrow        = inner.indexOf(" => ")
          if arrow < 0 then text
          else
            val params = inner.substring(0, arrow).stripPrefix("(").stripSuffix(")")
            val body   = inner.substring(arrow + 4)
            kindProjectorAsLambda(s"${text.substring(0, start)}[$params] =>> $body${text.substring(end + 1)}")

  private val modifiers   = Set("protected", "private", "abstract", "final", "implicit", "sealed", "case", "lazy", "inline", "opaque")
  private val kinds       = Set("class", "trait", "object", "enum")
  private val contextBound = """\[[^\]]*:\s*\w""".r
  private val defaults    = """(?<=\([^()]{0,200}) = [^,)]*""".r

  def classify(expected: String, actual: String): String =
    def words(s: String)       = s.split(' ').toList
    def withoutMods(s: String) = words(s).filterNot(w => modifiers(w.takeWhile(_ != '['))).mkString(" ")
    if contextBound.findFirstIn(expected).isDefined && (actual.contains("evidence$") || actual.contains("x$")) then "context bounds"
    else if withoutMods(expected) == withoutMods(actual) then "modifiers"
    else if defaults.replaceAllIn(expected, "") == defaults.replaceAllIn(actual, "") then "default values"
    else if actual.contains("=>>") && !expected.contains("=>>") then "type lambdas"
    else if words(expected).exists(kinds) then "class declaration"
    else "types"

object ScalaLanguage:
  private[bench] val compilerNamed = """(evidence|x)\$\d+""".r

  // Members the compiler writes for case classes, enums and objects, which no source declares.
  private val generated = Set(
    "copy", "productArity", "productPrefix", "productElement", "productElementName", "productElementNames",
    "productIterator", "canEqual", "equals", "hashCode", "toString", "apply", "unapply", "unapplySeq",
    "fromProduct", "ordinal", "values", "valueOf", "fromOrdinal", "writeReplace", "readResolve"
  )
  private val productElement = """_\d+""".r

  val scala3 = ScalaLanguage(
    "Scala3",
    scala2 = false,
    List(
      "org.typelevel:cats-core_3:2.12.0",
      "co.fs2:fs2-core_3:3.11.0",
      "io.circe:circe-core_3:0.14.10",
      "dev.zio:zio_3:2.1.14"
    )
  )

  val scala2 = ScalaLanguage(
    "Scala2",
    scala2 = true,
    List(
      "org.typelevel:cats-core_2.13:2.12.0",
      "co.fs2:fs2-core_2.13:3.11.0",
      "io.circe:circe-core_2.13:0.14.10",
      "dev.zio:zio_2.13:2.1.14"
    )
  )

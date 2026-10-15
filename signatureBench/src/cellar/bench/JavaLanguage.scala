package cellar.bench

import cellar.{DetectedLanguage, JavaClassfiles, PublicApiFilter, TypePrinter}
import fs2.io.file.Path
import tastyquery.Contexts.Context
import tastyquery.Symbols.{ClassSymbol, TermSymbol}

import scala.util.Try

object JavaLanguage extends Language:
  val name = "Java"

  val defaultLibraries = List(
    "com.google.guava:guava:33.3.1-jre",
    "org.apache.commons:commons-lang3:3.17.0",
    "com.fasterxml.jackson.core:jackson-databind:2.18.2",
    "io.netty:netty-buffer:4.1.115.Final",
    "org.junit.jupiter:junit-jupiter-api:5.11.4",
    "org.slf4j:slf4j-api:2.0.16"
  )

  def sourceDecls(sourcesJar: Path): SourceSide = JavaSourceDecls.read(sourcesJar)

  def printedDecls(owners: Set[String])(using ctx: Context): List[Decl] =
    owners.toList.flatMap { owner =>
      findClass(owner).toList.flatMap { cls =>
        val statics = cls.companionClass.toList
        val members = (cls :: statics).flatMap(c => c.declarations.collect {
          case t: TermSymbol if PublicApiFilter.isPublic(t) => (c, t)
        })
        Decl(owner, "type", print(cls)) :: members.map((c, t) => Decl(owner, key(t, c), print(t)))
      }
    }

  // Static nested classes sit on the outer class's module class and inner ones on the class itself,
  // so each `$` step looks in both.
  private def findClass(binary: String)(using ctx: Context): Option[ClassSymbol] =
    val top :: nested = binary.split('$').toList: @unchecked
    nested.foldLeft(Try(ctx.findStaticClass(top)).toOption) { (outer, name) =>
      outer.flatMap { cls =>
        (cls :: cls.companionClass.toList).flatMap(_.declarations).collectFirst {
          case c: ClassSymbol if c.name.toString == name => c
        }
      }
    }

  private def print(sym: ClassSymbol | TermSymbol)(using Context): String =
    TypePrinter.printSymbolSignatureSafe(sym, DetectedLanguage.Java)

  private def key(term: TermSymbol, owner: ClassSymbol)(using Context): String =
    if term.isMethod then
      val all    = JavaClassfiles.erasedParams(term, owner).getOrElse(Nil).map(simpleName)
      val erased = if JavaClassfiles.leadsWithOuterInstance(term) then all.drop(1) else all
      s"method:${term.name}(${erased.mkString(",")})"
    else s"field:${term.name}"

  private def simpleName(binary: String): String =
    binary.substring(math.max(binary.lastIndexOf('.'), binary.lastIndexOf('$')) + 1)

  // Source may qualify a type (`java.util.List`, `Map.Entry`) wherever the printer does not, and
  // the reverse; which outer classes are spelled out is a style choice, not a signature difference.
  private val qualifier = """\b[\w$]+\.(?=[A-Za-z_$])""".r

  def normalize(text: String): String =
    // JavaParser prints type arguments as `Map<K,V>`, and `? extends Object` is `?` written longhand.
    qualifier.replaceAllIn(text, "").replaceAll(",(?=\\S)", ", ").replace("? extends Object>", "?>")
      .replace("? extends Object,", "?,").replaceAll("\\s+", " ").trim

  private val modifiers  = Set("protected", "static", "abstract", "default", "final", "sealed", "non-sealed")
  private val kinds      = Set("class", "interface", "enum", "@interface", "record")
  private val paramNames = """\s[\w$]+(?=[,)])""".r

  def classify(expected: String, actual: String): String =
    def words(s: String)      = s.split(' ').toList
    def withoutMods(s: String) = words(s).filterNot(modifiers).mkString(" ")
    def withoutNames(s: String) = paramNames.replaceAllIn(s, "")
    if withoutMods(expected) == withoutMods(actual) then "modifiers"
    else if withoutNames(expected) == withoutNames(actual) then "parameter names"
    else if words(expected).exists(kinds) then "type declaration"
    else "types"

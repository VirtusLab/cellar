package cellar

import com.github.javaparser.ParserConfiguration.LanguageLevel
import com.github.javaparser.ast.`type`.{ArrayType, ClassOrInterfaceType, PrimitiveType, Type, TypeParameter}
import com.github.javaparser.ast.body.{ConstructorDeclaration, MethodDeclaration, Parameter, TypeDeclaration}
import com.github.javaparser.ast.nodeTypes.{NodeWithJavadoc, NodeWithTypeParameters}
import com.github.javaparser.{JavaParser, ParserConfiguration}
import fs2.io.file.Path
import tastyquery.Contexts.Context
import tastyquery.Symbols.{ClassSymbol, Symbol, TermSymbol}

import java.util.zip.ZipFile
import java.util.{Collections, WeakHashMap}
import scala.collection.concurrent.TrieMap
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.util.Using
import scala.util.control.NonFatal

/** Parameter names and Javadoc read from the `-sources.jar`, for Java members whose classfile
  * carries neither: abstract and interface methods have no `LocalVariableTable`, and jars built
  * without `-g` (sbt's default) have none at all. The source is only parsed, never resolved
  * (javac itself cannot run inside the native image), so a member is matched by name plus the
  * simple names of its erased parameter types.
  */
object JavaSources:
  private final case class Member(paramNames: List[String], doc: Option[String])
  private final case class Parsed(classDocs: Map[String, String], members: Map[(String, String, List[String]), Member])

  private final class Registered(val sourceJars: SourceJars):
    val files = TrieMap.empty[String, Option[Parsed]]

  private val registry = Collections.synchronizedMap(new WeakHashMap[Context, Registered])

  def register(ctx: Context, sourceJars: SourceJars): Unit =
    registry.put(ctx, Registered(sourceJars)): Unit

  def paramNames(method: TermSymbol)(using ctx: Context): Option[List[String]] =
    member(method).map(_.paramNames)

  def docFor(sym: Symbol)(using ctx: Context): Option[String] =
    sym match
      case cls: ClassSymbol =>
        for
          binary <- JavaParamNames.binaryName(cls)
          parsed <- parsedFor(cls, binary)
          doc    <- parsed.classDocs.get(binary)
        yield doc
      case term: TermSymbol if term.isMethod => member(term).flatMap(_.doc)
      case _                                 => None

  private def member(method: TermSymbol)(using ctx: Context): Option[Member] =
    for
      owner  <- method.owner match
                  case c: ClassSymbol => Some(c)
                  case _              => None
      binary <- JavaParamNames.binaryName(owner)
      parsed <- parsedFor(owner, binary)
      erased <- JavaParamNames.erasedParams(method, owner)
      member <- parsed.members.get((binary, method.name.toString, erased.map(simpleName)))
    yield member

  private def parsedFor(cls: ClassSymbol, binary: String)(using ctx: Context): Option[Parsed] =
    for
      registered <- Option(registry.get(ctx))
      jar        <- registered.sourceJars.forSymbol(cls)
      parsed     <- registered.files.getOrElseUpdate(binary, parseSafe(jar, binary))
    yield parsed

  /** The source file of the toplevel class, `pkg/Top.java` for a member of `pkg.Top$Inner`. */
  private def sourcePath(binary: String): String =
    val dot = binary.lastIndexOf('.')
    val top = binary.substring(dot + 1).takeWhile(_ != '$')
    val pkg = if dot < 0 then "" else binary.substring(0, dot).replace('.', '/') + "/"
    s"$pkg$top.java"

  // Sources are a best-effort extra; a jar or file javac cannot parse must only cost names and docs.
  private def parseSafe(jar: Path, binary: String): Option[Parsed] =
    try
      Using.resource(ZipFile(jar.toNioPath.toFile)) { zip =>
        Option(zip.getEntry(sourcePath(binary))).map { entry =>
          val text = String(zip.getInputStream(entry).readAllBytes(), "UTF-8")
          parse(binary.take(binary.lastIndexOf('.') + 1), text)
        }
      }
    catch case NonFatal(_) => None

  private def parse(pkgPrefix: String, text: String): Parsed =
    val config = ParserConfiguration().setLanguageLevel(LanguageLevel.BLEEDING_EDGE)
    val unit   = JavaParser(config).parse(text).getResult.get

    val classDocs = Map.newBuilder[String, String]
    val members   = Map.newBuilder[(String, String, List[String]), Member]

    def doc(node: NodeWithJavadoc[?]): Option[String] =
      node.getJavadocComment.map(_.getContent).toScala

    // Members of local and anonymous classes are not reached: only type declarations nested
    // directly in a type are walked, and `<init>` is what tasty-query names a constructor.
    def visit(decl: TypeDeclaration[?], binary: String, bounds: Map[String, String]): Unit =
      doc(decl).foreach(classDocs += binary -> _)
      val own = bounds ++ typeVarBounds(decl)
      decl.getMembers.asScala.foreach {
        case m: MethodDeclaration      => add(binary, m.getNameAsString, m.getParameters.asScala.toList, own ++ typeVarBounds(m), doc(m))
        case c: ConstructorDeclaration => add(binary, "<init>", c.getParameters.asScala.toList, own ++ typeVarBounds(c), doc(c))
        case t: TypeDeclaration[?]     => visit(t, s"$binary$$${t.getNameAsString}", own)
        case _                         => ()
      }

    def add(binary: String, name: String, params: List[Parameter], bounds: Map[String, String], doc: Option[String]): Unit =
      val erased = params.map(p => erase(p.getType, bounds) + (if p.isVarArgs then "[]" else ""))
      members += (binary, name, erased) -> Member(params.map(_.getNameAsString), doc)

    unit.getTypes.asScala.foreach(t => visit(t, s"$pkgPrefix${t.getNameAsString}", Map.empty))
    Parsed(classDocs.result(), members.result())

  // Enums and annotations declare no type parameters, and are not `NodeWithTypeParameters`.
  private def typeVarBounds(node: Any): Map[String, String] =
    val params = node match
      case d: NodeWithTypeParameters[?] => d.getTypeParameters.asScala.toList
      case _                            => Nil
    params.map { (p: TypeParameter) =>
      p.getNameAsString -> p.getTypeBound.asScala.headOption.map(_.getNameAsString).getOrElse("Object")
    }.toMap

  /** Simple name of a parameter's erasure as written in source, `T extends Foo<T>` → `Foo`. */
  private def erase(tpe: Type, bounds: Map[String, String]): String =
    tpe match
      case t: PrimitiveType        => t.asString
      case t: ArrayType            => s"${erase(t.getComponentType, bounds)}[]"
      case t: ClassOrInterfaceType => bounds.getOrElse(t.getNameAsString, t.getNameAsString)
      case other                   => other.asString

  private def simpleName(erased: String): String =
    val suffix = erased.takeRight(erased.length - erased.replace("[]", "").length)
    val base   = erased.dropRight(suffix.length)
    base.substring(math.max(base.lastIndexOf('.'), base.lastIndexOf('$')) + 1) + suffix

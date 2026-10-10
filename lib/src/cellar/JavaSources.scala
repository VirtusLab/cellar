package cellar

import com.sun.source.tree.*
import com.sun.source.util.{JavacTask, TreePath, TreePathScanner, Trees}
import fs2.io.file.Path
import tastyquery.Contexts.Context
import tastyquery.Symbols.{ClassSymbol, Symbol, TermSymbol}

import java.net.URI
import java.util.zip.ZipFile
import java.util.{Collections, WeakHashMap}
import javax.tools.{SimpleJavaFileObject, ToolProvider}
import scala.collection.concurrent.TrieMap
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** Parameter names and Javadoc read from the `-sources.jar`, for Java members whose classfile
  * carries neither: abstract and interface methods have no `LocalVariableTable`, and jars built
  * without `-g` (sbt's default) have none at all. The source is only parsed, never attributed,
  * so a member is matched by name plus the simple names of its erased parameter types.
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
    catch case _: Exception => None

  private def parse(pkgPrefix: String, text: String): Parsed =
    val compiler = ToolProvider.getSystemJavaCompiler
    val file = new SimpleJavaFileObject(URI.create("string:///Source.java"), javax.tools.JavaFileObject.Kind.SOURCE):
      override def getCharContent(ignoreEncodingErrors: Boolean): CharSequence = text
    val task  = compiler.getTask(null, null, null, null, null, List(file).asJava).asInstanceOf[JavacTask]
    val trees = Trees.instance(task)
    val units = task.parse().asScala

    val classDocs = Map.newBuilder[String, String]
    val members   = Map.newBuilder[(String, String, List[String]), Member]
    val scanner = new TreePathScanner[Unit, (String, Map[String, String])]:
      override def visitClass(cls: ClassTree, scope: (String, Map[String, String])): Unit =
        val (outer, bounds) = scope
        val binary          = if outer.isEmpty then s"$pkgPrefix${cls.getSimpleName}" else s"$outer$$${cls.getSimpleName}"
        doc(trees).foreach(classDocs += binary -> _)
        super.visitClass(cls, (binary, bounds ++ typeVarBounds(cls.getTypeParameters.asScala.toList)))

      override def visitMethod(method: MethodTree, scope: (String, Map[String, String])): Unit =
        val (binary, bounds) = scope
        val own    = bounds ++ typeVarBounds(method.getTypeParameters.asScala.toList)
        val params = method.getParameters.asScala.toList
        val key    = (binary, method.getName.toString, params.map(p => erase(p.getType, own)))
        members += key -> Member(params.map(_.getName.toString), doc(trees))

      // Local and anonymous classes are not API, and their members would shadow the enclosing
      // class's under the same binary name.
      // javac keeps one space after each stripped `*`, which would otherwise indent every line but the first
      private def doc(trees: Trees): Option[String] =
        Option(trees.getDocComment(getCurrentPath)).map(_.replaceAll("(?m)^ ", ""))

      override def visitBlock(block: BlockTree, scope: (String, Map[String, String])): Unit = ()

    units.foreach(unit => scanner.scan(new TreePath(unit), ("", Map.empty)))
    Parsed(classDocs.result(), members.result())

  private def typeVarBounds(params: List[TypeParameterTree]): Map[String, String] =
    params.map { p =>
      p.getName.toString -> p.getBounds.asScala.headOption.map(eraseRaw).getOrElse("Object")
    }.toMap

  /** Simple name of a parameter's erasure as written in source, `T extends Foo<T>` → `Foo`. */
  private def erase(tpe: Tree, bounds: Map[String, String]): String =
    tpe match
      case t: IdentifierTree => bounds.getOrElse(t.getName.toString, t.getName.toString)
      case t: ArrayTypeTree  => s"${erase(t.getType, bounds)}[]"
      case _                 => eraseRaw(tpe)

  private def eraseRaw(tpe: Tree): String =
    tpe match
      case t: PrimitiveTypeTree     => t.getPrimitiveTypeKind.name.toLowerCase
      case t: ArrayTypeTree         => s"${eraseRaw(t.getType)}[]"
      case t: ParameterizedTypeTree => eraseRaw(t.getType)
      case t: AnnotatedTypeTree     => eraseRaw(t.getUnderlyingType)
      case t: MemberSelectTree      => t.getIdentifier.toString
      case t: IdentifierTree        => t.getName.toString
      case other                    => other.toString

  private def simpleName(erased: String): String =
    val suffix = erased.takeRight(erased.length - erased.replace("[]", "").length)
    val base   = erased.dropRight(suffix.length)
    base.substring(math.max(base.lastIndexOf('.'), base.lastIndexOf('$')) + 1) + suffix

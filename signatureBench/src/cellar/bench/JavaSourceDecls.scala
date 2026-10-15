package cellar.bench

import com.github.javaparser.ParserConfiguration.LanguageLevel
import com.github.javaparser.ast.`type`.{ArrayType, ClassOrInterfaceType, PrimitiveType, Type, TypeParameter}
import com.github.javaparser.ast.body.*
import com.github.javaparser.ast.expr.AnnotationExpr
import com.github.javaparser.ast.nodeTypes.NodeWithTypeParameters
import com.github.javaparser.ast.{Modifier, Node}
import com.github.javaparser.{JavaParser, ParserConfiguration}
import fs2.io.file.Path

import java.util.zip.ZipFile
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** The expected rendering of every public declaration in a sources jar, written the way
  * `JavaTypePrinter` aims to print it: annotations, bodies and `public` dropped, and the modifiers
  * Java implies (interface fields' `static final`, nested interfaces' `static`) made explicit,
  * because those are what the classfile, and so the printer, sees.
  */
object JavaSourceDecls:
  def read(sourcesJar: Path): SourceSide =
    val parser = JavaParser(ParserConfiguration().setLanguageLevel(LanguageLevel.RAW))
    val decls  = List.newBuilder[Decl]
    var throws = 0
    Using.resource(ZipFile(sourcesJar.toNioPath.toFile)) { zip =>
      zip.entries.asScala
        .filter(e => e.getName.endsWith(".java") && !e.getName.endsWith("-info.java"))
        .foreach { entry =>
          val text = String(zip.getInputStream(entry).readAllBytes(), "UTF-8")
          parser.parse(text).getResult.ifPresent { unit =>
            unit.findAll(classOf[AnnotationExpr]).asScala.toList.foreach(_.remove(): Unit)
            val pkg = unit.getPackageDeclaration.map(_.getNameAsString + ".").orElse("")
            unit.getTypes.asScala.foreach { t =>
              val visitor = Visitor(decls, () => throws += 1)
              visitor.visitType(t, s"$pkg${t.getNameAsString}", nested = false, inInterface = false, Map.empty)
            }
          }
        }
    }
    // tasty-query drops `throws`, so it is stripped from the expectation and only counted.
    SourceSide(decls.result(), Map("declarations with `throws` (not printed)" -> throws))

  private final class Visitor(out: collection.mutable.Builder[Decl, List[Decl]], countThrows: () => Unit):
    private def emit(decl: Decl): Unit = out += decl: Unit

    def visitType(decl: TypeDeclaration[?], binary: String, nested: Boolean, inInterface: Boolean, outer: Map[String, String]): Unit =
      if visible(decl, inInterface) then
        val bounds      = outer ++ typeVarBounds(decl)
        val isInterface = decl match
          case c: ClassOrInterfaceDeclaration => c.isInterface
          case _: AnnotationDeclaration       => true
          case _                              => false
        emit(Decl(binary, "type", typeHeader(decl, nested, inInterface)))
        decl.getMembers.asScala.foreach {
          case m: MethodDeclaration if visible(m, isInterface) =>
            if !m.getThrownExceptions.isEmpty then countThrows()
            val mods = List(
              Option.when(m.isProtected)("protected"),
              Option.when(m.isStatic)("static"),
              Option.when(m.isAbstract && !isInterface)("abstract"),
              Option.when(m.isDefault)("default"),
              Option.when(m.isFinal)("final")
            ).flatten
            val typeParams = renderTypeParams(m.getTypeParameters.asScala.toList)
            val head       = (mods ++ Option.when(typeParams.nonEmpty)(typeParams)).mkString(" ")
            val sig        = s"${render(m.getType)} ${m.getNameAsString}(${params(m.getParameters.asScala.toList)})"
            emit(Decl(binary, key("method", m.getNameAsString, m.getParameters.asScala.toList, bounds ++ typeVarBounds(m)), join(head, sig)))
          case c: ConstructorDeclaration if visible(c, isInterface) && !decl.isInstanceOf[EnumDeclaration] =>
            if !c.getThrownExceptions.isEmpty then countThrows()
            val mods = Option.when(c.isProtected)("protected").toList ++
              Option.when(!c.getTypeParameters.isEmpty)(renderTypeParams(c.getTypeParameters.asScala.toList))
            emit(Decl(binary, key("method", "<init>", c.getParameters.asScala.toList, bounds ++ typeVarBounds(c)), join(mods.mkString(" "), s"${c.getNameAsString}(${params(c.getParameters.asScala.toList)})")))
          case f: FieldDeclaration if visible(f, isInterface) =>
            val mods = List(
              Option.when(f.isProtected)("protected"),
              Option.when(f.isStatic || isInterface)("static"),
              Option.when(f.isFinal || isInterface)("final")
            ).flatten.mkString(" ")
            f.getVariables.asScala.foreach { v =>
              emit(Decl(binary, s"field:${v.getNameAsString}", join(mods, s"${render(v.getType)} ${v.getNameAsString}")))
            }
          case a: AnnotationMemberDeclaration =>
            emit(Decl(binary, s"method:${a.getNameAsString}()", s"${render(a.getType)} ${a.getNameAsString}()"))
          case t: TypeDeclaration[?] =>
            visitType(t, s"$binary$$${t.getNameAsString}", nested = true, inInterface = isInterface, bounds)
          case _ => ()
        }
        decl match
          case e: EnumDeclaration =>
            val name = e.getNameAsString
            e.getEntries.asScala.foreach { c =>
              emit(Decl(binary, s"field:${c.getNameAsString}", s"static final $name ${c.getNameAsString}"))
            }
            // Declared by the compiler (JLS 8.9.3), so the classfile has them and the source does not.
            emit(Decl(binary, "method:values()", s"static $name[] values()"))
            emit(Decl(binary, "method:valueOf(String)", s"static $name valueOf(String name)"))
          case c: ClassOrInterfaceDeclaration if !c.isInterface && c.getConstructors.isEmpty =>
            // The default constructor (JLS 8.8.9) takes the class's own access.
            val mods = Option.when(c.isProtected)("protected").toList.mkString(" ")
            emit(Decl(binary, "method:<init>()", join(mods, s"${c.getNameAsString}()")))
          case _ => ()

    private def typeHeader(decl: TypeDeclaration[?], nested: Boolean, inInterface: Boolean): String =
      val (kind, isClass) = decl match
        case c: ClassOrInterfaceDeclaration => if c.isInterface then ("interface", false) else ("class", true)
        case _: EnumDeclaration             => ("enum", false)
        case _: AnnotationDeclaration       => ("@interface", false)
        case _: RecordDeclaration           => ("record", false)
        case _                              => ("class", true)
      val keywords = decl.getModifiers.asScala.map(_.getKeyword.asString).toSet
      val mods = List(
        Option.when(keywords("protected"))("protected"),
        Option.when(nested && (keywords("static") || !isClass || inInterface))("static"),
        Option.when(isClass && keywords("abstract"))("abstract"),
        Option.when(isClass && keywords("final"))("final"),
        Option.when(keywords("sealed"))("sealed"),
        Option.when(keywords("non-sealed"))("non-sealed")
      ).flatten
      val typeParams = decl match
        case d: NodeWithTypeParameters[?] => renderTypeParams(d.getTypeParameters.asScala.toList)
        case _                            => ""
      val inheritance = decl match
        case c: ClassOrInterfaceDeclaration if c.isInterface => clause("extends", c.getExtendedTypes.asScala.toList)
        case c: ClassOrInterfaceDeclaration =>
          clause("extends", c.getExtendedTypes.asScala.toList) + clause("implements", c.getImplementedTypes.asScala.toList) +
            clause("permits", c.getPermittedTypes.asScala.toList)
        case e: EnumDeclaration   => clause("implements", e.getImplementedTypes.asScala.toList)
        case r: RecordDeclaration => clause("implements", r.getImplementedTypes.asScala.toList)
        case _                    => ""
      (mods :+ kind :+ s"${decl.getNameAsString}$typeParams$inheritance").mkString(" ")

    private def clause(keyword: String, types: List[ClassOrInterfaceType]): String =
      if types.isEmpty then "" else types.map(render).mkString(s" $keyword ", ", ", "")

    private def params(ps: List[Parameter]): String =
      ps.map(p => s"${render(p.getType)}${if p.isVarArgs then "..." else ""} ${p.getNameAsString}").mkString(", ")

    private def renderTypeParams(tps: List[TypeParameter]): String =
      if tps.isEmpty then ""
      else
        tps.map { tp =>
          val bounds = tp.getTypeBound.asScala.toList.map(render) match
            case List("Object") => Nil
            case all            => all
          if bounds.isEmpty then tp.getNameAsString else s"${tp.getNameAsString} extends ${bounds.mkString(" & ")}"
        }.mkString("<", ", ", ">")

    private def key(kind: String, name: String, ps: List[Parameter], bounds: Map[String, String]): String =
      val erased = ps.map(p => erase(p.getType, bounds) + (if p.isVarArgs then "[]" else ""))
      s"$kind:$name(${erased.mkString(",")})"

  private def visible(node: Node, inInterface: Boolean): Boolean =
    node match
      case m: com.github.javaparser.ast.nodeTypes.modifiers.NodeWithAccessModifiers[?] =>
        m.hasModifier(Modifier.Keyword.PUBLIC) || m.hasModifier(Modifier.Keyword.PROTECTED) ||
          (inInterface && !m.hasModifier(Modifier.Keyword.PRIVATE))
      case _ => false

  private def typeVarBounds(node: Node): Map[String, String] =
    node match
      case d: NodeWithTypeParameters[?] =>
        d.getTypeParameters.asScala.map { tp =>
          tp.getNameAsString -> tp.getTypeBound.asScala.headOption.map(_.getNameAsString).getOrElse("Object")
        }.toMap
      case _ => Map.empty

  private def erase(tpe: Type, bounds: Map[String, String]): String =
    tpe match
      case t: PrimitiveType        => t.asString
      case t: ArrayType            => s"${erase(t.getComponentType, bounds)}[]"
      // A bound may itself be a type variable (`<S extends T>`), which erases on to T's bound.
      case t: ClassOrInterfaceType =>
        bounds.get(t.getNameAsString) match
          case Some(bound) if bound != t.getNameAsString => erase(ClassOrInterfaceType(null, bound), bounds - t.getNameAsString)
          case Some(bound)                               => bound
          case None                                      => t.getNameAsString
      case other                   => other.asString

  private def render(tpe: Type): String = tpe.asString

  private def join(head: String, sig: String): String =
    if head.isEmpty then sig else s"$head $sig"

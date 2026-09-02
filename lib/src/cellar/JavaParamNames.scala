package cellar

import org.objectweb.asm.{ClassReader, ClassVisitor, Label, MethodVisitor, Opcodes, Type as AsmType}
import tastyquery.Classpaths.Classpath
import tastyquery.Contexts.Context
import tastyquery.Symbols.{ClassSymbol, PackageSymbol, TermSymbol}
import tastyquery.Types.*

import java.util.{Collections, WeakHashMap}
import scala.collection.concurrent.TrieMap

/** Parameter names for Java methods whose classfile lacks `MethodParameters` (javac only emits it
  * with `-parameters`, which the JDK itself does not use). The `LocalVariableTable` from `-g` still
  * names every parameter of a method that has a body, so we read it with ASM and match the method
  * by name plus erased parameter types, the only identity a classfile knows.
  */
object JavaParamNames:
  private type Table = Map[(String, List[String]), List[String]]

  private final class Registered(val classpath: Classpath):
    val tables = TrieMap.empty[String, Option[Table]]

  // tasty-query's Context does not expose its classpath, and the printer only has the Context,
  // so the classpath is remembered here at construction time.
  private val registry = Collections.synchronizedMap(new WeakHashMap[Context, Registered])

  def register(ctx: Context, classpath: Classpath): Unit =
    registry.put(ctx, Registered(classpath)): Unit

  def namesFor(method: TermSymbol)(using ctx: Context): Option[List[String]] =
    for
      registered <- Option(registry.get(ctx))
      owner      <- method.owner match
                      case c: ClassSymbol => Some(c)
                      case _              => None
      binary     <- binaryName(owner)
      table      <- registered.tables.getOrElseUpdate(binary, readTable(registered.classpath, binary))
      erased     <- erasedParams(method, owner)
      names      <- table.get((method.name.toString, erased))
    yield names

  /** Java statics are declared on tasty-query's synthetic module class, but they live in the
    * same classfile as the instance members, so the `$` module suffix is dropped.
    */
  private def binaryName(cls: ClassSymbol): Option[String] =
    val name = if cls.isModuleClass then cls.name.toString.stripSuffix("$") else cls.name.toString
    cls.owner match
      case pkg: PackageSymbol => Some(s"${pkg.fullName}.$name")
      case outer: ClassSymbol => binaryName(outer).map(o => s"$o$$$name")
      case _                  => None

  private def readTable(classpath: Classpath, binary: String): Option[Table] =
    val dot          = binary.lastIndexOf('.')
    val pkg          = if dot < 0 then "" else binary.substring(0, dot)
    val simpleBinary = binary.substring(dot + 1)
    classpath.iterator
      .flatMap(_.listAllPackages())
      .filter(_.dotSeparatedName == pkg)
      .flatMap(_.getClassDataByBinaryName(simpleBinary))
      .find(_.hasClassFile)
      .map(data => parse(data.readClassFileBytes().unsafeArray))

  private def parse(bytes: Array[Byte]): Table =
    val table = Map.newBuilder[(String, List[String]), List[String]]
    val visitor = new ClassVisitor(Opcodes.ASM9):
      override def visitMethod(
          access: Int,
          name: String,
          descriptor: String,
          signature: String,
          exceptions: Array[String]
      ): MethodVisitor =
        val argTypes   = AsmType.getArgumentTypes(descriptor)
        val fromAttr   = Array.fill[String](argTypes.length)(null)
        val fromLvt    = Array.fill[String](argTypes.length)(null)
        var attrIndex  = 0
        // slot of each parameter: `this` takes slot 0 of an instance method, long/double take two
        val slots      = argTypes.scanLeft(if (access & Opcodes.ACC_STATIC) != 0 then 0 else 1)(_ + _.getSize)
        new MethodVisitor(Opcodes.ASM9):
          override def visitParameter(name: String, access: Int): Unit =
            if attrIndex < fromAttr.length then fromAttr(attrIndex) = name
            attrIndex += 1
          override def visitLocalVariable(
              name: String,
              descriptor: String,
              signature: String,
              start: Label,
              end: Label,
              index: Int
          ): Unit =
            val i = slots.indexOf(index)
            if i >= 0 && i < fromLvt.length && fromLvt(i) == null then fromLvt(i) = name
          override def visitEnd(): Unit =
            val names = List(fromAttr, fromLvt).find(_.forall(_ != null))
            names.foreach(n => table += (name, argTypes.map(_.getClassName).toList) -> n.toList)
    new ClassReader(bytes).accept(visitor, ClassReader.SKIP_FRAMES)
    table.result()

  /** Erases a Java method's parameter types to the names ASM's `Type.getClassName` produces.
    * Done structurally, without resolving symbols: tasty-query's own erasure throws on some Java
    * generic arrays, and a failure here must only cost the names, never the signature.
    */
  private def erasedParams(method: TermSymbol, owner: ClassSymbol): Option[List[String]] =
    def clauses(tpe: TypeOrMethodic): List[Type] =
      tpe match
        case t: MethodType => t.paramTypes ++ clauses(t.resultType)
        case t: PolyType   => clauses(t.resultType)
        case _             => Nil
    try Some(clauses(method.declaredType).map(erase(_, owner)))
    catch case _: Exception => None

  private val primitives = Map(
    "scala.Int"     -> "int",
    "scala.Long"    -> "long",
    "scala.Double"  -> "double",
    "scala.Float"   -> "float",
    "scala.Boolean" -> "boolean",
    "scala.Byte"    -> "byte",
    "scala.Short"   -> "short",
    "scala.Char"    -> "char",
    "scala.Unit"    -> "void",
    "scala.Any"     -> "java.lang.Object",
    "scala.AnyRef"  -> "java.lang.Object"
  )

  private def erase(tpe: Type, owner: ClassSymbol): String =
    tpe match
      case t: TypeRef if t.name.toString == "<FromJavaObject>" => "java.lang.Object"
      case t: TypeRef =>
        t.prefix match
          case p: PackageRef => primitives.getOrElse(s"${p.fullyQualifiedName}.${t.name}", s"${p.fullyQualifiedName}.${t.name}")
          case p: TypeRef    => s"${erase(p, owner)}$$${t.name}"
          case _ =>
            classTypeParamBound(owner, t.name.toString) match
              case Some(bound) => erase(bound, owner)
              case None        => s"${binaryName(owner).get}$$${t.name}"
      case t: AppliedType =>
        t.tycon match
          case tycon: TypeRef if tycon.name.toString == "Array" => s"${eraseArg(t.args.head, owner)}[]"
          case tycon                                             => erase(tycon, owner)
      case t: TypeParamRef  => erase(t.binder.paramTypeBounds(t.paramNum).high, owner)
      case t: RepeatedType  => s"${erase(t.elemType, owner)}[]"
      case t: AnnotatedType => erase(t.typ, owner)
      case t: FlexibleType  => erase(t.nonNullableType, owner)
      case t: AndType       => erase(t.first, owner)
      case other            => throw IllegalArgumentException(s"cannot erase ${other.getClass.getSimpleName}")

  private def eraseArg(arg: TypeOrWildcard, owner: ClassSymbol): String =
    arg match
      case t: Type            => erase(t, owner)
      case w: WildcardTypeArg => erase(w.bounds.high, owner)

  private def classTypeParamBound(cls: ClassSymbol, name: String): Option[Type] =
    cls.typeParams.find(_.name.toString == name).map(_.declaredBounds.high).orElse {
      cls.owner match
        case outer: ClassSymbol => classTypeParamBound(outer, name)
        case _                  => None
    }

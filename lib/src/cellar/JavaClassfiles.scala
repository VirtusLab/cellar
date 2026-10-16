package cellar

import org.objectweb.asm.{ClassReader, ClassVisitor, Label, MethodVisitor, Opcodes, Type as AsmType}
import tastyquery.Classpaths.Classpath
import tastyquery.Contexts.Context
import tastyquery.Symbols.{ClassSymbol, PackageSymbol, TermSymbol}
import tastyquery.Types.*

import java.util.{Collections, WeakHashMap}
import scala.collection.concurrent.TrieMap

/** Facts about Java declarations that tasty-query does not read from the classfile, read with ASM.
  * A method is matched by name plus erased parameter types, the only identity a classfile knows.
  *
  *  - Parameter names, when `MethodParameters` is absent (javac only emits it with `-parameters`,
  *    which the JDK itself does not use): the `LocalVariableTable` from `-g` still names every
  *    parameter of a method that has a body.
  *  - `ACC_VARARGS` on constructors, which tasty-query only honours on methods.
  *  - Whether a method has a `Signature` attribute, which decides whether tasty-query's parameters
  *    include an inner class constructor's outer instance (the descriptor has it, the signature not).
  *  - A nested class's declared access, which only the `InnerClasses` attribute records: the class's
  *    own access flags say `public` for a `protected` class.
  */
object JavaClassfiles:
  private type Method = (String, List[String])
  private final case class Table(
      names: Map[Method, List[String]],
      varargs: Set[Method],
      unsigned: Set[Method],
      innerAccess: Map[String, Int]
  )

  private final class Registered(val classpath: Classpath):
    val tables = TrieMap.empty[String, Option[Table]]

  // tasty-query's Context does not expose its classpath, and the printer only has the Context,
  // so the classpath is remembered here at construction time.
  private val registry = Collections.synchronizedMap(new WeakHashMap[Context, Registered])

  def register(ctx: Context, classpath: Classpath): Unit =
    registry.put(ctx, Registered(classpath)): Unit

  def namesFor(method: TermSymbol)(using ctx: Context): Option[List[String]] =
    methodIn(method).flatMap { (table, key) =>
      table.names.get(key).orElse {
        // A generic inner class constructor's signature leaves out the outer instance that its
        // descriptor, and so the table, leads with.
        for
          outer <- method.owner.owner match
                     case c: ClassSymbol if method.name.toString == "<init>" && !c.isModuleClass => binaryName(c)
                     case _                                                                      => None
          names <- table.names.get((key._1, outer :: key._2))
        yield names.drop(1)
      }
    }

  def isVarargs(method: TermSymbol)(using ctx: Context): Boolean =
    methodIn(method).exists((table, key) => table.varargs.contains(key))

  /** True for an inner class's constructor whose parameters, as tasty-query read them from the
    * descriptor, start with the outer instance that no source passes explicitly.
    */
  def leadsWithOuterInstance(ctor: TermSymbol)(using ctx: Context): Boolean =
    val inner = ctor.owner match
      case cls: ClassSymbol =>
        cls.owner match
          case outer: ClassSymbol => !outer.isModuleClass
          case _                  => false
      case _ => false
    inner && ctor.name.toString == "<init>" && methodIn(ctor).exists((table, key) => table.unsigned.contains(key))

  def isProtectedNested(cls: ClassSymbol)(using ctx: Context): Boolean =
    cls.owner.isInstanceOf[ClassSymbol] && (for
      binary <- binaryName(cls)
      table  <- tableFor(binary)
      access <- table.innerAccess.get(binary)
    yield (access & Opcodes.ACC_PROTECTED) != 0).getOrElse(false)

  private def methodIn(method: TermSymbol)(using ctx: Context): Option[(Table, Method)] =
    for
      owner  <- method.owner match
                  case c: ClassSymbol => Some(c)
                  case _              => None
      binary <- binaryName(owner)
      table  <- tableFor(binary)
      erased <- erasedParams(method, owner)
    yield (table, (method.name.toString, erased))

  private def tableFor(binary: String)(using ctx: Context): Option[Table] =
    Option(registry.get(ctx)).flatMap { registered =>
      registered.tables.getOrElseUpdate(binary, readTableSafe(registered.classpath, binary))
    }

  /** Java statics are declared on tasty-query's synthetic module class, but they live in the
    * same classfile as the instance members, so the `$` module suffix is dropped.
    */
  private[cellar] def binaryName(cls: ClassSymbol): Option[String] =
    val decoded = if cls.isModuleClass then cls.name.toString.stripSuffix("$") else cls.name.toString
    val name    = SourceJars.encode(decoded)
    cls.owner match
      case pkg: PackageSymbol =>
        val pkgName = SourceJars.packageName(pkg)
        Some(if pkgName.isEmpty then name else s"$pkgName.$name")
      case outer: ClassSymbol => binaryName(outer).map(o => s"$o$$$name")
      case _                  => None

  private def readTable(classpath: Classpath, binary: String): Option[Table] =
    classBytes(classpath, binary).map(parse)

  private[cellar] def classBytes(classpath: Classpath, binary: String): Option[Array[Byte]] =
    val dot          = binary.lastIndexOf('.')
    val pkg          = if dot < 0 then "" else binary.substring(0, dot)
    val simpleBinary = binary.substring(dot + 1)
    classpath.iterator
      .flatMap(_.listAllPackages())
      .filter(_.dotSeparatedName == pkg)
      .flatMap(_.getClassDataByBinaryName(simpleBinary))
      .find(_.hasClassFile)
      .map(_.readClassFileBytes().unsafeArray)

  // A classfile ASM cannot read (newer than it supports, or corrupt) must only cost these facts.
  private def readTableSafe(classpath: Classpath, binary: String): Option[Table] =
    try readTable(classpath, binary)
    catch case _: Exception => None

  private def parse(bytes: Array[Byte]): Table =
    val table       = Map.newBuilder[Method, List[String]]
    val varargs     = Set.newBuilder[Method]
    val unsigned    = Set.newBuilder[Method]
    val innerAccess = Map.newBuilder[String, Int]
    val visitor = new ClassVisitor(Opcodes.ASM9):
      override def visitInnerClass(name: String, outerName: String, innerName: String, access: Int): Unit =
        innerAccess += name.replace('/', '.') -> access: Unit

      override def visitMethod(
          access: Int,
          name: String,
          descriptor: String,
          signature: String,
          exceptions: Array[String]
      ): MethodVisitor =
        val argTypes   = AsmType.getArgumentTypes(descriptor)
        val key        = (name, argTypes.map(_.getClassName).toList)
        if (access & Opcodes.ACC_VARARGS) != 0 then varargs += key
        if signature == null then unsigned += key
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
            names.foreach(n => table += key -> n.toList)
    new ClassReader(bytes).accept(visitor, ClassReader.SKIP_FRAMES)
    Table(table.result(), varargs.result(), unsigned.result(), innerAccess.result())

  /** Erases a Java method's parameter types to the names ASM's `Type.getClassName` produces.
    * Done structurally, without resolving symbols: tasty-query's own erasure throws on some Java
    * generic arrays, and a failure here must only cost the names, never the signature.
    */
  private[cellar] def erasedParams(method: TermSymbol, owner: ClassSymbol): Option[List[String]] =
    def clauses(tpe: TypeOrMethodic): List[Type] =
      tpe match
        case t: MethodType => t.paramTypes ++ clauses(t.resultType)
        case t: PolyType   => clauses(t.resultType)
        case _             => Nil
    try Some(clauses(method.declaredType).map(erase(_, owner)))
    catch case _: Exception => None

  private[cellar] val primitives = Map(
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
          case p: TermRef    => s"${moduleName(p)}$$${t.name}"
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

  /** Java static nested types are referenced through their enclosing class's companion module. */
  private def moduleName(ref: TermRef): String =
    ref.prefix match
      case p: PackageRef => s"${p.fullyQualifiedName}.${ref.name}"
      case p: TermRef    => s"${moduleName(p)}$$${ref.name}"
      case other         => throw IllegalArgumentException(s"cannot erase prefix ${other.getClass.getSimpleName}")

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

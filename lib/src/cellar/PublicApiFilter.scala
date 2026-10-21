package cellar

import tastyquery.Symbols.{ClassSymbol, Symbol, TermSymbol, TypeSymbol}

object PublicApiFilter:
  def isPublic(sym: Symbol): Boolean =
    !isPrivateSym(sym) && !isSyntheticSym(sym)

  /** [[isPublic]], listing each `object` once. Resolution still needs the value half. */
  def isListed(sym: Symbol): Boolean =
    isPublic(sym) && !isObjectValue(sym)

  private def isPrivateSym(sym: Symbol): Boolean =
    sym match
      case s: (ClassSymbol | TermSymbol | TypeSymbol) => s.isPrivate || isJavaPackagePrivate(s)
      case _              => false

  // tasty-query reads Java's package-private as `ScopedPrivate(pkg)`, not as Private.
  private def isJavaPackagePrivate(sym: ClassSymbol | TermSymbol | TypeSymbol): Boolean =
    sym.sourceLanguage.productPrefix == "Java" && sym.visibility.productPrefix == "ScopedPrivate"

  private def isSyntheticSym(sym: Symbol): Boolean =
    sym match
      case s: (ClassSymbol | TermSymbol | TypeSymbol) =>
        s.isSynthetic || s.name.toString.startsWith("$") || isDefaultGetter(s) ||
          isStaticInitializer(s) || isUncallableConstructor(s) || isJavaStaticsHolder(s)
      case _ => false

  /** tasty-query models a Java class's statics as a companion object; listed beside the class it
   *  would duplicate it as a declaration Java does not have.
   */
  private def isJavaStaticsHolder(sym: ClassSymbol | TermSymbol | TypeSymbol): Boolean =
    sym.sourceLanguage.productPrefix == "Java" && (sym match
      case c: ClassSymbol => c.isModuleClass
      case t: TermSymbol  => t.isModuleVal
      case _              => false)

  /** The value half of an `object`, listed beside its module class, which carries the parents.
   *  Mirrors scaladoc's `SyntheticSupport.isSyntheticField`. A given or implicit object keeps
   *  it: the modifier lives only on the value (dotty's `Flags.RetainedModuleClassFlags`).
   */
  private def isObjectValue(sym: Symbol): Boolean =
    sym match
      case t: TermSymbol => t.isModuleVal && !t.isGivenOrUsing && !t.isImplicit
      case _             => false

  /** Scala 2 pickles flag default getters synthetic; TASTy does not, so they surface by name.
   *  The default is already shown as `= ...` on the parameter itself.
   */
  private def isDefaultGetter(sym: Symbol): Boolean =
    sym.name.toString.contains("$default$")

  /** A Java class's static initializer: it runs on class load and is never callable. */
  private def isStaticInitializer(sym: Symbol): Boolean =
    sym.name.toString == "<clinit>"

  /** Only a concrete class has a user-callable constructor; for an object or a
   *  trait `<init>` is pure noise. Class constructors are kept: for a Java type
   *  they are the only listing that shows how the type is built.
   */
  private def isUncallableConstructor(sym: Symbol): Boolean =
    sym.name.toString == "<init>" && (sym.owner match
      case owner: ClassSymbol => owner.isModuleClass || owner.isTrait
      case _                  => true)

package cellar

import tastyquery.Symbols.{ClassSymbol, Symbol, TermSymbol, TypeSymbol}

object PublicApiFilter:
  def isPublic(sym: Symbol): Boolean =
    !isPrivateSym(sym) && !isSyntheticSym(sym)

  private def isPrivateSym(sym: Symbol): Boolean =
    sym match
      case s: (ClassSymbol | TermSymbol | TypeSymbol) => s.isPrivate
      case _              => false

  private def isSyntheticSym(sym: Symbol): Boolean =
    sym match
      case s: (ClassSymbol | TermSymbol | TypeSymbol) =>
        s.isSynthetic || s.name.toString.startsWith("$") || isUncallableConstructor(s)
      case _ => false

  /** Only a concrete class has a user-callable constructor; for an object or a
   *  trait `<init>` is pure noise. Class constructors are kept: for a Java type
   *  they are the only listing that shows how the type is built.
   */
  private def isUncallableConstructor(sym: Symbol): Boolean =
    sym.name.toString == "<init>" && (sym.owner match
      case owner: ClassSymbol => owner.isModuleClass || owner.isTrait
      case _                  => true)

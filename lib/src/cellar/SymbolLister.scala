package cellar

import cats.effect.IO
import fs2.Stream
import tastyquery.Contexts.Context
import tastyquery.Symbols.{ClassSymbol, PackageSymbol, Symbol, TermOrTypeSymbol}

sealed trait ListTarget
object ListTarget:
  final case class Package(sym: PackageSymbol) extends ListTarget
  final case class Cls(sym: ClassSymbol)       extends ListTarget

sealed trait ListResolveResult
object ListResolveResult:
  final case class Found(target: ListTarget)                                   extends ListResolveResult
  final case class PartialMatch(resolvedFqn: String, missingMember: String)    extends ListResolveResult
  case object NotFound                                                         extends ListResolveResult

object SymbolLister:
  def resolve(fqn: String)(using ctx: Context): IO[ListResolveResult] =
    IO.blocking {
      val pkgOpt =
        try Some(ctx.findPackage(fqn))
        catch case _: Exception => None
      pkgOpt.map(p => ListResolveResult.Found(ListTarget.Package(p))).getOrElse {
        SymbolResolver.resolveToClass(fqn) match
          case Right(cls) => ListResolveResult.Found(ListTarget.Cls(cls))
          case Left(Some(partial)) => ListResolveResult.PartialMatch(partial.resolvedFqn, partial.missingMember)
          case Left(None) => ListResolveResult.NotFound
      }
    }

  def listMembers(target: ListTarget)(using ctx: Context): Stream[IO, Symbol] =
    target match
      case ListTarget.Package(pkg) =>
        Stream
          .eval(IO.blocking(pkg.declarations))
          .flatMap(decls => Stream.emits(decls))
          .filter(PublicApiFilter.isPublic)

      case ListTarget.Cls(cls) =>
        Stream
          .eval(IO.blocking(collectClassMembers(cls)))
          .flatMap(syms => Stream.emits(syms))
          .filter(PublicApiFilter.isPublic)

  private def collectClassMembers(cls: ClassSymbol)(using ctx: Context): List[TermOrTypeSymbol] =
    // Walk linearization (MRO), collecting decls. Deduplicate by unsigned name, keeping
    // the most-derived occurrence (first seen in MRO order).
    val seen      = scala.collection.mutable.Set.empty[String]
    val result    = List.newBuilder[TermOrTypeSymbol]
    val linearization =
      try cls.linearization
      catch case _: Exception => List(cls)
    for
      klass <- linearization
      decl  <-
        try klass.declarations
        catch case _: Exception => Nil
    do
      val key = decl.name.toString
      if !seen.contains(key) then
        seen += key
        result += decl
    result.result()

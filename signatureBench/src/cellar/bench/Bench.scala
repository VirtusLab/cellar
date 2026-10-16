package cellar.bench

import cats.effect.IO
import cats.syntax.all.*
import cellar.CoursierFetchClient.ResolvedClasspath
import cellar.{ContextResource, CoursierFetchClient, JreClasspath, MavenCoordinate}
import org.typelevel.otel4s.trace.Tracer.Implicits.noop

final case class Mismatch(owner: String, expected: String, actual: String, category: String)

final case class LibraryResult(
    coordinate: String,
    matched: Int,
    mismatches: List[Mismatch],
    missing: List[Decl],
    extra: List[Decl],
    knownGaps: Map[String, Int]
):
  def compared: Int = matched + mismatches.size

/** Compares, per library, the declarations its sources jar makes with what cellar prints for them. */
object Bench:
  def run(language: Language, coordinates: List[String]): IO[List[LibraryResult]] =
    JreClasspath.jrtPath().flatMap { jre =>
      coordinates.traverse { raw =>
        for
          coord    <- IO.fromEither(MavenCoordinate.parse(raw).leftMap(IllegalArgumentException(_)))
          resolved <- CoursierFetchClient.fetchClasspathWithSources(coord)
          sources  <- IO.fromOption(ownSourcesJar(resolved, coord))(IllegalStateException(s"no sources jar for $raw"))
          expected <- IO.blocking(language.sourceDecls(sources))
          owners    = expected.decls.map(_.owner).toSet
          printed  <- ContextResource.makeWithSources(resolved, jre).use { (ctx, _, _) =>
                        IO.blocking(language.printedDecls(owners)(using ctx))
                      }
        yield compare(language, raw, expected, printed)
      }
    }

  private def ownSourcesJar(resolved: ResolvedClasspath, coord: MavenCoordinate) =
    resolved.jars
      .find(_.fileName.toString == s"${coord.artifact}-${coord.version}.jar")
      .flatMap(resolved.sourcesJars.get)

  private def compare(language: Language, coordinate: String, expected: SourceSide, printed: List[Decl]): LibraryResult =
    // Overloads whose parameters share their names have no key that tells them apart.
    def ambiguous(decls: List[Decl]) = decls.groupBy(d => (d.owner, d.key)).filter(_._2.sizeIs > 1).keySet
    val clashing = ambiguous(expected.decls) ++ ambiguous(printed)
    def index(decls: List[Decl]) = decls.filterNot(d => clashing((d.owner, d.key))).map(d => (d.owner, d.key) -> d).toMap
    val want = index(expected.decls)
    val got  = index(printed)
    val pairs = want.keySet.intersect(got.keySet).toList.sorted.map { k =>
      (want(k), language.normalize(want(k).text), language.normalize(got(k).text))
    }
    val mismatches = pairs.collect {
      case (decl, e, a) if e != a => Mismatch(decl.owner, e, a, language.classify(e, a))
    }
    LibraryResult(
      coordinate,
      matched = pairs.size - mismatches.size,
      mismatches = mismatches,
      missing = (want.keySet -- got.keySet).toList.sorted.map(want),
      extra = (got.keySet -- want.keySet).toList.sorted.map(got).filterNot(language.isGenerated),
      knownGaps = expected.knownGaps ++
        Option.when(clashing.nonEmpty)("overloads with identical parameter names (not compared)" -> clashing.size)
    )

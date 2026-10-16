package cellar.bench

import fs2.io.file.Path
import tastyquery.Contexts.Context

/** One public declaration. `key` pairs a source declaration with the printed one for the same
  * symbol within `owner` (a binary class name), so it must be derivable from both sides alike.
  */
final case class Decl(owner: String, key: String, text: String)

/** What a sources jar declares, plus counts of constructs the expectation deliberately leaves
  * out because the printer cannot know them.
  */
final case class SourceSide(decls: List[Decl], knownGaps: Map[String, Int])

/** Everything language-specific in the benchmark: where the expected declarations come from, how
  * cellar's printed ones are collected, and how differences between the two are compared.
  */
trait Language:
  def name: String

  /** Maven coordinates benchmarked when none are given; each must publish a sources jar. */
  def defaultLibraries: List[String]

  def sourceDecls(sourcesJar: Path): SourceSide

  /** cellar's rendering of the members of `owners`, keyed as `sourceDecls` keys them. */
  def printedDecls(owners: Set[String])(using Context): List[Decl]

  /** A printed declaration the compiler wrote, which is not reported when no source declares it. */
  def isGenerated(decl: Decl): Boolean = false

  /** Applied to both sides before comparing, for differences that carry no meaning. */
  def normalize(text: String): String

  /** A short label for why `expected` and `actual` differ, used to group the report. */
  def classify(expected: String, actual: String): String

package cellar.bench

import weaver.FunSuite

object DiffReportTest extends FunSuite:

  private def run(pairs: Pair*) = JsonReport.Run("Scala3", pairs.toList.map(JsonReport.Entry("lib", _)))
  private def pair(key: String, matched: Boolean) =
    Pair("p.O$", key, "def f: Int", if matched then "def f: Int" else "def f: Long", Option.unless(matched)("types"))

  test("a pair that matched before and is gone after counts as no longer paired"):
    val diff = DiffReport.diff(run(pair("term:f", matched = true)), run())
    expect(clue(diff.changes.map(c => (c.key, c.outcome))) == List("term:f" -> DiffReport.Outcome.Unpaired)) and
      expect(DiffReport.summary(diff).contains("no longer paired: 1"), DiffReport.summary(diff))

  test("a pair that mismatched before and is gone after is not a change"):
    val diff = DiffReport.diff(run(pair("term:f", matched = false)), run())
    expect(clue(diff.changes).isEmpty) and
      expect.eql(1, diff.onlyBefore)

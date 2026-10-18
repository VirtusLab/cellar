package cellar.bench

import scala.io.Source
import scala.util.Using

/** The signatures whose printed form differs between two runs of the same benchmark, rendered as
  * a self-contained HTML page: before/after diffs against the source, grouped by outcome.
  */
object DiffReport:
  enum Outcome(val label: String):
    case Fixed        extends Outcome("fixed")
    case Regressed    extends Outcome("regressed")
    case StillDiffers extends Outcome("still differs")
    case Changed      extends Outcome("changed")

  final case class Change(
      library: String,
      owner: String,
      key: String,
      source: String,
      before: String,
      after: String,
      outcome: Outcome,
      // The mismatch the change touched: the one it removed, or the one that remains.
      category: String
  )

  final case class Diff(language: String, changes: List[Change], unchanged: Int, onlyBefore: Int, onlyAfter: Int)

  def diff(before: JsonReport.Run, after: JsonReport.Run): Diff =
    def index(run: JsonReport.Run) = run.entries.map(e => (e.library, e.pair.owner, e.pair.key) -> e.pair).toMap
    val (was, now) = (index(before), index(after))
    val shared     = was.keySet.intersect(now.keySet)
    val changes = shared.toList.sorted.flatMap { k =>
      val (b, a) = (was(k), now(k))
      Option.when(b.printed != a.printed) {
        val outcome =
          if !b.matched && a.matched then Outcome.Fixed
          else if b.matched && !a.matched then Outcome.Regressed
          else if !a.matched then Outcome.StillDiffers
          else Outcome.Changed
        Change(k._1, k._2, k._3, a.source, b.printed, a.printed, outcome, a.category.orElse(b.category).getOrElse("none"))
      }
    }
    Diff(after.language, changes, shared.size - changes.size, (was.keySet -- shared).size, (now.keySet -- shared).size)

  def html(diff: Diff, beforeLabel: String, afterLabel: String): String =
    val data = ujson.Obj(
      "language"   -> diff.language,
      "before"     -> beforeLabel,
      "after"      -> afterLabel,
      "unchanged"  -> diff.unchanged,
      "onlyBefore" -> diff.onlyBefore,
      "onlyAfter"  -> diff.onlyAfter,
      "changes" -> diff.changes.map { c =>
        ujson.Obj(
          "library"  -> c.library,
          "owner"    -> c.owner,
          "key"      -> c.key,
          "source"   -> c.source,
          "before"   -> c.before,
          "after"    -> c.after,
          "outcome"  -> c.outcome.label,
          "category" -> c.category
        )
      }
    )
    // Embedded in a <script> element, where only a closing tag could end it early.
    val json     = ujson.write(data).replace("</", "<\\/")
    val template = Using.resource(Source.fromResource("cellar/bench/diff-report.html", getClass.getClassLoader))(_.mkString)
    template.replace("/*DATA*/", json)

  def summary(diff: Diff): String =
    val counts = Outcome.values.map(o => s"${o.label}: ${diff.changes.count(_.outcome == o)}").mkString(", ")
    s"${diff.language}: ${diff.changes.size} changed ($counts), ${diff.unchanged} unchanged"

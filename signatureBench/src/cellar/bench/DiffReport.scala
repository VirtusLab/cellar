package cellar.bench

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToString}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}

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
    // Matched its source before, and is no longer compared: the member vanished, its key changed,
    // or it now collides with an overload.
    case Unpaired     extends Outcome("no longer paired")

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
    val unpaired = (was.keySet -- shared).toList.sorted.filter(was(_).matched).map { k =>
      val b = was(k)
      Change(k._1, k._2, k._3, b.source, b.printed, "", Outcome.Unpaired, "not paired")
    }
    Diff(after.language, changes ++ unpaired, shared.size - changes.size, (was.keySet -- shared).size, (now.keySet -- shared).size)

  private final case class ChangeData(
      library: String,
      owner: String,
      key: String,
      source: String,
      before: String,
      after: String,
      outcome: String,
      category: String
  )
  private final case class ReportData(
      language: String,
      before: String,
      after: String,
      unchanged: Int,
      onlyBefore: Int,
      onlyAfter: Int,
      changes: List[ChangeData]
  )

  // The page reads `changes` even when nothing changed, which jsoniter would leave out by default.
  private given JsonValueCodec[ReportData] = JsonCodecMaker.make(CodecMakerConfig.withTransientEmpty(false))

  def html(diff: Diff, beforeLabel: String, afterLabel: String): String =
    val data = ReportData(
      diff.language,
      beforeLabel,
      afterLabel,
      diff.unchanged,
      diff.onlyBefore,
      diff.onlyAfter,
      diff.changes.map(c => ChangeData(c.library, c.owner, c.key, c.source, c.before, c.after, c.outcome.label, c.category))
    )
    // Embedded in a <script> element, where only a closing tag could end it early.
    val json     = writeToString(data).replace("</", "<\\/")
    val template = Using.resource(Source.fromResource("cellar/bench/diff-report.html", getClass.getClassLoader))(_.mkString)
    template.replace("/*DATA*/", json)

  def summary(diff: Diff): String =
    val counts = Outcome.values.map(o => s"${o.label}: ${diff.changes.count(_.outcome == o)}").mkString(", ")
    s"${diff.language}: ${diff.changes.size} changed ($counts), ${diff.unchanged} unchanged"

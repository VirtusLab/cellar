package cellar.bench

object Report:
  private val SamplesPerGroup = 8

  def render(language: Language, results: List[LibraryResult]): String =
    val sb = StringBuilder()
    sb ++= s"# ${language.name} signature benchmark\n\n"
    sb ++= "| Library | Compared | Matched | Match % | Mismatched | Missing | Extra |\n"
    sb ++= "|---|---:|---:|---:|---:|---:|---:|\n"
    results.foreach { r =>
      sb ++= s"| `${r.coordinate}` | ${r.compared} | ${r.matched} | ${percent(r.matched, r.compared)} | " +
        s"${r.mismatches.size} | ${r.missing.size} | ${r.extra.size} |\n"
    }

    val mismatches = results.flatMap(_.mismatches)
    if mismatches.nonEmpty then
      sb ++= "\n## Mismatches by category\n"
      mismatches.groupBy(_.category).toList.sortBy(-_._2.size).foreach { (category, ms) =>
        sb ++= s"\n### $category (${ms.size})\n\n```diff\n"
        ms.take(SamplesPerGroup).foreach { m =>
          sb ++= s"# ${m.owner}\n- ${m.expected}\n+ ${m.actual}\n"
        }
        sb ++= "```\n"
      }

    section(sb, "In source, not printed", results.flatMap(_.missing))
    section(sb, "Printed, not in source", results.flatMap(_.extra))

    val gaps = results.flatMap(_.knownGaps).groupMapReduce(_._1)(_._2)(_ + _)
    if gaps.nonEmpty then
      sb ++= "\n## Known gaps (left out of the comparison)\n\n"
      gaps.toList.sorted.foreach((gap, n) => sb ++= s"- $gap: $n\n")
    sb.toString

  private def section(sb: StringBuilder, title: String, decls: List[Decl]): Unit =
    if decls.nonEmpty then
      sb ++= s"\n## $title (${decls.size})\n\n```\n"
      decls.take(SamplesPerGroup * 2).foreach(d => sb ++= s"${d.owner}: ${d.text}\n")
      sb ++= "```\n"

  private def percent(n: Int, of: Int): String =
    if of == 0 then "–" else f"${100.0 * n / of}%.1f"

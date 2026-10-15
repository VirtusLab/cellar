package cellar

object DepsFormatter:
  def format(resolved: ResolvedDeps): String =
    val header = s"${resolved.root.render}\n  (${resolved.deps.size} transitive dependencies)"
    val deps   = resolved.deps
      .map(d => s"  ${d.module.organization.value}:${d.module.name.value}:${d.versionConstraint.asString}")
      .mkString("\n")
    if deps.isEmpty then header else s"$header\n\n$deps"

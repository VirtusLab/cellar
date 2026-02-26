package cellar

object DepsFormatter:
  def format(resolved: ResolvedDeps): String =
    val header = s"${resolved.root.render}\n  (${resolved.deps.size} transitive dependencies)"
    val deps   = resolved.deps
      .map(d => s"  ${d.getModule.getOrganization}:${d.getModule.getName}:${d.getVersion}")
      .mkString("\n")
    if deps.isEmpty then header else s"$header\n\n$deps"

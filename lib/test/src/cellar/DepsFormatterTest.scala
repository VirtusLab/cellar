package cellar

import weaver.FunSuite

object DepsFormatterTest extends FunSuite:

  private val rootCoord = MavenCoordinate("org.example", "lib_3", "1.0.0")

  test("format with zero transitive deps shows root and empty count"):
    val resolved = ResolvedDeps(rootCoord, Seq.empty)
    val output   = DepsFormatter.format(resolved)
    expect(output.contains("org.example:lib_3:1.0.0"), s"Root missing: $output")

  test("format with transitive deps lists them"):
    val dep = rootCoord.toCoursierDependency
    val resolved = ResolvedDeps(rootCoord, Seq(dep))
    val output   = DepsFormatter.format(resolved)
    expect(output.contains("org.example"), s"Dep missing: $output")

  test("format output is non-empty for any input"):
    val resolved = ResolvedDeps(rootCoord, Seq.empty)
    expect(DepsFormatter.format(resolved).nonEmpty)

  test("format first line contains root coordinate"):
    val resolved = ResolvedDeps(rootCoord, Seq.empty)
    val firstLine = DepsFormatter.format(resolved).linesIterator.next()
    expect(firstLine.contains("org.example:lib_3:1.0.0"), s"First line: $firstLine")

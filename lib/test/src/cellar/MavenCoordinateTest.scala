package cellar

class MavenCoordinateTest extends munit.FunSuite:

  test("parse happy path returns Right"):
    assertEquals(
      MavenCoordinate.parse("org.typelevel:cats-core_3:2.10.0"),
      Right(MavenCoordinate("org.typelevel", "cats-core_3", "2.10.0"))
    )

  test("render round-trips the original string"):
    val raw = "org.typelevel:cats-core_3:2.10.0"
    MavenCoordinate.parse(raw).foreach(c => assertEquals(c.render, raw))

  test("toCoursierDependency group matches"):
    val coord = MavenCoordinate("org.typelevel", "cats-core_3", "2.10.0")
    val dep   = coord.toCoursierDependency
    assertEquals(dep.module.organization.value, "org.typelevel")

  test("toCoursierDependency artifact matches"):
    val coord = MavenCoordinate("org.typelevel", "cats-core_3", "2.10.0")
    val dep   = coord.toCoursierDependency
    assertEquals(dep.module.name.value, "cats-core_3")

  test("toCoursierDependency version matches"):
    val coord = MavenCoordinate("org.typelevel", "cats-core_3", "2.10.0")
    val dep   = coord.toCoursierDependency
    assertEquals(dep.versionConstraint.asString, "2.10.0")

  test("empty string returns Left"):
    assert(MavenCoordinate.parse("").isLeft)

  test("one segment returns Left"):
    assert(MavenCoordinate.parse("onlyone").isLeft)

  test("two segments returns Left"):
    assert(MavenCoordinate.parse("a:b").isLeft)

  test("four segments returns Left"):
    assert(MavenCoordinate.parse("a:b:c:d").isLeft)

  test("double-colon gives empty segment — Left"):
    assert(MavenCoordinate.parse("org.typelevel::cats-core:2.10.0").isLeft)

  test("trailing colon makes empty version — Left"):
    assert(MavenCoordinate.parse("a:b:").isLeft)

  test("leading colon makes empty group — Left"):
    assert(MavenCoordinate.parse(":b:c").isLeft)

  test("whitespace-only segment returns Left"):
    assert(MavenCoordinate.parse("a:b:   ").isLeft)

  test("version with hyphen is valid — Right"):
    assert(MavenCoordinate.parse("a:b:1.0.0-SNAPSHOT").isRight)

  test("error message contains the raw string"):
    val raw = "bad-coord"
    MavenCoordinate.parse(raw) match
      case Left(msg) => assert(msg.contains(raw))
      case Right(_)  => fail("expected Left")

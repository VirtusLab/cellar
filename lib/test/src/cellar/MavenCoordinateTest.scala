package cellar

object MavenCoordinateTest extends weaver.FunSuite:

  test("parse happy path returns Right"):
    expect(
      clue(MavenCoordinate.parse("org.typelevel:cats-core_3:2.10.0")) ==
        Right(MavenCoordinate("org.typelevel", "cats-core_3", "2.10.0"))
    )

  test("render round-trips the original string"):
    val raw = "org.typelevel:cats-core_3:2.10.0"
    forEach(MavenCoordinate.parse(raw).toOption)(c => expect.eql(raw, c.render))

  test("toCoursierDependency group matches"):
    val coord = MavenCoordinate("org.typelevel", "cats-core_3", "2.10.0")
    val dep   = coord.toCoursierDependency
    expect.eql("org.typelevel", dep.module.organization.value)

  test("toCoursierDependency artifact matches"):
    val coord = MavenCoordinate("org.typelevel", "cats-core_3", "2.10.0")
    val dep   = coord.toCoursierDependency
    expect.eql("cats-core_3", dep.module.name.value)

  test("toCoursierDependency version matches"):
    val coord = MavenCoordinate("org.typelevel", "cats-core_3", "2.10.0")
    val dep   = coord.toCoursierDependency
    expect.eql("2.10.0", dep.versionConstraint.asString)

  test("empty string returns Left"):
    expect(MavenCoordinate.parse("").isLeft)

  test("one segment returns Left"):
    expect(MavenCoordinate.parse("onlyone").isLeft)

  test("two segments returns Left"):
    expect(MavenCoordinate.parse("a:b").isLeft)

  test("four segments returns Left"):
    expect(MavenCoordinate.parse("a:b:c:d").isLeft)

  test("double-colon gives empty segment — Left"):
    expect(MavenCoordinate.parse("org.typelevel::cats-core:2.10.0").isLeft)

  test("trailing colon makes empty version — Left"):
    expect(MavenCoordinate.parse("a:b:").isLeft)

  test("leading colon makes empty group — Left"):
    expect(MavenCoordinate.parse(":b:c").isLeft)

  test("whitespace-only segment returns Left"):
    expect(MavenCoordinate.parse("a:b:   ").isLeft)

  test("version with hyphen is valid — Right"):
    expect(MavenCoordinate.parse("a:b:1.0.0-SNAPSHOT").isRight)

  test("error message contains the raw string"):
    val raw = "bad-coord"
    MavenCoordinate.parse(raw) match
      case Left(msg) => expect(msg.contains(raw))
      case Right(_)  => failure("expected Left")

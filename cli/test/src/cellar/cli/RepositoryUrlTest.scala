package cellar.cli

import munit.FunSuite

class RepositoryUrlTest extends FunSuite:

  private def reason(raw: String): String =
    RepositoryUrl.parse(raw).left.getOrElse(fail(s"expected '$raw' to be rejected"))

  private def accepted(raw: String): String =
    RepositoryUrl.parse(raw).fold(err => fail(s"expected '$raw' to be accepted, got: $err"), _.root)

  test("accepts http, https and file URLs"):
    assertEquals(accepted("https://artifactory.company.com/maven"), "https://artifactory.company.com/maven")
    assertEquals(accepted("http://localhost:8081/repo"), "http://localhost:8081/repo")
    assertEquals(accepted("file:///home/you/.m2/repository"), "file:///home/you/.m2/repository")

  test("trims surrounding whitespace"):
    assertEquals(accepted("  https://repo.example.com/maven  "), "https://repo.example.com/maven")

  test("rejects an empty URL"):
    assertEquals(reason(""), "the URL is empty")
    assertEquals(reason("   "), "the URL is empty")

  test("rejects a URL with no scheme, naming the offending value"):
    val message = reason("artifactory.company.com/maven")
    assert(message.contains("artifactory.company.com/maven"), message)
    assert(message.contains("no scheme"), message)

  test("rejects an unsupported scheme"):
    val message = reason("ftp://repo.example.com/maven")
    assert(message.contains("unsupported scheme 'ftp'"), message)

  test("rejects an http URL with no host"):
    assert(reason("https:///maven").contains("no host"))

  test("rejects a string that is not a URL at all"):
    assert(reason("not a url").contains("not a valid URL"))

  test("moves URL credentials onto the repository, sent up front and over https only"):
    val repo = RepositoryUrl.parse("https://alice:s3cret@pkgs.example.com/maven").fold(fail(_), identity)
    val auth = repo.authentication.getOrElse(fail("expected credentials"))
    assertEquals(repo.root, "https://pkgs.example.com/maven")
    assertEquals((auth.userOpt, auth.passwordOpt), (Some("alice"), Some("s3cret")))
    assert(!auth.optional && auth.httpsOnly, auth)

  test("never echoes a URL password in an error"):
    val message = reason("ftp://alice:s3cret@pkgs.example.com/maven")
    assert(message.contains("ftp://alice:****@pkgs.example.com/maven"), message)
    assert(!message.contains("s3cret"), message)

package cellar.cli

import weaver.{Expectations, FunSuite}

object RepositoryUrlTest extends FunSuite:

  private def reason(raw: String)(check: String => Expectations): Expectations =
    RepositoryUrl.parse(raw).fold(check, _ => failure(s"expected '$raw' to be rejected"))

  private def accepted(raw: String)(check: String => Expectations): Expectations =
    RepositoryUrl.parse(raw).fold(err => failure(s"expected '$raw' to be accepted, got: $err"), repo => check(repo.root))

  test("accepts http, https and file URLs"):
    accepted("https://artifactory.company.com/maven")(expect.eql("https://artifactory.company.com/maven", _)) and
      accepted("http://localhost:8081/repo")(expect.eql("http://localhost:8081/repo", _)) and
      accepted("file:///home/you/.m2/repository")(expect.eql("file:///home/you/.m2/repository", _))

  test("trims surrounding whitespace"):
    accepted("  https://repo.example.com/maven  ")(expect.eql("https://repo.example.com/maven", _))

  test("rejects an empty URL"):
    reason("")(expect.eql("the URL is empty", _)) and
      reason("   ")(expect.eql("the URL is empty", _))

  test("rejects a URL with no scheme, naming the offending value"):
    reason("artifactory.company.com/maven") { message =>
      expect(message.contains("artifactory.company.com/maven"), message) and
        expect(message.contains("no scheme"), message)
    }

  test("rejects an unsupported scheme"):
    reason("ftp://repo.example.com/maven") { message =>
      expect(message.contains("unsupported scheme 'ftp'"), message)
    }

  test("rejects an http URL with no host"):
    reason("https:///maven")(message => expect(message.contains("no host")))

  test("rejects a string that is not a URL at all"):
    reason("not a url")(message => expect(message.contains("not a valid URL")))

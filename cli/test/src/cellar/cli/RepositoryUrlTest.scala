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

  test("moves URL credentials onto the repository, sent up front and over https only"):
    RepositoryUrl.parse("https://alice:s3cret@pkgs.example.com/maven").fold(
      err => failure(s"expected the URL to be accepted, got: $err"),
      repo =>
        repo.authentication.fold(failure("expected credentials")) { auth =>
          expect.eql("https://pkgs.example.com/maven", repo.root) and
            expect.eql((Some("alice"), Some("s3cret")), (auth.userOpt, auth.passwordOpt)) and
            expect(!auth.optional && auth.httpsOnly)
        }
    )

  test("never echoes URL credentials in an error, whatever characters they contain"):
    def masked(raw: String, secret: String) =
      reason(raw)(message => expect(message.contains("****@") && !message.contains(secret), message))
    masked("ftp://alice:s3cret@pkgs.example.com/maven", "s3cret") and
      masked("https://alice:ab/cd@pkgs.example.com/maven", "ab/cd") and
      masked("https://alice:p@ss@pkgs.example.com/maven", "p@ss") and
      masked("ftp://ghp_token@pkgs.example.com/maven", "ghp_token")

  test("explains that credentials with URL delimiters cannot go in a URL"):
    reason("https://alice:ab/cd@pkgs.example.com/maven")(message => expect(message.contains("COURSIER_CREDENTIALS"), message))

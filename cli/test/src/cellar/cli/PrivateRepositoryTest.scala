package cellar.cli

import cats.effect.{IO, Resource}
import cellar.{CoursierFetchClient, MavenCoordinate}
import com.sun.net.httpserver.{HttpExchange, HttpServer}
import weaver.SimpleIOSuite

import java.net.{Authenticator, InetAddress, InetSocketAddress}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.Base64
import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.*

/** Fetches through real HTTP servers standing in for the private-repository setups the README documents. */
object PrivateRepositoryTest extends SimpleIOSuite:

  // The proxy tests set JVM-wide system properties and the default Authenticator.
  override def maxParallelism = 1

  private val coord   = MavenCoordinate("org.example.private", "lib", "1.0.0")
  private val dir     = "/org/example/private/lib/1.0.0/"
  private val content = Map(
    s"${dir}lib-1.0.0.pom" ->
      "<project><modelVersion>4.0.0</modelVersion><groupId>org.example.private</groupId><artifactId>lib</artifactId><version>1.0.0</version></project>",
    s"${dir}lib-1.0.0.jar" -> "not inspected"
  )

  // coursier caches by URL, credentials' user included; a run reusing an earlier URL would be
  // answered from the cache without reaching the servers under test.
  private val run = java.util.UUID.randomUUID().toString.take(8)

  private def basic(user: String, password: String) =
    "Basic " + Base64.getEncoder.encodeToString(s"$user:$password".getBytes(UTF_8))

  private def respond(exchange: HttpExchange, code: Int, body: String = ""): Unit =
    val bytes = body.getBytes(UTF_8)
    exchange.sendResponseHeaders(code, if bytes.isEmpty then -1 else bytes.length.toLong)
    if bytes.nonEmpty then exchange.getResponseBody.write(bytes)
    exchange.close()

  private def serve(exchange: HttpExchange): Unit =
    content.get(exchange.getRequestURI.getPath).fold(respond(exchange, 404))(respond(exchange, 200, _))

  /** Starts `handle` on a free port; yields the port. */
  private def server(handle: HttpExchange => Unit): Resource[IO, Int] =
    Resource
      .make(IO.blocking {
        val server = HttpServer.create(InetSocketAddress("127.0.0.1", 0), 0)
        server.createContext("/", exchange => handle(exchange))
        server.start()
        server
      })(server => IO.blocking(server.stop(0)))
      .map(_.getAddress.getPort)

  test("URL credentials reach a feed that answers anonymous requests with a 203 sign-in page"):
    val anonymous = ConcurrentLinkedQueue[String]()
    val feed = server { exchange =>
      if exchange.getRequestHeaders.getFirst("Authorization") == basic(s"alice-$run", "s3cret") then serve(exchange)
      else
        anonymous.add(exchange.getRequestURI.getPath): Unit
        respond(exchange, 203, "<html>Sign in</html>")
    }
    feed.use { port =>
      RepositoryUrl.parse(s"http://alice-$run:s3cret@127.0.0.1:$port/").fold(
        err => IO.pure(failure(err)),
        repo =>
          CoursierFetchClient.fetchPom(coord, Seq(repo)).map { pom =>
            expect(pom.isDefined) and expect.eql(Nil, anonymous.asScala.toList)
          }
      )
    }

  test("an authenticating proxy is passed the proxy user and password"):
    val proxied = ConcurrentLinkedQueue[String]()
    // A forward proxy is sent the full target URL; this one serves the repository itself.
    val proxy = server { exchange =>
      if exchange.getRequestHeaders.getFirst("Proxy-Authorization") == basic("proxyuser", "proxypw") then
        proxied.add(exchange.getRequestURI.toString): Unit
        serve(exchange)
      else
        exchange.getResponseHeaders.add("Proxy-Authenticate", """Basic realm="corp-proxy"""")
        respond(exchange, 407)
    }
    val properties = List("http.proxyHost", "http.proxyPort", "http.proxyUser", "http.proxyPassword")
    proxy.use { port =>
      val install = JvmProperties.install(
        properties.zip(List("127.0.0.1", port.toString, "proxyuser", "proxypw"))
      )
      val repo = RepositoryUrl.parse(s"http://repo-$run.example.invalid/").fold(err => throw AssertionError(err), identity)
      (install *> CoursierFetchClient.fetchPom(coord, Seq(repo)))
        .guarantee(IO(properties.foreach(sys.props.remove)))
        .map { pom =>
          expect(pom.isDefined) and
            expect(proxied.asScala.exists(_.startsWith(s"http://repo-$run.example.invalid/org/example/private/lib/")))
        }
    }

  test("a proxy configured only through https.proxy* properties is answered too"):
    val properties = List("https.proxyHost", "https.proxyPort", "https.proxyUser", "https.proxyPassword")
    val install    = JvmProperties.install(properties.zip(List("127.0.0.1", "3128", "proxyuser", "proxypw")))
    // The JDK reports "http" for any proxy challenge, an HTTPS tunnel's included.
    val challenge = IO(
      Authenticator.requestPasswordAuthentication(
        "127.0.0.1", InetAddress.getLoopbackAddress, 3128, "http", "corp-proxy", "basic", null,
        Authenticator.RequestorType.PROXY
      )
    )
    (install *> challenge)
      .guarantee(IO((properties ++ List("http.proxyProtocol", "https.proxyProtocol")).foreach(sys.props.remove)))
      .map(auth => expect(auth != null && auth.getUserName == "proxyuser", String.valueOf(auth)))

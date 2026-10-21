package cellar.cli

import cats.effect.IO
import coursier.proxy.SetupProxy

object JvmProperties:

  /** Sets each `java.properties` entry not already set (e.g. by `-D`) and enables proxy
    * authentication from `http(s).proxyUser` / `http(s).proxyPassword`.
    *
    * Must run before the first HTTP connection: the JDK reads its tunneling settings once.
    */
  def install(properties: List[(String, String)]): IO[Unit] =
    IO {
      properties.foreach(sys.props.getOrElseUpdate(_, _))
      // coursier answers a proxy's challenge only when the protocol the JDK reports matches these,
      // and it defaults the https one to "https". The JDK only ever speaks plain HTTP to a proxy,
      // so a setup with just `https.proxy*` would never be answered.
      List("http.proxyProtocol", "https.proxyProtocol").foreach(sys.props.getOrElseUpdate(_, "http"))
      // The JDK ignores the proxy user and password on its own and refuses Basic auth for HTTPS
      // tunnels; this installs what the coursier launcher does, and only when a proxy user is set.
      SetupProxy.setupAuthenticator(): Unit
    }

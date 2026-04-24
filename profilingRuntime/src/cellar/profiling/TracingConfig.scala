package cellar.profiling

case class LocalTracingSpec(otlpEndpoint: String, pyroscopeEndpoint: String)

case class RemoteTelemetrySpec(otlpEndpoint: String, allowlistedAttributes: Set[String])

case class TracingConfig(
    appName: String,
    local: Option[LocalTracingSpec],
    remote: Option[RemoteTelemetrySpec]
)

object TracingConfig:
  val disabled: TracingConfig = TracingConfig("app", None, None)

object AllowedAttributes:
  val default: Set[String] = Set(
    "command.name",
    "command.success",
    "error.category",
    "error.type",
    "target.lang",
    "build.tool",
    "is.external",
    "cellar.version",
    "os.type",
    "installation.id",
    "session.id"
  )

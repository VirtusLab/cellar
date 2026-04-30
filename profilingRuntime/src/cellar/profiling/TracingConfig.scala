package cellar.profiling

case class LocalTracingSpec(otlpEndpoint: String, pyroscopeEndpoint: String)

case class RemoteTelemetrySpec(otlpEndpoint: String, pyroscopeEndpoint: String, allowlistedAttributes: Set[String])

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
    "build.tool",
    "cellar.version",
    "os.type",
    "service.name",
    "installation.id",
    "session.id",
    "pyroscope.profile.id"
  )

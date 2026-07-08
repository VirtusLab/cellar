package cellar.profiling

case class TracingConfig(
    appName: String,
    otlpEndpoint: Option[String],
    pyroscopeEndpoint: Option[String]
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
    "pyroscope.profile.id"
  )

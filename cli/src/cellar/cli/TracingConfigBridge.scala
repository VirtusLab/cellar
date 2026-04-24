package cellar.cli

import cellar.Config
import cellar.profiling.{AllowedAttributes, LocalTracingSpec, RemoteTelemetrySpec, TracingConfig}

/** Translates cellar's domain `Config` into the library-agnostic
  * `TracingConfig` consumed by `profilingRuntime`. Local tracing always
  * targets the developer-stack endpoints; remote telemetry honors the
  * configurable endpoint from `cellar.conf`.
  */
object TracingConfigBridge:

  private val LocalOtlpEndpoint      = "http://localhost:4318/v1/traces"
  private val LocalPyroscopeEndpoint = "http://localhost:4040"

  def fromCellarConfig(c: Config): TracingConfig =
    TracingConfig(
      appName = "cellar",
      local = Option.when(c.profiling.enabled)(
        LocalTracingSpec(LocalOtlpEndpoint, LocalPyroscopeEndpoint)
      ),
      remote = Option.when(c.telemetry.enabled)(
        RemoteTelemetrySpec(c.telemetry.endpoint, AllowedAttributes.default)
      )
    )

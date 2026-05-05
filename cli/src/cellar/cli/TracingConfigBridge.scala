package cellar.cli

import cellar.Config
import cellar.profiling.TracingConfig

/** Translates cellar's domain `Config` into the library-agnostic `TracingConfig`
  * consumed by `profilingRuntime`. Each endpoint is opt-in independently:
  * `profiling.enabled` switches Pyroscope on, `otel.enabled` switches OTLP traces on.
  */
object TracingConfigBridge:

  def fromCellarConfig(c: Config): TracingConfig =
    TracingConfig(
      appName           = "cellar",
      otlpEndpoint      = Option.when(c.otel.enabled)(c.otel.endpoint),
      pyroscopeEndpoint = Option.when(c.profiling.enabled)(c.profiling.pyroscopeEndpoint)
    )

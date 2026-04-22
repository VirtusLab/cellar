package cellar.profiling

import cats.effect.{ExitCode, IO, IOLocal, Resource}
import cats.mtl.Local
import cats.syntax.all.*
import org.http4s.Uri
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.context.LocalProvider
import org.typelevel.otel4s.sdk.context.Context
import org.typelevel.otel4s.sdk.exporter.RetryPolicy
import org.typelevel.otel4s.sdk.exporter.otlp.OtlpProtocol
import org.typelevel.otel4s.sdk.exporter.otlp.trace.OtlpSpanExporter
import org.typelevel.otel4s.sdk.trace.SdkTracerProvider
import org.typelevel.otel4s.sdk.trace.exporter.SpanExporter
import org.typelevel.otel4s.sdk.trace.processor.{BatchSpanProcessor, SpanProcessor}
import org.typelevel.otel4s.trace.{StatusCode, Tracer}

import scala.concurrent.duration.*

object TracingRuntime:

  val NativeImage: Boolean =
    sys.props.get("org.graalvm.nativeimage.imagecode").contains("runtime")

  /** Builds a [[Tracer]] for one CLI invocation. Returns a noop tracer if
    * nothing is configured, or if running under native image and only the
    * JVM-only local path is configured.
    */
  def resource(config: TracingConfig): Resource[IO, Tracer[IO]] =
    val wantLocal  = config.local.isDefined && !NativeImage
    val wantRemote = config.remote.isDefined
    if !wantLocal && !wantRemote then Resource.pure[IO, Tracer[IO]](Tracer.noop[IO])
    else
      for
        ioLocal        <- Resource.eval(IOLocal(Context.root))
        localCtx       <- Resource.eval {
                            given IOLocal[Context] = ioLocal
                            LocalProvider[IO, Context].local
                          }
        processors     <- buildProcessors(config, wantLocal, wantRemote)
        tracerProvider <- Resource.eval {
                            given Local[IO, Context] = localCtx
                            val base                 = SdkTracerProvider.builder[IO]
                            processors.foldLeft(base)(_.addSpanProcessor(_)).build
                          }
        tracer         <- Resource.eval(tracerProvider.get("cellar"))
      yield tracer

  private def buildProcessors(
      config: TracingConfig,
      wantLocal: Boolean,
      wantRemote: Boolean
  ): Resource[IO, List[SpanProcessor[IO]]] =
    val localProc  =
      if wantLocal then config.local.traverse(spec => localProcessor(spec.otlpEndpoint))
      else Resource.pure[IO, Option[SpanProcessor[IO]]](None)
    val remoteProc =
      if wantRemote then config.remote.traverse(spec => remoteProcessor(spec))
      else Resource.pure[IO, Option[SpanProcessor[IO]]](None)
    (localProc, remoteProc).mapN((l, r) => List(l, r).flatten)

  private def localProcessor(endpoint: String): Resource[IO, SpanProcessor[IO]] =
    otlpExporter(endpoint).flatMap(batched)

  private def remoteProcessor(spec: RemoteTelemetrySpec): Resource[IO, SpanProcessor[IO]] =
    otlpExporter(spec.otlpEndpoint)
      .map(new AllowlistExporter(_, spec.allowlistedAttributes))
      .flatMap(batched)

  private def otlpExporter(endpoint: String): Resource[IO, SpanExporter[IO]] =
    Resource.eval(IO.fromEither(Uri.fromString(endpoint))).flatMap { uri =>
      OtlpSpanExporter
        .builder[IO]
        .withEndpoint(uri)
        .withProtocol(OtlpProtocol.httpJson)
        .withTimeout(100.millis)
        .withRetryPolicy(RetryPolicy.builder.withMaxAttempts(1).build)
        .build
    }

  private def batched(exporter: SpanExporter[IO]): Resource[IO, SpanProcessor[IO]] =
    BatchSpanProcessor
      .builder(exporter)
      .withMaxQueueSize(128)
      .withMaxExportBatchSize(128)
      .withScheduleDelay(1.minute)
      .withExporterTimeout(100.millis)
      .build

  /** Runs `body` inside a root `cellar.command` span. Records the outcome as
    * `command.success` plus `error.category`. `classifyUserError` distinguishes
    * known domain errors (reported as `user`) from unexpected exceptions
    * (reported as `system` + `StatusCode.Error`).
    */
  def tracedCommand(
      config: TracingConfig,
      cellarVersion: String,
      commandName: String,
      classifyUserError: Throwable => Boolean = _ => false
  )(body: Tracer[IO] ?=> IO[ExitCode]): IO[ExitCode] =
    resource(config).use { tracer =>
      given Tracer[IO] = tracer
      val rootAttrs    = List(
        Attribute("command.name", commandName),
        Attribute("cellar.version", cellarVersion),
        Attribute("os.type", System.getProperty("os.name", "unknown"))
      )
      Tracer[IO]
        .spanBuilder("cellar.command")
        .addAttributes(rootAttrs*)
        .build
        .use { span =>
          body.attempt.flatMap {
            case Right(ExitCode.Success) =>
              span.addAttribute(Attribute("command.success", true)).as(ExitCode.Success)
            case Right(other)            =>
              span
                .addAttributes(
                  Attribute("command.success", false),
                  Attribute("error.category", "user")
                )
                .as(other)
            case Left(err)               =>
              val userCategory = classifyUserError(err)
              val category     = if userCategory then "user" else "system"
              val setStatus    = if userCategory then IO.unit else span.setStatus(StatusCode.Error)
              span.addAttributes(
                Attribute("command.success", false),
                Attribute("error.category", category),
                Attribute("error.type", err.getClass.getSimpleName)
              ) *> setStatus *> IO.raiseError(err)
          }
        }
    }

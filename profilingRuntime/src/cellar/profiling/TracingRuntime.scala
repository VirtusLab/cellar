package cellar.profiling

import cats.effect.std.Console
import cats.effect.{ExitCode, IO, IOLocal, Resource}
import cats.mtl.Local
import cats.syntax.all.*
import org.http4s.{Header, Headers, Uri}
import org.typelevel.ci.CIString
import org.typelevel.otel4s.context.LocalProvider
import org.typelevel.otel4s.sdk.TelemetryResource
import org.typelevel.otel4s.sdk.context.Context
import org.typelevel.otel4s.sdk.exporter.RetryPolicy
import org.typelevel.otel4s.sdk.exporter.otlp.OtlpProtocol
import org.typelevel.otel4s.sdk.exporter.otlp.trace.OtlpSpanExporter
import org.typelevel.otel4s.sdk.trace.SdkTracerProvider
import org.typelevel.otel4s.sdk.trace.exporter.SpanExporter
import org.typelevel.otel4s.sdk.trace.processor.{BatchSpanProcessor, SpanProcessor}
import org.typelevel.otel4s.trace.{StatusCode, Tracer}
import org.typelevel.otel4s.{AllowlistExporter, Attribute, Attributes, JavaNetHttpOtlpExporter, ProfilingSpanProcessor}

import scala.concurrent.duration.*

object TracingRuntime:

  val NativeImage: Boolean =
    sys.props.get("org.graalvm.nativeimage.imagecode").contains("runtime")

  /** Builds a [[Tracer]] for one CLI invocation. Returns a noop tracer when no OTLP
    * endpoint is configured — without an exporter, spans have nowhere to go and
    * `ProfilingSpanProcessor`'s `pyroscope.profile.id` tags would be unqueryable.
    * Pyroscope can still run independently and produce uncorrelated flame graphs.
    */
  def resource(
      config: TracingConfig,
      ioLocal: IOLocal[Context],
      installationId: Option[String] = None
  ): Resource[IO, Tracer[IO]] =
    if config.otlpEndpoint.isEmpty then
      Resource.pure[IO, Tracer[IO]](Tracer.noop[IO])
    else
      for
        localCtx       <- Resource.eval {
                            given IOLocal[Context] = ioLocal
                            LocalProvider[IO, Context].local
                          }
        processors     <- buildProcessors(config, installationId)
        tracerProvider <- Resource.eval {
                            given Local[IO, Context] = localCtx
                            val serviceResource      = TelemetryResource(Attributes(Attribute("service.name", config.appName)), None)
                            val base                 = SdkTracerProvider.builder[IO].addResource(serviceResource)
                            processors.foldLeft(base)(_.addSpanProcessor(_)).build
                          }
        tracer         <- Resource.eval(tracerProvider.get(config.appName))
      yield tracer

  private def buildProcessors(config: TracingConfig, installationId: Option[String]): Resource[IO, List[SpanProcessor[IO]]] =
    val profileProc = Option.when(!NativeImage && config.pyroscopeEndpoint.isDefined)(new ProfilingSpanProcessor)
    val otlpProc    = config.otlpEndpoint.traverse(otlpProcessor(_, installationId))
    otlpProc.map(o => List(profileProc, o).flatten)

  private def otlpProcessor(endpoint: String, installationId: Option[String]): Resource[IO, SpanProcessor[IO]] =
    otlpSpanExporter(endpoint, installationId)
      .map(new AllowlistExporter(_, AllowedAttributes.default))
      .flatMap(batched)

  // On native image, Ember has no reachability metadata — use java.net.http.HttpClient instead.
  private def otlpSpanExporter(endpoint: String, installationId: Option[String]): Resource[IO, SpanExporter[IO]] =
    if NativeImage then Resource.pure(JavaNetHttpOtlpExporter(endpoint, installationId))
    else otlpExporter(endpoint, installationId)

  private given Console[IO] = new Console[IO]:
    def readLineWithCharset(charset: java.nio.charset.Charset): IO[String] =
      IO.raiseError(new UnsupportedOperationException)
    def print[A](a: A)(using cats.Show[A]): IO[Unit]    = IO.unit
    def println[A](a: A)(using cats.Show[A]): IO[Unit]  = IO.unit
    def error[A](a: A)(using cats.Show[A]): IO[Unit]    = IO.unit
    def errorln[A](a: A)(using cats.Show[A]): IO[Unit]  = IO.unit

  private def otlpExporter(endpoint: String, installationId: Option[String]): Resource[IO, SpanExporter[IO]] =
    Resource.eval(IO.fromEither(Uri.fromString(endpoint))).flatMap { uri =>
      val headers = installationId.fold(Headers.empty)(id => Headers(Header.Raw(CIString("X-Installation-Id"), id)))
      OtlpSpanExporter
        .builder[IO]
        .withEndpoint(uri)
        .withProtocol(OtlpProtocol.httpJson)
        .withTimeout(2.seconds)
        .withRetryPolicy(RetryPolicy.builder.withMaxAttempts(2).withInitialBackoff(50.milliseconds).build)
        .addHeaders(headers)
        .build
    }

  private def batched(exporter: SpanExporter[IO]): Resource[IO, SpanProcessor[IO]] =
    BatchSpanProcessor
      .builder(exporter)
      .withExporterTimeout(3.seconds)
      .build

  /** Runs `body` inside a root `cellar.command` span. Records the outcome as
    * `command.success` plus `error.category`. `classifyUserError` distinguishes
    * known domain errors (reported as `user`) from unexpected exceptions
    * (reported as `system` + `StatusCode.Error`).
    */
  def tracedCommand(
      config: TracingConfig,
      ioLocal: IOLocal[Context],
      cellarVersion: String,
      commandName: String,
      installationId: Option[String] = None,
      classifyUserError: Throwable => Boolean = _ => false
  )(body: Tracer[IO] ?=> IO[ExitCode]): IO[ExitCode] =
    resource(config, ioLocal, installationId).use { tracer =>
      given Tracer[IO] = tracer
      val rootAttrs    = List(
        Attribute("command.name", commandName),
        Attribute("cellar.version", cellarVersion),
        Attribute("os.type", System.getProperty("os.name", "unknown"))
      ) ++
        installationId.map(Attribute("installation.id", _))
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
              ) *> setStatus *>
                IO(System.err.println(Option(err.getMessage).getOrElse(err.getClass.getName)))
                  .as(ExitCode.Error)
          }
        }
    }

package cellar.profiling

import cats.effect.{ExitCode, IO, IOLocal, Resource}
import cats.effect.std.Console
import cats.mtl.Local
import cats.syntax.all.*
import org.http4s.Uri
import org.typelevel.otel4s.{Attribute, Attributes}
import org.typelevel.otel4s.context.LocalProvider
import org.typelevel.otel4s.sdk.TelemetryResource
import org.typelevel.otel4s.sdk.context.Context
import org.typelevel.otel4s.sdk.exporter.RetryPolicy
import org.typelevel.otel4s.sdk.exporter.otlp.OtlpProtocol
import org.typelevel.otel4s.sdk.exporter.otlp.trace.OtlpSpanExporter
import org.typelevel.otel4s.sdk.trace.SdkTracerProvider
import org.typelevel.otel4s.sdk.trace.exporter.SpanExporter
import org.typelevel.otel4s.sdk.trace.processor.{BatchSpanProcessor, SimpleSpanProcessor, SpanProcessor}
import org.typelevel.otel4s.trace.{StatusCode, Tracer}

import scala.concurrent.duration.*

object TracingRuntime:

  val NativeImage: Boolean =
    sys.props.get("org.graalvm.nativeimage.imagecode").contains("runtime")

  /** Builds a [[Tracer]] for one CLI invocation. Returns a noop tracer if nothing is configured. */
  def resource(config: TracingConfig, ioLocal: IOLocal[Context]): Resource[IO, Tracer[IO]] =
    val wantLocal  = config.local.isDefined
    val wantRemote = config.remote.isDefined
    if !wantLocal && !wantRemote then Resource.pure[IO, Tracer[IO]](Tracer.noop[IO])
    else
      for
        localCtx       <- Resource.eval {
                            given IOLocal[Context] = ioLocal
                            LocalProvider[IO, Context].local
                          }
        processors     <- buildProcessors(config)
        tracerProvider <- Resource.eval {
                            given Local[IO, Context] = localCtx
                            val serviceResource      = TelemetryResource(Attributes(Attribute("service.name", config.appName)), None)
                            val base                 = SdkTracerProvider.builder[IO].addResource(serviceResource)
                            processors.foldLeft(base)(_.addSpanProcessor(_)).build
                          }
        tracer         <- Resource.eval(tracerProvider.get(config.appName))
      yield tracer

  private def buildProcessors(config: TracingConfig): Resource[IO, List[SpanProcessor[IO]]] =
    val profileProc = if !NativeImage then config.local.map(_ => new ProfilingSpanProcessor) else None
    val localProc   = config.local.traverse(spec => localProcessor(spec.otlpEndpoint))
    val remoteProc  = config.remote.traverse(spec => remoteProcessor(spec))
    (localProc, remoteProc).mapN((l, r) => List(profileProc, l, r).flatten)

  private def localProcessor(endpoint: String): Resource[IO, SpanProcessor[IO]] =
    (if NativeImage then Resource.pure(JavaNetHttpOtlpExporter(endpoint))
     else otlpExporter(endpoint)).map(SimpleSpanProcessor(_))

  private def remoteProcessor(spec: RemoteTelemetrySpec): Resource[IO, SpanProcessor[IO]] =
    remoteExporter(spec.otlpEndpoint)
      .map(new AllowlistExporter(_, spec.allowlistedAttributes))
      .flatMap(batched)

  // On native image, Ember has no reachability metadata — use java.net.http.HttpClient instead.
  private def remoteExporter(endpoint: String): Resource[IO, SpanExporter[IO]] =
    if NativeImage then Resource.pure(JavaNetHttpOtlpExporter(endpoint))
    else otlpExporter(endpoint)

  private given Console[IO] = new Console[IO]:
    def readLineWithCharset(charset: java.nio.charset.Charset): IO[String] =
      IO.raiseError(new UnsupportedOperationException)
    def print[A](a: A)(using cats.Show[A]): IO[Unit]    = IO.unit
    def println[A](a: A)(using cats.Show[A]): IO[Unit]  = IO.unit
    def error[A](a: A)(using cats.Show[A]): IO[Unit]    = IO.unit
    def errorln[A](a: A)(using cats.Show[A]): IO[Unit]  = IO.unit

  private def otlpExporter(endpoint: String): Resource[IO, SpanExporter[IO]] =
    Resource.eval(IO.fromEither(Uri.fromString(endpoint))).flatMap { uri =>
      OtlpSpanExporter
        .builder[IO]
        .withEndpoint(uri)
        .withProtocol(OtlpProtocol.httpJson)
        .withTimeout(2.seconds)
        .withRetryPolicy(RetryPolicy.builder.withMaxAttempts(2).withInitialBackoff(50.milliseconds).build)
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
    resource(config, ioLocal).use { tracer =>
      given Tracer[IO] = tracer
      val rootAttrs    = List(
        Attribute("command.name", commandName),
        Attribute("cellar.version", cellarVersion),
        Attribute("os.type", System.getProperty("os.name", "unknown"))
      ) ++
        sys.env.get("CELLAR_SESSION_ID").map(Attribute("session.id", _)) ++
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

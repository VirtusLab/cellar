package cellar.profiling

import cats.effect.IO
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.sdk.trace.SpanRef
import org.typelevel.otel4s.sdk.trace.processor.SpanProcessor
import org.typelevel.otel4s.trace.SpanContext

/** Attaches a `pyroscope.profile.id` attribute to each started span, derived
  * from the span context's span id. Grafana can then pivot from a span to
  * the corresponding Pyroscope profile slice by that id.
  */
final class ProfilingSpanProcessor extends SpanProcessor[IO]:
  val name: String = "ProfilingSpanProcessor"

  val onStart: SpanProcessor.OnStart[IO] = new SpanProcessor.OnStart[IO]:
    def apply(parentContext: Option[SpanContext], span: SpanRef[IO]): IO[Unit] =
      span.addAttributes(List(Attribute("pyroscope.profile.id", span.context.spanIdHex)))

  val onEnd: SpanProcessor.OnEnd[IO] = SpanProcessor.OnEnd.noop[IO]

  def forceFlush: IO[Unit] = IO.unit

package cellar.profiling

import cats.effect.IO
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.sdk.trace.SpanRef
import org.typelevel.otel4s.sdk.trace.data.SpanData
import org.typelevel.otel4s.sdk.trace.processor.SpanProcessor
import org.typelevel.otel4s.trace.SpanContext

final class ProfilingSpanProcessor extends SpanProcessor[IO]:
  val name: String = "ProfilingSpanProcessor"

  val onStart: SpanProcessor.OnStart[IO] = new SpanProcessor.OnStart[IO]:
    def apply(parentContext: Option[SpanContext], span: SpanRef[IO]): IO[Unit] =
      span.addAttributes(Seq(Attribute("pyroscope.profile.id", span.context.spanIdHex)))

  val onEnd: SpanProcessor.OnEnd[IO] = new SpanProcessor.OnEnd[IO]:
    def apply(span: SpanData): IO[Unit] = IO.unit

  def forceFlush: IO[Unit] = IO.unit

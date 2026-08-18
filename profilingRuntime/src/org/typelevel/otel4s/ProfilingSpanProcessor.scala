package org.typelevel.otel4s

import cats.effect.IO
import org.typelevel.otel4s.sdk.trace.SpanRef
import org.typelevel.otel4s.sdk.trace.data.SpanData
import org.typelevel.otel4s.sdk.trace.processor.SpanProcessor
import org.typelevel.otel4s.trace.SpanContext

final class ProfilingSpanProcessor extends SpanProcessor.Unsealed[IO]:
  val name: String = "ProfilingSpanProcessor"

  val onStart: SpanProcessor.OnStart[IO] = SpanProcessor.OnStart:
    (_: Option[SpanContext], span: SpanRef[IO]) =>
      span.addAttributes(Seq(Attribute("pyroscope.profile.id", span.context.spanIdHex)))

  val onEnd: SpanProcessor.OnEnd[IO] = SpanProcessor.OnEnd:
    (_: SpanData) => IO.unit

  def forceFlush: IO[Unit] = IO.unit

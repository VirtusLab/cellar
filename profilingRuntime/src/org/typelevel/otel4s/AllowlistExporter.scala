package org.typelevel.otel4s

import cats.Foldable
import cats.effect.IO
import cats.syntax.all.*
import org.typelevel.otel4s.sdk.TelemetryResource
import org.typelevel.otel4s.sdk.trace.data.SpanData
import org.typelevel.otel4s.sdk.trace.exporter.SpanExporter

/** Wraps a [[SpanExporter]] and strips attributes not in `allowlist`,
  * plus all events (which may carry unfiltered user data) from each
  * span and its resource before forwarding.
  */
final class AllowlistExporter(delegate: SpanExporter[IO], allowlist: Set[String])
    extends SpanExporter.Unsealed[IO]:

  def name: String = s"AllowlistExporter(${delegate.name})"

  def exportSpans[G[_]: Foldable](spans: G[SpanData]): IO[Unit] =
    delegate.exportSpans(spans.toList.map(filterSpan)).handleError(_ => ())

  def flush: IO[Unit] = delegate.flush.handleError(_ => ())

  private def filterAttributes(attrs: Attributes): Attributes =
    val kept: Iterable[Attribute[?]] = attrs.filter(a => allowlist.contains(a.key.name))
    Attributes.fromSpecific(kept)

  private def filterSpan(s: SpanData): SpanData =
    SpanData(
      name                 = s.name,
      spanContext          = s.spanContext,
      parentSpanContext    = s.parentSpanContext,
      kind                 = s.kind,
      startTimestamp       = s.startTimestamp,
      endTimestamp         = s.endTimestamp,
      status               = s.status,
      attributes           = s.attributes.map(filterAttributes),
      events               = s.events.map(_ => Vector.empty),
      links                = s.links,
      instrumentationScope = s.instrumentationScope,
      resource             = TelemetryResource(filterAttributes(s.resource.attributes), s.resource.schemaUrl)
    )

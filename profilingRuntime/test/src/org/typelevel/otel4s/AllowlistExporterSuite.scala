package org.typelevel.otel4s

import cats.Foldable
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import munit.CatsEffectSuite
import org.typelevel.otel4s.sdk.TelemetryResource
import org.typelevel.otel4s.sdk.common.InstrumentationScope
import org.typelevel.otel4s.sdk.data.LimitedData
import org.typelevel.otel4s.sdk.trace.data.{EventData, LinkData, SpanData, StatusData}
import org.typelevel.otel4s.sdk.trace.exporter.SpanExporter
import org.typelevel.otel4s.trace.{SpanContext, SpanKind}

import scala.concurrent.duration.*

class AllowlistExporterSuite extends CatsEffectSuite:

  private final class CaptureExporter(ref: Ref[IO, List[SpanData]]) extends SpanExporter.Unsealed[IO]:
    val name: String                                              = "Capture"
    def exportSpans[G[_]: Foldable](spans: G[SpanData]): IO[Unit] =
      ref.update(spans.toList ::: _)
    def flush: IO[Unit]                                           = IO.unit

  private def emptyAttrs: LimitedData[Attribute[?], Attributes] =
    LimitedData.attributes(Int.MaxValue, Int.MaxValue)

  private def attrs(attributes: Attribute[?]*): LimitedData[Attribute[?], Attributes] =
    emptyAttrs.appendAll(Attributes.fromSpecific(attributes))

  private def emptyEvents: LimitedData[EventData, Vector[EventData]] =
    LimitedData.vector[EventData](Int.MaxValue)

  private def emptyLinks: LimitedData[LinkData, Vector[LinkData]] =
    LimitedData.vector[LinkData](Int.MaxValue)

  private def mkSpan(
      spanAttrs: Attributes = Attributes.empty,
      events: Vector[EventData] = Vector.empty,
      resourceAttrs: Attributes = Attributes.empty
  ): SpanData =
    SpanData(
      name                 = "root",
      spanContext          = SpanContext.invalid,
      parentSpanContext    = None,
      kind                 = SpanKind.Internal,
      startTimestamp       = 0.seconds,
      endTimestamp         = Some(1.second),
      status               = StatusData.Unset,
      attributes           = emptyAttrs.appendAll(spanAttrs),
      events               = emptyEvents.appendAll(events),
      links                = emptyLinks,
      instrumentationScope = InstrumentationScope.empty,
      resource             = TelemetryResource(resourceAttrs, None)
    )

  private def runExport(allow: Set[String])(span: SpanData): IO[SpanData] =
    Ref[IO].of(List.empty[SpanData]).flatMap { ref =>
      val capture  = new CaptureExporter(ref)
      val exporter = new AllowlistExporter(capture, allow)
      exporter.exportSpans(List(span)) *> ref.get.map(_.head)
    }

  test("strips attributes not in the allowlist"):
    val span = mkSpan(spanAttrs = Attributes(Attribute("keep", "yes"), Attribute("drop", "no")))
    runExport(Set("keep"))(span).map { out =>
      val names = out.attributes.elements.toList.map(_.key.name).sorted
      assertEquals(names, List("keep"))
    }

  test("strips all events from each span"):
    val event = EventData("sensitive-event", 0.seconds, attrs(Attribute("payload", "secret")))
    val span  = mkSpan(events = Vector(event))
    runExport(Set("keep"))(span).map { out =>
      assertEquals(out.events.elements.toVector, Vector.empty)
    }

  test("filters resource attributes by allowlist"):
    val resourceAttrs = Attributes(Attribute("installation.id", "abc"), Attribute("hostname", "secret"))
    val span          = mkSpan(resourceAttrs = resourceAttrs)
    runExport(Set("installation.id", "command.name"))(span).map { out =>
      val names = out.resource.attributes.toList.map(_.key.name).sorted
      assertEquals(names, List("installation.id"))
    }

  test("strips exception events containing user data such as coordinates"):
    val err   = new RuntimeException("Could not resolve 'com.example:secret-lib:1.0'")
    val event = EventData.fromException(0.seconds, err, emptyAttrs)
    val span  = mkSpan(events = Vector(event))
    runExport(Set("command.name"))(span).map { out =>
      assertEquals(out.events.elements.toVector, Vector.empty)
    }

  test("preserves string, long, and boolean attribute value types"):
    val span = mkSpan(spanAttrs = Attributes(Attribute("s", "text"), Attribute("n", 42L), Attribute("b", true)))
    runExport(Set("s", "n", "b"))(span).map { out =>
      val kept = out.attributes.elements.toList.map(a => a.key.name -> a.value).toMap
      assertEquals(kept("s"), "text")
      assertEquals(kept("n"), 42L)
      assertEquals(kept("b"), true)
    }

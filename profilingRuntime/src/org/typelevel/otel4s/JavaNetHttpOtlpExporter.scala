package org.typelevel.otel4s

import cats.Foldable
import cats.effect.IO
import org.typelevel.otel4s.sdk.trace.data.{SpanData, StatusData}
import org.typelevel.otel4s.sdk.trace.exporter.SpanExporter
import org.typelevel.otel4s.trace.{SpanKind, StatusCode}

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.Duration as JDuration

/** Native-image-safe OTLP/HTTP/JSON exporter backed by [[java.net.http.HttpClient]].
  * Used instead of the Ember-based [[OtlpSpanExporter]] on GraalVM native image,
  * which has no reachability metadata for http4s-ember-client.
  */
object JavaNetHttpOtlpExporter:

  def apply(endpoint: String, installationId: Option[String]): SpanExporter[IO] =
    val client = HttpClient.newBuilder().connectTimeout(JDuration.ofSeconds(2)).build()
    new Impl(client, endpoint, installationId)

  private final class Impl(client: HttpClient, endpoint: String, installationId: Option[String])
      extends SpanExporter.Unsealed[IO]:

    def name: String = "JavaNetHttpOtlpExporter"

    def exportSpans[G[_]: Foldable](spans: G[SpanData]): IO[Unit] =
      IO.blocking {
        val body    = buildJson(Foldable[G].toList(spans))
        val builder = HttpRequest.newBuilder()
          .uri(URI.create(endpoint))
          .header("Content-Type", "application/json")
          .POST(HttpRequest.BodyPublishers.ofString(body))
          .timeout(JDuration.ofSeconds(2))
        // Used by the ingest gateway's per-installation rate-limit zone.
        installationId.foreach(builder.header("X-Installation-Id", _))
        val request = builder.build()
        try { client.send(request, HttpResponse.BodyHandlers.discarding()); () }
        catch case _: Exception => () // silent drop — don't hang the CLI
      }

    def flush: IO[Unit] = IO.unit

  private def buildJson(spans: List[SpanData]): String =
    val grouped = spans.groupBy(s => (s.resource, s.instrumentationScope))
    val scopeSpans = grouped.map { case ((resource, scope), ss) =>
      val attrsArr = attrsJson(resource.attributes)
      val spansArr = ss.map(spanJson).mkString(",")
      s"""{"resource":{"attributes":[$attrsArr]},"scopeSpans":[{"scope":{"name":${str(scope.name)}},"spans":[$spansArr]}]}"""
    }.mkString(",")
    s"""{"resourceSpans":[$scopeSpans]}"""

  private def spanJson(s: SpanData): String =
    val parentField = s.parentSpanContext
      .map(p => s""""parentSpanId":"${p.spanIdHex}",""")
      .getOrElse("")
    val endNano  = s.endTimestamp.map(_.toNanos).getOrElse(s.startTimestamp.toNanos)
    val attrsArr = attrsJson(s.attributes.elements)
    s"""{"traceId":"${s.spanContext.traceIdHex}","spanId":"${s.spanContext.spanIdHex}",${parentField}"name":${str(s.name)},"kind":${kindInt(s.kind)},"startTimeUnixNano":"${s.startTimestamp.toNanos}","endTimeUnixNano":"$endNano","attributes":[$attrsArr],"status":${statusJson(s.status)}}"""

  private def attrsJson(attrs: org.typelevel.otel4s.Attributes): String =
    attrs.map(attrJson).mkString(",")

  private def attrJson(a: Attribute[?]): String =
    val v = a.key.`type` match
      case AttributeType.String  => s"""{"stringValue":${str(a.value.asInstanceOf[String])}}"""
      case AttributeType.Long    => s"""{"intValue":"${a.value}"}"""
      case AttributeType.Boolean => s"""{"boolValue":${a.value}}"""
      case AttributeType.Double  => s"""{"doubleValue":${a.value}}"""
      case _                     => s"""{"stringValue":${str(a.value.toString)}}"""
    s"""{"key":${str(a.key.name)},"value":$v}"""

  private def statusJson(s: StatusData): String =
    val code = s.status match
      case StatusCode.Unset => 0
      case StatusCode.Ok    => 1
      case StatusCode.Error => 2
    s"""{"code":$code}"""

  private def kindInt(k: SpanKind): Int = k match
    case SpanKind.Internal => 1
    case SpanKind.Server   => 2
    case SpanKind.Client   => 3
    case SpanKind.Producer => 4
    case SpanKind.Consumer => 5

  private def str(s: String): String =
    val sb = new StringBuilder(s.length + 2)
    sb.append('"')
    var i = 0
    while i < s.length do
      val c = s.charAt(i)
      c match
        case '"'                 => sb.append("\\\"")
        case '\\'                => sb.append("\\\\")
        case '\b'                => sb.append("\\b")
        case '\f'                => sb.append("\\f")
        case '\n'                => sb.append("\\n")
        case '\r'                => sb.append("\\r")
        case '\t'                => sb.append("\\t")
        case ch if ch.toInt < 32 => sb.append(f"\\u${ch.toInt}%04x")
        case ch                  => sb.append(ch)
      i += 1
    sb.append('"')
    sb.toString

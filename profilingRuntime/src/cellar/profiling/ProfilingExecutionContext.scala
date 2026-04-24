package cellar.profiling

import io.pyroscope.labels.v2.{LabelsSet, ScopedContext}
import org.typelevel.otel4s.sdk.context.Context
import org.typelevel.otel4s.sdk.trace.SdkContextKeys

import scala.concurrent.ExecutionContext

object ProfilingExecutionContext:

  def wrap(ec: ExecutionContext, localCtx: ThreadLocal[Context]): ExecutionContext =
    new ExecutionContext:
      def execute(runnable: Runnable): Unit =
        ec.execute(new Runnable:
          val ctx = localCtx.get()
          def run(): Unit =
            ctx.get(SdkContextKeys.SpanContextKey).filter(_.isValid).map(_.spanIdHex) match
              case None         => runnable.run()
              case Some(spanId) =>
                val scope = new ScopedContext(new LabelsSet("span_id", spanId))
                try runnable.run()
                finally scope.close()
        )
      def reportFailure(cause: Throwable): Unit = ec.reportFailure(cause)

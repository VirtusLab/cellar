package cellar.profiling

import cats.effect.{IOLocal, SyncIO}
import cats.effect.unsafe.{IORuntimeBuilder, IORuntime}
import cats.syntax.all.*
import org.typelevel.otel4s.sdk.context.Context

trait ProfilingIOApp extends cats.effect.IOApp:
  def profilingEnabled: Boolean

  protected final lazy val profilingOnJvm: Boolean = profilingEnabled && !TracingRuntime.NativeImage

  protected final val sharedIOLocal: IOLocal[Context] =
    IOLocal[Context](Context.root)
      .syncStep(100)
      .flatMap(_.leftMap(_ => new RuntimeException("Failed to initialize IOLocal")).liftTo[SyncIO])
      .unsafeRunSync()

  // Built once so that every call to `runtime` returns the same instance.
  // IOApp.main calls `this.runtime` multiple times (null check, installGlobal, metrics, unsafeRunFiber…).
  private lazy val customRuntime: Option[IORuntime] =
    if profilingOnJvm then
      System.setProperty("cats.effect.trackFiberContext", "true")
      val threadLocal = sharedIOLocal.unsafeThreadLocal()
      Some(
        IORuntimeBuilder()
          .transformCompute(ProfilingExecutionContext.wrap(_, threadLocal))
          .transformBlocking(ProfilingExecutionContext.wrap(_, threadLocal))
          .build()
      )
    else None

  override protected def runtime: IORuntime = customRuntime.getOrElse(super.runtime)

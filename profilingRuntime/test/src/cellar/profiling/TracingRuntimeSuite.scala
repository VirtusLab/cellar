package cellar.profiling

import cats.effect.{ExitCode, IO, IOLocal, Ref}
import munit.CatsEffectSuite
import org.typelevel.otel4s.sdk.context.Context
import org.typelevel.otel4s.trace.Tracer

class TracingRuntimeSuite extends CatsEffectSuite:

  private def withLocal[A](f: IOLocal[Context] => IO[A]): IO[A] =
    IOLocal(Context.root).flatMap(f)

  test("tracedCommand with empty TracingConfig provides a noop Tracer and returns the body's ExitCode"):
    withLocal { local =>
      Ref[IO].of(false).flatMap { ran =>
        TracingRuntime
          .tracedCommand(TracingConfig.disabled, local, "test-version", "test-cmd") {
            ran.set(true) *> IO.pure(ExitCode.Success)
          }
          .flatMap { exit =>
            ran.get.map { bodyRan =>
              assertEquals(exit, ExitCode.Success)
              assertEquals(bodyRan, true)
            }
          }
      }
    }

  test("tracedCommand with empty TracingConfig supplies a Tracer implicitly visible to the body"):
    withLocal { local =>
      TracingRuntime
        .tracedCommand(TracingConfig.disabled, local, "test-version", "test-cmd") {
          val _ = summon[Tracer[IO]]
          IO.pure(ExitCode.Success)
        }
        .map(exit => assertEquals(exit, ExitCode.Success))
    }

  test("tracedCommand propagates a non-success ExitCode from the body"):
    withLocal { local =>
      TracingRuntime
        .tracedCommand(TracingConfig.disabled, local, "test-version", "test-cmd") {
          IO.pure(ExitCode.Error)
        }
        .map(exit => assertEquals(exit, ExitCode.Error))
    }

  test("tracedCommand re-raises errors from the body"):
    withLocal { local =>
      interceptIO[RuntimeException] {
        TracingRuntime.tracedCommand(TracingConfig.disabled, local, "test-version", "test-cmd") {
          IO.raiseError(new RuntimeException("boom"))
        }
      }
    }

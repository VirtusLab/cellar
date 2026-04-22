package cellar.profiling

import cats.effect.{ExitCode, IO, Ref}
import munit.CatsEffectSuite
import org.typelevel.otel4s.trace.Tracer

class TracingRuntimeSuite extends CatsEffectSuite:

  test("tracedCommand with empty TracingConfig provides a noop Tracer and returns the body's ExitCode"):
    Ref[IO].of(false).flatMap { ran =>
      TracingRuntime
        .tracedCommand(TracingConfig.disabled, "test-version", "test-cmd") {
          ran.set(true) *> IO.pure(ExitCode.Success)
        }
        .flatMap { exit =>
          ran.get.map { bodyRan =>
            assertEquals(exit, ExitCode.Success)
            assertEquals(bodyRan, true)
          }
        }
    }

  test("tracedCommand with empty TracingConfig supplies a Tracer implicitly visible to the body"):
    TracingRuntime
      .tracedCommand(TracingConfig.disabled, "test-version", "test-cmd") {
        // Body compiles only if a `Tracer[IO]` is in scope.
        val _ = summon[Tracer[IO]]
        IO.pure(ExitCode.Success)
      }
      .map(exit => assertEquals(exit, ExitCode.Success))

  test("tracedCommand propagates a non-success ExitCode from the body"):
    TracingRuntime
      .tracedCommand(TracingConfig.disabled, "test-version", "test-cmd") {
        IO.pure(ExitCode.Error)
      }
      .map(exit => assertEquals(exit, ExitCode.Error))

  test("tracedCommand re-raises errors from the body"):
    interceptIO[RuntimeException] {
      TracingRuntime.tracedCommand(TracingConfig.disabled, "test-version", "test-cmd") {
        IO.raiseError(new RuntimeException("boom"))
      }
    }

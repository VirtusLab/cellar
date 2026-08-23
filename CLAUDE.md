## Important tools

1. Cellar

When you need the API of a JVM dependency, always use cellar. Use it before metals-mcp.

### Project-aware commands (run from project root)

For querying the current project's code and dependencies (auto-detects build tool):

    cellar get [--module <name>] <fqn>       # single symbol
    cellar list [--module <name>] <package>  # explore a package
    cellar search [--module <name>] <query>  # find by name

- Mill/sbt projects: `--module` is required (e.g. `--module lib`, `--module core`)
- scala-cli projects: `--module` is not supported (omit it)
- `--test`: query the test-scope classpath (sbt/scala-cli; for Mill, use the test module directly, e.g. `--module foo.test`)
- `--no-cache`: skip classpath cache, re-extract from build tool
- `--java-home`: override JRE classpath

### External commands (query arbitrary Maven coordinates)

For querying any published artifact by explicit coordinate:

    cellar get-external <coordinate> <fqn>       # single symbol
    cellar list-external <coordinate> <package>  # explore a package
    cellar search-external <coordinate> <query>  # find by name
    cellar deps <coordinate>                     # dependency tree

Coordinates must be explicit: group:artifact_3:version

## Development Principles

1. Think Before Coding

Don't assume. Don't hide confusion. Surface tradeoffs.

Before implementing:

    State your assumptions explicitly. If uncertain, ask.
    If multiple interpretations exist, present them - don't pick silently.
    If a simpler approach exists, say so. Push back when warranted.
    If something is unclear, stop. Name what's confusing. Ask.

2. Simplicity First

Minimum code that solves the problem. Nothing speculative.

    No features beyond what was asked.
    No abstractions for single-use code.
    No "flexibility" or "configurability" that wasn't requested.
    No error handling for impossible scenarios.
    If you write 200 lines and it could be 50, rewrite it.

Ask yourself: "Would a senior engineer say this is overcomplicated?" If yes, simplify.

3. Surgical Changes

Touch only what you must. Clean up only your own mess.

When editing existing code:

    Don't "improve" adjacent code, comments, or formatting.
    Don't refactor things that aren't broken.
    Match existing style, even if you'd do it differently.
    If you notice unrelated dead code, mention it - don't delete it.

When your changes create orphans:

    Remove imports/variables/functions that YOUR changes made unused.
    Don't remove pre-existing dead code unless asked.

The test: Every changed line should trace directly to the user's request.

4. Goal-Driven Execution

Define success criteria. Loop until verified.

Transform tasks into verifiable goals:

    "Add validation" → "Write tests for invalid inputs, then make them pass"
    "Fix the bug" → "Write a test that reproduces it, then make it pass"
    "Refactor X" → "Ensure tests pass before and after"

For multi-step tasks, state a brief plan:

1. [Step] → verify: [check]
2. [Step] → verify: [check]
3. [Step] → verify: [check]

Strong success criteria let you loop independently. Weak criteria ("make it work") require constant clarification.

5. Don't write tests that test the compiler

Examples are testing match exhaustivity, typesystem etc.

6. Always verify end-to-end output quality when fixing a bug

A passing unit test on an intermediate data structure is not proof the
user-visible output is correct. After any bug fix or feature touching
rendered output, run the relevant `cellar` command against a real
fixture or published artifact and read the printed Markdown yourself
before declaring the task done. Internal assertions like "the resolver
returned Found(symbol)" do not catch leaked synthetic names, broken
signatures, or missing separators.

## Code Conventions

- Use `fs2.io.file.Path` for file references, not `java.io.File` or `java.nio.file.Path`
- Coursier error handling: match `coursierapi.error.CoursierError`, call `CoordinateCompleter.suggest` to attach suggestions to `CellarError.CoordinateNotFound`
- Telemetry allowlists are enforced server-side and silently drop unknown data. The Privacy Policy is the legal disclosure of that allowlist and must not drift from it. When you:
  - **add a new span** (`Tracer[IO].span("...")` or `.spanBuilder("...")`): also add the name to `deploy/otel-collector-config.yml`'s `filter/span_names`, *and* to the allow-listed span names in `PRIVACY_POLICY.md`
  - **add a new span or resource attribute**: also add the key to `AllowedAttributes.default` in `profilingRuntime/src/cellar/profiling/TracingConfig.scala`, to `transform/allowlist` in `deploy/otel-collector-config.yml`, *and* to the data-fields list in `PRIVACY_POLICY.md` ("Purposes and legal basis of data processing")
  - **promote an attribute to a Prometheus dimension**: also add it to `metrics_generator.processor.span_metrics.dimensions` in `deploy/tempo-config.yml`
  - **add or change a subprocessor** (hosting provider, third-party service that touches telemetry): update the subprocessor paragraph in `PRIVACY_POLICY.md` and flag it to the DPO (dpo@virtuslab.com) for the Art. 30 GDPR register

## Documentation

When adding or modifying CLI commands, flags, or config options, update `README.md` accordingly.

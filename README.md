<p align="center">
  <img src="docs/cellar.png" alt="Cellar" width="200"/>
</p>

<h1 align="center">Cellar</h1>

<p align="center">
  Look up the public API of any JVM dependency from the terminal.
</p>

---

When a coding agent needs to call an unfamiliar library method, its options are bad: parse HTML docs (expensive, unreliable), find source on GitHub (requires knowing the URL), or hallucinate the API.

Cellar gives agents — and humans — a single shell command that returns exactly the type signatures, members, and docs needed to write correct code. Output is plain Markdown on stdout, ready to be injected into an LLM prompt with zero post-processing.

## Supported artifacts

| Format | Support |
|---|---|
| Scala 3 (TASTy) | Full — signatures, flags, companions, sealed hierarchies, givens, extensions, docstrings |
| Scala 2 (pickles) | Best-effort — type information may be incomplete |
| Java (.class) | Good — signatures, members |

## Installation

### Nix

Run without installing:

```sh
nix run github:VirtusLab/cellar -- get-external org.typelevel:cats-core_3:2.10.0 cats.Monad
```

Install into your profile:

```sh
nix profile install github:VirtusLab/cellar
```

A Nix overlay is also available at `github:VirtusLab/cellar#overlays.default`.

### Manual

Download the native binary for your platform from https://github.com/VirtusLab/cellar/releases/latest, then extract and install:

```sh
tar xz -f cellar-*.tar.gz
sudo mv cellar /usr/local/bin/
```

To verify checksums and signatures, see [RELEASING.md](RELEASING.md).

## Quick start

```sh
# Look up a trait from cats
cellar get-external org.typelevel:cats-core_3:2.10.0 cats.Monad

# Look up a symbol from the current project
cellar get --module core cats.Monad

# List top-level symbols in a package
cellar list-external org.typelevel:cats-core_3:2.10.0 cats

# Search for a method name
cellar search-external org.typelevel:cats-core_3:2.10.0 flatMap

# View source code
cellar get-source org.typelevel:cats-core_3:2.10.0 cats.Monad

# Dependency tree
cellar deps org.typelevel:cats-effect_3:3.5.4
```

## Commands

Cellar has two modes: **project-aware** commands that work against your current project's classpath, and **external** commands that query arbitrary Maven coordinates.

### Project-aware commands

Run from your project root. Cellar auto-detects the build tool (Mill, sbt, or scala-cli), extracts the classpath, and queries your project's code and all its dependencies.

```sh
cellar get [--module <name>] <fqn>
cellar list [--module <name>] <package>
cellar search [--module <name>] <query>
```

- **Mill / sbt**: `--module` is required (e.g. `--module lib`, `--module core`)
- **scala-cli**: `--module` is not supported — omit it

The classpath is cached after the first run. Use `--no-cache` to force re-extraction.

### External commands

Query any published Maven artifact by explicit coordinate:

```sh
cellar get-external <coordinate> <fqn>
cellar list-external <coordinate> <package>
cellar search-external <coordinate> <query>
cellar get-source <coordinate> <fqn>
cellar deps <coordinate>
```

### Command reference

| Command | Description |
|---|---|
| `get` | Symbol info from the current project (signature, flags, members, docs) |
| `get-external` | Symbol info from a Maven coordinate |
| `get-source` | Source code from a published `-sources.jar` |
| `list` | List public symbols in a package/class from the current project |
| `list-external` | List public symbols from a Maven coordinate |
| `search` | Case-insensitive substring search in the current project |
| `search-external` | Case-insensitive substring search from a Maven coordinate |
| `deps` | Print the transitive dependency list |

### Maven coordinates

Coordinates use the format `group:artifact:version`. The `::` shorthand is **not** supported — use the full artifact name.

```
org.typelevel:cats-core_3:2.10.0        # Scala 3
org.typelevel:cats-core_2.13:2.10.0     # Scala 2
org.apache.commons:commons-lang3:3.14.0 # Java
```

Use `latest` as the version to automatically resolve the most recent release:

```sh
cellar get-external org.typelevel:cats-core_3:latest cats.Monad
```

### Options

| Flag | Applies to | Description |
|---|---|---|
| `--module <name>`, `-m` | project commands | Build module name (required for Mill/sbt) |
| `--no-cache` | project commands | Skip classpath cache, re-extract from build tool |
| `--java-home <path>` | all | Use a specific JDK for JRE classpath |
| `-r`, `--repository <url>` | external commands | Extra Maven repository URL (repeatable) |
| `-l`, `--limit <N>` | `list`, `list-external`, `search`, `search-external` | Max results (default: 50) |

## Configuration

Cellar loads configuration from HOCON files and environment variables. Files are loaded in order, with later values overriding earlier ones:

1. Built-in defaults
2. `~/.cellar/cellar.conf` (user-level, optional)
3. `.cellar/cellar.conf` (project-level, optional)

### Default config

```hocon
mill {
  # Binary to invoke when extracting Mill classpaths
  binary = "./mill"           # env: CELLAR_MILL_BINARY
}

sbt {
  # Binary to invoke when extracting sbt classpaths (e.g. "sbt", "sbtn")
  binary = "sbt"              # env: CELLAR_SBT_BINARY
  # Extra arguments passed to sbt, space-separated (e.g. "--client")
  extra-args = ""             # env: CELLAR_SBT_EXTRA_ARGS
}

starvation-checks {
  # Enable Cats Effect CPU starvation warnings (default: false).
  # Set to true during development or CI to surface warnings.
  enabled = false             # env: CELLAR_STARVATION_CHECKS_ENABLED
}
```

### Examples

Use `sbtn` instead of `sbt`:

```hocon
sbt { binary = "sbtn" }
```

Use sbt in `--client` mode:

```hocon
sbt { extra-args = "--client" }
```

Use a custom Mill wrapper:

```hocon
mill { binary = "./millw" }
```

Or via environment: `CELLAR_SBT_BINARY=sbtn cellar get --module core cats.Monad`

## Output conventions

- **stdout** — Markdown content (signatures, docs, source)
- **stderr** — diagnostics (warnings, truncation notices)
- **Exit 0** — success
- **Exit 1** — error

## Example output

<details>
<summary><code>cellar get-external org.typelevel:cats-core_3:2.10.0 cats.Monad</code></summary>

    ## cats.Monad
    ```scala
    trait Monad[F] extends FlatMap[F] with Applicative[F]
    ```
    Monad.
    Allows composition of dependent effectful functions.

    **Flags:** abstract
    **Origin:** cats.Monad
    **Members:**
    ```scala
    def flatMap[A, B](fa: F[A])(f: Function1[A, F[B]]): F[B]
    def pure[A](x: A): F[A]
    def flatten[A](ffa: F[F[A]]): F[A]
    def iterateWhile[A](f: F[A])(p: Function1[A, Boolean]): F[A]
    ...
    ```

</details>

<details>
<summary><code>cellar list-external org.typelevel:cats-core_3:2.10.0 cats --limit 5</code></summary>

    object Eval$
    trait ComposedContravariantCovariant[F, G] extends Contravariant[TypeLambda]
    object Later$
    object Show$
    trait EvalSemigroup[A] extends Semigroup[Eval[A]]
    Note: results truncated at 5. Use --limit to increase.

</details>

<details>
<summary><code>cellar search-external org.typelevel:cats-core_3:2.10.0 flatMap --limit 3</code></summary>

    cats.FlatMap — object FlatMap$
    cats.FlatMap — trait FlatMap[F] extends Apply[F] with FlatMapArityFunctions[F]
    cats.FlatMap$ — object FlatMap$
    Note: results truncated at 3. Use --limit to increase.

</details>

## Using cellar with Claude Code

### As a plugin (recommended)

Install cellar as a Claude Code plugin for auto-updates:

```sh
/plugin marketplace add virtuslab/cellar
/plugin install cellar@cellar
```

Claude will automatically have access to the `/cellar:cellar` skill and know when to use it.

### Manual setup

Alternatively, add the following to your project's `CLAUDE.md`:

````markdown
## Cellar

When you need the API of a JVM dependency, use cellar. Always prefer cellar over hallucinating API signatures.

### Project-aware commands (run from project root)

For querying the current project's code and dependencies (auto-detects build tool):

    cellar get [--module <name>] <fqn>       # single symbol
    cellar list [--module <name>] <package>  # explore a package
    cellar search [--module <name>] <query>  # find by name

- Mill/sbt projects: `--module` is required (e.g. `--module lib`, `--module core`)
- scala-cli projects: `--module` is not supported (omit it)
- `--no-cache`: skip classpath cache, re-extract from build tool
- `--java-home`: override JRE classpath

### External commands (query arbitrary Maven coordinates)

For querying any published artifact by explicit coordinate:

    cellar get-external <coordinate> <fqn>       # single symbol
    cellar list-external <coordinate> <package>  # explore a package
    cellar search-external <coordinate> <query>  # find by name
    cellar get-source <coordinate> <fqn>         # source code
    cellar deps <coordinate>                     # dependency tree

Coordinates must be explicit: `group:artifact_3:version` (use `latest` for newest version).

### Workflow

1. **Don't know the package?** → `cellar search <query>` or `cellar search-external <coordinate> <query>`
2. **Know the package, not the type?** → `cellar list <package>` or `cellar list-external <coordinate> <package>`
3. **Know the type?** → `cellar get <fqn>` or `cellar get-external <coordinate> <fqn>`
4. **Need the source?** → `cellar get-source <coordinate> <fqn>`
````

## Building from source

Requires JDK 17+ and [Mill](https://mill-build.org/).

```sh
# Fat JAR
./mill cli.assembly
java -jar out/cli/assembly.dest/out.jar get-external org.typelevel:cats-core_3:2.10.0 cats.Monad

# Native image (GraalVM)
./mill cli.nativeImage

# Wrapper script (for development)
./scripts/cellar get-external org.typelevel:cats-core_3:2.10.0 cats.Monad
```

### Running tests

```sh
# Publish test fixtures to local Maven first
./mill publishFixtures

# Run tests
./mill lib.test
```

### Installing a local build with Nix

```sh
./mill cli.nativeImage && nix profile install --impure .#dev
```

## Tech stack

Scala 3, Cats Effect, fs2, [tasty-query](https://github.com/scalacenter/tasty-query), [Coursier](https://get-coursier.io/), [decline](https://ben.kirw.in/decline/), Mill.

## License

[MPL-2.0](LICENSE)

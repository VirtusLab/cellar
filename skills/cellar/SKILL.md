---
name: cellar
description: Look up the public API of any JVM dependency (Scala 3, Scala 2, Java) from the terminal. Returns type signatures, members, docstrings, and source code as Markdown.
---

# Cellar

Use cellar to look up the API of JVM dependencies instead of guessing or hallucinating signatures.

## Project-aware commands (run from project root)

Query the current project's code and all its dependencies. Cellar auto-detects the build tool (Mill, sbt, scala-cli).

    cellar get [--module <name>] <fqn>       # single symbol (signature, members, docs)
    cellar list [--module <name>] <package>  # list symbols in a package or class
    cellar search [--module <name>] <query>  # case-insensitive substring search

- Mill/sbt projects: `--module` is required (e.g. `--module lib`, `--module core`)
- scala-cli projects: omit `--module`
- `--no-cache`: skip classpath cache, re-extract from build tool
- `--java-home <path>`: override JRE classpath
- `-l`, `--limit <N>`: max results for `list`/`search` (default: 50), max members for `get`
- `--hide-inherited`: show only members declared on the type itself (`get` commands)
- `--group-inherited`: group members by declaring type with section headers (`get` commands)

## External commands (query arbitrary Maven coordinates)

Query any published artifact by explicit coordinate (`group:artifact:version`):

    cellar get-external <coordinate> <fqn>       # single symbol
    cellar list-external <coordinate> <package>  # list symbols
    cellar search-external <coordinate> <query>  # search by name
    cellar get-source <coordinate> <fqn>         # fetch source code
    cellar deps <coordinate>                     # dependency tree

- Coordinates must be explicit: `group:artifact_3:version` (no `::` shorthand)
- For sbt plugins, use the full Scala and sbt suffix: `group:artifact_2.12_1.0:version` (e.g. `org.scala-native:sbt-scala-native_2.12_1.0:latest`)
- For compiler plugins and other artifacts with full Scala version suffixes, use the full version: `group:artifact_3.3.8:version`
- Use `latest` as the version to resolve the most recent release
- `-r`, `--repository <url>`: extra Maven repository (repeatable)

## Workflow

1. **Don't know the package?** → `cellar search` / `cellar search-external`
2. **Know the package, not the type?** → `cellar list` / `cellar list-external`
3. **Know the type?** → `cellar get` / `cellar get-external`
4. **Need the implementation?** → `cellar get-source`

## Examples

```sh
# Look up a Scala 3 trait
cellar get-external org.typelevel:cats-core_3:2.10.0 cats.Monad

# Look up a Java class
cellar get-external org.apache.commons:commons-lang3:3.14.0 org.apache.commons.lang3.StringUtils

# List a package
cellar list-external io.circe:circe-core_3:0.14.6 io.circe

# Search for a method
cellar search-external org.typelevel:cats-core_3:2.10.0 flatMap

# Get source code
cellar get-source org.typelevel:cats-core_3:2.10.0 cats.Monad

# Dependency tree
cellar deps org.typelevel:cats-effect_3:3.5.4

# sbt plugin (use full Scala + sbt suffix)
cellar deps org.scala-native:sbt-scala-native_2.12_1.0:latest

# Project-aware (from a Mill project root)
cellar get --module lib cats.Monad
cellar list --module core cats
cellar search --module lib flatMap
```

## Output

- **stdout**: Markdown — ready to consume directly
- **stderr**: diagnostics (warnings, truncation notices)
- **Exit 0**: success, **Exit 1**: error

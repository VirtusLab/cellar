# Releasing Cellar

This document describes how to cut a release, what artifacts are produced, and how to verify them.

## Cutting a release

1. Ensure the `main` branch is in a releasable state and tests pass.
2. Trigger the [Release workflow](.github/workflows/release.yml) manually — either via the GitHub Actions UI ("Run workflow" → enter version without `v` prefix) or via `gh`:

   ```sh
   gh workflow run release.yml -f version=1.2.3
   ```

   All jobs check out `main`, so the dispatch ref is irrelevant.

3. The workflow:
   - Builds GraalVM native binaries for all supported platforms in parallel.
   - Packages each binary into a platform-appropriate archive.
   - Publishes the `lib` and `cli` modules to Maven Central as `org.virtuslab:cellar-lib_3:<version>` and `org.virtuslab:cellar-cli_3:<version>`.
   - Generates a SHA256 checksum file.
   - Signs the checksum file using cosign keyless (OIDC).
   - Updates `flake.nix` with the new version and SRI hashes, commits to `main`.
   - Creates and pushes a `v<version>` git tag.
   - Publishes a GitHub Release with all artifacts attached.

The Maven publish waits for the native-binary and JAR builds to succeed before uploading — a Sonatype release is immutable, so we must not publish a version whose corresponding GitHub Release assets (referenced by the coursier app descriptor) failed to build. If Sonatype then rejects the upload (e.g. duplicate version, namespace mismatch, signature failure), no GitHub Release is created and no tag is pushed.

No container images are built or published by this release flow.

## Supported platforms

| Platform | Runner | Archive |
|---|---|---|
| Linux x86_64 | `ubuntu-latest` | `cellar-<version>-linux-x86_64.tar.gz` |
| Linux aarch64 | `ubuntu-24.04-arm` | `cellar-<version>-linux-aarch64.tar.gz` |
| macOS arm64 | `macos-latest` | `cellar-<version>-macos-arm64.tar.gz` |

> **Windows** — not currently supported. The Mill build uses a Unix shell launcher (`./mill`). Windows support can be added when a `mill.bat` launcher is available in the repository.

## Release artifacts

Each GitHub Release contains:

| File | Description |
|---|---|
| `cellar-<version>-<os>-<arch>.tar.gz` | Archive containing the `cellar` binary and `README.md` |
| `checksums.txt` | SHA256 checksums for all archives |
| `checksums.txt.bundle` | Sigstore bundle for the checksum file |

## Maven Central artifacts

Two modules are published per release:

| Coordinate | Contents |
|---|---|
| `org.virtuslab:cellar-lib_3:<version>` | The dependency-API library — symbol resolver, formatters, Maven coordinate parsing, etc. |
| `org.virtuslab:cellar-cli_3:<version>` | The CLI driver (`cellar.cli.CellarApp` and friends). Regular Scala library JAR — does **not** include the bundled JRE blob used by the GraalVM native image. |

`cellar-lib` is what coursier resolves for `cs install cellar` — the [coursier/apps](https://github.com/coursier/apps) descriptor reads `maven-metadata.xml` for `cellar-lib_3` to determine the latest version, then downloads the matching native binary from this repo's GitHub Release.

Both modules are independently usable as Scala 3 dependencies:

```scala
mvn"org.virtuslab::cellar-lib:<version>"
mvn"org.virtuslab::cellar-cli:<version>"
```

### Required secrets — on the `maven-central` GitHub Environment

The `publish-maven` job is gated on a GitHub Environment named `maven-central`. Without it the job will fail to start and no secrets are exposed. Configure the environment under **Settings → Environments → New environment → `maven-central`**:

- **Required reviewers**: at least one trusted maintainer. Each release run pauses at the publish step until a reviewer approves in the Actions UI.
- **Deployment branches and tags**: "Selected branches" → only `main`. Prevents a workflow_dispatch from a feature branch from accessing the secrets at all.
- **Environment secrets** (NOT repo-level secrets):

| Secret | Purpose |
|---|---|
| `SONATYPE_USERNAME` | Central Portal user-token name (generated at central.sonatype.com) |
| `SONATYPE_PASSWORD` | Central Portal user-token value |
| `PGP_SECRET` | Base64-encoded ASCII-armored PGP private key (artifact signatures) |
| `PGP_PASSPHRASE` | Passphrase for the PGP key |

Why on the environment, not the repo: a workflow on a non-`main` branch can read any repo-level secret if it's modified to `echo` it; environment secrets are only injected when the environment's branch rule is satisfied AND the reviewer approves.

Mill reads these via `MILL_SONATYPE_USERNAME` / `MILL_SONATYPE_PASSWORD` / `MILL_PGP_SECRET_BASE64` / `MILL_PGP_PASSPHRASE` environment variables — the workflow's `env:` block is the bridge.

The PGP signing here is unrelated to the cosign signing of `checksums.txt` — Sonatype Central mandates `.asc` signatures on every uploaded artifact (`.jar`, `.pom`, `-sources.jar`, `-javadoc.jar`), while cosign covers the GitHub Release assets.

## Verifying checksums

Download the archive and `checksums.txt`, then:

```sh
sha256sum --check --ignore-missing checksums.txt
```

On macOS:

```sh
shasum -a 256 --check --ignore-missing checksums.txt
```

## Verifying the cosign signature

The `checksums.txt` file is signed using [cosign](https://github.com/sigstore/cosign) with GitHub OIDC (keyless signing). No private key is stored in the repository.

Install cosign ([instructions](https://docs.sigstore.dev/cosign/system_config/installation/)), then verify:

```sh
cosign verify-blob \
  --bundle checksums.txt.bundle \
  --certificate-identity-regexp "https://github.com/simple-scala-tooling/cellar/.github/workflows/release.yml@refs/tags/v" \
  --certificate-oidc-issuer "https://token.actions.githubusercontent.com" \
  checksums.txt
```

A successful verification prints `Verified OK`.

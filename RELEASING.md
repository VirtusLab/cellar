# Releasing Cellar

This document describes how to cut a release, what artifacts are produced, and how to verify them.

## Cutting a release

1. Ensure the `main` branch is in a releasable state and tests pass.
2. Push a version tag (the workflow triggers on `v*`):

   ```sh
   git tag v1.2.3
   git push origin v1.2.3
   ```

3. The [Release workflow](.github/workflows/release.yml) runs automatically and:
   - Builds GraalVM native binaries for all supported platforms in parallel.
   - Packages each binary into a platform-appropriate archive.
   - Generates a SHA256 checksum file.
   - Signs the checksum file using cosign keyless (OIDC).
   - Publishes a GitHub Release with all artifacts attached.

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

# Native smoke suite

End-to-end checks that run a built `cellar` native binary through every
subcommand — external artifacts (Scala 3, Scala 2, Java) and project-aware
commands against the sample scala-cli, Mill and sbt projects in `projects/`.

CI runs it on a runner that did not build the binary and without a checkout
(see `.github/workflows/`), because image-build-time state only breaks away
from the build filesystem. Locally:

```sh
./mill cli.nativeImage
./mill publishFixtures
smoke/native-smoke.sh out/cli/nativeImage.dest/native-executable "file://$HOME/.m2/repository"
```

Omit the repository URL to assert against Maven Central artifacts instead of
the local fixtures. Requires `scala-cli` and `sbt` on `PATH`.

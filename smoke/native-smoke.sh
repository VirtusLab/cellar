#!/usr/bin/env bash
# Runs a built cellar native binary through every subcommand against real
# artifacts and real build-tool projects, asserting on the printed output.
# Meant to run on a machine that did NOT build the binary: anything baked in
# at image-build time (jrt java.home, embedded resources) only breaks away
# from the build filesystem (issue #133).
#
# Usage: native-smoke.sh <cellar-binary> [file://<local-m2-repo>]
# With a repo URL the external assertions target the locally published test
# fixtures; without one they target published Maven Central artifacts.
#
# Requires scala-cli and sbt on PATH for the project-aware section; the mill
# sample project carries its own ./mill bootstrap script.
set -euo pipefail

BINARY="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"
REPO_URL="${2:-}"
PROJECTS="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/projects"

# Nothing may resolve through an ambient JDK — a user's machine has none of
# the build runner's paths, so any lookup that escapes the binary is a bug.
# Build tools still find `java` on PATH.
unset JAVA_HOME

for tool in scala-cli sbt; do
  command -v "$tool" >/dev/null || { echo "FAIL: $tool not on PATH" >&2; exit 1; }
done

step() { echo; echo "--- $*"; }

# `! grep` would be ignored by set -e (negated pipelines never trigger exit),
# so the absence checks need explicit failure branches.
assert_contains() {
  grep -qF -- "$2" "$1" || { echo "FAIL: expected '$2' in $1" >&2; exit 1; }
}
assert_not_contains() {
  if grep -qF -- "$2" "$1"; then echo "FAIL: unexpected '$2' in $1" >&2; exit 1; fi
}
assert_clean() {
  assert_not_contains "$1" 'Exception while compiling'
  assert_not_contains "$1" 'needs_input'
}
# Captures stdout to $1, passes stderr through, keeps the exit code.
run() { local out="$1"; shift; "$BINARY" "$@" | tee "$out"; }
run_expect_fail() {
  local out="$1"; shift
  if "$BINARY" "$@" >"$out" 2>&1; then
    echo "FAIL: expected non-zero exit from: $*" >&2; exit 1
  fi
  cat "$out"
}

if [[ -n "$REPO_URL" ]]; then
  REPO=(-r "$REPO_URL")
  S3_COORD=cellar.test:cellar-fixture-scala3_3:0.1.0-SNAPSHOT
  S3_PKG=cellar.fixture.scala3
  S3_FQN=cellar.fixture.scala3.CellarADT
  S3_DOC='Sealed ADT hierarchy for testing sealedChildren extraction.'
  S3_MEMBER=CellarA
  S3_SEARCH=CellarADT
  S3_META='Cellar Scala 3 test fixture'
  S2_COORD=cellar.test:cellar-fixture-scala2_2.13:0.1.0-SNAPSHOT
  S2_PKG=cellar.fixture.scala2
  S2_FQN=cellar.fixture.scala2.CellarTypeClass
  S2_SIG='trait CellarTypeClass[A]'
  S2_SEARCH=TypeClass
  J_COORD=cellar.test:cellar-fixture-java:0.1.0-SNAPSHOT
  J_PKG=cellar.fixture.java
  J_FQN=cellar.fixture.java.CellarJavaClass
  J_SIG='class CellarJavaClass'
  J_MEMBER=getDefault
  J_SEARCH=JavaEnum
else
  REPO=()
  S3_COORD=org.typelevel:cats-effect_3:3.6.1
  S3_PKG=cats.effect
  S3_FQN=cats.effect.IO
  S3_DOC='A pure abstraction representing the intention'
  S3_MEMBER=flatMap
  S3_SEARCH=IOApp
  S3_META='cats-effect'
  S2_COORD=org.typelevel:cats-core_2.13:2.13.0
  S2_PKG=cats
  S2_FQN=cats.Monad
  S2_SIG='trait Monad[F[_]]'
  S2_SEARCH=Monad
  J_COORD=com.google.guava:guava:33.4.0-jre
  J_PKG=com.google.common.collect
  J_FQN=com.google.common.collect.ImmutableSet
  J_SIG='class ImmutableSet'
  J_MEMBER=of
  J_SEARCH=ImmutableList
fi

WORK="$(mktemp -d)"
cd "$WORK"

# ─── startup ──────────────────────────────────────────────────────────────

step 'cellar --version'
run version.txt --version
assert_contains version.txt 'native-image'

# Regular commands are withheld with exit 2 until a telemetry choice is
# recorded; a fresh runner has none.
step 'cellar telemetry disable --global'
run telemetry.txt telemetry disable --global

# ─── external: Scala 3 ────────────────────────────────────────────────────

step "get-external Scala 3: $S3_FQN (docstring)"
run s3-get.md get-external "${REPO[@]}" "$S3_COORD" "$S3_FQN"
assert_contains s3-get.md "## $S3_FQN"
assert_contains s3-get.md "$S3_DOC"
assert_clean s3-get.md

step "list-external Scala 3: $S3_PKG"
run s3-list.md list-external "${REPO[@]}" "$S3_COORD" "$S3_PKG"
assert_contains s3-list.md "${S3_FQN##*.}"
assert_clean s3-list.md

step "list-external Scala 3 members: $S3_FQN"
run s3-members.md list-external "${REPO[@]}" "$S3_COORD" "$S3_FQN"
assert_clean s3-members.md

step "search-external Scala 3: $S3_SEARCH"
run s3-search.md search-external "${REPO[@]}" "$S3_COORD" "$S3_SEARCH"
assert_contains s3-search.md "$S3_SEARCH"
assert_clean s3-search.md

step "get-source Scala 3: $S3_FQN"
run s3-source.md get-source "${REPO[@]}" "$S3_COORD" "$S3_FQN"
assert_contains s3-source.md '```scala'
assert_contains s3-source.md "${S3_FQN##*.}"
assert_clean s3-source.md

step "deps: $S3_COORD"
run s3-deps.txt deps "${REPO[@]}" "$S3_COORD"
assert_contains s3-deps.txt 'org.scala-lang:scala-library'

step "meta: $S3_COORD"
run s3-meta.txt meta "${REPO[@]}" "$S3_COORD"
assert_contains s3-meta.txt "$S3_META"
assert_contains s3-meta.txt 'License:'

# ─── external: Scala 2 ────────────────────────────────────────────────────

step "get-external Scala 2: $S2_FQN"
run s2-get.md get-external "${REPO[@]}" "$S2_COORD" "$S2_FQN"
assert_contains s2-get.md "$S2_SIG"
assert_clean s2-get.md

step "list-external Scala 2: $S2_PKG"
run s2-list.md list-external "${REPO[@]}" "$S2_COORD" "$S2_PKG"
assert_contains s2-list.md "${S2_FQN##*.}"
assert_contains s2-list.md 'Scala 2'
assert_clean s2-list.md

step "search-external Scala 2: $S2_SEARCH"
run s2-search.md search-external "${REPO[@]}" "$S2_COORD" "$S2_SEARCH"
assert_contains s2-search.md "$S2_FQN"
assert_clean s2-search.md

step "get-source Scala 2 is rejected with a clear message"
run_expect_fail s2-source.txt get-source "${REPO[@]}" "$S2_COORD" "$S2_FQN"
assert_contains s2-source.txt 'Only Scala 3 (TASTy) and Java symbols are supported'

# ─── external: Java (bundled JRE path) ────────────────────────────────────

step "get-external Java: $J_FQN"
run j-get.md get-external "${REPO[@]}" "$J_COORD" "$J_FQN"
assert_contains j-get.md "$J_SIG"
assert_contains j-get.md "$J_MEMBER"
assert_clean j-get.md

step "list-external Java: $J_PKG"
run j-list.md list-external "${REPO[@]}" "$J_COORD" "$J_PKG"
assert_contains j-list.md "${J_FQN##*.}"
assert_clean j-list.md

step "search-external Java: $J_SEARCH"
run j-search.md search-external "${REPO[@]}" "$J_COORD" "$J_SEARCH"
assert_contains j-search.md "$J_SEARCH"
assert_clean j-search.md

step "get-source Java: $J_FQN"
run j-source.md get-source "${REPO[@]}" "$J_COORD" "$J_FQN"
assert_contains j-source.md '```java'
assert_contains j-source.md "${J_FQN##*.}"

# ─── external: error paths ────────────────────────────────────────────────

step 'get-external unknown symbol exits non-zero'
run_expect_fail err-symbol.txt get-external "${REPO[@]}" "$S3_COORD" "$S3_PKG.DoesNotExist99999"
assert_contains err-symbol.txt 'not found'

step 'get-external unknown coordinate exits non-zero'
run_expect_fail err-coord.txt get-external com.nonexistent.x12345:foo:1.0.0 bar.Baz
assert_contains err-coord.txt 'Check that the group ID'

# ─── project-aware: scala-cli ─────────────────────────────────────────────

cp -R "$PROJECTS/scala-cli" "$WORK/proj-scala-cli"
cd "$WORK/proj-scala-cli"

step 'scala-cli get: project class'
run get.md get example.ScalaCliClass
assert_contains get.md 'class ScalaCliClass'
assert_contains get.md 'greet'
assert_clean get.md

step 'scala-cli get: dependency symbol'
run get-dep.md get cats.Monad
assert_contains get-dep.md 'Monad'
assert_clean get-dep.md

step 'scala-cli list: project class members'
run list.md list example.ScalaCliClass
assert_contains list.md 'greet'
assert_contains list.md 'farewell'

step 'scala-cli search: project symbol'
run search.md search ScalaCliClass
assert_contains search.md 'example.ScalaCliClass'

step 'scala-cli --test: test-only dependency is invisible on the main classpath'
run_expect_fail test-main.txt get munit.FunSuite

step 'scala-cli --test: test-only dependency resolves'
run test-scope.md get --test munit.FunSuite
assert_contains test-scope.md 'FunSuite'

step 'scala-cli --module is rejected'
run_expect_fail module.txt get --module app example.ScalaCliClass

# ─── project-aware: mill ──────────────────────────────────────────────────

cp -R "$PROJECTS/mill" "$WORK/proj-mill"
cd "$WORK/proj-mill"
# GitHub artifact zips drop the executable bit.
chmod +x mill

step 'mill get: project class (cold, populates classpath cache)'
run get.md get --module app example.MillClass
assert_contains get.md 'class MillClass'
assert_contains get.md 'greet'
assert_clean get.md
ls .cellar/cache/*.txt >/dev/null || { echo 'FAIL: mill get did not write .cellar/cache' >&2; exit 1; }

step 'mill get: cached classpath'
run get-cached.md get --module app example.MillClass
assert_contains get-cached.md 'class MillClass'

step 'mill get --no-cache'
run get-nocache.md get --no-cache --module app example.MillClass
assert_contains get-nocache.md 'class MillClass'

step 'mill list: project package'
run list.md list --module app example
assert_contains list.md 'MillClass'

step 'mill search: project symbol'
run search.md search --module app MillClass
assert_contains search.md 'example.MillClass'

step 'mill --module is required'
run_expect_fail module.txt get example.MillClass
assert_contains module.txt '--module is required for Mill'

step 'mill --test is rejected'
run_expect_fail test.txt get --test --module app example.MillClass
assert_contains test.txt '--test is not supported for Mill'

# ─── project-aware: sbt ───────────────────────────────────────────────────

cp -R "$PROJECTS/sbt" "$WORK/proj-sbt"
cd "$WORK/proj-sbt"

step 'sbt get: project class (cold, populates classpath cache)'
run get.md get --module smoke-app example.SbtClass
assert_contains get.md 'class SbtClass'
assert_contains get.md 'greet'
assert_clean get.md
ls .cellar/cache/*.txt >/dev/null || { echo 'FAIL: sbt get did not write .cellar/cache' >&2; exit 1; }

step 'sbt get: cached classpath'
run get-cached.md get --module smoke-app example.SbtClass
assert_contains get-cached.md 'class SbtClass'

step 'sbt list: project package'
run list.md list --module smoke-app example
assert_contains list.md 'SbtClass'

step 'sbt search: project symbol'
run search.md search --module smoke-app SbtClass
assert_contains search.md 'example.SbtClass'

step 'sbt --test: test-only dependency resolves'
run test-scope.md get --test --module smoke-app munit.FunSuite
assert_contains test-scope.md 'FunSuite'

step 'sbt --module is required'
run_expect_fail module.txt get example.SbtClass
assert_contains module.txt '--module is required for sbt'

echo
echo 'native smoke OK'

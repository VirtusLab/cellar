#!/usr/bin/env bash
# Runs a built cellar native binary against real artifacts and asserts on the
# printed Markdown. Meant to run on a machine that did NOT build the binary:
# anything baked in at image-build time (jrt java.home, embedded resources)
# only breaks away from the build filesystem (issue #133).
#
# Usage: native-smoke.sh <cellar-binary> [file://<local-m2-repo>]
# With a repo URL the assertions target the locally published test fixtures;
# without one they target published Maven Central artifacts.
set -euo pipefail

BINARY="$1"
REPO_URL="${2:-}"

# Nothing may resolve through an ambient JDK — a user's machine has none of
# the build runner's paths, so any lookup that escapes the binary is a bug.
unset JAVA_HOME

if [[ -n "$REPO_URL" ]]; then
  SCALA3_ARGS=(-r "$REPO_URL" cellar.test:cellar-fixture-scala3_3:0.1.0-SNAPSHOT cellar.fixture.scala3.CellarADT)
  SCALA3_DOC='Sealed ADT hierarchy for testing sealedChildren extraction.'
  JAVA_ARGS=(-r "$REPO_URL" cellar.test:cellar-fixture-java:0.1.0-SNAPSHOT cellar.fixture.java.CellarJavaClass)
  JAVA_SIG='class CellarJavaClass'
else
  SCALA3_ARGS=(org.typelevel:cats-effect_3:3.6.1 cats.effect.IO)
  SCALA3_DOC='A pure abstraction representing the intention'
  JAVA_ARGS=(com.google.guava:guava:33.4.0-jre com.google.common.collect.ImmutableSet)
  JAVA_SIG='class ImmutableSet'
fi

echo '--- cellar --version'
"$BINARY" --version | tee version.txt
grep -q 'native-image' version.txt

# Regular commands are withheld with exit 2 until a telemetry choice is
# recorded; a fresh runner has none.
echo '--- cellar telemetry disable --global'
"$BINARY" telemetry disable --global

# `! grep` would be ignored by set -e (negated pipelines never trigger exit),
# so the absence check needs an explicit failure branch.
assert_no_crash_report() {
  if grep -q 'Exception while compiling' "$1"; then
    echo "FAIL: compiler crash report leaked into $1" >&2
    exit 1
  fi
}

echo '--- cellar get-external (Scala 3 docstring)'
"$BINARY" get-external "${SCALA3_ARGS[@]}" | tee scala3.md
grep -qF "$SCALA3_DOC" scala3.md
assert_no_crash_report scala3.md

echo '--- cellar get-external (Java signature, bundled JRE)'
"$BINARY" get-external "${JAVA_ARGS[@]}" | tee java.md
grep -qF "$JAVA_SIG" java.md
assert_no_crash_report java.md

echo 'native smoke OK'

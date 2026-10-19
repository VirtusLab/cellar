#!/usr/bin/env bash
# Compares the signatures cellar prints in this working tree with those it prints at a base ref,
# and writes an HTML report of every signature whose printing changed.
#
# Usage: signatureBench/compare.sh <java|scala2|scala3> [base-ref]   (base-ref defaults to origin/main)
# Exits 1 when a signature that matched its source at the base no longer does, and 2 when a step
# fails.
set -euo pipefail
# Any other failure must not read as a regression.
trap 'exit 2' ERR

lang=${1:?usage: signatureBench/compare.sh <java|scala2|scala3> [base-ref]}
base=${2:-origin/main}
root=$(git rev-parse --show-toplevel)
out="$root/out/signatureBench/compare"
mkdir -p "$out"

# Runs a step with its output in a log, shown only when the step fails.
quietly() {
  local log=$1; shift
  "$@" > "$log" 2>&1 || { cat "$log"; exit 2; }
}

sha=$(git -C "$root" rev-parse --verify "$base^{commit}")
# Both sides run this checkout's benchmark, so the base result depends on it as much as on the
# base printer; the cache key covers both.
bench=$(cd "$root" && find signatureBench -type f -print0 | LC_ALL=C sort -z | xargs -0 git hash-object | git hash-object --stdin | cut -c1-16)
cache="${XDG_CACHE_HOME:-$HOME/.cache}/cellar-signature-bench"
mkdir -p "$cache"
base_json="$cache/$sha-$bench-$lang.json"

if [[ ! -f $base_json ]]; then
  echo "benchmarking $base (${sha:0:10}) in a temporary worktree..."
  tmp=$(mktemp -d)
  trap 'git -C "$root" worktree remove --force "$tmp/base" >/dev/null 2>&1 || true; rm -rf "$tmp"' EXIT
  git -C "$root" worktree add -q --detach "$tmp/base" "$sha"
  rm -rf "$tmp/base/signatureBench"
  cp -r "$root/signatureBench" "$tmp/base/signatureBench"
  # A base from before the benchmark moved to signatureBench/package.mill defines it in build.mill.
  awk '/^\/\*\* Compares cellar.s printed signatures/ || /^object signatureBench / { skip = 1 }
       skip && /^}/ { skip = 0; next }
       !skip' "$tmp/base/build.mill" > "$tmp/build.mill"
  mv "$tmp/build.mill" "$tmp/base/build.mill"
  (cd "$tmp/base" && quietly "$out/$lang-base.log" ./mill --no-daemon signatureBench.run "$lang" --json "$base_json.part")
  mv "$base_json.part" "$base_json"
fi

echo "benchmarking the working tree..."
(cd "$root" && quietly "$out/$lang-head.log" ./mill signatureBench.run "$lang" --json "$out/$lang-head.json")

(cd "$root" && quietly "$out/$lang-diff.log" ./mill signatureBench.run diff "$base_json" "$out/$lang-head.json" "$out/$lang-report.html")
summary=$(grep -E '^[[:alnum:]]+: [0-9]+ changed' "$out/$lang-diff.log" || true)
[[ -n $summary ]] || { cat "$out/$lang-diff.log"; echo "no summary in the diff output"; exit 2; }
echo "$summary"
echo "report: $out/$lang-report.html"

count() { sed -E "s/.*$1: ([0-9]+).*/\\1/" <<< "$summary"; }
[[ $(count regressed) == 0 && $(count "no longer paired") == 0 ]] || exit 1

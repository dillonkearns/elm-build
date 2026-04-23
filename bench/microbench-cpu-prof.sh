#!/usr/bin/env bash
# CPU-profile wrapper around the TCO microbench.
#
# Generates the same synthetic fixture as microbench-tco.sh, runs it
# under `node --cpu-prof`, and writes the resulting .cpuprofile into
# $OUT_DIR (default: /tmp/cpuprof). Caller can then run
# `node bench/analyze-cpuprofile.mjs <file>` to rank self-time frames.
#
# Companion to ~/.claude/plans/recursive-chasing-pearl.md Phase 1.
#
# Usage:
#
#   bash bench/microbench-cpu-prof.sh <label> [shape] [count]
#
# Defaults:
#   shape  = count-only
#   count  = 1000000
#
# Env vars:
#   CORE_EXTRA_DIR   path to core-extra (default: ~/src/github.com/elmcraft/core-extra)
#   OUT_DIR          where .cpuprofile files land (default: /tmp/cpuprof)
#   PROF_INTERVAL_US CPU-prof sampling interval in µs (default: 100)
#   N                overrides count arg (as in microbench-tco.sh)
#
# After the run, prints the .cpuprofile path and invokes the analyzer
# to dump the top 30 self-time frames to stdout. Save the analyzer
# output to `.scratch/` alongside the cpuprofile for cross-experiment
# comparison.

set -euo pipefail

if [ $# -lt 1 ]; then
  cat >&2 <<USAGE
usage: $0 <label> [shape] [count]

Generates tests/MicroBenchTco.elm in \$CORE_EXTRA_DIR, runs it once
under node --cpu-prof, and dumps the top 30 self-time frames. The raw
.cpuprofile is preserved in \$OUT_DIR for later diffing.

shape ∈ count-only, sum, isInfixOf   (default: count-only)
count = positive integer              (default: 1000000, or env \$N)
USAGE
  exit 2
fi

LABEL=$1
SHAPE=${2:-count-only}
COUNT=${N:-${3:-1000000}}

BUILD_DIR=$(cd "$(dirname "$0")/.." && pwd)
CORE_EXTRA_DIR=${CORE_EXTRA_DIR:-$HOME/src/github.com/elmcraft/core-extra}
RUNNER_ELM="$BUILD_DIR/src/TestRunner.elm"
BUNDLED_RUNNER="$BUILD_DIR/dist/TestRunner.mjs"
SYNTH_FILE="$CORE_EXTRA_DIR/tests/MicroBenchTco.elm"
OUT_DIR=${OUT_DIR:-/tmp/cpuprof}
PROF_INTERVAL_US=${PROF_INTERVAL_US:-100}
ANALYZER="$BUILD_DIR/bench/analyze-cpuprofile.mjs"

if [ ! -d "$CORE_EXTRA_DIR" ]; then
  echo "core-extra dir not found: $CORE_EXTRA_DIR" >&2
  exit 1
fi

# Bundle once to pick up any interpreter edits.
echo ">> bundling $RUNNER_ELM → $BUNDLED_RUNNER" >&2
(cd "$BUILD_DIR" && bunx elm-pages bundle-script src/TestRunner.elm --output "$BUNDLED_RUNNER" >&2) || {
  echo "bundle-script failed" >&2
  exit 1
}

echo ">> generating $SYNTH_FILE  (shape=$SHAPE, count=$COUNT)" >&2
node "$BUILD_DIR/bench/gen-synthetic-tco.mjs" \
  --count "$COUNT" \
  --shape "$SHAPE" \
  --out "$SYNTH_FILE"

# Use a per-label subdir so concurrent/successive runs don't clobber.
TIMESTAMP=$(date -u +%Y%m%dT%H%M%SZ)
PROFILE_DIR="$OUT_DIR/$LABEL-$TIMESTAMP"
mkdir -p "$PROFILE_DIR"

cd "$CORE_EXTRA_DIR"
find .elm-build -maxdepth 1 -type f ! -name 'package-*' -delete 2>/dev/null || true

echo "=== $LABEL  shape=$SHAPE  count=$COUNT  prof-interval=${PROF_INTERVAL_US}us" >&2
echo ">> profiling → $PROFILE_DIR" >&2

# --cpu-prof-interval is in microseconds. Lower values give finer
# resolution at the cost of larger .cpuprofile files.
NODE_OPTIONS="--cpu-prof --cpu-prof-interval=$PROF_INTERVAL_US --cpu-prof-dir=$PROFILE_DIR" \
  node "$BUNDLED_RUNNER" --test "tests/MicroBenchTco.elm" > "$PROFILE_DIR/stdout.log" 2>&1 || {
    echo "TestRunner errored — see $PROFILE_DIR/stdout.log" >&2
    tail -30 "$PROFILE_DIR/stdout.log" >&2
    exit 1
  }

DURATION_LINE=$(grep -E 'Duration: *[0-9]+ms' "$PROFILE_DIR/stdout.log" | head -1 || true)
echo ">> $DURATION_LINE" >&2

PROFILE_FILE=$(find "$PROFILE_DIR" -maxdepth 1 -type f -name '*.cpuprofile' | head -1)
if [ -z "$PROFILE_FILE" ]; then
  echo "no .cpuprofile file produced in $PROFILE_DIR" >&2
  exit 1
fi

echo ">> profile saved: $PROFILE_FILE" >&2

if [ -f "$ANALYZER" ]; then
  echo ""
  echo "=== Top frames for $LABEL ==="
  node "$ANALYZER" "$PROFILE_FILE" --top 40 | tee "$PROFILE_DIR/analysis.txt"
else
  echo "analyzer not found at $ANALYZER — raw profile is at $PROFILE_FILE" >&2
fi

echo ""
echo ">> artifacts:"
echo "     profile:  $PROFILE_FILE"
echo "     analysis: $PROFILE_DIR/analysis.txt"
echo "     stdout:   $PROFILE_DIR/stdout.log"

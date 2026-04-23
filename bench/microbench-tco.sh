#!/usr/bin/env bash
# Microbench harness for the interpreter's TCO hot path.
#
# Generates a synthetic single-test Elm file (countTo / sumTo / isInfixOf
# shape over `List.repeat N 5`), runs it through the bundled TestRunner
# once per iter, captures the `Duration: Xms` line, and reports the mean
# duration plus per-iteration nanoseconds across runs.
#
# Unlike `bench/testrunner-ab.sh`, this doesn't wrap total wall time —
# the Duration line is emitted by TestRunner.elm AFTER node startup
# + project load + TestRunner load, so the reported number isolates
# the eval phase. That's the piece experiments in
# ~/.claude/plans/recursive-chasing-pearl.md target.
#
# Usage:
#
#   bash bench/microbench-tco.sh <label> <runs> [shape] [count]
#
# Defaults:
#   shape  = count-only
#   count  = 1000000
#
# Env vars:
#   CORE_EXTRA_DIR   path to core-extra (default: ~/src/github.com/elmcraft/core-extra)
#                    Required because the synth file is written into
#                    core-extra/tests/ and uses core-extra's elm.json.
#   N                overrides the count arg (convenient for sweeps).
#
# Examples:
#
#   # Default: count-only, N=1M, 5 runs.
#   bash bench/microbench-tco.sh baseline 5
#
#   # Sweep N to verify linear scaling.
#   for N in 100000 500000 1000000 2000000 5000000; do
#     N=$N bash bench/microbench-tco.sh "baseline-$N" 3
#   done
#
#   # The actual isInfixOf workload.
#   bash bench/microbench-tco.sh baseline-isInfixOf 5 isInfixOf 1000000
#
# Appends a line to `bench/results/microbench-tco.tsv` with:
#   timestamp  label  shape  count  runs  mean_ms  stddev_ms  min_ms  max_ms  ns_per_iter
#
# ns_per_iter = mean_ms * 1_000_000 / count, the primary comparison metric.

set -euo pipefail

if [ $# -lt 2 ]; then
  cat >&2 <<USAGE
usage: $0 <label> <runs> [shape] [count]

Generates tests/MicroBenchTco.elm in \$CORE_EXTRA_DIR (default
~/src/github.com/elmcraft/core-extra), runs it <runs> times through
the bundled TestRunner, and reports mean Duration + ns/iter.

shape ∈ count-only, sum, isInfixOf   (default: count-only)
count = positive integer              (default: 1000000, or env \$N)
USAGE
  exit 2
fi

LABEL=$1
RUNS=$2
SHAPE=${3:-count-only}
COUNT=${N:-${4:-1000000}}

BUILD_DIR=$(cd "$(dirname "$0")/.." && pwd)
CORE_EXTRA_DIR=${CORE_EXTRA_DIR:-$HOME/src/github.com/elmcraft/core-extra}
RUNNER_ELM="$BUILD_DIR/src/TestRunner.elm"
BUNDLED_RUNNER="$BUILD_DIR/dist/TestRunner.mjs"
RESULTS_FILE="$BUILD_DIR/bench/results/microbench-tco.tsv"
SYNTH_FILE="$CORE_EXTRA_DIR/tests/MicroBenchTco.elm"

if [ ! -d "$CORE_EXTRA_DIR" ]; then
  echo "core-extra dir not found: $CORE_EXTRA_DIR" >&2
  exit 1
fi

if [ ! -f "$RUNNER_ELM" ]; then
  echo "TestRunner.elm not found at $RUNNER_ELM" >&2
  exit 1
fi

# Bundle once per invocation so experiments pick up elm-interpreter
# changes. Mirrors testrunner-ab.sh with BUNDLED=1.
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

mkdir -p "$BUILD_DIR/bench/results"
if [ ! -f "$RESULTS_FILE" ]; then
  printf 'timestamp\tlabel\tshape\tcount\truns\tmean_ms\tstddev_ms\tmin_ms\tmax_ms\tns_per_iter\n' > "$RESULTS_FILE"
fi

cd "$CORE_EXTRA_DIR"

# Warmup: one untimed run to fill page cache / JIT, and clear cold
# bits of .elm-build we want to re-run from scratch each iter.
find .elm-build -maxdepth 1 -type f ! -name 'package-*' -delete 2>/dev/null || true
node "$BUNDLED_RUNNER" --test "tests/MicroBenchTco.elm" > /dev/null 2>&1 || true

SAMPLES_FILE=$(mktemp)
trap 'rm -f "$SAMPLES_FILE"' EXIT

echo "=== $LABEL  shape=$SHAPE  count=$COUNT  runs=$RUNS" >&2
for i in $(seq 1 "$RUNS"); do
  # Clear non-package cache so the project-load phase isn't amortized
  # across runs — mirrors testrunner-ab.sh cache filter.
  find .elm-build -maxdepth 1 -type f ! -name 'package-*' -delete 2>/dev/null || true
  out=$(node "$BUNDLED_RUNNER" --test "tests/MicroBenchTco.elm" 2>&1 || true)
  duration=$(printf '%s\n' "$out" | grep -E 'Duration: *[0-9]+ms' | head -1 | sed -E 's/.*Duration: *([0-9]+)ms.*/\1/')
  if [ -z "$duration" ]; then
    echo "  run $i: NO Duration line found — TestRunner likely errored" >&2
    echo "$out" | tail -20 >&2
    exit 1
  fi
  printf '%s\n' "$duration" >> "$SAMPLES_FILE"
  ns_per_iter=$(python3 -c "print(int($duration * 1_000_000 / $COUNT))")
  printf '  run %2d: %6d ms   (%6d ns/iter)\n' "$i" "$duration" "$ns_per_iter" >&2
done

STATS=$(python3 - <<PY
import statistics
samples = [float(x.strip()) for x in open("$SAMPLES_FILE") if x.strip()]
mean = statistics.mean(samples)
stdev = statistics.stdev(samples) if len(samples) > 1 else 0.0
ns_per_iter = mean * 1_000_000 / $COUNT
print(f"{mean:.1f}\t{stdev:.1f}\t{min(samples):.1f}\t{max(samples):.1f}\t{ns_per_iter:.0f}")
PY
)
read -r MEAN STDDEV MIN MAX NS_PER_ITER <<<"$STATS"

printf '=== %s  mean=%s ms  sd=%s  min=%s  max=%s  ns/iter=%s\n' \
  "$LABEL" "$MEAN" "$STDDEV" "$MIN" "$MAX" "$NS_PER_ITER" >&2

TIMESTAMP=$(date -u +%Y-%m-%dT%H:%M:%SZ)
printf '%s\t%s\t%s\t%d\t%d\t%s\t%s\t%s\t%s\t%s\n' \
  "$TIMESTAMP" "$LABEL" "$SHAPE" "$COUNT" "$RUNS" "$MEAN" "$STDDEV" "$MIN" "$MAX" "$NS_PER_ITER" \
  >> "$RESULTS_FILE"

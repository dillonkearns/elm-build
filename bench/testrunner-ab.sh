#!/usr/bin/env bash
# Data-driven A/B benchmark harness for the interpreter's cold-user path.
#
# Runs `npx elm-pages run src/TestRunner.elm --test <files>` N times, records wall time per
# run, and reports mean / stddev / min / max. Unlike `hyperfine` with the
# elm-test side-by-side, this never hangs on orphaned elm-test workers —
# it only invokes the interpreter. Compare against elm-test separately
# with `bash bench/core-extra-8-file-subset.sh` when you want the ratio.
#
# Between runs the same cache filter the 8-file subset uses applies:
# `.elm-build/package-*` stays warm, everything else goes cold.
#
# Usage:
#
#   bash bench/testrunner-ab.sh \
#       <label> \
#       <core-extra-path> \
#       <runs> \
#       <comma-separated-test-files> \
#       [env KEY=VAL ...]
#
# Examples:
#
#   # 8-file subset, 10 runs, default flags:
#   bash bench/testrunner-ab.sh default ~/src/github.com/elmcraft/core-extra 10 \
#     tests/BasicsTests.elm,tests/CharTests.elm,tests/DictTests.elm,tests/FloatTests.elm,tests/MaybeTests.elm,tests/OrderTests.elm,tests/ResultTests.elm,tests/SetTests.elm
#
# Appends a single line to `bench/results/testrunner-ab.tsv` with the
# columns:
#
#   timestamp  label  runs  mean_ms  stddev_ms  min_ms  max_ms  files  env

set -euo pipefail

if [ $# -lt 4 ]; then
  cat >&2 <<USAGE
usage: $0 <label> <core-extra-path> <runs> <comma-separated-test-files> [env KEY=VAL ...]

Wrap any optional env KEY=VAL pairs after the required args so they're
forwarded to the child process only.
USAGE
  exit 2
fi

LABEL=$1
CORE_EXTRA_DIR=$2
RUNS=$3
TESTS_COMMA=$4
shift 4

BUILD_DIR=$(cd "$(dirname "$0")/.." && pwd)
RUNNER_ELM="$BUILD_DIR/src/TestRunner.elm"
RESULTS_FILE="$BUILD_DIR/bench/results/testrunner-ab.tsv"

if [ ! -f "$RUNNER_ELM" ]; then
  echo "TestRunner.elm not found at $RUNNER_ELM" >&2
  exit 1
fi

mkdir -p "$BUILD_DIR/bench/results"
if [ ! -f "$RESULTS_FILE" ]; then
  printf 'timestamp\tlabel\truns\tmean_ms\tstddev_ms\tmin_ms\tmax_ms\tfiles\tenv\n' > "$RESULTS_FILE"
fi

ENV_PREFIX=""
ENV_NOTE=""
for kv in "$@"; do
  ENV_PREFIX="$ENV_PREFIX $kv"
  ENV_NOTE="${ENV_NOTE:+$ENV_NOTE }$kv"
done

cd "$CORE_EXTRA_DIR"

# Warmup: one untimed run to fill page cache / JIT.
find .elm-build -maxdepth 1 -type f ! -name 'package-*' -delete
env $ENV_PREFIX npx elm-pages run "$RUNNER_ELM" --test "$TESTS_COMMA" > /dev/null 2>&1 || true

SAMPLES_FILE=$(mktemp)
trap 'rm -f "$SAMPLES_FILE"' EXIT

echo "=== $LABEL  ($RUNS runs, $ENV_NOTE)" >&2
for i in $(seq 1 "$RUNS"); do
  find .elm-build -maxdepth 1 -type f ! -name 'package-*' -delete
  t0=$(python3 -c 'import time; print(int(time.monotonic_ns()))')
  env $ENV_PREFIX npx elm-pages run "$RUNNER_ELM" --test "$TESTS_COMMA" > /dev/null 2>&1
  t1=$(python3 -c 'import time; print(int(time.monotonic_ns()))')
  elapsed_ms=$(python3 -c "print(($t1 - $t0) / 1_000_000)")
  printf '%s\n' "$elapsed_ms" >> "$SAMPLES_FILE"
  printf '  run %2d: %7.1f ms\n' "$i" "$elapsed_ms" >&2
done

STATS=$(python3 - <<PY
import statistics
samples = [float(x.strip()) for x in open("$SAMPLES_FILE") if x.strip()]
mean = statistics.mean(samples)
stdev = statistics.stdev(samples) if len(samples) > 1 else 0.0
print(f"{mean:.1f}\t{stdev:.1f}\t{min(samples):.1f}\t{max(samples):.1f}")
PY
)
read -r MEAN STDDEV MIN MAX <<<"$STATS"

printf '=== %s  mean=%s ms  sd=%s  min=%s  max=%s\n' \
  "$LABEL" "$MEAN" "$STDDEV" "$MIN" "$MAX" >&2

TIMESTAMP=$(date -u +%Y-%m-%dT%H:%M:%SZ)
printf '%s\t%s\t%d\t%s\t%s\t%s\t%s\t%s\t%s\n' \
  "$TIMESTAMP" "$LABEL" "$RUNS" "$MEAN" "$STDDEV" "$MIN" "$MAX" "$TESTS_COMMA" "$ENV_NOTE" \
  >> "$RESULTS_FILE"

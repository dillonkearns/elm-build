#!/usr/bin/env bash
# Interleaved A/B benchmark harness for the interpreter's cold-user path.
#
# Compares two already-built test-runner bundles by alternating their run
# order across pairs while keeping package caches warm in separate build dirs.
# This reduces clock drift when the expected delta is small.
#
# Usage:
#   bash bench/testrunner-interleaved-ab.sh \
#     <label-a> <script-a> <build-dir-a> \
#     <label-b> <script-b> <build-dir-b> \
#     <core-extra-path> <pairs> <comma-separated-test-files>
#
# The script appends one summary row per variant to
# `bench/results/testrunner-interleaved-ab.tsv`.

set -euo pipefail

if [ $# -ne 9 ]; then
  cat >&2 <<USAGE
usage: $0 <label-a> <script-a> <build-dir-a> <label-b> <script-b> <build-dir-b> <core-extra-path> <pairs> <comma-separated-test-files>
USAGE
  exit 2
fi

LABEL_A=$1
SCRIPT_A=$2
BUILD_A=$3
LABEL_B=$4
SCRIPT_B=$5
BUILD_B=$6
CORE_EXTRA_DIR=$7
PAIRS=$8
TESTS_COMMA=$9

REPO_ROOT=$(cd "$(dirname "$0")/.." && pwd)
RESULTS_FILE="$REPO_ROOT/bench/results/testrunner-interleaved-ab.tsv"

mkdir -p "$REPO_ROOT/bench/results"
if [ ! -f "$RESULTS_FILE" ]; then
  printf 'timestamp\tlabel\tpairs\tmean_ms\tstddev_ms\tmin_ms\tmax_ms\tmean_pair_delta_ms\tfiles\tscript\tbuild_dir\n' > "$RESULTS_FILE"
fi

if [ ! -f "$SCRIPT_A" ]; then
  echo "script A not found: $SCRIPT_A" >&2
  exit 1
fi

if [ ! -f "$SCRIPT_B" ]; then
  echo "script B not found: $SCRIPT_B" >&2
  exit 1
fi

cleanup_build_dir() {
  local build_dir=$1
  mkdir -p "$build_dir"
  find "$build_dir" -mindepth 1 -maxdepth 1 ! -name 'package-*' -exec rm -rf {} +
}

run_one() {
  local script_path=$1
  local build_dir=$2

  cleanup_build_dir "$build_dir"
  local t0
  local t1
  t0=$(python3 -c 'import time; print(int(time.monotonic_ns()))')
  node --stack-size=8192 "$script_path" --build "$build_dir" --test "$TESTS_COMMA" > /dev/null 2>&1
  t1=$(python3 -c 'import time; print(int(time.monotonic_ns()))')
  python3 -c "print(($t1 - $t0) / 1_000_000)"
}

cd "$CORE_EXTRA_DIR"

# Warm package caches separately.
cleanup_build_dir "$BUILD_A"
node --stack-size=8192 "$SCRIPT_A" --build "$BUILD_A" --test "$TESTS_COMMA" > /dev/null 2>&1 || true
cleanup_build_dir "$BUILD_B"
node --stack-size=8192 "$SCRIPT_B" --build "$BUILD_B" --test "$TESTS_COMMA" > /dev/null 2>&1 || true

SAMPLES_A=$(mktemp)
SAMPLES_B=$(mktemp)
PAIR_DELTAS_A=$(mktemp)
PAIR_DELTAS_B=$(mktemp)
trap 'rm -f "$SAMPLES_A" "$SAMPLES_B" "$PAIR_DELTAS_A" "$PAIR_DELTAS_B"' EXIT

echo "=== $LABEL_A vs $LABEL_B  ($PAIRS pairs)" >&2
for i in $(seq 1 "$PAIRS"); do
  if [ $((i % 2)) -eq 1 ]; then
    FIRST_LABEL=$LABEL_A
    FIRST_SCRIPT=$SCRIPT_A
    FIRST_BUILD=$BUILD_A
    SECOND_LABEL=$LABEL_B
    SECOND_SCRIPT=$SCRIPT_B
    SECOND_BUILD=$BUILD_B
  else
    FIRST_LABEL=$LABEL_B
    FIRST_SCRIPT=$SCRIPT_B
    FIRST_BUILD=$BUILD_B
    SECOND_LABEL=$LABEL_A
    SECOND_SCRIPT=$SCRIPT_A
    SECOND_BUILD=$BUILD_A
  fi

  FIRST_MS=$(run_one "$FIRST_SCRIPT" "$FIRST_BUILD")
  SECOND_MS=$(run_one "$SECOND_SCRIPT" "$SECOND_BUILD")

  if [ "$FIRST_LABEL" = "$LABEL_A" ]; then
    A_MS=$FIRST_MS
    B_MS=$SECOND_MS
  else
    A_MS=$SECOND_MS
    B_MS=$FIRST_MS
  fi

  printf '%s\n' "$A_MS" >> "$SAMPLES_A"
  printf '%s\n' "$B_MS" >> "$SAMPLES_B"
  python3 -c "print($A_MS - $B_MS)" >> "$PAIR_DELTAS_A"
  python3 -c "print($B_MS - $A_MS)" >> "$PAIR_DELTAS_B"

  printf '  pair %2d: %-24s %7.1f ms | %-24s %7.1f ms | delta(A-B) %+7.1f ms\n' \
    "$i" "$LABEL_A" "$A_MS" "$LABEL_B" "$B_MS" "$(python3 -c "print($A_MS - $B_MS)")" >&2
done

summarize() {
  local samples_file=$1
  local delta_file=$2
  python3 - <<PY
import statistics
samples = [float(x.strip()) for x in open("$samples_file") if x.strip()]
deltas = [float(x.strip()) for x in open("$delta_file") if x.strip()]
mean = statistics.mean(samples)
stdev = statistics.stdev(samples) if len(samples) > 1 else 0.0
delta_mean = statistics.mean(deltas) if deltas else 0.0
print(f"{mean:.1f}\t{stdev:.1f}\t{min(samples):.1f}\t{max(samples):.1f}\t{delta_mean:.1f}")
PY
}

read -r MEAN_A STDDEV_A MIN_A MAX_A DELTA_A <<< "$(summarize "$SAMPLES_A" "$PAIR_DELTAS_A")"
read -r MEAN_B STDDEV_B MIN_B MAX_B DELTA_B <<< "$(summarize "$SAMPLES_B" "$PAIR_DELTAS_B")"

printf '=== %-24s mean=%s ms sd=%s min=%s max=%s mean_pair_delta=%s ms\n' \
  "$LABEL_A" "$MEAN_A" "$STDDEV_A" "$MIN_A" "$MAX_A" "$DELTA_A" >&2
printf '=== %-24s mean=%s ms sd=%s min=%s max=%s mean_pair_delta=%s ms\n' \
  "$LABEL_B" "$MEAN_B" "$STDDEV_B" "$MIN_B" "$MAX_B" "$DELTA_B" >&2

TIMESTAMP=$(date -u +%Y-%m-%dT%H:%M:%SZ)
printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
  "$TIMESTAMP" "$LABEL_A" "$PAIRS" "$MEAN_A" "$STDDEV_A" "$MIN_A" "$MAX_A" "$DELTA_A" "$TESTS_COMMA" "$SCRIPT_A" "$BUILD_A" \
  >> "$RESULTS_FILE"
printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
  "$TIMESTAMP" "$LABEL_B" "$PAIRS" "$MEAN_B" "$STDDEV_B" "$MIN_B" "$MAX_B" "$DELTA_B" "$TESTS_COMMA" "$SCRIPT_B" "$BUILD_B" \
  >> "$RESULTS_FILE"

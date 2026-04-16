#!/usr/bin/env bash
# Benchmark: TestRunner (warm deps, cold user) vs elm-test on
# dillonkearns/elm-markdown.
#
# This workload is currently timeout-prone for the interpreter, so this
# harness is timeout-aware and records both successful timings and
# timeouts instead of assuming every run completes.
#
# Usage:
#   bash bench/elm-markdown-ab.sh /abs/path/to/elm-markdown [runs] [test-file|auto] [timeout-secs] [runner-args...]
#
# Examples:
#   bash bench/elm-markdown-ab.sh /tmp/elm-markdown-fresh-2026-04-15
#   bash bench/elm-markdown-ab.sh /tmp/elm-markdown-fresh-2026-04-15 3 tests/Tests.elm 60
#   bash bench/elm-markdown-ab.sh /tmp/elm-markdown-fresh-2026-04-15 3 auto 60
#   bash bench/elm-markdown-ab.sh /tmp/elm-markdown-fresh-2026-04-15 3 tests/Tests.elm 60 --user-normalization-experiment fixpoint-off

set -euo pipefail

MARKDOWN_DIR=${1:?"usage: $0 /abs/path/to/elm-markdown [runs] [test-file] [timeout-secs]"}
RUNS=${2:-1}
TEST_FILE=${3:-tests/HelpersTests.elm}
TIMEOUT_SECS=${4:-60}
if [ $# -gt 4 ]; then
  RUNNER_EXTRA_ARGS=("${@:5}")
else
  RUNNER_EXTRA_ARGS=()
fi

BUILD_DIR=$(cd "$(dirname "$0")/.." && pwd)
RUNNER_ELM="$BUILD_DIR/src/TestRunner.elm"
RESULTS_FILE="$BUILD_DIR/bench/results/elm-markdown-ab.tsv"

if [ ! -f "$RUNNER_ELM" ]; then
  echo "TestRunner.elm not found at $RUNNER_ELM" >&2
  exit 1
fi

mkdir -p "$BUILD_DIR/bench/results"
if [ ! -f "$RESULTS_FILE" ]; then
  printf 'timestamp\tlabel\truns\ttest_file\ttimeout_secs\tok_runs\ttimeouts\tmean_ms\tmin_ms\tmax_ms\n' > "$RESULTS_FILE"
fi

run_with_timeout() {
  local label=$1
  local cmd_json=$2

  python3 - "$label" "$cmd_json" "$TIMEOUT_SECS" <<'PY'
import json
import subprocess
import sys
import time

label = sys.argv[1]
cmd = json.loads(sys.argv[2])
timeout_secs = float(sys.argv[3])
t0 = time.monotonic()

try:
    completed = subprocess.run(
        cmd,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
        timeout=timeout_secs,
        check=False,
    )
    elapsed_ms = (time.monotonic() - t0) * 1000
    if completed.returncode == 0:
        print(f"{label}\tok\t{elapsed_ms:.1f}\t{completed.returncode}")
    else:
        print(f"{label}\terror\t{elapsed_ms:.1f}\t{completed.returncode}")
except subprocess.TimeoutExpired:
    elapsed_ms = (time.monotonic() - t0) * 1000
    print(f"{label}\ttimeout\t{elapsed_ms:.1f}\t124")
PY
}

summarize_samples() {
  local label=$1
  local samples_file=$2

  python3 - "$label" "$samples_file" "$RESULTS_FILE" "$RUNS" "$TEST_FILE" "$TIMEOUT_SECS" <<'PY'
import statistics
import sys
from datetime import datetime, timezone

label = sys.argv[1]
samples_file = sys.argv[2]
results_file = sys.argv[3]
runs = sys.argv[4]
test_file = sys.argv[5]
timeout_secs = sys.argv[6]

rows = []
with open(samples_file) as f:
    for line in f:
        parts = line.strip().split("\t")
        if len(parts) == 4:
            rows.append(parts)

ok_rows = [row for row in rows if row[1] == "ok"]
timeout_rows = [row for row in rows if row[1] == "timeout"]
ok_times = [float(row[2]) for row in ok_rows]

if ok_times:
    mean_ms = f"{statistics.mean(ok_times):.1f}"
    min_ms = f"{min(ok_times):.1f}"
    max_ms = f"{max(ok_times):.1f}"
else:
    mean_ms = "NA"
    min_ms = "NA"
    max_ms = "NA"

print(f"{label}: ok={len(ok_rows)} timeout={len(timeout_rows)} mean={mean_ms} min={min_ms} max={max_ms}")

timestamp = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
with open(results_file, "a") as out:
    out.write(
        "\t".join(
            [
                timestamp,
                label,
                runs,
                test_file,
                timeout_secs,
                str(len(ok_rows)),
                str(len(timeout_rows)),
                mean_ms,
                min_ms,
                max_ms,
            ]
        )
        + "\n"
    )
PY
}

cd "$MARKDOWN_DIR"

TEST_RUNNER_SAMPLES=$(mktemp)
ELM_TEST_SAMPLES=$(mktemp)
trap 'rm -f "$TEST_RUNNER_SAMPLES" "$ELM_TEST_SAMPLES"' EXIT

if [ ${#RUNNER_EXTRA_ARGS[@]} -gt 0 ]; then
TEST_RUNNER_CMD=$(python3 - "$RUNNER_ELM" "$TEST_FILE" "${RUNNER_EXTRA_ARGS[@]}" <<'PY'
import json
import os
import sys

runner = os.path.abspath(sys.argv[1])
test_file = sys.argv[2]
extra_args = sys.argv[3:]
cmd = ["npx", "elm-pages", "run", runner]
cmd.extend(extra_args)
if test_file != "auto":
    cmd.extend(["--test", test_file])
print(json.dumps(cmd))
PY
)
else
TEST_RUNNER_CMD=$(python3 - "$RUNNER_ELM" "$TEST_FILE" <<'PY'
import json
import os
import sys

runner = os.path.abspath(sys.argv[1])
test_file = sys.argv[2]
cmd = ["npx", "elm-pages", "run", runner]
if test_file != "auto":
    cmd.extend(["--test", test_file])
print(json.dumps(cmd))
PY
)
fi

ELM_TEST_CMD=$(python3 - "$TEST_FILE" <<'PY'
import json
import sys

test_file = sys.argv[1]
cmd = ["elm-test"]
if test_file != "auto":
    cmd.append(test_file)
print(json.dumps(cmd))
PY
)

# Prime package caches once so subsequent TestRunner runs match the
# "warm deps, cold user" scenario. If this times out, the package cache
# artifacts created during load still remain.
find .elm-build -maxdepth 1 -type f ! -name 'package-*' -delete 2>/dev/null || true
run_with_timeout "TestRunner-prime" "$TEST_RUNNER_CMD" >/dev/null || true

echo "=== elm-markdown benchmark ($RUNS runs, test=$TEST_FILE, timeout=${TIMEOUT_SECS}s)" >&2

for i in $(seq 1 "$RUNS"); do
  find .elm-build -maxdepth 1 -type f ! -name 'package-*' -delete 2>/dev/null || true
  result=$(run_with_timeout "TestRunner" "$TEST_RUNNER_CMD")
  printf '%s\n' "$result" >> "$TEST_RUNNER_SAMPLES"
  printf '%s\n' "$result" >&2
done

for i in $(seq 1 "$RUNS"); do
  rm -rf elm-stuff
  result=$(run_with_timeout "elm-test" "$ELM_TEST_CMD")
  printf '%s\n' "$result" >> "$ELM_TEST_SAMPLES"
  printf '%s\n' "$result" >&2
done

if grep -q $'\terror\t' "$TEST_RUNNER_SAMPLES" "$ELM_TEST_SAMPLES"; then
  echo "Benchmark command failed; refusing to record results." >&2
  exit 1
fi

summarize_samples "TestRunner" "$TEST_RUNNER_SAMPLES" >&2
summarize_samples "elm-test" "$ELM_TEST_SAMPLES" >&2

#!/usr/bin/env bash
# Chunking parameter sweep harness with hang detection.
#
# Driver for the investigation in `~/.claude/plans/recursive-chasing-pearl.md`.
# Each scenario sets ELM_BUILD_TARGET_CHUNK_COUNT, ELM_BUILD_PER_CHILD_SPLIT_THRESHOLD,
# ELM_BUILD_POOL_SIZE for one run of `npx elm-pages run src/TestRunner.elm`.
# Successful cells get N iterations; HANG cells get 1 iteration (reproducible).
#
# Usage:
#   bash bench/chunking-sweep.sh <label> <core-extra-dir> [runs=5] [hang_timeout_s=60] < scenarios.tsv
#
# Scenarios on stdin, one per line, TSV columns:
#   chunk_count  per_child_split  pool_size  suite_files
#
# `suite_files` is comma-separated, relative to <core-extra-dir>. Lines
# starting with `#` are ignored; blank lines are ignored.
#
# Example:
#   echo $'4\t10\t2\ttests/SyntheticBigDescribe.elm' | \
#     bash bench/chunking-sweep.sh probe-1 /tmp/core-extra 5 60
#
# Output appended to bench/results/chunking-sweep.tsv with columns:
#   timestamp label chunk_count per_child_split pool_size iters status \
#   mean_ms sd_ms min_ms max_ms suite_files

set -euo pipefail

if [ $# -lt 2 ]; then
    cat >&2 <<USAGE
usage: $0 <label> <core-extra-dir> [runs=5] [hang_timeout_s=60] < scenarios.tsv

Reads scenarios on stdin (TSV: chunk_count per_child_split pool_size suite_files).
Appends to bench/results/chunking-sweep.tsv.
USAGE
    exit 2
fi

LABEL=$1
CORE_EXTRA_DIR=$2
RUNS=${3:-5}
HANG_TIMEOUT_S=${4:-60}

BUILD_DIR=$(cd "$(dirname "$0")/.." && pwd)
RUNNER_ELM="$BUILD_DIR/src/TestRunner.elm"
RESULTS_FILE="$BUILD_DIR/bench/results/chunking-sweep.tsv"

if [ ! -f "$RUNNER_ELM" ]; then
    echo "TestRunner.elm not found at $RUNNER_ELM" >&2
    exit 1
fi

mkdir -p "$BUILD_DIR/bench/results"
if [ ! -f "$RESULTS_FILE" ]; then
    printf 'timestamp\tlabel\tchunk_count\tper_child_split\tpool_size\titers\tstatus\tmean_ms\tsd_ms\tmin_ms\tmax_ms\tsuite_files\n' \
        > "$RESULTS_FILE"
fi

# Cleanup leftover workers between scenarios. Kept liberal — per
# `recursive-chasing-pearl.md` 0.3 worker processes can outlive a hung
# parent.
reap_workers() {
    pkill -f '.elm-pages-parallel-worker.mjs' 2>/dev/null || true
    pkill -f 'elm-pages run.*TestRunner.elm' 2>/dev/null || true
}

cd "$CORE_EXTRA_DIR"

# Run one iteration of one scenario. Echoes wall_ms or "TIMEOUT" to stdout.
# Args: chunk_count per_child_split pool_size suite_files
run_one_iter() {
    local cc=$1
    local pcs=$2
    local ps=$3
    local suite=$4

    find .elm-build -maxdepth 1 -type f ! -name 'package-*' -delete 2>/dev/null || true

    local t0
    t0=$(python3 -c 'import time; print(int(time.monotonic_ns()))')

    # perl's `alarm` sends SIGALRM after HANG_TIMEOUT_S; its `exec`
    # replaces perl with npx, but the alarm timer persists across exec
    # per POSIX. When it fires, npx (now in perl's PID slot) terminates
    # with SIGALRM (exit 142). macOS lacks `setsid` by default, so we
    # don't isolate process groups; reap stragglers via `pkill` after.
    set +e
    env \
        ELM_BUILD_TARGET_CHUNK_COUNT="$cc" \
        ELM_BUILD_PER_CHILD_SPLIT_THRESHOLD="$pcs" \
        ELM_BUILD_POOL_SIZE="$ps" \
        perl -e 'alarm shift @ARGV; exec @ARGV' \
            "$HANG_TIMEOUT_S" \
            npx elm-pages run "$RUNNER_ELM" --test "$suite" \
        > /dev/null 2>&1
    local ec=$?
    set -e

    local t1
    t1=$(python3 -c 'import time; print(int(time.monotonic_ns()))')
    local elapsed_ms
    elapsed_ms=$(python3 -c "print(($t1 - $t0) / 1_000_000)")

    # Reap any orphaned workers that outlived the parent kill.
    reap_workers

    # SIGALRM exit = 142 (128 + 14). Anything else non-zero is an
    # actual command failure — keep it distinct.
    if [ "$ec" -eq 142 ]; then
        echo "TIMEOUT"
    elif [ "$ec" -ne 0 ]; then
        echo "ERROR:$ec:$elapsed_ms"
    else
        printf '%.1f\n' "$elapsed_ms"
    fi
}

# Run a full scenario: warmup + N iters, write TSV row.
# Args: chunk_count per_child_split pool_size suite_files
run_scenario() {
    local cc=$1
    local pcs=$2
    local ps=$3
    local suite=$4

    echo "=== scenario: K=$cc, T=$pcs, P=$ps, suite=$suite" >&2

    # Warmup (best-effort; a hung warmup means the scenario will hang too)
    echo "  warmup..." >&2
    local warm
    warm=$(run_one_iter "$cc" "$pcs" "$ps" "$suite")

    if [ "$warm" = "TIMEOUT" ]; then
        echo "  warmup TIMEOUT — recording HANG, skipping iters" >&2
        local ts
        ts=$(date -u +%Y-%m-%dT%H:%M:%SZ)
        printf '%s\t%s\t%d\t%d\t%d\t%d\t%s\t%s\t%s\t%s\t%s\t%s\n' \
            "$ts" "$LABEL" "$cc" "$pcs" "$ps" 1 HANG TIMEOUT TIMEOUT TIMEOUT TIMEOUT "$suite" \
            >> "$RESULTS_FILE"
        return
    fi

    local samples=()
    local status=OK
    for i in $(seq 1 "$RUNS"); do
        local r
        r=$(run_one_iter "$cc" "$pcs" "$ps" "$suite")
        if [ "$r" = "TIMEOUT" ]; then
            echo "  iter $i: TIMEOUT" >&2
            status=HANG
            samples=(TIMEOUT)
            break
        fi
        case "$r" in
            ERROR:*)
                echo "  iter $i: $r" >&2
                status=ERROR
                samples+=("$r")
                break
                ;;
            *)
                echo "  iter $i: $r ms" >&2
                samples+=("$r")
                ;;
        esac
    done

    local ts
    ts=$(date -u +%Y-%m-%dT%H:%M:%SZ)

    if [ "$status" = "OK" ]; then
        local stats
        stats=$(printf '%s\n' "${samples[@]}" | python3 -c 'import sys, statistics
xs = [float(line) for line in sys.stdin if line.strip()]
mean = statistics.mean(xs)
sd = statistics.stdev(xs) if len(xs) > 1 else 0.0
print(f"{mean:.1f}\t{sd:.1f}\t{min(xs):.1f}\t{max(xs):.1f}")
')
        local mean sd lo hi
        IFS=$'\t' read -r mean sd lo hi <<< "$stats"
        printf '%s\t%s\t%d\t%d\t%d\t%d\t%s\t%s\t%s\t%s\t%s\t%s\n' \
            "$ts" "$LABEL" "$cc" "$pcs" "$ps" "${#samples[@]}" OK \
            "$mean" "$sd" "$lo" "$hi" "$suite" \
            >> "$RESULTS_FILE"
        echo "=== K=$cc T=$pcs P=$ps  mean=${mean} sd=${sd}  iters=${#samples[@]}" >&2
    else
        printf '%s\t%s\t%d\t%d\t%d\t%d\t%s\t%s\t%s\t%s\t%s\t%s\n' \
            "$ts" "$LABEL" "$cc" "$pcs" "$ps" "${#samples[@]}" "$status" \
            "${samples[0]}" '-' '-' '-' "$suite" \
            >> "$RESULTS_FILE"
        echo "=== K=$cc T=$pcs P=$ps  status=$status" >&2
    fi
}

# Read scenarios from stdin, one per line.
while IFS=$'\t' read -r cc pcs ps suite; do
    case "$cc" in
        ''|\#*) continue ;;
    esac
    run_scenario "$cc" "$pcs" "$ps" "$suite"
done

echo "" >&2
echo "Done. Results in $RESULTS_FILE" >&2

#!/usr/bin/env python3
"""Run review-runner-benchmark N times and report median/MAD/range per scenario.

Usage:
    python3 bench/run-bench-n.py [N] [LABEL] [--fixture FIXTURE]

Examples:
    python3 bench/run-bench-n.py 5
    python3 bench/run-bench-n.py 5 baseline-58c45fe
    python3 bench/run-bench-n.py 3 post-dispatch-consolidation

N defaults to 5. LABEL defaults to the current git HEAD short SHA.

Writes each sample's scenario-level wall/internal to
.scratch/bench-samples-<timestamp>.json and prints a median summary to stdout.
Passes --skip-bundle so the caller is responsible for `bunx elm-pages bundle-script
src/ReviewRunner.elm --output dist/review-runner-bench.mjs` before running.
"""
import argparse
import json
import os
import re
import statistics
import subprocess
import sys
import time
from pathlib import Path

SCENARIOS = [
    "cold",
    "warm",
    "warm_1_file_body_edit",
    "warm_1_file_comment_only",
    "warm_import_graph_change",
]

# Row format in benchmark output:
#   <scenario name>  <wall_sec>  <internal_sec>  <decision>
# e.g.:
#   cold                        299.10      269.77  cold_miss
ROW_RE = re.compile(
    r"^(?P<scenario>\S+)\s+(?P<wall>\d+\.\d+)\s+(?P<internal>\d+\.\d+)\s+\S+\s*$"
)


def run_one(fixture: str, env_mode: str) -> dict:
    """Run one benchmark pass and return {scenario: {wall, internal}}."""
    result = subprocess.run(
        [
            "node",
            "bench/review-runner-benchmark.mjs",
            "--fixture",
            fixture,
            "--skip-bundle",
            "--env-mode",
            env_mode,
        ],
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        print(result.stdout, file=sys.stderr)
        print(result.stderr, file=sys.stderr)
        raise SystemExit(f"Benchmark failed with exit {result.returncode}")

    scenarios: dict = {}
    for line in result.stdout.splitlines():
        m = ROW_RE.match(line)
        if not m:
            continue
        scenarios[m.group("scenario")] = {
            "wall": float(m.group("wall")),
            "internal": float(m.group("internal")),
        }
    missing = [s for s in SCENARIOS if s not in scenarios]
    if missing:
        print(result.stdout, file=sys.stderr)
        raise SystemExit(f"Could not parse scenarios: {missing}")
    return scenarios


def summarize(samples: list[float]) -> dict:
    xs = sorted(samples)
    median = statistics.median(xs)
    mad = statistics.median([abs(x - median) for x in xs])
    return {
        "n": len(xs),
        "median": median,
        "mad": mad,
        "min": xs[0],
        "max": xs[-1],
        "range": xs[-1] - xs[0],
        "range_pct": (xs[-1] - xs[0]) / median * 100 if median else 0,
        "samples": xs,
    }


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("n", type=int, nargs="?", default=5, help="number of samples")
    ap.add_argument("label", nargs="?", help="label for this run (defaults to HEAD SHA)")
    ap.add_argument("--fixture", default="small-12", help="fixture name")
    ap.add_argument(
        "--env-mode",
        default="legacy-ast",
        choices=["legacy-ast", "resolved-list-unplanned", "resolved-list-slotted"],
        help="Interpreter env representation mode to pass through to the runner (default: legacy-ast)",
    )
    args = ap.parse_args()

    if not args.label:
        try:
            args.label = subprocess.run(
                ["git", "rev-parse", "--short", "HEAD"],
                capture_output=True,
                text=True,
                check=True,
            ).stdout.strip()
        except Exception:
            args.label = "unlabeled"

    print(
        f"Running {args.n} samples on fixture={args.fixture} env-mode={args.env_mode} label={args.label}",
        file=sys.stderr,
    )
    print(
        "Reminder: bundle must be fresh — "
        "`bunx elm-pages bundle-script src/ReviewRunner.elm --output dist/review-runner-bench.mjs`",
        file=sys.stderr,
    )

    samples = []
    t0 = time.time()
    for i in range(args.n):
        print(f"  [{i+1}/{args.n}] running...", file=sys.stderr, flush=True)
        scenarios = run_one(args.fixture, args.env_mode)
        samples.append(scenarios)
        for s in SCENARIOS:
            print(
                f"    {s:30} {scenarios[s]['wall']:>10.2f}s wall  {scenarios[s]['internal']:>10.2f}s internal",
                file=sys.stderr,
            )
    elapsed = time.time() - t0
    print(f"Done in {elapsed:.0f}s", file=sys.stderr)

    # Aggregate per scenario
    agg = {}
    for s in SCENARIOS:
        agg[s] = {
            "wall": summarize([x[s]["wall"] for x in samples]),
            "internal": summarize([x[s]["internal"] for x in samples]),
        }

    # Persist to .scratch
    scratch_dir = Path(".scratch")
    scratch_dir.mkdir(exist_ok=True)
    timestamp = time.strftime("%Y%m%d-%H%M%S")
    out_path = scratch_dir / f"bench-samples-{timestamp}-{args.label}.json"
    out_path.write_text(
        json.dumps(
            {
                "label": args.label,
                "fixture": args.fixture,
                "env_mode": args.env_mode,
                "n": args.n,
                "elapsed_seconds": elapsed,
                "samples": samples,
                "aggregate": agg,
            },
            indent=2,
        )
    )
    print(f"Wrote {out_path}", file=sys.stderr)

    # Print a clean summary to stdout
    print(f"\n== {args.label} n={args.n} fixture={args.fixture} env-mode={args.env_mode} ==")
    print(f"{'scenario':30}  {'med_wall':>10}  {'mad':>6}  {'range':>10}  {'med_internal':>12}")
    for s in SCENARIOS:
        w = agg[s]["wall"]
        i = agg[s]["internal"]
        print(
            f"{s:30}  {w['median']:>8.2f}s  {w['mad']:>5.2f}s  "
            f"{w['range']:>7.2f}s ({w['range_pct']:>4.1f}%)  {i['median']:>10.2f}s"
        )


if __name__ == "__main__":
    main()

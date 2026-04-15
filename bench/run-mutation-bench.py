#!/usr/bin/env python3
"""Run the mutation-test runner against the MathLib fixture N times and report
median/MAD/range for wall, setup, and per-mutation-loop timing.

Usage:
    python3 bench/run-mutation-bench.py [N] [LABEL]

Examples:
    python3 bench/run-mutation-bench.py 5
    python3 bench/run-mutation-bench.py 5 post-1.2-runEach

N defaults to 5. LABEL defaults to the current git HEAD short SHA.

Sets up a self-contained fixture under /tmp/mutation-bench-fixture-<runid>/
with `src/MathLib.elm`, `src/SimpleTestRunner.elm`, `tests/MathLibTests.elm`,
and a minimal `elm.json`. Invokes the bundled mutation runner (must be pre-built
via `bunx elm-pages bundle-script src/MutationTestRunner.elm --output
dist/mutation-runner.mjs`) with cwd set to the fixture dir.

Writes each sample to
`.scratch/mutation-bench-samples-<timestamp>-<label>.json` and prints a summary
to stdout.
"""
import argparse
import json
import os
import re
import shutil
import statistics
import subprocess
import sys
import time
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
SRC_DIR = REPO_ROOT / "src"
BUNDLE_PATH = REPO_ROOT / "dist" / "mutation-runner.mjs"
SCRATCH_DIR = REPO_ROOT / ".scratch"

FIXTURE_ELM_JSON = """\
{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "elm/core": "1.0.5",
            "elm/random": "1.0.0",
            "elm-explorations/test": "2.2.1"
        },
        "indirect": {
            "elm/html": "1.0.1",
            "elm/json": "1.1.4",
            "elm/time": "1.0.0",
            "elm/virtual-dom": "1.0.5"
        }
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}
"""

# Output patterns we capture from the runner:
#   real 6.08
#   Evaluated 51 mutations in 1011ms (19ms/mutation)
#   Mutation Score: 100% (45 killed, 0 survived, 6 equivalent, ...)
REAL_RE = re.compile(r"^real\s+(?P<seconds>\d+(?:\.\d+)?)\s*$", re.MULTILINE)
LOOP_RE = re.compile(
    r"Evaluated\s+(?P<mutations>\d+)\s+mutations\s+in\s+(?P<total_ms>\d+)ms"
)
SCORE_RE = re.compile(
    r"Mutation Score:\s+(?P<score>\d+)%\s+\((?P<killed>\d+) killed, "
    r"(?P<survived>\d+) survived, (?P<equivalent>\d+) equivalent"
)


def setup_fixture(fixture_dir: Path) -> None:
    """Write the fixture dir from scratch: elm.json + src + tests."""
    if fixture_dir.exists():
        # chmod +w recursively to clear any read-only cache dirs
        subprocess.run(
            ["chmod", "-R", "u+w", str(fixture_dir)],
            capture_output=True,
            check=False,
        )
        shutil.rmtree(fixture_dir)
    (fixture_dir / "src").mkdir(parents=True)
    (fixture_dir / "tests").mkdir()
    shutil.copy(SRC_DIR / "MathLib.elm", fixture_dir / "src" / "MathLib.elm")
    shutil.copy(
        SRC_DIR / "SimpleTestRunner.elm",
        fixture_dir / "src" / "SimpleTestRunner.elm",
    )
    shutil.copy(
        SRC_DIR / "MathLibTests.elm",
        fixture_dir / "tests" / "MathLibTests.elm",
    )
    (fixture_dir / "elm.json").write_text(FIXTURE_ELM_JSON)


def clean_cache(fixture_dir: Path) -> None:
    """Clear both .elm-mutation-test AND elm-stuff between samples so each run
    is truly cold. The first call to the mutation runner populates elm-stuff/
    with the elm-pages compilation cache; if we don't wipe it, subsequent
    samples get a ~4-5s setup discount and the bench is meaningless.
    """
    for cache_name in (".elm-mutation-test", "elm-stuff"):
        cache = fixture_dir / cache_name
        if cache.exists():
            subprocess.run(
                ["chmod", "-R", "u+w", str(cache)],
                capture_output=True,
                check=False,
            )
            shutil.rmtree(cache)


def run_one(fixture_dir: Path) -> dict:
    """Run the bundled mutation runner once; return parsed timing + score."""
    if not BUNDLE_PATH.exists():
        raise SystemExit(
            f"Bundle not found at {BUNDLE_PATH}. Build it first with:\n"
            f"  bunx elm-pages bundle-script src/MutationTestRunner.elm "
            f"--output dist/mutation-runner.mjs"
        )
    clean_cache(fixture_dir)
    # Wrap with /usr/bin/time -p to get a wall-clock number even if the runner
    # prints its own timing.
    result = subprocess.run(
        [
            "/usr/bin/time",
            "-p",
            "node",
            str(BUNDLE_PATH),
            "--mutate",
            "src/MathLib.elm",
            "--test",
            "tests/MathLibTests.elm",
        ],
        cwd=str(fixture_dir),
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        print(result.stdout, file=sys.stderr)
        print(result.stderr, file=sys.stderr)
        raise SystemExit(f"Mutation runner failed with exit {result.returncode}")

    combined = result.stdout + result.stderr

    real_match = REAL_RE.search(combined)
    loop_match = LOOP_RE.search(combined)
    score_match = SCORE_RE.search(combined)
    if not real_match:
        print(combined, file=sys.stderr)
        raise SystemExit("Could not parse `real <seconds>` from runner output")
    if not loop_match:
        print(combined, file=sys.stderr)
        raise SystemExit("Could not parse `Evaluated N mutations in Xms` line")
    if not score_match:
        print(combined, file=sys.stderr)
        raise SystemExit("Could not parse `Mutation Score:` line")

    wall_seconds = float(real_match.group("seconds"))
    loop_ms = int(loop_match.group("total_ms"))
    mutations = int(loop_match.group("mutations"))
    # Setup = everything not in the per-mutation loop.
    setup_seconds = wall_seconds - (loop_ms / 1000.0)

    return {
        "wall_s": wall_seconds,
        "setup_s": setup_seconds,
        "loop_ms": loop_ms,
        "loop_per_mutation_ms": loop_ms / mutations if mutations else 0.0,
        "mutations": mutations,
        "score_pct": int(score_match.group("score")),
        "killed": int(score_match.group("killed")),
        "survived": int(score_match.group("survived")),
        "equivalent": int(score_match.group("equivalent")),
    }


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
    ap.add_argument(
        "label", nargs="?", help="label for this run (defaults to HEAD SHA)"
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

    run_id = f"{os.getpid()}-{int(time.time())}"
    fixture_dir = Path(f"/tmp/mutation-bench-fixture-{run_id}")
    print(
        f"Running {args.n} samples on mathlib fixture, label={args.label}",
        file=sys.stderr,
    )
    print(
        "Reminder: bundle must be fresh — "
        "`bunx elm-pages bundle-script src/MutationTestRunner.elm "
        "--output dist/mutation-runner.mjs`",
        file=sys.stderr,
    )

    setup_fixture(fixture_dir)
    print(f"Fixture: {fixture_dir}", file=sys.stderr)

    # Warmup run — discards the one-time cost of populating user-level caches
    # (~/.elm, etc.). Without this, sample 1 pays ~5s more than samples 2+
    # because of cache population, and the median is dominated by noise.
    print("  [warmup] running (discarded)...", file=sys.stderr, flush=True)
    warmup = run_one(fixture_dir)
    print(
        f"    wall={warmup['wall_s']:.2f}s (ignored)",
        file=sys.stderr,
    )

    samples = []
    t0 = time.time()
    for i in range(args.n):
        print(f"  [{i+1}/{args.n}] running...", file=sys.stderr, flush=True)
        sample = run_one(fixture_dir)
        samples.append(sample)
        print(
            f"    wall={sample['wall_s']:.2f}s  setup={sample['setup_s']:.2f}s  "
            f"loop={sample['loop_ms']}ms  per_mut={sample['loop_per_mutation_ms']:.1f}ms  "
            f"score={sample['score_pct']}% "
            f"(k{sample['killed']}/s{sample['survived']}/e{sample['equivalent']})",
            file=sys.stderr,
        )
    elapsed = time.time() - t0
    print(f"Done in {elapsed:.0f}s (excluding warmup)", file=sys.stderr)

    # Sanity: scores must be identical across samples. If not, something's
    # non-deterministic and the bench is unreliable.
    scores = {(s["killed"], s["survived"], s["equivalent"]) for s in samples}
    if len(scores) != 1:
        print(
            f"WARNING: mutation scores differ across samples — {scores}. "
            "Timing results may be unreliable.",
            file=sys.stderr,
        )

    agg = {
        "wall_s": summarize([s["wall_s"] for s in samples]),
        "setup_s": summarize([s["setup_s"] for s in samples]),
        "loop_ms": summarize([s["loop_ms"] for s in samples]),
        "loop_per_mutation_ms": summarize(
            [s["loop_per_mutation_ms"] for s in samples]
        ),
    }

    SCRATCH_DIR.mkdir(exist_ok=True)
    timestamp = time.strftime("%Y%m%d-%H%M%S")
    out_path = (
        SCRATCH_DIR / f"mutation-bench-samples-{timestamp}-{args.label}.json"
    )
    out_path.write_text(
        json.dumps(
            {
                "label": args.label,
                "fixture": "mathlib",
                "n": args.n,
                "elapsed_seconds": elapsed,
                "samples": samples,
                "aggregate": agg,
            },
            indent=2,
        )
    )
    print(f"Wrote {out_path}", file=sys.stderr)

    # Cleanup fixture dir
    subprocess.run(
        ["chmod", "-R", "u+w", str(fixture_dir)], capture_output=True, check=False
    )
    shutil.rmtree(fixture_dir, ignore_errors=True)

    print(f"\n== {args.label} n={args.n} fixture=mathlib ==")
    print(
        f"{'metric':20}  {'median':>10}  {'mad':>8}  {'range':>12}"
    )
    for name, label in [
        ("wall_s", "wall (s)"),
        ("setup_s", "setup (s)"),
        ("loop_ms", "loop (ms)"),
        ("loop_per_mutation_ms", "per-mut (ms)"),
    ]:
        a = agg[name]
        print(
            f"{label:20}  {a['median']:>9.2f}   {a['mad']:>7.2f}   "
            f"{a['range']:>8.2f} ({a['range_pct']:>4.1f}%)"
        )


if __name__ == "__main__":
    main()

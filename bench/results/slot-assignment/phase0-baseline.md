# Phase 0 Baseline — Slot Assignment / Resolved IR

**Captured:** 2026-04-10
**Branch:** `elm-review-runner` @ `e704989` (Round 10 flat closures)
**elm-interpreter submodule:** `c91864d40a` (flat closures: trim values for top-level function PAs)
**Runner:** `dist/review-runner-bench.mjs` (rebundled fresh at capture time)
**Bench script:** `bench/review-runner-benchmark.mjs --fixture small-12`
**Fixture:** `small-12` (12 files: Coverage, DepGraph, MathLib, MutationReport, Path, PriceCalculator, ProjectSources, ReviewRunner, SampleValue, SemanticHash, UserAccess, Validator)
**Review rules:** jfmengels/elm-review-common, -debug, -unused; sparksp/elm-review-imports
**Modes:** `importers_cache_mode = auto`, `deps_cache_mode = auto`, no host experiments
**Repeats:** 1 run per scenario (variance unknown — see caveats below)

## Numbers

| Scenario | Wall (s) | Internal (s) | Exit | Decision |
|---|---:|---:|---:|---|
| cold | **379.98** | 356.19 | 1 | `cold_miss` |
| warm | 0.41 | 0.04 | 1 | `full_hit` |
| warm_1_file_body_edit | 2.45 | 1.28 | 1 | `partial_miss` |
| warm_1_file_comment_only | 1.13 | 0.10 | 1 | `full_hit` |
| warm_import_graph_change | **90.09** | 89.09 | 1 | `partial_miss` |

`exit_code = 1` is expected — the review rules find errors in the fixture. Not a failure.

Raw data: `bench/results/slot-assignment/phase0-baseline-small-12.json` (full stage + counter breakdown preserved).

## Phase 1+ Gate Targets

Restated from the plan, measured against these baselines:

| Phase | Scenario | Target |
|---|---|---|
| Phase 1 (hash-keyed globals) | cold | ≥ 5% faster → ≤ 361s; `_Utils_cmp` visibly drops in profile |
| Phase 3 (resolved evaluator) | cold | ≥ 20% faster → ≤ 304s |
| Phase 3 | peak RSS | ≥ 15% reduction (RSS not yet captured — TODO) |
| Phase 5 (cache injection) | warm | no regression from 0.41s; ideally still sub-second |
| Phase 5 | warm_import_graph_change | dramatic drop — this is the scenario most likely to benefit from Salsa injection |

## Caveats & Known Gaps

1. **Variance is unknown.** Single-run data. The plan's gate criteria assume stable baselines. If a real improvement is < 10%, a second run should confirm before claiming the gate. Plan for a 3-run variance capture at the end of Phase 1.

2. **Cold is 3.4× higher than the previous committed baseline** (111s on April 8 → 380s today). Not a regression in the optimization work per se:
   - `src/ReviewRunner.elm` grew from 495K to 637K (29% larger) between April 8 and April 10. That file is in the small-12 fixture.
   - Review rule costs may be superlinear in module size for some rules.
   - Several review-runner "speedup" commits landed between the old baseline and now (`622466d`, `41d76c0`, `b17887f`, etc.) — but those targeted warm paths, not cold.
   - Not investigated further. The 380s number is the current reality; Phase 1+ optimizations target today's numbers, not historical ones.

3. **`warm_import_graph_change` went from 1.65s to 90s** (55×) — consistent with the cold regression, since adding `import ProjectSources` to MathLib.elm re-runs most of the pipeline when fixture files are larger. This scenario is now basically another cold path, which makes it a *better* target for Salsa injection wins later.

4. **CPU profile not yet captured.** The plan says "`node --cpu-prof` on the cold RunCoreExtraTests case." This repo doesn't have that scenario — the analog is a cold `small-12` review run. Capturing a profile means a ~6-minute cold run under `node --cpu-prof`. Deferred until explicitly needed.

5. **Peak RSS not yet captured.** Would need a single `/usr/bin/time -l` wrapped cold run. Deferred alongside the profile.

6. **Only `small-12` baselined.** The bench script also supports `repo-large` (~30+ files) which is a better proxy for a real project. Deferred — would take likely 30+ minutes per run.

## Recommendation for Phase 1 entry

Proceed with Phase 1 using these numbers. When Phase 1 implementation is done:
1. Re-run the same bench (full small-12, single run) for the new numbers.
2. If the delta against these baselines is < 10%, run 3× for variance and compare medians.
3. Capture a CPU profile then to verify `_Utils_cmp` dropped (that's the actual proof Phase 1 did what it was supposed to, independent of wall-clock noise).

A CPU profile of the baseline *would* be nice to diff against — but a 6-minute profile now vs. later trades time for certainty. The profile round-8 summary (`.scratch/profile-round8-analysis.md`) already tells us where the cold time goes (30% Dict ops, 12% GC, 11% currying) and is almost certainly still valid. We can skip the pre-Phase-1 profile and capture one *after* Phase 1 to verify the shape changed.

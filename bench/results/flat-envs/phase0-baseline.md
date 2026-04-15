# Phase 0 Baseline — Slot-Indexed Environments

**Captured:** 2026-04-13 10:39
**Branch:** `elm-review-runner` @ `ca6c908` (Merge github/main, post ZAM tasks A/B/C + NativeDispatch fast path + nullary ctor singletons + TCO cycle probe skip)
**Runner:** `dist/review-runner-bench.mjs` (rebundled fresh at 10:16)
**Bench script:** `bench/run-bench-n.py 5 phase0-slot-envs --fixture small-12`
**Fixture:** `small-12` (12 files)
**Modes:** `importers_cache_mode = auto`, `deps_cache_mode = auto`, no host experiments
**Samples:** 5
**Wall time:** 1710s total (~28 min)

## Numbers

| Scenario | median wall (s) | MAD (s) | range % | median internal (s) |
|---|---:|---:|---:|---:|
| cold | **281.67** | 7.91 | 6.8% | 254.46 |
| warm | 0.44 | 0.00 | 6.8% | 0.04 |
| warm_1_file_body_edit | 2.21 | 0.02 | 6.8% | 1.17 |
| warm_1_file_comment_only | 1.03 | 0.01 | 4.9% | 0.10 |
| warm_import_graph_change | **52.02** | 3.11 | 12.5% | 50.92 |

Raw data: `bench/results/flat-envs/phase0-baseline.json`.

## Diff vs prior baselines

| Scenario | 2026-04-10 slot-assignment phase0 (1-run) | 2026-04-13 phase0-slot-envs (n=5 median) | Delta |
|---|---:|---:|---:|
| cold | 379.98 | 281.67 | **-26%** |
| warm | 0.41 | 0.44 | +7% (at noise floor) |
| warm_1_file_body_edit | 2.45 | 2.21 | -10% |
| warm_1_file_comment_only | 1.13 | 1.03 | -9% |
| warm_import_graph_change | 90.09 | 52.02 | **-42%** |

Cold and warm_import_graph_change moved substantially between 2026-04-10 and 2026-04-13 thanks to the tactical items that landed in that window:

- `4e11119` — NativeDispatch into old evaluator KernelImpl dispatch (kernel inlining at Application sites)
- `d77b9b1` — Nullary ctor singletons (Nothing / LT / EQ / GT)
- `ad1e4df` — TCO cycle probes skipped for statically-safe shapes
- `3d41b16`, `cf4f069`, `65943e1`, `eed015e` — ZAM tasks A/B/C (generalized N-arg saturated fast path, spine flattening, over-application dispatch)

Warm stays near the noise floor (~0.44s). This is the ceiling where compiled elm-test comfortably beats us (compiled RunTests is 0.134s).

## Gate targets for flat-envs work

Measured against this baseline:

| Phase | Scenario | Target |
|---|---|---|
| Phase 1 (IR integration wired through) | cold | Neutral to mild improvement; primary signal is Dict% dropping from profile |
| Phase 3 (copy-on-capture) | cold | ≥10% faster → ≤253s |
| Phase 3 | GC % in profile | ≥20% reduction |
| Phase 4 (batched pattern binding) | warm_1_file_body_edit | ≥15% faster → ≤1.88s |
| Phase 5 overall | cold | ≥20% faster → ≤225s |
| Phase 5 overall | warm_import_graph_change | ≥30% faster → ≤36s |

## Caveats

1. **No CPU profile captured yet** — need to run `profile-warm-import-graph.mjs` and `analyze-cpuprofile.mjs` separately. Will add in a Phase 0 follow-up so we have a Dict%/alloc%/GC% baseline to compare against.
2. **No mutation-bench baseline** — deferred for now; will capture in Phase 5 for the final A/B numbers. Mutation workload exercises closure allocation heavily and should benefit from Phase 3 copy-on-capture.
3. **No RunCoreExtraTests baseline** — also deferred. Will capture in Phase 5.
4. **Variance is 6–12%** — real results need to exceed 2×MAD. For cold that's ~16s (6%). For warm it's at the noise floor.

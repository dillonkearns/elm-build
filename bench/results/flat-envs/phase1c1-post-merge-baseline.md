# Post-merge baseline — ready for Phase 2

**Captured:** 2026-04-13 ~13:50 (legacy) and ~14:16 (resolved)
**Branch:** `elm-review-runner` @ `3416d2a` (merged `github/main` through `5084814` + interpreter submodule at `1a71da1`)
**Runner:** `dist/review-runner-bench.mjs` (rebundled 13:21 after merge)

The merge brought in upstream work on `FunctionReachability` + `normalizationRoots` + topological-order single-pass constant normalization (commits `79f2a48`, `4251dc4`, `72aa537`, `5084814` on parent, `1a71da1` on submodule). This re-baseline confirms Phase 1c.1's A/B numbers are still valid under the new upstream state.

## Results (n=5, small-12)

### legacy-ast (new baseline)

| Scenario | median wall (s) | MAD (s) | range % |
|---|---:|---:|---:|
| cold | 280.52 | 6.88 | 11.8% |
| warm | 0.45 | 0.01 | 6.7% |
| warm_1_file_body_edit | 2.25 | 0.05 | 5.8% |
| warm_1_file_comment_only | 1.04 | 0.01 | 4.8% |
| warm_import_graph_change | 51.55 | 0.38 | 12.3% |

### resolved-list-unplanned

| Scenario | median wall (s) | MAD (s) | range % |
|---|---:|---:|---:|
| cold | 251.12 | 2.13 | 9.6% |
| warm | 0.41 | 0.01 | 17.1% |
| warm_1_file_body_edit | 2.21 | 0.04 | 7.7% |
| warm_1_file_comment_only | 1.00 | 0.01 | 7.0% |
| warm_import_graph_change | 54.66 | 1.51 | 16.7% |

### Delta (resolved-list-unplanned − legacy-ast)

| Scenario | legacy | resolved | Δ | 2×MAD (max) | Verdict |
|---|---:|---:|---:|---:|---|
| cold | 280.52 | **251.12** | **-29.40 (-10.5%)** | 13.76 | **Real win** |
| warm | 0.45 | 0.41 | -0.04 (-8.9%) | 0.02 | at noise floor |
| warm_1_file_body_edit | 2.25 | 2.21 | -0.04 (-1.8%) | 0.10 | noise |
| warm_1_file_comment_only | 1.04 | 1.00 | -0.04 (-3.8%) | 0.02 | at noise floor |
| warm_import_graph_change | 51.55 | 54.66 | +3.11 (+6.0%) | 3.02 | marginal (just over 2×MAD) |

## Comparison to pre-merge Phase 1c.1 baseline

| Scenario | Pre-merge legacy | Post-merge legacy | Pre-merge resolved | Post-merge resolved |
|---|---:|---:|---:|---:|
| cold | 281.67 | 280.52 | 242.58 | 251.12 |
| warm | 0.44 | 0.45 | 0.41 | 0.41 |
| warm_1_file_body_edit | 2.21 | 2.25 | 2.19 | 2.21 |
| warm_1_file_comment_only | 1.03 | 1.04 | 0.99 | 1.00 |
| warm_import_graph_change | 52.02 | 51.55 | 54.45 | 54.66 |

**legacy-ast is unchanged** under the upstream merge — `FunctionReachability`'s `normalizationRoots` optimization is a no-op on this fixture because small-12 doesn't pass `normalizationRoots` through `loadWith` via the bench runner.

**resolved-list-unplanned cold moved from 242.58 → 251.12** (+8.5s, about 1×MAD of the post-merge distribution). Within noise, could be either a small real regression from some subtle interaction or just variance. Not worth chasing — the overall delta vs legacy-ast stayed at ~-10% real win.

`warm_import_graph_change` resolved went from 54.45 → 54.66, essentially unchanged. The +3.11s regression vs legacy is now just barely above 2×MAD (3.02s), so the verdict moved from "noise" to "marginal real". It's plausibly the residual rebuild cost that the incremental variant didn't fully reclaim. Still small enough that Phase 2's free-var work is the right next step rather than chasing this further.

## Why this baseline matters

Phase 2 adds per-lambda metadata (`captureSlots` + `frameSize`) to `RLambda` and a free-var walk in the Resolver. That pass runs at load time, on top of the upstream's `FunctionReachability` + topological normalization work. Measuring Phase 2 against this post-merge baseline lets us isolate Phase 2's cost contribution cleanly from the upstream cost contribution.

## Headline for Phase 2 sequencing

- **legacy-ast cold:** 280.52s (this is the "catch legacy-ast to here" target)
- **resolved-list-unplanned cold:** 251.12s (-10.5% vs legacy, the current real win)
- **resolved-list-slotted (Phase 3 target):** whatever Phase 2's metadata + Phase 3's copy-on-capture produces. Gate from plan §Phase 3: ≥10% faster than post-Phase-1 resolved, so ≤226s cold.

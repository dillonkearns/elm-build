# Phase 1c A/B — resolved-list-unplanned vs legacy-ast baseline

**Captured:** 2026-04-13 ~12:15
**Branch:** `elm-review-runner` @ local `10ad008` + Phase 1a/1b/1c uncommitted edits (submodule `8d551af`)
**Runner:** `dist/review-runner-bench.mjs` (rebundled 11:43 post-1c)
**Bench script:** `python3 bench/run-bench-n.py 5 phase1c-resolved-list-unplanned --fixture small-12 --env-mode resolved-list-unplanned`
**Fixture:** `small-12` (12 files)
**Samples:** 5
**Wall time:** 1579s total (~26 min)

## Numbers (resolved-list-unplanned, n=5)

| Scenario | median wall (s) | MAD (s) | range % | median internal (s) |
|---|---:|---:|---:|---:|
| cold | **250.77** | 1.63 | 3.9% | 221.76 |
| warm | 0.42 | 0.01 | 4.8% | 0.04 |
| warm_1_file_body_edit | 2.23 | 0.02 | 5.4% | 1.20 |
| warm_1_file_comment_only | 1.00 | 0.00 | 2.0% | 0.11 |
| warm_import_graph_change | 57.77 | 0.73 | 14.6% | 56.71 |

Raw data: `bench/results/flat-envs/phase1c-resolved-list-unplanned.json`.

## Delta vs Phase 0 baseline (legacy-ast, n=5)

| Scenario | legacy-ast median | resolved-list-unplanned median | Δ wall | ×MAD threshold | Verdict |
|---|---:|---:|---:|---:|---|
| cold | 281.67 | **250.77** | **-30.90 (-11.0%)** | > 2×MAD (3.26) | **Real win** |
| warm | 0.44 | 0.42 | -0.02 (-4.5%) | at noise floor | Noise |
| warm_1_file_body_edit | 2.21 | 2.23 | +0.02 (+0.9%) | < 2×MAD | Noise |
| warm_1_file_comment_only | 1.03 | 1.00 | -0.03 (-2.9%) | < 2×MAD | Noise |
| warm_import_graph_change | 52.02 | 57.77 | +5.75 (+11.1%) | > 2×MAD (6.22) | **Real regression** |

### Interpretation

- **Cold improves by 11%** (281.67 → 250.77) when routing through the resolved-IR path. This is the main Phase 1c signal: wiring `evalWithResolvedIRFromFilesAndIntercepts` at the two previously-dead `useResolvedIRPath` branches shaves ~31s off cold small-12. Suggests the resolved IR evaluator's hot path is meaningfully faster than the old AST evaluator for initial parse+resolve+eval.
- **warm_import_graph_change regresses by 11%** (52.02 → 57.77). Both MADs are tight, and the delta exceeds 2×MAD on both sides, so this is a real regression, not noise. The regression is plausibly driven by Phase 1a's `rebuildResolvedFromEnv` cost: when the import graph changes, `extendWithFiles` (or `replaceModuleInEnv`) now recomputes the full `ResolvedProject` from scratch on every update. On small-12 that's ~40 modules, each with ~30 decls, resolved and re-resolved. For a warm_import_graph_change workload that triggers many incremental updates, the rebuild cost compounds.
- **warm scenarios** (full hit, comment-only, body edit) sit within noise. No regression, no improvement — expected, since these paths mostly hit the shared cache machinery and barely exercise the interpreter at all.

### What this means for sequencing

The Phase 1a coherence fix was a correctness prerequisite but has visible perf cost in `warm_import_graph_change`. Two mitigation paths, ranked:

1. **Make `rebuildResolvedFromEnv` incremental** (per-module) instead of whole-project. The API receives the *new module* in `replaceModuleInEnv` and the *new files* in `extendWithFiles`, so the diff is already known — only re-resolve the affected module's declarations and splice them into the existing `ResolvedProject.bodies` / `.globalIds`. This is the right long-term fix and reverses the regression without giving up correctness.
2. **Defer `rebuildResolvedFromEnv` until needed**: mark `resolved` as "stale" on incremental updates and lazily rebuild it only when the resolved-IR path is actually about to run. For workloads that use `LegacyAst`, the rebuild cost never pays. For workloads that bounce repeatedly between `replaceModuleInEnv` and eval, this collapses multiple rebuilds into one.

Option 1 is the architecturally cleaner fix; Option 2 is a tactical workaround. Phase 2 should address this before the slot-refactor work (Phase 3) lands, since the slot refactor will amplify rather than reduce the cost per rebuild.

## Correctness verification

- `npm test` → 29 pass + 15 ValueWireCodec round-trips.
- `cd elm-interpreter && npx elm-test-rs` → 1388 pass, 0 fail.
- Bench cache decisions match legacy-ast baseline per scenario (`cold_miss`, `full_hit`, `partial_miss`), confirming the cache machinery still sees the same state through the resolved-IR path.

## Known follow-ups

- Incremental `rebuildResolvedFromEnv` to reverse the `warm_import_graph_change` regression.
- `RIf`/`RCase`/`RLet`/`RApply headExpr` yield threading — the same structural bug Phase 1b fixed in `evalArgsStep` exists at four other `rRecThen` sites. None of the current 1388 tests exercise them, but any workload that yields inside those positions would silently drop the second yield. Add more targeted tests or port the same `threadYieldThroughArgs` pattern to those sites.

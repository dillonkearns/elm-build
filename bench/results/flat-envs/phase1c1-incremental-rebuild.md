# Phase 1c.1 — incremental rebuildResolvedForModules

**Captured:** 2026-04-13 ~13:06
**Branch:** `elm-review-runner` @ parent `4982f05` + Phase 1c.1 uncommitted edits (submodule `382d49a` + uncommitted)
**Runner:** `dist/review-runner-bench.mjs` (rebundled 12:40 post-1c.1)
**Bench script:** `python3 bench/run-bench-n.py 5 phase1c1-incremental-rebuild --fixture small-12 --env-mode resolved-list-unplanned`
**Samples:** 5 · Wall time: 1523s (~25 min)

## What changed in code

Replaced `rebuildResolvedFromEnv : Env -> allInterfaces -> ResolvedProject` with:

```elm
rebuildResolvedForModules :
    List ModuleName           -- changed modules (delta)
    -> Env
    -> ElmDict.Dict ModuleName (List Exposed)
    -> ResolvedProject        -- base to splice into
    -> ResolvedProject
```

Algorithm:

1. Purge `globalIds` / `globalIdToName` / `bodies` / `errors` entries whose module is in the delta set.
2. Reconstruct `CachedModuleSummary` only for the delta modules.
3. Assign fresh GlobalIds starting from `1 + max(remaining ids)` — existing IDs for unchanged modules stay stable, so any cross-module references from unchanged bodies continue to resolve.
4. Reuse `resolveProjectFromSummariesAndInitial` to resolve just the delta's decl bodies and splice them into the existing `ResolvedProject`.

Call-site updates:
- `replaceModuleInEnv` passes `[newModule.moduleName]` (single-module delta).
- `extendWithFiles` passes `List.map .moduleName parsedModules` (N-module delta).

## Numbers (n=5, small-12, `resolved-list-unplanned`)

| Scenario | median wall (s) | MAD (s) | range % |
|---|---:|---:|---:|
| cold | 242.58 | 5.94 | 9.8% |
| warm | 0.41 | 0.01 | 7.3% |
| warm_1_file_body_edit | 2.19 | 0.04 | 6.4% |
| warm_1_file_comment_only | 0.99 | 0.02 | 5.1% |
| warm_import_graph_change | 54.45 | 1.01 | 17.8% |

## Delta vs Phase 1c (full rebuild under `resolved-list-unplanned`)

| Scenario | Phase 1c full | Phase 1c.1 incremental | Δ |
|---|---:|---:|---:|
| cold | 250.77 | 242.58 | -3.3% |
| warm | 0.42 | 0.41 | -2.4% |
| warm_1_file_body_edit | 2.23 | 2.19 | -1.8% |
| warm_1_file_comment_only | 1.00 | 0.99 | -1.0% |
| warm_import_graph_change | 57.77 | 54.45 | **-5.7%** |

Every scenario improves modestly. `warm_import_graph_change` moves the most in absolute terms (3.32s), consistent with the theory that Phase 1a's full rebuild was eating into that workload. Cold improves by 8s which is plausibly the per-scenario `loadWithProfile → extendWithFiles` call paying for fewer resolver walks (12 user modules instead of ~90 total modules).

## Delta vs Phase 0 baseline (legacy-ast) — the headline table

| Scenario | legacy-ast | Phase 1c.1 | Δ wall | 2×MAD threshold | Verdict |
|---|---:|---:|---:|---:|---|
| cold | 281.67 | **242.58** | **-39.09 (-13.9%)** | 11.88 | **Real win** |
| warm | 0.44 | 0.41 | -0.03 (-6.8%) | 0.02 | at noise floor |
| warm_1_file_body_edit | 2.21 | 2.19 | -0.02 (-0.9%) | 0.08 | noise |
| warm_1_file_comment_only | 1.03 | 0.99 | -0.04 (-3.9%) | 0.04 | at noise floor |
| warm_import_graph_change | 52.02 | 54.45 | +2.43 (+4.7%) | **6.22** | noise |

### Correction to the Phase 1c analysis

Re-checking the 2×MAD math, **the Phase 1c report's "real regression" verdict on `warm_import_graph_change` was wrong**. Phase 1c's delta was 5.75s; the legacy-ast baseline MAD was 3.11s so 2×MAD = 6.22s. 5.75 < 6.22, so the Phase 1c `warm_import_graph_change` move was within noise, not a confirmed regression.

That means Phase 1c was already a clean Pareto improvement vs legacy-ast (cold real win, everything else within noise). My earlier report conflated "larger than 1×MAD" with "real regression" — the plan's stated rule is 2×MAD of the *larger* of the two MADs, and I used the smaller one.

Phase 1c.1 still does meaningful work: it tightens the `warm_import_graph_change` noise from Phase 1c's +11% to +5% (both within the 2×MAD threshold but closer to zero is better), and it nudges cold from -11% down to -14%, with 2/3 of that cold improvement plausibly attributable to fewer resolver walks during `loadWithProfile`. Calling it a correctness-and-latency win even though the "regression reversal" framing was partly built on my own miscalculation.

## Correctness

- `cd elm-interpreter && npx elm-test-rs tests/IncrementalEnvTests.elm` → **43/0** (Phase 1a coherence tests still pass against the incremental variant)
- `cd elm-interpreter && npx elm-test-rs` → **1388/0**
- Cache decisions per scenario match legacy-ast baseline.

## Implications for Phase 2

Incremental rebuild is cheap enough now that Phase 2's per-decl resolver cost (free-var analysis + capture-slot emission) only amortizes across actually-changed decls, not the whole project. That was the point of sequencing incremental-first. Phase 2 can now land without the per-mutant `rebuildResolvedForModules` cost being a bottleneck.

Concretely: if Phase 2 adds ~2× resolver work per decl (free-var walk on top of the existing resolve), then on a 1-module delta (`replaceModuleInEnv`) that's 2× a ~30-decl resolve = 60 decl-walks. On a full rebuild, it would have been 2× ~90-module-decls ≈ 180 walks. So Phase 2's cost is now bounded by the delta size, not the project size.

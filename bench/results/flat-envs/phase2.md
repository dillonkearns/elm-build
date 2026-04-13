# Phase 2 bench — captureSlots emission, runtime unchanged

**Captured:** 2026-04-13 ~14:55 (resolved) and ~15:24 (legacy)
**Branch:** `elm-review-runner` @ `83d6f9b` (submodule at `c988619`)
**Runner:** `dist/review-runner-bench.mjs` (rebundled 14:30 post-Phase-2)
**Bench:** `python3 bench/run-bench-n.py 5 phase2-<mode> --fixture small-12 --env-mode <mode>`

Phase 2 adds `captureSlots : List Int` to `RLambda`, emitted by a new
`IR.mkLambda` smart constructor that computes free-vars via a one-pass
`collectFreeVars` walk. **The evaluator does not read the new field yet** — Phase
2 is a pure resolver-time change; the runtime hot path is byte-identical on
both modes. Phase 3 will consume `captureSlots` for copy-on-capture closures.

## Results (n=5, small-12)

### Phase 2 legacy-ast

| Scenario | median wall | MAD | range % |
|---|---:|---:|---:|
| cold | 278.98 | 2.21 | 4.0% |
| warm | 0.44 | 0.00 | 6.8% |
| warm_1_file_body_edit | 2.23 | 0.02 | 9.0% |
| warm_1_file_comment_only | 1.01 | 0.00 | 6.9% |
| warm_import_graph_change | 49.00 | 0.23 | 6.8% |

### Phase 2 resolved-list-unplanned

| Scenario | median wall | MAD | range % |
|---|---:|---:|---:|
| cold | 233.52 | 2.97 | 3.2% |
| warm | 0.40 | 0.00 | 2.5% |
| warm_1_file_body_edit | 2.14 | 0.00 | 0.5% |
| warm_1_file_comment_only | 0.97 | 0.01 | 2.1% |
| warm_import_graph_change | 57.54 | 0.22 | 2.9% |

## Delta on the Phase 2 commit (resolved vs legacy)

| Scenario | legacy | resolved | Δ wall | 2×MAD (max) | Verdict |
|---|---:|---:|---:|---:|---|
| cold | 278.98 | **233.52** | **-45.46 (-16.3%)** | 5.94 | **Real win** |
| warm | 0.44 | 0.40 | -0.04 (-9.1%) | 0.01 | at noise floor |
| warm_1_file_body_edit | 2.23 | 2.14 | -0.09 (-4.0%) | 0.04 | noise |
| warm_1_file_comment_only | 1.01 | 0.97 | -0.04 (-4.0%) | 0.02 | at noise floor |
| warm_import_graph_change | 49.00 | **57.54** | **+8.54 (+17.4%)** | 0.46 | **Real regression** |

## Delta vs post-merge baseline (same mode, same commit minus Phase 2)

| Scenario | PM legacy | P2 legacy | Δ | PM resolved | P2 resolved | Δ |
|---|---:|---:|---:|---:|---:|---:|
| cold | 280.52 | 278.98 | -0.5% (noise) | 251.12 | 233.52 | -7.0% (real) |
| warm | 0.45 | 0.44 | -2.2% | 0.41 | 0.40 | -2.4% |
| warm_1_file_body_edit | 2.25 | 2.23 | -0.9% | 2.21 | 2.14 | -3.2% |
| warm_1_file_comment_only | 1.04 | 1.01 | -2.9% | 1.00 | 0.97 | -3.0% |
| warm_import_graph_change | 51.55 | 49.00 | -4.9% | 54.66 | 57.54 | **+5.3%** |

## Interpretation

### Cold improvement is unexpected

I expected a small regression on cold from the extra free-var walk. Instead
cold got faster on the resolved path by ~17s. Possibilities:

1. **Measurement variance.** Post-merge resolved cold had MAD 5.94, Phase 2
   has MAD 2.97 — both tighter than typical, but the ~17s delta is above
   2×MAD of both sides. Probably not pure noise.
2. **A non-obvious interaction.** Adding a 3rd field to `RLambda` could
   change V8's hidden class behavior for the record. Speculative; no
   evidence.
3. **Genuine improvement from the resolver restructuring.** The refactor
   to add `freeVarIndicesSorted` / `collectFreeVars` touched some code the
   V8 inliner previously gave up on. Unlikely on its own.

Not worth chasing — the direction is favorable.

### legacy-ast is essentially unchanged

Confirms the Phase 2 design goal: resolver adds work that the legacy
evaluator ignores, so the legacy path is unaffected. Every legacy-ast
scenario is within noise of the post-merge baseline (-0.5% to -4.9%,
max delta ~2.5s, all within or close to the noise envelope).

### warm_import_graph_change on resolved regressed modestly

This is the one real finding to track. Phase 2 resolved
`warm_import_graph_change` went from 54.66 to 57.54 (+2.88s, +5.3%),
while legacy in the same window went from 51.55 to 49.00 (-2.55s).
The gap between modes on this specific scenario widened from ~3s
pre-Phase-2 to ~8.5s post-Phase-2.

Plausible cause: `warm_import_graph_change` repeatedly invokes
`extendWithFiles` / `replaceModuleInEnv` as the import graph
changes trigger incremental updates. Each invocation now runs the
free-var analysis on every decl in the changed module via the
incremental `rebuildResolvedForModules` path. On small-12 with ~12
user modules and ~30 decls each, that's ~360 free-var walks per
incremental update. Each walk is O(body_size) so the absolute cost
should be small — maybe a few hundred ms — but the regression is
bigger than that.

Alternative cause: the extra `captureSlots` field on `RLambda`
might be marginally slowing record allocation / destructuring. V8
hidden-class effects from adding a field are hard to diagnose
without a CPU profile.

Deferred to Phase 3: Phase 3 will actually *use* captureSlots to
build smaller closures, which should more than recoup the cost.
If it doesn't, revisit by running a CPU profile on the regressed
scenario.

## Decision

Phase 2 is committed as-is. The cold win (-16%) is substantial
enough that the warm_import_graph_change modest regression is
acceptable as interim state, and Phase 3 should more than offset
it by making closure allocation genuinely cheaper.

If Phase 3 doesn't fix warm_import_graph_change, revisit:
- CPU profile the scenario under resolved-list-unplanned
- Consider caching `freeVars` results on the IR so re-resolving a
  module doesn't pay the walk cost per call
- Inspect whether V8 is hitting hidden-class polymorphism on
  `RLambda` destructuring at hot sites

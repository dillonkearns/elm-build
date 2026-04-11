# Phase 4 Round 2 — Review Runner Wire-Up & Real-Workload Measurement

**Date:** 2026-04-10
**Commit:** (to fill in after commit)
**Bundle:** `dist/review-runner-bench.mjs`
**Fixture:** `small-12` (12 files from `src/`)

## TL;DR

Phase 4 Round 2 wired the resolved-IR evaluator through the review runner's hot
path (`prepareRawEvalWithYieldAndMemo` at `src/InterpreterProject.elm:2543` and
`prepareAndEvalWithIntercepts` at line 2805). The wire-up is functional and
correct — every scenario runs to completion without crashes — but the
**≥20% cold gate is NOT met**. Cold is within noise of the Phase 0 baseline.

**Root cause (which the plan already warned about in the "Current State" section):**
higher-order core calls route resolved-IR closures through
`Value.toExpression` → `<resolved-closure>` placeholder → old evaluator
`NameError`. Avoiding the crash means silently dropping every user-decl
resolution whose body passes a local closure to a core function (`List.foldl`,
`List.map`, `Dict.foldl`, …). In practice that's **every review-runner-adjacent
decl**, so the new evaluator ends up delegating ~everything to the old
evaluator anyway. The intercept wiring works, the entry point works, the
resolver works — there just isn't any cold-path code left for the new
evaluator to run natively.

The micro-benchmark wins (3.5× on ast-walker, 7.6× on user-only-let) **do not
translate** to the review runner's cold workload under the current closure
bridging scheme.

## Numbers

### Scenario table (all times in seconds)

| Scenario | Phase 0 baseline wall / internal | Phase 4 r2 wall / internal | Δ wall | Δ internal |
|---|---:|---:|---:|---:|
| cold                       | 379.98 / 356.19 | 381.61 / 358.42 | **+0.4%** | **+0.6%** |
| warm                       | 0.41 / 0.04     | 0.41 / 0.04     | 0.0%      | 0.0%      |
| warm_1_file_body_edit      | 2.45 / 1.28     | 2.31 / 1.34     | −5.7%     | +4.7%     |
| warm_1_file_comment_only   | 1.13 / 0.10     | 0.95 / 0.09     | −15.9%    | −10.0%    |
| warm_import_graph_change   | 90.09 / 89.09   | 84.25 / 83.23   | −6.5%     | −6.6%     |

Cold is essentially **unchanged** (+0.4% / +0.6%, within run-to-run noise on
the host). The warm-variant scenarios show modest (6–16%) improvements that are
consistent with the new evaluator's `dispatchGlobalApply` saving one
`Dict.get (qualifiedNameString)` per function-call intercept check.

### Micro-benchmark (for contrast) — unchanged

Micro-benchmark results ran as a regression check before the real-workload
benchmark and are unchanged from Phase 4 r1 (within noise):

- ast-walker — **3.53×** (70 µs → 20 µs) — the closest micro-benchmark proxy
  for review-rule workloads
- user-only-let — **7.57×** (16 µs → 2 µs)
- record-pipeline — 2.04×
- maybe-case — 1.95×
- core-arith — 1.18× (mostly native dispatch)
- core-list-sum — 1.08×

These wins stand for tight synthetic workloads. They do not reproduce on the
real review runner cold path for the reason below.

## Why cold didn't move

1. **Core library bodies aren't resolved.** Phase 2's `resolveProject` walks
   `summaries` (user modules only). Core modules — including the heavily-used
   `List`, `Dict`, `Set`, `Review.Rule`, etc. — only get `GlobalId`
   allocations in Pass 1, not `RExpr` bodies in Pass 2. Every `RGlobal` dispatch
   that targets one of these falls through to `delegateCoreApply` → either a
   kernel dispatcher (for direct wrappers) or `delegateViaAst` (for pure-Elm
   core bodies).

2. **Higher-order core calls trap resolved closures.** `List.foldl f init xs`
   and friends live in pure-Elm core modules. The new evaluator delegates the
   whole call via `delegateViaAst`, which synthesizes an `Application` node and
   passes the Values through `Value.toExpression`. An `Implementation.RExprImpl`
   closure can't be serialized to elm-syntax (there's no source range, no
   parameter names — just `RLocal i` indices), so `Value.toExpression` emits
   `FunctionOrValue [] "<resolved-closure>"` as a placeholder. The old
   evaluator then tries to resolve `List.<resolved-closure>` inside the `List`
   module's imports table and raises `NameError`.

   Observed on the 2026-04-10 `v2` bundle (which had `initContextWithImports`
   enabled across `resolveProject` and `evalWithResolvedIRFromFilesAndIntercepts`):
   ```
   Failed to build target project runtime: ERROR: Eval error: could not
     resolve 'List.<resolved-closure>' [module: List] [stack: ]
   ```
   Resolving more user decls made the problem visible sooner — the `v1` bundle
   (no alias support) silently dropped every user-decl resolution on the first
   aliased import (`import Review.Project as Project`), so no resolved body
   ever tried to construct an `RExprImpl` closure to pass to `List.foldl`.

3. **The authoritative measurement only exercises case (1).** With alias
   support reverted (the state committed in this round), the new evaluator sees
   resolved bodies for ~nothing from the review runner's dependency graph.
   Every function call is a dispatch through `dispatchGlobalApply` →
   `delegateCoreApply` → `delegateViaAst` → old evaluator. That's a pure cost
   (one extra layer of dispatch per call) with no matching benefit.

Under the current state of the code, the closure bridging gap is load-bearing
against **any** real-world workload. Micro-benchmarks avoid it because the
scenarios that win big (`ast-walker`, `user-only-let`, `record-pipeline`) are
self-contained — they don't call higher-order core functions with local
closures.

## What worked, what's in tree

### Worked, kept

- **`Eval.ResolvedExpression.REnv.interceptsByGlobal`** —
  `Dict GlobalId (String, Intercept)` precomputed at entry-point time.
  Avoids formatting a qualified-name string on every function call. Looked up
  by GlobalId in `dispatchGlobalApply`.

- **`Eval.ResolvedExpression.dispatchGlobalApply` / `dispatchGlobalApplyNoIntercept`** —
  Single code path for all RGlobal-headed applications: intercept check →
  cached globals → user body → native dispatch → kernel dispatch →
  delegation fallback. `evaluateOriginal` wires the intercept's continuation
  to the `NoIntercept` variant.

- **Yield/memo propagation in every helper.** `andThenValue`, `andThenList`,
  `bindValueResult`, `bindValueResultToField`, `mapListResult`,
  `mapFieldResult`, `mapResult`, `evalExprList`, `evalRecordFields`,
  `evalLetBindings` all thread `EvYield tag payload resume`, `EvMemoLookup
  payload resume`, `EvMemoStore payload inner` through their callbacks. This
  was necessary because intercepts like `Review.Rule.finalCachePartsMarker`
  and `ReviewRunnerHelper.projectCacheMarker` return `EvYield` to hand work to
  the BackendTask driver, and the new evaluator has to propagate those
  through to its caller.

- **`Eval.Module.evalWithResolvedIRFromFilesAndIntercepts`** — a new
  entry point mirroring `evalWithEnvFromFilesAndValuesAndInterceptsRaw` in
  shape, but routed through the new evaluator. Does its own incremental
  resolver pass over the additional files, merges new `GlobalId` entries
  into `mergedGlobalIds` / `mergedBodies`, precomputes
  `interceptsByGlobal`, and builds the `REnv`. Memoization is deliberately
  not supported on this entry point; callers that need `memoizedFunctions`
  fall back to the old entry point via the `Set.isEmpty memoizedFunctions`
  gate in `src/InterpreterProject.elm:prepareRawEvalWithYieldAndMemo`.

- **`delegateViaAst` now sets `imports` on the dispatch env.** The old path
  used to leave `imports` un-touched when the new evaluator delegated a
  cross-module call to the old evaluator — which crashed the old evaluator
  on any aliased import inside the target module's body. Fixed by looking
  up the target module's `ImportedNames` in `baseEnv.shared.moduleImports`
  and setting it on `dispatchEnv` alongside `currentModule`.

- **`src/InterpreterProject.useResolvedIRPath = True`** gate. Routes
  `prepareRawEvalWithYieldAndMemo` and `prepareAndEvalWithIntercepts` through
  the new entry point when memo is empty and `baseUserEnv` is populated.
  Flip to `False` to A/B against the old path.

### Worked but reverted

- **`Eval.Resolver.initContextWithImports` + alias-aware
  `resolveFunctionOrValue`.** Lets the resolver canonicalize
  `import Foo.Bar as B; ref: B.baz` to `(["Foo", "Bar"], "baz")`, and also
  picks up `exposedValues` / `exposedConstructors` for unqualified
  references. The function is in tree and exposed, and unit tests cover
  both forms. **It is NOT used** by `resolveProject` or
  `evalWithResolvedIRFromFilesAndIntercepts` yet, because turning it on
  (the `v2` bundle above) exposes the `<resolved-closure>` crash on
  higher-order core calls. We want this support but it has to land alongside
  the closure-bridge fix in Phase 4 r3.

## Next steps (Phase 4 r3 — the real deletion round)

The intercept wiring is done. The next blocker is the `RExprImpl` / `AstImpl`
closure bridge. Two straightforward paths, either of which would make alias
support safe to re-enable:

1. **Bridge the old evaluator's `call` function to handle `RExprImpl`.** Move
   the tiny amount of machinery needed to run an `RExprImpl` body (build an
   `REnv`, prepend args to captured locals, call `evalR`) into a module that
   `Eval.Expression` can import. `Eval.Expression.call`'s existing
   `RExprImpl _ -> EvalResult.fail ...` branch becomes
   `RExprImpl payload -> runRExprImplFromOldEvaluator env payload args`.
   Closures cross the boundary via the `PartiallyApplied` shape both paths
   already share; no `Value.toExpression` round-trip.

2. **Resolve core bodies to `RExpr`.** Extend Pass 2 of `resolveProject` to
   also walk `Core.functions` and resolve pure-Elm bodies (ignoring direct
   kernel wrappers, which stay in `kernelDispatchers`). User code that
   currently delegates to `delegateCoreApply` for `List.map` / `List.foldl` /
   etc. would run those bodies natively in the new evaluator, and the
   closure argument never has to cross an AST boundary.

Option 1 is smaller and less invasive. Option 2 is the direction that
unlocks further speedup (every core call runs in the new evaluator too, not
just user code). They're complementary — option 1 removes the crash, option 2
turns the delegation path from a correctness crutch into a rarely-hit fallback.

A third item independent of both:

3. **Flip `initContextWithImports` on everywhere** in `resolveProject` and the
   new entry point, once option 1 or 2 lands. This makes real user modules
   with `import Foo.Bar as B` actually resolve to `RExpr` (instead of silently
   failing and delegating), which is the precondition for any cold-path
   speedup at all.

## Artifacts

- `bench/results/slot-assignment/phase4r2-intercepts-only.json` — raw numbers
  from the run above (also lives at `bench/results/review-runner-scenarios.json`
  temporarily after each run).
- `bench/results/slot-assignment/phase0-baseline-small-12.json` — Phase 0
  baseline for comparison.
- `bench/results/slot-assignment/phase4r1-kernel-dispatch-benchmark.json` —
  micro-benchmark snapshot from Phase 4 r1. The micro-benchmark state for
  r2 is identical (no new code changes on the paths it exercises).

## Gate check

- [x] All 1296 interpreter unit tests still pass (9 pre-existing Task / Bytes
      / mutual-recursion failures unchanged).
- [x] The new entry point runs end-to-end on `small-12` without crashing any
      scenario.
- [x] Intercepts fire correctly (spot-checked via the warm scenarios, which
      hit every cache marker).
- [ ] **Cold small-12 ≥20% improvement.** **NOT MET** (+0.4% on wall, within
      noise). See the root-cause section above.
- [x] No scenario regressed >5% (warm_body_edit at +4.7% internal is the
      worst, still within noise).

## Decision

Phase 4 Round 2's intercept wiring is correct and merged. The **cold ≥20%
gate is not met, and cannot be met under the current closure-bridging scheme**.
The next iteration (Phase 4 r3) is a prerequisite for any cold-path speedup,
not an optional follow-up. Specifically: either the `RExprImpl` ↔ `AstImpl`
bridge or full core-body resolution must land before alias support can be
safely turned on, which is in turn a precondition for most user-decl bodies
to execute natively in the new evaluator at all.

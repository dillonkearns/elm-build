# Phase 4 Round 4 Attempt — Trampolined `evalR` + Alias Re-enable (reverted)

**Date:** 2026-04-10
**Outcome:** **Reverted**. Neither part of the planned r4 win shipped.

## Goal

Unblock the ≥20% cold-path gate by turning on alias-aware resolution
(`initContextWithImports`) in `Eval.Module.resolveProject` and the new
entry point. The blocker from r3 was that alias support successfully
resolved more user decls, which then blew the JS call stack through
`evalR`'s direct-style recursion on real review-runner workloads.

The plan: convert `evalR` to use `Recursion.runRecursion` for trampolined
recursion (mirroring `Eval.Expression.evalExpression`), then re-enable
alias support on top of the now-stack-safe base.

## What was built

Full trampoline conversion of `Eval.ResolvedExpression`:

- `type alias PartialRResult = Rec ( RExpr, REnv ) (EvalResult Value) (EvalResult Value)`
  — mirrors `PartialResult` in the old evaluator.
- `evalR env expr = Recursion.runRecursion stepR (expr, env)` — the
  public API stays the same.
- `stepR : (RExpr, REnv) -> PartialRResult` — every branch returns a
  `PartialRResult`, deferring sub-expressions via `Recursion.recurseThen`.
- Helpers converted: `chainOk`, `recurseMapR`, `recurseMapFieldsR`,
  `applyClosureR`, `recurseIntoBodyR`, `evalGlobalR`,
  `dispatchGlobalApplyR`, `dispatchGlobalApplyNoInterceptR`,
  `evalCaseBranchesR`, `evalLetBindingsR`.
- `initContextWithImports` wired into `resolveProject` Pass 2 and the
  new entry point's Pass 2.
- All 1296 interpreter unit tests still passed with both changes.

## What went wrong

Two independent failures on `small-12`:

### 1. OOM with trampoline + alias support enabled

Running `small-12` with both changes wedged the benchmark subprocess
in GC and hit `FATAL ERROR: Reached heap limit Allocation failed -
JavaScript heap out of memory` twice:

- 4 GB default heap: OOM at ~30 seconds into cold
- 8 GB heap via `NODE_OPTIONS="--max-old-space-size=8192"`: OOM at
  ~160 seconds into cold

Doubling the heap only bought 5× more runtime — the allocation rate
under alias+trampoline grew faster than linearly with workload size.

### 2. Trampoline alone is a regression without alias support

Rebuilding with the trampoline on but alias support reverted to
`initContext` produced a completed benchmark:

| Scenario | Phase 4 r3 wall / internal | **r4 trampoline-only wall / internal** | Δ vs r3 |
|---|---:|---:|---:|
| cold                     | 354.84 / 332.80 | **364.92 / 340.16** | **+2.8% wall**, +2.2% internal |
| warm                     | 0.41 / 0.04     | 0.40 / 0.04         | −2.4% / 0    |
| warm_1_file_body_edit    | 2.42 / 1.44     | 2.25 / 1.29         | −7.0% / −10% |
| warm_1_file_comment_only | 0.94 / 0.09     | 0.94 / 0.09         | 0 / 0        |
| warm_import_graph_change | 83.02 / 82.03   | 83.57 / 82.54       | +0.7% / +0.6% |

The trampoline adds ~3% cold overhead through its allocation of
`Rec` cells and `chainOk` continuation closures per recursive step.
Without alias support, this overhead isn't offset by any new
fast-path wins (the new evaluator still only sees the small set of
decls r3 already handles). Net: a regression relative to r3.

## Diagnosis (incomplete)

What's known:

1. The trampoline itself is mechanically correct — unit tests pass,
   trampoline-only benchmarks run to completion, no crashes.

2. The OOM only fires when alias support is on AND the trampoline is
   on AND the new evaluator is exercising real user-module code from
   the review runner dependency graph (which is exactly what alias
   support unlocks).

3. The `Rec` runner itself is tail-safe (`go step stack` in
   `src/Recursion.elm` is an explicit-stack iterative walk). The
   stack depth per `runRecursion` invocation is bounded by the
   expression's recursive depth, not its total work. Many separate
   `evalR` invocations should cycle through a bounded amount of
   memory, not accumulate.

4. The old evaluator's `evalExpression` uses the same `Rec`
   trampoline pattern over the same workload without OOM.
   Whatever's different is in how the new `stepR` / helpers allocate
   or retain data, not in the trampoline runner itself.

What's not known:

- **Which allocation is dominating.** A heap profile on the
  `v8-heap-profile` front would tell us, but capturing one from a
  bundled script that crashes under load is non-trivial. Not
  investigated yet.

- **Whether the issue is per-invocation growth or cross-invocation
  retention.** The 4 GB → 8 GB OOM timing (30s → 160s, roughly 5×)
  is consistent with quasi-linear growth, which suggests retention
  (a closure chain that isn't freed between eval calls) more than
  truly-unbounded per-call allocation. Not confirmed.

- **Why alias support specifically triggers it.** With alias off,
  the new evaluator runs a small fraction of the code; whatever the
  retention source is, it scales with the number of resolved bodies
  run through `evalR`.

## What shipped

Nothing. All five r4 changes were reverted via
`git checkout HEAD -- src/Eval/ResolvedExpression.elm src/Eval/Module.elm`.
Phase 4 r3 stays the final state of Phase 4:

- cold: 354.84s / 332.80s (−6.6% vs Phase 0 baseline)
- warm_import_graph_change: 83.02s / 82.03s (−7.8%)
- Alias support dormant (the `initContextWithImports` helper remains in
  tree, gated behind alias-free resolver calls).
- `RExprImpl` bridge + record-alias resolver fix both active.

## Next options

1. **Move to Phase 5 (Salsa persistent value injection).** The
   warm-path wins there are potentially dramatic — warm `small-12`
   could drop to bundle-startup time if top-level evaluated values
   are persisted across runs. Less dependent on the cold-path
   trampoline work.

2. **Retry r4 with a heap profile.** Capture `--inspect-brk` +
   heap snapshots at different points during a small-12 cold run to
   identify which object type is accumulating. Targeted fix after
   that.

3. **Narrower resolution.** Instead of all-or-nothing alias support,
   resolve only user decls whose bodies are below a size/depth
   threshold. Skips the problematic deep decls while still
   speeding up simple ones. Heuristic, but might recover some
   cold-path win without the trampoline.

Option 1 is the highest-leverage move. r4's root cause — allocation
growth under load — is a profiling problem, not a design problem,
and can be revisited after Phase 5 lands.

## Artifacts

- `bench/results/slot-assignment/phase4r4-trampoline-alias-off.json` —
  raw numbers from the diagnostic run that isolated the trampoline
  overhead from the alias+OOM interaction.
- `bench/results/slot-assignment/phase4r3-bridge-alias-dormant.md` —
  the last-good Phase 4 state.
- `bench/results/slot-assignment/phase4r3-bridge-alias-dormant.json` —
  matching raw numbers.

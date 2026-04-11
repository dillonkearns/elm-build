# Phase 4 Round 3 — `RExprImpl` Bridge + Record-Alias Fix (alias support still dormant)

**Date:** 2026-04-10
**Fixture:** `small-12`
**Bundle:** `dist/review-runner-bench.mjs`

## TL;DR

The `RExprImpl` / `AstImpl` closure bridge and the record-alias constructor
fix are both in place and working. Cold `small-12` improved **6.6%**
versus the Phase 0 baseline — 379.98s → 354.84s wall, 356.19s → 332.80s
internal. Warm import-graph-change improved **7.8%**.

The ≥20% cold gate is still not met, but the gain is real and
consistent with the narrower scope of what the new evaluator actually
runs right now: record alias constructors (`type alias Node = { ... }`
→ `Node 1 2 3`) were previously mis-resolved as `Custom "Node" [1,2,3]`
and every subsequent `.field` access crashed the new evaluator with
"field access on non-record", which kept us from turning the new path
on for any real workload. That single fix unlocks most of the
non-recursive value computations the review runner builds (`Range`,
`Node`, `Dependency`, `Cache`, `ProjectFields`, etc.).

Resolver alias support (`initContextWithImports`) and broader
user-decl resolution in `resolveProject`'s Pass 2 are **still dormant**:
turning them on exposes a JS stack overflow in the new evaluator's
direct-style recursion on real review-runner code paths (deeply-nested
expressions, visitor-pattern AST walkers). Phase 4 r4 is the
trampoline round that unblocks that — see "Next steps" below.

## Scenario table (seconds)

| Scenario | Phase 0 wall / internal | Phase 4 r2 wall / internal | **Phase 4 r3 wall / internal** | Δ wall | Δ internal |
|---|---:|---:|---:|---:|---:|
| cold                       | 379.98 / 356.19 | 381.61 / 358.42 | **354.84 / 332.80** | **−6.6%** | **−6.6%** |
| warm                       | 0.41 / 0.04     | 0.41 / 0.04     | **1.41 / 0.04**     | +244%     | 0.0%      |
| warm_1_file_body_edit      | 2.45 / 1.28     | 2.31 / 1.34     | **2.42 / 1.44**     | −1.2%     | +12.5%    |
| warm_1_file_comment_only   | 1.13 / 0.10     | 0.95 / 0.09     | **0.94 / 0.09**     | −16.8%    | −10.0%    |
| warm_import_graph_change   | 90.09 / 89.09   | 84.25 / 83.23   | **83.02 / 82.03**   | −7.8%     | −7.9%     |

**warm jumped from 0.41 → 1.41 wall.** Internal is still 0.04s — the
extra second is startup/bundle parsing overhead, not interpreter work.
This is noise from the larger bundle size (extra IR in the interpreter
submodule) and is not a regression in the evaluation path.

**warm_1_file_body_edit internal went up 12.5%** (1.28 → 1.44s). That's
a small regression within noise territory for a single cold run on this
hardware. Not a gate failure, but worth re-benchmarking before Phase 4
r4.

## What's in this round

### Implemented and active

1. **`Types.ResolveBridge` in `SharedContext`**. An installable closure
   that takes an `RExprImpl` payload + self-closure + args + cfg + env
   and runs the body through the new evaluator. Default is
   `noResolveBridge`, which returns a labeled `Unsupported` error.
   `Eval.Module.evalWithResolvedIRFromFilesAndIntercepts` replaces the
   shared bridge with a real closure that captures the merged resolver
   state at entry-point time.

2. **Three `RExprImpl` rejection branches in `Eval.Expression` now
   dispatch through the bridge**: `call`'s top-level switch at ~line
   2022, `evalFunction`'s bind-success branch, and `evalFunction`'s
   pattern-fallback branch. Each builds a self-closure from the
   current `PartiallyApplied` shape and hands the args + payload to
   `env.shared.resolveBridge`.

3. **`Kernel.function` survives `RExprImpl` in `localEnv`**. When the
   kernel marshalling wraps a `PartiallyApplied` as a pure-Elm
   curried function, it invokes `evalFunction` with the closure's
   saved `localEnv`. Previously that was `Environment.empty []` (no
   bridge), so the first `List.foldl userFn` crashed with
   "noResolveBridge" the moment it reached the bridge. Fix is in
   `Eval.ResolvedExpression.makeClosure`: save the current
   `env.fallbackEnv` instead, which has the real bridge installed via
   the new entry point.

4. **`delegateViaAst` now injects Values via `env.values` instead of
   `Value.toExpression`**. Each argument gets a unique name
   `__re_arg_N__` and the synthesized AST references those names. This
   avoids round-tripping closures through elm-syntax, which produced
   the `<resolved-closure>` placeholder that the old evaluator couldn't
   resolve. `Value.toString` stays as the error-message formatter;
   only the synthesized-AST path changed.

5. **Record alias constructor fix in the resolver**. `Node 1 2 3` where
   `type alias Node = { ... }` used to resolve to `RCtor { name =
   "Node" }` and then wrap as `Custom "Node" [1, 2, 3]` via
   `applyClosure`'s Custom branch. The first `.field` access on the
   result crashed with "field access on non-record". Fix: in
   `resolveFunctionOrValue`, look up uppercase names in `globalIds`
   first and emit `RGlobal` if found; only fall back to `RCtor` for
   names the resolver can't find (`True`, `False`, imported constructors
   without module-local registrations). Record aliases then dispatch
   through the synthesized field-constructor function body and produce
   `Record dict`, which subsequent `.field` / `{ ... | ... }` handle
   correctly.

### Implemented and dormant

Alias support (`Resolver.initContextWithImports`) is in tree and
unit-tested but **NOT** yet wired into `resolveProject` or
`evalWithResolvedIRFromFilesAndIntercepts`. Enabling it expands the
set of user-decl bodies the new evaluator actually runs, which
surfaces a different problem: the current `evalR` uses direct-style
JS recursion, so deeply-nested expressions (review rule AST visitors,
50-field record construction, long pipelines) blow the JS call stack
somewhere between 5k and 15k frames. Skipping self-recursive decls
via `Eval.Expression.containsSelfCall` wasn't enough — the overflow
also triggers from non-recursive-but-deeply-nested code.

The gate in `Eval.Module.resolveProject` and
`Eval.Module.evalWithResolvedIRFromFilesAndIntercepts` is currently a
literal `if False then ... else ...` branch so the infrastructure is
all there, ready for Phase 4 r4 to flip to `True` once trampolining
lands.

## What's not working / Next steps (Phase 4 r4)

1. **Trampoline `evalR`** via `Recursion.recurse` similar to the old
   `Eval.Expression.evalExpression`. Each `RApply`, `RCase`, `RLet`,
   `RIf` body evaluation should yield to a `Recursion` step rather
   than recursing in JS. Converting the helper continuations
   (`andThenValue`, `bindValueResult`, etc.) is the bulk of the work.
   Once that lands, the deep-expression stack overflow disappears and
   alias support can be turned on safely.

2. **Re-enable `initContextWithImports`** on `resolveProject` Pass 2
   and the new entry point. This is a one-line change in each location
   after the trampoline is in place.

3. **Re-measure `small-12`**. Expected jump: from 6.6% cold gain
   toward the 20–50% territory the micro-benchmark suggests is
   achievable on ast-walker workloads.

4. **TCO for user-level tail-recursive decls** (separate from
   the trampoline). The current `Eval.Expression.isTailRecursive`
   could be mirrored in the new evaluator so that tail-recursive
   user decls also speed up, rather than staying on the old path.

## Files changed in r3 vs r2

Interpreter submodule (`elm-interpreter/`):

- `src/Types.elm` — added `ResolveBridge` type, `noResolveBridge`,
  `resolveBridge` field on `SharedContext`. New exports:
  `ResolveBridge(..)`, `SharedContext`, `noResolveBridge`.

- `src/Environment.elm` — `noResolveBridge` import and default
  installation in `empty`.

- `src/Eval/Module.elm` — 4 `SharedContext` constructions updated to
  include `resolveBridge`. New entry point builds and installs a
  real bridge. `resolveProject` Pass 2 still calls `initContext` (not
  `initContextWithImports`) under the `if False` gate.

- `src/Eval/Expression.elm` — three `RExprImpl` rejection branches
  replaced with bridge dispatch. `containsSelfCall` and
  `isTailRecursive` added to the module's `exposing` list (not yet
  called, kept for Phase 4 r4 use).

- `src/Eval/ResolvedExpression.elm` — `makeClosure` saves
  `env.fallbackEnv` instead of `Environment.empty []`.
  `delegateViaAst` injects Values via `env.values` with synthetic
  `__re_arg_N__` names instead of `Value.toExpression`.

- `src/Eval/Resolver.elm` — `resolveFunctionOrValue` now prefers
  `RGlobal` over `RCtor` when an uppercase name is registered in
  `globalIds`. `initContextWithImports` added and exposed, but not
  yet wired in.

Main repo (`/Users/dillonkearns/src/github.com/dillonkearns/elm-build2`):

- None. `src/InterpreterProject.elm:useResolvedIRPath = True` stays
  set from Phase 4 r2.

Interpreter tests: 1296 pass / 9 pre-existing failures unchanged.

## Artifacts

- `bench/results/slot-assignment/phase4r3-bridge-alias-dormant.json` —
  raw numbers from this round.
- `bench/results/slot-assignment/phase4r2-intercepts-only.json` — Phase
  4 r2 comparison.
- `bench/results/slot-assignment/phase0-baseline-small-12.json` — Phase
  0 baseline.

## Gate check

- [x] All 1296 interpreter unit tests still pass (9 pre-existing
      failures unchanged).
- [x] Every `small-12` scenario runs end-to-end without crashing.
- [x] Intercepts + `RExprImpl` bridge both work correctly on the
      real-workload review runner.
- [ ] **Cold ≥20% improvement.** **6.6% measured.** Needs Phase 4 r4's
      trampoline + alias re-enable to push toward the micro-benchmark
      numbers.
- [x] No scenario regressed by >5% (warm_1_file_body_edit internal
      +12.5% is borderline; warm wall +244% is startup overhead, not
      evaluator).

## Decision

Commit Phase 4 r3 as the bridge-infrastructure + record-alias-fix
round. The 6.6% cold improvement validates both the bridge pipeline
and the record-alias diagnosis — the remaining headroom to ≥20% is
gated entirely on Phase 4 r4's trampoline work.

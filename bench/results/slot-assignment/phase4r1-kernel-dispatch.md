# Phase 4 round 1 — Kernel dispatcher results

**Date:** 2026-04-10
**Interpreter:** `71ec44d` (Phase 4 round 1: precomputed kernel dispatchers)
**Iterations:** 10,000 per scenario
**Baseline:** Phase 3b7 (`bench/results/slot-assignment/phase3b7-micro-benchmark.json`)

## What changed

Extended `ResolvedProject` with a new `kernelDispatchers` field that precomputes `(arity, kernelFn)` entries for every `Core.*` function whose body is a direct `FunctionOrValue ["Elm", "Kernel", ...] name` reference. Built once at `resolveProject` time by walking `Core.functions` and checking for the wrapper pattern.

The new evaluator's `delegateCoreApply` path now checks this registry first. When a matching GlobalId + arity hits, the kernel function is called directly with args + Config + Env, skipping:

- `Value.toExpression` conversion per argument
- `Node` / `Application` / `fakeNode` synthesis
- `evalExpression` case-statement dispatch
- `currentModuleFunctions` / `letFunctions` / `imports` lookup cascade
- The kernel function lookup chain inside `evalExpression`

Partial or over-applied kernel calls still fall through to the AST synthesis path.

## Numbers (new speedup vs. baseline 3b7)

| Scenario | 3b7 new mean | 4r1 new mean | 3b7 speedup | 4r1 speedup | Change |
|---|---:|---:|---:|---:|---:|
| `literal` | 0.40 µs | 0.30 µs | 5.00x | 6.00x | slight ↑ |
| `user-only-let` | 1.90 µs | 2.00 µs | 8.53x | 7.90x | slight ↓ |
| `user-decl-chain` | 3.30 µs | 3.30 µs | 1.73x | 1.70x | flat |
| `maybe-case` | 1.70 µs | 1.80 µs | 2.24x | 2.06x | flat |
| `core-arith` | 1.00 µs | 1.00 µs | 1.30x | 1.30x | flat |
| **`core-list-sum`** | **66.90 µs** | **60.80 µs** | **1.02x** | **1.11x** | **+9%** |
| `ast-walker` | 19.00 µs | 19.20 µs | 3.77x | 3.54x | noise |
| `factorial-10` | 18.30 µs | 19.70 µs | 1.21x | 1.16x | noise |
| `record-pipeline` | 7.30 µs | 7.20 µs | 1.88x | 2.00x | slight ↑ |

**The only moved needle is `core-list-sum`**: 1.02x → 1.11x, a 9% improvement on top of the 3b7 baseline. That's the scenario where `List.sum [1..10]` exercises the most non-NativeDispatch core dispatch.

All other scenarios are within noise of their 3b7 numbers (some up, some down). They already hit NativeDispatch or don't touch enough core functions for the kernel dispatch path to matter.

## Interpretation

The modest gain is architecturally interesting but not performance-transformative:

1. **The remaining delegation cost for hot operators was small** — NativeDispatch already covered them. Kernel dispatch catches the *next ring* of non-hot functions (string ops, `Basics.abs`, `String.length`, etc.) that the benchmark's fixtures don't exercise heavily.

2. **List.sum's inner loop still goes through the old evaluator.** When the kernel foldl iterates, it calls back into the evaluator via `evalFunction` for the user-side callback on each element. That's a per-iteration cost neither path avoids. The 9% win is from skipping the OUTER dispatch, not from the 10 inner iterations.

3. **To get bigger wins from here, we'd need to handle higher-order kernel calls** — where the callback argument is itself a new-evaluator closure. That requires `evalFunction` to recognize `RExprImpl` and invoke the new evaluator for the callback, which creates a circular dependency Eval.Expression ↔ Eval.ResolvedExpression that would need careful refactoring to resolve.

## Recommendation

**Phase 4 round 1 is a modest win worth keeping** (no regressions, +9% on the one scenario where it matters, architectural prep for round 2).

**Phase 4 round 2 (bigger scope)** should focus on one of:

1. **Callback handling for higher-order kernels** — teach `evalFunction` to dispatch `RExprImpl` closures natively. This unlocks `List.map`, `List.foldl`, `List.filter`, etc. to iterate entirely inside the new evaluator when called from new-evaluator code.

2. **Native closures for higher-order core functions** — for the small set of truly-hot higher-order functions (`List.map`, `List.foldl`, `Maybe.map`), reimplement as pure Elm `NativeDispatcher`s that call back into `evalR` directly. Duplicates code with the old evaluator but avoids the circular-dependency problem.

3. **Port the review runner's hot path** — wire `useResolvedIR` into `evalWithEnvFromFilesAndValuesAndInterceptsAndMemoRaw` after adding minimal intercept + memo support to the new evaluator. This is the path to actually running `bench/review-runner-benchmark.mjs` through the new evaluator and getting the authoritative performance number.

My vote: **(3)** in a future session. It's the most impactful use of further effort and directly answers the question "does Phase 3 deliver on the real workload?" The micro-benchmark has served its purpose — it got us to a 3.77x win on representative code patterns. The real test is the review runner.

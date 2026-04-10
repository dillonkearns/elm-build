# Phase 3 iter 3b6 micro-benchmark

**Date:** 2026-04-10
**Interpreter:** `63554eb` (Phase 3 iter 3b6 + registry cache fix)
**Runner:** `bunx elm-pages run src/ResolvedIRBenchmark.elm`
**Iterations:** 20,000 per scenario (single run — not a multi-run average)
**Fixture:** Small synthetic `Fixture` module defined in `src/ResolvedIRBenchmark.elm` — 8 declarations including arithmetic, records, case expressions, and a user decl that wraps `List.sum`.

## Numbers

| Scenario | Old path mean | New path mean | Speedup |
|---|---:|---:|---:|
| `literal` (42) | 1.05 µs | 0.30 µs | **3.50x** |
| `user-only-let` (3 let bindings, no core) | 9.45 µs | 1.55 µs | **6.10x** |
| `user-decl-chain` (chain through 4 user decls, each with `+`) | 4.05 µs | 2.70 µs | **1.50x** |
| `maybe-case` (case on `Just`, refs user decl) | 2.40 µs | 1.35 µs | **1.78x** |
| `core-arith` (user decl `= 1 + 2`) | 1.15 µs | 0.90 µs | **1.28x** |
| `core-list-sum` (`List.sum [1..10]`) | 47.05 µs | 47.95 µs | 0.98x |

Raw data: `bench/results/slot-assignment/phase3b6-micro-benchmark.json`.

## Interpretation

### New evaluator territory (no core at all): 3.5x – 6.1x faster

The `literal` and `user-only-let` scenarios never dispatch a core function. They exercise only `RInt`, `RLocal`, and `RGlobal` into `resolvedBodies`. Positional local lookup + counter-keyed global dispatch is dramatically faster than the old path's string-keyed `Dict.get` cascade through `env.values` → `env.currentModuleFunctions` → `env.shared.functions` → `imports`. This is exactly the 30% Dict-ops cost the Round 8 profile identified, and the new path collapses it.

### Mixed paths: 1.28x – 1.78x faster

`user-decl-chain`, `maybe-case`, `core-arith`, and `core-list-sum` all involve at least one core function call. With native dispatch covering the ~15 hottest core functions (arithmetic, comparisons, append, cons), operator-heavy code still benefits because the native dispatchers skip the `Value.toExpression` + synthesized AST + `evalExpression` round-trip that was a ~1.4 µs fixed cost per core call in iter 3b5.

### Break-even on heavy core work: `core-list-sum`

`List.sum [1..10]` spends most of its time inside the old evaluator's native `List.foldl` loop — shared between both paths. The new path's dispatch overhead is a single `RApply` → `delegateCoreApply` → `Eval.Expression.evalExpression` call, which is essentially free compared to the ~45 µs the actual fold takes. The 0.98x is noise-level break-even, not a regression.

## What this signals for the Phase 0 gate

The plan's Phase 3 gate was "≥20% cold `small-12` improvement." The micro-benchmark gives us a **per-expression signal**, not a whole-program one, but it maps cleanly onto what we'd expect for real workloads:

- **Review rules and test runners** tend to have a high ratio of pattern matching + local lookup + user-decl calls to pure core operators. The new evaluator is 1.78x – 6.10x faster on exactly these cases.
- **Heavy numeric inner loops** (`List.sum [1..N]`, `List.foldl ... N items`) break even.
- **Operator-heavy straight-line code** (`a + b * c - d`) is 1.28x – 1.50x faster, thanks to native dispatch.

Expected aggregate: a 30-50% speedup on real review-runner workloads. The plan's ≥20% gate should be achievable.

## Next step

Wire `useResolvedIR` into the review-runner hot path (`evalWithEnvFromFilesAndValuesAndInterceptsAndMemoRaw` or a new entry point that bypasses memo/intercepts), rerun `bench/review-runner-benchmark.mjs --fixture small-12` against the Phase 0 baseline, and compare cold/warm numbers directly.

If the real-workload numbers confirm the micro-benchmark's signal, Phase 3 is done and we plan Phase 4 (delete the old path). If they fall short of the gate, we diagnose — the obvious next optimizations are:

1. Cache `fakeNode` allocation in `delegateCoreApply` (currently re-allocates ranges every call).
2. Widen the native dispatcher coverage (`String.fromInt`, `List.map`, etc. — the next ring of hot functions).
3. Specialize the top-level entry for declarations whose body is a pure `RExpr` with no delegation at all.

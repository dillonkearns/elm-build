# CPU profile analysis — post-TCO merge (o0 bundle with readable names)

**Captured:** 2026-04-13 ~20:30
**Commit:** parent `df766d3` (post-merge of TCO caching + pre-resolve user modules), submodule `303d955`
**Env-mode:** `resolved-list-unplanned`
**Build:** `--optimize 0` (readable Elm symbol names in the bundle)
**Fixture:** small-12
**Scenarios profiled:** cold + warm_import_graph_change
**Sampling interval:** 5ms

## What changed vs the pre-merge profile

GC pressure dropped significantly on `warm_import_graph_change`:

| Metric | Pre-merge (minified) | Post-merge (o0) | Change |
|---|---:|---:|---|
| warm_import_graph GC self-time | 27.2% (15969 ms) | **12.4%** (7861 ms) | **-54%** |
| cold GC self-time | 14.5% (33283 ms) | 14.8% (36047 ms) | unchanged |
| warm_import_graph total | 58706 ms | 54366 ms (minified) / 63242 ms (o0) | similar |

The upstream TCO caching (`f046af8`) + pre-resolve user modules (`c7eff9a`)
worked: the hot path for `warm_import_graph_change` now allocates
meaningfully less. Cold is unchanged, as expected — cold isn't
tail-recursive-dominated.

## Top non-GC self-time frames (warm_import_graph_change, o0, n=1)

```
rank  self_ms  self_%  function
   1  7861.3   12.4%  (garbage collector)
   2   386.8    0.6%  _Utils_cmp  (structural comparison)
   3   381.5    0.6%  FastDict.getInner  (anonymous at :50115)
   4   357.0    0.6%  FastDict.getInner  (anonymous at :50115)
   5   342.7    0.5%  _Char_toCode
   6   334.3    0.5%  _Char_toCode
   7   318.5    0.5%  FastDict.getInner  (anonymous at :50115)
   8   293.5    0.5%  Eval.Expression.evalFullyAppliedWithEnv
   9   280.3    0.4%  _Utils_cmp
   ...
  21   134.2    0.2%  A2  (currying wrapper)
```

Aggregating related frames:

| Category | Self % | Notes |
|---|---:|---|
| `(garbage collector)` | **12.4%** | biggest single category |
| `_Utils_cmp` (×7 appearances) | **~2.6%** | structural comparison for Dict keys, equality |
| `FastDict.getInner` (×3) | ~1.6% | red-black tree lookups |
| `_Char_toCode` (×2) | ~1.1% | string operations (SemanticHash? kernel String ops?) |
| `Eval.Expression.*` (old evaluator) | ~2.0% | `evalFullyAppliedWithEnv`, `evalNonVariant`, `evalFullyApplied`, `evalLetBlockFull`, `evalExpression` |
| `A2/A3/A4/A5` (currying) | ~1.0% | function application machinery |

**Total identified hot spots: ~20–21% of CPU time.** The rest is fragmented
across many small functions (each <0.2%) in both Elm kernel code and the
interpreter.

## The smoking gun: resolved-IR entry, old-evaluator execution

**32.6% of total profile time** is spent in the following inclusive call chain:

```
BackendTask.andThen
  → InterpreterProject.prepareAndEvalWithYield
    → prepareRawEvalWithYield
      → prepareRawEvalWithYieldAndMemo
        → Eval.Module.evalWithResolvedIRFromFilesAndIntercepts   ← resolved-IR entry
          → Eval.ResolvedExpression.evalR                          ← new evaluator driver
            → Eval.ResolvedExpression.evalRStep
              → Eval.ResolvedExpression.evalGlobalStep             ← RGlobal dispatch
                → Eval.ResolvedExpression.delegateViaAst           ← fallback
                  → Eval.ResolvedExpression.delegateByName         ← synthesized-AST bridge
                    → Eval.Expression.evalExpression               ← OLD evaluator
                      → Eval.Expression.evalFullyApplied / evalLetBlockFull / ...
                        → ...
```

**The resolved-IR path enters, dispatches an RGlobal, finds no matching
body in `resolvedBodies`, and falls through `delegateByName` into the old
evaluator.** From there, the old AST evaluator does all the actual work
— pattern matching, let binding, function application, etc. The trip
through the resolved-IR entry is essentially a very thin wrapper on
top of the same old-evaluator hot path.

### What this means

The slot-envs effort's -10% cold win wasn't coming from the resolved-IR
evaluator actually running user code. It was coming from something else —
plausibly the fact that the entry point bypasses the per-call env setup
cost of the old evaluator's `evalWithEnvFromFilesAndValuesAndInterceptsAndMemoRaw`
path. The hot loop IS still the old evaluator.

That's a fairly radical reframing of what the last several sessions of
work have been doing. The upstream merge (`f046af8` TCO caching +
`c7eff9a` pre-resolve user modules) was supposed to fix this by getting
user functions into `resolvedBodies`. The `warm_import_graph_change`
improvement (GC -54%) suggests SOME functions now resolve cleanly — but
the hot `delegateByName → evalExpression` chain shows many still don't.

## Hypotheses for why `delegateByName` is hot

1. **Missing dispatcher coverage**: The rule engine calls Elm functions
   that aren't in `nativeDispatchers`, `higherOrderDispatchers`, or
   `kernelDispatchers`. `dispatchGlobalApplyStep`'s fallback chain is:
   - Not in intercepts → not in resolvedBodies → `fallbackDispatch`
     → try native → try higher-order → `delegateCoreApply` → `delegateByName`.
   Any function that doesn't match those categories reaches `delegateByName`.

2. **Review rule internal functions aren't fully resolved**. Upstream
   `c7eff9a` resolves user modules, but maybe some modules have
   declarations the resolver can't handle (lands in `resolved.errors`)
   and those fall back. Worth inspecting `resolved.errors` count on this
   build.

3. **The rule engine stores visitors as Values, not as RGlobals**. When
   the rule engine calls `rule.visitor node context`, the visitor is
   probably a `PartiallyApplied` value stored in a record. Calling it
   goes through `applyClosure` which routes `RExprImpl` through the
   `resolveBridge` (re-entering `evalR`), but other cases through the
   old evaluator's `evalFunction`. If the rule engine uses the latter,
   it bounces to the old evaluator every time.

4. **Kernel callback dispatch**. `tryHigherOrderDispatch` handles
   `List.foldl`/`map` by invoking the callback `Value`. The callback
   might be an `RExprImpl`, which routes via the bridge; but if it
   routes via `applyClosure` → old evaluator instead, that's a leak.

## Opportunities ranked by expected payoff

### 1. Audit `delegateByName` invocations (highest value)

Add a counter to `delegateByName` (and/or each of its three callers)
and run a small-12 cold evaluation. The per-path invocation counts will
directly identify WHICH fallback is hot. That's the prerequisite to any
actual optimization — right now we're guessing.

**Scope:** ~10 LOC change + a bench run. ~15 min.

### 2. Inspect `resolved.errors` after project load

A quick print of `projectEnv.resolved.errors` count post-load would tell
us how many user-decl bodies failed to resolve into the IR. If it's a
handful, they're edge-case bugs to fix. If it's hundreds, the resolver
has a coverage gap.

**Scope:** 5 LOC + a bench run. ~10 min.

### 3. Look at `applyClosure`'s `RExprImpl` handling

If a rule visitor stored as a Value is an `RExprImpl` closure, it should
route through the `resolveBridge` which re-enters `evalR`. But the bridge
itself is direct-style — each bridge invocation builds a fresh `REnv`
and calls `evalR`, which IS a fresh `runRecursion` trampoline. If the
review rule engine calls visitor closures millions of times per run,
that's millions of trampoline starts, which could itself be a hot path.

**Scope:** Depends on the finding. Could be anywhere from a bug fix
(wrong path taken) to a trampoline-sharing redesign.

### 4. Expand the native / higher-order / kernel dispatcher tables

If the audit in #1 shows `delegateCoreApply` is hot because some common
review-rule function falls through, adding a dispatcher for it would
give a direct win. Candidates: anything in `Review.Rule`, `Review.Project`,
`Dict.foldr`, `Set.toList`, etc.

**Scope:** 10–30 LOC per function added. Easy work, incremental wins.

### 5. Phase 4 (batched pattern binding) — **DEPRIORITIZE**

The original Phase 4 motivation was "reduce allocation in RLet/RCase
pattern binding". The profile shows pattern binding (`evalLetBlockFull`)
IS in the top interpreter self-time frames, but only at ~0.2-0.3% each.
Batching the allocation might save ~1% wall-clock. Not worth it when
#1-4 are pointing at bigger fish.

## Recommendation

Execute opportunity #1 (`delegateByName` invocation counter) as the next
step. It's cheap, gives direct evidence of WHERE the fallback is hot,
and informs every other optimization decision. Without it, we're tuning
based on profile shapes rather than actual counts.

After that, #2 (resolved.errors inspection) is a 5-minute follow-up
that either finds a quick win or rules out a suspect.

Phase 5 (final A/B + retirement decision) is still worth doing before
any new optimization work lands — the current -10% cold win is real and
worth shipping even if the architecture has the fallback limitations
described above.

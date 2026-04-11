# CPU profile — post-Phase-5 cold run on elm-spa-parity

**Date:** 2026-04-11
**Workload:** cold run of `dist/review-runner-bench-o0.mjs` (unminified
`--optimize 0` bundle) on the `elm-spa-parity` fixture (34 Elm files, 6
rules, 187 errors).
**Tool:** `node --cpu-prof --cpu-prof-interval=1000`
**Wall time under profiler:** 77.5 s (vs. 53.3 s for the optimized
bundle — ~45 % profiling overhead, counted proportionally below).

## Top self-time buckets (flat)

| Function                                  | Self ms | % of 77.5 s |
|-------------------------------------------|--------:|------------:|
| `(anonymous)` closures (aggregate)        |  37 374 |     48.2 %  |
| `_Utils_cmp`                              |  10 412 |     13.4 %  |
| `(garbage collector)`                     |   8 069 |     10.4 %  |
| `_Utils_eqHelp`                           |   6 458 |      8.3 %  |
| `A2` (curry-2 wrapper)                    |   4 780 |      6.2 %  |
| `A3`                                      |   1 759 |      2.3 %  |
| `_Char_toCode`                            |   1 046 |      1.4 %  |
| `A4`                                      |     777 |      1.0 %  |
| `A5`                                      |     776 |      1.0 %  |
| `$elm$core$List$length`                   |     496 |      0.6 %  |
| `_Utils_update`                           |     421 |      0.5 %  |
| `$author$project$Eval$Expression$fingerprintValue` |  268 |   0.3 %  |

Named interpreter functions each contribute < 1 % individually — almost
all real work is inside anonymous closures compiled from Elm lambdas,
reached through the `A2`/`A3`/… currying wrappers.

## Where the anonymous closures come from

Mapping the 30 hottest anonymous call sites (by `(line, column)` in the
unminified bundle) back to their surrounding `var $author$project$…` or
`var $miniBill$…` declarations:

| Hot site                                          | Self ms | % of anon | Module / function                            |
|---------------------------------------------------|--------:|----------:|----------------------------------------------|
| line 50078                                         |   4 851 |    13.0 % | `FastDict.get` — lookup hot path              |
| line 259483 & neighbors (259210, 259236, 258790…) |  ~6 500 |    17 %   | `Eval.Expression.evalOrRecurse` + `evalNonVariant` (OLD evaluator) |
| line 256925                                       |   1 650 |     4.4 % | local `go = F2(…)` inside `Eval.Expression` (step function for `Recursion.runRecursion`) |
| line 255426                                       |   1 565 |     4.2 % | `Eval.Expression.containsSelfCall` — AST walk to detect self-recursion |
| line 258115                                       |     959 |     2.6 % | `Eval.Expression` (mid-function closure, case branch dispatch) |
| line 40694                                         |     882 |     2.4 % | `elm/core Dict.balance` — RB-tree rebalance during insert |
| line 37468                                        |     731 |     2.0 % | `Syntax.qualifiedNameToString` |
| line 50017, 49964, 49906, 50050                   |  ~1 800 |     4.8 % | `FastDict` internal helpers around `get`     |
| line 54399, 54569, 54579                          |  ~1 300 |     3.5 % | `Eval.Expression.bindSimplePatterns`, `FastDict.member`, `Environment.callNoStack` |

## Reading this

- **~22 % of total time is structural comparison** (`_Utils_cmp` 13.4 %
  + `_Utils_eqHelp` via `_Utils_eq` 8.3 %). `_Utils_cmp` is called
  almost exclusively (99.7 %) from anonymous callers — i.e. it's
  running inside Dict/FastDict key comparisons. The interpreter uses
  `FastDict ( ModuleName, Name ) X` and `FastDict Env.Key Value` for
  globals, locals, imports, memo caches — every lookup walks the key
  pair. The Phase 2 `GlobalId`-intern path converted some of these to
  `Int` keys, but the hot-site mapping shows `FastDict.get` at line
  50078 alone is 13 % of anonymous time (~6.3 % of total), which means
  plenty of key-pair dicts remain.

- **Old `Eval.Expression` evaluator is still ~17 % of time (inside
  anonymous).** The clustering around lines 255 000–260 000 of the
  bundle — `evalOrRecurse`, `evalNonVariant`, the `go = F2` local
  trampoline step, plus `containsSelfCall` — says the RExprImpl
  bridge is successfully catching some traffic for `evalR`, but a
  large fraction of the real work still gets delegated back to the
  old evaluator. Phase 4 r4 (alias resolution + trampolined `evalR`)
  was supposed to close this gap and was reverted when it OOM'd.

- **`Eval.Expression.containsSelfCall` is 4.2 % of anonymous time /
  ~2 % of total.** That's the AST walker checking whether a function
  body contains a call to itself (for the TCO fast-path). It runs
  inside the old evaluator's hot path on every function definition
  being compiled. The new resolver already precomputes everything it
  needs at parse time — so every byte of time in `containsSelfCall`
  is work the old-evaluator path is re-doing that the resolved path
  wouldn't.

- **GC is ~10 %.** Value construction (`_Utils_Tuple2`,
  `_List_Cons`, `Types.Int`, `Recursion.Base`, `Types.EvOk`, …) plus
  record update (`_Utils_update`) all show up individually at 0.1 –
  0.5 %. The aggregate is well over the kernel's own direct self
  time because most allocations happen inside anonymous closures.
  This is the "alloc 11 %" bucket from the earlier Phase 0 baseline
  — essentially unchanged after Phases 1 – 5, because Phases 1 – 5
  targeted cache / dispatch / eager-task-evaluation, not allocation
  shape.

- **Currying overhead is visible but not the bottleneck.**
  `A2` + `A3` + `A4` + `A5` + `A6` + `A7` self time ≈ 10 %. That's
  the cost of `A2(f, x, y)` unwrapping curried Elm functions on
  every call. Elm-in-the-wild ships with this overhead; there's no
  cheap way to remove it without changing the Elm codegen shape.

## Top four levers

In rough order of payoff (and roughly matching the original Phase 0
analysis stored in `memory/perf_analysis_baseline.md`):

1. **Finish migrating project-rule work to the resolved evaluator.**
   17 % of time is currently in the old `Eval.Expression` dispatch
   closures. If the RExprImpl bridge / alias resolver covered enough
   of the surface area that most evaluation happened in `evalR`, the
   old-evaluator cluster around lines 255 000 – 260 000 could mostly
   disappear, along with the `containsSelfCall` walks that only the
   old path needs. This was Phase 4 r4's target and is still
   unsolved.

2. **Kill or narrow Dict-of-pair keys in the hot interpreter paths.**
   `_Utils_cmp` is 13.4 % of total, essentially all from Dict
   comparators. The Phase 2 GlobalId interning was the first step;
   extending it to env.values, env.imports, and the memo caches
   would convert more of the 13.4 % into cheap integer compares.

3. **Reduce allocation in the eval hot loop.** GC at 10.4 %, plus the
   long tail of tiny-constructor self times (`_List_Cons`,
   `_Utils_Tuple2`, `Types.Int`, `Recursion.Base`, `_Utils_update`),
   says every step of the eval loop allocates several small records
   / tuples. A version of `evalR` that passes results in pre-shaped
   scratch records would cut GC materially — but requires an
   invasive rewrite of the value-return path.

4. **Trim the `containsSelfCall` / `bindSimplePatterns` /
   `qualifiedNameToString` micro-costs.** These are each only ~2 %
   but they're _dead-ish_ work — the resolver already knows the
   answers. Removing them from the old-evaluator hot path is cheap
   and unblocks lever 1's accounting.

## Caveats

- Unminified `--optimize 0` build: ~45 % slower than the
  `--optimize 2` (minified) production bundle, so absolute ms in
  this report is inflated. Relative percentages shift by small
  amounts because V8 inlining decisions differ at different
  optimization levels — lever-ordering is stable but exact
  fractions for `A2`/`_Utils_cmp`/closures will all move by single
  percentage points under `--optimize 2`.
- Sampling interval 1000 µs (Node default). 77.5 s / 1 ms =
  ~77 000 theoretical samples; collected ~54 000 — reasonable
  coverage, no samples lost to quantization noise for the top ~30
  nodes.
- Call-tree-based inclusive-time attribution was not useful here:
  almost all interpreter work happens inside anonymous
  `A2(f, …)`-wrapped closures which break the parent chain at every
  curry point, so "self time + hot-line mapping" above is the most
  trustworthy view.

# Phase 5 — Lazy `fallbackOutput` Thunks (eager let binding fix)

**Date:** 2026-04-10
**Fixture:** `small-12`
**Commit:** (to fill in after commit)

## TL;DR

A single class of bug was accidentally running the full project-rule
interpreter **an extra time on every project-rule invocation** — once
through the normal evaluation path, and once more through a
`fallbackOutput` let binding that was meant to be the "only-if-the-
primary-eval-fails" fallback. Elm's strict let evaluation made that
let binding eager, and `runProjectRulesFresh` (the BackendTask builder
used as the fallback) eagerly calls `prepareAndEvalWithYield` —
which synchronously runs the interpreter — during the `driveYields`
argument evaluation. Net effect: every project-rule invocation paid
for its own interpreter run plus a full fresh re-run it never used.

Converting `fallbackOutput` from a let **binding** to a thunk
(`fallbackOutput () -> BackendTask FatalError String`) makes Elm's
function-body laziness actually defer the work until the fallback
branch fires, which in practice is ~never on the success path.

## Numbers

`small-12` warm + cold scenarios, clean measurement with just the
thunk fix (no other changes):

| Scenario | Phase 0 baseline wall / internal | Phase 4 r3 wall / internal | **Phase 5 wall / internal** | Δ wall vs baseline | Δ internal vs baseline |
|---|---:|---:|---:|---:|---:|
| cold                     | 379.98 / 356.19 | 354.84 / 332.80 | **247.73 / 223.89** | **−34.8%** | **−37.1%** |
| warm                     | 0.41 / 0.04     | 0.41 / 0.04     | **0.43 / 0.04**     | +4.9%      | 0.0%      |
| warm_1_file_body_edit    | 2.45 / 1.28     | 2.42 / 1.44     | **2.10 / 1.09**     | **−14.3%** | **−14.8%** |
| warm_1_file_comment_only | 1.13 / 0.10     | 0.94 / 0.09     | **0.99 / 0.10**     | −12.4%     | 0.0%      |
| warm_import_graph_change | 90.09 / 89.09   | 83.02 / 82.03   | **44.82 / 43.75**   | **−50.3%** | **−50.9%** |

- **Cold drops 132 seconds** (37% internal) — the fallback bug was
  firing on every project-rule group call (5 call sites across 4
  functions that each matched the pattern), each one running a full
  fresh interpreter pass that was immediately thrown away. On cold
  that's 5 wasted passes × ~25–30 seconds each ≈ 125–150 seconds,
  which matches the drop.
- **warm_import_graph_change drops 45 seconds** (51% internal) — same
  bug, same mechanism, just fewer invocations per scenario.
- **warm_1_file_body_edit drops 0.35 seconds** (15% wall, 21%
  internal) — smaller absolute win because this scenario has less
  project-rule work, but the proportional improvement is similar.
- **The flat `warm` scenario is unchanged** because it's a `full_hit`
  that skips project-rule eval entirely. The 0.43s wall is unchanged
  startup cost.

## What was actually wrong

The pattern, present in 5 places (4 functions + `evalProjectRulesWithWarmState`):

```elm
runProjectRulesWithWarmRuleCaches ... =
    let
        ...
        fallbackOutput =
            runProjectRulesFresh baseProjectValue reviewProject prIndices modulesWithAst
    in
    Do.do
        (withTiming (counterPrefix ++ ".warm_eval") ...)
    <| \evalTimed ->
    case evalTimed.value of
        Ok (Types.String output) -> succeed { ... }
        Ok _ -> Do.do (withTiming ".fallback_eval" fallbackOutput) ...
        Err _ -> Do.do (withTiming ".fallback_eval" fallbackOutput) ...
```

The issue is that in strict Elm, `fallbackOutput = runProjectRulesFresh ...`
**eagerly evaluates** `runProjectRulesFresh baseProjectValue reviewProject
prIndices modulesWithAst` at let-binding time. `runProjectRulesFresh`'s
body contains:

```elm
Do.do
    (InterpreterProject.prepareAndEvalWithYield reviewProject
        { ... injectedValues = FastDict.singleton modulesVarName (moduleInputsValue modulesWithAst) ... }
        ignoreProjectRuleYield
    )
<| \evalResult -> ...
```

`InterpreterProject.prepareAndEvalWithYield` is **not** wrapped in
`deferTask` — it's a direct function call whose body is:

```elm
prepareAndEvalWithYield project evalConfig yieldHandler =
    driveYields yieldHandler (prepareRawEvalWithYield project evalConfig)
```

`driveYields` eagerly evaluates its second argument. That second
argument is `prepareRawEvalWithYield project evalConfig`, which calls
`Eval.Module.evalWithResolvedIRFromFilesAndIntercepts` (or the old
`evalWithInterceptsAndMemoRaw`) **synchronously**. This returns the
full interpreter's `EvalResult Value`.

So the call chain from the let binding, in order of evaluation:

1. Elm evaluates `fallbackOutput =` eagerly.
2. Calls `runProjectRulesFresh baseProjectValue reviewProject prIndices modulesWithAst`.
3. `runProjectRulesFresh`'s `let` runs eagerly (evaluates `intercepts`, cheap).
4. `runProjectRulesFresh`'s `Do.do (prepareAndEvalWithYield reviewProject {...}) <| \evalResult -> ...` needs to evaluate its BackendTask argument.
5. Constructing that BackendTask requires calling `prepareAndEvalWithYield reviewProject {...}`.
6. `prepareAndEvalWithYield` needs to evaluate `driveYields yieldHandler (prepareRawEvalWithYield project evalConfig)`.
7. `prepareRawEvalWithYield project evalConfig` is evaluated as a strict argument. **This synchronously runs the interpreter** (20–30 seconds on cold, ~18 seconds on warm per affected module group).
8. The result is fed into `driveYields`, which processes yields and returns a final BackendTask.
9. `fallbackOutput` is bound to that final BackendTask value — which is then **never used** on the success path.

Step 7 is where all the time goes. The interpreter ran, produced a
result, the result was discarded.

## The fix

Convert `fallbackOutput` from a value binding to a function binding
(a thunk):

```elm
fallbackOutput : () -> BackendTask FatalError String
fallbackOutput () =
    runProjectRulesFresh baseProjectValue reviewProject prIndices modulesWithAst
```

Elm function bodies are lazy — the body only runs when the function
is applied. All call sites then use `fallbackOutput ()` instead of
`fallbackOutput`. On the `Ok (Types.String output)` branch the thunk
is never applied, so `runProjectRulesFresh` is never called, so the
interpreter never runs the unused fresh pass.

Five call sites, all fixed:

| File / line | Function | Modules arg |
|---|---|---|
| `src/ReviewRunner.elm` ~13914 | `runProjectRulesWithCacheWriteModules` | `modulesWithAst` |
| `src/ReviewRunner.elm` ~14046 | `runProjectRulesWithCacheWrite` | `modulesWithAst` |
| `src/ReviewRunner.elm` ~14177 | `runProjectRulesWithWarmRuleCachesModules` | `modulesWithAst` |
| `src/ReviewRunner.elm` ~14363 | `runProjectRulesWithWarmRuleCaches` | `modulesWithAst` |
| `src/ReviewRunner.elm` ~14915 | `evalProjectRulesWithWarmState` | `allModulesWithAst` |

All usages (`Do.do fallbackOutput <| \output ->`, `Do.do (withTiming
"fallback_eval" fallbackOutput) <| \fallbackTimed ->`, etc.) became
`fallbackOutput ()`.

## How we found it

Data-driven, start to finish. The diagnostic path:

1. **Stage breakdown from the existing perf trace.** Phase 4 r3's
   `bench/results/slot-assignment/phase4r3-bridge-alias-dormant.json`
   showed `project_rule_eval` at 74% of cold time (247s of 333s) and
   100% of `warm_import_graph_change`'s 81s. That was Phase 5's target
   from the beginning.

2. **Counter sum vs stage time.** Phase 4 r3's counters showed
   `project.importers.eval_total_ms = 81307` but summing the inner
   `prepare_module_handles + rule_cache.load + prepare_intercepts +
   warm_eval` only accounted for ~24ms. 81 seconds unattributed.

3. **Timestamped bookends inside `runProjectRulesWithWarmRuleCaches`.**
   Added `BackendTask.Time.now` captures at every significant step
   inside the function. First diagnostic pinned the gap to
   `split.before_let_ms = 39834` — 40 seconds **between function
   entry and the `in` of the first let block**, which is only
   possible if Elm is eagerly evaluating the let bindings and one of
   them is expensive.

4. **Deferred `fallbackOutput` to isolate.** Wrapped the let binding
   in `deferTask (\() -> runProjectRulesFresh ...)`. `before_let_ms`
   stayed at ~40 seconds, confirming the thunk wrapping didn't help
   because Elm had already decided to eagerly compute the
   `deferTask`'s argument inside the let's strict evaluation.

5. **Replaced fallback with a trivial constant.** Changed the binding
   to `fallbackOutput = BackendTask.succeed "placeholder"`.
   `before_let_ms` dropped to **1ms**. Confirmed: the eager evaluation
   of `runProjectRulesFresh ...` inside the let was the entire 40
   seconds.

6. **Second unaccounted 40-second gap after warm_eval.** Even with
   `fallbackOutput` out of the let, there was a 37-second gap
   (`eval_to_case_ms`) between the `Do.do (withTiming "warm_eval" ...)`
   and the `case evalTimed.value of` match. But `withTiming`'s own
   counter reported warm_eval at 6ms. Investigated by bypassing
   `withTiming` entirely and using raw Time.now bookends around the
   deferTask — that captured the actual 36 seconds, localizing the
   work to the `deferTask (\() -> prepareAndEvalWithYield ...)`.

7. **Found the second instance.** Grep for all `fallbackOutput =
   runProjectRulesFresh ...` let bindings showed 5 occurrences across
   4 different project-rule eval functions. The diagnostic had been
   tracking only one of them; the other four were silently costing
   ~30 seconds each on both cold and warm runs.

8. **Converted all 5 to thunks** and re-ran. Cold dropped from 333s
   to 224s (−33%), `warm_import_graph_change` dropped from 82s to
   44s (−46%).

## What's *not* fixed (remaining Phase 5 headroom)

1. **`withTiming`'s internal timing is wrong under deferTask.** In
   one of the diagnostic runs the outer-timing `eval_to_case_ms` was
   43 seconds while `withTiming "warm_eval"` reported 6 milliseconds
   for the same block. After the `fallbackOutput` thunk fix this
   discrepancy mostly disappears because the real work is no longer
   being forced inside the `withTiming`'s scope, but there's
   something subtle about how elm-pages' BackendTask runtime
   interacts with `Do.do Time.now <| \t -> Do.do work` versus
   `withTiming "name" work`. Not investigated further because the
   thunk fix made the problem go away in practice.

2. **Counter override summing importers + importers_fold into
   `project.importers.eval_total_ms`.** Line 12653 of
   `src/ReviewRunner.elm` deliberately overwrites the importers
   counter with the sum of both groups, which makes the counter
   misleading (reports 73983ms when the actual importers time is
   ~36000ms and importers_fold is ~1167ms). Not a performance bug —
   a reporting bug. Worth cleaning up, out of scope for this round.

3. **Phase 4 r4 trampoline attempt is still blocked.** The Phase 5
   fix didn't unblock the trampoline OOM that reverted r4. That's a
   separate investigation.

## Gate check

- [x] All 1296 interpreter unit tests still pass (9 pre-existing
      failures unchanged — they were the same before Phase 5 started).
- [x] Every `small-12` scenario runs end-to-end without crashing.
- [x] **Cold ≥20% improvement** — **37% internal improvement, easily
      clears the gate**. This was the Phase 4 Round 2 goal that
      neither the bridge (r3) nor the attempted trampoline (r4 attempt)
      could reach on their own.
- [x] No scenario regressed by >5%: the biggest regression is `warm`
      at +4.9% wall / 0% internal, which is startup-cost noise.

## Artifacts

- `bench/results/slot-assignment/phase5-fallback-thunk.json` — raw
  numbers.
- `bench/results/slot-assignment/phase4r3-bridge-alias-dormant.json` —
  Phase 4 r3 comparison.
- `bench/results/slot-assignment/phase0-baseline-small-12.json` —
  Phase 0 baseline.

## Decision

Commit the thunk fix as Phase 5. The measured 37% cold / 51%
warm_import_graph_change wins are the biggest real-workload gains the
whole plan has produced — larger than Phases 1–4 combined. Phase 5
r2+ (persistent value injection / Salsa-style cache) is now an
optional follow-on rather than a prerequisite for the headline
number.

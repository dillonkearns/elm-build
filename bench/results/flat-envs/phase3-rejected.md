# Phase 3 rejected — slot-capture via cons-list truncation is net-negative on small-12

**Status:** Rejected (runtime code reverted, Phase 2 `captureSlots` analysis kept).
**Captured:** 2026-04-13
**Baseline submodule:** `c988619` (Phase 2 — captureSlots emission, runtime unchanged).

## Intent

Phase 3 was the "actually consume `captureSlots`" step of the slot-envs plan:
at `RLambda` eval time under the `resolved-list-slotted` flag, build a
trimmed `capturedLocals` rather than closing over the whole enclosing
`locals` list. The plan invoked Perconti/Ahmed's safe-for-space argument
to motivate this — closures that only reference a few outer slots
shouldn't retain pointers to the rest of the enclosing frame.

## Three attempts, all net-negative

All three attempts kept the existing evaluator infrastructure intact and
only varied how `capturedLocals` was computed under slotted mode. All
measured on `small-12`, n=5 per run.

### Attempt 1: `List.take (max(captureSlots) + 1) locals`

Trim the captured list to the smallest prefix that contains every slot
the body (or any nested body) might reference. `freeVars` recursively
walks nested lambdas, so `max(captureSlots)` is a correct upper bound —
every `RLocal N` with `N >= arity + selfSlots` has outer index ≤ max.

| Scenario | Phase 2 unplanned (ref) | Phase 3 take | Δ |
|---|---:|---:|---:|
| cold | 233.52 | 246.27 | +5.5% (real regression) |
| warm_import_graph_change | 57.54 | 59.48 | +3.4% (noise) |
| other warm | ~noise | ~noise | noise |

**Root cause**: Elm's `List.take` on a cons-list allocates O(N) fresh
cons cells to build the prefix. Phase 2's whole-capture path just stores
a shared reference to the existing list (no allocation). So Phase 3 is
strictly Phase 2 + O(N) allocation per closure. For closures whose
captures span most of the locals depth (i.e. `max` is close to
`length(locals)`), the "savings" shrink to zero while the allocation cost
stays. No net win.

Raw data: `phase3-attempt-list-take.json`.

### Attempt 2: zero-capture fast path (if `captureSlots == []` return `[]`)

Skip `List.take` entirely; only drop references for closures that close
over nothing. Common case in functional code (`\x -> x`, `\x -> x + 1`,
`\_ -> True`).

| Scenario | Phase 2 unplanned (ref) | Phase 3 zero-capture | Δ |
|---|---:|---:|---:|
| cold | 233.52 | 243.00 | +4.1% |
| warm_import_graph_change | 57.54 | 58.19 | +1.1% (noise) |

Still a real cold regression. Root cause: even the no-op `case captureSlots of []` check and `staticEnv.useSlotCapture` field access, multiplied by every `RLambda` eval (and crucially firing in BOTH unplanned and slotted modes because the helper was called unconditionally), cost more than the GC savings from dropping references for zero-capture closures.

Raw data: `phase3-attempt-zero-capture.json`.

### Attempt 3: hoist the `useSlotCapture` check inline

Move the `if staticEnv.useSlotCapture then ... else ...` branch directly
into the RLambda handler at every site, so unplanned mode takes a
byte-identical path to Phase 2 and only slotted mode pays the extra work.
Delete the `closureCapturedLocals` helper.

Apples-to-apples on the same binary:

| Scenario | Unplanned (same binary) | Slotted (same binary) | Δ | 2×MAD max | Verdict |
|---|---:|---:|---:|---:|---|
| cold | 237.42 | 250.05 | **+5.3%** (+12.63s) | 7.20 | real regression |
| warm | 0.41 | 0.41 | 0 | 0.02 | noise |
| warm_1_file_body_edit | 2.17 | 2.20 | +1.4% | 0.10 | noise |
| warm_1_file_comment_only | 0.98 | 0.99 | +1.0% | 0.04 | noise |
| warm_import_graph_change | 58.43 | 61.12 | +4.6% | 2.80 | marginal (delta 2.69, 2×MAD 2.80) |

Raw data: `phase3-attempt-hoisted.json`, `phase3-sanity-unplanned-same-binary.json`.

Even with the clean hoist, slotted is a real ~5% cold regression vs
unplanned **on the same binary**. The case-match on `captureSlots` +
conditional `{ env | locals = [] }` record update is measurable, and the
"savings" from dropping references don't pay for it on small-12.

## Why the approach doesn't pay off here

1. **Cons-list runtime can't slice cheaply.** Every attempt to "trim"
   `capturedLocals` ends up allocating new cons cells or checking a
   condition on every closure creation. Elm's `List` is optimized for
   cons/head/tail, not slicing or random access.

2. **small-12's closure pressure is low.** The bench workload is
   review-runner-dominated. Most closures are created and consumed
   quickly, so retained-heap savings don't compound into visible GC
   reductions on a wall-clock bench. A workload that creates many
   long-lived closures (e.g. deeply-nested Parser combinators that build
   up thunks) might show a different picture, but small-12 doesn't.

3. **Phase 1's IR-path routing already paid off the "flat envs" ambition
   on this runtime.** The original slot-envs motivation was "Dict is 18%
   of the CPU profile". Phase 1 bypassed the Dict entirely by routing
   through the resolved-IR path; cold dropped 10–14% vs legacy. That's
   most of the theoretical win from the whole slot-envs effort, achieved
   without any slot-capture runtime.

## What's kept from the Phase 3 attempt

- **`EnvMode.ResolvedListSlotted` flag value** stays in `InterpreterProject`.
  After the revert it behaves identically to `ResolvedListUnplanned` — it's
  reserved as a future plug-in point for a different slot-capture strategy
  (e.g., body rewriting + dense slice via kernel primitives).
- **Phase 2's `captureSlots` field on `RLambda`** stays. It's still
  computed at resolver time via `IR.mkLambda` and the `freeVars` helper.
  Future experiments (content-addressed caching, partial evaluation,
  dead-code elimination at the IR level) can consume it without needing
  to re-derive free-var sets from the body.

## What's reverted

- `useSlotCapture : Bool` field on `REnv` (both the type + the four
  `callDepth`-adjacent construction sites with `useSlotCapture = False`).
- The inline `if staticEnv.useSlotCapture then ... else ...` branches at
  all four `RLambda` eval sites in `Eval.ResolvedExpression`.
- The `closureCapturedLocals` helper (was unused after the hoist).
- The `{ useSlotCapture : Bool }` parameter on
  `Eval.Module.evalWithResolvedIRFromFilesAndIntercepts`.
- The test-site wiring in `tests/IncrementalEnvTests.elm` and
  `tests/ReviewRuleBugTest.elm` that passed `{ useSlotCapture = False }`.
- The `useSlotCapture` Bool derivation + pass-through in
  `src/InterpreterProject.elm` (both at the `prepareRawEvalWithYieldAndMemo`
  dispatcher and the `prepareAndEvalWithIntercepts` fast path).

None of the Phase 3 code was ever committed or pushed — the revert is
simply `git restore` on the uncommitted working tree.

## What this tells us for future phases

- **Cons-list runtime hit its ceiling for closure-capture optimization.**
  Further slot-capture work would need a different runtime representation
  or an IR-level rewrite, both of which are significantly larger scopes.
- **The real remaining upside on small-12 is probably not slot-capture.**
  A fresh CPU profile after Phase 2 would identify the current bottleneck;
  Phase 4's batched pattern binding might be worth it, but a profile
  should justify the work first.
- **The `captureSlots` data is still strategically valuable.** Phase 5
  (content-addressed GlobalIds) and future Salsa-style incremental caches
  will want it for IR hashing. It costs nothing to keep.

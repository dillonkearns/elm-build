# Interpreter Memo Benchmark

## Purpose

This benchmark now compares three paths on the same workload:

- plain evaluation
- host-driven memoization through the existing `EvYield` hook loop
- interpreter-local memoization with reusable cache state across invocations

The goal is not yet full `elm-review` speed. The goal is to validate the
foundation: can runtime-local memoization preserve semantics, avoid host
round-trips, and improve the constant factors we will depend on for warm
`elm-build`-driven runs?


## Harness

Entry point:

- `src/InterpreterMemoBenchmark.elm`

Recommended run:

```bash
bunx elm-pages bundle-script src/InterpreterMemoBenchmark.elm --output dist/interpreter-memo-benchmark.mjs
node dist/interpreter-memo-benchmark.mjs
```

The bundled Node path matters because it:

- uses optimized generated output
- avoids `elm-pages run` wrapper noise
- keeps project loading fixed inside one process


## What It Measures

Default scenarios:

- `single-call`
- `same-arg-8`
- `mixed-16-4unique`
- `unique-8`

Each scenario compares:

- plain result and average time
- hook-memo cold and warm runs
- runtime-memo cold and warm runs
- lookup/hit/miss/store counts
- cache entry counts

The warm runtime path reuses the returned `MemoRuntime.MemoCache` across a
second invocation, which mirrors the kind of reuse we want through
`elm-build`'s warm path.


## Tuning Knobs

- `--iterations N`
- `--work-scale N`
- `--scenario NAME`

Examples:

```bash
node dist/interpreter-memo-benchmark.mjs --scenario same-arg-32 --work-scale 4 --iterations 5
node dist/interpreter-memo-benchmark.mjs --scenario unique-32 --work-scale 4 --iterations 5
```


## Important Correction

An earlier version of this benchmark reported `0-1ms` timings that looked far
too small. That was a harness bug: the pure eval work was happening before the
timer started because the `BackendTask` was being constructed eagerly.

That is fixed now. The timer wraps a thunk, so the expensive pure evaluation is
inside the measured window.


## Current Results

Measured after fixing the timing bug and adding the fast path that skips shallow
arg fingerprinting when a `qualifiedName` has no memo buckets yet.

### Hot Reuse

Command:

```bash
node dist/interpreter-memo-benchmark.mjs --scenario same-arg-32 --work-scale 4 --iterations 5
```

Result:

| Scenario | Plain | Hook Cold | Hook Warm | Runtime Cold | Runtime Warm |
|---|---:|---:|---:|---:|---:|
| `same-arg-32` | 6995.8ms | 226.6ms | 0.6ms | 220.4ms | 0.4ms |

Key stats:

- hook cold: `32 lookups, 31 hits, 1 miss, 1 store`
- runtime cold: `32 lookups, 31 hits, 1 miss, 1 store`
- hook warm: `32/32 hits`
- runtime warm: `32/32 hits`

Takeaway:

- runtime-local cold is slightly faster than hook-driven cold (`220.4ms` vs `226.6ms`)
- runtime-local warm is also faster (`0.4ms` vs `0.6ms`)
- both massively beat plain once the repeated call becomes memoizable

### Mostly Misses, Then Warm Hits

Command:

```bash
node dist/interpreter-memo-benchmark.mjs --scenario unique-32 --work-scale 4 --iterations 5
```

Result:

| Scenario | Plain | Hook Cold | Hook Warm | Runtime Cold | Runtime Warm |
|---|---:|---:|---:|---:|---:|
| `unique-32` | 6802.4ms | 6804.4ms | 0.8ms | 6617.0ms | 0.4ms |

Key stats:

- hook cold: `32 lookups, 0 hits, 32 misses, 32 stores`
- runtime cold: `32 lookups, 0 hits, 32 misses, 32 stores`
- hook warm: `32/32 hits`
- runtime warm: `32/32 hits`

Takeaway:

- on all-miss cold work, runtime-local memo is still a bit faster than hook-driven memo
- the no-bucket fast path appears to help here because we avoid shallow hashing on the
  obvious first-miss path
- on the warm rerun, runtime-local memo avoids enough overhead to cut the all-hit path
  roughly in half (`0.4ms` vs `0.8ms`)


## Interpretation

This is the first evidence that the runtime-local memo design is architecturally
sound and not just theoretically nice.

What these results say:

- semantics are preserved across plain, hook, and runtime memo paths
- cache reuse across invocations works
- avoiding host round-trips produces a small but real improvement on cold runs
- the improvement is bigger on the warm all-hit path
- the qualified-name-first fast path is pointed in the right direction

What they do not say yet:

- that we have full Salsa-style invalidation
- that this is already enough to move `elm-review` end-to-end numbers
- that the current memo keying is the final design


## Direct Internal Effects

After replacing the generic memo `EvYield` payload round-trip with dedicated
`EvMemoLookup` / `EvMemoStore` result variants, I reran the same bundled
benchmark commands:

| Scenario | Plain | Hook Cold | Hook Warm | Runtime Cold | Runtime Warm |
|---|---:|---:|---:|---:|---:|
| `same-arg-32` | 6848.4ms | 215.6ms | 0.6ms | 210.6ms | 0.4ms |
| `unique-32` | 6621.2ms | 6584.6ms | 0.8ms | 6612.6ms | 0.4ms |

Takeaway:

- the direct internal memo effects shaved a bit more off the runtime-local cold path
- the warm all-hit path stayed clearly better than the hook-driven version
- the improvement is real but still small, which matches the runner result:
  the memo architecture is sound, but constant factors still matter a lot


## Memo Spec Registry

I then replaced the generic `Set.member` + per-call qualified-name branching in
`Eval.Expression` with a prebuilt memo spec registry:

- callers still pass a set of qualified names
- the interpreter now resolves that once into a `MemoSpec.Registry`
- hot-path lookup becomes one registry read, followed by the preselected key strategy

On the bundled `same-arg-32` benchmark:

| Scenario | Plain | Hook Cold | Hook Warm | Runtime Cold | Runtime Warm |
|---|---:|---:|---:|---:|---:|
| `same-arg-32` | 6858.2ms | 215.6ms | 0.6ms | 215.8ms | 0.4ms |

Takeaway:

- this change did not produce a measurable microbenchmark win by itself
- it does remove dispatch logic from the evaluator hot path and gives us a cleaner place
  to add richer memo metadata next


## Spec Id Buckets And Compact Projected Keys

I then made two more internal memo-runtime changes:

- memo cache namespaces are keyed by integer spec ids instead of qualified-name strings
- projected-key specs can use a compact direct-value cache shape instead of the generic
  nested dict-plus-list structure

On the bundled `same-arg-32` microbenchmark, neither change produced a clear win:

| Scenario | Plain | Hook Cold | Hook Warm | Runtime Cold | Runtime Warm |
|---|---:|---:|---:|---:|---:|
| `same-arg-32` | 6869.0ms | 220.6ms | 0.4ms | 217.4ms | 0.6ms |

Takeaway:

- these changes are targeted at real runner seams with multiple memoized specs and
  projected keys, so it is not surprising that the single-function structural microbenchmark
  stayed roughly flat
- the review-runner benchmark is the better signal for these particular optimizations


## No-Profile Spec Id Payloads

I then threaded `collectMemoStats` into the interpreter config so normal
memoized runs can omit `qualifiedName` from the memo payload entirely and stay
on `specId` only unless profiling is explicitly enabled.

On the same bundled `same-arg-32` benchmark:

| Scenario | Plain | Hook Cold | Hook Warm | Runtime Cold | Runtime Warm |
|---|---:|---:|---:|---:|---:|
| `same-arg-32` | 6685.8ms | 214.8ms | 0.4ms | 208.8ms | 0.6ms |

Takeaway:

- this is a small but real cold-path improvement over the recent `217.4ms` runtime-cold band
- it fits the runner result too: removing a little more memo payload overhead helped, but the
  effect size is still modest
- the remaining overhead is likely in effect construction/dispatch and in choosing seams whose
  bodies are expensive enough to amortize the memo machinery


## Next Step

The next implementation step should be to carry this runtime-local memo system
into one real `elm-review`-relevant computation family and measure it there.

Best candidates:

- per-file AST-derived helper computations
- project-rule contribution extraction
- other repeated pure helpers called many times with identical inputs

At that point, this benchmark remains the micro-level regression check for the
memo runtime itself, while the review-runner benchmarks tell us whether the new
foundation is paying off end to end.

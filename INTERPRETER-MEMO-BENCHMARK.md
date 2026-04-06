# Interpreter Memo Benchmark

## Purpose

This benchmark is meant to validate the eval-loop-hook approach itself before
we build more caching on top of it.

The question is not yet "does memoization make `elm-review` faster?"

The first question is:

"Can a generic intercept + yield/resume memo path preserve semantics for
repeated qualified-function calls in a single expression, and what does that
cost?"


## Benchmark Harness

Entry point:

- `src/InterpreterMemoBenchmark.elm`

Recommended run:

```bash
bunx elm-pages bundle-script src/InterpreterMemoBenchmark.elm --output dist/interpreter-memo-benchmark.mjs
node dist/interpreter-memo-benchmark.mjs --iterations 1
```

Why this path:

- avoids `elm-pages run` CLI overhead
- uses bundled optimized output
- keeps project load fixed inside one process


## What It Measures

It runs two scenarios:

- `single-call`
- `repeat-8`

Both scenarios generate:

1. a trivial `probe` function intercepted through the generic yield loop
2. a heavier `expensive` function intercepted through a generic in-memory memo
   lookup/store path

The harness records:

- probe call counts
- memo lookup/hit/miss/store counts
- plain result value
- memo cold result value
- memo warm result value


## Current Result

Current bundled run:

```text
Scenario: single-call
  probe calls: 1
  probe value: 2001
  memo cold lookups: 1
  values: plain=3003000, cold=3003000, warm=3003000
  plain avg: 2ms
  memo cold avg: 0ms
  memo warm avg: 1ms
  cold stats: lookups=1, hits=0, misses=1, stores=1, probeCalls=0, entries=1
  warm stats: lookups=1, hits=1, misses=0, stores=0, probeCalls=0, entries=1

Scenario: repeat-8
  probe calls: 8
  probe value: 16008
  memo cold lookups: 8
  values: plain=24024000, cold=24024000, warm=24024000
  plain avg: 0ms
  memo cold avg: 1ms
  memo warm avg: 0ms
  cold stats: lookups=8, hits=7, misses=1, stores=1, probeCalls=0, entries=1
  warm stats: lookups=8, hits=8, misses=0, stores=0, probeCalls=0, entries=1
```


## Interpretation

The benchmark is now semantically trustworthy.

### What is now correct

- `single-call` behaves as expected
- `repeat-8` now preserves all repeated-call semantics
- probe call counts match the source shape
- plain, memo cold, and memo warm all return the same value
- cold and warm memo states look structurally correct, with one miss/store on
  cold and then all hits on warm


## Why This Matters

This benchmark is now a usable foundation for the memoization architecture.

The important thing is that the generic intercept path is no longer disqualified
by a correctness bug. We can now use this harness to measure constant factors
without wondering whether memoization is silently changing evaluation results.


## Next Step

The next task is to make the benchmark heavy enough to measure actual
performance differences.

At the current workload, optimized bundled timings round to `0-2ms`, so this is
now mostly a semantic gate. The next benchmark pass should increase work and/or
iteration counts so we can compare:

- plain
- memo cold
- memo warm

on a signal large enough to expose the real overhead and payoff of the hook
design.

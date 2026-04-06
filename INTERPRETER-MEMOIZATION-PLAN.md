# Interpreter Memoization Plan

## Thesis

We already have some of the right primitives in `elm-interpreter` for Salsa-style
memoization, but we do **not** yet have the right **system**.

Today we have:

- qualified-function intercepts
- `EvYield` / resume support
- value fingerprinting and deep hashing
- lightweight call counting for profiling

That is enough to prototype coarse cache hooks, but not enough to build a fast,
generic, invalidation-aware memo engine that can become the foundation for
`elm-review` incremental evaluation.

The key design shift is:

- keep **intercepts** as the **control plane**
- move memo lookup/storage/dependency tracking into the **interpreter runtime**

The failed hook-only prototype was slow because it used the control plane as the
data plane.


## Do We Already Have The Right Machinery?

Partially.

### What we already have

`elm-interpreter` already exposes the core hook points:

- `Types.Config.intercepts`
- `Types.EvalResult.EvYield`
- `Eval.Expression.deepHashValue`
- `Types.CallCounts`

That means we can already:

- identify a qualified function call before normal AST evaluation
- override that call
- suspend evaluation and ask the host to do something
- compute fingerprints for values
- profile which functions look memoizable

That is a strong starting point.

### What is still missing

A real Salsa-style system also needs:

- a first-class memo table
- cheap in-memory lookup on the hot path
- dependency tracking between memoized computations
- a revision model for inputs and invalidation
- a clear boundary between fast runtime hits and slower persistence
- a generic way to opt functions into memoization without bespoke hook code

Today those are missing, so every cache experiment tends to reinvent them at the
runner layer.


## Current State

### Current interpreter machinery

The current call interception flow is:

1. Interpreter resolves a qualified function call.
2. It checks `cfg.intercepts`.
3. If there is an intercept, it runs that function instead of normal AST eval.

That is a good seam for:

- effect callbacks
- marker functions
- rule cache save/load boundaries
- profiling

It is **not** yet a low-overhead memo runtime.

### Current `ReviewRunner` usage

`ReviewRunner` currently uses intercepts for coarse markers such as:

- `Review.Rule.initialCacheMarker`
- `Review.Rule.finalCacheMarker`
- `Review.Cache.ContextHash.createContextHashMarker`
- `ReviewRunnerHelper.projectCacheMarker`

Those are useful integration seams, but they are still external hook points that
surface large values and often cross the interpreter/host boundary.


## Why The First Hook-Only Prototype Regressed

The slowdown looked incidental, not inherent to memoization.

The prototype was slow because it memoized at the wrong layer:

- it split caches after normal evaluation instead of hitting a memo table before eval
- it used `EvYield` for fine-grained cache work
- it serialized large `Types.Value` structures eagerly
- it did extra helper evaluation to split and rehydrate caches
- it pushed expensive logic into cache-save/cache-load boundaries

In other words:

- the cache lookup cost was too high
- the cache materialization cost was too high
- the host/interpreter boundary was crossed too often

That is not a flaw in memoization itself. It is a flaw in the placement of the
memo logic.


## Main Design Flaws In The Current Architecture

### 1. Intercepts are being asked to do too much

Intercepts are currently both:

- the place where we detect special functions
- the place where we try to implement caching behavior

That makes them too expensive for high-frequency memo hits.

### 2. `EvYield` is too coarse for hot-path memo hits

`EvYield` is great for:

- framework effects
- coarse checkpoints
- host-managed persistence

It is not a good fit for:

- every memoized function hit
- every fine-grained dependency edge
- every cache materialization event

Using it that way turns a local lookup into a control-flow boundary crossing.

### 3. We do not have a runtime-owned memo table

There is currently no first-class runtime store for:

- memo entries
- dependency edges
- revisions
- dirty flags
- query stack state

Without that, the interpreter cannot answer a memo hit cheaply.

### 4. We do not have a query graph

Salsa is not just memoization. It is memoization plus dependency tracking.

Right now, if computation `A` depends on `B`, that relationship is not captured
generically by the interpreter.

Without that, invalidation falls back to:

- rerunning too much
- bespoke cache wiring per feature
- coarse artifact replacement

### 5. Our hash story is incomplete

We currently have two different tools:

- cheap shallow fingerprints for recursion detection
- deeper hashes for cache keys

That is useful, but still incomplete:

- shallow fingerprints are not correctness-grade cache keys
- deep hashes may be too expensive to compute naively everywhere
- closures cannot be meaningfully hashed by content today

We need explicit memo key policy, not one generic hash used everywhere.

### 6. Persistence is happening at the wrong boundary

Today persistence is mostly driven by coarse save/load markers.

That encourages designs that:

- serialize too much
- serialize too often
- treat internal cache structure as external API

Persistence should be a secondary concern layered on top of a good in-memory
runtime model.


## Design Principles

### 1. Intercepts should be the control plane, not the data plane

Interpreted function calls can still consult an intercept registry to decide
whether a function is memoized, but actual memo hits should happen inside the
runtime with no yield and no helper re-eval.

### 2. Hot-path hits must stay in memory

If a memo hit requires:

- serialization
- host IO
- `EvYield`
- rebuilding large `Value` structures from disk

then it is probably too expensive for the core fast path.

### 3. Disk persistence should be batched and secondary

Persistence should be:

- coarse
- optional
- batched
- versioned

not part of every memo hit.

### 4. Memoization should be opt-in and sound

We should start by memoizing only:

- qualified top-level functions
- pure functions
- functions with stable argument semantics

Local lambdas and closure-heavy values should not be in scope for phase 1.

### 5. Dependency tracking must be automatic once a function is opted in

Once a function is registered as memoized, the interpreter should automatically:

- record parent/child query edges
- associate results with revisions
- reuse results when dependencies are still valid

That is the part that turns “cache entries” into a real query system.


## Proposed Architecture

## Layer 1: Query registration

Introduce a registry of memoized functions.

Conceptually:

```elm
type alias MemoSpec =
    { keyStrategy : KeyStrategy
    , durability : Durability
    , allowPersistence : Bool
    , allowClosures : Bool
    }
```

Key points:

- keyed by qualified function name
- opt-in, not automatic for every function
- allows per-function key strategy
- allows us to reject unsafe argument shapes early

Examples of key strategy:

- deep hash all args
- deep hash selected args only
- custom stable key builder
- “not memoizable when any arg is a closure”


## Layer 2: Runtime-owned memo state

Introduce a runtime-owned memo subsystem.

Conceptually:

```elm
type alias MemoRuntime =
    { revision : Int
    , activeStack : List QueryKey
    , entries : Dict QueryKey MemoEntry
    , reverseDeps : Dict QueryKey (Set QueryKey)
    , dirty : Set QueryKey
    }

type alias QueryKey =
    { fn : String
    , argHash : Int
    }

type alias MemoEntry =
    { value : Value
    , verifiedAt : Int
    , changedAt : Int
    , deps : Set QueryKey
    }
```

The exact shape may change, but these responsibilities need to exist somewhere.

### Important note about implementation

Semantically this is runtime state.

There are two implementation options:

1. Thread it explicitly through the interpreter in Elm.
2. Back it with hidden mutation under the hood while preserving pure semantics
   at the interpreter API boundary.

For performance, option 2 is likely the stronger long-term path.


## Layer 3: Query evaluation protocol

For a memoized qualified function call:

1. Build `QueryKey` from function name + key strategy.
2. Check runtime memo table.
3. If entry exists and is still valid, return it immediately.
4. If not, push query onto `activeStack`.
5. Evaluate function normally.
6. During nested memoized calls, record dependency edges.
7. Store final result in memo table with its dependency set.
8. Pop query from the active stack.

That means the interpreter itself learns:

- who depended on whom
- which results were reused
- which results were recomputed

This is the missing core of the current design.


## Layer 4: Inputs and revisions

To get Salsa-like invalidation, we need a notion of inputs.

Examples:

- file content by path
- parsed AST by file hash
- package manifest identity
- review app identity
- rule configuration identity

Those should be represented as input queries with revisions or stamps.

Then a higher-level query can depend on those inputs instead of embedding all
invalidation logic manually.

This is what allows:

- “one file changed”
- “review config changed”
- “dependency graph changed”

to invalidate only the right derived computations.


## Layer 5: Persistence as snapshotting

Persistence should sit outside the hot path.

Recommended model:

- keep memo runtime in memory during execution
- at well-chosen checkpoints, snapshot selected durable entries
- on next process start, preload those durable entries into the memo table
- treat preloaded entries as candidates that still need revision validation

This differs from the current marker-heavy approach in an important way:

- we persist memo runtime state because it is useful
- we do not design the runtime around serialization boundaries


## How This Differs From The Current Hook Approach

### Current approach

- marker functions are intercepted
- cache data is yielded to the host
- host serializes values
- later runs rehydrate those values externally
- helper functions are used to split or rebuild caches

### Proposed approach

- the interpreter sees a memoized function call
- it performs an in-memory query lookup immediately
- dependency recording happens automatically
- no yield occurs on a normal memo hit
- persistence is a later snapshot concern

This is a major cost-model improvement.


## Recommended First Scope

Phase 1 should **not** try to memoize everything.

Start with:

- qualified top-level functions only
- no local lambdas
- no memoization when any key-participating arg is a closure
- in-memory cache only
- no disk persistence yet
- dependency recording enabled from day 1

This gives us the smallest version that is still architecturally correct.


## Candidate Categories For elm-review

Once the generic machinery exists, the best first targets are functions that are:

- pure
- called frequently
- stable across many runs
- naturally shaped like per-file or per-module contributions

Good categories include:

- per-module cache builders
- module-to-project contribution builders
- rule helper functions that derive reusable context from AST/module inputs
- repeated project-fold helpers that are currently re-executed after a small edit

The important point is that these should become **query nodes**, not bespoke
runner-level artifacts.


## Phased Plan

### Phase 0: Measure and model candidates

Extend the existing profiling support so we can identify high-value candidates.

Deliverables:

- better call-count profiling for repeated qualified functions
- per-function argument-shape stats
- a shortlist of safe phase-1 memo candidates

### Phase 1: Add a generic in-memory memo engine

Deliverables:

- memo spec registry
- runtime-owned memo table
- query stack
- dependency recording
- zero-yield memo hits

Success criterion:

- miss-only runs should not regress badly
- repeated in-process hits should be obviously faster

### Phase 2: Add revisions and input queries

Deliverables:

- explicit input/query distinction
- revision stamps
- invalidation of dependent queries only

Success criterion:

- small input changes invalidate a narrow dependency slice

### Phase 3: Add durable snapshotting

Deliverables:

- stable serialized format for durable memo entries
- manifest/version validation
- selective preload on startup

Success criterion:

- warm process restarts preserve useful memo state without dominating runtime

### Phase 4: Apply to elm-review families

Deliverables:

- first memoized `elm-review` function families
- benchmarked 1-file change improvements
- evidence that contribution-style queries reuse properly

Success criterion:

- warm 1-file runs improve materially beyond the current baseline


## Implementation Strategy Recommendation

### Short version

Design the API as a real query engine, but do **not** force the first
implementation to be purely functional if that becomes a performance tax.

### Recommended split

1. Define the semantics cleanly in Elm terms:
   - query keys
   - memo specs
   - dependency tracking
   - revisions

2. Keep the public interpreter API semantically pure.

3. Be open to a hidden mutable implementation underneath if that gives the
   right constant factors.

This is consistent with the project’s broader philosophy:

- preserve referential transparency at the user-facing level
- swap in a faster implementation internally when it is semantically safe


## Risks And Open Questions

### Closure arguments

Closures do not currently have a meaningful stable content hash.

Initial plan:

- phase 1 should reject or bypass memoization when closures participate in the key

### Value hashing cost

Deep hashing large values may itself become expensive.

We may need:

- custom key strategies
- structural key extraction
- cached hashes for certain value families

### Runtime state storage

If we implement the memo runtime with persistent Elm structures only, the update
cost may be too high on very hot paths.

This is the biggest implementation choice to validate early.

### Recursion and re-entrancy

Memoized recursive queries need careful handling so that:

- we do not incorrectly treat an in-progress query as complete
- cycles are detected or represented cleanly

This needs to coexist with the existing recursion guard machinery.

### Persistence format

Persisting arbitrary `Types.Value` safely and compactly may need:

- selective durability
- better codecs
- possibly a lower-level/binary representation later


## What Success Looks Like

We should consider this foundation successful if it gives us:

- near-zero overhead on in-memory memo hits
- generic dependency-aware invalidation
- little or no host boundary crossing on hot hits
- reusable memo infrastructure across `elm-review`, `elm-pages`, and other
  interpreter workloads
- a cleaner path to the “unfair advantage” 1-file warm run story


## Practical Next Step

The next implementation step should be a small vertical slice:

1. add a memo spec registry for qualified functions
2. implement in-memory memo hits inside the interpreter call path
3. record parent/child query edges
4. benchmark with no persistence at all

That will tell us whether the core runtime design has the right constant factors
before we complicate it with serialization, disk snapshots, or broader `elm-review`
integration.

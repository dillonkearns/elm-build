# Architecture And Performance Analysis

Date: 2026-04-12

## Executive Summary

This repo has several genuinely strong pieces already in place:

- `elm-build` has a clean Shake-like DAG and content-addressed build model.
- `InterpreterProject` has the right high-level idea: pre-load package state once, build a reusable base env, then run many evals against it.
- `ReviewRunner` is attacking the right seam by changing the unit of recomputation instead of only micro-optimizing monolithic reruns.
- The mutation runner is using semantic dependency selection, which is the right direction.

The main weakness is not lack of cleverness. It is lack of one canonical incremental model.

Right now the system is split across:

- the old AST/string-keyed interpreter
- the newer resolved-IR interpreter
- `elm-build` content hashes
- `SemanticHash` declaration hashes
- `ReviewRunner` fact hashes and declaration caches
- memo runtime fingerprints

Those pieces are individually promising, but they do not yet compose into one coherent query graph. That causes drift, duplicated invalidation logic, and cache correctness boundaries that are weaker than they look.

My short verdict:

1. The repo is already beyond “PoC” in several important subsystems.
2. The next big wins are architectural, not more micro-tuning.
3. The most important immediate work is to tighten correctness boundaries before making caches more aggressive.
4. The most important medium-term decision is whether the resolved-IR path is actually the future or whether the old evaluator remains the real engine.

## Validation I Ran

I validated what I could locally.

- Top-level `npm test` passed:
  - 29 tests passed
  - `ValueWireCodec` round-trip self-test passed
- `elm-interpreter` package tests are not fully green right now:
  - 1362 passed
  - 8 failed
  - current failures are in Task support, coverage, one Bytes edge case, and mutual recursion
- Additional top-level Elm test modules for review/mutation-specific suites did not run cleanly under the current `elm-test` environment because `Lamdera.Wire3` was not available there.

The failing nested interpreter tests matter, because they line up with some of the architectural gaps below.

## Current State By Subsystem

### Interpreter

The old interpreter path is still the mature path. It has:

- intercepts
- yields
- memo runtime hooks
- tail-call handling
- many practical micro-optimizations

The resolved-IR path is promising, but it is not yet the canonical runtime.

The strongest evidence is in `src/InterpreterProject.elm`:

- `useResolvedIRPath = True` is documented as if the resolved-IR path is active.
- But the actual `useResolvedIRPath` branches still call `Eval.Module.evalWithEnvFromFilesAndValuesAndInterceptsAndMemoRaw` at `src/InterpreterProject.elm:2914-2936` and `src/InterpreterProject.elm:3305-3313`, which is the old evaluator entry point, not `evalWithResolvedIRFromFilesAndIntercepts`.

That means the conceptual architecture and the operational architecture have drifted apart.

There is a deeper issue too: `ProjectEnv` is split-brain.

`elm-interpreter/src/Eval/Module.elm` stores:

- `env`
- `allInterfaces`
- `resolved`

But the incremental update APIs only keep `env` coherent.

Examples:

- `replaceModuleInEnv` keeps `resolved = projectEnv.resolved` at `elm-interpreter/src/Eval/Module.elm:2297-2300`
- `extendWithFiles` keeps `resolved = projectEnv.resolved` at `elm-interpreter/src/Eval/Module.elm:3982-3985`

So if the resolved-IR path were actually used on top of those incremental APIs, it would be operating against stale resolution metadata.

That problem already leaks into loading:

- `InterpreterProject.loadWithProfile` builds `baseUserEnv` with `Eval.Module.extendWithFiles pkgEnv userModulesInOrder` at `src/InterpreterProject.elm:1683-1686`
- but `extendWithFiles` does not update `resolved`

So `baseUserEnv` contains user modules in `env`, but not in `resolved`.

That is the biggest architectural inconsistency in the repo right now.

### `elm-build`

`src/Cache.elm` is a strong local build substrate.

What is good:

- the monadic DAG model is clean
- file/directory outputs compose nicely
- `commandInReadonlyDirectory`, `commandInLinkedDirectory`, and `compute` are useful primitives
- the dependency story is understandable

What it is today:

- closest to Shake
- not yet close to Nix-level hermeticity

Why:

- file hashes are truncated to 8 hex chars in `inputHash` at `src/Cache.elm:953-961`
- internal content hashes use `FNV1a.hash` into `Hash Int` at `src/Cache.elm:1020-1024`
- pure computations are keyed by `label + depsHash` in `Cache.compute` at `src/Cache.elm:598-607`
- command actions do not capture tool version, environment, OS, or undeclared reads

So the current model is:

- deterministic enough for local experiments if callers are disciplined
- not strong enough for “this cache is correctness-grade” claims

### `InterpreterProject`

`InterpreterProject` is doing real work and is one of the best-designed layers in the repo.

Strong parts:

- package summary cache
- user normalization cache
- `baseUserEnv`
- `semanticIndex`
- file override support
- yield/intercept/memo integration

Weak parts:

- too many overlapping identity schemes
- a few unsound shortcuts

The clearest unsound shortcut is in `evalWithFileOverrides`:

- `sourceOverrideKey` only includes the first 100 characters of each source override at `src/InterpreterProject.elm:2200-2204`

That is not safe as a cache key. It can produce false hits when two override sources share the same first 100 chars.

There are also two easy missed perf wins:

- `evalSimple` ignores `baseUserEnv` and always evaluates from `project.packageEnv` plus reparsed user sources at `src/InterpreterProject.elm:2326-2359`
- `evalWithCoverage` does the same at `src/InterpreterProject.elm:2377-2425`

Those functions are used in runner code that performs many repeated helper evals. They should have the same base-env fast path that the richer eval APIs already have.

### Review runner

`ReviewRunner` is the most strategically promising subsystem in the repo.

What is already strong:

- build directory is namespaced by a review app hash
- declaration cache is also scoped by execution mode
- file analysis cache exists
- host-side fact extraction exists
- rule family invalidation is not purely file-based any more

This is all good.

The remaining issue is fragmentation, not direction.

Today review work is spread across:

- per-file analysis cache
- per-declaration cache
- rule-info cache
- importers/deps caches
- runtime memo/cache markers
- interpreter eval caches

That means there is still no single query graph explaining:

- what depends on what
- what invalidates what
- which cache is authoritative

So the runner is getting wins, but it is getting them through several partially-overlapping cache layers rather than one coherent incremental engine.

The checked-in progress notes already point to the practical next bottleneck: fixed setup cost and remaining module-rule evaluation, not more generic project-rule caching. I agree with that prioritization.

### Test runner

The test runner is pragmatic and workable, but it is still paying repeated helper-eval overhead that should be avoidable once `evalSimple` is fixed.

One design smell is duplication of the helper module:

- `src/SimpleTestRunner.elm` exists as a real module
- `src/TestRunner.elm` also embeds `simpleTestRunnerSource`
- `src/MutationTestRunner.elm` embeds another copy

That is manageable, but it creates drift risk in behavior and in cache keys.

### Mutation runner

The mutation runner has the right high-level lever:

- use semantic diffs to decide which tests/runners need to re-run

That is exactly the right direction.

But there are some glaring omissions.

The biggest low-hanging win is baseline collection:

- per-runner baselines are collected by calling `SimpleTestRunner.runNth` once per runner at `src/MutationTestRunner.elm:373-390`

That repeatedly re-decomposes the suite. A single `runEach` baseline pass would be much cheaper and would produce exactly the data the runner later wants to compare against.

There is also a half-implemented coverage lane:

- `probeLines` is precomputed for coverage filtering at `src/MutationTestRunner.elm:288-298`
- `NoCoverageResult` exists in the result type at `src/MutationTestRunner.elm:939-944`
- but no code path actually emits `NoCoverageResult`

So there is already a clear perf idea in the codebase that has not been wired through.

## Integration Assessment

The integration story is good enough to deliver value today, but it is not clean enough to be the long-term architecture.

What is working:

- `InterpreterProject` is already the natural seam between source loading, semantic identity, and evaluation.
- `ReviewRunner` is proving that finer-grained invalidation and reuse can work on top of that seam.
- `elm-build` is a credible outer orchestration layer for persistent artifacts and external processes.

What is not working:

- each layer brings its own notion of identity and invalidation
- runners still encode too much of their own cache policy instead of relying on shared queries
- helper/test harness source is duplicated in multiple places, which increases drift and weakens cache determinism
- the interpreter and runners are sharing results, but they are not yet sharing one canonical incremental model

The main missed integration opportunity is this:

- `elm-build` should eventually persist outputs of a shared query graph
- `InterpreterProject` should own the semantic inputs and executable artifacts for that graph
- `ReviewRunner`, `TestRunner`, and `MutationTestRunner` should mostly become consumers of shared queries, not owners of their own bespoke invalidation logic

Right now the stack is still more “several smart layers cooperating” than “one coherent engine”.

## Low-Hanging Perf Wins

These are the fastest improvements I see with good payoff-to-risk ratio.

### 1. Give `evalSimple` and `evalWithCoverage` the same `baseUserEnv` fast path as the other eval APIs

Why it matters:

- `TestRunner`
- `MutationTestRunner`
- helper evals
- runner counting
- test value discovery

all do lots of small evaluations.

Right now those functions rebuild from `project.packageEnv` plus reparsed user sources every time.

Expected impact:

- immediate speedups in test discovery and mutation setup
- probably the easiest perf win in the repo right now

### 2. Collect mutation baselines in one `runEach` call, not `runNth` N times

Why it matters:

- current code repeats `Test.Runner.fromTest` work per baseline
- the runner already has a `runEach` protocol and separator format

Expected impact:

- significantly lower fixed mutation startup cost
- simpler code path

### 3. Actually wire coverage pruning in the mutation runner

The structure is already there:

- `probeLines`
- `Coverage`
- `NoCoverageResult`

If you finish that lane, you should be able to skip obviously uncovered mutants early and reduce runner execution further.

### 4. Stop pretending the resolved-IR path is active if it is not

Either:

- actually wire `Eval.Module.evalWithResolvedIRFromFilesAndIntercepts`

or:

- set `useResolvedIRPath = False`
- delete or quarantine the dead branches

Right now there is conceptual load without corresponding runtime value.

### 5. Deduplicate the embedded `SimpleTestRunner` source

This is not the biggest perf win, but it will reduce drift and make caching behavior more predictable.

## Architectural Concerns

### 1. `ProjectEnv` is not a coherent immutable state object yet

This is the biggest one.

The type looks like one logical state:

- env
- interfaces
- resolved metadata

But the update functions only maintain part of it.

That means:

- stale sidecars are possible
- resolved-IR reuse is hard to reason about
- any future incremental query engine built on `ProjectEnv` will inherit a correctness trap

I would either:

- make every mutating builder return a fully coherent `ProjectEnv`

or:

- split it into distinct types so you cannot accidentally treat “updated env + stale resolved metadata” as one valid project state

### 2. Persistent cache boundaries are not data-only yet

`ValueCodec` is explicit that closures/regex/decoders are encoded as sentinels in `elm-interpreter/src/ValueCodec.elm:3-8` and `:83-91`.

`ValueWireCodec` is even narrower. It only handles a subset of `Value` variants and falls back to an unknown tag for the rest at `src/ValueWireCodec.elm:9-61`.

That is fine for carefully-scoped caches.

It is not a general persistence story for interpreter state.

If you want aggressive persistent reuse, the cache boundary must become explicitly data-only, not “best-effort encode a runtime `Value`”.

### 3. `GlobalId` is not stable across processes

`elm-interpreter/src/Eval/ResolvedIR.elm:224-230` explicitly says `GlobalId` is only stable for one `ProjectEnv`.

That makes sense for now, but it also means the resolved-IR layer cannot yet be the backbone of deterministic persisted caching.

If resolved IR is the future, stable semantic IDs need to come sooner rather than later.

### 4. The hash width is too small for a serious CAS

This affects both `elm-build` and surrounding caches.

Using truncated or 32-bit hashes is fine for local experimentation.
It is not fine if you want to lean harder on:

- long-lived caches
- cross-run reuse
- cross-machine reuse
- aggressive incremental correctness

I would treat a move to BLAKE3 or at least a 128-bit+ content address as foundational work, not polish.

### 5. `elm-build` is content-addressed, but not hermetic

This matters for how far you can push it.

Right now action identity does not capture:

- toolchain version
- environment variables
- system libraries
- undeclared filesystem reads

So it is a good build cache, but not yet a deterministic build system in the Nix sense.

## Assessment: Pure FP Immutable Interpreter

Yes, I think a cleaner pure-FP immutable interpreter is absolutely achievable here.

But it needs one runtime to become canonical.

The target architecture should be:

- one executable representation: resolved IR
- immutable env/state objects
- stable semantic IDs for globals and facts
- explicit effect protocol for host interactions
- data-only persistence boundary

The old AST evaluator should become:

- a compatibility fallback
- a migration crutch
- or deleted

It should not remain an equal peer forever.

Right now the codebase is “mostly pure in API shape, but not yet architecturally unified”.

That is a good place to be. It means the transition is feasible. But the transition has to be completed deliberately.

## Assessment: Salsa-Style Caching

You already have most of the ingredients:

- semantic declaration hashing
- rule/fact contracts
- explicit runtime yield/memo plumbing
- persistent normalized module artifacts
- host-side fact extraction

What is missing is the actual query system.

You need explicit queries such as:

- `ParseFile`
- `BuildModuleSummary`
- `BuildFactSet(module, family)`
- `ResolveDecl`
- `EvalDecl`
- `RunModuleRule(rule, module)`
- `RunProjectRuleFamily(ruleFamily, affectedModules)`
- `CollectTestRunners(testModule)`
- `RunTestRunner(runnerId, suiteHash)`

And then you need:

- stable query IDs
- revision tracking
- dependency edges
- one invalidation model shared across runners

At that point, the current mix of caches can collapse into one coherent graph.

The key thing I would not do is add more ad hoc memo hooks first.

The next step is graph definition, not hook proliferation.

## Assessment: `elm-build` As Nix/Hazel/Shake-Like CAS

Today it is closest to Shake:

- explicit build graph
- incremental cache
- user-declared dependencies
- pragmatic external command integration

It is not yet close to Nix because:

- hashes are too small
- actions are not hermetic
- toolchain identity is not part of action identity
- undeclared reads are not prevented

It could become a stronger deterministic cache layer, but I would describe the current state as:

- “good local content-addressed build substrate”
- not yet “content-addressable deterministic build system”

To move toward the latter, I would prioritize:

1. wider content hashes everywhere
2. explicit action identity including toolchain/env inputs
3. clearer declared input/output boundaries
4. stable semantic IDs for interpreter artifacts
5. data-only persisted outputs

## What To Prioritize Next

### 1. Make `ProjectEnv` coherent

This is the top priority.

As long as `env` and `resolved` can diverge, the resolved-IR transition will remain fragile and difficult to reason about.

### 2. Decide whether resolved IR is the actual future

If yes:

- wire it for real
- make it measurable
- make its state coherent
- migrate more runtime work onto it

If no:

- stop carrying fake gates and stale migration branches
- keep optimizing the old evaluator

The worst option is the current middle state where the code advertises one thing and executes another.

### 3. Fix correctness boundaries before adding more cache aggression

Specifically:

- remove `String.left 100` from override cache keys
- widen hashes
- tighten persistence to data-only values
- stop relying on transient IDs for long-lived cache stories

### 4. Harvest the easy runner wins

Do these immediately after the correctness fixes:

- `baseUserEnv` fast path for `evalSimple`
- `baseUserEnv` fast path for `evalWithCoverage`
- one-pass mutation baseline collection
- actual coverage pruning

### 5. Fix the interpreter correctness gaps surfaced by the current nested suite

The current `elm-interpreter` failures are not cosmetic.

They are exactly the kinds of gaps that will keep surfacing if more infrastructure is built on top of the interpreter first:

- missing Task support
- coverage regression
- Bytes endianness bug
- mutual recursion still overflowing in one path

### 6. After that, define the shared query graph

That is when Salsa-style caching will stop being “several good pieces” and become “one actual system”.

## Recommended Sequencing

If I were prioritizing this over the next several iterations, I would do it in this order.

### Phase 1: Correctness hardening

- make `ProjectEnv` coherent
- remove unsound cache keys like `String.left 100`
- widen content hashes
- fix the currently failing interpreter tests

This phase is boring, but it is the prerequisite for trusting more aggressive reuse.

### Phase 2: Cheap runtime wins

- give `evalSimple` the `baseUserEnv` fast path
- give `evalWithCoverage` the `baseUserEnv` fast path
- switch mutation baseline collection to one-pass execution
- wire actual coverage-based mutant skipping

This should reduce repeated setup costs without needing a large rewrite.

### Phase 3: Canonical runtime decision

- either make resolved IR the real execution path
- or explicitly defer it and simplify around the old evaluator

Do not keep investing in a half-migrated middle state.

### Phase 4: Shared query graph

- define the core queries
- define stable semantic IDs
- move runner-local caches behind shared query boundaries
- let `elm-build` persist those shared query outputs

This is the stage where the architecture becomes meaningfully Salsa-like instead of just “cache-rich”.

## Suggested North Star

My recommended target architecture is:

- `elm-build` as the outer CAS/query executor
- resolved IR as the only canonical executable representation
- stable semantic IDs for interpreter and fact artifacts
- one shared incremental query graph across review/test/mutation tools
- data-only persistence boundary
- old evaluator retained only as temporary compatibility fallback

That is the path that best preserves what is already strong in this repo while removing the current split-brain state.

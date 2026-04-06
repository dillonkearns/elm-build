# Deep Review: Salsa-Style Incremental `elm-review` Runner

## Scope

This review covers the current PoC spread across:

- `src/ReviewRunner.elm`
- `src/InterpreterProject.elm`
- `src/SemanticHash.elm`
- `src/Cache.elm`
- `elm-interpreter/src/Eval/Module.elm`
- `elm-interpreter/src/Eval/Expression.elm`
- `elm-interpreter/src/ValueCodec.elm`
- `bench/results/review-baseline.json`

I also compared the design against the upstream `node-elm-review` implementation, especially:

- [`documentation/tooling-integration.md`](https://github.com/jfmengels/node-elm-review/blob/main/documentation/tooling-integration.md)
- [`lib/elm-files.js`](https://github.com/jfmengels/node-elm-review/blob/main/lib/elm-files.js)
- [`lib/result-cache.js`](https://github.com/jfmengels/node-elm-review/blob/main/lib/result-cache.js)
- [`lib/optimize-js.js`](https://github.com/jfmengels/node-elm-review/blob/main/lib/optimize-js.js)


## Executive Summary

The PoC already has the right thesis.

The strongest parts are:

- it treats purity as a cacheability primitive rather than a convenience
- it already separates module rules from project rules
- it already carries rule-specific dependency profiles
- it already has a host-side parsing path and declaration-level semantic hashes
- it already experiments with preserving elm-review internal caches through intercepts

The main issue is not that the direction is wrong.
The main issue is that the current cache architecture is still split across several overlapping layers with different invalidation models:

- file-level declaration cache
- rule/file caches
- project-rule text caches
- interpreter semantic keys
- elm-review internal marker caches

That fragmentation means you are paying real cost without yet getting the full benefit of a single coherent incremental graph.

My short verdict:

1. The current PoC is a very strong proof that the approach is viable.
2. The biggest remaining wins are architectural, not micro-optimizations.
3. The next step should be to unify everything around one explicit incremental model:
   source hash -> parsed file facts -> dependency facts -> rule contributions -> reported errors.
4. If the target is "faster than elm-review CLI for warm single-file edits", a live daemon or long-lived worker is very likely required eventually.


## Current State

The checked-in benchmark already tells a useful story in [`bench/results/review-baseline.json`](bench/results/review-baseline.json):

- `elm-build` PoC cold: `60113ms`
- `elm-build` warm no-change: `480ms`
- `elm-build` warm one-file with cached rules: `12142ms`
- `elm-review CLI` cold: `631ms`
- `elm-review CLI` warm: `421ms`

That means:

- the "no work to do" path is already close to the CLI
- the one-file-changed path is the real battlefield
- the current design already proves that cache preservation helps
- the remaining gap is not "can this ever work?", it is "how do we stop rebuilding too much structure for one-file edits?"

One more concrete measurement from this repo: running

```bash
elm make src/ReviewConfig.elm --output /dev/null
```

inside `bench/review` took about `141-145ms` three times in a row on this machine. That matters because `ReviewRunner.task` does this unconditionally before any cache hit path.


## What Is Already Strong

### 1. You are attacking the right layer

The best thing in this codebase is that it is not trying to out-optimize the CLI by shaving a few milliseconds off a monolithic rerun.

It is trying to change the unit of recomputation.

That is the correct lever.

### 2. The host-side analysis path is promising

`ReviewRunner` already computes:

- file aspect hashes
- declaration semantic hashes
- rule dependency profiles
- dependency-graph-based invalidation

That is exactly the kind of information you need for a Salsa-style engine.

### 3. `InterpreterProject` already has useful reuse boundaries

`src/InterpreterProject.elm` already does some important things well:

- caches package sources on disk
- builds a reusable package env
- keeps a `baseUserEnv`
- carries a semantic index
- supports file overrides
- supports injected values
- supports intercept/yield flows

This is a good substrate for an incremental runner.

### 4. You are already matching an upstream trick that matters

Upstream `node-elm-review` does not just "run Elm".
It rewrites compiled JS to:

- install fast cache markers
- install a mutating `map` fast path for review internals
- replace context hash creation
- patch in other runtime-level optimizations

See [`lib/optimize-js.js`](https://github.com/jfmengels/node-elm-review/blob/main/lib/optimize-js.js).

So the premise "we should exploit referential transparency and substitute faster implementations under the hood" is not only valid, it is already what the CLI does, just in a JS/runtime-specific way.


## Biggest Concerns

## 1. Cache correctness is not fully namespaced yet

This is the biggest structural concern.

`ReviewRunner.task` reads and writes `review-decl-cache.json` keyed only by the analyzed source files, not by the review application identity. A change in any of these can leave stale results looking valid:

- `review/src/ReviewConfig.elm`
- any review rule source or dependency version
- `reviewRunnerHelperSource`
- interpreter behavior
- rule dependency profile table
- skip/patch configuration

Evidence:

- `review-decl-cache.json` is loaded before `checkCache` in `src/ReviewRunner.elm:1080-1142`
- `ensureReviewDeps` recompiles the review app, but its result is not part of the decl cache key in `src/ReviewRunner.elm:2989-2994`
- project-rule caches and rule-value caches also do not carry a full review-app hash

Why this matters:

- the runner can return a `FullCacheHit` even though the rules changed
- this becomes more dangerous as caches get more aggressive

Recommendation:

- define a first-class `reviewAppHash`
- include at least:
  - review `elm.json`
  - all review source files
  - helper source
  - interpreter version/build hash
  - cache schema version
  - rule profile manifest version
- namespace every cache directory and every hit path under that hash

This should be done before making the caches more aggressive.


## 2. The elm-review internal cache bridge is not fully correct yet

The marker intercept layer is promising, but currently too lossy to be the long-term foundation.

### Rule ID is ignored

Upstream keys result-cache entries by `ruleName-ruleId`.

Your intercept layer currently keys by `ruleName` only:

- `initialCacheMarker` lookup ignores the second arg in `src/ReviewRunner.elm:2443-2457`
- `finalCacheMarker` save path also ignores the second arg in `src/ReviewRunner.elm:2459-2477`
- `loadRuleCaches` says it returns `ruleName-ruleId`, but the writer currently saves only `ruleName`

That creates collisions when:

- the same rule appears twice with different configs
- future rule composition introduces multiple instances of the same rule name

### Persisted values are not fully serializable

`elm-interpreter/src/ValueCodec.elm:3-8` explicitly says non-data values are encoded as sentinels.
Closures, decoders, and regex values are not preserved; they become placeholders in `elm-interpreter/src/ValueCodec.elm:83-91`.

That means cross-process persistence of opaque rule caches is not semantically safe in the general case.

### Context hashing is still a heuristic

The current intercept for `Review.Cache.ContextHash.createContextHashMarker` uses `Eval.Expression.deepHashValue`, which is a custom integer hash with special cases and sentinel handling for closures.

Upstream uses a JSON/replacer-based representation in the optimized JS layer.
If your context hash diverges semantically from upstream, you can get false cache hits or misses inside rule caches.

### The project-cache path is not really wired yet

`ReviewRunnerHelper` defines `projectCacheMarker`, and there is an intercept for it in `src/ReviewRunner.elm:2491-2503`, but the helper path that returns `( errorStr, updatedRules )` never actually routes the project value through that marker.

So there is project-cache scaffolding in the code, but not yet a real persisted project reuse path.

Recommendation:

- switch to true `ruleName + ruleId` keying immediately
- treat persisted rule cache values as "data-only caches"
- do not rely on opaque persisted `Types.Value` for anything that can contain closures
- either:
  - extract/cache the exact data-only marker payloads, or
  - keep opaque rule cache values only in-memory inside a daemon


## 3. Full-cache hits still pay fixed setup costs

Right now the no-change path is not "return fast if cached".
It is "do some real work, then maybe return fast".

The clearest example is `ensureReviewDeps`:

- it runs before any cache decision in `src/ReviewRunner.elm:1086`
- on this machine it cost about `141-145ms` repeatedly

That is material when you want sub-second warm runs.

There is also repeated review-project setup:

- `getRuleInfo` loads the review project and evaluates rule names in `src/ReviewRunner.elm:2793-2818`
- cold/partial paths then load the review project again for actual evaluation

Recommendation:

- move review-app validation behind a `reviewAppHash` cache
- cache rule manifest data separately from source result caches
- ensure the `FullCacheHit` path does not rebuild anything unrelated to the changed target files


## 4. You are parsing and re-encoding too much on the host side

There is a lot of repeated host work in `ReviewRunner`.

The same source can be:

- parsed for declaration hashes
- parsed for aspect hashes
- parsed again to encode AST JSON
- decoded again inside the interpreter helper

`ReviewRunner` currently calls the relevant host analysis helpers many times:

- `computeAspectHashesFromSource` appears repeatedly
- `getDeclarationHashes` reparses whole files
- `encodeFileAsJson` reparses whole files

There is even an unused binary AST transport ready to go:

- `encodeFileAsWire` in `src/ReviewRunner.elm:825-845`
- `buildExpressionWithWire` in `src/ReviewRunner.elm:922-948`
- `src/AstWireCodec.elm`

But the hot path still uses JSON AST strings.

Recommendation:

- build a single per-file analysis record keyed by source hash:
  - parsed AST or binary AST
  - module name
  - imports
  - aspect hashes
  - declaration ranges
  - declaration semantic hashes
- persist that once
- never recompute those facts separately in different cache layers

This is one of the biggest disk-cache wins available.


## 5. The current granularity is still mostly file-level

This is the biggest gap between the thesis and the current implementation.

The code already stores declaration-level hashes and declaration-mapped errors, but invalidation still mostly happens at the file level:

- `checkCache` marks an entire file stale if any declaration hash or full-file aspect changed
- module rules are then rerun for the whole stale file

That means a change to one function body can still force rerunning all module rules for the whole module, even if most rule outputs are provably unaffected.

The current per-declaration cache is helpful, but it is still primarily an all-or-nothing file hit optimization.
It is not yet a true within-file incremental engine.

Recommendation:

- make declaration-level reuse the main abstraction, not just a stored artifact
- model module-rule outputs as:
  - per-declaration outputs
  - per-module bucket outputs
- invalidate only the declarations or module buckets touched by the rule's dependency profile

This is where the "unfair advantage" starts to become real.


## 6. Module-rule execution is still too fragmented

In `loadAndEvalHybridPartial`, stale module rules are executed per `(rule, file)`:

- one `Cache.compute`
- one `InterpreterProject.prepareAndEval`
- one `Review.Project` construction

See `src/ReviewRunner.elm:1398-1438`.

That is easy to cache, but expensive when a single file changes and several module rules miss.

Recommendation:

- batch module rules by file first, not by rule first
- ideally run all module rules for one stale file in one eval
- then split the output by rule afterward if you still want per-rule cache artifacts

Expected effect:

- much less repeated helper/module/project construction
- much better one-file warm-edit behavior

This is likely a much larger win than any interpreter micro-optimization at this stage.


## 7. `SemanticHash` does not yet resolve imported unqualified references

`SemanticHash.buildMultiModuleIndex` currently handles:

- same-module unqualified references
- explicitly qualified cross-module references

But unqualified imported exposed functions are not actually resolved through imports.
The code effectively assumes `[] -> current module` in `src/SemanticHash.elm:693-710`.

That means declaration-level semantic invalidation is currently incomplete for common Elm style:

```elm
import Foo exposing (bar)

baz =
    bar 1
```

If you want declaration-level incrementalism to be trustworthy, import resolution has to be part of the semantic index.

Recommendation:

- extend semantic indexing to use real import tables
- resolve:
  - exposed imports
  - aliases
  - constructors
  - same-module shadowing

This is important both for correctness and for unlocking finer-grained cache keys.


## 8. Review-project coverage is still limited by namespace/kernel gaps

`loadReviewProject` has to skip several packages because of kernel support and flat namespace collisions. See `src/ReviewRunner.elm:2997-3015`.

That means:

- the current runner is not yet benchmarking against the full real rule universe
- some of the best future wins may be hidden behind interpreter completeness work

This is not just a compatibility issue.
It also affects the performance story because any benchmark on a trimmed rule set risks being optimistic.

Recommendation:

- treat package/module isolation as an enabling architecture task
- if interpreter completeness is expensive, consider a rule-ABI layer that extracts just the pieces the runner needs instead of interpreting every dependency exactly as ordinary Elm code


## 9. Rule classification and dependency profiles are still too manual

This is an opportunity more than a bug, but it affects both correctness and performance ceilings.

The runtime path in `getRuleInfo` still classifies by hardcoded rule-name whitelist in `src/ReviewRunner.elm:2790-2851`, even though `classifyRuleSource` exists as a source-based idea elsewhere in the file.

That means:

- new module rules default to project rules
- you leave performance on the table until the table is updated manually
- profile drift becomes more likely as rule coverage expands

Recommendation:

- generate a rule manifest per `reviewAppHash`
- record for each rule:
  - `ruleId`
  - module/project classification
  - dependency profile
  - any known unsupported features
- make that manifest cached data, not hand-maintained runtime logic


## Where the Huge Wins Are

## 1. Review application cache as a first-class artifact

Right now the review app identity is implicit.
It should become explicit.

Cache:

- rule manifest
- rule classification/profile manifest
- review dependency package set
- helper module hash
- any interpreter-specific patching decisions

Key:

- `reviewAppHash`

Effect:

- removes setup work from warm hits
- fixes correctness holes
- makes every downstream cache composable


## 2. Per-file analysis database

This is the clearest disk-cache win.

Cache one record per source hash:

- raw source hash
- module name
- import list
- exposing info
- custom type info
- declaration ranges
- declaration semantic hashes
- binary AST blob

This one cache can feed:

- cache hit checking
- dependency graph updates
- module-rule invalidation
- project-rule invalidation
- host-to-interpreter AST transport

This should replace the current pattern of recomputing file facts in several places.


## 3. Batch module rules by stale file

For a single-file edit, this is probably the most immediate warm-run improvement.

Target shape:

- one stale file enters
- one parsed/module-analysis record reused
- one helper eval runs all module rules for that file
- results are split into per-rule/per-declaration caches

This directly attacks the "changed one file, paid N times" problem.


## 4. Declaration-level module-rule caches

For rules like:

- `NoMissingTypeAnnotation`
- `NoUnused.Patterns`
- `NoDebug.Log`

there is an obvious next level beyond file caches:

- cache outputs per declaration
- keep a separate `__module__` bucket for module-level concerns

Then one function-body change can avoid rerunning unaffected declaration checks in the same file.

This is one of the clearest places where purity gives you a genuine advantage over the current CLI architecture.


## 5. Project-rule contribution caches

This is the hardest and most valuable long-term step.

Instead of caching final project-rule outputs only, cache each module's contribution to the rule's global state.

Examples:

- export usage contribution
- variable/reference tables
- constructor usage tables
- context-hash keyed lookup tables

Then a one-file edit invalidates:

- the changed module's contribution
- maybe a small reverse-dependency frontier
- not the entire project-rule pipeline

This is the real Salsa move.

The current rule dependency profiles are already a decent starting language for deciding which frontier to invalidate.


## 6. Persisted package env or parsed package ASTs

`InterpreterProject.loadWith` still rebuilds a lot of review-project structure every process:

- package source loading
- package parsing
- package env building
- base user env building

See `src/InterpreterProject.elm:96-310`.

The biggest cold-start opportunity in the interpreter layer is to stop rebuilding immutable review-project/package structure on every invocation.

Possible approaches:

- persist parsed package ASTs
- persist interfaces separately from full ASTs
- persist a serialized package env representation if you can make kernel references re-hydratable
- ship a prebaked env for common `elm-review` dependencies


## 7. Daemon mode for the last big jump

I do not think disk caches alone are likely to get you to "clearly faster than CLI" for warm one-file edits on real projects.

The reason is simple:

- some high-value state is process-local
- some of it is not fully serializable
- building/re-hydrating large envs and rule caches still costs time

A daemon can keep in memory:

- review app identity
- package env
- target project analysis DB
- parsed ASTs
- `updatedRules`
- any opaque rule cache values that should not be serialized

If the goal is sub-second single-file edits, this is the most likely endgame.

I would treat daemon mode as a phase, not as the first step.
But I would design the intermediate caches so they can later back a daemon cleanly.


## Recommended Architecture

I would move toward five explicit cache layers.

### Layer 0: Review App Identity

Key:

- `reviewAppHash`

Contents:

- rule names
- rule ids
- module/project classification
- dependency profiles
- review helper hash
- review app compile validity

### Layer 1: Source Analysis Cache

Key:

- `sourceHash`

Contents:

- AST bytes
- file aspects
- declaration hashes
- imports
- module name
- declaration ranges

### Layer 2: Dependency Facts

Key:

- project snapshot of file hashes

Contents:

- import graph
- reverse import graph
- declaration dependency graph
- rule-specific invalidation frontiers

### Layer 3: Rule Contributions

Keys:

- module rule: `(reviewAppHash, ruleId, fileHash, declaration-or-module-bucket)`
- project rule contribution: `(reviewAppHash, ruleId, fileHash or declaration frontier hash)`

Contents:

- per-declaration diagnostics
- per-module diagnostics
- project-rule contribution data structures

### Layer 4: Session Memory

In a long-lived worker:

- package env
- review project env
- opaque rule caches
- in-memory ASTs
- hot context-hash tables


## Prioritized Plan

## Phase 1: Fix correctness boundaries first

Do this before making caches more aggressive.

1. Introduce `reviewAppHash` and namespace all caches under it.
2. Switch rule cache keys to `ruleName + ruleId`.
3. Stop treating opaque persisted `Types.Value` as universally safe.
4. Add validation tests for:
   - changing `ReviewConfig`
   - changing helper source
   - changing rule package version
   - same rule name appearing twice

Expected result:

- safer cache hits
- fewer hidden invalidation bugs


## Phase 2: Remove warm-hit fixed costs

1. Move `ensureReviewDeps` behind the review-app cache.
2. Cache rule manifest/classification separately.
3. Reuse one loaded review project per invocation instead of reloading in `getRuleInfo` and again in evaluation.

Expected result:

- materially faster "no source change" path
- better foundation for benchmarking the real remaining costs


## Phase 3: Build the per-file analysis DB

1. Centralize file parsing and fact extraction.
2. Persist AST bytes, aspect hashes, decl hashes, imports, module names.
3. Use this DB everywhere:
   - cache checks
   - graph building
   - AST transport
   - semantic invalidation

Expected result:

- much less host-side duplicate work
- better cold-start and post-restart performance


## Phase 4: Batch module rules by file

1. Run all module rules for one stale file together.
2. Split/cache outputs afterward.
3. Reuse the single parsed file analysis record.

Expected result:

- the first major win on warm single-file edits


## Phase 5: Promote declaration-level incrementalism

1. Turn per-declaration cache from storage into execution strategy.
2. Add module-level buckets explicitly.
3. Extend semantic dependency resolution through real imports.

Expected result:

- genuine within-file incremental reuse
- much better body-change behavior


## Phase 6: Project-rule contribution engine

1. Define contribution shapes per project-rule family.
2. Cache module contributions instead of just final error text.
3. Recompute only the changed frontier plus aggregation.

Expected result:

- this is the point where one-file edits can become dramatically cheaper than the CLI


## Phase 7: Optional but likely decisive daemon

1. Keep review env, parsed ASTs, and opaque rule caches alive in memory.
2. Use the disk caches as restart support, not the primary hot path.

Expected result:

- best shot at truly unfair warm-edit performance


## Concrete Things I Would Change Soon

If I were driving the next iteration, I would do these in roughly this order:

1. Add `reviewAppHash` and re-key all caches.
2. Fix marker cache keys to use `ruleId`.
3. Stop serializing opaque rule caches across processes unless proven data-only.
4. Build a per-file analysis cache and make `ReviewRunner` consume that instead of reparsing.
5. Batch module rules by file.
6. Replace JSON AST transport with the existing Wire/binary path or another compact representation.
7. Add real import resolution to `SemanticHash`.
8. Design a contribution cache for one high-value project-rule family, likely `NoUnused.*`.


## Final Assessment

The PoC is real.

It already demonstrates the central idea:

- warm no-change runs can be excellent
- elm-review internal caches can be preserved
- host-side purity-aware caching works

But the codebase is at the stage where the next 10x will not come from tuning the current shape.
It will come from choosing a stricter cache architecture and committing to more precise incremental units.

The biggest opportunity is not "make the current rerun cheaper".
It is:

- make file facts first-class
- make declaration invalidation real
- make project rules incremental by contribution
- keep the best non-serializable state alive in memory when possible

That is the path that could actually produce an unfair advantage over the current CLI on warm single-file edits.


## References

Local code:

- `src/ReviewRunner.elm`
- `src/InterpreterProject.elm`
- `src/SemanticHash.elm`
- `elm-interpreter/src/Eval/Module.elm`
- `elm-interpreter/src/Eval/Expression.elm`
- `elm-interpreter/src/ValueCodec.elm`
- `bench/results/review-baseline.json`

Upstream `node-elm-review`:

- [`documentation/tooling-integration.md`](https://github.com/jfmengels/node-elm-review/blob/main/documentation/tooling-integration.md)
- [`lib/elm-files.js`](https://github.com/jfmengels/node-elm-review/blob/main/lib/elm-files.js)
- [`lib/result-cache.js`](https://github.com/jfmengels/node-elm-review/blob/main/lib/result-cache.js)
- [`lib/optimize-js.js`](https://github.com/jfmengels/node-elm-review/blob/main/lib/optimize-js.js)

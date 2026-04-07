# Review Runner Progress

## 2026-04-07

### Implemented

- Reshaped the host-side file analysis cache into explicit query-style pieces:
  - `ParsedAst`
  - `ModuleSummary`
  - `BodySummary`
  - `InputRevision`
  - `Durability`
- Added structured perf tracing to `ReviewRunner` via `--perf-trace-json`.
- Recorded top-level stage timings for:
  - `prepare_config`
  - declaration cache load
  - target-file resolution
  - file reads
  - host parse/analysis
  - review project load
  - rule info load
  - module-rule eval
  - project-rule eval
  - declaration cache persist
- Recorded counters for:
  - file-analysis cache hits/misses/bytes
  - declaration-cache bytes
  - target file count
  - stale file count
  - source and AST JSON byte totals
  - project-rule family cache hits/misses and affected-module counts
  - memo stats when memoized project-rule evaluation is enabled
- Added a stable benchmark harness at `bench/review-runner-benchmark.mjs`.
  - It bundles `src/ReviewRunner.elm` once with `bundle-script`
  - runs a fixed scenario matrix
  - writes machine-readable results to `bench/results/review-runner-scenarios.json`
- The benchmark harness now keeps one stable build directory per fixture and mutates/restores files in place for warm-change scenarios.
  - I explicitly tried cloning the warmed build directory to a new temp path first
  - that invalidated too much state and produced near-cold timings
  - so the path-stable in-place mutation flow is now the canonical benchmark shape

### Benchmarks

Stable benchmark harness, `small-12` fixture, `bench/review` config:

- `cold`: `89.09s`
- `warm`: `0.33s`
- `warm_1_file_body_edit`: `1.53s`
- `warm_1_file_comment_only`: `0.50s`
- `warm_import_graph_change`: `29.54s`

Selected internal stage timings from the same run:

- `cold`
  - `module_rule_eval`: `34.90s`
  - `project_rule_eval`: `56.02s`
- `warm_1_file_body_edit`
  - `module_rule_eval`: `0.35s`
  - `project_rule_eval`: `0.31s`
- `warm_import_graph_change`
  - `module_rule_eval`: `0.28s`
  - `project_rule_eval`: `29.09s`

Selected counters from the same run:

- `warm_1_file_body_edit`
  - `analysis_cache.hits`: `11`
  - `analysis_cache.misses`: `1`
  - `project.importers.cache_hits`: `11`
  - `project.importers.cache_misses`: `1`
  - `project.deps.cache_hits`: `11`
  - `project.deps.cache_misses`: `1`
- `warm_import_graph_change`
  - `analysis_cache.hits`: `11`
  - `analysis_cache.misses`: `1`
  - `project.importers.cache_hits`: `9`
  - `project.importers.cache_misses`: `3`
  - `project.importers.affected_modules`: `4`
  - `project.deps.cache_hits`: `11`
  - `project.deps.cache_misses`: `1`
  - `project.deps.affected_modules`: `3`

### Takeaways

- The new benchmark harness is a better signal than the earlier ad hoc measurements because it distinguishes:
  - semantic body edits
  - comment-only edits
  - import-graph edits
- Comment-only edits are already in the right shape:
  - one file misses the host analysis cache by source hash
  - but the declaration cache still yields a `full_hit`
  - end-to-end time stays around `0.50s`
- Body edits are also in a much better place than the older `3s+` band on this fixture:
  - the new harness measured `1.53s`
  - both module-rule and project-rule phases were sub-second
- Import-graph edits are now the clearest remaining hot path:
  - the same harness measured `29.54s`
  - almost all of that time was in `project_rule_eval`
  - the trace shows that a single import change fans out into several `ImportersOf` misses and affected modules
- That makes the next optimization target much clearer:
  - contribution caches and/or backdated summaries for import-sensitive project-rule families
  - not more generic helper-function memoization
- The failed cloned-build benchmark path was still useful:
  - it showed that some warm cache value is path-stable only when the build directory itself stays fixed
  - so future cross-invocation cache design should assume the stable cache root is part of the performance model
- The perf trace plus stable benchmark matrix are now in place, so future caching experiments can be evaluated against the same scenario set instead of one-off stopwatch numbers
- The next big question is whether we can turn import-graph changes into the same “small bounded recompute” shape that body edits now have


## 2026-04-06

### Implemented

- Scoped the review runner build/cache directory by a hash of the review app inputs and helper source.
- Added a `review-deps-ready` stamp so true warm runs skip the unconditional `elm make src/ReviewConfig.elm`.
- Reused the loaded review project between `getRuleInfo` and the partial-eval path.
- Batched module rules by file instead of evaluating one `(rule, file)` pair at a time.
- Fixed the zero-module-rule path so project-only configs do not try to read missing `smr-*` cache outputs.
- Reworked `ImportersOf` and `DependenciesOf` project-rule misses to re-evaluate only the affected subgraph instead of the full project.
- Reprofiled `NoUnused.Parameters` from `FullProject` to `ImportersOf`.
- Added a persisted per-file host analysis cache keyed by file-content hash.
- Reused cached AST JSON, aspect hashes, declaration hashes, module names, and imports for unchanged files on subsequent runs.
- Fixed the rule-value cache bridge to key by `ruleName-ruleId` instead of only `ruleName`.
- Added a manifest-validated serialized project cache path for warm `FullProject` rule evaluation.
- Added per-function memo profiling in `elm-interpreter` and runner support for trying memo candidate sets via `--memoize-functions ... --memo-profile`.
- Tested broader cached-project reuse for `ImportersOf`/`DependenciesOf`, then reverted it after it erased the subgraph-narrowing win.
- Tested warm rule-cache reuse for narrowed `ImportersOf`/`DependenciesOf` slices, then reverted it after cache I/O outweighed any skip benefit.
- Batched `ImportersOf` + `DependenciesOf` slice evaluations into a single interpreter call when both miss, to reduce fixed per-eval overhead on warm partial runs.

### Benchmarks

Fixture: 12 non-test modules copied from this repo, `bench/review` rules, one semantic change by appending `benchmarkTouched__ = 42` to `MathLib.elm`.

After the first round of changes:

- `cold`: `59528.2ms`
- `warm`: `618.8ms`
- `warm_1_file_changed`: `37241.2ms`

Project/module split on that same fixture:

- Module-only rules: `24644.0ms cold`, `551.0ms warm`, `3223.1ms warm_1_file_changed`
- Project-only rules: `47055.3ms cold`, `535.0ms warm`, `34679.7ms warm_1_file_changed`

Project-rule subgroup split before the second round:

- `ImportersOf`: `17120.3ms warm_1_file_changed`
- `DependenciesOf`: `16840.1ms warm_1_file_changed`
- `FullProject` (`NoUnused.Parameters` only): `22861.1ms warm_1_file_changed`

After the second round of changes:

- `cold`: `72243.2ms`
- `warm`: `588.8ms`
- `warm_1_file_changed`: `3742.5ms`

Project-only on the same fixture after the second round:

- `cold`: `35885.0ms`
- `warm`: `556.9ms`
- `warm_1_file_changed`: `3295.4ms`

After the per-file host analysis cache round:

- Fixture: 12 copied non-test modules from `src`, `bench/review` rules, one semantic change by appending `benchmarkTouchedHostCache__ = 42` to `MathLib.elm`.
- `cold`: `74663.0ms` (down from `98098.0ms` on the same fixture before this round)
- `warm`: `398.0ms` (down from `548.0ms`)
- `warm_1_file_changed`: `3488.0ms` (down from `3617.0ms`)

After the project-cache experiment:

- Broad cached-project reuse for `ImportersOf` + `DependenciesOf` was a regression on the same fixture:
  - `cold`: `118820.0ms`
  - `warm`: `409.0ms`
  - `warm_1_file_changed`: `61910.0ms`
- Selective cached-project reuse only for `FullProject` rules restored the prior mixed-run shape:
  - `cold`: `74440.0ms`
  - `warm`: `400.0ms`
  - `warm_1_file_changed`: `3510.0ms`

After the narrowed warm-rule-cache experiment:

- `ImportersOf` warm_1_file_changed: `3180.0ms` (up from `2790.0ms`)
- `DependenciesOf` warm_1_file_changed: `3270.0ms` (up from `2750.0ms`)
- Mixed warm_1_file_changed: `3950.0ms` (up from `3510.0ms`)

Single-rule warm_1_file_changed timings on the same fixture:

- `NoUnused.Exports`: `3100.0ms`
- `NoUnused.CustomTypeConstructors`: `3030.0ms`
- `NoUnused.CustomTypeConstructorArgs`: `3020.0ms`
- `NoUnused.Parameters`: `2810.0ms`
- `NoUnused.Variables`: `2890.0ms`

After batching narrowed project-rule evaluations:

- Mixed full config on the same fixture:
  - `cold`: `71620.0ms`
  - `warm`: `410.0ms`
  - `warm_1_file_changed`: `3210.0ms`
- Project-only (`NoUnused.Exports`, `NoUnused.CustomTypeConstructors`, `NoUnused.CustomTypeConstructorArgs`, `NoUnused.Parameters`, `NoUnused.Variables`) on the same fixture:
  - `cold`: `45710.0ms`
  - `warm`: `370.0ms`
  - `warm_1_file_changed`: `3010.0ms`
- Group warm_1_file_changed after batching:
  - `ImportersOf`: `2780.0ms`
  - `DependenciesOf`: `2580.0ms`

After trying interpreter-local memoization in the runner:

- I wired the runner to use the new interpreter-local memo runtime for project-rule evaluation, then tried `ReviewRunnerHelper.decodeModule` as the first memo target.
- I explicitly removed the persisted memo-cache-on-disk experiment from this path so the measurement only reflected same-run in-memory reuse.
- On the same 12-file fixture:
  - `cold`: `90570.0ms`
  - `warm_1_file_changed`: `3600.0ms`
  - second `warm_1_file_changed` check on the same seeded build: `3890.0ms`
- I then backed out the `decodeModule` target and added a zero-cost bypass so the runner only enters the memoized eval path when there are actual memoized functions configured.
- Control run after backing out the target:
  - `cold`: `96290.0ms`
  - `warm_1_file_changed`: `3600.0ms`
- I also tried memoizing the heavily-used `Review.ModuleNameLookupTable.*` queries (`moduleNameAt`, `moduleNameFor`, `fullModuleNameAt`, `fullModuleNameFor`) to target cross-rule repeated AST lookups.
- That target was a strong regression on the same fixture:
  - `cold`: `279490.0ms`
  - `warm_1_file_changed`: `3760.0ms`
- I then changed the interpreter memo runtime so selected functions can use a projected memo key instead of hashing full structural arguments.
  For the lookup-table functions, the projected key uses current module name + range rather than the full lookup table and full `Node` value.
- That fixed the pathological cold regression and brought the run back to baseline shape:
  - `cold`: `96310.0ms`
  - `warm_1_file_changed`: `3740.0ms`
- Even with the better key, the lookup-table target still did not beat the non-memoized runner on the warm 1-file benchmark.
- Profiling the projected-key lookup-table target on the same fixture showed why this is interesting:
  - `cold`: `96570.0ms`
  - `warm_1_file_changed`: `4030.0ms`
  - `Review.ModuleNameLookupTable.moduleNameAt`: `17441` lookups, `12183` hits, `69.9%` hit rate on the cold partial-project pass
  - `Review.ModuleNameLookupTable.moduleNameAt`: `102` lookups, `70` hits, `68.6%` hit rate on the warm 1-file partial-project pass
  - `Review.ModuleNameLookupTable.fullModuleNameFor` and `fullModuleNameAt` had near-zero hit rates
- I then replaced the generic memo `EvYield` payload path with direct internal memo effects in the interpreter (`EvMemoLookup` / `EvMemoStore`), reran the same fixture, and compared it against a fresh non-memoized control:
  - fresh control: `3630.0ms` warm_1_file_changed
  - profiled lookup-table set (`moduleNameAt`, `moduleNameFor`, `fullModuleNameAt`, `fullModuleNameFor`): `3780.0ms` warm_1_file_changed
  - the same four-function target was previously `4030.0ms`, so the direct effect path materially reduced memo overhead even though it still did not beat control
- Narrowing the target set helped a lot:
  - `Review.ModuleNameLookupTable.moduleNameAt` only, profiled: `3650.0ms` warm_1_file_changed
  - this is close to break-even against the `3630.0ms` control and much better than the earlier four-function target
  - the profile confirmed the same `68.6%` warm hit rate for `moduleNameAt`
- I also disabled per-function memo stats on the non-profiled runner path so normal memoized evaluations no longer pay profiling `Dict.update` overhead by default.
  - on one fresh `moduleNameAt`-only non-profiled rerun, warm_1_file_changed was still about `3710.0ms`
  - that suggests the stats bookkeeping was not the dominant remaining cost on this seam, or the difference is within run-to-run noise at this scale
- I then replaced the interpreter’s generic `Set.member` + qualified-name branching with a prebuilt memo spec registry so each memoized call does one registry lookup and jumps straight to its keying strategy.
  - on a fresh `moduleNameAt`-only non-profiled rerun, warm_1_file_changed was `3780.0ms`
  - a second warm rerun on the same seeded build was `3870.0ms`
  - the matching bundled microbenchmark (`same-arg-32`) was also basically flat: runtime cold `215.8ms`, runtime warm `0.4ms`
  - so the registry is a good architectural cleanup, but it did not produce a measurable speedup on this seam by itself
- I then changed the memo cache namespace from string-keyed qualified names to integer spec ids derived from the memo registry.
  - on a fresh `moduleNameAt`-only non-profiled rerun, warm_1_file_changed was `3650.0ms`
  - that is better than the recent `3780.0ms` registry-only run and back in the same band as the best earlier `moduleNameAt` result
  - the structural microbenchmark remained effectively flat, which makes sense because it only exercises a single memoized function and does not stress the outer namespace key
- Finally, I added a specialized compact cache shape for projected-key specs (`moduleNameAt`, `moduleNameFor`, `fullModuleNameAt`, `fullModuleNameFor`) so they can use a direct compact-key lookup instead of the generic nested dict-plus-list path.
  - `moduleNameAt` only: first warm rerun `3640.0ms`, second warm rerun on the same seeded build `3770.0ms`
  - all four lookup-table functions together: `3760.0ms` warm_1_file_changed
  - so the specialized shape appears directionally helpful, but the win is still small enough that it is hard to separate from ordinary run-to-run variance on this fixture
- I then threaded `collectMemoStats` into the interpreter config so the normal memoized path can stay on `specId` only and stop carrying `qualifiedName` through memo payloads when profiling is off.
  - bundled microbenchmark (`same-arg-32`, `work-scale 4`, `iterations 5`): runtime cold `208.8ms`, down from the recent `217.4ms` band
  - fresh runner control on the same 12-file fixture: `3720.0ms` warm_1_file_changed
  - fresh `moduleNameAt`-only memoized run after the payload change: `3660.0ms` warm_1_file_changed
  - second warm rerun on the same seeded build: `3700.0ms`
  - this is the first small end-to-end win for that seam after the memo runtime cleanup work, though it is still much smaller than the earlier big wins from batching and narrowed project-rule evaluation

### Takeaways

- The unfair-advantage warm path is now much closer to the target shape: `37.2s -> 3.74s` on the 12-file fixture for a real 1-file semantic change.
- Warm no-change stays sub-second and is slightly better after removing the unconditional review-app prepare step from the hot path.
- The new host analysis cache mostly attacks parse/AST overhead, so its biggest win showed up on cold and full-hit warm paths. The 1-file warm path improved modestly because it is now dominated more by interpreter/rule execution than host parsing.
- Cached-project reuse is not the next big win for narrowed `ImportersOf` / `DependenciesOf` project rules. The existing affected-subgraph strategy is much better there.
- Warm rule-cache reuse is also not paying for narrowed slices in this architecture: the rule-cache serialization/deserialization cost is higher than the skip benefit here.
- The single-rule timings show that fixed interpreter/evaluation overhead is now a major remaining cost. Batching multiple narrowed project-rule slices into one interpreter call helps even without a new cache.
- The interpreter-local memo runtime is still promising, but the first runner seam (`decodeModule`) does not have enough real repeated work to pay for itself on the warm 1-file path.
- The apparently “hot” `Review.ModuleNameLookupTable.*` queries are also a poor generic memo target. Their bodies are too cheap relative to memo bookkeeping, and/or their argument patterns do not repeat in a way that amortizes the cache.
- The lookup-table experiment did reveal a real implementation flaw in the original memo runtime: generic structural keying was the wrong cost model for large shared context arguments like lookup tables and `Node` values.
- Projected memo keys are the right architectural escape hatch here. They make memoization viable for functions whose semantic identity is much smaller than their full structural arguments, even though this particular function family still was not a benchmark win.
- The new profiler shows that `moduleNameAt` really is reused often enough to be a plausible cache candidate. The reason it still does not help is likely constant-factor overhead in the current memo machinery, not lack of repeated calls.
- The direct internal memo-effect path improved the runner result for this seam from `4.03s` to roughly `3.65s - 3.78s` depending on the target set, so the architecture is working. It just is not quite cheap enough yet to make `moduleNameAt` a clean win.
- Narrow target sets matter. `moduleNameAt` is the only lookup-table function that looks plausibly worth memoizing; `fullModuleNameAt` and `fullModuleNameFor` are almost pure overhead here.
- The memo spec registry is still worth keeping: it centralizes memo strategy decisions and gives us a cleaner foundation for richer memo metadata. But its direct runtime benefit was too small to show up in the current benchmarks.
- Integer/spec-id keyed memo buckets look slightly better than string-keyed buckets on the real runner seam, even though the improvement is modest.
- The specialized compact cache shape for projected-key specs is the right direction architecturally, but on this benchmark it still did not create a decisive end-to-end win over control by itself.
- Removing `qualifiedName` from the normal memo payload path when profiling is off did help a bit: it moved the real `moduleNameAt` seam from roughly break-even/noise into a small positive result on a fresh control-vs-memo comparison.
- That said, the remaining win is still tiny compared to the total warm 1-file budget, so the next memo-runtime cuts should focus on payload/effect overhead and better memo targets rather than expecting lookup-table helpers alone to deliver a step-function improvement.
- Keeping the runner memo plumbing behind a zero-cost bypass is the right shape: we can now try better qualified-function targets without regressing the normal path when the set is empty.
- The real next performance step for 1-file warm runs is still rule-specific contribution caching, where a file change updates folded project-rule state instead of rerunning whole rule logic over any slice, or alternatively a daemon that keeps the interpreter state warm in memory.
- The next big opportunities are project-rule contribution caches, smaller AST transport than JSON, and eventually daemonized in-memory state.

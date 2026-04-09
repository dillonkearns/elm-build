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
- I tested one transport-bypass idea in this same phase as well:
  - project-rule module slices are now injected into the interpreter as `Types.Value` lists instead of always being embedded inline as huge expression literals
  - on its own, that did not produce a clear win on the `small-12` matrix
  - so expression transport is still worth keeping an eye on, but it was not the dominant wall in the bad import-graph case
- The much more important fix was changing `ImportersOf` invalidation from transitive reverse dependencies to direct reverse dependencies.
  - The previous logic treated any module that transitively depended on a file as an importer, which was too broad for rules like `NoUnused.Exports`.
  - On the `small-12` fixture, changing `MathLib` to import `ProjectSources` previously made `Path` miss and pulled `ReviewRunner.elm` into the `ImportersOf` reevaluation slice through transitive reverse deps.
  - With direct reverse deps instead, the import-change warm path improved from `29.54s` to `6.47s`.
  - `project_rule_eval` on that scenario dropped from about `28.33s` to `5.13s`.
  - `project.importers.cache_misses` dropped from `3` to `2`.
  - `project.importers.affected_modules` dropped from `4` to `2`.
- I also spot-checked correctness for this change:
  - the `warm_import_graph_change` scenario after the direct-importer change reported `361` errors
  - a separate fresh cold run on the same mutated fixture also reported `361` errors
  - so the narrower invalidation matched the cold result on that scenario


## 2026-04-08

### `DependenciesOf` Host-Fact Upper Bound

I extended the cached fact layer for `NoUnused.Variables` with:

- generic import summaries
- top-level declaration/use facts
- local type declarations
- let-binding summaries
- constructor-aware open-type usage from dependency `docs.json`

The isolated `DependenciesOf` family benchmark on `small-12` is now:

| Scenario | Interpreted baseline | Host-fact upper bound |
|---|---:|---:|
| Cold | 138.38s | 27.12s |
| Warm | 0.32s | 0.34s |
| Warm import-graph change | 5.95s | 1.35s |

Correctness on a fresh isolated workspace now matches the real `elm-review` CLI exactly:

| Metric | Count |
|---|---:|
| CLI findings | 22 |
| Host-fact findings | 22 |
| Missing | 0 |
| Extra | 0 |

The main bug there was not the fact model itself. It was the dependency docs decoder:

- I was decoding package union constructors from a non-existent `tags` field
- Elm package `docs.json` uses `cases`
- that left the dependency open-type constructor index effectively empty
- fixing that closed the remaining `Imported type ... is not used` mismatches for constructor-only usage

This puts `DependenciesOf` in the same category as the `ImportersOf` family:

- cached host-side facts are a strong seam
- the interpreted project-context fold is the real hot path
- the fact/policy split is still the desired end state, but the upper-bound experiment has now proven the size of the opportunity on both expensive `NoUnused` families

### Mixed `small-12` With All Five Host-Backed `NoUnused` Experiments

I then enabled all five host-backed `NoUnused` experiments in the mixed `bench/review` config:

- `NoUnused.Exports`
- `NoUnused.CustomTypeConstructors`
- `NoUnused.CustomTypeConstructorArgs`
- `NoUnused.Parameters`
- `NoUnused.Variables`

Current mixed `small-12` benchmark:

| Scenario | Runner | elm-review CLI |
|---|---:|---:|
| Cold | 111.19s | 2.95s |
| Warm | 0.37s | 1.05s |
| Warm 1-file body edit | 1.65s | 1.06s |
| Warm 1-file comment-only | 0.86s | 1.09s |
| Warm import-graph change | 1.65s | 1.14s |

Current mixed diff harness is exact in all five scenarios:

| Scenario | Result |
|---|---|
| Cold | match |
| Warm | match |
| Warm 1-file body edit | match |
| Warm 1-file comment-only | match |
| Warm import-graph change | match |

The most important shift is that the previous large project-rule wall is now effectively gone on this fixture:

| Scenario | `load_review_project` | `module_rule_eval` | `project_rule_eval` |
|---|---:|---:|---:|
| Warm 1-file body edit | 314ms | 242ms | 10ms |
| Warm import-graph change | 313ms | 245ms | 12ms |

That changes the optimization target again.

The remaining mixed gap versus the CLI is now mostly:

- fixed warm setup tax in `load_review_project`
- remaining module-rule execution
- outer wall-clock overhead outside the traced inner stages

So the broad thesis has held up:

- cached facts plus coarse bypasses are paying off
- expensive interpreted project-rule folds were the right seam to attack first
- the next round should focus on setup and module-rule costs rather than more project-rule cache work

### Benchmarks

After switching `ImportersOf` invalidation to direct reverse dependencies:

- `cold`: `97.20s`
- `warm`: `0.38s`
- `warm_1_file_body_edit`: `1.63s`
- `warm_1_file_comment_only`: `0.53s`
- `warm_import_graph_change`: `6.47s`

Most important stage delta on the same `small-12` fixture:

- `warm_import_graph_change`
  - before: `project_rule_eval 28.33s`
  - after: `project_rule_eval 5.13s`


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

### ImportersOf split-cache follow-up

- I revived the narrowed `ImportersOf` warm-rule-cache path, but this time made the strategy selectable with `--importers-cache-mode fresh|split|auto` so it could be compared on the exact same codebase and benchmark harness.
- I added counters for the split path so the trace now records:
  - `project.importers.rule_cache.load_ms`
  - `project.importers.warm_eval_ms`
  - `project.importers.rule_cache.entries`
  - `project.importers.rule_cache.loaded_bytes`
  - `project.importers.mode.fresh`
  - `project.importers.mode.split`
- That instrumentation exposed the real bug: the split-cache path was eagerly computing `fallbackOutput = runProjectRulesFresh ...` even on successful warm runs.
  - Because Elm evaluates `let` bindings eagerly enough here, the fallback fresh project-rule run was happening every time.
  - Fixing that one bug changed the split-cache experiment from a regression into a small real win.

Measured on `small-12` after the eager-fallback fix:

| Mode | Cold | Warm | Warm 1-file body edit | Warm import-graph change |
|---|---:|---:|---:|---:|
| `fresh` | `96.91s` | `0.32s` | `1.52s` | `5.90s` |
| `split` | `98.61s` | `0.33s` | `1.62s` | `5.61s` |
| `auto` | `94.13s` | `0.32s` | `1.50s` | `5.82s` |

What the new trace says on `warm_import_graph_change`:

- `fresh`
  - `project_rule_eval`: `4688ms`
  - `project.importers.cache_misses`: `2`
  - `project.importers.affected_modules`: `2`
- `split`
  - `project_rule_eval`: `4286ms`
  - `project.importers.rule_cache.load_ms`: `6ms`
  - `project.importers.warm_eval_ms`: `29ms`
  - `project.importers.rule_cache.entries`: `8`
  - `project.importers.rule_cache.loaded_bytes`: `54874`

Current read:

- The split-cache idea is viable once the accidental eager fallback is removed.
- It helps on import-graph changes where the narrowed importer slice is a bit wider, but it still loses on the trivial 1-file body-edit case.
- The `auto` heuristic is directionally right and keeps the body-edit path at the fresh baseline, but on a single run it landed between `fresh` and `split` on the import-graph case, which looks like ordinary run-to-run noise rather than a clear policy win yet.
- The next step should be to tighten the `auto` policy with a few repeated scenario-specific runs, then keep pushing toward smaller contribution boundaries inside the `ImportersOf` family rather than replaying whole project-rule cache machinery.

### Scenario Bench And DependenciesOf Split Cache

- I added a dedicated scenario benchmark runner in `bench/review-runner-scenario-benchmark.mjs` to isolate one warm case at a time.
- The first version accidentally destroyed warm state by copying the build to a different path; that benchmark paid off because it exposed another path-identity dependency in the runner.
- I then rewired the scenario runner to keep a fixed warmed workspace per mode and restore the source file between repeats instead of moving the build directory around.

Isolated `small-12` `warm_import_graph_change` measurements:

| Importers | Deps | Wall |
|---|---|---:|
| `fresh` | `auto` | `5.80s` |
| `split` | `auto` | `5.59s` |
| `split` | `fresh` | `5.57s` |
| `split` | `split` | `4.32s` |

- The `DependenciesOf` family turned out to be worth the same split-cache treatment as `ImportersOf`.
- I reused the same split cache plumbing for `DependenciesOf` and added `--deps-cache-mode fresh|split|auto`.
- On the isolated scenario, `split/split` was the first combination that clearly pushed the warm import-graph path into a new band (`4.32s`).

That still did **not** immediately show up in the full sequential matrix. The trace explained why:

- The isolated scenario bench seeded split caches up front.
- The full `auto` matrix often used the fresh narrowed path first, which meant there were no split-cache artifacts available when a later wider invalidation wanted to reuse them.
- In other words, `auto` was choosing the right reuse strategy later, but had nothing to reuse yet.

I fixed that by changing the fresh narrowed project-rule path to also run through the yield/intercept path with empty preloaded caches, so it writes split-cache artifacts even when it is not reusing any.

After that cache-seeding fix, the full `small-12` matrix with `auto/auto` became:

| Mode | Cold | Warm | Warm 1-file body edit | Warm import-graph change |
|---|---:|---:|---:|---:|
| `auto/auto` before cache seeding | `95.69s` | `0.32s` | `1.52s` | `5.89s` |
| `auto/auto` after cache seeding | `97.49s` | `0.33s` | `1.59s` | `4.44s` |

Current read:

- This is the first time the full sequential matrix inherited the broader split-cache win instead of only the isolated scenario benchmark.
- The import-graph case improved by about `1.45s` on the full harness (`5.89s -> 4.44s`).
- The 1-file body-edit case regressed slightly (`1.52s -> 1.59s`) because the fresh narrowed path now also seeds split-cache artifacts, but that overhead is much smaller than the import-graph win.
- The next question is whether that slight body-edit regression is acceptable as-is, or whether the cache-seeding step should be made more selective.

### Partial Project Bottleneck Isolation

I kept pushing on the partial project-rule path, but this round turned into a measurement win more than a direct perf win.

First, I tried shrinking the split-cache payload and tuning the `split`/`fresh` policy further:

| Variant | Warm 1-file body edit | Warm import-graph change |
|---|---:|---:|
| `auto/auto` stable control | `1.64s` | `4.51s` |
| `split/split` | `1.61s` | `4.48s` |

That was basically a wash, so I added better counters inside the partial project phase:

- `project.importers.eval_total_ms`
- `project.deps.eval_total_ms`
- `project.postprocess_ms`
- separate `project.deps.rule_cache.load_ms` / `project.deps.warm_eval_ms` counters instead of accidentally folding them into `project.importers.*`

On the restored `auto/auto` run, the important read for `warm_import_graph_change` is:

| Counter | Time |
|---|---:|
| `project_rule_eval` | `3245ms` |
| `project.importers.eval_total_ms` | `2174ms` |
| `project.importers.rule_cache.load_ms` | `6ms` |
| `project.importers.warm_eval_ms` | `33ms` |
| `project.deps.eval_total_ms` | `1050ms` |
| `project.deps.rule_cache.load_ms` | `6ms` |
| `project.deps.warm_eval_ms` | `6ms` |
| `project.postprocess_ms` | `1ms` |

That changes the diagnosis a lot:

- The remaining cost is **not** cache-file loading.
- It is **not** the interpreter's warm eval loop itself.
- It is **not** the host-side postprocess/parsing of rule output.
- The cost is concentrated inside the *setup* of the `ImportersOf` / `DependenciesOf` partial branches before the actual warm eval call.

The most likely culprit is the module payload transport/allocation path, especially constructing `moduleInputsValue modulesWithAst` with full `source` and `astJson` strings for every partial project eval.

I tested one direct bypass idea: pass only `path + source` into the interpreter and leave `astJson` blank so `ReviewRunnerHelper.buildProject` falls back to `Project.addModule`.

That regressed:

| Variant | Warm 1-file body edit | Warm import-graph change |
|---|---:|---:|
| Restored control | `1.64s` | `4.51s` |
| Source-only payload experiment | `1.60s` | `5.71s` |

So I reverted that experiment. The transport boundary is still the right place to attack, but the simple "drop AST JSON and reparse from source" version is not the answer.

Current read:

- The best current full-matrix result is still the split-cache-enabled path around `4.5s` for `warm_import_graph_change`.
- The next promising work is not more split-cache tuning. It is a better module payload transport that avoids rebuilding/copying large `Types.Value` trees for `{ path, source, astJson }` on every partial project-rule eval.
- A likely next direction is a lower-level payload/reference path inside the interpreter or helper layer, rather than reparsing from source or serializing giant AST strings repeatedly.

### Handle-Based Partial Module Payloads

I tried the more direct "shared references" version next:

- Keep the actual `{ path, source, astJson }` payloads in a host-side table.
- Inject only `List Int` handles into the partial project-rule helper.
- Add `ReviewRunnerHelper.moduleHandlePathMarker`, `moduleHandleSourceMarker`, and `moduleHandleAstJsonMarker`.
- Resolve those markers through interpreter intercepts instead of constructing a giant nested `Types.Value` list of module records up front.

That turned out to be the first transport-level change that actually helped.

`small-12`, full matrix, `auto/auto`:

| Scenario | Before handle path | After handle path |
|---|---:|---:|
| Cold | `100.54s` | `105.56s` |
| Warm | `0.33s` | `0.31s` |
| Warm 1-file body edit | `1.64s` | `1.55s` |
| Warm 1-file comment-only | `0.55s` | `0.53s` |
| Warm import-graph change | `4.51s` | `4.23s` |

The more detailed counters for `warm_import_graph_change` also moved in the right direction:

| Counter | Before | After |
|---|---:|---:|
| `project.importers.eval_total_ms` | `2174ms` | `2019ms` |
| `project.deps.eval_total_ms` | `1050ms` | `1004ms` |
| `project.importers.warm_eval_ms` | `33ms` | `26ms` |
| `project.deps.warm_eval_ms` | `6ms` | `7ms` |

So the benefit is small-but-real and lines up with the earlier diagnosis:

- The win is coming from reducing the partial-branch setup/transport cost.
- The cache load path was already cheap.
- Replacing the full injected module-record payload with lightweight handles is measurably better than rebuilding those values each time.

Current read:

- This is the clearest positive result yet for the "share references instead of reallocating big payload trees" direction.
- The next likely step is to push this same handle/reference idea deeper: avoid rebuilding handle->payload tables unnecessarily, and look for other partial-project seams where we still inject large immutable values eagerly.

I tried exactly that next: build one shared handle table for the whole partial run, then derive only small subset handle lists for the importers/deps branches.

That did **not** produce a clear second-step improvement over the simpler handle path. In the follow-up full-matrix run it landed back in roughly the earlier band rather than beating it, so I reverted that refinement and kept the simpler per-branch handle table.

Current read:

- The first handle/reference step was the real win.
- The extra shared-table layer did not obviously help, at least in the current implementation.
- The next promising direction is probably not "more handle table factoring", but a deeper bypass where the interpreter/helper can consume shared module payloads with fewer per-handle marker calls.

I also tried collapsing the three per-handle markers (`path`, `source`, `astJson`) into a single payload marker returning the full record.

That also did **not** win. In practice it landed worse than the simpler handle path, so I reverted it.

Current read:

- The good result is still the original handle/reference change itself.
- Neither of the follow-up refactors on top of that handle path clearly improved things:
  - shared handle table across the full partial run
  - single payload marker instead of 3 marker lookups
- That suggests the next meaningful gain probably needs a deeper interpreter/runtime seam, not another small rearrangement of the current helper-marker approach.

### Array-Backed Shared Module Payloads

I kept the same handle-based partial-project strategy, but changed the shared payload backing store from:

- `Dict Int { path, source, astJson }`

to:

- `Array String` for `paths`
- `Array String` for `sources`
- `Array String` for `astJsons`

That removes the `Dict.fromList` tree build and the per-handle record allocation from the hot setup path while keeping the same helper markers and interpreter behavior.

Full `small-12` matrix, `auto/auto`:

| Scenario | Before | After |
|---|---:|---:|
| Cold | `103.49s` | `104.53s` |
| Warm | `0.34s` | `0.33s` |
| Warm 1-file body edit | `1.70s` | `1.64s` |
| Warm 1-file comment-only | `0.56s` | `0.56s` |
| Warm import-graph change | `4.53s` | `4.48s` |

Isolated `small-12` `warm_import_graph_change`, `auto/auto`, repeated on one warmed workspace:

| Run | Wall |
|---|---:|
| 1 | `4.51s` |
| 2 | `3.15s` |
| 3 | `3.17s` |

Read:

- This looks like a small real improvement, not a breakthrough.
- It helps the two warm partial-miss cases a bit, especially the 1-file body edit path.
- The remaining large cost is still the setup around partial project-rule evaluation, not rule-cache I/O or the actual warm eval loop.
- The next thing worth measuring is an even leaner shared payload path:
  - either pre-wrapped `Types.Value` arrays so the intercepts stop allocating `Types.String`
  - or better timing around payload setup / intercept construction so we can confirm exactly where the remaining setup cost sits.

### Upstream BST Union Port

I also tested upstream `miniBill/elm-build` commit `a8a8d4041b4a55f0bb288c4fbc567e1ca4b60ede` (`Faster BST union`).

It did not cherry-pick cleanly because this repo has already deleted `src/BST.elm`; the equivalent BST implementation now lives inline inside `src/Cache.elm` as the backing store for `HashSet`.

So I ported the algorithm change there instead:

- `innerHashSetUnion` now unions by merging sorted BST contents and rebuilding a balanced tree
- `innerHashSetFromList` now goes through `innerHashSetFromSortedList`
- the inlined BST also picked up the upstream `unique` helper to dedupe adjacent sorted entries

Compared against the pushed baseline commit `0d6fe24`, the `small-12` full matrix improved across the board:

| Scenario | Before | After |
|---|---:|---:|
| Cold | `104.53s` | `102.13s` |
| Warm | `0.33s` | `0.32s` |
| Warm 1-file body edit | `1.64s` | `1.54s` |
| Warm 1-file comment-only | `0.56s` | `0.52s` |
| Warm import-graph change | `4.48s` | `4.30s` |

Internal timings moved the same direction:

| Scenario | Before internal | After internal |
|---|---:|---:|
| Cold | `104.00s` | `101.54s` |
| Warm 1-file body edit | `1.07s` | `1.00s` |
| Warm import-graph change | `3.92s` | `3.77s` |

Current read:

- This upstream BST change looks like a real keeper.
- The effect is broad rather than specialized: it helps cold, warm no-change, body-edit, and import-graph paths all at once.
- It is still a modest constant-factor win, not a new architecture-level breakthrough, but it is exactly the kind of low-risk improvement we should probably keep stacking.

### elm-review CLI Baseline

I also checked in a permanent apples-to-apples CLI harness at `bench/elm-review-cli-benchmark.mjs`.

On the same `small-12` fixture and scenario matrix, the stock `elm-review` CLI (`2.13.5`) measured:

| Scenario | elm-review CLI |
|---|---:|
| Cold | `2.65s` |
| Warm | `0.72s` |
| Warm 1-file body edit | `0.74s` |
| Warm 1-file comment-only | `0.85s` |
| Warm import-graph change | `0.76s` |

That makes the current competitive position much clearer:

| Scenario | This runner | elm-review CLI |
|---|---:|---:|
| Warm | `0.32s` | `0.72s` |
| Warm 1-file body edit | `1.54s` | `0.74s` |
| Warm import-graph change | `4.30s` | `0.76s` |

So the current state is:

- We already beat the CLI on full-hit warm runs.
- We are still behind on the most important partial-miss paths, especially import-graph changes.

The CLI's own `--benchmark-info` output is also a useful calibration point on this same fixture:

- warm no-change:
  - `parse/fetch parsed files`: `28.404ms`
  - `run-review`: `216.708ms`
  - `process-errors`: `112.906ms`
  - `review`: `330.421ms`
- warm body edit:
  - `parse/fetch parsed files`: `55.303ms`
  - `run-review`: `221.213ms`
  - `process-errors`: `111.091ms`
  - `review`: `333.413ms`
- warm import-graph change:
  - `parse/fetch parsed files`: `75.390ms`
  - `run-review`: `192.167ms`
  - `process-errors`: `128.582ms`
  - `review`: `321.524ms`

Against that, our own perf trace says the remaining big costs are not cache file I/O and not the final warm eval loop itself:

- warm body edit:
  - `load_review_project`: `239ms`
  - `get_rule_info`: `62ms`
  - `module_rule_eval`: `258ms`
  - `project_rule_eval`: `337ms`
- warm import-graph change:
  - `load_review_project`: `242ms`
  - `get_rule_info`: `61ms`
  - `module_rule_eval`: `268ms`
  - `project_rule_eval`: `3101ms`
  - within that, `project.importers.rule_cache.load_ms = 5ms`, `project.importers.warm_eval_ms = 26ms`, `project.importers.eval_total_ms = 2061ms`
  - and `project.deps.rule_cache.load_ms = 4ms`, `project.deps.warm_eval_ms = 6ms`, `project.deps.eval_total_ms = 1016ms`

Current read:

- Generic helper-function memoization is not the main bottleneck.
- Disk cache load/store is not the main bottleneck.
- The biggest remaining cost is setup/allocation/transport around partial project-rule execution, plus the fixed review-project/rule-info reload on partial misses.
- The best next architectural opportunities are:
  - disk-backed reuse of the loaded review app / interpreter project / rule info on partial misses
  - true direct contribution caching and folding for `ImportersOf` / `DependenciesOf`, so we stop re-entering full `Rule.review` setup for unchanged modules
  - continuing to replace large immutable payload reconstruction with shared handles or prebuilt values where the trace shows setup cost

### ruleInfo Cache And Review-App Load Breakdown

I added a disk-backed `ruleInfo` cache scoped under the hashed review-app build directory, plus internal `load_review_project.*` counters from `InterpreterProject.loadWithProfile`.

The direct, durable win is that `get_rule_info` on warm partial misses is now basically gone:

| Scenario | Before | After |
|---|---:|---:|
| Warm 1-file body edit | `62ms` | `1-2ms` |
| Warm import-graph change | `61ms` | `1-2ms` |

The first full-matrix run after that cache landed measured:

| Scenario | Before | After |
|---|---:|---:|
| Cold | `102.13s` | `100.55s` |
| Warm | `0.32s` | `0.30s` |
| Warm 1-file body edit | `1.54s` | `1.48s` |
| Warm 1-file comment-only | `0.52s` | `0.53s` |
| Warm import-graph change | `4.30s` | `4.22s` |

A repeated full-matrix run after adding the finer parse-vs-build counters was noisier (`1.54s` body edit, `4.44s` import-graph), so the main signal from this round should be taken from the internal timings rather than one wall-clock sample.

The important new diagnosis is inside `load_review_project` itself. On the current warm partial-miss runs:

- warm 1-file body edit:
  - `load_review_project`: `249ms`
  - `load_review_project.build_graph_ms`: `42ms`
  - `load_review_project.parse_package_sources_ms`: `165ms`
  - `load_review_project.build_package_env_from_parsed_ms`: `12ms`
  - `load_review_project.build_package_env_ms`: `177ms`
  - `load_review_project.cache_inputs_ms`: `6ms`
- warm import-graph change:
  - `load_review_project`: `267ms`
  - `load_review_project.build_graph_ms`: `42ms`
  - `load_review_project.parse_package_sources_ms`: `183ms`
  - `load_review_project.build_package_env_from_parsed_ms`: `13ms`
  - `load_review_project.build_package_env_ms`: `196ms`
  - `load_review_project.cache_inputs_ms`: `6ms`

Current read:

- The fixed partial-miss tax from `get_rule_info` is solved well enough.
- The fixed partial-miss tax from `load_review_project` is now clearly dominated by package-source parsing, not by source-directory resolution, file reads, or the env fold from parsed modules.
- That means the next best review-app-side cache is probably **not** a full serialized env snapshot as the first step.
- The cleaner next target is a disk-backed parsed-package-source cache (or equivalent pre-parsed review-app package snapshot), because that attacks the `~165-183ms` wall directly while leaving the cheap `buildProjectEnvFromParsed` step intact.

### Parsed Package Cache: Binary Experiment And Blocker

I tried the obvious next step: persist parsed review-app package sources and reload them instead of reparsing on every warm partial miss.

There were two iterations:

1. a text/blob transport based on decimal `intList` strings
2. a Lamdera-wire binary blob written through `custom-backend-task.js` and read back with `BackendTask.File.binaryFile`

The text form was clearly wrong for performance. Decode alone was slower than reparsing, and it never hit reliably.

The binary form gave a much better answer:

- `load_parsed_package_cache_ms` dropped to about `2ms`
- `decode_parsed_package_cache_ms` dropped to about `2ms`
- so the file I/O and binary transport were no longer the problem

But the crucial new counter was:

- `load_review_project.parsed_package_cache_roundtrip_ok = 0`

That counter validates the cache **in memory before writing it to disk**, and it still failed. So the blocker is not the filesystem path and not the binary transport. The blocker is that this `AstWireCodec` roundtrip does not currently work for the review-app package source set.

There was a second important finding too: encoding the full parsed package blob on the hot path is itself expensive enough to matter. Leaving that work enabled unconditionally pushed the runner well off baseline. I backed the hot path out again so the current runner stays fast.

Restored `small-12` matrix after taking the package-cache experiment back out of the normal runner path:

| Scenario | Time |
|---|---:|
| Cold | `106.94s` |
| Warm | `0.34s` |
| Warm 1-file body edit | `1.54s` |
| Warm 1-file comment-only | `0.59s` |
| Warm import-graph change | `4.40s` |

Current read:

- A parsed-package-source cache is still conceptually promising.
- The transport problem is basically solved by the binary approach.
- The real blocker is AST roundtrip correctness for review-app package sources.
- The next useful step is **not** more transport tuning. It is a focused validator to identify which package modules fail `AstWireCodec` roundtrip and why.
- If that turns out to be too broad a codec project, the next alternative is caching a later boundary than raw parsed files, such as a smaller package-env artifact that avoids reparsing without requiring full AST roundtrip fidelity.

### Package Module Summary Cache

I replaced the parsed-package-source blob with a smaller cache boundary: package module summaries.

Instead of persisting full parsed `File` ASTs for review-app package sources, the new cache stores the per-module pieces that `buildModuleEnv` actually needs:

- module name
- module interface
- resolved imported names
- function implementations needed in the shared env

The codec also strips source ranges from those cached function/interface payloads on disk, because package dependencies do not need real source locations to execute correctly in the interpreter.

This changed the review-app warm-load profile in the right direction:

| Metric | Parsed AST blob | Package summary blob |
|---|---:|---:|
| cache bytes | `3,327,828` | `2,479,004` |
| warm body-edit cache decode | `317ms` | `232ms` |
| warm import-change cache decode | `312ms` | `227ms` |
| warm body-edit `load_review_project` | `461ms` | `312ms` |
| warm import-change `load_review_project` | `448ms` | `307ms` |

Full `small-12` matrix after switching to package summaries:

| Scenario | Previous `bytesDecode` AST blob | Package summary cache |
|---|---:|---:|
| Cold | `117.97s` | `104.10s` |
| Warm | `0.34s` | `0.31s` |
| Warm 1-file body edit | `1.75s` | `1.50s` |
| Warm 1-file comment-only | `0.56s` | `0.53s` |
| Warm import-graph change | `4.82s` | `3.98s` |

This is also a real improvement over the earlier non-package-cache baseline:

| Scenario | Earlier stable baseline | Package summary cache |
|---|---:|---:|
| Warm 1-file body edit | `1.54s` | `1.50s` |
| Warm import-graph change | `4.30s` | `3.98s` |

Important internal timings from the new run:

- cold:
  - `parse_package_sources_ms`: `151ms`
  - `build_package_summaries_from_parsed_ms`: `6ms`
  - `build_package_env_from_summaries_ms`: `3ms`
- warm 1-file body edit:
  - `decode_package_summary_cache_ms`: `232ms`
  - `build_package_env_from_summaries_ms`: `4ms`
- warm import-graph change:
  - `decode_package_summary_cache_ms`: `227ms`
  - `build_package_env_from_summaries_ms`: `3ms`

Current read:

- This was the right cache boundary shift.
- We are no longer paying the old `buildProjectEnvFromParsed` fold on warm partial runs in any meaningful way.
- The remaining fixed review-app warm-load cost is now mostly decoding the package summary blob itself.
- That means the next review-app-side opportunities are:
  - making the package summary payload cheaper to decode
  - or splitting/reusing it in a way that avoids decoding one large blob every time
- But the largest end-to-end remaining wall is still `project_rule_eval`, especially on import-graph changes.

### ImportersOf Split-Group Experiment

I tried a more aggressive split inside the `ImportersOf` family:

- `NoUnused.Exports`
- `NoUnused.CustomTypeConstructorArgs`

into a fold-only subgroup, leaving:

- `NoUnused.CustomTypeConstructors`
- `NoUnused.Parameters`

in the existing importer-context subgroup.

The reasoning was that the fold-only subgroup does not use `withContextFromImportedModules`, so on a 1-file import change it should be able to:

- re-evaluate only the stale module visitors
- reuse cached contributions from unchanged modules
- avoid dragging unchanged direct importers through the more expensive contextual path

The first version was not a win. The big mistake was that the fold-only warm path still loaded cached module entries for **every unchanged file**, which made the extra subgroup cheaper in theory but too expensive in setup.

Tightening that path helped:

- only use split-cache mode for the fold-only subgroup when there is actually an unchanged miss to reuse
- only load cached module entries for the fold-only missed paths and their direct importers, not the whole unchanged project

Full `small-12` matrix with that tighter auto policy:

| Scenario | Result |
|---|---:|
| Cold | `130.46s` |
| Warm | `0.31s` |
| Warm 1-file body edit | `1.57s` |
| Warm 1-file comment-only | `0.54s` |
| Warm import-graph change | `4.12s` |

That is not a clean cross-commit win over the earlier `3.98s` import-change result, but the source fixture itself has also grown during this work, so I ran an apples-to-apples A/B on the **current** source tree:

`warm_import_graph_change`, current tree, `deps=auto`

| Importers mode | Avg | Run 1 | Run 2 |
|---|---:|---:|---:|
| `fresh` | `3.97s` | `4.06s` | `3.88s` |
| `auto` | `3.82s` | `4.47s` | `3.17s` |

`warm_1_file_body_edit`, current tree, `deps=auto`

| Importers mode | Avg | Run 1 | Run 2 |
|---|---:|---:|---:|
| `fresh` | `1.52s` | `1.64s` | `1.40s` |
| `auto` | `1.53s` | `1.61s` | `1.44s` |

Current read:

- The split-group idea is **not** a clear first-changed-run win.
- It is close to parity on common body edits.
- It looks meaningfully better on repeated warm import-graph changes once the narrower subgroup caches are seeded.
- The remaining gap is still setup/fixed-cost work, not cache file I/O.
- That suggests the next meaningful win is probably still a **more compact direct contribution artifact**, not more generic `Review.Rule` split-cache choreography.

### Differential Correctness Harness

I added a real runner-vs-CLI differential harness:

- [bench/review-diff-benchmark.mjs](/Users/dillonkearns/src/github.com/dillonkearns/elm-build2/bench/review-diff-benchmark.mjs)
- [bench/results/review-diff-scenarios.json](/Users/dillonkearns/src/github.com/dillonkearns/elm-build2/bench/results/review-diff-scenarios.json)

To make that possible, the runner now has a machine-readable mode:

- `--report=json`

which emits a stable JSON list of:

- `rule`
- `path`
- `line`
- `column`
- `message`

The first `small-12` differential run was very useful. It found **no runner-only errors**, but it did find runner under-reporting versus `elm-review`:

| Scenario | Runner | CLI | Missing from runner |
|---|---:|---:|---:|
| Cold | `430` | `431` | `1` |
| Warm | `430` | `431` | `1` |
| Warm 1-file body edit | `431` | `432` | `1` |
| Warm 1-file comment-only | `430` | `431` | `1` |
| Warm import-graph change | `430` | `435` | `5` |

The persistent missing finding in every scenario is:

- `NoUnused.Variables` in `src/SemanticHash.elm`
- missing import warning for `TypeAnnotation`

And on `warm_import_graph_change`, the runner also misses four CLI-reported `NoUnused.Exports` findings in `src/ProjectSources.elm`:

- `loadPackageDeps`
- `loadPackageDepsCached`
- `loadProjectSources`
- `resolvePackageVersions`

Current read:

- This harness is a big step forward because we now have a repeatable way to catch correctness regressions while optimizing.
- The current runner is **close**, but not yet fully equivalent to `elm-review` even on the benchmark fixture.
- The persistent `NoUnused.Variables` miss suggests there is still one baseline project-rule correctness gap, independent of warm caching.
- The extra `NoUnused.Exports` misses on import-graph change strongly suggest our narrowed `ImportersOf` invalidation/reuse is still too aggressive in at least one path.
- That means the next perf work should stay coupled to this harness; correctness should be treated as a gate, not a secondary check.

### Target Project Runtime Cache

I added a focused benchmark for just the fixed setup path that had become suspicious:

- [src/TargetProjectRuntimeBenchmark.elm](/Users/dillonkearns/src/github.com/dillonkearns/elm-build2/src/TargetProjectRuntimeBenchmark.elm)
- [src/ReviewRunner.elm](/Users/dillonkearns/src/github.com/dillonkearns/elm-build2/src/ReviewRunner.elm)

The benchmark exercises:

1. `prepareConfig`
2. `loadReviewProjectDetailed`
3. `loadTargetProjectSeed`
4. `buildTargetProjectRuntime`

with repeated iterations on the same build dir.

The important change was caching the fully built target-project `baseProject : Types.Value` with `ValueCodec`, instead of rebuilding it from raw target `elm.json` plus dependency `docs.json` on every partial miss.

Measured on the current repo as target project:

| Step | Cold / first iteration | Warm / repeated iteration |
|---|---:|---:|
| `prepare_config` | `136ms` | `14ms` |
| `load_review_project` | `1846ms` | `384ms` |
| `load_target_project_seed` | `7ms` | `8ms` |
| `build_target_project_runtime` | `20235ms` | `89ms` |

Repeated benchmark output:

| Iteration | `build_target_project_runtime` | Cache hit |
|---|---:|---:|
| 1 | `20384ms` | `false` |
| 2 | `78ms` | `true` |
| 3 | `84ms` | `true` |

Current read:

- This is the clearest isolated perf win in a while.
- The new cache removes an enormous fixed partial-miss setup tax.
- The previous hypothesis was correct: rebuilding the full target-project base project was pathological on this branch.
- Even on repeated iterations, `load_review_project` is still a meaningful fixed cost, but it is no longer the dominant wall inside this setup slice.

### Node CPU Profile: Cold vs Warm Runtime Build

I also profiled the same focused benchmark with Node `--cpu-prof`.

Cold benchmark result:

| Metric | Value |
|---|---:|
| `prepare_config` | `136ms` |
| `load_review_project` | `1846ms` |
| `build_target_project_runtime` | `20235ms` |

Warm benchmark result:

| Metric | Value |
|---|---:|
| `prepare_config` | `14ms` |
| `load_review_project` | `384ms` |
| `build_target_project_runtime` | `89ms` |

Profile summary:

- Cold runtime build is dominated by **garbage collection**.
- Warm runtime build still shows some GC, but at a dramatically lower level.
- That matches the architectural story: the uncached path was allocating/rebuilding a very large `Types.Value` graph, while the cached path mostly pays for loading and decoding it.

Top sampled cold frames included:

- `(garbage collector)`
- bundled helper frames in `target-project-runtime-benchmark.mjs`
- buffer allocation / string / module-loader frames

Top sampled warm frames included:

- much smaller `(garbage collector)` presence
- bundled helper frames
- modest buffer/string work

Current read:

- The Node profile reinforces the benchmark result rather than contradicting it.
- The big cold-path cost here is not “a mysterious CLI wrapper issue”; it is allocation-heavy rebuild work inside the JS/runtime execution of the uncached setup path.
- Caching the built `baseProject` is therefore the right direction, and the next thing to measure is how much of the end-to-end warm partial-miss regression this isolated win removes.

### Focused Warm Partial-Miss Probes With Explicit `--jobs`

The next debugging issue turned out to be environmental rather than architectural: in this sandbox, `Cache.run { jobs = Nothing }` could not resolve CPU count via `nproc` / `sysctl`. Falling back implicitly to one worker distorted the benchmark signal, so I added an explicit `--jobs` option to the runner and updated benchmark scripts to pass `os.cpus().length`.

That let me run direct warmed probes on preserved `small-12` workspaces.

Body-edit probe, one stale file (`MathLib.elm` body-only change):

| Metric | Value |
|---|---:|
| wall time | `2.41s` |
| `load_review_project` | `332ms` |
| `module_rule_eval` | `236ms` |
| `project_rule_eval` | `940ms` |
| `persist_decl_cache` | `179ms` |
| stale files | `1` |

Import-graph probe, one stale file (`ProjectSources.elm` import change on warmed workspace):

| Metric | Value |
|---|---:|
| wall time | `26.51s` |
| `load_review_project` | `346ms` |
| `module_rule_eval` | `2475ms` |
| `project_rule_eval` | `21164ms` |
| stale files | `1` |

The key counters on that warm import-change run were:

| Counter | Value |
|---|---:|
| `project.importers.affected_modules` | `4` |
| `project.importers.cache_hits` | `9` |
| `project.importers.cache_misses` | `3` |
| `project.importers.eval_total_ms` | `18081ms` |
| `project.importers_fold.eval_total_ms` | `971ms` |
| `project.deps.eval_total_ms` | `891ms` |

Current read:

- The common warm body-edit path is now in a much healthier range once the target-project runtime cache is warm and cache workers are not artificially serialized.
- The dominant remaining bottleneck is now sharply localized: **warm import-graph changes are dominated by the `ImportersOf` project-rule path**, not review-app loading, not file parsing, and not cache file I/O.
- On the measured warm import-change run, the cache system was mostly hitting already. The expensive part is what happens after those hits, inside `project.importers.eval_total_ms`.

### Node CPU Profile: Warm Import Change

I also profiled the warm import-change partial miss with Node `--cpu-prof`.

Top sampled frames:

| Samples | Frame |
|---|---:|
| `605` | `(garbage collector)` |
| `177`, `165`, `163`, `144`, ... | `gr` in bundled output |
| `108`, `36` | bundled frame near line `216` |
| `79`, `72`, `70`, `68`, ... | bundled frame near line `242` |

The useful part here is that `gr` is the bundled Elm generic comparison function, so the hot path is spending a lot of time in:

- generic compare / ordering
- GC / allocation churn
- data-structure heavy work in the bundled Elm runtime

Current read:

- This reinforces the import-change thesis: the remaining wall is not shelling out, not JSON formatting, and not review-app setup.
- The remaining wall looks like **allocation-heavy, comparison-heavy project-rule work**, especially in `ImportersOf`.
- The next optimization target should therefore be reducing comparison/allocation pressure in the `ImportersOf` path, likely by using a more compact direct contribution representation instead of repeatedly traversing large generic Elm values.

### Split Cache Seeding: Forced Extraction, Then Narrowed

I tested one specific hypothesis after the Node profile: perhaps the split `ImportersOf` cache path was still underperforming because we were not actually forcing `finalCachePartsMarker` / `extractRuleCaches` on the helper paths that are supposed to seed those artifacts.

What I validated:

- Forcing `extractRuleCaches updatedRules` in the helper layer **does** cause split rule-cache files to be written for the current workspace path.
- After doing that on a warmed body-edit workspace, the active `review-app-*` namespace gained current-path files such as:
  - `NoUnused.Exports-0.base.json`
  - `NoUnused.Parameters-0--..._workspace_src_MathLib.elm.module.json`
  - `NoUnused.CustomTypeConstructors-0--..._workspace_src_ProjectSources.elm.module.json`

That confirmed the mechanism, but the first implementation was too broad:

- forcing extraction directly inside generic helper functions made even ordinary warm runs much more expensive
- it was therefore not safe to leave enabled on every `runRulesByIndices*` / `runReviewCaching*` path

The code is now narrowed so forced extraction is only used on the explicit project cache-writing paths:

- `runProjectRulesWithCacheWrite`
- `runProjectRulesWithCacheWriteModules`

and ordinary helper calls have been restored to their cheaper behavior.

What I do **not** have yet from this narrowed version is a clean end-to-end benchmark number. The temp workspace sequencing got confounded by long-running benchmark processes and by cold-vs-warm setup when switching helper hashes. So the current stable conclusion is:

- the helper-level extraction mechanism is real
- broad helper-level extraction is too expensive
- targeted extraction on explicit project cache-write paths is the right shape
- the next measurement should use a clean dedicated per-family harness or a fresh same-path warm sequence

### Isolated `ImportersOf` Harness And Single-Rule Probes

I added a dedicated harness at [bench/importers-family-benchmark.mjs](bench/importers-family-benchmark.mjs) to isolate the `ImportersOf` family with its own temporary `ReviewConfig`.

The first important result is that `ImportersOf` alone reproduces the wall:

- isolated family cold run stayed active for multiple minutes
- the child runner process reached roughly `1.0GB` RSS while still in the cold run

That means the mixed-config import-change wall is not coming from the rest of the ruleset. The `ImportersOf` family alone is enough to produce the allocation-heavy behavior.

I then narrowed further to single rules inside that family using the same harness:

| Rule | Observation |
|---|---|
| `NoUnused.Exports` | child runner reached about `807MB` RSS after `~45s` in isolated cold mode |
| `NoUnused.Parameters` | child runner reached about `778MB` RSS after `~27s` in isolated cold mode |

These were not full completed timings; they were live-process observations taken with `ps` while the isolated cold run was still executing. That is enough to establish a ranking signal:

- this is not a single outlier hidden in the four-rule bundle
- at least `NoUnused.Exports` and `NoUnused.Parameters` individually hit the same memory-heavy wall

### Upstream Rule Shape: Why `ImportersOf` Is Expensive

I inspected the upstream `elm-review-unused` sources for the two representative rules:

- `NoUnused.Exports`
- `NoUnused.Parameters`

The shared pattern matches the Node CPU profile (`gr` + GC):

- both rules build large project contexts out of `Dict` and `Set`
- both rules merge those contexts through `foldProjectContexts`
- `NoUnused.Parameters` also accumulates per-function call-site lists across modules

Representative upstream structures:

- `NoUnused.Exports.ProjectContext`
  - `modules : Dict ModuleNameStr {...}`
  - `usedModules : Set ModuleNameStr`
  - `used : Set ElementIdentifier`
  - `usedInIgnoredModules : Set ElementIdentifier`
  - `constructors : Dict (ModuleNameStr, String) String`
  - `typesImportedWithConstructors : Set (ModuleNameStr, String)`

- `NoUnused.Parameters.ProjectContext`
  - `toReport : Dict ModuleName { key : ModuleKey, args : List ArgumentToReport }`
  - `functionCallsWithArguments : Dict ( ModuleName, FunctionName ) (List { key : ModuleKey, isFileFixable : Bool, callSites : List CallSite })`

And their folds are straightforward `Dict.union`, `Set.union`, or `Dict.foldl` merges. In the interpreter, that means a lot of:

- generic compare
- persistent structure allocation
- GC churn

### Current Read

The next optimization target should not be “more generic cache plumbing” around these rules. The new data points toward a deeper seam:

- either direct contribution artifacts for `ImportersOf`
- or a host-native implementation for the hottest `ImportersOf` summaries / folds

The important updated thesis is:

- `ImportersOf` itself is the wall
- the wall is shared `Dict` / `Set` project-context churn
- the likely winning move is to bypass that interpreted fold entirely, not just cache around it

### Host-Native `NoUnused.Exports` Experiment

I implemented a guarded host-native `NoUnused.Exports` path in [src/ReviewRunner.elm](src/ReviewRunner.elm), enabled with:

```bash
--host-no-unused-exports-experiment
```

Scope of the first slice:

- application-style `NoUnused.Exports`
- unused module detection
- value/function exports
- explicit exposing and `exposing (..)` value checks
- module alias resolution
- `exposing (..)` imported-value resolution from host summaries

This path intentionally bypasses interpreted `ImportersOf` evaluation for `NoUnused.Exports` entirely when enabled. The current implementation reparses source on the host for this experiment; it does **not** yet reuse a dedicated cached summary artifact.

#### Isolated `NoUnused.Exports` A/B

Using the isolated family harness with a temporary `ReviewConfig` that only includes `NoUnused.Exports`:

| Scenario | Interpreted runner | Host-native experiment |
|---|---:|---:|
| Cold | `89.76s` | `26.24s` |
| Warm | `0.29s` | `0.32s` |
| Warm import-graph change | `4.31s` | `1.27s` |

Important details from the trace:

- interpreted cold `project_rule_eval`: `63.60s`
- host-native cold `project_rule_eval`: `2ms`
- interpreted warm import-change `project_rule_eval`: `3.13s`
- host-native warm import-change `project_rule_eval`: `2ms`

The host-native run fully bypassed the interpreted `ImportersOf` machinery:

- `project.importers.eval_total_ms = 0`
- `project.importers.cache_hits = 0`
- `project.importers.cache_misses = 0`

That is the clearest result so far that bypassing the interpreted project-context folds is the right direction.

#### Correctness Read For The Experiment

I compared three things on the isolated `NoUnused.Exports` fixture:

1. current interpreted runner
2. host-native experiment
3. real `elm-review` CLI

The host-native experiment was initially different by one finding:

- current interpreted runner reported `ProjectSources.resolvePackageVersions` as unused
- host-native experiment did **not**

I then checked the real `elm-review` CLI on the same isolated config. The CLI also **did not** report `ProjectSources.resolvePackageVersions` as unused.

So on this point:

- host-native experiment matches the CLI
- current interpreted runner appears to have an extra incorrect finding

That means the host-native experiment is not just faster on this isolated rule; it is also at least competitive on correctness for the specific mismatch we found.

#### Current Read

This is the strongest architectural signal so far:

- bypassing interpreted `ImportersOf` folds for one hot rule produced a large win
- the win showed up on both cold and warm import-change paths
- the experiment did not depend on daemonization or JS patching

The next obvious expansion paths are:

- reuse host-side cached summaries instead of reparsing source for the experiment
- extend the same host-native / direct-contribution approach to the other `ImportersOf` rules
- use the same method on `DependenciesOf` if isolated profiling shows a comparable payoff

### Generic Cross-Module Fact Layer

I moved the `NoUnused.Exports` experiment onto a more general cached fact layer in
[src/ReviewRunner.elm](src/ReviewRunner.elm) instead of treating it as a one-off source parser.

`FileAnalysis.crossModuleSummary` now stores reusable cross-module facts such as:

- value exposure facts
- constructor declaration and exposure facts
- open-type import facts
- value dependency refs
- constructor refs collected from expressions and patterns

This keeps the architecture aligned with the intended split:

- host side owns reusable facts and caches
- rules can stay interpreted, or use the same facts for upper-bound experiments

I added a unit test for the new constructor-level facts in
[src/ReviewRunnerTest.elm](src/ReviewRunnerTest.elm), and the full test suite passed:

```bash
npx elm-pages run src/RunTests.elm
```

Result:

- `213 passed, 0 failed`

#### Re-check: isolated `NoUnused.Exports` after fact-layer reuse

I reran the same isolated benchmark after switching the host path to consume
`crossModuleSummary` instead of reparsing sources.

| Scenario | Interpreted runner | Host via cached facts |
|---|---:|---:|
| Cold | `94.72s` | `26.70s` |
| Warm | `0.30s` | `0.31s` |
| Warm import-graph change | `4.27s` | `1.22s` |

So the shared fact layer preserved the earlier win. That is the important signal:

- richer cached facts did not erase the `NoUnused.Exports` speedup
- the same cached summary shape is now broad enough to support the next `ImportersOf` experiment without adding another ad hoc parsing path

### Host-Native `NoUnused.CustomTypeConstructors` Experiment

I extended the same generic `crossModuleSummary` fact layer to carry constructor-level facts with an important policy split:

- constructor uses in expressions
- constructor appearances in patterns

That distinction matters for `NoUnused.CustomTypeConstructors`, because a pattern like:

```elm
case status of
    Killed ->
        ...
```

does **not** mean the constructor is created anywhere in the project. The first version of the host experiment over-counted those pattern appearances as real usage and incorrectly reported zero errors on the benchmark fixture. After splitting those fact kinds, the host experiment matched the interpreted runner on the isolated fixture’s 10 findings.

#### Direct correctness check

On the isolated `NoUnused.CustomTypeConstructors` fixture:

- interpreted runner: 10 errors
- host experiment after the pattern/use split: 10 errors

The host path now reports the same unused constructors in:

- `src/MutationReport.elm`
- `src/UserAccess.elm`

#### Isolated `NoUnused.CustomTypeConstructors` A/B

| Scenario | Interpreted runner | Host via cached facts |
|---|---:|---:|
| Cold | `103.39s` | `27.29s` |
| Warm | `0.32s` | `0.33s` |
| Warm import-graph change | `74.72s` | `1.28s` |

Important trace details from the host run:

- cold `project_rule_eval`: `3ms`
- warm import-change `project_rule_eval`: `2ms`
- host constructor errors on the fixture: `10`

This is the second strong isolated result on the same architectural seam:

- cached host-side facts
- project-rule fold bypass
- no daemon
- no compiled-JS patching

The main remaining caveat is the same as for `NoUnused.Exports`: this is still an upper-bound experiment with policy implemented on the host side. The fact layer itself is the keeper. The long-term direction remains:

- facts cached natively / host-side
- policy interpreted when possible on top of those facts

### Host-Native `NoUnused.CustomTypeConstructorArgs` Experiment

I extended the same cached fact layer one step further for `NoUnused.CustomTypeConstructorArgs`.

The additional generic facts needed were:

- constructor argument ranges
- constructor pattern usages with per-argument used positions
- constructor refs seen under `==` / `/=`

Those are now stored in `FileAnalysis.crossModuleSummary`, alongside the earlier
constructor declaration/exposure facts. The relevant architecture point is that
these are still reusable facts, not rule-specific caches.

#### Direct correctness check

I added unit coverage in
[src/ReviewRunnerTest.elm](src/ReviewRunnerTest.elm) for both:

- the pure-source host helper
- the new cross-module summary facts

Then I did a direct JSON check against the isolated fixture.

Both paths reported the exact same single finding:

- `NoUnused.CustomTypeConstructorArgs`
- `src/ReviewRunner.elm:3492:16`
- `Argument is never extracted and therefore never used.`

So on the isolated fixture, the host experiment matches the interpreted runner on reported output.

#### Isolated `NoUnused.CustomTypeConstructorArgs` A/B

| Scenario | Interpreted runner | Host via cached facts |
|---|---:|---:|
| Cold | `111.19s` | `28.20s` |
| Warm | `0.35s` | `0.35s` |
| Warm import-graph change | `5.45s` | `1.31s` |

Important trace details from the host run:

- cold `project_rule_eval`: `3ms`
- warm import-change `project_rule_eval`: `4ms`
- host constructor-arg errors on the fixture: `1`

This is the third isolated win on the same seam:

- `NoUnused.Exports`
- `NoUnused.CustomTypeConstructors`
- `NoUnused.CustomTypeConstructorArgs`

All three now show the same pattern:

- the expensive wall is in the interpreted `ImportersOf` fold
- cached host-side facts remove that wall almost entirely
- warm import-change time drops to about `1.2s - 1.3s` for each isolated rule

#### Combined `ImportersOf` family checkpoint

I also ran the isolated family harness with all three host experiments enabled together:

- `NoUnused.Exports`
- `NoUnused.CustomTypeConstructors`
- `NoUnused.CustomTypeConstructorArgs`

Result:

| Scenario | Interpreted family | Three fact-backed host experiments |
|---|---:|---:|
| Warm import-graph change | still very large | still very large (`158.09s`) |

The reason is now clearer: once those three rules are effectively removed from the interpreted project-rule wall, `NoUnused.Parameters` becomes the dominant remaining `ImportersOf` cost.

That is useful, because it narrows the next target cleanly:

- the generic cached fact layer is broad enough to support three rules already
- the next high-value rule in this family is `NoUnused.Parameters`
- after that, the `ImportersOf` family should be much closer to the CLI-relevant warm partial-miss target

### Host-Native `NoUnused.Parameters` Experiment

I extended the same cached fact layer to `NoUnused.Parameters`.

This first version is still an upper-bound experiment, but it now works from
cached generic per-file parameter facts rather than reparsing source:

- top-level function parameter summaries
- nested `let` function parameter summaries
- nested lambda parameter summaries
- binding-kind metadata so `as`-pattern aliases report the same message shape as the CLI

The important point is that the cached data is still generic fact data. The
host path uses it directly today, but this is the same fact/policy split we want
for a longer-term interpreted solution.

#### Isolated `NoUnused.Parameters` A/B

| Scenario | Interpreted runner | Host via cached facts |
|---|---:|---:|
| Cold | `143.93s` | `27.50s` |
| Warm | `0.32s` | `0.34s` |
| Warm import-graph change | `171.64s` | `1.35s` |

That is the strongest isolated win in this series so far. It confirms that the
remaining `ImportersOf` wall was not semantic complexity in the rule itself. It
was the interpreted project-rule machinery and the `Dict`/`Set` churn around it.

#### Combined `ImportersOf` Family A/B

After adding the `NoUnused.Parameters` host experiment, I reran the full
isolated `ImportersOf` family:

| Scenario | Interpreted family | Four fact-backed host experiments |
|---|---:|---:|
| Cold | `260.43s` | `26.14s` |
| Warm | `0.33s` | `0.33s` |
| Warm import-graph change | `188.70s` | `1.32s` |

That is the clearest result so far that the high-value optimization seam is:

- host-cached generic facts
- bypass of the interpreted `ImportersOf` fold
- no daemon
- no compiled-JS patching

#### Combined Family Correctness

I then ran a direct JSON diff for the combined host-backed family against the
real `elm-review` CLI on the same fixture workspace and review config.

Final result:

- host count: `47`
- CLI count: `47`
- missing: `[]`
- extra: `[]`

So the combined host-backed `ImportersOf` family matches the CLI exactly on the
isolated benchmark fixture.

### Full `small-12` Mixed-Rule Integration Checkpoint

I then ran the full mixed `small-12` benchmark with the four host-backed
`ImportersOf` experiments enabled through the normal runner path, and reran the
diff harness against the real `elm-review` CLI.

#### Full mixed correctness

The mixed diff harness now matches the CLI for all five benchmark scenarios:

| Scenario | Runner | CLI | Match |
|---|---:|---:|---|
| Cold | `568` | `568` | yes |
| Warm | `568` | `568` | yes |
| Warm 1-file body edit | `569` | `569` | yes |
| Warm 1-file comment-only | `568` | `568` | yes |
| Warm import-graph change | `569` | `569` | yes |

So the mixed-rule path is now correct for this fixture with the host-backed
`ImportersOf` shortcuts enabled.

#### Full mixed perf

Full mixed runner benchmark on `small-12` with:

- `--importers-cache-mode auto`
- `--deps-cache-mode auto`
- `--host-importers-experiments`

Result:

| Scenario | Runner |
|---|---:|
| Cold | `224.12s` |
| Warm | `0.40s` |
| Warm 1-file body edit | `2.41s` |
| Warm 1-file comment-only | `0.81s` |
| Warm import-graph change | `5.09s` |

Fresh CLI benchmark on the same fixture:

| Scenario | elm-review CLI |
|---|---:|
| Cold | `2.91s` |
| Warm | `1.00s` |
| Warm 1-file body edit | `0.98s` |
| Warm 1-file comment-only | `0.98s` |
| Warm import-graph change | `0.98s` |

#### What this means

This integration confirms two things clearly:

1. The `ImportersOf` seam was real.
   The isolated family win survives the mixed runner path and stays correct.

2. `ImportersOf` is no longer the whole bottleneck in mixed partial-miss runs.
   Even after collapsing that family, the mixed warm body-edit path is still
   `2.41s` and the mixed warm import-change path is still `5.09s`, both slower
   than the CLI.

So the next optimization target should not be more `ImportersOf` work. The next
target is the other remaining rule families and setup costs that dominate mixed
partial-miss runs once `ImportersOf` is no longer the wall.

### Current Status Summary

At this point, the main architectural questions are answered.

What has been proven:

- full-hit warm caching is a real advantage over `elm-review` CLI
- coarse cached facts are a much better optimization seam than generic
  eval-loop memoization for cheap helper calls
- the `ImportersOf` family can be collapsed dramatically by working from cached
  facts and bypassing the interpreted fold
- that fact-backed path can still match the CLI exactly on the benchmark fixture

What has **not** been achieved yet:

- beating `elm-review` CLI on the common mixed partial-miss paths
- a general interpreted-policy path over cached facts for these families

Current mixed `small-12` picture:

| Scenario | Runner | elm-review CLI |
|---|---:|---:|
| Warm | `0.40s` | `1.00s` |
| Warm 1-file body edit | `2.41s` | `0.98s` |
| Warm 1-file comment-only | `0.81s` | `0.98s` |
| Warm import-graph change | `5.09s` | `0.98s` |

Current bottleneck thesis:

- `ImportersOf` is no longer the dominant mixed-rule wall
- the next likely expensive family is `DependenciesOf` / `NoUnused.Variables`
- after that, remaining work is in setup (`load_review_project`) and module-rule cost

### Isolated `DependenciesOf` Baseline

I added a dedicated isolated benchmark harness at
[bench/dependencies-family-benchmark.mjs](bench/dependencies-family-benchmark.mjs)
for `NoUnused.Variables`, which is currently the only `DependenciesOf` rule in
the benchmark config.

Current isolated baseline on `small-12` with `--deps-cache-mode auto`:

| Scenario | `NoUnused.Variables` interpreted |
|---|---:|
| Cold | `138.38s` |
| Warm | `0.32s` |
| Warm import-graph change | `5.95s` |

Important trace details:

- cold `project_rule_eval`: `111.95s`
- warm import-change `project_rule_eval`: `4.66s`
- warm import-change `deps_eval_total_ms`: `1.49s`
- warm import-change `deps_rule_cache_loaded_bytes`: `87KB`

So the current `DependenciesOf` wall is real, but not as catastrophic as the
pre-bypass `ImportersOf` family. It also has the same shape:

- cache IO is tiny
- the remaining cost is dominated by interpreted project-rule work and setup

#### Current error shape on the fixture

The isolated CLI run reports `22` `NoUnused.Variables` findings on the fixture.
Most of them are simpler categories than the full rule surface might suggest:

- unused imported modules
- unused imported types
- unused top-level variables
- one unused `let in` variable
- one unnecessary import to implicitly imported `Char`
- one unused local type

That is useful because it suggests the first fact-backed upper-bound for
`NoUnused.Variables` can stay focused on:

- generic import facts
- top-level declaration/use facts
- local type declarations

before worrying about every corner of the scope machinery.

### Generic Import Fact Layer

I extended `crossModuleSummary` with generic import summaries:

- imported module name + module name range
- import range
- alias name + alias range
- exposing-all range
- explicit imported elements with per-element ranges and open-type ranges

These are cached facts, not rule-specific outputs. The immediate motivation is
`NoUnused.Variables`, but this data should also be broadly useful for other
import-related rules and fixes.

### `DependenciesOf` Host-Fact Upper Bound

I extended the generic cached fact layer again for the `NoUnused.Variables`
upper-bound:

- top-level declaration ranges
- local type declarations
- generic import summaries
- generic let-binding summaries

The first useful benchmark result is strong. On the isolated
`NoUnused.Variables` harness:

| Scenario | Interpreted baseline | Host-fact upper bound |
|---|---:|---:|
| Cold | `138.38s` | `27.45s` |
| Warm | `0.32s` | `0.32s` |
| Warm import-graph change | `5.95s` | `1.36s` |

So this is another clear proof that the right seam is cached facts plus bypass
of the interpreted project-rule fold.

#### Updated correctness status

The remaining mismatch turned out to be a dependency-docs decoder bug, not a
fact-model problem:

- I was decoding union constructor names from a non-existent `tags` field
- Elm package `docs.json` stores them under `cases`
- that left the dependency open-type constructor index effectively empty

After fixing that, the isolated `NoUnused.Variables` harness matches the real
`elm-review` CLI exactly on a fresh workspace:

| Metric | Count |
|---|---:|
| CLI findings | `22` |
| Host-fact findings | `22` |
| Missing | `0` |
| Extra | `0` |

### Mixed `small-12` With All Five Host-Backed `NoUnused` Experiments

With the `NoUnused.Variables` shortcut enabled alongside the four
`ImportersOf` shortcuts, the full mixed `bench/review` config is now exact in
all five scenarios.

Current mixed `small-12` benchmark:

| Scenario | Runner | elm-review CLI |
|---|---:|---:|
| Cold | `111.19s` | `2.95s` |
| Warm | `0.37s` | `1.05s` |
| Warm 1-file body edit | `1.65s` | `1.06s` |
| Warm 1-file comment-only | `0.86s` | `1.09s` |
| Warm import-graph change | `1.65s` | `1.14s` |

Diff harness result:

| Scenario | Result |
|---|---|
| Cold | `match` |
| Warm | `match` |
| Warm 1-file body edit | `match` |
| Warm 1-file comment-only | `match` |
| Warm import-graph change | `match` |

The previous project-rule wall is now effectively gone on this fixture:

| Scenario | `load_review_project` | `module_rule_eval` | `project_rule_eval` |
|---|---:|---:|---:|
| Warm 1-file body edit | `314ms` | `242ms` | `10ms` |
| Warm import-graph change | `313ms` | `245ms` | `12ms` |

That changes the next target again.

The remaining mixed gap to the CLI is now mostly:

- fixed warm setup tax in `load_review_project`
- remaining module-rule execution
- outer wall-clock overhead outside the traced inner stages

### Module-Rule Ranking Checkpoint

After the mixed run stopped being project-rule bound, I added an isolated
module-rule harness at `bench/module-rules-benchmark.mjs`.

The first targeted measurements point at the two type-annotation rules:

| Rule | Cold | Warm | Warm 1-file body edit | `load_review_project` | `module_rule_eval` |
|---|---:|---:|---:|---:|---:|
| `NoMissingTypeAnnotation` | `51.15s` | `0.35s` | `1.53s` | `317ms` | `138ms` |
| `NoMissingTypeAnnotationInLetIn` | `55.07s` | `0.33s` | `1.52s` | `332ms` | `139ms` |

Two reads from that:

- these rules are individually expensive enough to matter
- but the mixed gap is not only inside rule logic

So the next likely wins are:

- cheaper `load_review_project`, especially package summary decode / setup
- tighter measurement of outer wall-clock overhead outside traced inner stages
- then, if needed, targeted work on the type-annotation module rules

### Fact-Contract Hashing Checkpoint

I added the first explicit fact-contract layer and stable per-family hashes in
`src/ReviewRunner.elm`, with regression coverage in
`src/ReviewRunnerTest.elm`.

What is now explicit:

- `FactSet`
- `RuleFactContract`
- `FactHashes`
- `buildFactContractHashKey`

The first important correction from this work was that
`NoMissingTypeAnnotation` does **not** really depend on `FunctionUsage`. It
needed a new `DeclarationShape` fact family keyed by:

- function name
- arity
- whether a signature exists

That let the contract model capture the real dependency more precisely:

- import-only edits keep the `NoMissingTypeAnnotation` contract key stable
- body-only edits keep the `NoMissingTypeAnnotation` contract key stable
- signature edits change the `NoMissingTypeAnnotation` contract key

I then used that contract in the runtime by splitting declaration-shape-only
module rules into a separate cache batch instead of lumping them into the
broader expression-sensitive module-rule group.

The isolated `NoMissingTypeAnnotation` benchmark moved slightly:

| Scenario | Before | After |
|---|---:|---:|
| Cold | `51.15s` | `49.63s` |
| Warm | `0.35s` | `0.32s` |
| Warm 1-file body edit | `1.53s` | `1.45s` |
| `module_rule_eval` | `138ms` | `132ms` |

And the mixed direct probe with all five host-backed `NoUnused` shortcuts stayed
in the same overall band while shaving a little module-rule time:

| Scenario | Before | After |
|---|---:|---:|
| Warm 1-file body edit | `1.65s` | `1.647s` |
| `load_review_project` | `314ms` | `350ms` |
| `module_rule_eval` | `242ms` | `233ms` |
| `project_rule_eval` | `10ms` | `10ms` |

That is an important read:

- the visitor/fact/hash coupling is correct and testable
- it is already paying off strongly on the project-rule families
- the first module-rule use is valid and slightly positive
- but it is **not** the next big lever by itself

So the current bottleneck order remains:

1. `load_review_project`
2. remaining module-rule execution
3. outer wall-clock overhead outside the traced inner stages

### Package-Closure Reduction Checkpoint

I tightened the review-app package summary cache so it only stores and decodes
the package-module closure actually reachable from the review app roots,
instead of every package module discovered in the graph.

Implementation notes:

- `InterpreterProject.loadWithProfile` now computes the reachable package
  module set from the non-package roots plus explicit extra reachable imports.
- `ReviewRunner.loadReviewProjectDetailed` passes the helper module imports as
  `extraReachableImports` so virtual helper dependencies like `Elm.Project`
  stay in the closure.
- The package summary cache version was bumped to `v5`.

The direct mixed probe with all five host-backed `NoUnused` shortcuts showed a
clear setup win:

| Scenario | Before | After |
|---|---:|---:|
| Warm 1-file body edit | `1.647s` | `1.666s` |
| Warm import-graph change | `1.65s` | `1.59s` |
| `load_review_project` | `350ms` | `219-220ms` |
| `decode_package_summary_cache_ms` | `269ms` | `135-137ms` |
| `package_summary_cache_bytes` | `2479004` | `1385602` |

The overall read is:

- The closure reduction does exactly what it should: it materially reduces the
  fixed review-app warm-load tax.
- It is not, by itself, a large end-to-end body-edit win yet because the mixed
  path is now dominated by remaining module-rule cost plus outer overhead.
- It is still a clear keeper because it halves the package-summary decode cost
  and reduces a fixed tax that affects every warm partial miss.

### Warm-Path Attribution Checkpoint

I added explicit timing for the remaining pure work after evaluation:

- `parse_review_output`
- `update_decl_cache`
- `encode_decl_cache`

On the direct mixed warm body-edit probe, the traced stages are now:

| Stage | Time |
|---|---:|
| `prepare_config` | `13ms` |
| `load_decl_cache` | `5ms` |
| `resolve_target_files` | `2ms` |
| `read_target_files` | `4ms` |
| `analyze_target_files` | `45ms` |
| `load_review_project` | `224ms` |
| `get_rule_info` | `1ms` |
| `module_rule_eval` | `318ms` |
| `project_rule_eval` | `10ms` |
| `parse_review_output` | `1ms` |
| `update_decl_cache` | `3ms` |
| `encode_decl_cache` | `3ms` |
| `persist_decl_cache` | `96ms` |

Those stages sum to about `725ms`, while the full process wall is about
`1659ms`. So the remaining missing wall is **not**:

- project-rule work
- output parsing
- declaration-cache recomputation
- declaration-cache encoding

It is partly `persist_decl_cache`, but the larger remaining gap now looks like
process/bootstrap/runtime overhead outside the traced Elm stages. That makes
Node/V8 startup or bundle/runtime overhead the next attribution target, not
another guess inside the existing traced pipeline.

### `elm-pages3` Yield Experiment

I also tried the experimental `../elm-pages3` change that yields between
`BackendTask` batches to reduce GC backpressure during script execution.

I bundled the runner with the local `elm-pages3` branch and reran the direct
probe on `small-12` with the full host-backed `NoUnused` set:

| Scenario | Wall | `load_review_project` | `module_rule_eval` | `project_rule_eval` |
|---|---:|---:|---:|---:|
| Warm 1-file body edit | `1691ms` | `267ms` | `305ms` | `15ms` |

Compared to the previous direct probe, this is not a meaningful win. The warm
body-edit wall remained in the same band, and the traced inner stages did not
show a clear reduction that would justify attributing the remaining gap to GC
backpressure in `BackendTask` batch scheduling.

Current read:

- The yield-between-batches experiment does not appear to move the common warm
  path on this benchmark in a meaningful way.
- The main bottlenecks remain startup/runtime overhead outside traced Elm
  stages, plus remaining module-rule execution.
- This is a useful negative result, but it does not change the optimization
  priority order.

### Type-Annotation Module-Rule Upper Bound

I added two fact-backed upper-bound experiments for the remaining hot module
rules:

- `NoMissingTypeAnnotation`
- `NoMissingTypeAnnotationInLetIn`

They reuse the existing cached function summaries:

- top-level function summaries for `NoMissingTypeAnnotation`
- nested function summaries (excluding lambdas) for `NoMissingTypeAnnotationInLetIn`

Both experiments are isolated behind flags and come with pure helper tests.

Isolated results on `small-12`:

| Rule | Scenario | Interpreted baseline | Host/fact upper bound |
|---|---|---:|---:|
| `NoMissingTypeAnnotation` | Cold | `49.63s` | `24.26s` |
| `NoMissingTypeAnnotation` | Warm 1-file body edit | `1.45s` | `1.286s` |
| `NoMissingTypeAnnotationInLetIn` | Cold | `55.07s` | `25.36s` |
| `NoMissingTypeAnnotationInLetIn` | Warm 1-file body edit | `1.52s` | `1.285s` |

The key internal signal is that `module_rule_eval` for the isolated rules
effectively disappears:

- `NoMissingTypeAnnotation`: `17ms` cold, `3ms` warm body edit
- `NoMissingTypeAnnotationInLetIn`: `21ms` cold, `4ms` warm body edit

That is strong confirmation that these two rules are real remaining module-rule
hot spots, and that the cached summary layer is rich enough to bypass them.

Mixed direct probe with all five host-backed `NoUnused` experiments plus both
type-annotation experiments:

| Scenario | Before | After |
|---|---:|---:|
| Warm 1-file body edit | `1660ms` | `1479ms` |
| `module_rule_eval` | `307ms` | `209ms` |
| `load_review_project` | `213ms` | `207ms` |
| `project_rule_eval` | `12ms` | `10ms` |

So the type-annotation upper bound buys about `180ms` on the mixed warm
body-edit path. That is useful, but it also makes the next conclusion clearer:
the remaining gap to `elm-review` CLI is now increasingly dominated by
startup/runtime/setup overhead rather than project-rule execution.

### Startup / Bundle Overhead Attribution

I tested the “rich CLI parsing is slow” theory directly by adding a separate
fast entrypoint (`ReviewRunnerFast.elm`) that takes a single JSON config from
`REVIEW_RUNNER_CONFIG_JSON` instead of going through the large `Cli.*` options
surface.

That A/B did **not** show a win on the real warm body-edit path:

| Entry | Warm 1-file body edit |
|---|---:|
| Current runner entry | `1587ms` |
| Fast env-config entry | `1637ms` |

So the rich CLI/options parser is not the main culprit.

I then added two more probes:

- `StartupNoOp.elm`: a minimal no-op script
- `startupOnly` mode on the fast runner entrypoint, which exits after startup
  and `prepare_config`

Startup benchmark (`5` iterations):

| Script | Average | Min | Max |
|---|---:|---:|---:|
| `StartupNoOp` | `235ms` | `229ms` | `241ms` |
| `ReviewRunnerFast startupOnly` | `227ms` | `222ms` | `230ms` |

Bundle sizes are effectively identical:

| Bundle | Size |
|---|---:|
| `dist/startup-no-op-bench.mjs` | `1,318,265` bytes |
| `dist/review-runner-fast-bench.mjs` | `1,318,270` bytes |

This is the important conclusion:

- There is a real process/runtime floor of about `230ms`.
- That floor is basically the same for a no-op script and the fast runner
  startup-only path.
- So the large remaining gap on warm body edits is **not** primarily the rich
  CLI parser, and it is **not** primarily the runner bundle size/import cost.

That moves the priority back to traced work:

- `load_review_project` (~`220ms`)
- `module_rule_eval` (~`209ms` with current host-backed upper bounds)
- `persist_decl_cache` (~`90ms`)

And inside `load_review_project`, the biggest remaining target is the package
summary cache decode itself:

- `decode_package_summary_cache_ms` ~ `137ms`
- `build_package_env_from_summaries_ms` ~ `3ms`
- `build_graph_ms` ~ `45ms`

So the next likely high-value experiment is a faster durable cache format for
package summaries, not more CLI/parser work.

### Package Summary Cache Codec Benchmark

I added a direct codec benchmark for the review-app package summary cache in:

- `src/PackageSummaryCacheBenchmark.elm`
- `src/ReviewRunner.elm`
- `src/InterpreterProject.elm`

This uses the exact same review-project loading configuration as the runner:

- same `patchSource`
- same skipped kernel/conflicting packages
- same helper reachable imports
- same reduced package-module closure

It first seeds the package summary cache, then benchmarks repeated encode/decode
of the same cached summaries in two formats:

- current Lamdera.Wire binary blob
- JSON encoding of the same reduced package summary structure

Warm benchmark result (`5` iterations):

```json
{
  "iterations": 5,
  "summaryCount": 122,
  "binaryBytes": 1385602,
  "jsonChars": 8726090,
  "seedCacheHit": 1,
  "seedParsePackageSourcesMs": 0,
  "seedBuildPackageSummariesFromParsedMs": 0,
  "seedDecodePackageSummaryCacheMs": 156,
  "binaryEncodeMs": 832,
  "binaryDecodeMs": 776,
  "jsonEncodeMs": 601,
  "jsonDecodeMs": 1656
}
```

The read is straightforward:

- Binary stays much smaller (`1.39MB` vs `8.73MB`).
- JSON encode is a bit faster.
- JSON decode is much slower.
- So a simple “replace the package summary blob with JSON” change is not the
  right next optimization.

This rules out the easiest alternative format and narrows the next choices to:

- a more specialized binary format
- pushing more of the decode work into JS/custom backend tasks
- or shifting attention to the other remaining traced costs first

At this point, the package summary cache is still worth optimizing, but JSON is
not the path.

### Declaration Cache Sharding

I switched the on-disk declaration cache from one monolithic JSON file to:

- a lightweight manifest
- one shard per target file
- legacy fallback for old single-file caches

The in-memory cache shape is unchanged. Only the persistence layout changed.

The direct warm body-edit trace with the current host-backed `NoUnused.*` and
type-annotation shortcuts now shows:

| Metric | Before | After |
|---|---:|---:|
| `decl_cache.stored_bytes` | about `1.9MB` | `1455` bytes |
| `decl_cache.written_shards` | n/a | `1` |
| `persist_decl_cache` | about `90ms` | `6ms` |

Warm trace details after the change:

- `load_decl_cache`: `7ms`
- `decl_cache.loaded_bytes`: `1928102`
- `decl_cache.loaded_shards`: `12`
- `persist_decl_cache`: `6ms`
- `decl_cache.stored_bytes`: `1455`
- `decl_cache.written_shards`: `1`

So this is a clear constant-factor win on the common partial-miss path.

It does **not** reduce load cost yet because load still reads all current file
shards. But it removes most of the unnecessary write/encode churn on partial
misses, and it narrows the next bottleneck further to:

- `load_review_project`
- remaining `module_rule_eval`
- startup/runtime floor

### Explicit Fact Projections

I turned the existing fact-hash path into an explicit typed projection layer.

New pieces in `ReviewRunner.elm`:

- `FactProjectionArtifact a = { hash, value }`
- `FactProjections`
- `RuleProjectionKey`
- `factProjectionsForSource`
- `factProjectionsForSummary`
- `buildRuleProjectionKey`

The important part is that `factHashesForSummary` now derives from typed
projection artifacts instead of hashing directly from `CrossModuleSummary`.
Runtime behavior is intentionally unchanged in this slice. The goal was to make
the projection boundary explicit and testable before using it to drive more
invalidation decisions.

Added test coverage:

- direct projection extraction on a representative module
- projection artifact hash matches legacy `FactHashes`
- `buildRuleProjectionKey` stringifies to the same key as the legacy
  `buildFactContractHashKey`
- existing comment/import/body/signature/constructor hash-stability tests still
  pass

Validation:

- `npx elm-pages run src/RunTests.elm` -> `246 passed, 0 failed`
- `npx elm-pages bundle-script src/ReviewRunner.elm --output dist/review-runner-bench.mjs`

This gives us a safer next step for runtime work: use explicit
`FactProjection`/`RuleProjectionKey` values to narrow remaining interpreted
module-rule invalidation, instead of bolting more ad hoc fact-hash handling
onto the existing cache logic.

### Broader Contract-Keyed Module Rule Grouping

I extended the mixed module-rule batching so any module rule with an explicit
`RuleFactContract` now gets grouped into a projection-keyed batch, instead of
only the special `DeclarationShape` case.

New explicit module-rule contracts:

- `NoMissingTypeAnnotationInLetIn` -> `DeclarationShape`
- `NoExposingEverything` -> `ExportShape`
- `NoImportingEverything` -> `ImportShape`

That means these rules no longer fall into the broad default module-rule batch
on body-only edits when their projection hashes are unchanged.

Validation:

- `npx elm-pages run src/RunTests.elm` -> `250 passed, 0 failed`
- `npx elm-pages bundle-script src/ReviewRunner.elm --output dist/review-runner-bench.mjs`

I do **not** have a clean warm body-edit timing for this slice yet. The
existing direct benchmark harness is a poor fit because it spends most of its
time reseeding a fresh workspace before it reaches the warmed partial-miss run.
The next measurement task should be a tiny dedicated warmed-body-edit probe so
we can isolate the runtime effect of the broader grouping cleanly.

### Dedicated Module-Rule Grouping Probe

I added two measurement tools for the broader contract-keyed module-rule
grouping work:

- `moduleRuleGroupingMode` in `ReviewRunner` with two modes:
  - `contracts`
  - `legacy`
- `bench/module-rule-grouping-probe.mjs`

The reduced probe uses a module-only review config with:

- `NoExposingEverything`
- `NoImportingEverything`
- `NoMissingTypeAnnotation`
- `NoMissingTypeAnnotationInLetIn`
- `NoUnused.Patterns`

This isolates the grouping effect without the host-backed `NoUnused.*`
shortcuts.

Legacy mode completed and produced a clean warm body-edit trace:

| Metric | Legacy |
|---|---:|
| `load_review_project` | `188ms` |
| `module_rule_eval` | `287ms` |
| `project_rule_eval` | `3ms` |
| `persist_decl_cache` | `7ms` |

The broader `contracts` mode exposed an important negative signal on the cold
path: the reduced probe's cold seed did not complete even after running
materially longer than the legacy cold seed. I stopped that run before claiming
an A/B number.

Current read:

- The explicit projection layer is correct and test-covered.
- Grouping more module rules by explicit contracts is architecturally sound.
- A naive “group every contract-bearing module rule separately” strategy can
  over-fragment the cold path.

That points to the next refinement:

- apply broader contract-keyed grouping only on partial misses, or
- keep only the highest-value module rules on separate contract-keyed batches,
  instead of widening it indiscriminately.

### Auto Grouping On Partial Misses

I refined `moduleRuleGroupingMode` to support:

- `legacy`
- `contracts`
- `auto`

`auto` now uses:

- `legacy` for cold/full-stale module runs
- `contracts` only for true partial misses

I switched the reduced probe to run through `ReviewRunnerFast` with
`REVIEW_RUNNER_CONFIG_JSON`, because the bundled CLI entrypoint is currently
broken for benchmark flags.

That let me get a clean A/B from the reduced warmed body-edit path:

| Metric | `legacy` | `auto` |
|---|---:|---:|
| `load_review_project` | `203ms` | `196ms` |
| `module_rule_eval` | `374ms` | `567ms` |
| `project_rule_eval` | `3ms` | `3ms` |
| `persist_decl_cache` | `7ms` | `6ms` |
| grouping counters | `groups=2`, `legacy=1` | `groups=4`, `auto=1`, `contracts=1` |

So even after limiting contract grouping to partial misses, the reduced module
path is still materially worse. I changed the default module-rule grouping mode
back to `legacy`.

Current read:

- explicit projection artifacts are still the right abstraction
- broad contract-keyed module grouping is not paying for itself
- the next improvement should be narrower, rule-targeted use of those
  projections rather than more batching fragmentation

### Package Summary Cache: Sharded Decode Benchmark

I extended `PackageSummaryCacheBenchmark` to compare three representations of
the review-app package summary cache:

- one large Lamdera binary blob
- one Lamdera binary blob per module (“sharded”)
- JSON

Measured on the current reduced package closure:

| Metric | One blob | Sharded blobs | JSON |
|---|---:|---:|---:|
| Size | `1,385,602` bytes | `1,385,600` bytes | `8,726,090` chars |
| Encode (`5` iters) | `739ms` | `625ms` | `473ms` |
| Decode (`5` iters) | `747ms` | `716ms` | `915ms` |

Interpretation:

- JSON is still not the right runtime format because decode is much slower and
  the payload is much larger.
- Sharding is directionally better than one large blob, but only by a small
  margin on decode.
- So package-summary format work still looks incremental, not like the next
  big breakthrough.

### Remaining Module-Rule Hotspots

I patched `bench/module-rules-benchmark.mjs` to use the working fast
env-config path and re-ran isolated module rules.

Current isolated warm 1-file body-edit costs:

| Rule | `load_review_project` | `module_rule_eval` | wall |
|---|---:|---:|---:|
| `NoDebug.Log` | `188ms` | `180ms` | `1989ms` |
| `NoDebug.TodoOrToString` | `199ms` | `188ms` | `2073ms` |

So after the type-annotation work, the debug-family module rules are now the
next credible module-rule target.

### Generic Prep For Debug-Rule Shortcuts

I added `SemanticHash.extractDependenciesWithRanges` and changed
`CrossModuleSummary.dependencyRefs` to carry source ranges.

This is not a user-visible optimization by itself yet. The point is to unlock a
generic fact-based path for rules like:

- `NoDebug.Log`
- `NoDebug.TodoOrToString`

without introducing another fully ad hoc summary shape just for those rules.

### Visitor-Backed Debug Rules

I added the first explicit `VisitorContract` layer and used it to drive
host-backed upper-bound experiments for:

- `NoDebug.Log`
- `NoDebug.TodoOrToString`

The new path reuses `CrossModuleSummary.dependencyRefs` with ranges plus
module/import resolution helpers, instead of re-entering the interpreted
expression visitor.

#### Isolated module-rule result

| Rule | Mode | Cold | Warm | Warm 1-file body edit |
| --- | --- | ---: | ---: | ---: |
| `NoDebug.Log` | interpreted | `132.51s` | `0.54s` | `2.25s` |
| `NoDebug.Log` | host-backed | `36.53s` | `0.58s` | `2.19s` |
| `NoDebug.TodoOrToString` | interpreted | `131.84s` | `0.54s` | `2.25s` |
| `NoDebug.TodoOrToString` | host-backed | `36.47s` | `0.59s` | `2.21s` |

And the traced rule phase drops sharply:

| Rule | Mode | `module_rule_eval` on warm body edit |
| --- | --- | ---: |
| `NoDebug.Log` | interpreted | `198ms` |
| `NoDebug.Log` | host-backed | `8ms` |
| `NoDebug.TodoOrToString` | interpreted | `193ms` |
| `NoDebug.TodoOrToString` | host-backed | `7ms` |

So the visitor-backed seam is real for the isolated rules.

#### Mixed warm body-edit result

I then reran the reduced warmed-body probe on the full `small-12` config with:

- host-backed `NoUnused.*`
- host-backed type-annotation rules
- with and without host-backed debug rules

| Metric | No host debug | Host debug |
| --- | ---: | ---: |
| wall | `2411ms` | `2478ms` |
| `load_review_project` | `240ms` | `267ms` |
| `module_rule_eval` | `290ms` | `284ms` |
| `project_rule_eval` | `11ms` | `15ms` |

That is the important constraint: the visitor-backed debug shortcuts are
correct and valuable in isolation, but they do not currently move the mixed
warm body-edit path enough to justify treating them as the next primary
optimization lever.

Current interpretation:

- `VisitorContract` is the right architectural direction.
- Visitor-shaped cached inputs can collapse individual hot rules.
- The next big mixed-rule wins will come from the remaining shared overheads,
  not from simply adding more isolated host shortcuts one-by-one.

### Shape-Based Module Rules

I then targeted the two remaining shape-only rules directly:

- `NoExposingEverything`
- `NoImportingEverything`

These are simpler than the debug rules because error presence depends only on
already-cached shape facts:

- module `exposing (..)`
- import `exposing (..)`

The host-backed paths now use parser-derived facts from `CrossModuleSummary`
instead of string matching, so they are not fooled by comments/docs.

#### Isolated result

| Rule | Mode | Warm body edit wall | `module_rule_eval` |
| --- | --- | ---: | ---: |
| `NoExposingEverything` | interpreted | `2251ms` | `174ms` |
| `NoExposingEverything` | host-backed | `2224ms` | `12ms` |
| `NoImportingEverything` | interpreted | `2231ms` | `200ms` |
| `NoImportingEverything` | host-backed | `2306ms` | `10ms` |

So both shape-based rules collapse cleanly in isolation.

#### Mixed warmed body-edit probe

I reran the reduced warmed probe on the current source tree, with:

- host-backed `NoUnused.*`
- host-backed type-annotation rules
- host-backed debug rules
- with and without host-backed shape rules

| Metric | No host shape | Host shape |
| --- | ---: | ---: |
| wall | `2394ms` | `2380ms` |
| `load_review_project` | `229ms` | `240ms` |
| `module_rule_eval` | `260ms` | `202ms` |
| `project_rule_eval` | `14ms` | `11ms` |

Interpretation:

- the shape rules are worth keeping
- they remove about `58ms` from traced `module_rule_eval`
- but they only move wall time by about `14ms`, because shared overheads are
  now larger than those two individual rules

At this point the remaining interpreted module cost is effectively:

- `NoUnused.Patterns`

So the next optimization decision is now very focused:

1. eliminate `NoUnused.Patterns`
2. or switch attention back to `load_review_project`

### Package Summary Cache v6

I switched the package-summary cache format to stop storing `moduleKey`
strings inside imported-name mappings. That key is a pure function of
`moduleName`, so the old cache was carrying redundant data in every entry.

Implementation:

- bumped `packageSummaryCacheVersion` in [src/InterpreterProject.elm](src/InterpreterProject.elm)
- removed `moduleKey` from the serialized qualified-mapping payload
- reconstruct `moduleKey` during decode with `Environment.moduleKey`

This is a narrow cache-format optimization only. No runtime semantics changed.

#### Package-summary codec benchmark

Using `src/PackageSummaryCacheBenchmark.elm` after the cache-format change:

| Metric | Previous | Current |
| --- | ---: | ---: |
| binary bytes | `1,385,602` | `1,313,565` |
| sharded binary bytes | `1,385,600` | `1,313,563` |
| json chars | `8,726,090` | `8,537,727` |
| binary encode (`5` iters) | `739ms` | `723ms` |
| binary decode (`5` iters) | `747ms` | `741ms` |
| sharded binary encode (`5` iters) | `625ms` | `581ms` |
| sharded binary decode (`5` iters) | `716ms` | `705ms` |
| json encode (`5` iters) | `473ms` | `477ms` |
| json decode (`5` iters) | `915ms` | `933ms` |

Interpretation:

- The payload got materially smaller.
- Decode improved slightly, not dramatically.
- This is a real constant-factor improvement, but it is not the next
  breakthrough by itself.

#### `small-12` warm body-edit probe

After seeding a fresh workspace once, the reduced warmed body-edit probe
currently measures:

| Metric | Current |
| --- | ---: |
| wall | `2276ms` |
| `load_review_project` | `231ms` |
| `module_rule_eval` | `182ms` |
| `project_rule_eval` | `13ms` |
| `persist_decl_cache` | `12ms` |

The useful point here is that `load_review_project` is still one of the largest
remaining traced stages, and this cache-format change only trims it a bit.

### `elm-spa-example` spot check

I also ran a real-app comparison against `rtfeldman/elm-spa-example` (33 files).

Important caveat:

- the normal bundled `withCliOptions` entrypoint is currently broken for this repo
  under `elm-pages bundle-script`
- so these runner numbers use the working bundled fast entrypoint
  (`dist/review-runner-fast-bench.mjs`) rather than the normal flag-based CLI

Current numbers:

| Scenario | runner fast entrypoint | `elm-review` CLI |
| --- | ---: | ---: |
| warm, no changes | `300ms` | `436ms` |
| warm, 1-file body edit | `774ms` | `416ms` |

For the warm body-edit run on `elm-spa-example`, the runner trace showed:

| Stage | Time |
| --- | ---: |
| `load_review_project` | `214ms` |
| `module_rule_eval` | `7ms` |
| `project_rule_eval` | `8ms` |

Interpretation:

- On a real app, we already beat `elm-review` CLI on warm full-hit runs.
- On a trivial 1-file body edit, we are still behind.
- On that body-edit path, the remaining gap is mostly shared overhead and
  `load_review_project`, not project-rule work.

That keeps the priority order the same:

1. reduce `load_review_project`, especially package-summary decode and related
   review-project setup
2. avoid chasing low-leverage transport tweaks that do not reuse decoded data
3. only optimize individual remaining rules when a benchmark shows they still
   matter in the mixed path

### Package env-seed experiment

I also tried a more interpreter-ready package cache artifact: a direct
`PackageEnvSeed` containing:

- interfaces
- functions keyed by module
- imported names keyed by module

I benchmarked that artifact against the current summary blob and the sharded
summary blob.

| Metric | One blob | Env seed | Sharded blob |
| --- | ---: | ---: | ---: |
| bytes | `1,313,565` | `1,318,239` | `1,313,563` |
| encode (`5` iters) | `757ms` | `828ms` | `679ms` |
| decode (`5` iters) | `784ms` | `721ms` | `703ms` |

Interpretation:

- The env-seed artifact decodes faster than the current one-blob summary cache.
- But it is only modestly better, and still not better than sharded binary.

I then wired the env-seed artifact into `load_review_project` so warm runs could
decode it and build the package env directly. That did not produce an end-to-end
win on the warmed body-edit path:

| Metric | Runtime env-seed attempt |
| --- | ---: |
| wall | `2659ms` |
| `load_review_project` | `234ms` |
| `module_rule_eval` | `194ms` |
| `project_rule_eval` | `14ms` |

That is effectively flat to slightly worse than the previous warmed probe, so I
reverted the runtime integration and kept only the benchmark helpers.

Conclusion:

- `PackageEnvSeed` is a useful measurement artifact.
- It is not the next production cache boundary.
- The remaining opportunity inside `load_review_project` is not just “use a
  slightly different binary payload”; it likely needs a more structural change
  or a different source of overhead reduction.

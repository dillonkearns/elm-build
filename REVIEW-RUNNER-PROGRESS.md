# Review Runner Progress

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

### Takeaways

- The unfair-advantage warm path is now much closer to the target shape: `37.2s -> 3.74s` on the 12-file fixture for a real 1-file semantic change.
- Warm no-change stays sub-second and is slightly better after removing the unconditional review-app prepare step from the hot path.
- The new host analysis cache mostly attacks parse/AST overhead, so its biggest win showed up on cold and full-hit warm paths. The 1-file warm path improved modestly because it is now dominated more by interpreter/rule execution than host parsing.
- Cached-project reuse is not the next big win for narrowed `ImportersOf` / `DependenciesOf` project rules. The existing affected-subgraph strategy is much better there.
- Warm rule-cache reuse is also not paying for narrowed slices in this architecture: the rule-cache serialization/deserialization cost is higher than the skip benefit here.
- The single-rule timings show that fixed interpreter/evaluation overhead is now a major remaining cost. Batching multiple narrowed project-rule slices into one interpreter call helps even without a new cache.
- The real next performance step for 1-file warm runs is still rule-specific contribution caching, where a file change updates folded project-rule state instead of rerunning whole rule logic over any slice, or alternatively a daemon that keeps the interpreter state warm in memory.
- The next big opportunities are project-rule contribution caches, smaller AST transport than JSON, and eventually daemonized in-memory state.

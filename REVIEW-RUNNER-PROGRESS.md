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

### Takeaways

- The unfair-advantage warm path is now much closer to the target shape: `37.2s -> 3.74s` on the 12-file fixture for a real 1-file semantic change.
- Warm no-change stays sub-second and is slightly better after removing the unconditional review-app prepare step from the hot path.
- The new host analysis cache mostly attacks parse/AST overhead, so its biggest win showed up on cold and full-hit warm paths. The 1-file warm path improved modestly because it is now dominated more by interpreter/rule execution than host parsing.
- The next big opportunities are project-rule contribution caches, smaller AST transport than JSON, and eventually daemonized in-memory state.

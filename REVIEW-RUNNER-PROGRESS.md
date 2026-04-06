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

### Takeaways

- The unfair-advantage warm path is now much closer to the target shape: `37.2s -> 3.74s` on the 12-file fixture for a real 1-file semantic change.
- Warm no-change stays sub-second and is slightly better after removing the unconditional review-app prepare step from the hot path.
- Cold performance is still poor and regressed on this fixture. The next big opportunities are cold-path batching/caching, AST transport/caching, and possibly daemonized in-memory state.

# CPU profile analysis — post-Phase-2 / post-Phase-3-revert

**Captured:** 2026-04-13 18:18 (warm_import_graph_change) and 18:20 (cold)
**Commit:** parent `24a805c` (Phase 3 rejected), submodule `c988619` (Phase 2)
**Env-mode:** `resolved-list-unplanned`
**Fixture:** small-12
**Scenarios profiled:** cold + warm_import_graph_change
**Sampling interval:** 5ms

The `.cpuprofile` binaries are 200–500 MB each and not committed — they're
regenerable via `node bench/profile-cold.mjs --env-mode resolved-list-unplanned`
and `node bench/profile-warm-import-graph.mjs --env-mode resolved-list-unplanned`.

## Headline findings

### Warm_import_graph_change — NOT an interpreter bottleneck

Trace summary (internal 57.72s):

```
prepare_config:        0.02s
load_decl_cache:       0.01s
resolve_target_files:  0.00s
read_target_files:     0.01s
analyze_target_files:  0.07s
load_review_project:   0.39s
get_rule_info:         0.00s
module_rule_eval:      0.16s   ← negligible
project_rule_eval:    57.06s   ← 98.9% of internal
parse_review_output:   0.00s
update_decl_cache:     0.00s
persist_decl_cache:    0.00s
```

Within `project_rule_eval`, the counter breakdown is clearer still:

```
project.importers.eval_total_ms:       53952   (94.6% of internal)
project.importers.warm_eval_ms:        53928
project.importers_fold.eval_total_ms:   1559
project.importers_fold.warm_eval_ms:    1552
project.deps.eval_total_ms:             1522
project.deps.warm_eval_ms:              1512
```

**One review rule's importers-of computation accounts for ~94% of this
scenario's internal time.** The `warm_eval_ms` counter is almost exactly
equal to `eval_total_ms`, meaning the warm cache is **not short-circuiting
this run** — it's doing full re-evaluation despite the "warm" label.

### Cold — review-rule-dominated, fragmented interpreter work

Trace summary (internal 202.53s):

```
prepare_config:       0.16s
load_decl_cache:      0.00s
resolve_target_files: 0.00s
read_target_files:    0.00s
analyze_target_files: 0.43s
load_review_project:  1.59s
get_rule_info:        0.18s
module_rule_eval:    66.03s   (32.6%)
project_rule_eval:  134.04s   (66.2%)
parse_review_output:  0.00s
update_decl_cache:    0.00s
persist_decl_cache:   0.08s
```

98.8% of cold is rule evaluation (module + project). Every eval call routes
through the resolved-IR evaluator's hot paths, but no single hot path
dominates — see the self-time breakdown below.

## CPU self-time breakdown (cold, n=37402 samples)

```
rank  self_ms   self_%   function (minified bundle location)
   1   33283.2   14.5%  (garbage collector)
   2     757.1    0.3%  ur  (review-runner-bench.mjs:144)
   3     724.9    0.3%  ur  (review-runner-bench.mjs:144)
   4     621.8    0.3%  kr  (review-runner-bench.mjs:144)
   5     514.5    0.2%  kr  (review-runner-bench.mjs:144)
   6-30   <400ms <0.2%  kr/or/CW/ur at bundle lines 144/216/253
  16     384.2    0.2%  (idle)
```

Warm_import_graph_change is similar but with GC even higher at 27.2%
(15969 ms out of 58706 ms total).

### What this tells us

1. **GC is real but not extreme.** Cold's 14.5% is within typical range
   for an allocation-heavy interpreter workload; warm_import_graph_change's
   27.2% is higher and reflects the concentrated allocation of one specific
   rule evaluating in a tight loop.

2. **No single function is a hot spot.** The 29 top non-GC frames each
   take 0.1–0.3% of self-time, clustered at bundle lines `:144`, `:216`,
   `:253`, `:161`. These are minified locations that contain many original
   source functions — likely some mix of `List.foldl`, `Dict.get`,
   `applyClosure`, `evalR`, etc. Without source maps, we can't demangle.

3. **The inclusive stacks all route through the BackendTask scheduler.**
   The top inclusive frames (30-40% of samples each) go through
   `processTimers → listOnTimeout → vI/hN/r0 → (anonymous)`. This is
   `BackendTask.andThen` continuation flushing. It's expected — the
   interpreter is driven from the BackendTask orchestrator — but it's
   not actionable.

4. **Interpreter work is fragmented across the resolved IR's hot paths.**
   With no single function dominating, there's no obvious "fix this one
   thing" target. Any further interpreter optimization would have to
   be a broad allocation-reduction pass, and the bench signal would be
   small (each improvement competes with noise).

## Implications for Phase 4

The plan's Phase 4 was **batched pattern binding**: restructure `RLet` /
`RCase` handlers so they construct the new `locals` list in one batched
step rather than multiple sequential `cons` + env-record-rebuild calls.
Primary motivation: reduce allocation in a hot interpreter path.

**The profile doesn't clearly justify Phase 4.** Arguments:

- Cold GC is 14.5%. If Phase 4 eliminates ~10% of allocation (optimistic),
  that's a ~1.5% wall-clock cold win. Within noise for a single bench run.
- The allocation source is fragmented. Pattern binding is one of many
  allocators; others (closure creation, list cons, tuple allocation,
  record updates) would need separate passes to make a dent.
- Phase 4 would require touching the `matchPattern` logic at the four
  `RLet`/`RCase` sites in `ResolvedExpression.elm`, and any bugs in
  pattern match compilation silently corrupt eval results.

The profile *does* point at one valuable thing, but it's review-runner-level
not interpreter-level: **the `project.importers.warm_eval_ms` cache miss
on `warm_import_graph_change`**. That's ~54s of one rule doing full
re-evaluation when it should be hitting a warm cache. Fixing the cache
logic there would be a much bigger win than any interpreter optimization.
But it's out of scope for the slot-envs effort.

## Recommendation

**Ship the slot-envs effort as-is.** The headline result stands:

- Cold: legacy 280.52 → resolved 251.12 (**-10.5% real win**)
- Other scenarios: within noise or marginal regression

Phase 4 (batched pattern binding) is not justified by this profile. Phase 5
(final A/B + retirement decision) is the natural next step. After that,
the next large opportunity is either:

1. **Review-runner-level investigation** of the `importers.warm_eval_ms`
   cache miss. Out of scope for slot-envs but the single biggest win on
   `warm_import_graph_change`.

2. **Architectural items** from the original analysis: stable
   content-hashed `GlobalId` for cross-process caching, Unison-style test
   cache, query graph unification. Each is a multi-day focused effort.

3. **The four deferred `rRecThen` yield-threading holes** from Phase 1b
   (`RIf`/`RCase`/`RLet`/`RApply headExpr`). None of the 1400 existing
   tests exercise them, but they're known correctness hazards. ~30 LOC
   each, same fix pattern as `evalArgsStep`.

My ranking: **(3) correctness first, then (5) final A/B, then (2)
architectural choice.** (1) is out of scope for this effort but worth
flagging for a separate conversation.

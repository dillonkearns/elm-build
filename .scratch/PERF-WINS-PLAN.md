# Performance Optimization Plan for elm-build

## Context & Vision

elm-build has a pure Elm interpreter that evaluates Elm code without compiling to JS. Combined with content-addressed caching and Unison-style semantic hashing, it can serve as both a test runner and mutation testing tool. The goal: make it competitive with (or faster than) `elm-test` for incremental development — edit a function, see only affected tests re-run in milliseconds.

Elm's purity is our superpower. Same inputs = same outputs, guaranteed. No side effects, no hidden state. This means we can **provably skip unchanged computations** — not "probably," but provably. No other mainstream build system has this guarantee.

## What We've Built (This Session)

### Unison-Style Semantic Hashing (the biggest win so far)

**What**: Each function declaration gets a Merkle-style hash based on its AST structure + the semantic hashes of all functions it calls. Changes propagate only through actual dependency chains.

**How**: `SemanticHash.elm` — `buildMultiModuleIndex` takes all module sources, extracts declarations, topologically sorts by dependencies, computes hashes bottom-up. Package references hashed as `packageName/version.Module.function` (e.g., `elm/core/1.0.5.Basics.negate`).

**Impact**: MathLib warm cache went from 34ms/mutation to **2ms/mutation** (93% improvement). The cache key now reflects exactly which functions are transitively called, so unrelated changes don't bust the cache.

**Key files**: `src/SemanticHash.elm`, `src/SemanticHashTests.elm`, `src/InterpreterProject.elm` (lines 621-680)

**Research basis**: Unison language (https://www.unison-lang.org/docs/language-reference/hashes/). Each definition identified by SHA3 hash of its AST with names stripped and references replaced by dependency hashes. We adapted this for Elm: we don't strip names (Elm doesn't need alpha-equivalence at top level) but we DO replace cross-module function references with their semantic hashes.

### Coverage-Based Mutation Testing

**What**: Run tests once unmutated with interpreter tracing to collect expression-level coverage. Mutations in uncovered code are marked `NoCoverage` and skipped entirely.

**Impact**: MaybeTests: 36 of 85 mutations skipped (42%), saving ~42% evaluation time on cold runs.

**Key files**: `src/Coverage.elm`, `src/InterpreterProject.elm` (`evalWithCoverage`), `src/MutationTestRunner.elm` (coverage pass before mutation loop)

**Research basis**: Stryker's `perTest` mode, Google's coverage-based mutation testing. Our approach uses the interpreter's existing trace mechanism (CallTree) — no new data structures needed.

### Equivalent Mutation Detection (Early Cutoff)

**What**: Compare each mutation's test output against the unmutated baseline. If identical, the mutation is provably equivalent (Elm's purity guarantees this). Excluded from mutation score denominator.

**Impact**: MathLib: 6 mutations correctly identified as equivalent, score went from 88% to 100%. MaybeTests: 24 of 85 mutations are equivalent.

**Research basis**: Shake build system's "early cutoff" — if output unchanged despite input change, don't propagate invalidation. We're the only mutation testing tool that can provably detect equivalents (thanks to Elm purity).

### Direct AST Evaluation

**What**: Pass mutated `File` ASTs directly to the interpreter via `evalWithEnvFromFiles`, skipping the write→parse round-trip.

**Impact**: 25% faster warm cache. Eliminated `Elm.Writer` from the eval path entirely.

### Per-Entry Semantic Hash Cache Keys

**What**: Cache keys include only semantic hashes of declarations transitively referenced by the specific entry-point expression, not all declarations from all needed modules.

**Impact**: BasicsTests warm: 344ms → 128ms (63% faster). MaybeTests warm: 453ms → 210ms (54%).

## Current Performance Baselines

| Benchmark | Cold | Warm | Notes |
|-----------|------|------|-------|
| MathLib mutation (51 mut) | 2.3s, 45ms/mut | 0.13s, **2ms/mut** | Best case |
| core-extra BasicsTests mutation (49 mut) | 1.4s, 29ms/mut | 0.13s, **2ms/mut** | |
| core-extra MaybeTests mutation (85 mut) | 1.8s, 21ms/mut | 0.21s, **2ms/mut** | 36 NoCoverage skipped |
| core-extra BasicsTests test runner | 1.3s wall | 0.85s wall | 0.09s eval, rest is startup |
| elm-test full core-extra (622 tests) | 5.0s | 2.1s | Baseline comparison |
| elm-build per-module test (warm) | — | 0.85s wall | ~0.7s is bun startup |

### Wall Clock Breakdown (single warm test file)

```
0.40s  bun startup + loading 1.8MB bundled script (FIXED COST)
0.20s  project loading (loadWith, b3sum of source files, package env build)
0.09s  cached test evaluation (semantic hash computation + cache file read)
0.15s  other (dep fetch check, test discovery, output formatting)
─────
0.85s  total wall clock
```

## Remaining Optimization Opportunities

### Tier 1: High Impact, Achievable Now

#### 1. Fix Memory Leak (BLOCKER for large codebases)

See `.scratch/MEMORY-LEAK-PLAN.md` for full details. The `BackendTask.andThen` closure chain retains all intermediate state. 53 mutations → OOM. This MUST be fixed before any large-scale benchmarking.

**Approach**: `BackendTask.foldSequence` in elm-pages + `Cache.runWith` for shared listExisting.

#### 2. Single-Process Multi-File Test Runner

Currently running 7 test files = 7 bun invocations = 7 × 0.7s startup = 4.9s wasted. The auto-discover mode runs all files in one process (project loads once) but hangs on String/Unicode tests.

**Fix**: Skip test files that exceed a step limit (already partially done), or add a per-file timeout. With single-process, 7 modules would take ~1.5s instead of 5.9s.

#### 3. Eliminate b3sum Subprocess Calls

`Cache.inputs` spawns `b3sum` as a subprocess for file hashing. This is ~0.2s per invocation. Since we already have semantic hashing, we could compute file hashes in pure Elm (FNV1a is already used for the semantic hash cache key) and eliminate the subprocess entirely.

**Impact**: ~0.2s per invocation saved. For multi-file runs, this compounds.

### Tier 2: Medium Impact

#### 4. Incremental Semantic Index

Persist `DeclarationIndex` to disk between runs. On next run, check file mtimes, only reparse changed files, recompute semantic hashes only for changed declarations + their reverse deps.

**Impact**: Saves ~50-100ms of parsing on startup. Small win for individual runs, but compounds for watch mode.

#### 5. Pre-Parsed User Sources

`evalWithFileOverrides` re-parses user source strings (string → File) inside every `Cache.compute` cache miss. These are the same strings every time. Parse once during `loadWith`, store as `List File`, pass to eval.

**Impact**: Eliminates N × parse time on cold runs. Medium win.

#### 6. Incremental Env Building

`buildModuleEnv` runs for ALL user modules on every eval (even cached). Since only ONE module changes per mutation, build the base env once and only re-register the changed module.

**Impact**: O(1) module registrations per mutation instead of O(num user modules). Significant for projects with many source files.

### Tier 3: Larger Architectural Changes

#### 7. Mutant Schemata

Embed all mutations for a file in ONE AST with conditional dispatch (like Heretic's Clojure approach). Build env once, toggle mutations via runtime value.

**Impact**: Eliminates ALL per-mutation setup overhead. 10-100x potential speedup for cold runs.

**Research**: Untch/Offutt/Harrold 1993 "Mutation analysis using mutant schemata." Stryker.NET uses this. Heretic (Clojure) uses dynamic variables.

#### 8. Watch Mode

File watcher → incremental semantic index → identify affected tests → re-run only those → update cache.

**Impact**: Sub-second feedback loop. Edit a function, see results in <200ms.

#### 9. Remote/Shared Cache

Push semantic hash → result mappings to a shared store (S3, HTTP). Teams and CI share cached results.

**Why safe for elm-build**: Elm's purity + semantic hashing = cache is trustworthy by construction. No hermeticity issues (unlike Bazel where different compilers can poison the cache).

## Research & Prior Art Summary

### Unison (https://www.unison-lang.org/)
- Content-addressed code: every definition identified by hash of its AST + dependency hashes
- Test results cached permanently by hash — if hash unchanged, test doesn't re-run
- Cycles handled by hashing as a unit, members addressed by canonical index
- We implemented the core hashing idea in `SemanticHash.elm`

### Shake Build System (Neil Mitchell)
- "Early cutoff": if output unchanged despite input change, don't propagate
- We implemented this as equivalent mutation detection
- Verifying traces: record inputs used, validate on next run
- We went further with semantic hashing (purely static, no bootstrapping needed)

### Build Systems à la Carte (Mokhov, Mitchell, Peyton Jones)
- Taxonomy: verifying traces (Shake) vs constructive traces (Bazel) vs deep constructive (Nix)
- elm-build is closest to constructive traces with early cutoff
- Elm's purity means our traces are provably correct (unlike Shake which hedges against impurity)

### Bazel
- Content-addressed action cache: same inputs → same outputs → shareable
- Remote caching via HTTP API
- elm-build could adopt this for shared team/CI caching

### Salsa / rust-analyzer
- Demand-driven incremental computation
- Durability tiers: stable inputs (packages) skip revalidation
- We partially have this: packageEnv is built once, userSources are re-checked

### Google Mutation Testing at Scale
- 85% of surfaced mutants initially unproductive — we address this with NoCoverage + equivalent detection
- Coverage-based test selection is #1 optimization — implemented
- Arid node filtering — our `filterNoOps` is a basic version of this

## How to Benchmark

### Build the tools
```bash
cd /Users/dillonkearns/src/github.com/dillonkearns/elm-build
npx elm-pages bundle-script src/MutationTestRunner.elm --output dist/MutationTestRunner.mjs
npx elm-pages bundle-script src/TestRunner.elm --output dist/TestRunner.mjs
```

### Quick benchmarks (use these for iteration)
```bash
# MathLib mutations (51 mut, ~1s) — best for testing mutation runner changes
cd /Users/dillonkearns/src/github.com/dillonkearns/elm-build
chmod -R u+w .elm-mutation-test 2>/dev/null; rm -rf .elm-mutation-test
echo "COLD:" && /usr/bin/time -p bun dist/MutationTestRunner.mjs --mutate src/MathLib.elm --test src/MathLibTests.elm --break 0 2>&1 | grep -E "(Evaluated|real)"
echo "WARM:" && /usr/bin/time -p bun dist/MutationTestRunner.mjs --mutate src/MathLib.elm --test src/MathLibTests.elm --break 0 2>&1 | grep -E "(Evaluated|real)"

# BasicsTests (15 tests, ~1s) — best for testing test runner changes
cd /Users/dillonkearns/src/github.com/elmcraft/core-extra
chmod -R u+w .elm-build 2>/dev/null; rm -rf .elm-build
echo "COLD:" && /usr/bin/time -p bun /Users/dillonkearns/src/github.com/dillonkearns/elm-build/dist/TestRunner.mjs --test tests/BasicsTests.elm 2>&1 | grep -E "(Passed|Duration|real)"
echo "WARM:" && /usr/bin/time -p bun /Users/dillonkearns/src/github.com/dillonkearns/elm-build/dist/TestRunner.mjs --test tests/BasicsTests.elm 2>&1 | grep -E "(Passed|Duration|real)"
```

### Medium benchmarks
```bash
# core-extra MaybeTests (85 mutations, 36 NoCoverage) — good for coverage optimization testing
bun dist/MutationTestRunner.mjs --test tests/MaybeTests.elm --break 0

# Multiple test files sequentially — measures per-invocation overhead
for f in BasicsTests CharTests DictTests MaybeTests ResultTests SetTests TripleTests; do
  echo -n "$f: " && /usr/bin/time -p bun dist/TestRunner.mjs --test "tests/$f.elm" 2>&1 | grep -E "(Passed|real)" | tr '\n' ' ' && echo
done
```

### Memory profiling
```bash
# Cap memory to detect leaks
node --max-old-space-size=2048 dist/MutationTestRunner.mjs --mutate src/Format.elm --test tests/FormatTests.elm --break 0

# Peak RSS on macOS
/usr/bin/time -l bun dist/MutationTestRunner.mjs --mutate src/MathLib.elm --test src/MathLibTests.elm --break 0 2>&1 | grep "maximum resident"
```

### elm-test comparison (the north star)
```bash
cd /Users/dillonkearns/src/github.com/elmcraft/core-extra
echo "elm-test:" && /usr/bin/time -p npx elm-test 2>&1 | grep -E "(Duration|Passed|real)"
```

### IMPORTANT: Cache cleanup
Cache directories use read-only permissions. MUST chmod before rm:
```bash
chmod -R u+w .elm-build 2>/dev/null; rm -rf .elm-build
chmod -R u+w .elm-mutation-test 2>/dev/null; rm -rf .elm-mutation-test
```

### What NOT to benchmark
- Don't run full core-extra auto-discover (hangs on String/Unicode tests, 10+ min)
- Don't benchmark `npx elm-pages bundle-script` build time (it's a build step, not runtime)
- Don't benchmark with stale cache from before code changes (clear cache first)

## Success Criteria

### Short-term (memory leak fix)
- elm-ical Format.elm (53 mutations) completes without OOM at 2GB heap limit
- MathLib warm performance unchanged (2ms/mutation)

### Medium-term (test runner competitive with elm-test)
- 7 core-extra test modules in single process: <2s wall clock (vs 5.9s now)
- Warm re-run after editing one function: <0.5s (vs 0.85s now)

### Long-term (faster than elm-test for incremental)
- Edit one function, re-run tests: <200ms (only affected tests run)
- Full mutation analysis on 50-function module: <30s cold, <5s warm
- elm-test comparison: elm-build matches or beats elm-test warm time for partial re-runs

## Test Suite Status

115 elm-build tests passing. Key test modules:
- `SemanticHashTests.elm` (14 tests): AST hashing, dependency extraction, Merkle computation, cross-module resolution
- `CoverageTests.elm` (6 tests): range containment, coverage detection
- `MutatorTests.elm` (many): all mutation operators + no-op filter + multi-line exposing
- `DepGraphTests.elm` (6 tests): sourcesTestedBy, transitive deps
- `MutationReportTests.elm` (9 tests): Stryker schema JSON encoding

496 core-extra tests passing through the interpreter (80% of elm-test's 622).

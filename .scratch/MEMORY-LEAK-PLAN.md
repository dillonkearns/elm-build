# Memory Leak Investigation & Fix Plan

## Symptom

Running mutation testing on real codebases (53+ mutations) causes OOM crashes. Memory grows linearly at ~700MB-1.2GB/min. GC reports `mu = 0.001` (essentially 0% freed). With 16GB heap limit, crashes after ~49 minutes.

## Architecture: How Mutations Are Evaluated

```
MutationTestRunner.task
  → coveredMutations |> List.map (\mutation -> Cache.run ... ) |> BackendTask.Extra.sequence
```

For each mutation:
1. `Cache.run { jobs = Nothing } config.buildDirectory (InterpreterProject.evalWithFileOverrides ...)`
2. Inside `Cache.run`: calls `listExisting buildPath` (reads dir listing), creates `Input`, runs `runMonad`
3. Inside `evalWithFileOverrides`: computes semantic cache key, calls `Cache.compute`
4. Inside `Cache.compute`: calls `derive` which either returns cached hash or runs the interpreter
5. Result: `BackendTask FatalError { output : Path, dependencies : List Path }`

All 53 results are accumulated via `BackendTask.Extra.sequence` → `BackendTask.sequence` → chained `andThen` calls.

## Root Causes (Multiple, Compounding)

### Cause 1: `Cache.run` calls `listExisting` on every mutation

**File**: `src/Cache.elm`, line 590

```elm
run config buildPath m =
    Do.do (listExisting buildPath) <| \existing ->
    ...
```

`listExisting` calls `BackendTask.Custom.run "readdir"` which reads the entire cache directory listing. For 53 mutations, this is called 53 times. As the cache grows (each mutation adds entries), the directory listing gets larger. Each listing is retained in the `andThen` closure chain.

**Fix**: Call `listExisting` ONCE before the mutation loop, pass the result through. Or make `Cache.run` accept a pre-computed `existing` set.

### Cause 2: `BackendTask.Extra.sequence` retains all closures

**File**: `src/BackendTask/Extra.elm`, lines 161-196

```elm
sequence inputs =
    ...
    arr |> Array.slice ... |> BackendTask.sequence |> BackendTask.map Rope.fromList
```

`BackendTask.sequence` (the built-in one from elm-pages) chains `andThen` calls. Each `andThen` creates a closure that captures the previous result AND the continuation. For 53 mutations, this creates a chain of 53 closures, none of which can be GC'd until the entire chain completes.

The closure for mutation N retains:
- The `InterpreterProject` (full project with all parsed sources, package env, semantic index)
- The `mutation.mutatedFile` (full AST)
- The `Cache.Monad` state (`HashSet` of dependencies)
- The result of the interpreter evaluation
- All intermediate BackendTask state

**Fix**: Use `BackendTask.sequence` with a streaming/folding pattern that discards intermediate state. Or process each mutation result immediately instead of accumulating all results.

### Cause 3: Each `Cache.run` creates subprocess overhead

Each `Cache.run` → `derive` → `execLog` chain spawns multiple subprocesses:
- `b3sum` for input hashing (via `Cache.inputs` during `loadWith`)
- `cp` for caching input files
- `mkdir -p` for cache directories
- `chmod -R` for setting permissions
- `mv` for atomic cache entry creation

For 53 mutations with warm cache (all hits), most of these are skipped. But for cold runs, each mutation creates ~5-10 subprocess calls, and their file descriptors / buffers may be retained.

### Cause 4: `InterpreterProject` is captured in every closure

The `project` value (type `InterpreterProject`) contains:
- `patchedPackageSources : List String` — ALL package source code as strings
- `userFileContents : Dict String String` — all user source files
- `moduleGraph : ModuleGraph` — `Dict String String` of all module sources
- `packageEnv : Eval.Module.ProjectEnv` — the full parsed package environment
- `semanticIndex : SemanticHash.DeclarationIndex` — hash index

This is a large data structure. It's referenced from every mutation's `andThen` closure because the mutation lambda captures `project` from the outer scope. Even though it's the SAME reference, JS engines may retain multiple references through the closure chain.

### Cause 5: `mutation.mutatedFile` retains full AST per mutation

Each mutation stores `mutatedFile : File` — a complete copy of the source file's AST with one expression replaced. For 53 mutations of a single file, that's 53 copies of the full AST. These are all constructed upfront by `Mutator.generateMutations` and retained in the `coveredMutations` list throughout the entire mutation loop.

## Proposed Fixes (Priority Order)

### Fix 1: Process mutations one-at-a-time (Highest Impact)

Instead of:
```elm
coveredMutations
    |> List.map (\mutation -> Cache.run ... )
    |> BackendTask.Extra.sequence  -- accumulates ALL results
```

Do:
```elm
coveredMutations
    |> List.foldl
        (\mutation accTask ->
            accTask |> BackendTask.andThen (\accResults ->
                Cache.run ... |> BackendTask.map (\result ->
                    -- Process result immediately (log, write to report)
                    -- Only keep the summary (Killed/Survived/etc), not the full data
                    parseMutationResult ... :: accResults
                )
            )
        )
        (BackendTask.succeed [])
```

This processes each mutation and immediately discards the heavy data (mutated AST, cache state). Only the lightweight `MutationResult` is kept.

**But**: This still chains `andThen` calls. The deeper fix needs elm-pages support.

### Fix 2: `Cache.run` with shared `existing` set

Add a variant that accepts a pre-computed `existing : HashSet`:

```elm
runWith : { jobs : Maybe Int, existing : HashSet } -> Path -> Monad FileOrDirectory -> BackendTask FatalError { output : Path, dependencies : List Path }
```

Call `listExisting` ONCE in `MutationTestRunner.task`, pass the result to all mutation evals. This eliminates 52 redundant `readdir` calls and their retained data.

### Fix 3: Lazy mutation generation

Instead of generating all 53 `mutatedFile` ASTs upfront (53 full AST copies), generate each mutation's AST lazily — only when that mutation is about to be evaluated:

```elm
type alias LazyMutation =
    { line : Int
    , column : Int
    , operator : String
    , description : String
    , generateMutatedFile : () -> File  -- lazy
    , spliceRange : Range
    , spliceText : String
    }
```

This avoids retaining 53 AST copies simultaneously.

### Fix 4: Incremental report writing

Write each mutation result to the `--report` JSON file as it's evaluated, not at the end. This way:
- Progress is visible immediately
- If OOM occurs, partial results are saved
- Stdout can be flushed per mutation

### Fix 5: elm-pages BackendTask streaming (Deepest Fix)

The fundamental issue is that `BackendTask.andThen` creates closures that retain all previous state. A streaming/iterative execution model would allow the runtime to process one BackendTask at a time without retaining the chain.

This could be a new primitive in elm-pages:
```elm
BackendTask.foldSequence : (a -> b -> BackendTask error b) -> b -> List (BackendTask error a) -> BackendTask error b
```

Where each step's BackendTask is resolved and its data is reduced into the accumulator `b`, allowing the runtime to drop the step's full data.

## Files to Modify

- `src/MutationTestRunner.elm` (lines 328-380): Mutation loop — restructure to process one at a time
- `src/Cache.elm` (line 588-609): Add `runWith` variant accepting pre-computed `existing`
- `src/Mutator.elm` (lines 23-31): Consider lazy `mutatedFile` generation
- `src/MutationReport.elm`: Add incremental write support
- elm-pages `BackendTask` module: Consider adding `foldSequence` primitive

## Testing

1. Run against elm-ical `src/Format.elm` (53 mutations) — should complete without OOM
2. Monitor memory with `node --max-old-space-size=2048` — should stay under 2GB
3. Verify mutation results are identical to small-batch runs
4. Check that `--report` file is written incrementally

## Quick Win vs Deep Fix

**Quick win (Fix 1 + 2 + 4)**: Restructure mutation loop to process one mutation at a time with shared `existing` set and incremental reporting. This reduces peak memory from O(N × AST_size) to O(1 × AST_size). Estimated effort: ~1 hour.

**Deep fix (Fix 5)**: Add `BackendTask.foldSequence` to elm-pages. This fixes the underlying issue for ALL elm-pages users who chain many BackendTasks. Estimated effort: ~4 hours (elm-pages core change).

## Notes on Elm/JS Memory Model

Elm compiles to JS. The `andThen` chain creates nested closures:
```js
// Simplified: what BackendTask.sequence produces
andThen(result1 => 
    andThen(result2 =>
        andThen(result3 =>
            // ... 53 levels deep
            // result1, result2, result3 are all retained by their enclosing closures
        )
    )
)
```

V8's GC cannot collect `result1` until the innermost callback completes, because each closure captures its enclosing scope. This is the classic "closure chain retention" problem in JS.

The fix is to break the chain: process each result immediately, extract only the summary data, and let the heavy data (AST, cache state) be GC'd before starting the next mutation.

---

## Benchmarking Guide

### How to build

The mutation test runner and test runner are elm-pages scripts that get bundled into standalone .mjs files:

```bash
cd /Users/dillonkearns/src/github.com/dillonkearns/elm-build

# Build mutation test runner
npx elm-pages bundle-script src/MutationTestRunner.elm --output dist/MutationTestRunner.mjs

# Build test runner (elm-test replacement)
npx elm-pages bundle-script src/TestRunner.elm --output dist/TestRunner.mjs
```

Both produce self-contained files (~1.8MB) that can be run with `bun` or `node` in any Elm project directory.

### Running the tools

```bash
cd /path/to/target/elm/project

# Mutation test runner
bun /path/to/elm-build/dist/MutationTestRunner.mjs --break 0
bun /path/to/elm-build/dist/MutationTestRunner.mjs --test tests/MyTests.elm --break 0
bun /path/to/elm-build/dist/MutationTestRunner.mjs --mutate src/MyModule.elm --test tests/MyTests.elm --break 0
bun /path/to/elm-build/dist/MutationTestRunner.mjs --test tests/MyTests.elm --break 0 --report reports/mutation.json

# Test runner (elm-test replacement)
bun /path/to/elm-build/dist/TestRunner.mjs
bun /path/to/elm-build/dist/TestRunner.mjs --test tests/MyTests.elm
```

### Benchmark targets

**Small/fast (use for quick iteration):**
```bash
# elm-build's own MathLib (51 mutations, 15 tests) — ~1s per run
cd /Users/dillonkearns/src/github.com/dillonkearns/elm-build
bun dist/MutationTestRunner.mjs --mutate src/MathLib.elm --test src/MathLibTests.elm --break 0

# core-extra BasicsTests (49 mutations, 15 tests) — ~1s per run
cd /Users/dillonkearns/src/github.com/elmcraft/core-extra
bun /Users/dillonkearns/src/github.com/dillonkearns/elm-build/dist/MutationTestRunner.mjs --test tests/BasicsTests.elm --break 0
```

**Medium (good signal without waiting forever):**
```bash
# core-extra MaybeTests (85 mutations, 30 tests, 36 NoCoverage) — ~2s warm
bun dist/MutationTestRunner.mjs --test tests/MaybeTests.elm --break 0

# core-extra ResultTests (test runner only, 69 tests) — ~1s
bun dist/TestRunner.mjs --test tests/ResultTests.elm
```

**Large (use sparingly — hangs on String/Unicode tests):**
```bash
# core-extra auto-discover ALL tests — WILL HANG on String/Unicode modules
# Don't run this without a timeout. Instead, run individual fast modules:
for f in BasicsTests CharTests DictTests MaybeTests ResultTests SetTests TripleTests; do
  bun dist/TestRunner.mjs --test "tests/$f.elm"
done
```

**Real-world mutation test (the OOM case):**
```bash
# elm-ical Format.elm — 53 mutations, triggers OOM without the fix
cd /Users/dillonkearns/src/github.com/dillonkearns/elm-ical
bun /path/to/dist/MutationTestRunner.mjs --mutate src/Format.elm --test tests/FormatTests.elm --break 0
```

### Cold vs warm runs

**Cold** = no `.elm-build` or `.elm-mutation-test` cache directory. Everything is computed from scratch.
**Warm** = cache exists from previous run. Unchanged computations return cached results instantly.

```bash
# Cold run: clear cache first
chmod -R u+w .elm-build 2>/dev/null; rm -rf .elm-build   # note: chmod needed because cache files are read-only
chmod -R u+w .elm-mutation-test 2>/dev/null; rm -rf .elm-mutation-test

# Then run the tool — this is a cold run

# Run again without clearing — this is a warm run
```

**Key insight**: The cache uses read-only file permissions (content-addressable store pattern). You MUST `chmod -R u+w` before `rm -rf` or it will fail silently, leaving stale cache that confuses benchmarks.

### What to measure

1. **Wall clock time** — `/usr/bin/time -p bun ... 2>&1 | grep real`
2. **Test Duration** — reported by the tool itself (eval time only, excludes startup)
3. **Memory** — `node --max-old-space-size=2048 dist/TestRunner.mjs` to cap and detect leaks
4. **Cache hit rate** — compare cold vs warm times. If warm ≈ cold, caching isn't working.
5. **Mutations evaluated** — compare total vs NoCoverage (skipped) vs Equivalent

### Current performance baselines (as of this session)

| Benchmark | Cold | Warm |
|-----------|------|------|
| MathLib mutation (51 mut) | 2.3s, 45ms/mut | 0.13s, **2ms/mut** |
| core-extra BasicsTests mutation (49 mut) | 1.4s, 29ms/mut | 0.13s, **2ms/mut** |
| core-extra BasicsTests test runner | 1.3s wall | 0.85s wall (0.09s eval) |
| elm-test full core-extra (622 tests) | 5.0s cold / 2.1s warm | — |
| elm-build individual module test | 1.1s cold / 0.85s warm | per file, ~0.7s is bun startup |

### Performance bottleneck breakdown

For a single test file run (warm cache):
- **~0.4s**: bun startup + loading 1.8MB bundled script
- **~0.2s**: project loading (loadWith, b3sum of source files, package env)
- **~0.09s**: cached test evaluation (semantic hash key computation + cache file read)
- **~0.15s**: other overhead (dep fetching check, test discovery, output formatting)

The **bun startup** is the single biggest cost and is outside our control. For multi-file runs, the project loads once so the amortized cost per test file is just the ~0.09s eval.

### Known issues affecting benchmarks

1. **String/Unicode tests hang** — `String.RemoveAccentsTest`, `String.RemoveDiacriticsTests`, `String.UnicodeTests` process huge character mapping tables that exceed the 5M step limit or take >5 minutes. Skip these for benchmarking.

2. **Cache permissions** — `.elm-build` uses read-only files. Use `chmod -R u+w .elm-build && rm -rf .elm-build` to clear.

3. **Stale cache format** — if you change the semantic hashing logic, old cache entries have incompatible hash keys. Clear the cache before benchmarking.

4. **elm-pages compilation** — `npx elm-pages bundle-script` recompiles if source changed. The bundle step itself takes ~5-10s but is NOT part of the benchmark (it's a build step, not runtime).

5. **Memory leak** — runs with 50+ mutations will eventually OOM. For benchmarking, use small targets (MathLib, BasicsTests) or add `--max-old-space-size=4096` for medium runs.

### Comparing with elm-test

elm-test compiles ALL test files to JS once (via `elm make`), then runs them in Node. elm-build interprets each test file individually via the Elm interpreter. The comparison:

```bash
# elm-test (baseline — all 622 tests)
cd /Users/dillonkearns/src/github.com/elmcraft/core-extra
/usr/bin/time -p npx elm-test 2>&1 | grep -E "(Duration|Passed|real)"

# elm-build (individual modules)
for f in BasicsTests CharTests DictTests MaybeTests ResultTests SetTests TripleTests; do
  echo -n "$f: "
  /usr/bin/time -p bun /path/to/dist/TestRunner.mjs --test "tests/$f.elm" 2>&1 | grep -E "(Passed|real)" | tr '\n' ' '
  echo
done
```

### Other feedback items from .scratch/elm-mutant-feedback.md (not yet fixed)

- **"1 tests" problem**: Per-test coverage only discovers top-level exposed values (like `suite`), not individual `test` calls within them. This means per-test optimization doesn't work for single-suite modules.
- **"using first"**: Auto-discover mode picks only the first test file when `--mutate` is not specified.
- **Transitive imports**: `findTestFilesImporting` only checks direct imports, not transitive. Internal helper modules can't be mutated without `--test`.
- **Read-only cache**: No `--clean` flag. Must manually `chmod -R u+w && rm -rf`.

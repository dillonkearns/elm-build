# Memory Leak Fix Plan — TDD + Benchmarking

## Problem

Running mutation testing on real codebases (53+ mutations) causes OOM crashes. Memory grows
at ~700MB-1.2GB/min. V8 GC reports mu=0.001 (0% freed). With 16GB heap, crashes after ~49min.

## Root Cause Analysis (Confirmed via Code Review)

Three compounding causes, ordered by impact:

### 1. BackendTask.andThen closure chain retains all intermediate state (CRITICAL)

`BackendTask.sequence` uses `List.foldl andThen`, creating N nested closures. Each closure
captures the enclosing scope, so V8 cannot GC any intermediate result until the entire chain
completes. For 53 mutations, this means 53 interpreter evaluations (each producing ASTs,
Env bindings, cache state) all live simultaneously.

**Where**: elm-pages `BackendTask.elm:494-519` (andThen), `BackendTask.elm:392-405` (sequence)
**Used by**: `MutationTestRunner.elm:406` via `BackendTask.Extra.sequence`

### 2. Cache.run calls listExisting (readdir) on every mutation (MEDIUM)

`Cache.run` calls `listExisting buildPath` which does a full `readdir` of the cache directory.
For 53 mutations, that's 53 readdir calls, each returning a growing list of cache entries.
Each result is retained in the andThen closure chain.

**Where**: `Cache.elm:588-609`

### 3. All Mutation ASTs generated upfront (MEDIUM)

`Mutator.generateMutations` creates 53 full `File` AST copies immediately. All are held in
memory for the entire mutation loop, even though they're processed one at a time.

**Where**: `MutationTestRunner.elm:309-310`, `Mutator.elm:23-31`

## Fix Strategy

Each fix follows strict red-green-refactor TDD and is validated by benchmarking.

---

### Phase 0: Establish Baseline Benchmarks

**Before touching any code**, capture reproducible baseline numbers.

Benchmarks to run:
1. **elm-build MathLib** (51 mutations, fast): cold + warm wall time, peak RSS
2. **elm-ical Format.elm** (53 mutations, the OOM case): time-to-OOM or completion, peak RSS
3. **core-extra BasicsTests** (49 mutations): cold + warm

Measurement method:
```bash
# Peak RSS via /usr/bin/time -l (macOS)
/usr/bin/time -l bun dist/MutationTestRunner.mjs --mutate src/MathLib.elm --test src/MathLibTests.elm --break 0 2>&1

# For OOM case: cap memory and measure
node --max-old-space-size=2048 dist/MutationTestRunner.mjs --mutate src/Format.elm --test tests/FormatTests.elm --break 0
```

Record in `.scratch/benchmarks.md`.

---

### Phase 1: Cache.runWith — shared listExisting (Quick Win)

**Why first**: Smallest change, most isolated, easy to TDD. Removes 52 redundant readdir calls.

#### RED: Write failing test

Add a test in `src/CacheTests.elm` (or equivalent) that verifies `Cache.runWith` accepts
a pre-computed `existing` set and produces the same result as `Cache.run`.

More concretely: since Cache is tested via integration (BackendTask execution), the test is:
- Run `Cache.listExisting` once
- Run `Cache.runWith existing ...` with that result
- Assert same output as `Cache.run ...`

#### GREEN: Implement Cache.runWith

```elm
runWith : { jobs : Maybe Int, existing : HashSet } -> Path -> Monad FileOrDirectory -> BackendTask FatalError { output : Path, dependencies : List Path }
runWith config buildPath m =
    let
        input_ : Input
        input_ =
            { existing = config.existing
            , prefix = []
            , buildPath = buildPath
            , jobs = config.jobs
            }
    in
    runMonad m input_ hashSetEmpty
        |> BackendTask.map (\( output, deps ) -> ...)
```

Also expose `listExisting` from Cache module.

#### REFACTOR: Wire into MutationTestRunner

In `MutationTestRunner.task`, call `Cache.listExisting` once before the mutation loop,
pass the result to `Cache.runWith` for each mutation.

#### BENCHMARK: Verify improvement

Re-run MathLib benchmark. Expect: warm time unchanged (readdir was fast), cold time slightly
improved. Memory: small reduction from not retaining 52 readdir results in closure chain.

---

### Phase 2: BackendTask.foldSequence — break the closure chain (Critical Fix)

**Why**: This is the root cause. Without this, fixes 1 and 3 only delay the OOM.

#### RED: Write failing test in elm-pages

Add test for new `BackendTask.foldSequence`:
```elm
test "foldSequence processes items without retaining intermediate state" <|
    \_ ->
        BackendTask.foldSequence
            (\item acc -> BackendTask.succeed (item + acc))
            0
            (List.range 1 100 |> List.map BackendTask.succeed)
        |> expectBackendTask (Expect.equal 5050)
```

Also test that it produces same results as `sequence |> map (List.foldl ...)`.

#### GREEN: Implement BackendTask.foldSequence in elm-pages

Two options explored:

**Option A: Elm-level fold with forced evaluation** (simpler, may not fully fix GC)
```elm
foldSequence : (a -> b -> BackendTask error b) -> b -> List (BackendTask error a) -> BackendTask error b
foldSequence step init items =
    case items of
        [] -> succeed init
        first :: rest ->
            first |> andThen (\a -> step a init |> andThen (\newAcc -> foldSequence step newAcc rest))
```

This still uses andThen but the key difference: each step's result is reduced into the
accumulator immediately, so the heavy `a` value is not retained — only the lightweight `b`.

**Option B: New ADT variant in BackendTask** (deeper, guaranteed fix)
Add a `FoldStep` variant to `RawRequest` that the runtime processes iteratively without
building nested closures. The runtime loop would:
1. Evaluate item N
2. Call step function with result + accumulator
3. Drop item N's data
4. Proceed to item N+1

Option A is sufficient if the step function doesn't capture heavy data in its closure.
For mutation testing, the step function would extract just the mutation result status
(Killed/Survived/etc.) and discard the full interpreter output.

#### REFACTOR: Use foldSequence in MutationTestRunner

Replace:
```elm
coveredMutations
    |> List.indexedMap (\i m -> evaluateMutation ...)
    |> BackendTask.Extra.sequence
```

With:
```elm
coveredMutations
    |> List.indexedMap Tuple.pair
    |> BackendTask.foldSequence
        (\(i, mutation) results ->
            evaluateMutation mutation
                |> BackendTask.map (\r -> r :: results)
        )
        []
```

Where `evaluateMutation` returns a lightweight `MutationResult` (just status + location),
not the full interpreter output.

#### BENCHMARK: Critical validation

- **MathLib**: Should see minimal change (already fast)
- **elm-ical Format.elm**: Should complete without OOM at 2GB heap limit
- **Memory profile**: `node --max-old-space-size=2048` should stay under 2GB for 53 mutations

---

### Phase 3: Lazy Mutation Generation (Memory Reduction)

**Why**: Avoids holding 53 full AST copies simultaneously.

#### RED: Write failing test

Test that `Mutator.generateLazyMutations` returns mutation descriptors without the full AST,
and that calling `mutation.generateMutatedFile()` produces the same File as the eager version.

```elm
test "lazy mutation produces same AST as eager" <|
    \_ ->
        let
            source = "module Foo exposing (..)\n\nfoo = 1 + 2"
            eager = Mutator.generateMutations source
            lazy = Mutator.generateLazyMutations source
        in
        List.map2
            (\e l ->
                Expect.equal e.mutatedFile (l.generateMutatedFile ())
            )
            eager lazy
            |> Expect.all
```

#### GREEN: Implement lazy mutations

Change `Mutation` to store the original file + splice info instead of the mutated AST:
```elm
type alias Mutation =
    { line : Int
    , column : Int
    , operator : String
    , description : String
    , spliceRange : Range
    , spliceText : String
    , originalFile : File          -- shared reference (1 copy)
    , applyMutation : () -> File   -- lazy, creates AST on demand
    }
```

All 53 mutations share the same `originalFile` reference. Each `applyMutation` is a thunk
that splices in the mutation when called.

#### BENCHMARK: Measure reduction

For 53 mutations of a 136-line file, this changes from 53 AST copies (~53 * AST_size) to
1 AST copy + 53 lightweight descriptors. Expect ~50x reduction in mutation-related memory.

---

### Phase 4: Incremental Report Writing (UX + Crash Resilience)

#### RED: Write failing test

Test that `MutationReport.writeIncremental` appends a single mutation result to the report
file, and that reading the file after N incremental writes produces valid JSON matching
the batch `toJson` output.

#### GREEN: Implement incremental writing

- After each mutation result, append to the report file
- Use JSONL (one JSON object per line) for append-friendliness
- Add a finalization step that writes the proper Stryker-format JSON from accumulated JSONL

#### BENCHMARK: Verify no regression

Wall time should not increase meaningfully (file write per mutation is ~1ms).

---

### Phase 5: Additional Feedback Items (Post-Memory-Fix)

These are from elm-mutant-feedback.md but not memory-related:

1. **Per-test granularity** ("1 tests" problem): Discover individual `test` calls within
   `suite`, not just top-level exposed values. Requires deeper AST analysis in TestAnalysis.
2. **Transitive import discovery**: `findTestFilesImporting` should walk the dep graph
   transitively, not just check direct imports.
3. **Multi-test-file support**: Remove "(using first)" behavior; run all discovered test files.
4. **Cache cleanup**: Add `--clean` flag or don't use read-only permissions.

These are important but orthogonal to the memory fix. Tackle after Phase 2 is validated.

---

## Files to Modify (by phase)

| Phase | Repo | Files |
|-------|------|-------|
| 1 | elm-build | `src/Cache.elm`, `src/MutationTestRunner.elm` |
| 2 | elm-pages | `src/BackendTask.elm` or `src/BackendTask/Extra.elm` |
| 2 | elm-build | `src/MutationTestRunner.elm`, `src/BackendTask/Extra.elm` |
| 3 | elm-build | `src/Mutator.elm`, `src/MutationTestRunner.elm` |
| 4 | elm-build | `src/MutationReport.elm`, `src/MutationTestRunner.elm` |

## Success Criteria

1. `elm-ical Format.elm` (53 mutations) completes without OOM at `--max-old-space-size=2048`
2. Peak RSS stays under 2GB for 53-mutation runs
3. MathLib warm benchmark stays at or below current 0.13s
4. All existing tests pass
5. Partial results survive OOM crashes (Phase 4)

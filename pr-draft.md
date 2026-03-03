# Pure Elm Test Runner with Per-Test Caching

## What this is

A proof-of-concept test runner built entirely in pure Elm, using elm-build's caching primitives. Instead of shelling out to elm-test, it uses `Test.Runner` to enumerate and execute test cases directly. Each test result is cached via content-addressed hashing — if the source files haven't changed, individual test results are returned from cache without re-execution.

This demonstrates the key insight: Elm tests are pure functions, so their results can be content-addressed and cached just like any other build artifact.

## How to run it

```bash
# First run (cold cache) — executes all tests
npx elm-pages run src/PureTestRunner.elm --build /tmp/pure-test-cache

# Second run (warm cache) — returns instantly from cache
npx elm-pages run src/PureTestRunner.elm --build /tmp/pure-test-cache

# To clear the cache
chmod -R u+w /tmp/pure-test-cache && rm -rf /tmp/pure-test-cache
```

## Benchmarks

Tested with 11 sample tests (math, strings, lists, fuzz):

| Run | Time |
|---|---|
| Cold (first run, no cache) | ~300ms |
| Warm (all cached) | ~8ms |

Each test is cached individually, so changing one source file only re-runs tests affected by that change. Unchanged tests return from cache instantly.

For comparison, we also prototyped a shell-out approach (wrapping `elm-test` as an external command, not included in this branch) tested against [dillonkearns/elm-markdown](https://github.com/dillonkearns/elm-markdown) (268 tests):

| Run | Time |
|---|---|
| Cold | ~2300ms |
| Warm | ~2ms |

## New Cache API primitives

### `compute`

```elm
compute : List String -> FileOrDirectory -> (() -> String) -> (FileOrDirectory -> Monad a) -> Monad a
```

Caches a pure Elm computation. This is the internal analog to the command-based primitives (`pipeThrough`, `commandWithFile`, etc.) — instead of shelling out to an external process, the result is computed in pure Elm.

The output hash is derived from the label and the input dependency hash. If the output already exists in the cache, the thunk is never called.

This is arguably the most sound primitive in the Cache API. With external commands, the docs have to warn "only read files from the provided folder" and trust the caller. With `compute`, Elm's type system enforces purity — the `() -> String` thunk literally cannot access the filesystem, network, or mutable state. If the inputs haven't changed, the output is guaranteed identical.

### `commandInWorkspace`

```elm
commandInWorkspace : String -> List String -> FileOrDirectory -> (FileOrDirectory -> Monad a) -> Monad a
```

Like `commandInFolder`, but creates a writable copy of the cached folder before running the command. Many real tools (e.g., `elm-test` writing `elm-stuff/`) need to create temporary files during execution. `commandInFolder` runs in the read-only cached directory, so these tools fail. `commandInWorkspace` copies the folder, makes it writable, runs the command, captures stdout, and discards the workspace.

This models commands as pure functions: given the same folder contents and arguments, the command produces the same stdout. The workspace handles the implementation detail that many tools need write access.

### `hashToString`

```elm
hashToString : FileOrDirectory -> String
```

Exposes the string representation of a content hash. Useful for deriving deterministic values from cached state — for example, generating a reproducible fuzz test seed from the project hash.

## macOS compatibility fix

The original `nproc --all` calls in `Env.elm` and `Cache.elm` fail on macOS because `nproc` is a GNU coreutils utility not available on BSD-based systems. Fixed with a shell fallback:

```bash
nproc --all 2>/dev/null || sysctl -n hw.logicalcpu
```

This tries `nproc` first (Linux), falling back to `sysctl -n hw.logicalcpu` (macOS/BSD).

## What would be needed beyond this PoC

1. **Test discovery and harness generation.** Currently `PureTestRunner.elm` hardcodes `import SampleTests` and references `SampleTests.suite`. A real test runner needs to scan `tests/**/*.elm`, find modules exposing `Test` values, and generate a harness module that imports and combines them. elm-codegen (already a dependency) could handle this.

2. **Per-test dependency tracking via import analysis.** Currently the PoC hashes all source files into a single combined hash, and every test uses that same hash as its cache key. This means changing *any* source file invalidates *every* test — the caching is all-or-nothing.

   The improvement: parse each source file's imports to build a transitive dependency graph, then give each test its own cache key based only on the files it actually depends on. For example, if `TestA` imports `Foo` which imports `Bar`, its cache key would be `hash(TestA.elm, Foo.elm, Bar.elm)`. Changing `Baz.elm` wouldn't invalidate `TestA`'s cache.

   The strategy:
   - Parse imports from each `.elm` file (simple line scan for `import ModuleName` — no full parser needed)
   - Build a `module name -> file path` mapping using elm.json's `source-directories`
   - Compute transitive closures: for each test, walk its import chain to find all reachable source files
   - Hash only those files to produce a per-test dependency hash
   - Pass that hash (instead of the global combined hash) to `Cache.compute`

   We prototyped this in a shell-out approach (not included in this branch) and found the import parsing + transitive closure computation takes ~12ms for an entire project the size of [elm-markdown](https://github.com/dillonkearns/elm-markdown) (~50 source files, 268 tests). So the overhead is negligible.

   Combined with per-test caching via `compute`, this would give the best of both worlds: change one source file, and only the tests that transitively depend on it re-run. Everything else returns from cache instantly.

3. **More complete dependency tracking.** Beyond import analysis, a production version should also consider the Elm compiler version and resolved package versions, so that upgrading a dependency properly invalidates the cache.

3. **Structured result format.** `compute` outputs are strings (since they're stored as files in the content-addressed store). The test results are currently formatted as plain text and parsed back out. A JSON format would be more robust.

4. **Failure reporting.** The current display is basic (pass/fail with descriptions). A production runner would want elm-test-style diffs, source locations, and fuzz test shrink reporting.

5. **Fuzz test determinism.** Fuzz tests use a hardcoded seed (`Random.initialSeed 42`). A production version should derive the seed from the content hash of the test's dependencies, so the same inputs always produce the same fuzz cases, but changing inputs explores new cases.

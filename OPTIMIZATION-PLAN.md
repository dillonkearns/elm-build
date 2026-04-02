# Interpreter Performance: Optimization Plan

## Current State (2026-03-03)

The core-extra test suite benchmark runs 333 tests across 13 modules in ~46s wall time.
757 unit tests pass, 333/333 integration tests pass.

### Benchmark command
```
NODE_OPTIONS="--max-old-space-size=8192 --expose-gc" npx elm-pages run src/RunCoreExtraTests.elm -- --build .build/core-extra
```

Clear cache before benchmarking (important — cached results will skip evaluation):
```
chmod -R u+w .build/core-extra; rm -rf .build/core-extra; rm -f elm-stuff/elm-pages/elm.cjs
```

### Measuring wall time

Combine cache clear + timed run in one command:
```bash
chmod -R u+w .build/core-extra 2>/dev/null; rm -rf .build/core-extra; rm -f elm-stuff/elm-pages/elm.cjs; \
python3 -c "import time; print(int(time.time()*1000))" > /tmp/start_time.txt && \
NODE_OPTIONS="--max-old-space-size=8192 --expose-gc" npx elm-pages run src/RunCoreExtraTests.elm -- --build .build/core-extra > /tmp/bench.txt 2>&1; \
python3 -c "import time; start=int(open('/tmp/start_time.txt').read().strip()); print(f'Wall time: {int(time.time()*1000)-start}ms')"; \
grep '"passed"' /tmp/bench.txt
```

Run 2-3 times and average — variance of ~5-10% is normal. The result line
shows pass/fail counts; wall time includes lamdera compilation (~0.2s) + parsing + evaluation.

### CPU profiling

Add `--cpu-prof` to get a V8 CPU profile (adds ~15-20% overhead to timings):
```bash
chmod -R u+w .build/core-extra 2>/dev/null; rm -rf .build/core-extra; rm -f elm-stuff/elm-pages/elm.cjs; \
NODE_OPTIONS="--max-old-space-size=8192 --expose-gc --cpu-prof --cpu-prof-dir=./profiles" \
npx elm-pages run src/RunCoreExtraTests.elm -- --build .build/core-extra
```

Then analyze with:
```
node analyze-profile.mjs
```
The script reads the latest `.cpuprofile` from `./profiles/` and prints the top 30
functions by self-time, plus caller breakdowns for key functions.

## The test suite benchmark is misleading for cold starts

The 46s total is dominated by fuzz tests, not by parsing or simple evaluation:

| Module | Time | % of total | Notes |
|--------|------|------------|-------|
| **ListTests** | **25.1s** | **55%** | Fuzz tests, hundreds of iterations |
| **SetTests** | **6.5s** | **14%** | Fuzz-heavy |
| FloatTests | 2.2s | 5% | Fuzz tests |
| ArrayTests | 0.9s | 2% | |
| OrderTests | 0.5s | 1% | |
| DictTests | 0.4s | 1% | |
| MaybeTests | 0.3s | 1% | |
| BasicsTests | 0.3s | <1% | |
| CharTests | 0.3s | <1% | |
| String + Triple tests | ~2s | ~4% | |
| **Parse/init overhead** | **~7s** | **~15%** | elm-syntax parser + env building |

A real elm-pages cold start does NOT run fuzz tests. It:
1. Parses user source files + dependencies (lazy, only what's transitively needed)
2. Builds the environment (registers functions, types, imports)
3. Evaluates route modules and page generation expressions

**Estimated realistic cold start: 5-15s depending on project size.** The 10s target
is plausible without radical interpreter changes.

### How to build a better benchmark

To get a realistic cold-start proxy, create a benchmark that:
- Parses a representative set of modules (20-50 user files + dependencies)
- Evaluates simple top-level expressions (route definitions, page metadata)
- Does NOT run fuzz tests or test suites
- Measures parse time vs. evaluation time separately

An elm-pages project with 10-20 routes and typical dependencies would be ideal.

## Micro-optimizations completed (diminishing returns)

These are the optimizations done so far. Each was validated with CPU profiling.
Further micro-optimizations in this vein will yield ~5-10% gains at best.

### String operations
- **moduleKey pattern matching**: Handles 1-3 element lists with `++` instead of `String.join`. Eliminated `_String_join` from hot path entirely.
- **Pre-computed module keys in ImportedNames**: `Dict String (ModuleName, String)` caches the moduleKey alongside each module name. Avoids redundant `moduleKey` calls during import resolution.
- **qualifiedNameToString deferred to error paths**: Was eagerly computed in `let` bindings on every kernel function call (~2.8% of CPU). Now only computed when an error actually occurs.

### Record/allocation
- **bindSimplePatterns batching**: Operates on `EnvValues` (Dict) directly, creating 1 Env record at the end instead of N intermediate records per function argument.
- **Environment.replaceValues**: Explicit record construction instead of `{ env | values = ... }` which generates `_Utils_update` (slow `for...in` loop). Avoid record update syntax on hot paths in general.
- **CallTree conditional creation**: `CallNode` only created when `cfg.trace = True`. This was the fix that made the test suite completable at all (was OOM before).

### Environment
- **callNoStack/callKernelNoStack**: Skip callStack push when trace is off.
- **currentModuleFunctions caching**: Env stores `currentModuleFunctions` to avoid re-lookup when calling within same module.

### General lesson: avoid `_Utils_update`
Elm's `{ record | field = value }` syntax compiles to `_Utils_update` which uses
`for...in` to copy all fields. On hot paths, use explicit record construction instead
(list all fields). This is especially impactful for large records like `Env` (8 fields).

## Current CPU profile breakdown

After micro-optimizations, the remaining CPU is dominated by fundamentals:

| Category | % of CPU | Notes |
|----------|----------|-------|
| Parser equality (`_Utils_eqHelp`) | ~10% | elm-syntax `isNotRelevant`, can't optimize |
| FastDict + `_Utils_cmp` | ~16% | String comparisons in AVL tree lookups, fundamental |
| Function wrappers (A2/A3/A4/A5) | ~10% | Elm's currying overhead, needs elm-optimize-level-2 |
| Trampoline (`runRecursion` + `evalOrRecurse`) | ~6% | Stack-safety mechanism, already optimized |
| Everything else | ~58% | Distributed across many small functions |

## Architectural improvements (where the big wins are)

These are the changes that could yield 20-80% improvements, getting to the 10s target:

### 1. elm-optimize-level-2 on the interpreter (~10% speedup)
The interpreter itself is compiled with `--debug` (required due to mutual recursion
in `evalExpression` / `evalApplication`). This means A2/A3/F2/F3 wrappers are NOT
inlined, costing ~10% of CPU.

**Status**: elm-pages `compile-elm.js` line 147 has `elmOptimizeLevel2()` commented out.
The `run` command hardcodes `{ debug: true }`. The `bundle-script` command passes
through the debug flag.

**Blocker**: Compiling without `--debug` fails with "infinite recursion in top-level
definitions" due to mutual recursion. eol2 also converts thunked lazy top-level defs
to eager evaluation, causing runtime infinite recursion.

**Path forward**: Would need to restructure the interpreter to avoid mutual top-level
recursion, or find a way to make eol2 preserve the lazy thunks.

**How to validate**: Once unblocked, compare profiles with and without eol2. The A2
entries in the profile should largely disappear.

### 2. Cached parsed ASTs (~20-40% of cold start)
Parsing is ~15% of the benchmark (~7s). For a real cold start, parsing could be
an even larger fraction since evaluation would be lighter (no fuzz tests).

**Idea**: After parsing a module, serialize the AST to a binary cache file. On
subsequent cold starts, load the binary AST instead of re-parsing. Invalidate
by source file hash.

**How to validate**: Measure parse time separately. Add timing around
`Elm.Parser.parseToFile` calls in `Eval/Module.elm`. Compare cold start with
and without cached ASTs.

### 3. Pre-baked standard library (largest potential win)
elm/core, elm/json, elm/html, etc. never change for a given elm.json. Their ASTs
could be pre-parsed and shipped as binary blobs, eliminating parse + env building
time for all standard packages.

The interpreter already has `Core.dependency` which provides baked-in interfaces.
Extending this to include full parsed ASTs for common packages would skip the most
expensive re-parsing.

**How to validate**: Count how many of the parsed modules during a cold start are
from packages vs. user code. Measure time spent parsing package modules specifically.

### 4. Module-level evaluation caching
`InterpreterProject` already caches evaluation results by hash. Verify this is working
effectively and that cache invalidation is correct. The cache should survive across
cold starts (persisted to disk).

**How to validate**: Run the same script twice and compare timing. The second run
should be significantly faster if caching is working.

### 5. Lazy module evaluation
Currently `buildProjectEnv` processes ALL transitively-needed modules. If some modules
are only needed for type information (interfaces) but not for runtime evaluation,
their function bodies don't need to be parsed/registered.

**How to validate**: Log which modules' functions are actually called during evaluation
vs. which are registered. If many registered functions are never called, lazy
registration could help.

## How to approach further optimization

1. **Build a realistic benchmark first.** The fuzz test suite is good for correctness
   but bad for performance measurement. Create an elm-pages project benchmark that
   represents actual cold-start workloads.

2. **Measure parse vs. eval separately.** Add timing instrumentation to distinguish:
   - Source loading (IO)
   - Parsing (elm-syntax)
   - Environment building (import processing, function registration)
   - Expression evaluation

3. **Profile the realistic benchmark.** The CPU profile will look very different
   without fuzz tests. Parser overhead will dominate more; interpreter loop overhead
   will dominate less.

4. **Pursue architectural wins in priority order:**
   - Pre-baked stdlib (biggest bang, most modules parsed are packages)
   - AST caching (benefits user code on repeat cold starts)
   - eol2 integration (benefits everything, but has a blocker to resolve)

5. **Keep micro-optimizing only when profiling reveals a clear hotspot** above ~2%
   of CPU that has an actionable fix.

Btw, I just got the `elmcraft/core-extra` test suite time down to:

```
bunx elm-pages run (51 interpreter tests)
  Cold  2.87s
  Warm  0.48s
```

That includes all of the overhead of `bunx elm-pages run` (module resolution, recompilation checks, loading the 2.1MB compiled Elm app, etc.). Using `elm-pages bundle-script` to pre-bundle into a single .mjs file and running with `bun` directly eliminates that CLI overhead:

```
bun ./dist/run-core-extra.mjs (pre-bundled)
  Cold  2.46s
  Warm  0.25s

node ./dist/run-core-extra.mjs (pre-bundled)
  Cold  2.88s
  Warm  0.32s
```

The warm run savings are real — `bunx` adds ~200ms of CLI overhead on every invocation, so pre-bundling gets us from 0.48s → 0.25s with bun (or 0.32s with node). The cold runs are dominated by the interpreter parsing+evaluating ~35 modules (~2.4s).

For context, `elm-test` runs the full 622-test suite (all modules) in about 1.05s. We're currently running 51 of those tests — the ones the interpreter handles today. The remaining modules need interpreter fixes for things like module alias resolution and partial application edge cases in kernel code.

The key optimization that got us here: lazy parsing. Instead of eagerly parsing all ~339 package modules upfront, we now build a lightweight dependency graph (~48ms) and only parse the ~35 modules that are transitively needed. That single change took the warm run from ~4.9s → 0.48s (10x speedup).

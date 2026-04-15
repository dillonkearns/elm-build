# External Test Suites

This folder holds a small external-suite harness for comparing `elm-test` against `src/TestRunner.elm`.

Tracked files:
- `suites.mjs`: starter suite definitions.

Ignored folders:
- `checkouts/`: cloned target repositories.
- `results/`: JSON run artifacts from the oracle/benchmark harness.

Usage:

```sh
npm run bench:external-suites:sync
npm run bench:external-suites -- --list
npm run bench:external-suites -- --suite elm-review-common
npm run bench:external-suites -- --suite starter --runs 3
```

Useful flags:
- `--suite starter|all|comma,separated,ids`
- `--runs 3`
- `--fuzz 100`
- `--seed 42`
- `--timeout-secs 300`
- `--globs tests/NoMissingTypeAnnotationTest.elm`
- `--sync-only`

The harness treats the measured path as "warm deps, cold user":
- `elm-test` runs after deleting local `elm-stuff`, while global `~/.elm` stays warm.
- `TestRunner` keeps `.elm-build/.../package-*` warm, but clears user-specific cache files between measured runs.

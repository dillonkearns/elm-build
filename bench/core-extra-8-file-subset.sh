#!/usr/bin/env bash
# Benchmark: TestRunner (warm deps, cold user project) vs elm-test (cold elm-stuff)
# on the subset of elmcraft/core-extra's test files that our interpreter runs
# cleanly end-to-end (218 tests, ~fuzz-heavy).
#
# Expected (pre-regression): ~834 ms (TestRunner) vs ~1378 ms (elm-test),
# so TestRunner ~1.65× faster.
#
# Usage:
#   bash bench/core-extra-8-file-subset.sh /abs/path/to/elmcraft/core-extra
#
# Prereqs on $PATH: hyperfine, elm-test, node.
# The current repo's myscript.mjs must already be built (npx elm-pages
# bundle-script src/TestRunner.elm writes it to the repo root).

set -euo pipefail

CORE_EXTRA_DIR=${1:?"usage: $0 /abs/path/to/elmcraft/core-extra"}
BUILD_DIR=$(cd "$(dirname "$0")/.." && pwd)
SCRIPT="$BUILD_DIR/myscript.mjs"

if [ ! -f "$SCRIPT" ]; then
  echo "myscript.mjs not found at $SCRIPT. Run:" >&2
  echo "  cd $BUILD_DIR && npx elm-pages bundle-script src/TestRunner.elm" >&2
  exit 1
fi

TESTS_COMMA="tests/BasicsTests.elm,tests/CharTests.elm,tests/DictTests.elm,tests/FloatTests.elm,tests/MaybeTests.elm,tests/OrderTests.elm,tests/ResultTests.elm,tests/SetTests.elm"
TESTS_SPACED="tests/BasicsTests.elm tests/CharTests.elm tests/DictTests.elm tests/FloatTests.elm tests/MaybeTests.elm tests/OrderTests.elm tests/ResultTests.elm tests/SetTests.elm"

cd "$CORE_EXTRA_DIR"

# IMPORTANT about the TestRunner prep step:
#
# `.elm-build/` contains three flavors of cache:
#   1. `package-*.blob` / `.key`   — parsed package interfaces (deps).
#      Want to keep WARM for a fair "warm pkgs, cold user" comparison.
#   2. `user-normalized-v3-*.blob` — user top-level-constant normalization.
#      Part of "processing the user project" → treat as COLD.
#   3. `<8-hex>` hash-named files  — per-`Cache.run` pre-computed results
#      from `SimpleTestRunner.runToString`. If these stay warm, the runner
#      reads the cached output verbatim and SKIPS EVAL ENTIRELY — a subtle
#      short-circuit that quietly inflates the speedup on repeat runs.
#      Treat as COLD.
#
# Therefore the prep deletes every top-level file under `.elm-build/`
# that is not a package-cache artifact.
hyperfine \
  --warmup 3 \
  --min-runs 12 \
  --prepare "find .elm-build -maxdepth 1 -type f ! -name 'package-*' -delete" \
  -n "TestRunner (warm pkgs, cold user)" \
  "node --stack-size=8192 $SCRIPT --test $TESTS_COMMA" \
  --prepare "rm -rf elm-stuff" \
  -n "elm-test (cold elm-stuff)" \
  "elm-test $TESTS_SPACED"

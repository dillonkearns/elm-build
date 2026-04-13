#!/usr/bin/env bash
# Benchmark: TestRunner (warm deps, cold user project) vs elm-test (cold elm-stuff)
# on the FULL set of elmcraft/core-extra's non-String test files (514 tests).
#
# Note: the interpreter can't yet resolve the String/* test modules
# (cross-module reference issue at load time), so they're excluded from
# both sides for an apples-to-apples comparison.
#
# Usage:
#   bash bench/core-extra-full.sh /abs/path/to/elmcraft/core-extra
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

TESTS_COMMA="tests/ArrayTests.elm,tests/BasicsTests.elm,tests/CharTests.elm,tests/DictTests.elm,tests/FloatTests.elm,tests/ListTests.elm,tests/MaybeTests.elm,tests/OrderTests.elm,tests/ResultTests.elm,tests/SetTests.elm,tests/TripleTests.elm"
TESTS_SPACED="tests/ArrayTests.elm tests/BasicsTests.elm tests/CharTests.elm tests/DictTests.elm tests/FloatTests.elm tests/ListTests.elm tests/MaybeTests.elm tests/OrderTests.elm tests/ResultTests.elm tests/SetTests.elm tests/TripleTests.elm"

cd "$CORE_EXTRA_DIR"

# See the 8-file-subset script for the full explanation of the cache
# layout. Short version: keep `.elm-build/package-*` (warm package
# interfaces) between runs, delete every other top-level file to force
# a cold user-project evaluation.
hyperfine \
  --warmup 3 \
  --min-runs 12 \
  --prepare "find .elm-build -maxdepth 1 -type f ! -name 'package-*' -delete" \
  -n "TestRunner (warm pkgs, cold user)" \
  "node --stack-size=8192 $SCRIPT --test $TESTS_COMMA" \
  --prepare "rm -rf elm-stuff" \
  -n "elm-test (cold elm-stuff)" \
  "elm-test $TESTS_SPACED"

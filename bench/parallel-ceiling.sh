#!/usr/bin/env bash
# Phase 1 of the parallelization plan (plans/foamy-nibbling-lecun.md):
# measure the parallel speedup ceiling of elm-test-rs on elmcraft/core-extra,
# plus references for elm-test and our TestRunner.
#
# Cache semantics (warm global, cold user) are identical across all tools:
#   - Preserve per-tool user-home caches (~/.elm, elm-test-rs tests-runner cache).
#   - Delete the project-local user cache before each run.
#     - elm-test / elm-test-rs: `rm -rf elm-stuff`
#     - TestRunner (elm-build): delete everything in `.elm-build/` that isn't
#       a `package-*` file (matches bench/core-extra-8-file-subset.sh).
#
# Usage:
#   bash bench/parallel-ceiling.sh /abs/path/to/elmcraft/core-extra [full|subset]
#
# Prereqs: hyperfine, elm-test on PATH, node_modules/.bin/elm-test-rs v3+,
# npx available for TestRunner via elm-pages.

set -euo pipefail

CORE_EXTRA_DIR=${1:?"usage: $0 /abs/path/to/elmcraft/core-extra [full|subset]"}
SUITE=${2:-full}

BUILD_DIR=$(cd "$(dirname "$0")/.." && pwd)
RUNNER_ELM="$BUILD_DIR/src/TestRunner.elm"
ELM_TEST_RS="$BUILD_DIR/node_modules/.bin/elm-test-rs"

if [ ! -x "$ELM_TEST_RS" ]; then
  echo "elm-test-rs not found at $ELM_TEST_RS — run 'npm install' first" >&2
  exit 1
fi

case "$SUITE" in
  full)
    TESTS_COMMA="tests/ArrayTests.elm,tests/BasicsTests.elm,tests/CharTests.elm,tests/DictTests.elm,tests/FloatTests.elm,tests/ListTests.elm,tests/MaybeTests.elm,tests/OrderTests.elm,tests/ResultTests.elm,tests/SetTests.elm,tests/TripleTests.elm"
    TESTS_SPACED="tests/ArrayTests.elm tests/BasicsTests.elm tests/CharTests.elm tests/DictTests.elm tests/FloatTests.elm tests/ListTests.elm tests/MaybeTests.elm tests/OrderTests.elm tests/ResultTests.elm tests/SetTests.elm tests/TripleTests.elm"
    LABEL="11-file full"
    ;;
  subset)
    TESTS_COMMA="tests/BasicsTests.elm,tests/CharTests.elm,tests/DictTests.elm,tests/FloatTests.elm,tests/MaybeTests.elm,tests/OrderTests.elm,tests/ResultTests.elm,tests/SetTests.elm"
    TESTS_SPACED="tests/BasicsTests.elm tests/CharTests.elm tests/DictTests.elm tests/FloatTests.elm tests/MaybeTests.elm tests/OrderTests.elm tests/ResultTests.elm tests/SetTests.elm"
    LABEL="8-file subset"
    ;;
  *)
    echo "suite must be 'full' or 'subset'" >&2
    exit 1
    ;;
esac

RESULTS_DIR="$BUILD_DIR/bench/results"
mkdir -p "$RESULTS_DIR"
JSON_OUT="$RESULTS_DIR/parallel-ceiling-${SUITE}.json"

cd "$CORE_EXTRA_DIR"

# Each tool gets a --prepare that wipes only its *project-local user* cache.
# elm-test-rs and elm-test both live under elm-stuff/; TestRunner lives under
# .elm-build/. All three leave warm user-home caches (~/.elm) untouched.
RESET_ELMTEST='rm -rf elm-stuff'
RESET_TESTRUNNER="find .elm-build -maxdepth 1 -type f ! -name 'package-*' -delete"

hyperfine \
  --warmup 2 \
  --min-runs 8 \
  --export-json "$JSON_OUT" \
  --prepare "$RESET_ELMTEST" \
  -n "elm-test-rs w=1 ($LABEL)" \
  "$ELM_TEST_RS --workers=1 $TESTS_SPACED" \
  --prepare "$RESET_ELMTEST" \
  -n "elm-test-rs w=default ($LABEL)" \
  "$ELM_TEST_RS $TESTS_SPACED" \
  --prepare "$RESET_ELMTEST" \
  -n "elm-test ($LABEL)" \
  "elm-test $TESTS_SPACED" \
  --prepare "$RESET_TESTRUNNER" \
  -n "TestRunner ($LABEL)" \
  "npx elm-pages run \"$RUNNER_ELM\" --test $TESTS_COMMA"

echo ""
echo "results written to $JSON_OUT"

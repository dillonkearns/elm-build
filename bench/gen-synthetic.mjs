#!/usr/bin/env node
/**
 * Synthetic test fixture generator for the chunked-eval choke
 * investigation. Writes a single Elm test file with N children to a
 * target directory.
 *
 * Used by `bench/chunking-sweep.sh` to feed the investigation in
 * `~/.claude/plans/recursive-chasing-pearl.md`. Lets us vary one knob
 * at a time (N, shape) without the noise of a real-world test suite.
 *
 * Usage:
 *   node bench/gen-synthetic.mjs \
 *     --count 15 \
 *     --shape bare-ref \
 *     --out /tmp/core-extra/tests/SyntheticBigDescribe.elm
 *
 * Shapes:
 *   trivial          each child is `test "tN" (\\_ -> Expect.equal 1 1)` inlined
 *                    in the all = describe "..." [...] list literal
 *   bare-ref         each child is a bare reference cN; cN = test "tN" ...
 *                    is co-defined as a top-level (mirrors what
 *                    extractDescribeChildren qualifies to ModuleName.cN
 *                    for chunk dispatch)
 *   nested-describe  each child is itself describe "subN" [test ..., test ...]
 *                    (forces nested RList resolution)
 *   inline-fuzz      each child uses fuzz Fuzz.int "tN" (\\n -> Expect.equal n n)
 *                    (different Test.Runner.fromTest branch)
 *
 * The generated module is named `SyntheticBigDescribe`. The output
 * file path's basename should match (e.g. `SyntheticBigDescribe.elm`).
 */

import fs from "node:fs";
import path from "node:path";

function parseArgs(argv) {
    const args = { count: null, shape: null, out: null };
    for (let i = 0; i < argv.length; i++) {
        const arg = argv[i];
        if (arg === "--count") args.count = parseInt(argv[++i], 10);
        else if (arg === "--shape") args.shape = argv[++i];
        else if (arg === "--out") args.out = argv[++i];
        else {
            console.error(`unknown arg: ${arg}`);
            process.exit(2);
        }
    }
    if (!Number.isFinite(args.count) || args.count < 1) {
        console.error("--count <positive integer> required");
        process.exit(2);
    }
    if (!["trivial", "bare-ref", "nested-describe", "inline-fuzz"].includes(args.shape)) {
        console.error("--shape <trivial|bare-ref|nested-describe|inline-fuzz> required");
        process.exit(2);
    }
    if (!args.out) {
        console.error("--out <path/to/SyntheticBigDescribe.elm> required");
        process.exit(2);
    }
    return args;
}

function header() {
    return [
        "module SyntheticBigDescribe exposing (all)",
        "",
        "import Expect",
        "import Fuzz",
        "import Test exposing (Test, describe, fuzz, test)",
        "",
        "",
    ].join("\n");
}

function trivialChild(i) {
    return `        test "t${i}" (\\_ -> Expect.equal 1 1)`;
}

function nestedDescribeChild(i) {
    // Each child is a multi-line `describe "..." [test, test, …]` block.
    // Five inner tests so each child's source spans ~7 lines, matching
    // the rough shape of core-extra's ListTests children.
    return [
        `        describe "sub${i}"`,
        `            [ test "${i}-a" (\\_ -> Expect.equal 1 1)`,
        `            , test "${i}-b" (\\_ -> Expect.equal 1 1)`,
        `            , test "${i}-c" (\\_ -> Expect.equal 1 1)`,
        `            , test "${i}-d" (\\_ -> Expect.equal 1 1)`,
        `            , test "${i}-e" (\\_ -> Expect.equal 1 1)`,
        `            ]`,
    ].join("\n");
}

function inlineFuzzChild(i) {
    return `        fuzz Fuzz.int "t${i}" (\\n -> Expect.equal n n)`;
}

function bareRefList(n) {
    const refs = [];
    for (let i = 1; i <= n; i++) refs.push(`        c${i}`);
    return refs;
}

function bareRefDecls(n) {
    const decls = [];
    for (let i = 1; i <= n; i++) {
        decls.push(`c${i} : Test`);
        decls.push(`c${i} =`);
        decls.push(`    test "t${i}" (\\_ -> Expect.equal 1 1)`);
        decls.push("");
        decls.push("");
    }
    return decls.join("\n");
}

function buildAll(shape, n) {
    let entries;
    switch (shape) {
        case "trivial":
            entries = Array.from({ length: n }, (_, i) => trivialChild(i + 1));
            break;
        case "bare-ref":
            entries = bareRefList(n);
            break;
        case "nested-describe":
            entries = Array.from({ length: n }, (_, i) => nestedDescribeChild(i + 1));
            break;
        case "inline-fuzz":
            entries = Array.from({ length: n }, (_, i) => inlineFuzzChild(i + 1));
            break;
    }

    // Indent with 4-space leading; entries already include 8-space indent.
    const formatted =
        entries.length === 0
            ? "        []"
            : "        [ " +
              entries[0].trimStart() +
              "\n" +
              entries
                  .slice(1)
                  .map((e) => "        , " + e.trimStart())
                  .join("\n") +
              "\n        ]";

    return [
        "all : Test",
        "all =",
        `    describe "Synthetic"`,
        formatted,
        "",
    ].join("\n");
}

function buildSource(shape, n) {
    const parts = [header(), buildAll(shape, n), ""];
    if (shape === "bare-ref") {
        parts.push(bareRefDecls(n));
    }
    return parts.join("\n");
}

function main() {
    const args = parseArgs(process.argv.slice(2));
    const source = buildSource(args.shape, args.count);

    fs.mkdirSync(path.dirname(args.out), { recursive: true });
    fs.writeFileSync(args.out, source);

    process.stderr.write(
        `wrote ${args.out} (shape=${args.shape}, count=${args.count}, ${source.length} bytes)\n`,
    );
}

main();

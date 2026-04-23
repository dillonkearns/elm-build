#!/usr/bin/env node
/**
 * Synthetic tail-recursive microbench fixture generator.
 *
 * Writes an Elm test module that walks a `List.repeat N 5` via a
 * hand-written tail-recursive helper. The workload isolates the
 * interpreter's TCO hot path from other test-runner overhead so the
 * Duration: Xms reported by TestRunner reflects the per-iteration
 * cost of a `case list of [] / x :: xs ->` self-recursive walk.
 *
 * Companion to ~/.claude/plans/recursive-chasing-pearl.md Phase 0.
 *
 * Usage:
 *   node bench/gen-synthetic-tco.mjs \
 *     --count 1000000 \
 *     --shape count-only \
 *     --out /tmp/core-extra/tests/MicroBenchTco.elm
 *
 * Shapes:
 *   count-only  countTo acc (x :: xs) = countTo (acc+1) xs
 *               matches the raw walk cost with no inner work.
 *   isInfixOf   List.Extra.isInfixOf [5,7,13] (List.repeat N 5)
 *               matches the actual core-extra benchmark.
 *   sum         sumTo acc (x :: xs) = sumTo (acc+x) xs
 *               adds a single Int+ per iter (lets us measure if the
 *               arithmetic is free compared to the walk).
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
    if (!["count-only", "isInfixOf", "sum"].includes(args.shape)) {
        console.error("--shape <count-only|isInfixOf|sum> required");
        process.exit(2);
    }
    if (!args.out) {
        console.error("--out <path/to/MicroBenchTco.elm> required");
        process.exit(2);
    }
    return args;
}

const { count, shape, out } = parseArgs(process.argv.slice(2));

const moduleName = path.basename(out, ".elm");

let body;
switch (shape) {
    case "count-only":
        body = `
countTo : Int -> List Int -> Int
countTo acc list =
    case list of
        [] ->
            acc

        _ :: xs ->
            countTo (acc + 1) xs


all : Test
all =
    test "count 1..N walks tail-recursive list of ${count}" <|
        \\() ->
            countTo 0 (List.repeat ${count} 5)
                |> Expect.equal ${count}
`;
        break;

    case "sum":
        body = `
sumTo : Int -> List Int -> Int
sumTo acc list =
    case list of
        [] ->
            acc

        x :: xs ->
            sumTo (acc + x) xs


all : Test
all =
    test "sum tail-recursive list of ${count}" <|
        \\() ->
            sumTo 0 (List.repeat ${count} 5)
                |> Expect.equal ${count * 5}
`;
        break;

    case "isInfixOf":
        body = `
all : Test
all =
    test "isInfixOf across List.repeat ${count} 5" <|
        \\() ->
            List.Extra.isInfixOf [ 5, 7, 13 ] (List.repeat ${count} 5)
                |> Expect.equal False
`;
        break;
}

const imports = shape === "isInfixOf"
    ? `import List.Extra\n`
    : ``;

const source = `module ${moduleName} exposing (all)

import Expect
${imports}import Test exposing (Test, test)

${body}`;

fs.mkdirSync(path.dirname(out), { recursive: true });
fs.writeFileSync(out, source);
process.stderr.write(`wrote ${out}  (shape=${shape}, count=${count})\n`);

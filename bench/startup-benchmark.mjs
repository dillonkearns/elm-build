#!/usr/bin/env node

import { spawnSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { performance } from "node:perf_hooks";

const repoRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), "..");
const reviewDir = path.join(repoRoot, "bench", "review");
const iterations = Number(readArg("--iterations", "5"));

const runners = [
  {
    name: "no_op",
    distPath: path.join(repoRoot, "dist", "startup-no-op-bench.mjs"),
    spawnArgs: () => [path.join(repoRoot, "dist", "startup-no-op-bench.mjs")],
    env: () => process.env,
  },
  {
    name: "review_runner_fast_startup_only",
    distPath: path.join(repoRoot, "dist", "review-runner-fast-bench.mjs"),
    spawnArgs: () => [path.join(repoRoot, "dist", "review-runner-fast-bench.mjs")],
    env: (tracePath, buildDir) => ({
      ...process.env,
      REVIEW_RUNNER_CONFIG_JSON: JSON.stringify({
        reviewDir,
        buildDirectory: buildDir,
        reportFormat: "quiet",
        perfTraceJson: tracePath,
        startupOnly: true,
      }),
    }),
  },
];

function readArg(flag, fallback) {
  const index = process.argv.indexOf(flag);
  return index >= 0 ? process.argv[index + 1] : fallback;
}

function ensureExists(filePath) {
  if (!fs.existsSync(filePath)) {
    throw new Error(`Missing bundle: ${filePath}`);
  }
}

function average(values) {
  return Math.round(values.reduce((sum, value) => sum + value, 0) / values.length);
}

function main() {
  for (const runner of runners) {
    ensureExists(runner.distPath);
  }

  const root = fs.mkdtempSync(path.join(os.tmpdir(), "elm-build2-startup-bench-"));
  const results = runners.map((runner) => {
    const samples = [];

    for (let index = 0; index < iterations; index += 1) {
      const tracePath = path.join(root, `${runner.name}-${index}.trace.json`);
      const buildDir = path.join(root, `${runner.name}-${index}.build`);
      fs.mkdirSync(buildDir, { recursive: true });

      const start = performance.now();
      const result = spawnSync("node", runner.spawnArgs(tracePath, buildDir), {
        cwd: repoRoot,
        encoding: "utf8",
        env: runner.env(tracePath, buildDir),
      });
      const wallMs = Math.round(performance.now() - start);

      if (result.status !== 0) {
        throw new Error(`${runner.name} failed:\n${result.stdout}\n${result.stderr}`);
      }

      samples.push(wallMs);
    }

    return {
      name: runner.name,
      iterations,
      samples_ms: samples,
      average_ms: average(samples),
      min_ms: Math.min(...samples),
      max_ms: Math.max(...samples),
    };
  });

  console.log(JSON.stringify({ iterations, results }, null, 2));
}

main();

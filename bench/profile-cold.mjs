#!/usr/bin/env node

/**
 * CPU-profile the COLD run in isolation (sister to profile-warm-import-graph.mjs).
 *
 * 1. Sets up a fresh workspace with the small-12 fixture
 * 2. Runs the profiled cold run — this is the resolved-IR path's big win
 *
 * The .cpuprofile file lands in the workspace directory AND is copied
 * to .scratch/cold.cpuprofile for easy analysis with
 * `bench/analyze-cpuprofile.mjs .scratch/cold.cpuprofile --top 30`.
 *
 * Usage:
 *   node bench/profile-cold.mjs [--env-mode <mode>]
 *
 * env-mode defaults to legacy-ast; pass resolved-list-unplanned or
 * resolved-list-slotted to profile the new evaluator.
 */

import { spawnSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";

const repoRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), "..");
const srcRoot = path.join(repoRoot, "src");
const reviewDir = path.join(repoRoot, "bench", "review");
const distRunnerPath = path.join(repoRoot, "dist", "review-runner-bench.mjs");
const jobs = String(os.cpus().length);
const envMode = process.argv.includes("--env-mode")
  ? process.argv[process.argv.indexOf("--env-mode") + 1]
  : "legacy-ast";

const fixtureFiles = [
  "Coverage.elm",
  "DepGraph.elm",
  "MathLib.elm",
  "MutationReport.elm",
  "Path.elm",
  "PriceCalculator.elm",
  "ProjectSources.elm",
  "ReviewRunner.elm",
  "SampleValue.elm",
  "SemanticHash.elm",
  "UserAccess.elm",
  "Validator.elm",
];

function ensureDir(d) {
  fs.mkdirSync(d, { recursive: true });
}

function prepareWorkspace() {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "elm-build2-profile-cold-"));
  const fixtureSrcDir = path.join(root, "src");
  const buildDir = path.join(root, ".elm-review-build");
  ensureDir(fixtureSrcDir);
  ensureDir(buildDir);

  for (const fileName of fixtureFiles) {
    fs.copyFileSync(path.join(srcRoot, fileName), path.join(fixtureSrcDir, fileName));
  }

  const rootElmJson = JSON.parse(fs.readFileSync(path.join(repoRoot, "elm.json"), "utf8"));
  rootElmJson["source-directories"] = ["src"];
  fs.writeFileSync(path.join(root, "elm.json"), JSON.stringify(rootElmJson, null, 4));

  return { root, fixtureSrcDir, buildDir };
}

// ── Main ──

const workspace = prepareWorkspace();

console.log("=== env-mode:", envMode);
console.log("=== workspace:", workspace.root);
console.log("=== Profiling cold run (fresh caches, no prior warm)...");

const profDir = path.join(workspace.root, "profiles");
ensureDir(profDir);
const tracePath = path.join(workspace.root, "cold-trace.json");

const result = spawnSync(
  "node",
  [
    "--cpu-prof",
    "--cpu-prof-dir", profDir,
    "--cpu-prof-interval", "5000",
    distRunnerPath,
    "--review-dir", reviewDir,
    "--source-dirs", workspace.fixtureSrcDir,
    "--build", workspace.buildDir,
    "--jobs", jobs,
    "--importers-cache-mode", "auto",
    "--deps-cache-mode", "auto",
    "--env-mode", envMode,
    "--perf-trace-json", tracePath,
  ],
  { cwd: repoRoot, encoding: "utf8" }
);

console.log(`    exit=${result.status}`);

const profiles = fs.readdirSync(profDir).filter(f => f.endsWith(".cpuprofile"));
if (profiles.length > 0) {
  const profilePath = path.join(profDir, profiles[0]);
  console.log(`\n=== CPU profile written to: ${profilePath}`);

  const destPath = path.join(repoRoot, ".scratch", "cold.cpuprofile");
  ensureDir(path.dirname(destPath));
  fs.copyFileSync(profilePath, destPath);
  console.log(`    Copied to: ${destPath}`);
  console.log(`    Analyze: node bench/analyze-cpuprofile.mjs .scratch/cold.cpuprofile --top 30`);
} else {
  console.log("=== No .cpuprofile file found in", profDir);
}

if (fs.existsSync(tracePath)) {
  const trace = JSON.parse(fs.readFileSync(tracePath, "utf8"));
  const totalMs = trace.stages.reduce((s, st) => s + st.ms, 0);
  console.log(`\n=== Trace summary (internal ${(totalMs/1000).toFixed(2)}s):`);
  for (const stage of trace.stages) {
    console.log(`    ${stage.name}: ${(stage.ms/1000).toFixed(2)}s`);
  }
}

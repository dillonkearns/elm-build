#!/usr/bin/env node

/**
 * CPU-profile the warm_import_graph_change scenario in isolation.
 *
 * 1. Sets up workspace with the small-12 fixture
 * 2. Cold run (primes caches)
 * 3. Warm run (primes warm caches)
 * 4. Mutates MathLib.elm to change the import graph
 * 5. Runs the import-graph-change scenario under --cpu-prof
 *
 * The .cpuprofile file lands in the workspace directory.
 * Load it in Chrome DevTools (Performance tab → Load profile).
 */

import { spawnSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";

const repoRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), "..");
const srcRoot = path.join(repoRoot, "src");
const reviewDir = path.join(repoRoot, "bench", "review");
const distRunnerPath = path.join(repoRoot, "dist", "review-runner-bench-debug.mjs");
const jobs = String(os.cpus().length);

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
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "elm-build2-profile-"));
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

function runReviewer(workspace, traceName, extraNodeArgs = []) {
  const tracePath = path.join(workspace.root, traceName);
  const result = spawnSync(
    "node",
    [
      ...extraNodeArgs,
      distRunnerPath,
      "--review-dir", reviewDir,
      "--source-dirs", workspace.fixtureSrcDir,
      "--build", workspace.buildDir,
      "--jobs", jobs,
      "--importers-cache-mode", "auto",
      "--deps-cache-mode", "auto",
      "--perf-trace-json", tracePath,
    ],
    { cwd: repoRoot, encoding: "utf8" }
  );

  return {
    exitCode: result.status,
    stdout: result.stdout,
    stderr: result.stderr,
    tracePath,
  };
}

function mutateImportGraph(fixtureSrcDir) {
  const filePath = path.join(fixtureSrcDir, "MathLib.elm");
  const source = fs.readFileSync(filePath, "utf8");
  const marker = "-}\n\n";
  const mutated = source.replace(marker, "-}\n\nimport ProjectSources\n\n");
  fs.writeFileSync(filePath, mutated);
}

// ── Main ──

const workspace = prepareWorkspace();

console.log("=== workspace:", workspace.root);
console.log("=== Step 1: Cold run (priming caches)...");
const cold = runReviewer(workspace, "cold-trace.json");
console.log(`    exit=${cold.exitCode}`);

console.log("=== Step 2: Warm run (priming warm caches)...");
const warm = runReviewer(workspace, "warm-trace.json");
console.log(`    exit=${warm.exitCode}`);

console.log("=== Step 3: Mutate import graph...");
mutateImportGraph(workspace.fixtureSrcDir);

console.log("=== Step 4: Profiled import-graph-change run...");
const profDir = path.join(workspace.root, "profiles");
ensureDir(profDir);

const profiled = runReviewer(workspace, "import-graph-trace.json", [
  "--cpu-prof",
  "--cpu-prof-dir", profDir,
]);
console.log(`    exit=${profiled.exitCode}`);

// Find the .cpuprofile file
const profiles = fs.readdirSync(profDir).filter(f => f.endsWith(".cpuprofile"));
if (profiles.length > 0) {
  const profilePath = path.join(profDir, profiles[0]);
  console.log(`\n=== CPU profile written to: ${profilePath}`);
  console.log(`    Load in Chrome DevTools → Performance → Load profile`);

  // Also copy to a convenient location
  const destPath = path.join(repoRoot, ".scratch", "warm-import-graph.cpuprofile");
  ensureDir(path.dirname(destPath));
  fs.copyFileSync(profilePath, destPath);
  console.log(`    Copied to: ${destPath}`);
} else {
  console.log("=== No .cpuprofile file found in", profDir);
}

// Print trace summary
if (fs.existsSync(profiled.tracePath)) {
  const trace = JSON.parse(fs.readFileSync(profiled.tracePath, "utf8"));
  const totalMs = trace.stages.reduce((s, st) => s + st.ms, 0);
  console.log(`\n=== Trace summary (internal ${(totalMs/1000).toFixed(2)}s):`);
  for (const stage of trace.stages) {
    console.log(`    ${stage.name}: ${(stage.ms/1000).toFixed(2)}s`);
  }
  if (trace.counters) {
    const interesting = Object.entries(trace.counters)
      .filter(([k]) => k.includes("eval") || k.includes("rule") || k.includes("module"))
      .sort(([,a], [,b]) => b - a)
      .slice(0, 15);
    if (interesting.length > 0) {
      console.log("\n=== Key counters:");
      for (const [k, v] of interesting) {
        console.log(`    ${k}: ${v}`);
      }
    }
  }
}

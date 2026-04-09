#!/usr/bin/env node

/**
 * Direct A/B comparison of the isolated NoUnused.Exports family
 * (interpreted path, no host experiments) between the baseline and
 * kernel-Dict/Set interpreter bundles.
 *
 * Rebuilds workspaces the same way as importers-family-benchmark.mjs but
 * runs each scenario against both dist/review-runner-bench-baseline.mjs
 * and dist/review-runner-bench.mjs, capturing wall time and traced stages.
 */

import { spawnSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { performance } from "node:perf_hooks";

const repoRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), "..");
const srcRoot = path.join(repoRoot, "src");
const baseReviewDir = path.join(repoRoot, "bench", "review");
const baselineRunnerPath = path.join(repoRoot, "dist", "review-runner-bench-baseline.mjs");
const kernelRunnerPath = path.join(repoRoot, "dist", "review-runner-bench.mjs");
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

const ruleSet = process.argv[2] || "exports";

const reviewConfigs = {
  exports: `module ReviewConfig exposing (config)

import NoUnused.Exports
import Review.Rule as Rule exposing (Rule)


config : List Rule
config =
    [ NoUnused.Exports.rule ]
`,
  parameters: `module ReviewConfig exposing (config)

import NoUnused.Parameters
import Review.Rule as Rule exposing (Rule)


config : List Rule
config =
    [ NoUnused.Parameters.rule ]
`,
};

const reviewConfig = reviewConfigs[ruleSet];
if (!reviewConfig) {
  console.error(`Unknown rule set: ${ruleSet}`);
  process.exit(2);
}

function ensureDir(dirPath) {
  fs.mkdirSync(dirPath, { recursive: true });
}

function writeElmJson(workspaceRoot) {
  const rootElmJson = JSON.parse(fs.readFileSync(path.join(repoRoot, "elm.json"), "utf8"));
  rootElmJson["source-directories"] = ["src"];
  fs.writeFileSync(path.join(workspaceRoot, "elm.json"), JSON.stringify(rootElmJson, null, 2));
}

function prepareWorkspace(tag) {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), `elm-build2-importers-kernel-ab-${tag}-`));
  const fixtureSrcDir = path.join(root, "src");
  const buildDir = path.join(root, ".elm-review-build");
  ensureDir(fixtureSrcDir);
  ensureDir(buildDir);
  for (const fileName of fixtureFiles) {
    fs.copyFileSync(path.join(srcRoot, fileName), path.join(fixtureSrcDir, fileName));
  }
  writeElmJson(root);
  return {
    root,
    fixtureSrcDir,
    buildDir,
    mathLibPath: path.join(fixtureSrcDir, "MathLib.elm"),
  };
}

function prepareReviewDir() {
  const reviewRoot = fs.mkdtempSync(path.join(os.tmpdir(), "elm-build2-importers-kernel-review-"));
  ensureDir(path.join(reviewRoot, "src"));
  fs.copyFileSync(path.join(baseReviewDir, "elm.json"), path.join(reviewRoot, "elm.json"));
  fs.writeFileSync(path.join(reviewRoot, "src", "ReviewConfig.elm"), reviewConfig);
  return reviewRoot;
}

function runRunner(runnerPath, { fixtureSrcDir, buildDir, root }, reviewDir, name) {
  const tracePath = path.join(root, `${name}.trace.json`);
  const start = performance.now();
  const result = spawnSync(
    "node",
    [
      runnerPath,
      "--review-dir",
      reviewDir,
      "--source-dirs",
      fixtureSrcDir,
      "--build",
      buildDir,
      "--jobs",
      jobs,
      "--importers-cache-mode",
      "fresh",
      "--deps-cache-mode",
      "fresh",
      "--report",
      "quiet",
      "--perf-trace-json",
      tracePath,
    ],
    {
      cwd: repoRoot,
      stdio: ["ignore", "ignore", "ignore"],
    }
  );
  const wallMs = performance.now() - start;
  if (!fs.existsSync(tracePath)) {
    return { name, wall_ms: Math.round(wallMs), error: `no trace (exit ${result.status})` };
  }
  const trace = JSON.parse(fs.readFileSync(tracePath, "utf8"));
  const stage = (stageName) => trace.stages.find((entry) => entry.name === stageName)?.ms ?? 0;
  const counter = (key) => trace.counters[key] ?? 0;
  return {
    name,
    wall_ms: Math.round(wallMs),
    errors_total: counter("errors.total"),
    load_review_project_ms: stage("load_review_project"),
    module_rule_eval_ms: stage("module_rule_eval"),
    project_rule_eval_ms: stage("project_rule_eval"),
    importers_eval_total_ms: counter("project.importers.eval_total_ms"),
  };
}

function mutateImportGraphEdit(mathLibPath, originalSource) {
  fs.writeFileSync(
    mathLibPath,
    originalSource.replace("-}\n\n", "-}\n\nimport ProjectSources\n\n")
  );
}

function runOne(runnerPath, tag) {
  const workspace = prepareWorkspace(tag);
  const reviewDir = prepareReviewDir();
  const originalMathLib = fs.readFileSync(workspace.mathLibPath, "utf8");
  const cold = runRunner(runnerPath, workspace, reviewDir, "cold");
  const warm = runRunner(runnerPath, workspace, reviewDir, "warm");
  mutateImportGraphEdit(workspace.mathLibPath, originalMathLib);
  const importGraph = runRunner(runnerPath, workspace, reviewDir, "warm_import_graph_change");
  return {
    tag,
    runner: runnerPath,
    scenarios: [cold, warm, importGraph],
  };
}

function main() {
  const results = [];
  console.error("Running baseline (pre-kernel Dict/Set) ...");
  results.push(runOne(baselineRunnerPath, "baseline"));
  console.error("Running kernel Dict/Set ...");
  results.push(runOne(kernelRunnerPath, "kernel"));
  console.log(JSON.stringify({ family: ruleSet + " interpreted", results }, null, 2));
}

main();

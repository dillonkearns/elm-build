#!/usr/bin/env node

/**
 * Capture a Node --cpu-prof of the warm_import_graph_change run on
 * NoUnused.Parameters using the kernel-Dict/Set review-runner bundle.
 *
 * Preps a workspace, does a cold + warm pair (no profiling) to seed
 * caches, mutates MathLib.elm to trigger an import-graph invalidation,
 * then runs the third invocation with --cpu-prof so we only capture
 * the partial-miss hot path.
 *
 * Usage:
 *   node bench/profile-kernel-parameters.mjs [--output-dir <dir>]
 */

import { spawnSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { performance } from "node:perf_hooks";

const repoRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), "..");
const srcRoot = path.join(repoRoot, "src");
const baseReviewDir = path.join(repoRoot, "bench", "review");
const runnerPath = path.join(repoRoot, "dist", "review-runner-bench.mjs");
const jobs = String(os.cpus().length);

const outputDirArgIndex = process.argv.indexOf("--output-dir");
const outputDir =
  outputDirArgIndex !== -1 && process.argv[outputDirArgIndex + 1]
    ? path.resolve(process.argv[outputDirArgIndex + 1])
    : path.join(repoRoot, "bench", "profiles", "kernel-parameters");

fs.mkdirSync(outputDir, { recursive: true });

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

const reviewConfig = `module ReviewConfig exposing (config)

import NoUnused.Parameters
import Review.Rule as Rule exposing (Rule)


config : List Rule
config =
    [ NoUnused.Parameters.rule ]
`;

function ensureDir(d) {
  fs.mkdirSync(d, { recursive: true });
}

function writeElmJson(workspaceRoot) {
  const rootElmJson = JSON.parse(fs.readFileSync(path.join(repoRoot, "elm.json"), "utf8"));
  rootElmJson["source-directories"] = ["src"];
  fs.writeFileSync(path.join(workspaceRoot, "elm.json"), JSON.stringify(rootElmJson, null, 2));
}

function prepareWorkspace() {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "elm-build2-profile-params-"));
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
  const reviewRoot = fs.mkdtempSync(path.join(os.tmpdir(), "elm-build2-profile-params-review-"));
  ensureDir(path.join(reviewRoot, "src"));
  fs.copyFileSync(path.join(baseReviewDir, "elm.json"), path.join(reviewRoot, "elm.json"));
  fs.writeFileSync(path.join(reviewRoot, "src", "ReviewConfig.elm"), reviewConfig);
  return reviewRoot;
}

function runRunner({ fixtureSrcDir, buildDir, root }, reviewDir, name, { profile } = {}) {
  const tracePath = path.join(root, `${name}.trace.json`);
  const nodeArgs = profile
    ? [
        "--cpu-prof",
        "--cpu-prof-dir",
        outputDir,
        "--cpu-prof-name",
        `${name}.cpuprofile`,
        // Sample every 10ms instead of the default 1ms so the profile
        // file stays well under the V8 string limit on long runs.
        "--cpu-prof-interval",
        "10000",
      ]
    : [];

  console.error(`[${name}] starting ${profile ? "(with --cpu-prof)" : ""}`);
  const start = performance.now();
  const result = spawnSync(
    "node",
    [
      ...nodeArgs,
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

  const trace = fs.existsSync(tracePath) ? JSON.parse(fs.readFileSync(tracePath, "utf8")) : null;
  const stage = (stageName) => trace?.stages.find((entry) => entry.name === stageName)?.ms ?? 0;

  return {
    name,
    wall_ms: Math.round(wallMs),
    load_review_project_ms: stage("load_review_project"),
    module_rule_eval_ms: stage("module_rule_eval"),
    project_rule_eval_ms: stage("project_rule_eval"),
    errors_total: trace?.counters?.["errors.total"] ?? null,
  };
}

function mutateImportGraphEdit(mathLibPath, originalSource) {
  fs.writeFileSync(
    mathLibPath,
    originalSource.replace("-}\n\n", "-}\n\nimport ProjectSources\n\n")
  );
}

function main() {
  const workspace = prepareWorkspace();
  const reviewDir = prepareReviewDir();
  const originalMathLib = fs.readFileSync(workspace.mathLibPath, "utf8");

  console.error(`workspace: ${workspace.root}`);
  console.error(`review:    ${reviewDir}`);
  console.error(`outputDir: ${outputDir}`);

  const cold = runRunner(workspace, reviewDir, "cold");
  const warm = runRunner(workspace, reviewDir, "warm");
  mutateImportGraphEdit(workspace.mathLibPath, originalMathLib);
  const importGraph = runRunner(workspace, reviewDir, "warm_import_graph_change", { profile: true });

  const summary = {
    workspace: workspace.root,
    outputDir,
    scenarios: [cold, warm, importGraph],
  };
  fs.writeFileSync(path.join(outputDir, "summary.json"), JSON.stringify(summary, null, 2));
  console.log(JSON.stringify(summary, null, 2));
}

main();

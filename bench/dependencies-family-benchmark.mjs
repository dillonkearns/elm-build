#!/usr/bin/env node

import { spawnSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { performance } from "node:perf_hooks";

const repoRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), "..");
const srcRoot = path.join(repoRoot, "src");
const baseReviewDir = path.join(repoRoot, "bench", "review");
const distRunnerPath = path.join(repoRoot, "dist", "review-runner-bench.mjs");
const jobs = String(os.cpus().length);
const hostVariablesExperiment = process.argv.includes("--host-variables-experiment");

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

import NoUnused.Variables
import Review.Rule as Rule exposing (Rule)


config : List Rule
config =
    [ NoUnused.Variables.rule ]
`;

function ensureDir(dirPath) {
  fs.mkdirSync(dirPath, { recursive: true });
}

function writeElmJson(workspaceRoot) {
  const rootElmJson = JSON.parse(fs.readFileSync(path.join(repoRoot, "elm.json"), "utf8"));
  rootElmJson["source-directories"] = ["src"];
  fs.writeFileSync(path.join(workspaceRoot, "elm.json"), JSON.stringify(rootElmJson, null, 2));
}

function prepareWorkspace() {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "elm-build2-dependencies-family-"));
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
  const reviewRoot = fs.mkdtempSync(path.join(os.tmpdir(), "elm-build2-dependencies-review-"));
  ensureDir(path.join(reviewRoot, "src"));
  fs.copyFileSync(path.join(baseReviewDir, "elm.json"), path.join(reviewRoot, "elm.json"));
  fs.writeFileSync(path.join(reviewRoot, "src", "ReviewConfig.elm"), reviewConfig);
  return reviewRoot;
}

function runRunner({ fixtureSrcDir, buildDir, root }, reviewDir, name, depsMode) {
  const tracePath = path.join(root, `${name}.trace.json`);
  console.error(`[${depsMode}] starting ${name}`);
  const start = performance.now();
  const result = spawnSync(
    "node",
    [
      distRunnerPath,
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
      depsMode,
      "--report",
      "quiet",
      "--perf-trace-json",
      tracePath,
      ...(hostVariablesExperiment ? ["--host-no-unused-variables-experiment"] : []),
    ],
    {
      cwd: repoRoot,
      stdio: ["ignore", "ignore", "ignore"],
    }
  );
  const wallMs = performance.now() - start;

  if (!fs.existsSync(tracePath)) {
    throw new Error(`${name} did not produce trace output (status ${result.status})`);
  }

  const trace = JSON.parse(fs.readFileSync(tracePath, "utf8"));
  const stage = (stageName) => trace.stages.find((entry) => entry.name === stageName)?.ms ?? 0;
  const counter = (key) => trace.counters[key] ?? 0;

  return {
    name,
    wall_ms: Math.round(wallMs),
    exit_code: result.status ?? 0,
    cache_decision: trace.cacheDecision,
    load_review_project_ms: stage("load_review_project"),
    module_rule_eval_ms: stage("module_rule_eval"),
    project_rule_eval_ms: stage("project_rule_eval"),
    deps_eval_total_ms: counter("project.deps.eval_total_ms"),
    deps_rule_cache_entries: counter("project.deps.rule_cache.entries"),
    deps_rule_cache_loaded_bytes: counter("project.deps.rule_cache.loaded_bytes"),
    deps_cache_hits: counter("project.deps.cache_hits"),
    deps_cache_misses: counter("project.deps.cache_misses"),
    deps_affected_modules: counter("project.deps.affected_modules"),
    deps_mode_split: counter("project.deps.mode.split"),
    deps_mode_fresh: counter("project.deps.mode.fresh"),
    project_rule_count: counter("rules.project.count"),
    module_rule_count: counter("rules.module.count"),
  };
}

function mutateImportGraphEdit(mathLibPath, originalSource) {
  fs.writeFileSync(
    mathLibPath,
    originalSource.replace("-}\n\n", "-}\n\nimport ProjectSources\n\n")
  );
}

function runMode(depsMode) {
  const workspace = prepareWorkspace();
  const reviewDir = prepareReviewDir();
  const originalMathLib = fs.readFileSync(workspace.mathLibPath, "utf8");

  const cold = runRunner(workspace, reviewDir, "cold", depsMode);
  const warm = runRunner(workspace, reviewDir, "warm", depsMode);
  mutateImportGraphEdit(workspace.mathLibPath, originalMathLib);
  const importGraph = runRunner(workspace, reviewDir, "warm_import_graph_change", depsMode);

  return {
    mode: depsMode,
    host_variables_experiment: hostVariablesExperiment,
    workspace: workspace.root,
    reviewDir,
    scenarios: [cold, warm, importGraph],
  };
}

function main() {
  const modeArgIndex = process.argv.indexOf("--mode");
  const selectedModes =
    modeArgIndex !== -1 && process.argv[modeArgIndex + 1]
      ? [process.argv[modeArgIndex + 1]]
      : ["fresh", "split", "auto"];

  const results = [];
  for (const mode of selectedModes) {
    results.push(runMode(mode));
  }

  console.log(JSON.stringify({ fixture: "small-12", family: "DependenciesOf", results }, null, 2));
}

main();

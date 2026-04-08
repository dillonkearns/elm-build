#!/usr/bin/env node

import { spawnSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { performance } from "node:perf_hooks";

const repoRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), "..");
const srcRoot = path.join(repoRoot, "src");
const reviewDir = path.join(repoRoot, "bench", "review");
const distRunnerPath = path.join(repoRoot, "dist", "review-runner-bench.mjs");
const jobs = String(os.cpus().length);
const scenarios = readArg("--scenarios", "cold,warm,warm_1_file_body_edit,warm_import_graph_change")
  .split(",")
  .map((value) => value.trim())
  .filter(Boolean);
const useHostImportersExperiments = process.argv.includes("--host-importers-experiments");

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

function ensureDir(dirPath) {
  fs.mkdirSync(dirPath, { recursive: true });
}

function readArg(flag, fallback) {
  const index = process.argv.indexOf(flag);
  return index >= 0 ? process.argv[index + 1] : fallback;
}

function hostExperimentArgs() {
  if (!useHostImportersExperiments) {
    return [];
  }

  return [
    "--host-no-unused-exports-experiment",
    "--host-no-unused-custom-type-constructors-experiment",
    "--host-no-unused-custom-type-constructor-args-experiment",
    "--host-no-unused-parameters-experiment",
    "--host-no-unused-variables-experiment",
  ];
}

function writeElmJson(workspaceRoot) {
  const rootElmJson = JSON.parse(fs.readFileSync(path.join(repoRoot, "elm.json"), "utf8"));
  rootElmJson["source-directories"] = [ "src" ];
  fs.writeFileSync(path.join(workspaceRoot, "elm.json"), JSON.stringify(rootElmJson, null, 2));
}

function prepareWorkspace() {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "elm-build2-direct-bench-"));
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

function runRunner({ fixtureSrcDir, buildDir, root }, name) {
  const tracePath = path.join(root, `${name}.trace.json`);
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
      "auto",
      "--deps-cache-mode",
      "auto",
      "--report",
      "quiet",
      "--perf-trace-json",
      tracePath,
      ...hostExperimentArgs(),
    ],
    {
      cwd: repoRoot,
      stdio: ["ignore", "ignore", "ignore"],
    }
  );
  const wallMs = performance.now() - start;

  if (!fs.existsSync(tracePath)) {
    console.error(result.stdout);
    console.error(result.stderr);
    throw new Error(`${name} did not produce trace output (status ${result.status})`);
  }

  const trace = JSON.parse(fs.readFileSync(tracePath, "utf8"));
  const stage = (stageName) => trace.stages.find((entry) => entry.name === stageName)?.ms ?? 0;

  return {
    name,
    wall_ms: Math.round(wallMs),
    exit_code: result.status ?? 0,
    cache_decision: trace.cacheDecision,
    load_review_project_ms: stage("load_review_project"),
    module_rule_eval_ms: stage("module_rule_eval"),
    project_rule_eval_ms: stage("project_rule_eval"),
    counters: trace.counters,
  };
}

function mutateBodyEdit(mathLibPath, originalSource) {
  fs.writeFileSync(
    mathLibPath,
    `${originalSource}\n\nbenchmarkBodyEdit__ : Int\nbenchmarkBodyEdit__ =\n    abs -7\n`
  );
}

function mutateImportGraphEdit(mathLibPath, originalSource) {
  fs.writeFileSync(
    mathLibPath,
    originalSource.replace("-}\n\n", "-}\n\nimport ProjectSources\n\n")
  );
}

function main() {
  const workspace = prepareWorkspace();
  const originalMathLib = fs.readFileSync(workspace.mathLibPath, "utf8");
  const scenarioResults = [];

  if (scenarios.includes("cold")) {
    scenarioResults.push(runRunner(workspace, "cold"));
  }

  if (scenarios.includes("warm")) {
    scenarioResults.push(runRunner(workspace, "warm"));
  }

  if (scenarios.includes("warm_1_file_body_edit")) {
    mutateBodyEdit(workspace.mathLibPath, originalMathLib);
    scenarioResults.push(runRunner(workspace, "warm_1_file_body_edit"));
    fs.writeFileSync(workspace.mathLibPath, originalMathLib);
    runRunner(workspace, "restore_after_body");
  }

  if (scenarios.includes("warm_import_graph_change")) {
    mutateImportGraphEdit(workspace.mathLibPath, originalMathLib);
    scenarioResults.push(runRunner(workspace, "warm_import_graph_change"));
  }

  console.log(
    JSON.stringify(
      {
        fixture: "small-12",
        host_importers_experiments: useHostImportersExperiments,
        scenarios_requested: scenarios,
        root: workspace.root,
        scenarios: scenarioResults,
      },
      null,
      2
    )
  );
}

main();

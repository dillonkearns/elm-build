#!/usr/bin/env node

import { spawnSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { performance } from "node:perf_hooks";

const repoRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), "..");
const srcRoot = path.join(repoRoot, "src");
const reviewDir = path.join(repoRoot, "bench", "review");
const elmPagesBin = path.join(repoRoot, "node_modules", ".bin", "elm-pages");
const jobs = os.cpus().length;

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

const includeHostImporters = !process.argv.includes("--no-host-importers");
const includeHostTypeAnnotations = !process.argv.includes("--no-host-type-annotations");
const includeHostShape = !process.argv.includes("--no-host-shape");
const includeHostDebug = !process.argv.includes("--no-host-debug");
const transportMode = readArg("--transport-mode", "handles");
const groupingModes = (readArg("--modes", "legacy"))
  .split(",")
  .map((value) => value.trim())
  .filter(Boolean);

function readArg(flag, fallback) {
  const index = process.argv.indexOf(flag);
  return index >= 0 ? process.argv[index + 1] : fallback;
}

function ensureDir(dirPath) {
  fs.mkdirSync(dirPath, { recursive: true });
}

function prepareWorkspace() {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "elm-build2-warm-body-probe-"));
  const fixtureSrcDir = path.join(root, "src");
  const buildDir = path.join(root, ".elm-review-build");

  ensureDir(fixtureSrcDir);
  ensureDir(buildDir);

  for (const fileName of fixtureFiles) {
    fs.copyFileSync(path.join(srcRoot, fileName), path.join(fixtureSrcDir, fileName));
  }

  const rootElmJson = JSON.parse(fs.readFileSync(path.join(repoRoot, "elm.json"), "utf8"));
  rootElmJson["source-directories"] = [ "src" ];
  fs.writeFileSync(path.join(root, "elm.json"), JSON.stringify(rootElmJson, null, 2));

  return {
    root,
    fixtureSrcDir,
    buildDir,
    mathLibPath: path.join(fixtureSrcDir, "MathLib.elm"),
  };
}

function runRunner(workspace, scenarioName, groupingMode) {
  const tracePath = path.join(workspace.root, `${groupingMode}-${scenarioName}.trace.json`);
  const start = performance.now();
  const runnerConfig = {
    reviewDir,
    sourceDirs: [workspace.fixtureSrcDir],
    buildDirectory: workspace.buildDir,
    jobs,
    moduleRuleGroupingMode: groupingMode,
    moduleRuleTransportMode: transportMode,
    importersCacheMode: "auto",
    depsCacheMode: "auto",
    reportFormat: "quiet",
    perfTraceJson: tracePath,
    hostNoUnusedExportsExperiment: includeHostImporters,
    hostNoUnusedCustomTypeConstructorsExperiment: includeHostImporters,
    hostNoUnusedCustomTypeConstructorArgsExperiment: includeHostImporters,
    hostNoUnusedParametersExperiment: includeHostImporters,
    hostNoUnusedVariablesExperiment: includeHostImporters,
    hostNoExposingEverythingExperiment: includeHostShape,
    hostNoImportingEverythingExperiment: includeHostShape,
    hostNoDebugLogExperiment: includeHostDebug,
    hostNoDebugTodoOrToStringExperiment: includeHostDebug,
    hostNoMissingTypeAnnotationExperiment: includeHostTypeAnnotations,
    hostNoMissingTypeAnnotationInLetInExperiment: includeHostTypeAnnotations,
  };

  const result = spawnSync("node", [elmPagesBin, "run", "src/ReviewRunnerFast.elm"], {
    cwd: repoRoot,
    encoding: "utf8",
    env: {
      ...process.env,
      REVIEW_RUNNER_CONFIG_JSON: JSON.stringify(runnerConfig),
    },
  });
  const wallMs = Math.round(performance.now() - start);

  if (!fs.existsSync(tracePath)) {
    console.error(result.stdout);
    console.error(result.stderr);
    throw new Error(`${scenarioName} (${groupingMode}) did not produce trace output`);
  }

  const trace = JSON.parse(fs.readFileSync(tracePath, "utf8"));
  const stage = (stageName) => trace.stages.find((entry) => entry.name === stageName)?.ms ?? 0;

  return {
    scenario: scenarioName,
    grouping_mode: groupingMode,
    wall_ms: wallMs,
    cache_decision: trace.cacheDecision,
    load_review_project_ms: stage("load_review_project"),
    module_rule_eval_ms: stage("module_rule_eval"),
    project_rule_eval_ms: stage("project_rule_eval"),
    persist_decl_cache_ms: stage("persist_decl_cache"),
    load_decl_cache_ms: stage("load_decl_cache"),
    counters: trace.counters,
  };
}

function mutateBodyEdit(mathLibPath, originalSource) {
  fs.writeFileSync(
    mathLibPath,
    `${originalSource}\n\nbenchmarkBodyEdit__ : Int\nbenchmarkBodyEdit__ =\n    abs -7\n`
  );
}

function runProbe(groupingMode) {
  const workspace = prepareWorkspace();
  const originalMathLib = fs.readFileSync(workspace.mathLibPath, "utf8");
  const warmSeed = runRunner(workspace, "warm_seed", groupingMode);
  mutateBodyEdit(workspace.mathLibPath, originalMathLib);
  const warmBodyEdit = runRunner(workspace, "warm_body_edit", groupingMode);

  return {
    grouping_mode: groupingMode,
    transport_mode: transportMode,
    root: workspace.root,
    warm_seed: warmSeed,
    warm_body_edit: warmBodyEdit,
  };
}

function main() {
  const results = groupingModes.map(runProbe);

  console.log(JSON.stringify({ results }, null, 2));
}

main();

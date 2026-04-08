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

const reviewConfigs = {
  all: `module ReviewConfig exposing (config)

import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Exports
import NoUnused.Parameters
import Review.Rule as Rule exposing (Rule)


config : List Rule
config =
    [ NoUnused.Parameters.rule
    , NoUnused.Exports.rule
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.CustomTypeConstructorArgs.rule
    ]
`,
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
  constructors: `module ReviewConfig exposing (config)

import NoUnused.CustomTypeConstructors
import Review.Rule as Rule exposing (Rule)


config : List Rule
config =
    [ NoUnused.CustomTypeConstructors.rule [] ]
`,
  constructorArgs: `module ReviewConfig exposing (config)

import NoUnused.CustomTypeConstructorArgs
import Review.Rule as Rule exposing (Rule)


config : List Rule
config =
    [ NoUnused.CustomTypeConstructorArgs.rule ]
`,
};

function ensureDir(dirPath) {
  fs.mkdirSync(dirPath, { recursive: true });
}

function writeElmJson(workspaceRoot) {
  const rootElmJson = JSON.parse(fs.readFileSync(path.join(repoRoot, "elm.json"), "utf8"));
  rootElmJson["source-directories"] = ["src"];
  fs.writeFileSync(path.join(workspaceRoot, "elm.json"), JSON.stringify(rootElmJson, null, 2));
}

function prepareWorkspace() {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "elm-build2-importers-family-"));
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

function prepareReviewDir(ruleSet) {
  const reviewRoot = fs.mkdtempSync(path.join(os.tmpdir(), "elm-build2-importers-review-"));
  ensureDir(path.join(reviewRoot, "src"));
  fs.copyFileSync(path.join(baseReviewDir, "elm.json"), path.join(reviewRoot, "elm.json"));
  fs.writeFileSync(path.join(reviewRoot, "src", "ReviewConfig.elm"), reviewConfigs[ruleSet]);
  return reviewRoot;
}

function runRunner({ fixtureSrcDir, buildDir, root }, reviewDir, name, importersMode) {
  const useHostNoUnusedExports = process.argv.includes("--host-no-unused-exports-experiment");
  const useHostNoUnusedCustomTypeConstructors = process.argv.includes("--host-no-unused-custom-type-constructors-experiment");
  const useHostNoUnusedCustomTypeConstructorArgs = process.argv.includes("--host-no-unused-custom-type-constructor-args-experiment");
  const useHostNoUnusedParameters = process.argv.includes("--host-no-unused-parameters-experiment");
  const tracePath = path.join(root, `${name}.trace.json`);
  console.error(`[${importersMode}] starting ${name}`);
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
      importersMode,
      "--deps-cache-mode",
      "fresh",
      "--report",
      "quiet",
      "--perf-trace-json",
      tracePath,
      ...(useHostNoUnusedExports ? ["--host-no-unused-exports-experiment"] : []),
      ...(useHostNoUnusedCustomTypeConstructors ? ["--host-no-unused-custom-type-constructors-experiment"] : []),
      ...(useHostNoUnusedCustomTypeConstructorArgs ? ["--host-no-unused-custom-type-constructor-args-experiment"] : []),
      ...(useHostNoUnusedParameters ? ["--host-no-unused-parameters-experiment"] : []),
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
    importers_eval_total_ms: counter("project.importers.eval_total_ms"),
    importers_rule_cache_entries: counter("project.importers.rule_cache.entries"),
    importers_rule_cache_loaded_bytes: counter("project.importers.rule_cache.loaded_bytes"),
    importers_cache_hits: counter("project.importers.cache_hits"),
    importers_cache_misses: counter("project.importers.cache_misses"),
    importers_affected_modules: counter("project.importers.affected_modules"),
    importers_mode_split: counter("project.importers.mode.split"),
    importers_mode_fresh: counter("project.importers.mode.fresh"),
    host_no_unused_exports_enabled: counter("project.host_exports.enabled"),
    host_no_unused_exports_ms: counter("project.host_exports.ms"),
    host_no_unused_exports_errors: counter("project.host_exports.errors"),
    host_no_unused_custom_type_constructors_enabled: counter("project.host_constructors.enabled"),
    host_no_unused_custom_type_constructors_ms: counter("project.host_constructors.ms"),
    host_no_unused_custom_type_constructors_errors: counter("project.host_constructors.errors"),
    host_no_unused_custom_type_constructor_args_enabled: counter("project.host_constructor_args.enabled"),
    host_no_unused_custom_type_constructor_args_ms: counter("project.host_constructor_args.ms"),
    host_no_unused_custom_type_constructor_args_errors: counter("project.host_constructor_args.errors"),
    host_no_unused_parameters_enabled: counter("project.host_parameters.enabled"),
    host_no_unused_parameters_ms: counter("project.host_parameters.ms"),
    host_no_unused_parameters_errors: counter("project.host_parameters.errors"),
  };
}

function mutateImportGraphEdit(mathLibPath, originalSource) {
  fs.writeFileSync(
    mathLibPath,
    originalSource.replace("-}\n\n", "-}\n\nimport ProjectSources\n\n")
  );
}

function runMode(importersMode, ruleSet) {
  const workspace = prepareWorkspace();
  const reviewDir = prepareReviewDir(ruleSet);
  const originalMathLib = fs.readFileSync(workspace.mathLibPath, "utf8");

  const cold = runRunner(workspace, reviewDir, "cold", importersMode);
  const warm = runRunner(workspace, reviewDir, "warm", importersMode);
  mutateImportGraphEdit(workspace.mathLibPath, originalMathLib);
  const importGraph = runRunner(workspace, reviewDir, "warm_import_graph_change", importersMode);

  return {
    mode: importersMode,
    ruleSet,
    workspace: workspace.root,
    reviewDir,
    scenarios: [cold, warm, importGraph],
  };
}

function main() {
  const modeArgIndex = process.argv.indexOf("--mode");
  const rulesArgIndex = process.argv.indexOf("--rules");
  const selectedModes =
    modeArgIndex !== -1 && process.argv[modeArgIndex + 1]
      ? [process.argv[modeArgIndex + 1]]
      : ["fresh", "split"];
  const selectedRuleSets =
    rulesArgIndex !== -1 && process.argv[rulesArgIndex + 1]
      ? [process.argv[rulesArgIndex + 1]]
      : ["all"];

  const results = [];
  for (const ruleSet of selectedRuleSets) {
    for (const mode of selectedModes) {
      results.push(runMode(mode, ruleSet));
    }
  }

  console.log(JSON.stringify({ fixture: "small-12", family: "ImportersOf", results }, null, 2));
}

main();

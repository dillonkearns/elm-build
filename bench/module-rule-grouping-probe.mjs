#!/usr/bin/env node

import { spawnSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { performance } from "node:perf_hooks";

const repoRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), "..");
const srcRoot = path.join(repoRoot, "src");
const baseReviewDir = path.join(repoRoot, "bench", "review");
const fastRunnerPath = path.join(repoRoot, "dist", "review-runner-fast-bench.mjs");
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

const selectedRules = [
  {
    importLine: "import NoExposingEverything",
    ruleExpr: "NoExposingEverything.rule",
  },
  {
    importLine: "import NoImportingEverything",
    ruleExpr: "NoImportingEverything.rule []",
  },
  {
    importLine: "import NoMissingTypeAnnotation",
    ruleExpr: "NoMissingTypeAnnotation.rule",
  },
  {
    importLine: "import NoMissingTypeAnnotationInLetIn",
    ruleExpr: "NoMissingTypeAnnotationInLetIn.rule",
  },
  {
    importLine: "import NoUnused.Patterns",
    ruleExpr: "NoUnused.Patterns.rule",
  },
];

function ensureDir(dirPath) {
  fs.mkdirSync(dirPath, { recursive: true });
}

function readArg(flag, fallback) {
  const index = process.argv.indexOf(flag);
  return index >= 0 ? process.argv[index + 1] : fallback;
}

const groupingModes = readArg("--modes", "legacy,auto")
  .split(",")
  .map((value) => value.trim())
  .filter(Boolean);

function writeElmJson(workspaceRoot) {
  const rootElmJson = JSON.parse(fs.readFileSync(path.join(repoRoot, "elm.json"), "utf8"));
  rootElmJson["source-directories"] = [ "src" ];
  fs.writeFileSync(path.join(workspaceRoot, "elm.json"), JSON.stringify(rootElmJson, null, 2));
}

function prepareWorkspace() {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "elm-build2-module-grouping-"));
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
  const reviewRoot = fs.mkdtempSync(path.join(os.tmpdir(), "elm-build2-module-grouping-review-"));
  ensureDir(path.join(reviewRoot, "src"));
  fs.copyFileSync(path.join(baseReviewDir, "elm.json"), path.join(reviewRoot, "elm.json"));
  fs.writeFileSync(
    path.join(reviewRoot, "src", "ReviewConfig.elm"),
    `module ReviewConfig exposing (config)

${selectedRules.map((rule) => rule.importLine).join("\n")}
import Review.Rule as Rule exposing (Rule)


config : List Rule
config =
    [ ${selectedRules.map((rule) => rule.ruleExpr).join("\n    , ")} ]
`
  );
  return reviewRoot;
}

function runRunner(workspace, reviewDir, scenarioName, groupingMode) {
  const tracePath = path.join(workspace.root, `${groupingMode}-${scenarioName}.trace.json`);
  const runnerConfig = {
    reviewDir,
    sourceDirs: [workspace.fixtureSrcDir],
    buildDirectory: workspace.buildDir,
    jobs: Number(jobs),
    moduleRuleGroupingMode: groupingMode,
    importersCacheMode: "fresh",
    depsCacheMode: "fresh",
    reportFormat: "quiet",
    perfTraceJson: tracePath,
  };

  const start = performance.now();
  const result = spawnSync("node", [fastRunnerPath], {
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
    errors_total: trace.counters["errors.total"] ?? 0,
    module_rule_count: trace.counters["rules.module.count"] ?? 0,
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
  const reviewDir = prepareReviewDir();
  const originalMathLib = fs.readFileSync(workspace.mathLibPath, "utf8");

  const warmSeed = runRunner(workspace, reviewDir, "warm_seed", groupingMode);
  mutateBodyEdit(workspace.mathLibPath, originalMathLib);
  const warmBodyEdit = runRunner(workspace, reviewDir, "warm_body_edit", groupingMode);

  return {
    grouping_mode: groupingMode,
    warm_seed: warmSeed,
    warm_body_edit: warmBodyEdit,
  };
}

function main() {
  console.log(
    JSON.stringify(
      {
        results: groupingModes.map(runProbe),
      },
      null,
      2
    )
  );
}

main();

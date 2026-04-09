#!/usr/bin/env node

import { spawnSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { performance } from "node:perf_hooks";

const repoRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), "..");
const srcRoot = path.join(repoRoot, "src");
const baseReviewDir = path.join(repoRoot, "bench", "review");
const elmPagesBin = path.join(repoRoot, "node_modules", ".bin", "elm-pages");
const useFastEntrypoint = process.argv.includes("--fast-entrypoint");
const distRunnerPath = path.join(
  repoRoot,
  "dist",
  useFastEntrypoint ? "review-runner-fast-bench.mjs" : "review-runner-bench.mjs"
);
const jobs = String(os.cpus().length);
const useHostTypeAnnotationExperiments = process.argv.includes("--host-type-annotation-experiments");
const useHostDebugExperiments = process.argv.includes("--host-debug-experiments");
const useHostShapeExperiments = process.argv.includes("--host-shape-experiments");

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

const moduleRules = {
  "NoDebug.Log": {
    importLine: "import NoDebug.Log",
    ruleExpr: "NoDebug.Log.rule",
  },
  "NoDebug.TodoOrToString": {
    importLine: "import NoDebug.TodoOrToString",
    ruleExpr: "NoDebug.TodoOrToString.rule",
  },
  "NoExposingEverything": {
    importLine: "import NoExposingEverything",
    ruleExpr: "NoExposingEverything.rule",
  },
  "NoImportingEverything": {
    importLine: "import NoImportingEverything",
    ruleExpr: "NoImportingEverything.rule []",
  },
  "NoMissingTypeAnnotation": {
    importLine: "import NoMissingTypeAnnotation",
    ruleExpr: "NoMissingTypeAnnotation.rule",
  },
  "NoMissingTypeAnnotationInLetIn": {
    importLine: "import NoMissingTypeAnnotationInLetIn",
    ruleExpr: "NoMissingTypeAnnotationInLetIn.rule",
  },
  "NoUnused.Patterns": {
    importLine: "import NoUnused.Patterns",
    ruleExpr: "NoUnused.Patterns.rule",
  },
};

const selectedRuleName = process.argv.includes("--rule")
  ? process.argv[process.argv.indexOf("--rule") + 1]
  : null;

function ensureDir(dirPath) {
  fs.mkdirSync(dirPath, { recursive: true });
}

function writeElmJson(workspaceRoot) {
  const rootElmJson = JSON.parse(fs.readFileSync(path.join(repoRoot, "elm.json"), "utf8"));
  rootElmJson["source-directories"] = ["src"];
  fs.writeFileSync(path.join(workspaceRoot, "elm.json"), JSON.stringify(rootElmJson, null, 2));
}

function prepareWorkspace() {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "elm-build2-module-rules-"));
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

function prepareReviewDir(ruleName, rule) {
  const reviewRoot = fs.mkdtempSync(path.join(os.tmpdir(), "elm-build2-module-rules-review-"));
  ensureDir(path.join(reviewRoot, "src"));
  fs.copyFileSync(path.join(baseReviewDir, "elm.json"), path.join(reviewRoot, "elm.json"));
  fs.writeFileSync(
    path.join(reviewRoot, "src", "ReviewConfig.elm"),
    `module ReviewConfig exposing (config)

${rule.importLine}
import Review.Rule as Rule exposing (Rule)


config : List Rule
config =
    [ ${rule.ruleExpr} ]
`
  );
  return reviewRoot;
}

function hostExperimentArgs(ruleName) {
  if (!useHostTypeAnnotationExperiments) {
    return [];
  }

  if (ruleName === "NoMissingTypeAnnotation") {
    return ["--host-no-missing-type-annotation-experiment"];
  }

  if (ruleName === "NoMissingTypeAnnotationInLetIn") {
    return ["--host-no-missing-type-annotation-in-let-in-experiment"];
  }

  if (useHostDebugExperiments && ruleName === "NoDebug.Log") {
    return ["--host-no-debug-log-experiment"];
  }

  if (useHostDebugExperiments && ruleName === "NoDebug.TodoOrToString") {
    return ["--host-no-debug-todo-or-to-string-experiment"];
  }

  if (useHostShapeExperiments && ruleName === "NoExposingEverything") {
    return ["--host-no-exposing-everything-experiment"];
  }

  if (useHostShapeExperiments && ruleName === "NoImportingEverything") {
    return ["--host-no-importing-everything-experiment"];
  }

  return [];
}

function runRunner({ fixtureSrcDir, buildDir, root }, reviewDir, scenarioName, ruleName) {
  const tracePath = path.join(root, `${scenarioName}.trace.json`);
  const start = performance.now();
  const runnerConfig = {
    reviewDir,
    sourceDirs: [fixtureSrcDir],
    buildDirectory: buildDir,
    jobs: Number(jobs),
    memoizedFunctions: [],
    memoProfile: false,
    importersCacheMode: "fresh",
    depsCacheMode: "fresh",
    reportFormat: "quiet",
    perfTraceJson: tracePath,
    hostNoUnusedExportsExperiment: false,
    hostNoUnusedCustomTypeConstructorsExperiment: false,
    hostNoUnusedCustomTypeConstructorArgsExperiment: false,
    hostNoUnusedParametersExperiment: false,
    hostNoUnusedVariablesExperiment: false,
    hostNoExposingEverythingExperiment:
      useHostShapeExperiments && ruleName === "NoExposingEverything",
    hostNoImportingEverythingExperiment:
      useHostShapeExperiments && ruleName === "NoImportingEverything",
    hostNoDebugLogExperiment: useHostDebugExperiments && ruleName === "NoDebug.Log",
    hostNoDebugTodoOrToStringExperiment:
      useHostDebugExperiments && ruleName === "NoDebug.TodoOrToString",
    hostNoMissingTypeAnnotationExperiment:
      useHostTypeAnnotationExperiments && ruleName === "NoMissingTypeAnnotation",
    hostNoMissingTypeAnnotationInLetInExperiment:
      useHostTypeAnnotationExperiments && ruleName === "NoMissingTypeAnnotationInLetIn",
  };

  const result = useFastEntrypoint
    ? spawnSync("node", [elmPagesBin, "run", "src/ReviewRunnerFast.elm"], {
        cwd: repoRoot,
        encoding: "utf8",
        env: {
          ...process.env,
          REVIEW_RUNNER_CONFIG_JSON: JSON.stringify(runnerConfig),
        },
      })
    : spawnSync(
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
          "fresh",
          "--report",
          "quiet",
          "--perf-trace-json",
          tracePath,
          ...hostExperimentArgs(ruleName),
        ],
        {
          cwd: repoRoot,
          encoding: "utf8",
        }
      );
  const wallMs = performance.now() - start;

  if (!fs.existsSync(tracePath)) {
    console.error(result.stdout);
    console.error(result.stderr);
    throw new Error(`${scenarioName} did not produce trace output (status ${result.status})`);
  }

  const trace = JSON.parse(fs.readFileSync(tracePath, "utf8"));
  const stage = (stageName) => trace.stages.find((entry) => entry.name === stageName)?.ms ?? 0;

  return {
    name: scenarioName,
    wall_ms: Math.round(wallMs),
    exit_code: result.status ?? 0,
    cache_decision: trace.cacheDecision,
    load_review_project_ms: stage("load_review_project"),
    module_rule_eval_ms: stage("module_rule_eval"),
    project_rule_eval_ms: stage("project_rule_eval"),
    stale_files: trace.counters["stale.files"] ?? 0,
    errors_total: trace.counters["errors.total"] ?? 0,
  };
}

function mutateBodyEdit(mathLibPath, originalSource) {
  fs.writeFileSync(
    mathLibPath,
    `${originalSource}\n\nbenchmarkBodyEdit__ : Int\nbenchmarkBodyEdit__ =\n    abs -7\n`
  );
}

function runRule(ruleName, rule) {
  const workspace = prepareWorkspace();
  const reviewDir = prepareReviewDir(ruleName, rule);
  const originalMathLib = fs.readFileSync(workspace.mathLibPath, "utf8");

  const cold = runRunner(workspace, reviewDir, "cold", ruleName);
  const warm = runRunner(workspace, reviewDir, "warm", ruleName);
  mutateBodyEdit(workspace.mathLibPath, originalMathLib);
  const bodyEdit = runRunner(workspace, reviewDir, "warm_1_file_body_edit", ruleName);

  return {
    rule: ruleName,
    workspace: workspace.root,
    reviewDir,
    scenarios: [cold, warm, bodyEdit],
  };
}

function main() {
  const selectedEntries =
    selectedRuleName == null
      ? Object.entries(moduleRules)
      : Object.entries(moduleRules).filter(([ruleName]) => ruleName === selectedRuleName);

  if (selectedEntries.length === 0) {
    throw new Error(`Unknown rule '${selectedRuleName}'`);
  }

  const results = selectedEntries.map(([ruleName, rule]) => runRule(ruleName, rule));
  console.log(JSON.stringify({ fixture: "small-12", family: "ModuleRules", results }, null, 2));
}

main();

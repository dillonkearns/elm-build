#!/usr/bin/env node

import { execFileSync, spawnSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { performance } from "node:perf_hooks";

const repoRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), "..");
const srcRoot = path.join(repoRoot, "src");
const reviewDir = path.join(repoRoot, "bench", "review");
const distRunnerPath = path.join(repoRoot, "dist", "review-runner-bench.mjs");
const resultsPath = path.join(repoRoot, "bench", "results", "review-runner-scenarios.json");
const runId = `${Date.now().toString(36)}-${process.pid}`;
const fixtureFilter = process.argv.includes("--fixture")
  ? process.argv[process.argv.indexOf("--fixture") + 1]
  : null;
const skipBundle = process.argv.includes("--skip-bundle");
const importersCacheMode = process.argv.includes("--importers-cache-mode")
  ? process.argv[process.argv.indexOf("--importers-cache-mode") + 1]
  : "auto";
const depsCacheMode = process.argv.includes("--deps-cache-mode")
  ? process.argv[process.argv.indexOf("--deps-cache-mode") + 1]
  : "auto";
const jobs = String(os.cpus().length);
const hostImportersExperiments = process.argv.includes("--host-importers-experiments");
const envMode = process.argv.includes("--env-mode")
  ? process.argv[process.argv.indexOf("--env-mode") + 1]
  : "legacy-ast";

function hostExperimentArgs() {
  if (!hostImportersExperiments) {
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

const smallFixtureFiles = [
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

const largeFixtureExcludes = new Set([
  "Build.elm",
  "CoreExtraBenchmark.elm",
  "EvalDemo.elm",
  "InterpreterBenchmark.elm",
  "InterpreterMemoBenchmark.elm",
  "InterpreterTestRunner.elm",
  "MutationTestRunner.elm",
  "ParseVsEvalBenchmark.elm",
  "PureTestRunner.elm",
  "ReviewRunnerTest.elm",
  "RunCoreExtraTests.elm",
  "RunTests.elm",
  "SimpleTestRunner.elm",
  "TestRunner.elm",
]);

function largeFixtureFiles() {
  return fs
    .readdirSync(srcRoot)
    .filter((name) => name.endsWith(".elm"))
    .filter((name) => !name.endsWith("Tests.elm"))
    .filter((name) => !name.includes("Benchmark"))
    .filter((name) => !name.includes("TestRunner"))
    .filter((name) => !largeFixtureExcludes.has(name))
    .sort();
}

function ensureDir(dirPath) {
  fs.mkdirSync(dirPath, { recursive: true });
}

function writeFile(filePath, body) {
  ensureDir(path.dirname(filePath));
  fs.writeFileSync(filePath, body);
}

function copyFixture(fileNames, fixtureSrcDir) {
  ensureDir(fixtureSrcDir);

  for (const fileName of fileNames) {
    fs.copyFileSync(path.join(srcRoot, fileName), path.join(fixtureSrcDir, fileName));
  }
}

function writeElmJson(workspaceRoot) {
  const rootElmJson = JSON.parse(fs.readFileSync(path.join(repoRoot, "elm.json"), "utf8"));
  rootElmJson["source-directories"] = ["src"];
  fs.writeFileSync(path.join(workspaceRoot, "elm.json"), JSON.stringify(rootElmJson, null, 4));
}

function mutateBodyEdit(fixtureSrcDir) {
  const filePath = path.join(fixtureSrcDir, "MathLib.elm");
  const source = fs.readFileSync(filePath, "utf8");
  const mutated = `${source}\n\nbenchmarkBodyEdit__ : Int\nbenchmarkBodyEdit__ =\n    abs -7\n`;
  fs.writeFileSync(filePath, mutated);
}

function mutateCommentOnlyEdit(fixtureSrcDir) {
  const filePath = path.join(fixtureSrcDir, "MathLib.elm");
  const source = fs.readFileSync(filePath, "utf8");
  const mutated = `${source}\n\n{- benchmark comment only edit -}\n`;
  fs.writeFileSync(filePath, mutated);
}

function mutateImportGraphEdit(fixtureSrcDir) {
  const filePath = path.join(fixtureSrcDir, "MathLib.elm");
  const source = fs.readFileSync(filePath, "utf8");
  const marker = "-}\n\n";

  if (!source.includes(marker)) {
    throw new Error(`Could not find module doc block in ${filePath}`);
  }

  const mutated = source.replace(marker, "-}\n\nimport ProjectSources\n\n");
  fs.writeFileSync(filePath, mutated);
}

function bundleRunner() {
  ensureDir(path.dirname(distRunnerPath));
  execFileSync(
    "bunx",
    ["elm-pages", "bundle-script", "src/ReviewRunner.elm", "--output", distRunnerPath],
    { cwd: repoRoot, stdio: "inherit" }
  );
}

function runBundledReviewRunner({ fixtureSrcDir, buildDir, tracePath, importersCacheMode, depsCacheMode }) {
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
      importersCacheMode,
      "--deps-cache-mode",
      depsCacheMode,
      "--env-mode",
      envMode,
      "--perf-trace-json",
      tracePath,
      ...hostExperimentArgs(),
    ],
    {
      cwd: repoRoot,
      encoding: "utf8",
    }
  );
  const wallMs = performance.now() - start;

  if (!fs.existsSync(tracePath)) {
    throw new Error(
      `Expected perf trace at ${tracePath}, but it was not written.\nstdout:\n${result.stdout}\nstderr:\n${result.stderr}`
    );
  }

  const trace = JSON.parse(fs.readFileSync(tracePath, "utf8"));

  return {
    wallMs,
    exitCode: result.status ?? 0,
    stdout: result.stdout,
    stderr: result.stderr,
    trace,
  };
}

function stageMap(trace) {
  return Object.fromEntries(trace.stages.map((stage) => [stage.name, stage.ms]));
}

function sumStageMs(trace) {
  return trace.stages.reduce((total, stage) => total + stage.ms, 0);
}

function scenarioResult(name, runResult) {
  return {
    name,
    importers_cache_mode: importersCacheMode,
    deps_cache_mode: depsCacheMode,
    host_importers_experiments: hostImportersExperiments,
    wall_ms: Math.round(runResult.wallMs),
    internal_ms: sumStageMs(runResult.trace),
    exit_code: runResult.exitCode,
    cache_decision: runResult.trace.cacheDecision,
    stages: stageMap(runResult.trace),
    counters: runResult.trace.counters,
  };
}

function prepareScenarioWorkspace(fixtureName, scenarioName, fileNames) {
  const root = path.join(
    os.tmpdir(),
    "elm-build2-review-runner-bench",
    runId,
    fixtureName,
    scenarioName
  );
  fs.rmSync(root, { recursive: true, force: true });

  const fixtureSrcDir = path.join(root, "src");
  const buildDir = path.join(root, ".elm-review-build");
  const tracePath = path.join(root, "trace.json");

  copyFixture(fileNames, fixtureSrcDir);
  writeElmJson(root);
  ensureDir(buildDir);

  return { root, fixtureSrcDir, buildDir, tracePath };
}

function runScenarioSequence({ fixtureName, fileNames }) {
  const workspace = prepareScenarioWorkspace(fixtureName, "shared", fileNames);
  const mathLibPath = path.join(workspace.fixtureSrcDir, "MathLib.elm");
  const originalMathLib = fs.readFileSync(mathLibPath, "utf8");

  function restoreBaseline() {
    fs.writeFileSync(mathLibPath, originalMathLib);
  }

  function runTrace(traceName) {
    return runBundledReviewRunner({
      fixtureSrcDir: workspace.fixtureSrcDir,
      buildDir: workspace.buildDir,
      tracePath: path.join(workspace.root, traceName),
      importersCacheMode,
      depsCacheMode,
    });
  }

  const cold = scenarioResult(
    "cold",
    runTrace("cold-trace.json")
  );

  const warm = scenarioResult(
    "warm",
    runTrace("warm-trace.json")
  );

  mutateBodyEdit(workspace.fixtureSrcDir);
  const bodyEdit = scenarioResult(
    "warm_1_file_body_edit",
    runTrace("warm_1_file_body_edit.json")
  );
  restoreBaseline();
  runTrace("restore-after-body.json");

  mutateCommentOnlyEdit(workspace.fixtureSrcDir);
  const commentOnly = scenarioResult(
    "warm_1_file_comment_only",
    runTrace("warm_1_file_comment_only.json")
  );
  restoreBaseline();
  runTrace("restore-after-comment.json");

  mutateImportGraphEdit(workspace.fixtureSrcDir);
  const importGraph = scenarioResult(
    "warm_import_graph_change",
    runTrace("warm_import_graph_change.json")
  );
  restoreBaseline();

  return [ cold, warm, bodyEdit, commentOnly, importGraph ];
}

function fixtureResult(name, fileNames) {
  return {
    fixture: name,
    file_count: fileNames.length,
    scenarios: runScenarioSequence({ fixtureName: name, fileNames }),
  };
}

function printFixtureSummary(result) {
  console.log(`\nFixture: ${result.fixture} (${result.file_count} files)`);
  console.log(`Importers cache mode: ${importersCacheMode}`);
  console.log(`Deps cache mode: ${depsCacheMode}`);
  console.log("scenario                   wall(s)  internal(s)  decision");

  for (const scenario of result.scenarios) {
    const label = scenario.name.padEnd(26, " ");
    const wall = (scenario.wall_ms / 1000).toFixed(2).padStart(7, " ");
    const internal = (scenario.internal_ms / 1000).toFixed(2).padStart(11, " ");
    console.log(`${label} ${wall} ${internal}  ${scenario.cache_decision}`);
  }
}

function main() {
  if (!skipBundle) {
    bundleRunner();
  }

  const allFixtures = [
    { name: "small-12", fileNames: smallFixtureFiles },
    { name: "repo-large", fileNames: largeFixtureFiles() },
  ];

  const selectedFixtures = fixtureFilter
    ? allFixtures.filter((fixture) => fixture.name === fixtureFilter)
    : allFixtures;

  if (selectedFixtures.length === 0) {
    throw new Error(`Unknown fixture filter: ${fixtureFilter}`);
  }

  const results = {
    date: new Date().toISOString(),
    runner: path.relative(repoRoot, distRunnerPath),
    importers_cache_mode: importersCacheMode,
    deps_cache_mode: depsCacheMode,
    fixtures: selectedFixtures.map((fixture) => fixtureResult(fixture.name, fixture.fileNames)),
  };

  ensureDir(path.dirname(resultsPath));
  writeFile(resultsPath, `${JSON.stringify(results, null, 2)}\n`);

  for (const fixture of results.fixtures) {
    printFixtureSummary(fixture);
  }

  console.log(`\nWrote ${path.relative(repoRoot, resultsPath)}`);
}

main();

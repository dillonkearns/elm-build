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
const resultsPath = path.join(repoRoot, "bench", "results", "review-runner-scenario-bench.json");
const runId = `${Date.now().toString(36)}-${process.pid}`;

const fixtureName = readArg("--fixture", "small-12");
const scenarioName = readArg("--scenario", "warm_import_graph_change");
const importersModes = readArg("--importers-cache-modes", "fresh,split,auto")
  .split(",")
  .map((value) => value.trim())
  .filter(Boolean);
const depsCacheMode = readArg("--deps-cache-mode", "auto");
const repeatCount = Number.parseInt(readArg("--repeat", "5"), 10);
const skipBundle = process.argv.includes("--skip-bundle");
const jobs = String(os.cpus().length);
const hostImportersExperiments = process.argv.includes("--host-importers-experiments");

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

function readArg(flag, fallback) {
  const index = process.argv.indexOf(flag);
  return index >= 0 ? process.argv[index + 1] : fallback;
}

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

function fixtureFiles(name) {
  switch (name) {
    case "small-12":
      return smallFixtureFiles;

    case "repo-large":
      return largeFixtureFiles();

    default:
      throw new Error(`Unknown fixture: ${name}`);
  }
}

function ensureDir(dirPath) {
  fs.mkdirSync(dirPath, { recursive: true });
}

function removeDir(dirPath) {
  execFileSync("rm", ["-rf", dirPath]);
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

  return {
    wallMs,
    exitCode: result.status ?? 0,
    trace: JSON.parse(fs.readFileSync(tracePath, "utf8")),
  };
}

function stageMap(trace) {
  return Object.fromEntries(trace.stages.map((stage) => [stage.name, stage.ms]));
}

function sumStageMs(trace) {
  return trace.stages.reduce((total, stage) => total + stage.ms, 0);
}

function prepareTemplateWorkspace(fileNames, mode) {
  const modeRoot = path.join(
    os.tmpdir(),
    "elm-build2-review-runner-scenario-bench",
    runId,
    fixtureName,
    scenarioName,
    mode
  );

  fs.rmSync(modeRoot, { recursive: true, force: true });

  const workspaceRoot = path.join(modeRoot, "workspace");
  const snapshotRoot = path.join(modeRoot, "snapshot");

  const fixtureSrcDir = path.join(workspaceRoot, "src");
  const buildDir = path.join(workspaceRoot, ".elm-review-build");
  ensureDir(buildDir);
  copyFixture(fileNames, fixtureSrcDir);
  writeElmJson(workspaceRoot);

  const mathLibPath = path.join(fixtureSrcDir, "MathLib.elm");

  return {
    root: workspaceRoot,
    modeRoot,
    workspaceRoot,
    snapshotRoot,
    fixtureSrcDir,
    buildDir,
    baselineMathLib:
      fs.existsSync(mathLibPath) ? fs.readFileSync(mathLibPath, "utf8") : null,
  };
}

function snapshotWorkspace(templateWorkspace) {
  fs.rmSync(templateWorkspace.snapshotRoot, { recursive: true, force: true });
  ensureDir(templateWorkspace.snapshotRoot);
  fs.cpSync(
    templateWorkspace.fixtureSrcDir,
    path.join(templateWorkspace.snapshotRoot, "src"),
    { recursive: true }
  );
  fs.cpSync(
    templateWorkspace.buildDir,
    path.join(templateWorkspace.snapshotRoot, ".elm-review-build"),
    { recursive: true }
  );
}

function restoreWorkspaceFromSnapshot(templateWorkspace) {
  removeDir(templateWorkspace.fixtureSrcDir);
  removeDir(templateWorkspace.buildDir);
  fs.cpSync(
    path.join(templateWorkspace.snapshotRoot, "src"),
    templateWorkspace.fixtureSrcDir,
    { recursive: true }
  );
  fs.cpSync(
    path.join(templateWorkspace.snapshotRoot, ".elm-review-build"),
    templateWorkspace.buildDir,
    { recursive: true }
  );
}

function prepareScenarioWorkspaceFromTemplate(templateWorkspace) {
  return {
    root: templateWorkspace.workspaceRoot,
    fixtureSrcDir: templateWorkspace.fixtureSrcDir,
    buildDir: templateWorkspace.buildDir,
  };
}

function seedWarmState(workspace, mode) {
  runBundledReviewRunner({
    fixtureSrcDir: workspace.fixtureSrcDir,
    buildDir: workspace.buildDir,
    importersCacheMode: mode,
    depsCacheMode,
    tracePath: path.join(workspace.root, "cold-seed.json"),
  });

  if (scenarioName !== "cold") {
    runBundledReviewRunner({
      fixtureSrcDir: workspace.fixtureSrcDir,
      buildDir: workspace.buildDir,
      importersCacheMode: mode,
      depsCacheMode,
      tracePath: path.join(workspace.root, "warm-seed.json"),
    });
  }
}

function applyScenarioMutation(workspace) {
  switch (scenarioName) {
    case "cold":
    case "warm":
      return;

    case "warm_1_file_body_edit":
      mutateBodyEdit(workspace.fixtureSrcDir);
      return;

    case "warm_1_file_comment_only":
      mutateCommentOnlyEdit(workspace.fixtureSrcDir);
      return;

    case "warm_import_graph_change":
      mutateImportGraphEdit(workspace.fixtureSrcDir);
      return;

    default:
      throw new Error(`Unknown scenario: ${scenarioName}`);
  }
}

function restoreWarmBaseline(templateWorkspace, mode, repeatIndex) {
  if (templateWorkspace.baselineMathLib !== null) {
    fs.writeFileSync(
      path.join(templateWorkspace.fixtureSrcDir, "MathLib.elm"),
      templateWorkspace.baselineMathLib
    );
  }

  if (scenarioName !== "cold" && scenarioName !== "warm") {
    runBundledReviewRunner({
      fixtureSrcDir: templateWorkspace.fixtureSrcDir,
      buildDir: templateWorkspace.buildDir,
      importersCacheMode: mode,
      depsCacheMode,
      tracePath: path.join(templateWorkspace.root, `restore-repeat-${repeatIndex + 1}.json`),
    });
  }
}

function runSingleScenario(templateWorkspace, mode, repeatIndex) {
  const workspace = prepareScenarioWorkspaceFromTemplate(templateWorkspace);

  if (templateWorkspace.baselineMathLib !== null) {
    fs.writeFileSync(
      path.join(workspace.fixtureSrcDir, "MathLib.elm"),
      templateWorkspace.baselineMathLib
    );
  }

  applyScenarioMutation(workspace);

  const traceName =
    scenarioName === "cold"
      ? `cold-repeat-${repeatIndex + 1}.json`
      : `${scenarioName}-repeat-${repeatIndex + 1}.json`;

  const result = runBundledReviewRunner({
    fixtureSrcDir: workspace.fixtureSrcDir,
    buildDir: workspace.buildDir,
    importersCacheMode: mode,
    depsCacheMode,
    tracePath: path.join(workspace.root, traceName),
  });

  return {
    repeat: repeatIndex + 1,
    wall_ms: Math.round(result.wallMs),
    internal_ms: sumStageMs(result.trace),
    exit_code: result.exitCode,
    cache_decision: result.trace.cacheDecision,
    stages: stageMap(result.trace),
    counters: result.trace.counters,
  };
}

function summarizeRuns(runs) {
  const values = runs.map((run) => run.wall_ms).sort((a, b) => a - b);
  const sum = values.reduce((total, value) => total + value, 0);
  return {
    min_wall_ms: values[0],
    median_wall_ms: values[Math.floor(values.length / 2)],
    max_wall_ms: values[values.length - 1],
    avg_wall_ms: Math.round(sum / values.length),
  };
}

function printSummary(mode, runs, summary) {
  const toSeconds = (ms) => (ms / 1000).toFixed(2).padStart(7, " ");
  console.log(
    `${mode.padEnd(8, " ")} avg ${toSeconds(summary.avg_wall_ms)}  med ${toSeconds(
      summary.median_wall_ms
    )}  min ${toSeconds(summary.min_wall_ms)}  max ${toSeconds(summary.max_wall_ms)}`
  );

  for (const run of runs) {
    console.log(
      `  run ${String(run.repeat).padStart(2, " ")}  wall ${toSeconds(
        run.wall_ms
      )}  internal ${toSeconds(run.internal_ms)}  ${run.cache_decision}`
    );
  }
}

function main() {
  if (!skipBundle) {
    bundleRunner();
  }

  const fileNames = fixtureFiles(fixtureName);
  const results = {
    date: new Date().toISOString(),
    runner: path.relative(repoRoot, distRunnerPath),
    fixture: fixtureName,
    scenario: scenarioName,
    repeat: repeatCount,
    deps_cache_mode: depsCacheMode,
    host_importers_experiments: hostImportersExperiments,
    modes: [],
  };

  console.log(`Fixture: ${fixtureName} (${fileNames.length} files)`);
  console.log(`Scenario: ${scenarioName}`);
  console.log(`Repeats: ${repeatCount}`);
  console.log(`Deps cache mode: ${depsCacheMode}`);
  console.log(`Host importers experiments: ${hostImportersExperiments}`);

  for (const mode of importersModes) {
    const templateWorkspace = prepareTemplateWorkspace(fileNames, mode);
    seedWarmState(templateWorkspace, mode);

    const runs = [];
    for (let repeatIndex = 0; repeatIndex < repeatCount; repeatIndex += 1) {
      runs.push(runSingleScenario(templateWorkspace, mode, repeatIndex));

      if (repeatIndex < repeatCount - 1) {
        restoreWarmBaseline(templateWorkspace, mode, repeatIndex);
      }
    }

    const summary = summarizeRuns(runs);
    results.modes.push({ mode, summary, runs });
    printSummary(mode, runs, summary);
  }

  writeFile(resultsPath, `${JSON.stringify(results, null, 2)}\n`);
  console.log(`\nWrote ${path.relative(repoRoot, resultsPath)}`);
}

main();

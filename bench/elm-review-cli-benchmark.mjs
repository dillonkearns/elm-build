#!/usr/bin/env node

import { spawnSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { performance } from "node:perf_hooks";

const repoRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), "..");
const srcRoot = path.join(repoRoot, "src");
const reviewDir = path.join(repoRoot, "bench", "review");
const resultsPath = path.join(repoRoot, "bench", "results", "elm-review-cli-scenarios.json");
const runId = `${Date.now().toString(36)}-${process.pid}`;
const fixtureFilter = process.argv.includes("--fixture")
  ? process.argv[process.argv.indexOf("--fixture") + 1]
  : null;

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

function ensureDir(dirPath) {
  fs.mkdirSync(dirPath, { recursive: true });
}

function copyFixture(fileNames, fixtureSrcDir) {
  ensureDir(fixtureSrcDir);

  for (const fileName of fileNames) {
    fs.copyFileSync(path.join(srcRoot, fileName), path.join(fixtureSrcDir, fileName));
  }
}

function writeElmJson(workspaceRoot) {
  const rootElmJson = JSON.parse(fs.readFileSync(path.join(repoRoot, "elm.json"), "utf8"));
  rootElmJson["source-directories"] = [ "src" ];
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

function findElmReviewBinary() {
  const whichResult = spawnSync("which", [ "elm-review" ], { encoding: "utf8" });

  if (whichResult.status === 0) {
    return "elm-review";
  }

  const fallback = "/opt/homebrew/bin/elm-review";
  if (fs.existsSync(fallback)) {
    return fallback;
  }

  throw new Error("Could not find elm-review on PATH or at /opt/homebrew/bin/elm-review");
}

function parseBenchmarkInfo(stdout) {
  const lines = stdout.split("\n");
  const benchmarkInfo = {};

  for (const line of lines) {
    const match = /^([^:]+): ([0-9.]+)ms$/.exec(line.trim());
    if (match) {
      benchmarkInfo[match[1]] = Number(match[2]);
    }
  }

  return benchmarkInfo;
}

function runElmReviewCli({ workspaceRoot }) {
  const elmReviewBinary = findElmReviewBinary();
  const start = performance.now();
  const result = spawnSync(
    elmReviewBinary,
    [
      "src",
      "--config",
      reviewDir,
      "--report=json",
      "--no-details",
      "--benchmark-info",
    ],
    {
      cwd: workspaceRoot,
      encoding: "utf8",
    }
  );
  const wallMs = performance.now() - start;

  return {
    wallMs,
    exitCode: result.status ?? 0,
    stdoutLength: result.stdout.length,
    stderrLength: result.stderr.length,
    benchmarkInfo: parseBenchmarkInfo(result.stdout),
  };
}

function scenarioResult(name, runResult) {
  return {
    name,
    wall_ms: Math.round(runResult.wallMs),
    exit_code: runResult.exitCode,
    stdout_length: runResult.stdoutLength,
    stderr_length: runResult.stderrLength,
    benchmark_info_ms: runResult.benchmarkInfo,
  };
}

function prepareScenarioWorkspace(fixtureName, fileNames) {
  const root = path.join(
    os.tmpdir(),
    "elm-build2-elm-review-cli-bench",
    runId,
    fixtureName
  );
  fs.rmSync(root, { recursive: true, force: true });

  const fixtureSrcDir = path.join(root, "src");

  copyFixture(fileNames, fixtureSrcDir);
  writeElmJson(root);

  return { root, fixtureSrcDir };
}

function runScenarioSequence({ fixtureName, fileNames }) {
  const workspace = prepareScenarioWorkspace(fixtureName, fileNames);
  const mathLibPath = path.join(workspace.fixtureSrcDir, "MathLib.elm");
  const originalMathLib = fs.readFileSync(mathLibPath, "utf8");

  function restoreBaseline() {
    fs.writeFileSync(mathLibPath, originalMathLib);
  }

  function runScenario(name) {
    return scenarioResult(name, runElmReviewCli({ workspaceRoot: workspace.root }));
  }

  const cold = runScenario("cold");
  const warm = runScenario("warm");

  mutateBodyEdit(workspace.fixtureSrcDir);
  const bodyEdit = runScenario("warm_1_file_body_edit");
  restoreBaseline();
  runElmReviewCli({ workspaceRoot: workspace.root });

  mutateCommentOnlyEdit(workspace.fixtureSrcDir);
  const commentOnly = runScenario("warm_1_file_comment_only");
  restoreBaseline();
  runElmReviewCli({ workspaceRoot: workspace.root });

  mutateImportGraphEdit(workspace.fixtureSrcDir);
  const importGraph = runScenario("warm_import_graph_change");
  restoreBaseline();

  return [ cold, warm, bodyEdit, commentOnly, importGraph ];
}

function fixtureResult(fixtureName, fileNames) {
  return {
    fixture: fixtureName,
    file_count: fileNames.length,
    scenarios: runScenarioSequence({ fixtureName, fileNames }),
  };
}

const allFixtures = [
  { name: "small-12", fileNames: smallFixtureFiles },
];

const selectedFixtures =
  fixtureFilter == null
    ? allFixtures
    : allFixtures.filter((fixture) => fixture.name === fixtureFilter);

if (selectedFixtures.length === 0) {
  throw new Error(`Unknown fixture '${fixtureFilter}'`);
}

const results = {
  date: new Date().toISOString(),
  runner: "elm-review",
  fixtures: selectedFixtures.map((fixture) => fixtureResult(fixture.name, fixture.fileNames)),
};

ensureDir(path.dirname(resultsPath));
fs.writeFileSync(resultsPath, JSON.stringify(results, null, 2));

for (const fixture of results.fixtures) {
  console.log(`\nFixture: ${fixture.fixture} (${fixture.file_count} files)`);
  for (const scenario of fixture.scenarios) {
    console.log(`  ${scenario.name}: ${scenario.wall_ms}ms`);
  }
}

console.log(`\nWrote ${resultsPath}`);

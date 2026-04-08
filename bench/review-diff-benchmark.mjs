#!/usr/bin/env node

import { execFileSync, spawnSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";

const repoRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), "..");
const srcRoot = path.join(repoRoot, "src");
const reviewDir = path.join(repoRoot, "bench", "review");
const distRunnerPath = path.join(repoRoot, "dist", "review-runner-bench.mjs");
const resultsPath = path.join(repoRoot, "bench", "results", "review-diff-scenarios.json");
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

function bundleRunner() {
  ensureDir(path.dirname(distRunnerPath));
  execFileSync(
    "bunx",
    ["elm-pages", "bundle-script", "src/ReviewRunner.elm", "--output", distRunnerPath],
    { cwd: repoRoot, stdio: "inherit" }
  );
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

function normalizePath(workspaceRoot, filePath) {
  const raw =
    path.isAbsolute(filePath)
      ? path.relative(workspaceRoot, filePath)
      : filePath;

  return raw.split(path.sep).join("/");
}

function normalizeMessage(message) {
  return message.replace(/\s+/g, " ").trim();
}

function extractTrailingJson(stdout) {
  const lines = stdout
    .split("\n")
    .map((line) => line.trim())
    .filter(Boolean);

  for (let index = lines.length - 1; index >= 0; index -= 1) {
    const line = lines[index];
    if (line.startsWith("[") || line.startsWith("{")) {
      return line;
    }
  }

  return "[]";
}

function normalizeRunnerErrors(workspaceRoot, stdout) {
  const parsed = JSON.parse(extractTrailingJson(stdout));

  return parsed
    .map((error) => ({
      rule: error.rule,
      path: normalizePath(workspaceRoot, error.path),
      line: error.line,
      column: error.column,
      message: normalizeMessage(error.message),
    }))
    .sort(compareNormalizedError);
}

function normalizeCliErrors(workspaceRoot, stdout) {
  const parsed = JSON.parse(extractTrailingJson(stdout) || "{\"errors\":[]}");
  const fileGroups = Array.isArray(parsed.errors) ? parsed.errors : [];

  return fileGroups
    .flatMap((fileGroup) =>
      (fileGroup.errors || []).map((error) => ({
        rule: error.rule,
        path: normalizePath(workspaceRoot, fileGroup.path),
        line: error.region?.start?.line ?? 0,
        column: error.region?.start?.column ?? 0,
        message: normalizeMessage(error.message ?? ""),
      }))
    )
    .sort(compareNormalizedError);
}

function compareNormalizedError(a, b) {
  return signatureForError(a).localeCompare(signatureForError(b));
}

function signatureForError(error) {
  return [
    error.rule,
    error.path,
    error.line,
    error.column,
    error.message,
  ].join("|");
}

function diffErrors(runnerErrors, cliErrors) {
  const runnerSet = new Set(runnerErrors.map(signatureForError));
  const cliSet = new Set(cliErrors.map(signatureForError));

  const onlyRunner = runnerErrors.filter((error) => !cliSet.has(signatureForError(error)));
  const onlyCli = cliErrors.filter((error) => !runnerSet.has(signatureForError(error)));

  return {
    matches: onlyRunner.length === 0 && onlyCli.length === 0,
    runner_count: runnerErrors.length,
    cli_count: cliErrors.length,
    only_runner_count: onlyRunner.length,
    only_cli_count: onlyCli.length,
    only_runner_examples: onlyRunner.slice(0, 5),
    only_cli_examples: onlyCli.slice(0, 5),
  };
}

function runRunner({ workspaceRoot, fixtureSrcDir, buildDir, tracePath }) {
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
      "--report",
      "json",
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

  if (!fs.existsSync(tracePath)) {
    throw new Error(
      `Expected runner perf trace at ${tracePath}, but it was not written.\nstdout:\n${result.stdout}\nstderr:\n${result.stderr}`
    );
  }

  return {
    exitCode: result.status ?? 0,
    errors: normalizeRunnerErrors(workspaceRoot, result.stdout),
    trace: JSON.parse(fs.readFileSync(tracePath, "utf8")),
    stdout: result.stdout,
    stderr: result.stderr,
  };
}

function runElmReviewCli({ workspaceRoot }) {
  const elmReviewBinary = findElmReviewBinary();
  const result = spawnSync(
    elmReviewBinary,
    [
      "src",
      "--config",
      reviewDir,
      "--report=json",
      "--no-details",
    ],
    {
      cwd: workspaceRoot,
      encoding: "utf8",
    }
  );

  return {
    exitCode: result.status ?? 0,
    errors: normalizeCliErrors(workspaceRoot, result.stdout),
    stdout: result.stdout,
    stderr: result.stderr,
  };
}

function prepareScenarioWorkspace(fixtureName, fileNames) {
  const root = path.join(
    os.tmpdir(),
    "elm-build2-review-diff-bench",
    runId,
    fixtureName
  );
  fs.rmSync(root, { recursive: true, force: true });

  const fixtureSrcDir = path.join(root, "src");
  const runnerBuildDir = path.join(root, ".elm-review-build-runner");

  copyFixture(fileNames, fixtureSrcDir);
  writeElmJson(root);
  ensureDir(runnerBuildDir);

  return {
    root,
    fixtureSrcDir,
    runnerBuildDir,
  };
}

function runScenario(name, workspace) {
  const runner = runRunner({
    workspaceRoot: workspace.root,
    fixtureSrcDir: workspace.fixtureSrcDir,
    buildDir: workspace.runnerBuildDir,
    tracePath: path.join(workspace.root, `${name}-runner-trace.json`),
  });

  const cli = runElmReviewCli({ workspaceRoot: workspace.root });

  return {
    name,
    runner_exit_code: runner.exitCode,
    cli_exit_code: cli.exitCode,
    cache_decision: runner.trace.cacheDecision,
    diff: diffErrors(runner.errors, cli.errors),
  };
}

function runScenarioSequence({ fixtureName, fileNames }) {
  const workspace = prepareScenarioWorkspace(fixtureName, fileNames);
  const mathLibPath = path.join(workspace.fixtureSrcDir, "MathLib.elm");
  const originalMathLib = fs.readFileSync(mathLibPath, "utf8");

  function restoreBaseline() {
    fs.writeFileSync(mathLibPath, originalMathLib);
  }

  const cold = runScenario("cold", workspace);
  const warm = runScenario("warm", workspace);

  mutateBodyEdit(workspace.fixtureSrcDir);
  const bodyEdit = runScenario("warm_1_file_body_edit", workspace);
  restoreBaseline();
  runScenario("restore_after_body", workspace);

  mutateCommentOnlyEdit(workspace.fixtureSrcDir);
  const commentOnly = runScenario("warm_1_file_comment_only", workspace);
  restoreBaseline();
  runScenario("restore_after_comment", workspace);

  mutateImportGraphEdit(workspace.fixtureSrcDir);
  const importGraph = runScenario("warm_import_graph_change", workspace);
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

if (!skipBundle) {
  bundleRunner();
}

const results = {
  date: new Date().toISOString(),
  runner: path.relative(repoRoot, distRunnerPath),
  cli_runner: "elm-review",
  importers_cache_mode: importersCacheMode,
  deps_cache_mode: depsCacheMode,
  host_importers_experiments: hostImportersExperiments,
  fixtures: selectedFixtures.map((fixture) => fixtureResult(fixture.name, fixture.fileNames)),
};

ensureDir(path.dirname(resultsPath));
fs.writeFileSync(resultsPath, JSON.stringify(results, null, 2));

for (const fixture of results.fixtures) {
  console.log(`\nFixture: ${fixture.fixture} (${fixture.file_count} files)`);
  for (const scenario of fixture.scenarios) {
    const label = scenario.name.padEnd(26, " ");
    const marker = scenario.diff.matches ? "match" : "DIFF";
    console.log(
      `${label} ${marker}  runner=${scenario.diff.runner_count} cli=${scenario.diff.cli_count} onlyRunner=${scenario.diff.only_runner_count} onlyCli=${scenario.diff.only_cli_count}`
    );
  }
}

console.log(`\nWrote ${resultsPath}`);

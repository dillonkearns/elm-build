#!/usr/bin/env node

import { spawnSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { performance } from "node:perf_hooks";

const repoRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), "..");
const sourceRepo =
  process.argv.includes("--source-repo")
    ? process.argv[process.argv.indexOf("--source-repo") + 1]
    : "/Users/dillonkearns/src/github.com/rtfeldman/elm-spa-example";
const reviewDir = path.join(repoRoot, "bench", "review");
const fastRunnerPath = path.join(repoRoot, "dist", "review-runner-fast-bench.mjs");
const buildDirName = ".elm-review-build";

function ensureDir(dirPath) {
  fs.mkdirSync(dirPath, { recursive: true });
}

function findElmReviewBinary() {
  const whichResult = spawnSync("which", ["elm-review"], { encoding: "utf8" });
  if (whichResult.status === 0) {
    return "elm-review";
  }

  const fallback = "/opt/homebrew/bin/elm-review";
  if (fs.existsSync(fallback)) {
    return fallback;
  }

  throw new Error("Could not find elm-review on PATH or at /opt/homebrew/bin/elm-review");
}

function stage(trace, name) {
  return trace.stages.find((entry) => entry.name === name)?.ms ?? 0;
}

function collectElmFiles(rootDir) {
  const elmFiles = [];
  const stack = [rootDir];

  while (stack.length > 0) {
    const current = stack.pop();
    for (const entry of fs.readdirSync(current, { withFileTypes: true })) {
      const fullPath = path.join(current, entry.name);
      if (entry.isDirectory()) {
        if (entry.name === "elm-stuff" || entry.name === buildDirName || entry.name === ".git") {
          continue;
        }
        stack.push(fullPath);
      } else if (entry.isFile() && entry.name.endsWith(".elm")) {
        elmFiles.push(fullPath);
      }
    }
  }

  elmFiles.sort();
  return elmFiles;
}

function prepareWorkspace() {
  const tempRoot = fs.mkdtempSync(path.join(os.tmpdir(), "elm-build2-elm-spa-example-"));
  const workspaceRoot = path.join(tempRoot, "workspace");
  fs.cpSync(sourceRepo, workspaceRoot, { recursive: true });

  const elmJson = JSON.parse(fs.readFileSync(path.join(workspaceRoot, "elm.json"), "utf8"));
  const sourceDirs = (elmJson["source-directories"] || ["src"]).map((dir) => path.join(workspaceRoot, dir));
  const buildDir = path.join(workspaceRoot, buildDirName);
  ensureDir(buildDir);

  const elmFiles = sourceDirs.flatMap(collectElmFiles);
  if (elmFiles.length === 0) {
    throw new Error(`No Elm files found under source dirs: ${sourceDirs.join(", ")}`);
  }

  return {
    workspaceRoot,
    sourceDirs,
    buildDir,
    elmFiles,
    editFile: elmFiles[0],
  };
}

function runRunner(workspace, scenarioName) {
  const tracePath = path.join(workspace.workspaceRoot, `${scenarioName}.trace.json`);
  const config = {
    reviewDir,
    sourceDirs: workspace.sourceDirs,
    buildDirectory: workspace.buildDir,
    jobs: os.cpus().length,
    reportFormat: "quiet",
    perfTraceJson: tracePath,
    importersCacheMode: "auto",
    depsCacheMode: "auto",
    hostNoUnusedExportsExperiment: true,
    hostNoUnusedCustomTypeConstructorsExperiment: true,
    hostNoUnusedCustomTypeConstructorArgsExperiment: true,
    hostNoUnusedParametersExperiment: true,
    hostNoUnusedVariablesExperiment: true,
    hostNoExposingEverythingExperiment: true,
    hostNoImportingEverythingExperiment: true,
    hostNoDebugLogExperiment: true,
    hostNoDebugTodoOrToStringExperiment: true,
    hostNoMissingTypeAnnotationExperiment: true,
    hostNoMissingTypeAnnotationInLetInExperiment: true,
    hostNoUnusedPatternsExperiment: true,
  };

  const start = performance.now();
  const result = spawnSync("node", [fastRunnerPath], {
    cwd: repoRoot,
    encoding: "utf8",
    env: {
      ...process.env,
      REVIEW_RUNNER_CONFIG_JSON: JSON.stringify(config),
    },
  });
  const wallMs = Math.round(performance.now() - start);

  if (!fs.existsSync(tracePath)) {
    throw new Error(
      `${scenarioName} did not produce trace output (status ${result.status})\nSTDOUT:\n${result.stdout}\nSTDERR:\n${result.stderr}`
    );
  }

  const trace = JSON.parse(fs.readFileSync(tracePath, "utf8"));
  return {
    wall_ms: wallMs,
    exit_code: result.status ?? 0,
    load_review_project_ms: stage(trace, "load_review_project"),
    module_rule_eval_ms: stage(trace, "module_rule_eval"),
    project_rule_eval_ms: stage(trace, "project_rule_eval"),
    build_target_project_runtime_ms: stage(trace, "build_target_project_runtime"),
  };
}

function runCli(workspace) {
  const elmReviewBinary = findElmReviewBinary();
  const cliSourceDirs = workspace.sourceDirs.map((dir) => path.relative(workspace.workspaceRoot, dir));
  const start = performance.now();
  const result = spawnSync(
    elmReviewBinary,
    [...cliSourceDirs, "--config", reviewDir, "--report=json", "--no-details"],
    {
      cwd: workspace.workspaceRoot,
      encoding: "utf8",
    }
  );

  return {
    wall_ms: Math.round(performance.now() - start),
    exit_code: result.status ?? 0,
    stdout_length: result.stdout.length,
    stderr_length: result.stderr.length,
  };
}

function mutateBodyEdit(filePath, originalSource) {
  fs.writeFileSync(
    filePath,
    `${originalSource}\n\nbenchmarkBodyEdit__ : Int\nbenchmarkBodyEdit__ =\n    42\n`
  );
}

function main() {
  const workspace = prepareWorkspace();
  const originalEditSource = fs.readFileSync(workspace.editFile, "utf8");

  const seedRunner = runRunner(workspace, "seed_runner");
  const seedCli = runCli(workspace);
  const warmRunner = runRunner(workspace, "warm_runner");
  const warmCli = runCli(workspace);

  mutateBodyEdit(workspace.editFile, originalEditSource);
  const bodyRunner = runRunner(workspace, "body_runner");
  const bodyCli = runCli(workspace);

  console.log(
    JSON.stringify(
      {
        source_repo: sourceRepo,
        workspace_root: workspace.workspaceRoot,
        edited_file: path.relative(workspace.workspaceRoot, workspace.editFile),
        source_dirs: workspace.sourceDirs.map((dir) => path.relative(workspace.workspaceRoot, dir)),
        seed_runner: seedRunner,
        seed_cli: seedCli,
        warm_runner: warmRunner,
        warm_cli: warmCli,
        body_runner: bodyRunner,
        body_cli: bodyCli,
      },
      null,
      2
    )
  );
}

main();

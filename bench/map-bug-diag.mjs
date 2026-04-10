#!/usr/bin/env node

/**
 * Diagnostic harness for the List.map kernel shortcut correctness bug.
 *
 * Prepares a fresh workspace and runs NoUnused.Parameters against it
 * using the current review-runner bundle with --report=json, so we can
 * diff what the runner actually produces when List.map is in the
 * kernel shortcut vs when it's not.
 */

import { spawnSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";

const repoRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), "..");
const srcRoot = path.join(repoRoot, "src");
const baseReviewDir = path.join(repoRoot, "bench", "review");
const runnerPath = path.join(repoRoot, "dist", "review-runner-bench.mjs");
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

const reviewConfig = `module ReviewConfig exposing (config)

import NoUnused.Parameters
import Review.Rule as Rule exposing (Rule)


config : List Rule
config =
    [ NoUnused.Parameters.rule ]
`;

function ensureDir(d) {
  fs.mkdirSync(d, { recursive: true });
}

function writeElmJson(workspaceRoot) {
  const rootElmJson = JSON.parse(fs.readFileSync(path.join(repoRoot, "elm.json"), "utf8"));
  rootElmJson["source-directories"] = ["src"];
  fs.writeFileSync(path.join(workspaceRoot, "elm.json"), JSON.stringify(rootElmJson, null, 2));
}

function prepareWorkspace() {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "elm-build2-map-bug-"));
  const fixtureSrcDir = path.join(root, "src");
  const buildDir = path.join(root, ".elm-review-build");
  ensureDir(fixtureSrcDir);
  ensureDir(buildDir);
  for (const fileName of fixtureFiles) {
    fs.copyFileSync(path.join(srcRoot, fileName), path.join(fixtureSrcDir, fileName));
  }
  writeElmJson(root);
  return { root, fixtureSrcDir, buildDir };
}

function prepareReviewDir() {
  const reviewRoot = fs.mkdtempSync(path.join(os.tmpdir(), "elm-build2-map-bug-review-"));
  ensureDir(path.join(reviewRoot, "src"));
  fs.copyFileSync(path.join(baseReviewDir, "elm.json"), path.join(reviewRoot, "elm.json"));
  fs.writeFileSync(path.join(reviewRoot, "src", "ReviewConfig.elm"), reviewConfig);
  return reviewRoot;
}

const workspace = prepareWorkspace();
const reviewDir = prepareReviewDir();

const result = spawnSync(
  "node",
  [
    runnerPath,
    "--review-dir",
    reviewDir,
    "--source-dirs",
    workspace.fixtureSrcDir,
    "--build",
    workspace.buildDir,
    "--jobs",
    jobs,
    "--importers-cache-mode",
    "fresh",
    "--deps-cache-mode",
    "fresh",
    "--report",
    "human",
    "--perf-trace-json",
    path.join(workspace.root, "perf-trace.json"),
  ],
  {
    cwd: repoRoot,
    encoding: "utf8",
  }
);

console.log("=== exit code:", result.status);
console.log("=== stdout ===");
console.log(result.stdout);
console.log("=== stderr ===");
console.log(result.stderr);
console.log("=== workspace:", workspace.root);
console.log("=== review:", reviewDir);

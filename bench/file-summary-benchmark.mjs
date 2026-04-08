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
const resultsPath = path.join(repoRoot, "bench", "results", "file-summary-benchmark.json");
const storedRunnerResultsPath = path.join(repoRoot, "bench", "results", "review-runner-scenarios.json");
const fixtureName = readArg("--fixture", "small-12");
const skipBundle = process.argv.includes("--skip-bundle");
const useStoredRunner = process.argv.includes("--use-stored-runner");
const iterations = Number(readArg("--iterations", "500"));
const importersCacheMode = readArg("--importers-cache-mode", "auto");
const depsCacheMode = readArg("--deps-cache-mode", "auto");
const runId = `${Date.now().toString(36)}-${process.pid}`;
const jobs = String(os.cpus().length);

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

function readArg(flag, defaultValue) {
  return process.argv.includes(flag)
    ? process.argv[process.argv.indexOf(flag) + 1]
    : defaultValue;
}

function ensureDir(dirPath) {
  fs.mkdirSync(dirPath, { recursive: true });
}

function writeFile(filePath, body) {
  ensureDir(path.dirname(filePath));
  fs.writeFileSync(filePath, body);
}

function bundleRunner() {
  ensureDir(path.dirname(distRunnerPath));
  execFileSync(
    "bunx",
    ["elm-pages", "bundle-script", "src/ReviewRunner.elm", "--output", distRunnerPath],
    { cwd: repoRoot, stdio: "inherit" }
  );
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

function prepareWorkspace(fileNames) {
  const root = path.join(
    os.tmpdir(),
    "elm-build2-file-summary-bench",
    runId,
    fixtureName
  );
  fs.rmSync(root, { recursive: true, force: true });

  const fixtureSrcDir = path.join(root, "src");
  const buildDir = path.join(root, ".elm-review-build");

  copyFixture(fileNames, fixtureSrcDir);
  writeElmJson(root);
  ensureDir(buildDir);

  return { root, fixtureSrcDir, buildDir };
}

function mutateBodyEdit(fixtureSrcDir) {
  const filePath = path.join(fixtureSrcDir, "MathLib.elm");
  const source = fs.readFileSync(filePath, "utf8");
  const mutated = `${source}\n\nbenchmarkBodyEdit__ : Int\nbenchmarkBodyEdit__ =\n    abs -7\n`;
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

function runBundledReviewRunner({ fixtureSrcDir, buildDir, tracePath }) {
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
    ],
    {
      cwd: repoRoot,
      encoding: "utf8",
    }
  );

  if (!fs.existsSync(tracePath)) {
    throw new Error(
      `Expected perf trace at ${tracePath}, but it was not written.\nstdout:\n${result.stdout}\nstderr:\n${result.stderr}`
    );
  }

  return {
    wallMs: performance.now() - start,
    exitCode: result.status ?? 0,
    trace: JSON.parse(fs.readFileSync(tracePath, "utf8")),
  };
}

function stageMs(trace, stageName) {
  return trace.stages.find((stage) => stage.name === stageName)?.ms ?? 0;
}

function fixtureFileNames() {
  switch (fixtureName) {
    case "small-12":
      return smallFixtureFiles;

    default:
      throw new Error(`Unsupported fixture: ${fixtureName}`);
  }
}

function loadStoredRunnerFixture() {
  const stored = JSON.parse(fs.readFileSync(storedRunnerResultsPath, "utf8"));
  const fixture = stored.fixtures.find((entry) => entry.fixture === fixtureName);

  if (!fixture) {
    throw new Error(`Could not find fixture ${fixtureName} in ${storedRunnerResultsPath}`);
  }

  const scenarioByName = Object.fromEntries(
    fixture.scenarios.map((scenario) => [scenario.name, scenario])
  );

  return {
    warm_1_file_body_edit: {
      wallMs: scenarioByName.warm_1_file_body_edit.wall_ms,
      trace: {
        counters: scenarioByName.warm_1_file_body_edit.counters,
        stages: Object.entries(scenarioByName.warm_1_file_body_edit.stages).map(([name, ms]) => ({
          name,
          ms,
        })),
      },
    },
    warm_import_graph_change: {
      wallMs: scenarioByName.warm_import_graph_change.wall_ms,
      trace: {
        counters: scenarioByName.warm_import_graph_change.counters,
        stages: Object.entries(scenarioByName.warm_import_graph_change.stages).map(([name, ms]) => ({
          name,
          ms,
        })),
      },
    },
  };
}

function extractLightweightSummary(source) {
  const lines = source.split(/\r?\n/);
  let moduleName = "";
  let exposingKind = "unknown";
  let importCount = 0;
  let typeCount = 0;
  let declarationCount = 0;
  const imports = [];
  const types = [];
  const declarations = [];

  for (const line of lines) {
    if (!moduleName) {
      const moduleMatch = line.match(/^module\s+([A-Z][A-Za-z0-9_.]*)\s+exposing\s+\((.*)\)\s*$/);
      if (moduleMatch) {
        moduleName = moduleMatch[1];
        exposingKind = moduleMatch[2].trim() === ".." ? "all" : "explicit";
        continue;
      }
    }

    const importMatch = line.match(/^import\s+([A-Z][A-Za-z0-9_.]*)\b/);
    if (importMatch) {
      imports.push(importMatch[1]);
      importCount += 1;
      continue;
    }

    const typeAliasMatch = line.match(/^type\s+alias\s+([A-Z][A-Za-z0-9_]*)\b/);
    if (typeAliasMatch) {
      types.push(`alias:${typeAliasMatch[1]}`);
      typeCount += 1;
      continue;
    }

    const typeMatch = line.match(/^type\s+([A-Z][A-Za-z0-9_]*)\b/);
    if (typeMatch) {
      types.push(`type:${typeMatch[1]}`);
      typeCount += 1;
      continue;
    }

    const declarationMatch = line.match(/^([a-z][A-Za-z0-9_']*)\s*(?::|=)/);
    if (declarationMatch) {
      declarations.push(declarationMatch[1]);
      declarationCount += 1;
    }
  }

  return {
    moduleName,
    exposingKind,
    imports,
    types,
    declarations,
    checksum:
      moduleName.length +
      exposingKind.length +
      importCount * 7 +
      typeCount * 11 +
      declarationCount * 13 +
      imports.join("|").length +
      types.join("|").length +
      declarations.join("|").length,
  };
}

function benchmarkLightweightSummary(sources, iterationCount) {
  let checksum = 0;
  const start = performance.now();

  for (let i = 0; i < iterationCount; i += 1) {
    for (const source of sources) {
      checksum += extractLightweightSummary(source).checksum;
    }
  }

  const totalMs = performance.now() - start;

  return {
    iterations: iterationCount,
    fileCount: sources.length,
    totalMs: Math.round(totalMs * 1000) / 1000,
    avgPerIterationMs: Math.round((totalMs / iterationCount) * 1000) / 1000,
    avgPerFileMs: Math.round((totalMs / iterationCount / sources.length) * 1000) / 1000,
    checksum,
  };
}

function scenarioSummary(name, runResult, lightweightResult, changedFileCount) {
  const analyzeMs = stageMs(runResult.trace, "analyze_target_files");
  const readMs = stageMs(runResult.trace, "read_target_files");
  const loadReviewProjectMs = stageMs(runResult.trace, "load_review_project");
  const moduleRuleEvalMs = stageMs(runResult.trace, "module_rule_eval");
  const projectRuleEvalMs = stageMs(runResult.trace, "project_rule_eval");
  const upperBoundWallIfAnalysisWereFree = Math.max(0, Math.round(runResult.wallMs - analyzeMs));

  return {
    scenario: name,
    wall_ms: Math.round(runResult.wallMs),
    analyze_target_files_ms: analyzeMs,
    read_target_files_ms: readMs,
    load_review_project_ms: loadReviewProjectMs,
    module_rule_eval_ms: moduleRuleEvalMs,
    project_rule_eval_ms: projectRuleEvalMs,
    analysis_cache_hits: runResult.trace.counters["analysis_cache.hits"] ?? 0,
    analysis_cache_misses: runResult.trace.counters["analysis_cache.misses"] ?? 0,
    changed_file_count: changedFileCount,
    lightweight_summary_ms: lightweightResult.avgPerIterationMs,
    lightweight_summary_per_file_ms: lightweightResult.avgPerFileMs,
    upper_bound_wall_if_analysis_free_ms: upperBoundWallIfAnalysisWereFree,
  };
}

function printScenario(result) {
  console.log(`\n${result.scenario}`);
  console.log("metric                              seconds");
  console.log(`wall                                ${(result.wall_ms / 1000).toFixed(3)}`);
  console.log(`analyze_target_files                ${(result.analyze_target_files_ms / 1000).toFixed(3)}`);
  console.log(`load_review_project                 ${(result.load_review_project_ms / 1000).toFixed(3)}`);
  console.log(`module_rule_eval                    ${(result.module_rule_eval_ms / 1000).toFixed(3)}`);
  console.log(`project_rule_eval                   ${(result.project_rule_eval_ms / 1000).toFixed(3)}`);
  console.log(`lightweight_summary_avg             ${(result.lightweight_summary_ms / 1000).toFixed(6)}`);
  console.log(`lightweight_summary_per_file_avg    ${(result.lightweight_summary_per_file_ms / 1000).toFixed(6)}`);
  console.log(`upper_bound_if_analysis_free        ${(result.upper_bound_wall_if_analysis_free_ms / 1000).toFixed(3)}`);
}

function main() {
  if (!skipBundle && !useStoredRunner) {
    bundleRunner();
  }

  const fileNames = fixtureFileNames();
  const workspace = prepareWorkspace(fileNames);

  let bodyRun;
  let importRun;

  if (useStoredRunner) {
    const stored = loadStoredRunnerFixture();
    bodyRun = stored.warm_1_file_body_edit;
    importRun = stored.warm_import_graph_change;
  } else {
    const warmSeed = runBundledReviewRunner({
      fixtureSrcDir: workspace.fixtureSrcDir,
      buildDir: workspace.buildDir,
      tracePath: path.join(workspace.root, "cold-trace.json"),
    });

    if (warmSeed.exitCode !== 1) {
      throw new Error(`Expected cold review runner exit code 1, got ${warmSeed.exitCode}`);
    }

    runBundledReviewRunner({
      fixtureSrcDir: workspace.fixtureSrcDir,
      buildDir: workspace.buildDir,
      tracePath: path.join(workspace.root, "warm-trace.json"),
    });

    mutateBodyEdit(workspace.fixtureSrcDir);
    bodyRun = runBundledReviewRunner({
      fixtureSrcDir: workspace.fixtureSrcDir,
      buildDir: workspace.buildDir,
      tracePath: path.join(workspace.root, "body-trace.json"),
    });

    fs.rmSync(workspace.fixtureSrcDir, { recursive: true, force: true });
    copyFixture(fileNames, workspace.fixtureSrcDir);
    writeElmJson(workspace.root);
    runBundledReviewRunner({
      fixtureSrcDir: workspace.fixtureSrcDir,
      buildDir: workspace.buildDir,
      tracePath: path.join(workspace.root, "restore-warm-trace.json"),
    });

    mutateImportGraphEdit(workspace.fixtureSrcDir);
    importRun = runBundledReviewRunner({
      fixtureSrcDir: workspace.fixtureSrcDir,
      buildDir: workspace.buildDir,
      tracePath: path.join(workspace.root, "import-trace.json"),
    });
  }

  fs.rmSync(workspace.fixtureSrcDir, { recursive: true, force: true });
  copyFixture(fileNames, workspace.fixtureSrcDir);
  writeElmJson(workspace.root);

  mutateBodyEdit(workspace.fixtureSrcDir);
  const bodySource = fs.readFileSync(path.join(workspace.fixtureSrcDir, "MathLib.elm"), "utf8");

  fs.rmSync(workspace.fixtureSrcDir, { recursive: true, force: true });
  copyFixture(fileNames, workspace.fixtureSrcDir);
  writeElmJson(workspace.root);

  mutateImportGraphEdit(workspace.fixtureSrcDir);
  const importSource = fs.readFileSync(path.join(workspace.fixtureSrcDir, "MathLib.elm"), "utf8");

  const allSources = fileNames.map((name) => fs.readFileSync(path.join(srcRoot, name), "utf8"));
  const bodyLightweight = benchmarkLightweightSummary([bodySource], iterations);
  const importLightweight = benchmarkLightweightSummary([importSource], iterations);
  const allFilesLightweight = benchmarkLightweightSummary(allSources, iterations);

  const results = {
    date: new Date().toISOString(),
    fixture: fixtureName,
    iterations,
    runner_source: useStoredRunner ? path.relative(repoRoot, storedRunnerResultsPath) : path.relative(repoRoot, distRunnerPath),
    importers_cache_mode: importersCacheMode,
    deps_cache_mode: depsCacheMode,
    scenarios: [
      scenarioSummary("warm_1_file_body_edit", bodyRun, bodyLightweight, 1),
      scenarioSummary("warm_import_graph_change", importRun, importLightweight, 1),
    ],
    all_files_lightweight_summary: allFilesLightweight,
  };

  writeFile(resultsPath, `${JSON.stringify(results, null, 2)}\n`);

  for (const scenario of results.scenarios) {
    printScenario(scenario);
  }

  console.log("\nall_files_lightweight_summary");
  console.log("metric                              seconds");
  console.log(`avg_per_iteration                   ${(allFilesLightweight.avgPerIterationMs / 1000).toFixed(6)}`);
  console.log(`avg_per_file                        ${(allFilesLightweight.avgPerFileMs / 1000).toFixed(6)}`);

  console.log(`\nWrote ${path.relative(repoRoot, resultsPath)}`);
}

main();

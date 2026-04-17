#!/usr/bin/env node

import { spawnSync } from "node:child_process";
import fs from "node:fs";
import path from "node:path";
import { performance } from "node:perf_hooks";
import { fileURLToPath } from "node:url";

import { externalSuites } from "./external-suites/suites.mjs";

const benchDir = path.dirname(fileURLToPath(import.meta.url));
const repoRoot = path.resolve(benchDir, "..");
const checkoutsRoot = path.join(benchDir, "external-suites", "checkouts");
const resultsRoot = path.join(benchDir, "external-suites", "results");
const bundledRunnerPath = path.join(repoRoot, "test-runner.mjs");
const bundleInputRoots = [
  "elm.json",
  "src",
  "codegen",
  "elm-interpreter/src",
  "elm-interpreter/generated",
  "elm-interpreter/helpers",
];
const defaultBuildDir = ".elm-build/external-test-oracle";

const suiteSelector = readArg("--suite", "starter");
const caseSelector = readArg("--case", null);
const runs = readIntArg("--runs", 1);
const seed = readIntArg("--seed", 42);
const fuzzRuns = readIntArg("--fuzz", 100);
const timeoutOverride = readIntArg("--timeout-secs", null);
const globsOverride = splitCsv(readArg("--globs", null));
const listOnly = process.argv.includes("--list");
const syncOnly = process.argv.includes("--sync-only");
const skipSync = process.argv.includes("--no-sync");
const forceBundle = process.argv.includes("--bundle");
const skipBundle = process.argv.includes("--skip-bundle");

function readArg(flag, fallback) {
  const index = process.argv.indexOf(flag);
  return index >= 0 ? process.argv[index + 1] : fallback;
}

function readIntArg(flag, fallback) {
  const raw = readArg(flag, null);
  if (raw == null) {
    return fallback;
  }

  const parsed = Number.parseInt(raw, 10);
  if (Number.isNaN(parsed)) {
    throw new Error(`Expected integer for ${flag}, got '${raw}'`);
  }

  return parsed;
}

function splitCsv(raw) {
  if (raw == null || raw.trim() === "") {
    return [];
  }

  return raw
    .split(",")
    .map((entry) => entry.trim())
    .filter(Boolean);
}

function ensureDir(dirPath) {
  fs.mkdirSync(dirPath, { recursive: true });
}

function trimText(text, limit = 4000) {
  if (!text) {
    return "";
  }

  if (text.length <= limit) {
    return text;
  }

  return `${text.slice(0, limit)}\n...[truncated]`;
}

function runProcess(command, args, { cwd, timeoutSecs, env } = {}) {
  const start = performance.now();
  const result = spawnSync(command, args, {
    cwd,
    env,
    encoding: "utf8",
    timeout: timeoutSecs == null ? undefined : timeoutSecs * 1000,
    maxBuffer: 20 * 1024 * 1024,
  });
  const wallMs = Math.round(performance.now() - start);
  const timedOut = result.error?.code === "ETIMEDOUT";

  return {
    command,
    args,
    cwd,
    wallMs,
    exitCode: timedOut ? 124 : (result.status ?? 0),
    timedOut,
    signal: result.signal ?? null,
    stdout: result.stdout ?? "",
    stderr: result.stderr ?? "",
    error: result.error ? String(result.error) : null,
  };
}

function runChecked(command, args, options = {}) {
  const result = runProcess(command, args, options);

  if (result.timedOut || result.exitCode !== 0) {
    throw new Error(
      [
        `Command failed: ${[command, ...args].join(" ")}`,
        `cwd: ${options.cwd ?? process.cwd()}`,
        `exit: ${result.exitCode}`,
        result.stdout ? `stdout:\n${trimText(result.stdout)}` : null,
        result.stderr ? `stderr:\n${trimText(result.stderr)}` : null,
      ]
        .filter(Boolean)
        .join("\n\n")
    );
  }

  return result;
}

function suiteCheckoutDir(suite) {
  return path.join(checkoutsRoot, suite.id);
}

function listSuites() {
  for (const suite of externalSuites) {
    const suiteCases = getSuiteCases(suite);
    const caseSummary =
      suiteCases.length > 1 ? ` [cases: ${suiteCases.map((suiteCase) => suiteCase.id).join(", ")}]` : "";
    console.log(`${suite.id.padEnd(20)} ${suite.description}${caseSummary}`);
  }
}

function selectSuites(selector) {
  if (selector === "all") {
    return externalSuites;
  }

  if (selector === "starter") {
    return externalSuites.filter((suite) => suite.tags.includes("starter"));
  }

  const requested = new Set(splitCsv(selector));
  const selected = externalSuites.filter((suite) => requested.has(suite.id));

  if (selected.length !== requested.size) {
    const foundIds = new Set(selected.map((suite) => suite.id));
    const missing = [...requested].filter((id) => !foundIds.has(id));
    throw new Error(`Unknown suite id(s): ${missing.join(", ")}`);
  }

  return selected;
}

function getSuiteCases(suite) {
  if (Array.isArray(suite.cases) && suite.cases.length > 0) {
    return suite.cases;
  }

  return [
    {
      id: "default",
      description: suite.description,
      globs: suite.globs ?? [],
      timeoutSecs: suite.timeoutSecs,
    },
  ];
}

function selectSuiteCases(suite) {
  const suiteCases = getSuiteCases(suite);
  const requestedIds = splitCsv(caseSelector);

  if (requestedIds.length === 0) {
    return suiteCases;
  }

  const requested = new Set(requestedIds);
  const selected = suiteCases.filter((suiteCase) => requested.has(suiteCase.id));

  if (selected.length !== requested.size) {
    const foundIds = new Set(selected.map((suiteCase) => suiteCase.id));
    const missing = [...requested].filter((id) => !foundIds.has(id));
    throw new Error(`Unknown case id(s) for ${suite.id}: ${missing.join(", ")}`);
  }

  return selected;
}

function resolveSuiteCase(suite, suiteCase) {
  return {
    id: suiteCase.id,
    description: suiteCase.description ?? null,
    globs: globsOverride.length > 0 ? globsOverride : suiteCase.globs ?? suite.globs ?? [],
    timeoutSecs: timeoutOverride ?? suiteCase.timeoutSecs ?? suite.timeoutSecs,
  };
}

function syncSuite(suite) {
  ensureDir(checkoutsRoot);
  const checkoutDir = suiteCheckoutDir(suite);

  if (!fs.existsSync(checkoutDir)) {
    runChecked("git", ["clone", "--branch", suite.branch, "--depth", "1", suite.repo, checkoutDir], {
      cwd: repoRoot,
    });
    return {
      action: "cloned",
      checkoutDir,
      commit: gitStdout(checkoutDir, ["rev-parse", "HEAD"]),
      dirty: false,
    };
  }

  const dirty = gitStdout(checkoutDir, ["status", "--porcelain", "--untracked-files=no"]);
  if (dirty.trim() !== "") {
    return {
      action: "skipped-dirty",
      checkoutDir,
      commit: gitStdout(checkoutDir, ["rev-parse", "HEAD"]),
      dirty: true,
    };
  }

  runChecked("git", ["fetch", "origin", suite.branch, "--depth", "1"], { cwd: checkoutDir });
  runChecked("git", ["checkout", suite.branch], { cwd: checkoutDir });
  runChecked("git", ["merge", "--ff-only", "FETCH_HEAD"], { cwd: checkoutDir });

  return {
    action: "updated",
    checkoutDir,
    commit: gitStdout(checkoutDir, ["rev-parse", "HEAD"]),
    dirty: false,
  };
}

function gitStdout(cwd, args) {
  return runChecked("git", args, { cwd }).stdout.trim();
}

function pathIsNewerThan(entryPath, thresholdMs) {
  if (!fs.existsSync(entryPath)) {
    return false;
  }

  const stat = fs.statSync(entryPath);
  if (stat.isFile()) {
    return stat.mtimeMs > thresholdMs;
  }

  if (!stat.isDirectory()) {
    return false;
  }

  for (const entry of fs.readdirSync(entryPath, { withFileTypes: true })) {
    if (entry.name === "elm-stuff" || entry.name === ".elm-build" || entry.name === "node_modules") {
      continue;
    }

    if (pathIsNewerThan(path.join(entryPath, entry.name), thresholdMs)) {
      return true;
    }
  }

  return false;
}

function bundledRunnerIsStale() {
  if (!fs.existsSync(bundledRunnerPath)) {
    return true;
  }

  const bundleMtimeMs = fs.statSync(bundledRunnerPath).mtimeMs;
  return bundleInputRoots.some((relativePath) => pathIsNewerThan(path.join(repoRoot, relativePath), bundleMtimeMs));
}

function ensureBundledRunner() {
  if (skipBundle) {
    if (!fs.existsSync(bundledRunnerPath)) {
      throw new Error(`Missing ${bundledRunnerPath}. Remove --skip-bundle or bundle it first.`);
    }
    injectBlockingStdio();
    return;
  }

  if (!forceBundle && !bundledRunnerIsStale()) {
    injectBlockingStdio();
    return;
  }

  runChecked("bunx", ["elm-pages", "bundle-script", "src/TestRunner.elm", "--output", bundledRunnerPath], {
    cwd: repoRoot,
  });
  injectBlockingStdio();
}

/**
 * elm-pages bundle emits `console.log` for `Script.log` and then calls
 * `process.exit(...)` directly. Node buffers stdout writes to pipes
 * asynchronously, so when the test summary JSON is >64KB the tail of
 * the payload gets dropped before the exit drains — producing a
 * truncated first line that JSON.parse chokes on at byte 65536.
 *
 * Flipping stdout/stderr to blocking mode before any log happens makes
 * the writes synchronous, so every byte is on the pipe by the time
 * exit() fires. The stub is idempotent and safe to re-inject.
 */
function injectBlockingStdio() {
  const marker = "// external-test-oracle:blocking-stdio";
  const original = fs.readFileSync(bundledRunnerPath, "utf8");
  if (original.includes(marker)) {
    return;
  }

  const shebang = "#!/usr/bin/env node\n";
  const preamble =
    `${marker}\n` +
    "for (const stream of [process.stdout, process.stderr]) {\n" +
    "  if (stream && stream._handle && typeof stream._handle.setBlocking === \"function\") {\n" +
    "    stream._handle.setBlocking(true);\n" +
    "  }\n" +
    "}\n";

  const body = original.startsWith(shebang) ? original.slice(shebang.length) : original;
  fs.writeFileSync(bundledRunnerPath, shebang + preamble + body);
}

function loadSourceDirs(checkoutDir) {
  const elmJsonPath = path.join(checkoutDir, "elm.json");
  const elmJson = JSON.parse(fs.readFileSync(elmJsonPath, "utf8"));
  const declaredDirs = Array.isArray(elmJson["source-directories"]) ? elmJson["source-directories"] : ["src"];

  return [...new Set(declaredDirs.filter((dir) => dir !== "src" && dir !== "tests"))];
}

function clearInterpreterUserCache(checkoutDir) {
  // The bundled runner still writes package and user cache artifacts at the
  // `.elm-build` root, even when `--build` points at a nested subdirectory.
  const buildRoot = path.join(checkoutDir, ".elm-build");
  ensureDir(buildRoot);

  for (const entry of fs.readdirSync(buildRoot, { withFileTypes: true })) {
    if (entry.name.startsWith("package-")) {
      continue;
    }

    fs.rmSync(path.join(buildRoot, entry.name), { recursive: true, force: true });
  }

  ensureDir(path.join(checkoutDir, defaultBuildDir));
}

function hasInterpreterPackageCache(checkoutDir) {
  const buildRoot = path.join(checkoutDir, ".elm-build");
  if (!fs.existsSync(buildRoot)) {
    return false;
  }

  return fs.readdirSync(buildRoot).some((entry) => entry.startsWith("package-"));
}

function clearElmStuff(checkoutDir) {
  fs.rmSync(path.join(checkoutDir, "elm-stuff"), { recursive: true, force: true });
}

function buildSuiteArgs(checkoutDir, globs) {
  const extraSourceDirs = loadSourceDirs(checkoutDir);
  const interpreterArgs = [
    "--stack-size=8192",
    "--max-old-space-size=8192",
    bundledRunnerPath,
    "--report",
    "json",
    "--build",
    defaultBuildDir,
    "--fuzz",
    String(fuzzRuns),
    "--seed",
    String(seed),
  ];

  if (globs.length > 0) {
    interpreterArgs.push("--test", globs.join(","));
  }

  if (extraSourceDirs.length > 0) {
    interpreterArgs.push("--source-dirs", extraSourceDirs.join(","));
  }

  return {
    elmTest: ["--report=json", "--seed", String(seed), "--fuzz", String(fuzzRuns), ...globs],
    interpreter: interpreterArgs,
  };
}

function parseElmTestJson(stdout) {
  const lines = stdout
    .split("\n")
    .map((line) => line.trim())
    .filter(Boolean);

  const events = lines.map((line) => JSON.parse(line));
  const runStart = events.find((event) => event.event === "runStart") ?? null;
  const runComplete = [...events].reverse().find((event) => event.event === "runComplete") ?? null;
  const tests = events
    .filter((event) => event.event === "testCompleted")
    .map((event) => ({
      label: event.labels.join(" > "),
      status: event.status,
      failures: Array.isArray(event.failures) ? event.failures : [],
    }));

  return {
    testCount: runStart == null ? tests.length : Number.parseInt(runStart.testCount, 10),
    durationMs: runComplete == null ? null : Number.parseInt(runComplete.duration, 10),
    passed: runComplete == null ? tests.filter((test) => test.status === "pass").length : Number.parseInt(runComplete.passed, 10),
    failed: runComplete == null ? tests.filter((test) => test.status === "fail").length : Number.parseInt(runComplete.failed, 10),
    tests,
  };
}

function parseInterpreterJson(stdout) {
  const firstNonEmptyLine = stdout
    .split(/\r?\n/)
    .map((line) => line.trim())
    .find(Boolean);

  if (firstNonEmptyLine == null) {
    throw new Error("Interpreter stdout did not contain a JSON summary line");
  }

  return JSON.parse(firstNonEmptyLine);
}

function summarizeCommandResult(result, parsed, parseError) {
  return {
    exitCode: result.exitCode,
    timedOut: result.timedOut,
    wallMs: result.wallMs,
    parsed,
    parseError,
    stdoutPreview: parseError ? trimText(result.stdout) : "",
    stderrPreview: trimText(result.stderr),
  };
}

function runElmTestSuite(checkoutDir, globs, timeoutSecs) {
  clearElmStuff(checkoutDir);
  const result = runProcess("elm-test", buildSuiteArgs(checkoutDir, globs).elmTest, {
    cwd: checkoutDir,
    timeoutSecs,
  });

  let parsed = null;
  let parseError = null;

  if (!result.timedOut && result.stdout.trim() !== "") {
    try {
      parsed = parseElmTestJson(result.stdout);
    } catch (error) {
      parseError = String(error);
    }
  }

  return summarizeCommandResult(result, parsed, parseError);
}

function primeInterpreter(checkoutDir, globs, timeoutSecs) {
  if (hasInterpreterPackageCache(checkoutDir)) {
    return null;
  }

  clearInterpreterUserCache(checkoutDir);

  const result = runProcess("node", buildSuiteArgs(checkoutDir, globs).interpreter, {
    cwd: checkoutDir,
    timeoutSecs,
  });

  return {
    exitCode: result.exitCode,
    timedOut: result.timedOut,
    wallMs: result.wallMs,
    stdoutPreview: trimText(result.stdout),
    stderrPreview: trimText(result.stderr),
  };
}

function runInterpreterSuite(checkoutDir, globs, timeoutSecs) {
  clearInterpreterUserCache(checkoutDir);
  const result = runProcess("node", buildSuiteArgs(checkoutDir, globs).interpreter, {
    cwd: checkoutDir,
    timeoutSecs,
  });

  let parsed = null;
  let parseError = null;

  if (!result.timedOut && result.stdout.trim() !== "") {
    try {
      parsed = parseInterpreterJson(result.stdout);
    } catch (error) {
      parseError = String(error);
    }
  }

  return summarizeCommandResult(result, parsed, parseError);
}

function compareParsedResults(elmTest, interpreter) {
  if (elmTest == null || interpreter == null) {
    return {
      status: "indeterminate",
      reason: "missing parsed results",
      onlyInElmTest: [],
      onlyInInterpreter: [],
      statusMismatches: [],
    };
  }

  const elmMap = new Map(elmTest.tests.map((test) => [test.label, test.status]));
  const interpreterTests = interpreter.files.flatMap((file) =>
    file.tests.map((test) => ({
      label: test.label,
      status: test.status,
    }))
  );
  const interpreterMap = new Map(interpreterTests.map((test) => [test.label, test.status]));
  const onlyInElmTest = [];
  const onlyInInterpreter = [];
  const statusMismatches = [];

  for (const [label, status] of elmMap) {
    if (!interpreterMap.has(label)) {
      onlyInElmTest.push(label);
      continue;
    }

    if (interpreterMap.get(label) !== status) {
      statusMismatches.push({
        label,
        elmTest: status,
        interpreter: interpreterMap.get(label),
      });
    }
  }

  for (const [label] of interpreterMap) {
    if (!elmMap.has(label)) {
      onlyInInterpreter.push(label);
    }
  }

  const totalsMatch =
    elmTest.passed === interpreter.totalPassed &&
    elmTest.failed === interpreter.totalFailed &&
    interpreter.totalSkipped === 0;

  return {
    status:
      totalsMatch && onlyInElmTest.length === 0 && onlyInInterpreter.length === 0 && statusMismatches.length === 0
        ? "match"
        : "mismatch",
    reason: null,
    onlyInElmTest,
    onlyInInterpreter,
    statusMismatches,
  };
}

function printRunSummary(run, indent = "  ") {
  const compareStatus = run.compare.status.toUpperCase();
  console.log(`${indent}Run ${run.index}: ${compareStatus}`);
  console.log(`${indent}  elm-test: ${formatRunnerSummary(run.elmTest, "elm-test")}`);
  console.log(`${indent}  TestRunner: ${formatRunnerSummary(run.interpreter, "test-runner")}`);

  const skippedFiles =
    run.interpreter.parsed == null
      ? []
      : run.interpreter.parsed.files.filter((file) => file.status === "skipped").slice(0, 3);

  for (const file of skippedFiles) {
    console.log(`${indent}  skipped: ${file.moduleName}: ${trimText(file.message.replace(/\s+/g, " "), 160)}`);
  }

  if (run.compare.status === "mismatch") {
    console.log(
      `${indent}  mismatch counts: onlyInElmTest=${run.compare.onlyInElmTest.length}, onlyInInterpreter=${run.compare.onlyInInterpreter.length}, statusMismatches=${run.compare.statusMismatches.length}`
    );
  }
}

function formatRunnerSummary(result, label) {
  if (result.timedOut) {
    return `timeout after ${result.wallMs}ms`;
  }

  if (result.parsed == null) {
    return `exit=${result.exitCode}, parse-error=${result.parseError ?? "n/a"}`;
  }

  if (label === "elm-test") {
    return `exit=${result.exitCode}, wall=${result.wallMs}ms, passed=${result.parsed.passed}, failed=${result.parsed.failed}`;
  }

  return `exit=${result.exitCode}, wall=${result.wallMs}ms, passed=${result.parsed.totalPassed}, failed=${result.parsed.totalFailed}, skipped=${result.parsed.totalSkipped}`;
}

function writeResults(results) {
  ensureDir(resultsRoot);
  const timestamp = new Date().toISOString().replace(/[:]/g, "-");
  const latestPath = path.join(resultsRoot, "latest.json");
  const timestampedPath = path.join(resultsRoot, `${timestamp}.json`);
  const serialized = JSON.stringify(results, null, 2);
  fs.writeFileSync(latestPath, serialized);
  fs.writeFileSync(timestampedPath, serialized);
  return { latestPath, timestampedPath };
}

function main() {
  if (listOnly) {
    listSuites();
    return;
  }

  const suites = selectSuites(suiteSelector);

  if (suites.length === 0) {
    throw new Error("No suites selected");
  }

  if (!skipSync) {
    for (const suite of suites) {
      const syncResult = syncSuite(suite);
      console.log(`${suite.id}: ${syncResult.action} @ ${syncResult.commit}`);
      if (syncResult.dirty) {
        console.log(`  local edits detected in ${path.relative(repoRoot, syncResult.checkoutDir)}; leaving checkout untouched`);
      }
    }
  }

  if (syncOnly) {
    return;
  }

  ensureBundledRunner();

  const results = {
    date: new Date().toISOString(),
    suiteSelector,
    caseSelector,
    runs,
    seed,
    fuzzRuns,
    suites: [],
  };

  for (const suite of suites) {
    const checkoutDir = suiteCheckoutDir(suite);
    const selectedCases = selectSuiteCases(suite).map((suiteCase) => resolveSuiteCase(suite, suiteCase));
    const commit = gitStdout(checkoutDir, ["rev-parse", "HEAD"]);
    const suiteResult = {
      id: suite.id,
      repo: suite.repo,
      branch: suite.branch,
      commit,
      checkoutDir: path.relative(repoRoot, checkoutDir),
      cases: [],
    };

    console.log(`\n${suite.id} @ ${commit}`);

    for (const suiteCase of selectedCases) {
      const prime = primeInterpreter(checkoutDir, suiteCase.globs, suiteCase.timeoutSecs);
      const caseResult = {
        id: suiteCase.id,
        description: suiteCase.description,
        timeoutSecs: suiteCase.timeoutSecs,
        globs: suiteCase.globs,
        primeInterpreter: prime,
        runs: [],
      };

      if (selectedCases.length > 1 || suiteCase.id !== "default") {
        const globsLabel = suiteCase.globs.length === 0 ? "auto" : suiteCase.globs.join(",");
        const descriptionSuffix = suiteCase.description == null ? "" : ` - ${suiteCase.description}`;
        console.log(`  Case ${suiteCase.id}${descriptionSuffix} [globs=${globsLabel}]`);
      }

      if (prime != null) {
        console.log(`    primed interpreter package cache: exit=${prime.exitCode}, wall=${prime.wallMs}ms`);
      }

      for (let index = 1; index <= runs; index += 1) {
        const elmTest = runElmTestSuite(checkoutDir, suiteCase.globs, suiteCase.timeoutSecs);
        const interpreter = runInterpreterSuite(checkoutDir, suiteCase.globs, suiteCase.timeoutSecs);
        const compare = compareParsedResults(elmTest.parsed, interpreter.parsed);
        const runResult = { index, elmTest, interpreter, compare };
        caseResult.runs.push(runResult);
        printRunSummary(runResult, selectedCases.length > 1 || suiteCase.id !== "default" ? "    " : "  ");
      }

      suiteResult.cases.push(caseResult);
    }

    if (suiteResult.cases.length === 1) {
      const [onlyCase] = suiteResult.cases;
      suiteResult.timeoutSecs = onlyCase.timeoutSecs;
      suiteResult.globs = onlyCase.globs;
      suiteResult.primeInterpreter = onlyCase.primeInterpreter;
      suiteResult.runs = onlyCase.runs;
    }

    results.suites.push(suiteResult);
  }

  const outputPaths = writeResults(results);
  console.log(`\nSaved results to ${path.relative(repoRoot, outputPaths.latestPath)}`);
}

main();

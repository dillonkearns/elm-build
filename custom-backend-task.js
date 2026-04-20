import fs from "node:fs";
import path from "node:path";
// Worker pool lives in our path-dep'd elm-pages3 (`file:../elm-pages3`).
// When elm-pages 12.2 ships BackendTask.Parallel natively, this import and
// the `parallelDispatch` shim below can be deleted.
import * as parallelPool from "elm-pages/generator/src/parallel-worker-pool.js";

function withFsErrorLogging(label, input, task) {
    return task.catch((error) => {
        const renderedInput =
            typeof input === "string" ? input : JSON.stringify(input);
        console.error(`[custom-backend-task:${label}] ${renderedInput}`);
        console.error(error && error.stack ? error.stack : String(error));
        throw error;
    });
}

export function profile(label) {
    console.profile(label);
}

export function profileEnd(label) {
    console.profileEnd(label);
}

// export function appendLog(line) {
//     fs.appendFileSync("debug.log", line + "\n");
// }

export function triggerDebugger() {
    debugger;
}

export function forceGC() {
    if (global.gc) {
        global.gc();
    }
}

/**
 * Evaluate a function that might crash (e.g. modBy 0) and return the
 * result or an error string. Used by InterpreterProject.evalWithSourceOverrides
 * to catch runtime crashes from mutated code.
 *
 * @param {function} thunk - a zero-argument function to call
 * @returns {string} the result string, or "ERROR: Runtime crash: ..." on failure
 */
export function safeEval(thunk) {
    try {
        return thunk();
    } catch (e) {
        return "ERROR: Runtime crash: " + (e.message || String(e));
    }
}

/**
 * @param {string} path
 * @returns {Promise<string[]>}
 */
export function readdir(path) {
    return new Promise((resolve, reject) =>
        fs.readdir(path, (err, files) => {
            if (err) {
                reject(err);
            } else {
                resolve(files);
            }
        })
    );
}

/**
 * @param {{ path: string, bytes: number[] }} input
 * @returns {Promise<void>}
 */
export function writeBinaryFile(input) {
    const buffer = Buffer.from(input.bytes);

    return withFsErrorLogging(
        "writeBinaryFile",
        { path: input.path, bytesLength: input.bytes.length },
        new Promise((resolve, reject) =>
        fs.writeFile(input.path, buffer, (err) => {
            if (err) {
                reject(err);
            } else {
                resolve();
            }
        })
        )
    );
}

/**
 * @param {string[]} paths
 * @returns {Promise<string[]>}
 */
export function readFiles(paths) {
    return withFsErrorLogging(
        "readFiles",
        { count: paths.length, first: paths[0] ?? null },
        Promise.all(paths.map((filePath) => fs.promises.readFile(filePath, "utf8")))
    );
}

/**
 * @param {string} dirPath
 * @returns {Promise<void>}
 */
export function ensureDir(dirPath) {
    return withFsErrorLogging(
        "ensureDir",
        dirPath,
        fs.promises.mkdir(dirPath, { recursive: true }).then(() => undefined)
    );
}

/**
 * @param {string[]} dirPaths
 * @returns {Promise<void>}
 */
export async function ensureDirs(dirPaths) {
    return withFsErrorLogging(
        "ensureDirs",
        dirPaths,
        (async () => {
            for (const dirPath of dirPaths) {
                await fs.promises.mkdir(dirPath, { recursive: true });
            }
        })()
    );
}

/**
 * @param {string} targetPath
 * @returns {Promise<void>}
 */
export function removePath(targetPath) {
    return withFsErrorLogging(
        "removePath",
        targetPath,
        fs.promises.rm(targetPath, { recursive: true, force: true }).then(() => undefined)
    );
}

/**
 * @param {{ from: string, to: string }} input
 * @returns {Promise<void>}
 */
export function movePath(input) {
    return withFsErrorLogging(
        "movePath",
        input,
        fs.promises.rename(input.from, input.to).then(() => undefined)
    );
}

/**
 * @param {{ from: string, to: string }} input
 * @returns {Promise<void>}
 */
export function copyRecursive(input) {
    return withFsErrorLogging(
        "copyRecursive",
        input,
        fs.promises.cp(input.from, input.to, { recursive: true, force: true }).then(() => undefined)
    );
}

/**
 * @param {{ from: string, to: string }} input
 * @returns {Promise<void>}
 */
export function linkTree(input) {
    return withFsErrorLogging(
        "linkTree",
        input,
        hardlinkRecursive(input.from, input.to).then(() => undefined)
    );
}

async function chmodRecursive(targetPath, modeForEntry) {
    const stat = await fs.promises.lstat(targetPath);
    const nextMode = modeForEntry(stat);

    await fs.promises.chmod(targetPath, nextMode);

    if (stat.isDirectory()) {
        const entries = await fs.promises.readdir(targetPath);
        for (const entry of entries) {
            await chmodRecursive(path.join(targetPath, entry), modeForEntry);
        }
    }
}

/**
 * Approximate `chmod -R a=rX`.
 *
 * @param {string} targetPath
 * @returns {Promise<void>}
 */
export function chmodReadOnlyRecursive(targetPath) {
    return withFsErrorLogging(
        "chmodReadOnlyRecursive",
        targetPath,
        chmodRecursive(targetPath, (stat) => {
            const executableBits = stat.mode & 0o111;
            return stat.isDirectory() ? 0o555 : 0o444 | executableBits;
        }).then(() => undefined)
    );
}

/**
 * Approximate `chmod -R u+w`.
 *
 * @param {string} targetPath
 * @returns {Promise<void>}
 */
export function chmodUserWritableRecursive(targetPath) {
    return withFsErrorLogging(
        "chmodUserWritableRecursive",
        targetPath,
        chmodRecursive(targetPath, (stat) => stat.mode | 0o200).then(() => undefined)
    );
}

async function hardlinkRecursive(src, dest) {
    const stat = await fs.promises.lstat(src);

    if (stat.isDirectory()) {
        await fs.promises.mkdir(dest, { recursive: true });
        const entries = await fs.promises.readdir(src);
        for (const entry of entries) {
            await hardlinkRecursive(path.join(src, entry), path.join(dest, entry));
        }
        return;
    }

    if (stat.isSymbolicLink()) {
        const target = await fs.promises.readlink(src);
        await fs.promises.symlink(target, dest);
        return;
    }

    try {
        await fs.promises.link(src, dest);
    } catch (error) {
        if (error && (error.code === "EXDEV" || error.code === "EEXIST")) {
            await fs.promises.copyFile(src, dest);
        } else {
            throw error;
        }
    }
}

/**
 * @param {{ destDir: string, entries: Array<{ src: string, destName: string }> }} input
 * @returns {Promise<void>}
 */
export async function linkCopies(input) {
    return withFsErrorLogging(
        "linkCopies",
        { destDir: input.destDir, entries: input.entries.length },
        (async () => {
            await fs.promises.mkdir(input.destDir, { recursive: true });

            for (const entry of input.entries) {
                await hardlinkRecursive(entry.src, path.join(input.destDir, entry.destName));
            }
        })()
    );
}

// Dispatcher for BackendTask.Parallel.run (see src/BackendTask/Parallel.elm).
// Takes `{portName, input}` JSON, forwards the call to the Node Worker-thread
// pool from elm-pages3's parallel-worker-pool.js, and returns whatever the
// worker's port function returns (normalized to a JSON-serialisable value).
//
// JSON in/out keeps the transport simple until elm-pages 12.2 lands
// BackendTask.Parallel natively with a proper Bytes channel. For the
// TestRunner workload, per-task payloads are small (paths, names, results),
// so JSON overhead is immaterial.
export async function parallelDispatch(input, context) {
    const { portName, input: portInput } = input;
    const inputBytes = Buffer.from(JSON.stringify(portInput), "utf8");
    const outputBytes = await parallelPool.dispatch(
        portName,
        inputBytes,
        context
    );
    // Workers may return the Buffer as Uint8Array after structured clone;
    // normalize before converting to string.
    const asString = Buffer.isBuffer(outputBytes)
        ? outputBytes.toString("utf8")
        : Buffer.from(outputBytes).toString("utf8");
    return JSON.parse(asString);
}

// Smoke-test port for BackendTask.Parallel.run — wired to TestParallelShim.elm.
// Takes {n: Int} JSON, squares it, returns {squared: Int}.
// Busy-waits 50 ms so we can observe that concurrent calls overlap across
// worker threads (wall time ≪ N × 50 ms).
export async function squareU32(inputBytes) {
    const { n } = JSON.parse(inputBytes.toString("utf8"));
    const deadline = Date.now() + 50;
    while (Date.now() < deadline) {
        // spin
    }
    const result = { squared: (n * n) >>> 0 };
    return Buffer.from(JSON.stringify(result), "utf8");
}

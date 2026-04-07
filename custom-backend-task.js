import fs from "node:fs";

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

    return new Promise((resolve, reject) =>
        fs.writeFile(input.path, buffer, (err) => {
            if (err) {
                reject(err);
            } else {
                resolve();
            }
        })
    );
}

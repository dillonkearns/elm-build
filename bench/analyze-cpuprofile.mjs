#!/usr/bin/env node

/**
 * Analyze a Node --cpu-prof .cpuprofile file and print the top
 * self-time frames and top total-time (inclusive) call stacks.
 *
 * Usage:
 *   node bench/analyze-cpuprofile.mjs path/to/file.cpuprofile [--top 30]
 */

import fs from "node:fs";
import path from "node:path";

const filePath = process.argv[2];
if (!filePath) {
  console.error("Usage: analyze-cpuprofile.mjs <file.cpuprofile> [--top N]");
  process.exit(2);
}

const topArgIndex = process.argv.indexOf("--top");
const topN = topArgIndex !== -1 ? Number(process.argv[topArgIndex + 1]) : 30;

console.error(`reading ${filePath} (${(fs.statSync(filePath).size / 1024 / 1024).toFixed(1)}MB)...`);
const raw = fs.readFileSync(filePath, "utf8");
const profile = JSON.parse(raw);

const nodesById = new Map();
for (const node of profile.nodes) {
  nodesById.set(node.id, node);
}

// Build parent pointers so we can compute inclusive time by walking up.
const parentOf = new Map();
for (const node of profile.nodes) {
  if (node.children) {
    for (const childId of node.children) {
      parentOf.set(childId, node.id);
    }
  }
}

// Compute self time per node by walking samples.
const selfMicrosByNode = new Map();
let totalMicros = 0;
if (profile.samples && profile.timeDeltas) {
  for (let i = 0; i < profile.samples.length; i++) {
    const delta = profile.timeDeltas[i];
    totalMicros += delta;
    const nodeId = profile.samples[i];
    selfMicrosByNode.set(nodeId, (selfMicrosByNode.get(nodeId) ?? 0) + delta);
  }
}

function frameLabel(node) {
  const cf = node.callFrame;
  const name = cf.functionName || "(anonymous)";
  const url = cf.url ? cf.url.replace(/^file:\/\//, "").replace(/.*\//, "") : "";
  const line = cf.lineNumber >= 0 ? `:${cf.lineNumber + 1}` : "";
  return url ? `${name}  (${url}${line})` : name;
}

// Top self-time frames.
const selfEntries = [...selfMicrosByNode.entries()]
  .map(([id, micros]) => ({ node: nodesById.get(id), micros }))
  .filter((e) => e.node)
  .sort((a, b) => b.micros - a.micros);

const totalMs = totalMicros / 1000;
console.log(`\nTotal sampled time: ${totalMs.toFixed(1)}ms  (${profile.samples.length} samples)\n`);

console.log("=== Top self-time frames ===");
console.log("rank  self_ms   self_%   function");
for (let i = 0; i < Math.min(topN, selfEntries.length); i++) {
  const e = selfEntries[i];
  const ms = e.micros / 1000;
  const pct = (e.micros / totalMicros) * 100;
  console.log(
    `${String(i + 1).padStart(4)}  ${ms.toFixed(1).padStart(8)}  ${pct.toFixed(1).padStart(5)}%  ${frameLabel(e.node)}`
  );
}

// Compute inclusive (total) time by propagating self time up to ancestors.
const inclusiveMicrosByNode = new Map();
for (const [nodeId, selfMicros] of selfMicrosByNode) {
  let cur = nodeId;
  while (cur != null) {
    inclusiveMicrosByNode.set(cur, (inclusiveMicrosByNode.get(cur) ?? 0) + selfMicros);
    cur = parentOf.get(cur) ?? null;
  }
}

const inclusiveEntries = [...inclusiveMicrosByNode.entries()]
  .map(([id, micros]) => ({ node: nodesById.get(id), micros }))
  .filter((e) => e.node)
  .sort((a, b) => b.micros - a.micros);

console.log("\n=== Top inclusive-time frames (total, ancestors included) ===");
console.log("rank  total_ms  total_%  function");
for (let i = 0; i < Math.min(topN, inclusiveEntries.length); i++) {
  const e = inclusiveEntries[i];
  const ms = e.micros / 1000;
  const pct = (e.micros / totalMicros) * 100;
  console.log(
    `${String(i + 1).padStart(4)}  ${ms.toFixed(1).padStart(8)}  ${pct.toFixed(1).padStart(5)}%  ${frameLabel(e.node)}`
  );
}

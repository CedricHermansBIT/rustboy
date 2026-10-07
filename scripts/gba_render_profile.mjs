// Attribute an isolated CDP CPU profile built with gba-profile-rendering.
// Counts inclusive samples, not only hitCount: compositor callees belong to
// rendering, while timers/audio in advance_video must not be classified as PPU.
import fs from 'node:fs/promises';
import {pathToFileURL} from 'node:url';

export function summarizeRendering(profile) {
  if (!Array.isArray(profile.nodes) || !Array.isArray(profile.samples) || !profile.samples.length) {
    throw new Error('Expected a nonempty CDP CPU profile with nodes and samples');
  }
  const nodes = new Map(profile.nodes.map(node => [node.id, node]));
  const parents = new Map();
  for (const node of profile.nodes) {
    for (const child of node.children || []) {
      if (parents.has(child)) throw new Error('Profile has multiple parents for a node');
      parents.set(child, node.id);
    }
  }
  const rendering = /::ppu::render_line$/;
  const scheduling = /::advance_video$/;
  if (!profile.nodes.some(node => rendering.test(node.callFrame.functionName))) {
    throw new Error('No compositor boundary: build with --features gba-profile-rendering');
  }
  let renderingSamples = 0;
  let videoOtherSamples = 0;
  for (const id of profile.samples) {
    let isRendering = false;
    let isVideo = false;
    const seen = new Set();
    for (let current = id; current !== undefined; current = parents.get(current)) {
      if (seen.has(current)) throw new Error('Profile has a cyclic parent chain');
      seen.add(current);
      const node = nodes.get(current);
      if (!node) throw new Error(`Unknown profile node ${current}`);
      isRendering ||= rendering.test(node.callFrame.functionName);
      isVideo ||= scheduling.test(node.callFrame.functionName);
    }
    renderingSamples += Number(isRendering);
    videoOtherSamples += Number(isVideo && !isRendering);
  }
  const samples = profile.samples.length;
  return {
    samples,
    renderingSamples,
    renderingPercent: 100 * renderingSamples / samples,
    videoOtherSamples,
    videoOtherPercent: 100 * videoOtherSamples / samples,
    // An illustrative upper bound if sampling is representative and rendering
    // becomes free. Not a prediction for a worker or a different workload.
    zeroCostRenderingCeiling: samples / (samples - renderingSamples),
  };
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  try {
    if (process.argv.length !== 3) throw new Error('usage: node scripts/gba_render_profile.mjs <profile.cpuprofile>');
    console.log(JSON.stringify(summarizeRendering(JSON.parse(await fs.readFile(process.argv[2], 'utf8'))), null, 2));
  } catch (error) {
    console.error(error.message);
    process.exitCode = 1;
  }
}

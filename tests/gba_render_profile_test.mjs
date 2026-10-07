import assert from 'node:assert/strict';
import {test} from 'node:test';
import {summarizeRendering} from '../scripts/gba_render_profile.mjs';

function profile() {
  const node = (id, functionName, children = []) => ({id, callFrame: {functionName}, children});
  return {
    nodes: [node(1, '(root)', [2, 6]), node(2, 'Machine::advance_video', [3, 5]),
      node(3, 'rustboy_gba[hash]::ppu::render_line', [4]), node(4, 'palette'),
      node(5, 'clock_audio'), node(6, 'Cpu::step')],
    samples: [3, 4, 5, 2, 6, 6],
  };
}
test('includes compositor descendants but excludes audio and CPU', () => {
  const result = summarizeRendering(profile());
  assert.equal(result.samples, 6);
  assert.equal(result.renderingSamples, 2);
  assert.equal(result.videoOtherSamples, 2);
  assert.equal(result.zeroCostRenderingCeiling, 1.5);
});
test('refuses profiles without an explicit rendering boundary', () => {
  const data = profile();
  data.nodes[2].callFrame.functionName = 'inlined';
  assert.throws(() => summarizeRendering(data), /compositor boundary/);
});
test('refuses empty, missing, multiply-parented and cyclic profile nodes', () => {
  assert.throws(() => summarizeRendering({nodes: [], samples: []}), /nonempty/);
  const missing = profile();
  missing.samples.push(123);
  assert.throws(() => summarizeRendering(missing), /Unknown/);
  const shared = profile();
  shared.nodes[5].children.push(4);
  assert.throws(() => summarizeRendering(shared), /multiple parents/);
  const cyclic = profile();
  cyclic.nodes[3].children.push(1);
  assert.throws(() => summarizeRendering(cyclic), /cyclic/);
});

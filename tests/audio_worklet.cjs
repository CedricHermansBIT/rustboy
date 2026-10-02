const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
let Processor;
vm.runInNewContext(fs.readFileSync(path.join(__dirname, '../web/audio-worklet.js'), 'utf8'), {
  Float32Array,
  AudioWorkletProcessor: class { constructor() { this.port = {}; } },
  registerProcessor(name, type) { assert.equal(name, 'rustboy-audio'); Processor = type; },
});
const processor = new Processor();
function queue(count, left = 0.5, right = -0.25) {
  processor.port.onmessage({data: {type: 'samples', left: new Float32Array(count).fill(left), right: new Float32Array(count).fill(right)}});
}
function render(size = 128) {
  const output = [new Float32Array(size), new Float32Array(size)];
  assert.equal(processor.process([], [output]), true);
  return output;
}
queue(4096);
assert.ok(render()[0].every(value => value === 0));
assert.equal(processor.available, 4096);
queue(4096);
let [left, right] = render();
assert.equal(left[0], 0.5 / 64);
assert.equal(left[63], 0.5);
assert.equal(right[63], -0.25);
assert.ok(left.subarray(64).every(value => value === 0.5));
render(8192 - 128);
[left, right] = render();
assert.equal(left[0], 0.5 * 63 / 64);
assert.equal(left[63], 0);
assert.equal(right[0], -0.25 * 63 / 64);
assert.ok(left.subarray(64).every(value => value === 0));
assert.equal(processor.primed, false);
queue(8192); assert.equal(render()[0][0], 0.5 / 64);
queue(40000, -0.5, 0.25);
assert.equal(processor.available, 32768);
[left] = render(256);
assert.ok(Math.abs(left[0] - 0.5) < 0.02, 'overflow must crossfade, not jump');
assert.equal(left[63], -0.5);
processor.port.onmessage({data: {type: 'reset'}});
assert.equal(processor.available, 0);
assert.ok(render()[0].every(value => value === 0));
processor.port.onmessage({data: {type: 'samples', left: Float32Array.of(NaN), right: Float32Array.of(Infinity)}});
assert.equal(processor.left[0], 0); assert.equal(processor.right[0], 0);
processor.port.onmessage({data: {type: 'samples', left: new Float32Array(2), right: new Float32Array(1)}});
assert.equal(processor.available, 1, 'malformed chunks are ignored');
assert.equal(processor.process([], [[]]), true);
console.log('Audio worklet: stereo, prebuffer, variable blocks, smooth underrun/recovery/overflow, reset and malformed chunks passed');

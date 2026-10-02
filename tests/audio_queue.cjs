// Run with node tests/audio_queue.cjs; exercises the actual inline audio code.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const html = fs.readFileSync(path.join(__dirname, '..', 'index.html'), 'utf8');
const audio = html.slice(html.indexOf('      /* ── Audio ── */'), html.indexOf('      /* ── Picker ── */'));
assert.ok(audio.includes('function initAudio()'));
const clear = html.slice(html.indexOf('      function clearAudioQueue()'), html.indexOf('      async function refreshStateIndex()'));
let processor;
class AudioContext {
  constructor() { this.state = 'running'; this.sampleRate = 44100; }
  createGain() { return {gain: {value: 0}, connect() {}}; }
  createScriptProcessor(size) {
    assert.equal(size, 4096);
    return processor = {connect() {}};
  }
  resume() { return Promise.resolve(); }
}
const context = vm.createContext({
  window: {AudioContext, addEventListener() {}, removeEventListener() {}},
  localStorage: {getItem() {return null;}},
});
vm.runInContext(audio + '\n' + clear, context);
const run = code => vm.runInContext(code, context);
const available = () => run('audioSamplesAvailable');
function callback() {
  const channels = [new Float32Array(4096), new Float32Array(4096)];
  processor.onaudioprocess({outputBuffer: {getChannelData: i => channels[i]}});
  return channels;
}
function queue(count, left = 0.25, right = -0.5) {
  context.left = new Float32Array(count).fill(left);
  context.right = new Float32Array(count).fill(right);
  run('queueAudioSamples(left, right)');
}
run('initAudio()');
queue(4096);
assert.ok(callback().every(channel => channel.every(value => value === 0)));
assert.equal(available(), 4096, 'prebuffer must not consume queued samples');
queue(4096);
let [left, right] = callback();
assert.ok(left.every(value => value === 0.25));
assert.ok(right.every(value => value === -0.5));
callback();
callback(); // underrun: return to prebuffering
assert.equal(run('audioPrimed'), false);
queue(4096);
assert.ok(callback()[0].every(value => value === 0));
assert.equal(available(), 4096);
queue(4096);
assert.ok(callback()[0].every(value => value === 0.25));
run('clearAudioQueue()');
assert.equal(available(), 0);
assert.equal(run('audioPrimed'), false);
// Overflow keeps the newest samples, including across the ring boundary.
context.left = Float32Array.from({length: 40000}, (_, i) => i);
context.right = Float32Array.from(context.left, value => -value);
run('queueAudioSamples(left, right)');
assert.equal(available(), 32768);
[left, right] = callback();
assert.equal(left[0], 7232);
assert.equal(left[4095], 11327);
assert.equal(right[0], -7232);
assert.equal(right[4095], -11327);
// A backend may emit at a different rate; interpolation must span chunk edges.
run('clearAudioQueue()');
context.left = Float32Array.of(0, 2);
context.right = Float32Array.of(0, -2);
run('queueAudioSamples(left, right, 22050)');
assert.equal(available(), 3);
context.left = Float32Array.of(4, 6);
context.right = Float32Array.of(-4, -6);
run('queueAudioSamples(left, right, 22050)');
assert.deepEqual(Array.from(run('audioLeftBuf.slice(0, audioSamplesAvailable)')), [0, 1, 2, 3, 4, 5, 6]);
assert.deepEqual(Array.from(run('audioRightBuf.slice(0, audioSamplesAvailable)')), [0, -1, -2, -3, -4, -5, -6]);
// A rate switch clears stale queued audio; empty chunks must not damage phase.
run('queueAudioSamples(new Float32Array(0), new Float32Array(0), 44100)');
assert.equal(available(), 0);
queue(1, 0.5, -0.5);
assert.equal(available(), 1);
assert.throws(() => run('queueAudioSamples(left, right, 0)'), /Invalid backend audio/);
assert.throws(() => run('queueAudioSamples(left, new Float32Array(0))'), /Invalid backend audio/);
// The actual audio device may have a different rate than the requested one.
run('clearAudioQueue(); audioCtx.sampleRate = 48000');
context.left = Float32Array.from({length: 441}, (_, i) => i);
context.right = Float32Array.from(context.left, value => -value);
run('queueAudioSamples(left, right, 44100)');
assert.ok(available() >= 479 && available() <= 480);
// Downsampling also retains its fractional phase across small/empty chunks.
run('clearAudioQueue(); audioCtx.sampleRate = 44100');
context.left = Float32Array.of(0, 1);
context.right = Float32Array.of(0, -1);
run('queueAudioSamples(left, right, 88200)');
context.left = Float32Array.of(2, 3, 4);
context.right = Float32Array.of(-2, -3, -4);
run('queueAudioSamples(new Float32Array(0), new Float32Array(0), 88200); queueAudioSamples(left, right, 88200)');
assert.deepEqual(Array.from(run('audioLeftBuf.slice(0, audioSamplesAvailable)')), [0, 2, 4]);
console.log('Audio queue: buffering, stereo, underrun, reset, overflow, and backend/device rate conversion passed');

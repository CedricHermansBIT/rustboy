import assert from 'node:assert/strict';
import { LocalLinkCoordinator, isLocalLinkBinary, LOCAL_LINK_INPUT_DELAY, LOCAL_LINK_QUEUE_LIMIT,
  LOCAL_LINK_HEADER_BYTES, LOCAL_LINK_CHUNK_BYTES, LOCAL_LINK_MAX_STATE, LOCAL_LINK_MAX_ROM } from '../web/gba-link-local.mjs';

const FRAME_MS = 1000 * 280896 / 16777216;
const rom = (seed = 1, size = 32768) => {
  const data = Uint8Array.from({ length: size }, (_, i) => (i * 17 + seed) & 255);
  data[0xb2] = 0x96; return data;
};
const snapshot = (seed, size = 100) => Uint8Array.from({ length: size }, (_, i) => (i * 13 + seed) & 255);

function endpoint(player, { download = false, key = 'same-rom', stateSize = 100, romSize = 32768, transfers = 4000 } = {}) {
  const saved = snapshot(player + 1, stateSize);
  const image = download ? new Uint8Array() : rom(key === 'same-rom' ? 1 : player + 1, romSize);
  const state = { connected: false, frame: 0, queued: 0, keys: 0, transfers: 0, paused: false };
  const calls = { snapshots: 0, roms: 0, starts: [], inputs: [], paused: 0, detached: 0 };
  const queued = []; const completed = [];
  let credit = 0; let other;
  const adapter = {
    identity: () => ({ romKey: download ? '' : key, stateId: `snapshot-${player}-${key}`, download }),
    snapshot: () => { calls.snapshots++; return saved; },
    rom: () => { calls.roms++; return image; },
    title: () => download ? 'Download client' : `Fixture ${player}`,
    localStart: (peerRom, peerState, peerDownload, assignedPlayer) => {
      assert.equal(assignedPlayer, player);
      // Stand-in for native checksum/identity validation. Transport must supply
      // the complete untouched peer snapshot before this operation is allowed.
      assert.deepEqual(peerState, other.saved, 'native import receives the exact snapshot');
      assert.equal(peerDownload, other.download);
      const same = !download && !other.download && key === other.key;
      assert.deepEqual(peerRom, same || other.download ? new Uint8Array() : other.image);
      calls.starts.push({ peerRom: peerRom.slice(), peerState: peerState.slice(), peerDownload, assignedPlayer });
      state.connected = true; state.frame = 0; state.queued = 0; state.transfers = 0;
    },
    localInput: (frame, parentKeys, childKeys) => {
      assert.ok(state.connected); assert.equal(frame, state.frame + queued.length);
      assert.ok(queued.length < LOCAL_LINK_QUEUE_LIMIT);
      const input = { frame, parentKeys, childKeys };
      queued.push(input); calls.inputs.push(input); state.queued = queued.length;
    },
    localStatus: () => ({ ...state }),
    detach: () => { calls.detached++; state.connected = false; queued.length = 0; state.queued = 0; },
    pause: () => { calls.paused++; state.paused = true; },
  };
  return { adapter, state, calls, saved, image, download, key, queued, completed,
    setPeer: peer => { other = peer; },
    // This fake models only frame consumption, not GBA hardware. The Rust
    // paired-engine suite independently verifies CPU/DMA/SIO determinism.
    advance(ms, ready) {
      if (!state.connected || !ready || state.paused) return;
      credit = Math.min(FRAME_MS, credit + ms);
      if (credit >= FRAME_MS && queued.length) {
        const input = queued.shift(); assert.equal(input.frame, state.frame);
        completed.push(input); state.frame++; state.queued = queued.length;
        state.transfers += transfers; credit -= FRAME_MS;
      }
    },
  };
}

function pair({ delay = 0, options = [{}, {}] } = {}) {
  const endpoints = [endpoint(0, options[0]), endpoint(1, options[1])];
  endpoints[0].setPeer(endpoints[1]); endpoints[1].setPeer(endpoints[0]);
  let time = 0; let sequence = 0;
  const network = []; const packets = []; const statuses = [[], []];
  const buffered = [0, 0]; const maxBuffered = [0, 0];
  const coordinators = endpoints.map((entry, player) => new LocalLinkCoordinator({ adapter: entry.adapter, player,
    now: () => time, bufferedAmount: () => buffered[player], onStatus: message => statuses[player].push(message),
    send: raw => {
      packets.push({ player, packet: JSON.parse(raw) });
      network.push({ at: time + delay, sequence: sequence++, to: 1 - player, raw });
    },
    sendBinary: binary => {
      assert.ok(isLocalLinkBinary(binary)); buffered[player] += binary.byteLength;
      maxBuffered[player] = Math.max(maxBuffered[player], buffered[player]);
      packets.push({ player, binary: binary.slice(0) });
      network.push({ at: time + delay, sequence: sequence++, to: 1 - player, binary });
    },
  }));
  const deliver = () => {
    let budget = 20000;
    while (budget--) {
      network.sort((a, b) => a.at - b.at || a.sequence - b.sequence);
      if (!network.length || network[0].at > time) break;
      const item = network.shift();
      if (item.binary) { buffered[1 - item.to] -= item.binary.byteLength; coordinators[item.to].receiveBinary(item.binary); }
      else coordinators[item.to].receive(item.raw);
    }
    assert.ok(budget > 0, 'protocol must settle without a message loop');
  };
  const advance = milliseconds => {
    for (let elapsed = 0; elapsed < milliseconds; elapsed++) {
      time++; deliver(); coordinators.forEach(coordinator => coordinator.poll());
      endpoints.forEach((entry, player) => entry.advance(1, coordinators[player].hello));
    }
    deliver(); coordinators.forEach(coordinator => coordinator.poll()); deliver();
  };
  const start = () => { coordinators.forEach(coordinator => coordinator.start()); deliver(); };
  const ready = () => {
    for (let elapsed = 0; !coordinators.every(coordinator => coordinator.hello) && elapsed < 5000; elapsed++) advance(1);
    assert.ok(coordinators.every(coordinator => coordinator.hello), statuses.flat().join('; '));
  };
  return { endpoints, coordinators, network, packets, statuses, buffered, maxBuffered, start, ready, advance, deliver,
    get time() { return time; } };
}

{
  const p = pair();
  assert.deepEqual(p.endpoints.map(entry => entry.calls.snapshots), [0, 0], 'constructor exports nothing before Start');
  p.start(); p.ready();
  assert.deepEqual(p.endpoints.map(entry => [entry.calls.snapshots, entry.calls.roms, entry.calls.starts.length]), [[1, 1, 1], [1, 1, 1]]);
  const binaries = p.packets.filter(item => item.binary);
  assert.equal(binaries.length, 2, 'same cartridge sends only the two snapshots');
  assert.ok(binaries.every(item => new Uint8Array(item.binary)[8] === 1));
  for (const entry of p.endpoints) assert.deepEqual(entry.calls.inputs.slice(0, 6), Array.from({ length: 6 }, (_, frame) => ({ frame, parentKeys: 0, childKeys: 0 })));
  p.endpoints[0].state.keys = 0x10; p.endpoints[1].state.keys = 0x20;
  p.advance(150);
  assert.ok(p.endpoints.every(entry => entry.completed.length > 6));
  assert.deepEqual(p.endpoints[0].completed, p.endpoints[1].completed, 'both roles receive the same parent/child frame inputs');
  assert.equal(p.coordinators[0].transfers, p.endpoints[0].state.transfers);
}

{
  const p = pair({ options: [{ key: 'game-a', stateSize: LOCAL_LINK_CHUNK_BYTES + 17, romSize: 50000 },
    { key: 'game-b', stateSize: LOCAL_LINK_CHUNK_BYTES + 3, romSize: 35000 }] });
  p.start(); p.ready();
  const binaries = p.packets.filter(item => item.binary);
  assert.equal(binaries.filter(item => new Uint8Array(item.binary)[8] === 2).length, 7);
  assert.ok(binaries.every(item => item.binary.byteLength <= LOCAL_LINK_HEADER_BYTES + LOCAL_LINK_CHUNK_BYTES));
  for (const entry of p.endpoints) assert.equal(entry.calls.starts.length, 1);
}

{
  const p = pair({ options: [{}, { download: true }] }); p.start(); p.ready();
  assert.equal(p.endpoints[0].calls.starts[0].peerDownload, true);
  assert.equal(p.endpoints[0].calls.starts[0].peerRom.length, 0);
  assert.deepEqual(p.endpoints[1].calls.starts[0].peerRom, p.endpoints[0].image);
}

{
  const p = pair({ delay: 80 });
  p.start(); p.ready();
  const before = p.endpoints.map(entry => entry.state.frame);
  const at = p.time;
  for (let elapsed = 0; elapsed < 5000; elapsed++) {
    p.endpoints[0].state.keys = Math.floor(elapsed / 100) % 2 ? 0x10 : 0;
    p.endpoints[1].state.keys = Math.floor(elapsed / 130) % 2 ? 0x20 : 0;
    p.advance(1);
  }
  const fps = p.endpoints.map((entry, index) => (entry.state.frame - before[index]) * 1000 / (p.time - at));
  assert.ok(fps.every(value => value > 50), `80 ms one-way delay sustains frame pipeline: ${fps}`);
  const frames = Math.min(...p.endpoints.map(entry => entry.completed.length));
  assert.deepEqual(p.endpoints[0].completed.slice(0, frames), p.endpoints[1].completed.slice(0, frames));
  assert.ok(p.endpoints.every(entry => entry.state.transfers > 1_000_000), 'native serial work is independent of packet count');
  const controls = p.packets.filter(item => item.packet).map(item => item.packet);
  const inputs = controls.filter(packet => packet.type === 'local-input');
  assert.ok(inputs.length < 700, 'one input message per frame, not one packet per serial transfer');
  assert.ok(controls.every(packet => ['local-hello', 'local-installed', 'local-input', 'local-ping'].includes(packet.type)));
  assert.ok(p.coordinators.every(coordinator => !coordinator.closed));
  console.log(`Local paired input pipeline: ${fps.map(value => value.toFixed(1)).join('/')} fps with 80 ms one-way delay; ${inputs.length} input messages for ${p.endpoints[0].state.transfers} native serial events`);
}

{
  const p = pair({ delay: 10, options: [{ stateSize: 700000 }, { stateSize: 700000 }] });
  p.start(); p.ready();
  assert.ok(p.maxBuffered.every(bytes => bytes <= 262144 + LOCAL_LINK_CHUNK_BYTES + LOCAL_LINK_HEADER_BYTES), 'bootstrap obeys data-channel backpressure');
  assert.equal(p.coordinators[0].ownState, null); assert.equal(p.coordinators[0].ownRom, null);
  assert.equal(p.coordinators[0].peerState, null); assert.equal(p.coordinators[0].peerRom, null);
}

{
  const p = pair({ delay: 10 });
  p.coordinators[0].start(); p.coordinators[1].start();
  // Corrupt one byte; native validation rejects it before installation or queue.
  p.advance(10);
  const binary = p.network.find(item => item.binary && item.to === 1).binary;
  new Uint8Array(binary)[LOCAL_LINK_HEADER_BYTES] ^= 0xff;
  p.advance(30);
  assert.ok(p.coordinators.every(coordinator => coordinator.closed));
  assert.equal(p.endpoints[1].calls.starts.length, 0);
  assert.equal(p.endpoints[1].calls.inputs.length, 0);
}

{
  for (const packet of [
    { v: 1, type: 'local-hello' },
    { v: 2, type: 'local-input', frame: 8, keys: 0 },
    { v: 2, type: 'local-input', frame: 7, keys: 0x1000 },
    { v: 2, type: 'local-installed', player: 1 },
  ]) {
    const p = pair(); p.start(); p.ready(); p.coordinators[0].receive(JSON.stringify(packet)); p.deliver();
    assert.ok(p.coordinators[0].closed); assert.equal(p.endpoints[0].calls.paused, 1);
  }
  const p = pair(); p.start(); p.ready();
  p.endpoints[0].state.frame += 100;
  p.coordinators[0].poll(); p.deliver(); assert.ok(p.coordinators[0].closed);
}

{
  const p = pair({ delay: 10 }); p.start();
  const hello = p.network.find(item => item.to === 0 && item.raw);
  hello.raw = JSON.stringify({ ...JSON.parse(hello.raw), stateSize: LOCAL_LINK_MAX_STATE + 1 });
  p.advance(30); assert.ok(p.coordinators[0].closed);
  const second = pair({ delay: 10 }); second.start();
  const secondHello = second.network.find(item => item.to === 0 && item.raw);
  secondHello.raw = JSON.stringify({ ...JSON.parse(secondHello.raw), romSize: LOCAL_LINK_MAX_ROM + 1 });
  second.advance(30); assert.ok(second.coordinators[0].closed);
}

{
  const p = pair({ delay: 10 }); p.start(); p.advance(10);
  const item = p.network.find(item => item.to === 0 && item.binary);
  new DataView(item.binary).setUint32(12, 1, true);
  p.advance(30); assert.ok(p.coordinators[0].closed);
  assert.equal(isLocalLinkBinary(new ArrayBuffer(8)), false);
  assert.equal(isLocalLinkBinary(new Uint8Array([1, 2, 3])), false);
}

{
  const p = pair(); p.start(); p.ready();
  p.coordinators[0].close('User disconnected'); p.deliver();
  assert.ok(p.coordinators.every(coordinator => coordinator.closed));
  assert.equal(p.endpoints[1].calls.paused, 1);
  for (const coordinator of p.coordinators) {
    assert.equal(coordinator.ownState, null); assert.equal(coordinator.ownRom, null);
    assert.equal(coordinator.peerState, null); assert.equal(coordinator.peerRom, null);
    assert.equal(coordinator.ownInputs.size, 0); assert.equal(coordinator.peerInputs.size, 0);
    assert.equal(coordinator.outgoing.length, 0);
  }
  const timeout = pair({ delay: 10 }); timeout.start(); timeout.network.length = 0;
  timeout.advance(20001); assert.ok(timeout.coordinators.every(coordinator => coordinator.closed));
  assert.ok(timeout.statuses.flat().some(message => /stopped responding/.test(message)));
}

assert.equal(LOCAL_LINK_INPUT_DELAY, 6);
console.log('Local paired link: explicit bootstrap, equal/different/download cartridges, ordered snapshots/inputs, native validation, bounded buffers/queues, delayed input pipeline, cleanup and timeout passed');

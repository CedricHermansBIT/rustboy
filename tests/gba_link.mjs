import assert from 'node:assert/strict';
import { CableCoordinator, LINK_FRAME_CLOCKS, encodePairing, decodePairing, parseIceServers,
  wasmLinkAdapter, RtcCable, RomTransfer, ROM_CHUNK_BYTES, MAX_ROM_BYTES, validateLinkRom, gatherIce } from '../web/gba-link.mjs';

// These transport endpoints deliberately do not implement CPU, SIO timing or
// graphics; those are tested in Rust. They expose whether network coordination
// fabricated clock progress, sampled the wrong word, or released a barrier.
function endpoint(player) {
  const state = { connected: false, player, clock: 100, mode: 2, ready: true, so: true, request: null };
  const calls = { supplied: [], received: [], absent: [], limits: [], paused: 0, peer: [] };
  let incoming = null;
  return { state, calls, adapter: {
    attach: value => { assert.equal(value, player); state.connected = true; },
    detach: () => { state.connected = false; state.request = null; incoming = null; },
    status: () => ({ ...state }),
    title: () => `Fixture ${player}`,
    limit: clock => { calls.limits.push(clock); },
    pause: () => { calls.paused++; },
    peer: value => { calls.peer.push(value); },
    receive: request => {
      assert.equal(state.mode, request.mode);
      assert.equal(incoming, null);
      incoming = request;
      calls.received.push(request);
      state.ready = false;
      return player === 0 ? 0x1234 : 0xabcd;
    },
    supply: (id, words) => {
      assert.equal(id, state.request?.id || incoming?.id);
      calls.supplied.push({ id, words });
      state.request = null; incoming = null; state.ready = true;
    },
    absent: id => {
      assert.equal(id, state.request.id);
      calls.absent.push(id); state.request = null;
    },
  } };
}

function pair() {
  const endpoints = [endpoint(0), endpoint(1)];
  const queue = []; const statuses = [[], []];
  let time = 0;
  const cable = endpoints.map((entry, player) => new CableCoordinator({
    adapter: entry.adapter, player, now: () => time,
    send: raw => queue.push({ to: 1 - player, raw, packet: JSON.parse(raw) }),
    onStatus: message => statuses[player].push(message),
  }));
  const deliver = () => { const message = queue.shift(); if (message) cable[message.to].receive(message.raw); return message; };
  const flush = () => { let budget = 100; while (queue.length && budget--) deliver(); assert.ok(budget > 0, 'protocol must settle'); };
  cable[0].start(); cable[1].start(); flush();
  return { endpoints, cable, queue, statuses, deliver, flush, advanceTime: delta => { time += delta; } };
}

{
  const code = encodePairing({ type: 'offer', sdp: 'v=0\r\na=fixture\r\n' });
  assert.deepEqual(decodePairing(code, 'offer'), { type: 'offer', sdp: 'v=0\r\na=fixture\r\n' });
  assert.throws(() => decodePairing(code, 'answer'), /answer/);
  assert.throws(() => decodePairing('broken', 'offer'), /complete/);
  assert.throws(() => decodePairing('x'.repeat(200001), 'offer'), /Invalid/);
  assert.throws(() => decodePairing(JSON.stringify({ v: 3, type: 'offer', sdp: 'v=0' }), 'offer'), /complete/);
  assert.throws(() => decodePairing(encodePairing({ type: 'offer', sdp: '<html>' }), 'offer'), /complete/);
  assert.ok(parseIceServers()[0].urls.startsWith('stun:'));
  assert.deepEqual(parseIceServers('[{"urls":"turn:relay.example:3478","username":"a","credential":"b"}]'),
    [{ urls: ['turn:relay.example:3478'], username: 'a', credential: 'b' }]);
  assert.deepEqual(parseIceServers('[]'), []);
  assert.throws(() => parseIceServers('{'), /JSON array/);
  assert.throws(() => parseIceServers('[{"urls":"https://bad.example"}]'), /valid/);
  assert.throws(() => wasmLinkAdapter({}), /updated WASM/);
}

{
  const p = pair();
  assert.deepEqual(p.endpoints[0].calls.limits, [100, 100 + LINK_FRAME_CLOCKS]);
  assert.deepEqual(p.endpoints[1].calls.limits, [100, 100]);
  p.advanceTime(10000); p.cable.forEach(cable => cable.poll()); p.flush();
  assert.equal(p.endpoints[0].state.clock, 100, 'wall time must never advance the emulated clock');
  p.endpoints[0].state.clock += LINK_FRAME_CLOCKS;
  p.cable[0].poll(); p.flush();
  assert.equal(p.endpoints[0].calls.limits.length, 2, 'fast peer waits at the shared frame');
  assert.equal(p.endpoints[1].calls.limits.at(-1), 100 + LINK_FRAME_CLOCKS);
  p.endpoints[1].state.clock += LINK_FRAME_CLOCKS;
  p.cable[1].poll(); p.flush();
  assert.equal(p.endpoints[0].calls.limits.at(-1), 100 + 2 * LINK_FRAME_CLOCKS);
  assert.equal(p.endpoints[1].calls.limits.at(-1), 100 + LINK_FRAME_CLOCKS);
}

{
  const p = pair();
  p.endpoints[0].state.request = { id: 7, mode: 2, baud: 3, value: 0x1234 };
  p.cable[0].poll();
  while (p.queue[0]?.packet.type !== 'request') p.deliver();
  p.deliver();
  assert.equal(p.endpoints[1].calls.received.length, 1);
  assert.equal(p.endpoints[0].calls.supplied.length, 0);
  while (p.queue[0]?.packet.type !== 'commit') p.deliver();
  p.deliver();
  assert.deepEqual(p.endpoints[1].calls.supplied, [{ id: 7, words: [0x1234, 0xabcd] }]);
  assert.equal(p.endpoints[0].calls.supplied.length, 0, 'initiator waits for committed-peer acknowledgement');
  p.flush();
  assert.deepEqual(p.endpoints[0].calls.supplied, [{ id: 7, words: [0x1234, 0xabcd] }]);
  assert.deepEqual(p.cable.map(cable => cable.transfers), [1, 1]);
  assert.deepEqual(p.endpoints.map(entry => entry.state.clock), [100, 100]);
}

{
  const p = pair();
  p.endpoints[0].state.mode = 0;
  p.endpoints[1].state.mode = 0; p.endpoints[1].state.ready = false;
  p.endpoints[0].state.request = { id: 9, mode: 0, baud: 1, value: 0x5a };
  p.cable[0].poll(); p.flush();
  assert.deepEqual(p.endpoints[0].calls.absent, [9]);
  assert.equal(p.endpoints[1].calls.received.length, 0);
  assert.equal(p.endpoints[1].calls.supplied.length, 0, 'an unarmed slave receives no manufactured completion');
  assert.ok(p.cable.every(cable => !cable.closed));
}

{
  const p = pair();
  p.endpoints[0].state.request = { id: 11, mode: 2, baud: 0, value: 0x1234 };
  p.cable[0].poll();
  while (!p.queue.some(message => message.packet.type === 'commit')) p.deliver();
  const commit = p.queue.find(message => message.packet.type === 'commit');
  commit.raw = JSON.stringify({ ...commit.packet, words: [0xffff, 0xabcd] });
  p.flush();
  assert.equal(p.endpoints[1].calls.supplied.length, 0, 'a tampered commit is never delivered to emulated memory');
  assert.ok(p.cable.every(cable => cable.closed));
  assert.deepEqual(p.endpoints.map(entry => entry.calls.paused), [1, 1]);
}

{
  for (const packet of [JSON.stringify({ v: 2, type: 'ack', id: 77 }), JSON.stringify({ v: 2, type: 'frame', frame: 2 }),
    JSON.stringify({ v: 2, type: 'state', mode: 2, ready: 'yes', so: true }), 'x'.repeat(4097)]) {
    const p = pair(); p.cable[0].receive(packet); p.flush();
    assert.equal(p.cable[0].closed, true); assert.equal(p.endpoints[0].calls.paused, 1);
  }
  const p = pair(); p.advanceTime(20001); p.cable[0].poll(); p.flush();
  assert.equal(p.cable[0].closed, true);
  assert.match(p.statuses[0].at(-1), /stopped responding/);
}

{
  const p = pair();
  p.endpoints[0].state.connected = false;
  p.cable[0].poll(); p.flush();
  assert.ok(p.cable.every(cable => cable.closed), 'ROM/state changes tear down the live cable');
}

{
  const p = pair();
  p.endpoints[0].state.clock = 10100;
  p.endpoints[0].state.request = { id: 13, mode: 2, baud: 3, value: 0x1234 };
  p.cable[0].poll(); p.flush();
  assert.equal(p.endpoints[1].calls.received.length, 0, 'receiver must reach the transfer clock before sampling');
  assert.equal(p.endpoints[1].calls.limits.at(-1), 10100);
  p.endpoints[1].state.clock = 10100;
  p.cable[1].poll(); p.flush();
  assert.equal(p.endpoints[1].calls.received.length, 1);
  assert.deepEqual(p.cable.map(cable => cable.transfers), [1, 1]);
}

{
  const p = pair();
  p.endpoints[1].state.clock += 600; // Atomic guest BIOS copy/decompression.
  p.endpoints[0].state.request = { id: 17, mode: 2, baud: 3, value: 0x1234 };
  p.cable[0].poll(); p.flush();
  assert.ok(p.cable.every(cable => !cable.closed), 'a bounded BIOS service overrun must not disconnect a valid cartridge');
  assert.equal(p.cable[1].timingSlips, 1);
  assert.equal(p.cable[1].maxClockSkew, 600);
}

class FakePc {
  constructor(config) { this.config = config; this.iceGatheringState = 'complete'; this.signalingState = 'stable'; }
  createDataChannel(label, options) {
    this.channel = { label, ...options, readyState: 'connecting', close() { this.closed = true; } };
    return this.channel;
  }
  async createOffer() { return { type: 'offer', sdp: 'v=0\r\na=offer\r\n' }; }
  async createAnswer() { return { type: 'answer', sdp: 'v=0\r\na=answer\r\n' }; }
  async setLocalDescription(description) { this.localDescription = description; this.signalingState = description.type === 'offer' ? 'have-local-offer' : 'stable'; }
  async setRemoteDescription(description) { this.remoteDescription = description; this.signalingState = description.type === 'answer' ? 'stable' : 'have-remote-offer'; }
  close() { this.closed = true; }
}
{
  const pending = new EventTarget();
  pending.iceGatheringState = 'gathering';
  pending.localDescription = { type: 'offer', sdp: 'v=0\r\na=candidate:1 1 UDP 2122260223 phone.local 49152 typ host\r\n' };
  await gatherIce(pending, 10);
  assert.match(decodePairing(encodePairing(pending.localDescription), 'offer').sdp, /phone.local/,
    'pending STUN discovery preserves collected host candidates in the code');
  pending.localDescription.sdp = 'v=0\r\n';
  await assert.rejects(gatherIce(pending, 10), /No WebRTC connection addresses/,
    'a timeout without any candidates remains an actionable error');
  const complete = gatherIce(pending, 1000);
  pending.iceGatheringState = 'complete';
  pending.dispatchEvent(new Event('icegatheringstatechange'));
  await complete;
}
{
  const first = new RtcCable({ adapter: endpoint(0).adapter, PeerConnection: FakePc });
  const second = new RtcCable({ adapter: endpoint(1).adapter, PeerConnection: FakePc });
  const offer = await first.createOffer();
  assert.equal(first.pc.channel.ordered, true, 'cable packets use reliable ordered delivery');
  const answer = await second.acceptOffer(offer);
  await first.acceptAnswer(answer);
  assert.equal(first.pc.remoteDescription.type, 'answer');
  assert.equal(second.pc.remoteDescription.type, 'offer');
  await assert.rejects(() => first.acceptAnswer(answer), /Create.*offer/);
  first.close(); second.close();
  const unsupported = new RtcCable({ adapter: endpoint(0).adapter, PeerConnection: null });
  await assert.rejects(() => unsupported.createOffer(), /does not support WebRTC/);
}

console.log('GBA link: pairing, relay validation, mutual clock barriers, capture/commit exchange, readiness, disconnect and malformed-packet protection passed');

function romPair() {
  const queue = []; const loaded = []; const updates = [[], []]; const statuses = [[], []];
  const rom = [0, 1].map(player => new RomTransfer({
    send: packet => queue.push({ to: 1 - player, packet }),
    sendBinary: binary => queue.push({ to: 1 - player, binary }),
    onUpdate: update => updates[player].push(update),
    onStatus: status => statuses[player].push(status),
    loadRom: async (bytes, name) => loaded.push({ player, bytes: bytes.slice(), name }),
  }));
  const deliver = async () => {
    const item = queue.shift();
    if (item?.binary) rom[item.to].receiveBinary(item.binary);
    else if (item) await rom[item.to].receive(item.packet);
    return item;
  };
  const flush = async () => { let budget = 10000; while (queue.length && budget--) await deliver(); assert.ok(budget > 0); };
  const offer = async bytes => {
    const result = rom[0].offer(bytes, 'Original fixture.gba').then(() => ({ ok: true }), error => ({ error }));
    while (!queue.length) await new Promise(resolve => setTimeout(resolve, 1));
    await deliver(); return result;
  };
  return { queue, rom, loaded, updates, statuses, deliver, flush, offer };
}
function romBytes(size = ROM_CHUNK_BYTES * 4 + 123) {
  const bytes = Uint8Array.from({ length: size }, (_, i) => i * 17 & 255);
  bytes[0xb2] = 0x96; return bytes;
}

{
  const p = romPair(); const bytes = romBytes();
  // Do not await the completion promise until receiver has explicitly accepted.
  const completion = p.rom[0].offer(bytes, 'Original fixture.gba').then(() => ({ ok: true }), error => ({ error }));
  while (!p.queue.length) await new Promise(resolve => setTimeout(resolve, 1));
  assert.ok(JSON.stringify(p.queue[0].packet).length < 512, 'ROM offer contains metadata, not the ROM');
  await p.deliver();
  assert.equal(p.rom[1].active.bytes, undefined, 'receiver allocates no ROM storage before Accept');
  assert.equal(p.loaded.length, 0);
  p.rom[1].accept(); await p.deliver();
  const chunks = p.queue.filter(item => item.binary);
  assert.equal(chunks.length, 5);
  assert.ok(chunks.every(item => item.binary.byteLength <= ROM_CHUNK_BYTES + 8));
  await p.flush();
  assert.deepEqual(await completion, { ok: true });
  assert.equal(p.rom[1].active.phase, 'verified');
  assert.equal(p.loaded.length, 0, 'integrity verification must not automatically load a ROM');
  await p.rom[1].load();
  assert.equal(p.loaded[0].name, 'Original fixture.gba');
  assert.deepEqual(p.loaded[0].bytes, bytes);
  assert.equal(p.rom[1].active, null);
  assert.ok(p.updates[1].some(update => update?.phase === 'receiving' && update.received > 0));
}

{
  const p = romPair();
  const completion = p.rom[0].offer(romBytes(), 'Original fixture.gba').then(() => null, error => error);
  while (!p.queue.length) await new Promise(resolve => setTimeout(resolve, 1));
  await p.deliver(); p.rom[1].reject(); await p.flush();
  assert.match((await completion).message, /rejected/);
  assert.ok(!p.rom[0].active && !p.rom[1].active);
  assert.equal(p.loaded.length, 0);
}

{
  const p = romPair();
  const completion = p.rom[0].offer(romBytes(), 'Original fixture.gba').then(() => null, error => error);
  while (!p.queue.length) await new Promise(resolve => setTimeout(resolve, 1));
  await p.deliver(); p.rom[1].accept(); await p.deliver();
  new Uint8Array(p.queue.find(item => item.binary).binary)[8 + 256] ^= 0xff;
  await p.flush();
  assert.match((await completion).message, /cancelled/);
  assert.match(p.statuses[1].at(-1), /checksum did not match/);
  assert.equal(p.loaded.length, 0, 'corrupted ROM is never made loadable');
}

{
  const p = romPair();
  const completion = p.rom[0].offer(romBytes(), 'Original fixture.gba').then(() => null, error => error);
  while (!p.queue.length) await new Promise(resolve => setTimeout(resolve, 1));
  await p.deliver(); p.rom[1].accept(); await p.deliver();
  const chunk = p.queue.find(item => item.binary).binary;
  p.rom[1].receiveBinary(chunk);
  assert.throws(() => p.rom[1].receiveBinary(chunk), /out of sequence/);
  p.rom[1].cancel(); await p.flush();
  assert.match((await completion).message, /cancelled/);
  assert.equal(p.rom[1].active, null);
  assert.throws(() => p.rom[1].receiveBinary(new ArrayBuffer(ROM_CHUNK_BYTES + 9)), /Invalid/);
}

{
  const p = romPair();
  await assert.rejects(() => p.rom[0].offer(romBytes(), '../bad.gba'), /filename/);
  await assert.rejects(() => p.rom[0].offer(romBytes(), 'bad.gb'), /filename/);
  assert.throws(() => validateLinkRom(new Uint8Array(MAX_ROM_BYTES + 1), 'bad.gba'), /valid GBA ROM/);
  const bytes = romBytes(); bytes[0xb2] = 0;
  assert.throws(() => validateLinkRom(bytes, 'bad.gba'), /header/);
  await assert.rejects(() => p.rom[1].receive({ type: 'rom-offer', id: 1, size: MAX_ROM_BYTES + 1,
    name: 'bad.gba', hash: '0'.repeat(64) }), /Invalid/);
  await assert.rejects(() => p.rom[1].load(), /verify/);
}

{
  const p = romPair();
  let blocked = true; let transmitted = 0;
  p.rom[0].bufferedAmount = () => blocked ? 300000 : 0;
  const send = p.rom[0].sendBinary;
  p.rom[0].sendBinary = buffer => { assert.equal(blocked, false); transmitted++; send(buffer); };
  const completion = p.rom[0].offer(romBytes(), 'Original fixture.gba').then(() => ({ ok: true }), error => ({ error }));
  while (!p.queue.length) await new Promise(resolve => setTimeout(resolve, 1));
  await p.deliver(); p.rom[1].accept();
  const acceptance = p.deliver();
  await new Promise(resolve => setTimeout(resolve, 10));
  assert.equal(transmitted, 0, 'sender applies data-channel backpressure');
  blocked = false; await acceptance; await p.flush();
  assert.deepEqual(await completion, { ok: true });
  p.rom[1].close();
}

console.log('GBA ROM transfer: explicit acceptance/load, binary chunking, progress, SHA-256 integrity, rejection/cancel, size/header/name bounds and backpressure passed');

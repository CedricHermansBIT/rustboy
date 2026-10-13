import { LocalLinkCoordinator, isLocalLinkBinary } from './gba-link-local.mjs';
import { RoomPairing, roomServerUrl, inviteUrl } from './gba-link-rooms.mjs';
// Two-player GBA cable over a reliable, ordered WebRTC data channel. ROM files
// transfer only after sender and receiver actions. Starting linked play shares
// temporary initial session snapshots, then only delayed player inputs. Each
// device simulates the complete cable locally and persists its own saves.
export const LINK_VERSION = 2;
export const LINK_FRAME_CLOCKS = 280896;
// Replacement BIOS calls still execute atomically at instruction boundaries.
// Allow one frame of bounded overrun instead of disconnecting valid uploads;
// diagnostics expose the remaining instruction-granular timing approximation.
export const LINK_MAX_ATOMIC_OVERRUN = LINK_FRAME_CLOCKS;
const MAX_PACKET = 4096;
const MAX_PAIRING = 100000;
const DEFAULT_ICE = [{ urls: 'stun:stun.l.google.com:19302' }];

const integer = (value, max = 0xffffffff) => Number.isSafeInteger(value) && value >= 0 && value <= max;
const maskFor = mode => mode === 0 ? 0xff : mode === 2 ? 0xffff : 0xffffffff;

export function encodePairing(description) {
  const bytes = new TextEncoder().encode(JSON.stringify({ v: LINK_VERSION, type: description.type, sdp: description.sdp,
    ...(description.session ? { session: description.session } : {}) }));
  if (bytes.length > MAX_PAIRING) throw new Error('Pairing description is too large');
  let binary = '';
  for (const byte of bytes) binary += String.fromCharCode(byte);
  return `rustboy-link-v2:${btoa(binary)}`;
}

export function decodePairing(code, expectedType) {
  if (typeof code !== 'string' || code.length > MAX_PAIRING * 2) throw new Error('Invalid pairing code');
  try {
    const text = code.trim();
    const json = text.startsWith('rustboy-link-v2:')
      ? new TextDecoder('utf-8', { fatal: true }).decode(Uint8Array.from(atob(text.slice(16)), c => c.charCodeAt(0)))
      : text;
    const data = JSON.parse(json);
    if (data.v !== LINK_VERSION || data.type !== expectedType || typeof data.sdp !== 'string'
        || data.sdp.length > MAX_PAIRING || !data.sdp.startsWith('v=0')) throw new Error();
    if (data.session !== undefined && (typeof data.session !== 'string' || !/^[A-Za-z0-9-]{16,64}$/.test(data.session))) throw new Error();
    return { type: data.type, sdp: data.sdp, ...(data.session ? { session: data.session } : {}) };
  } catch {
    throw new Error(`Paste a complete RustBoy ${expectedType} code`);
  }
}

export function parseIceServers(text = '') {
  if (!text.trim()) return DEFAULT_ICE.map(server => ({ ...server }));
  let servers;
  try { servers = JSON.parse(text); } catch { throw new Error('Relay settings must be a JSON array'); }
  if (!Array.isArray(servers) || servers.length > 8) throw new Error('Use at most eight STUN/TURN servers');
  return servers.map(server => {
    const urls = typeof server?.urls === 'string' ? [server.urls] : server?.urls;
    if (!Array.isArray(urls) || !urls.length || urls.length > 8
        || urls.some(url => typeof url !== 'string' || url.length > 512 || !/^(stun|stuns|turn|turns):[^\s]+$/.test(url))) {
      throw new Error('Each relay needs valid stun:, stuns:, turn: or turns: URLs');
    }
    if (server.username !== undefined && (typeof server.username !== 'string' || server.username.length > 256)
        || server.credential !== undefined && (typeof server.credential !== 'string' || server.credential.length > 1024)) {
      throw new Error('Invalid relay username or credential');
    }
    return { urls, ...(server.username !== undefined ? { username: server.username } : {}),
      ...(server.credential !== undefined ? { credential: server.credential } : {}) };
  });
}

export function relayFormServers(url = '', username = '', password = '', custom = '') {
  if (!url.trim()) {
    if (username || password) throw new Error('Enter a TURN server address with the relay credentials');
    return parseIceServers(custom);
  }
  if (custom.trim()) throw new Error('Use the TURN fields or the custom server list, not both');
  if (!/^(turn|turns):/.test(url.trim())) throw new Error('TURN server must start with turn: or turns:');
  const relay = parseIceServers(JSON.stringify([{ urls: url.trim(), username, credential: password }]));
  return [...parseIceServers(), ...relay];
}

// The adapter is deliberately small, so protocol tests can exercise two
// independent emulated endpoints without introducing a second timing model.
export function wasmLinkAdapter(wasm) {
  const required = ['gba_link_attach', 'gba_link_detach', 'gba_link_status', 'gba_link_set_peer',
    'gba_link_receive', 'gba_link_supply', 'gba_link_absent', 'gba_link_set_limit'];
  if (required.some(name => typeof wasm[name] !== 'function')) {
    throw new Error('GBA link needs the updated WASM bundle; rebuild or refresh the page');
  }
  return {
    attach: player => wasm.gba_link_attach(player),
    detach: () => wasm.gba_link_detach(),
    status: () => {
      const status = JSON.parse(wasm.gba_link_status());
      if (!status || typeof status !== 'object' || status.clock === undefined || !integer(status.player, 1)) throw new Error('Load a GBA game before linking');
      status.clock = Number(status.clock);
      return status;
    },
    peer: state => wasm.gba_link_set_peer(state.mode, state.ready, state.so),
    receive: request => wasm.gba_link_receive(request.id, request.mode, request.baud),
    supply: (id, words) => wasm.gba_link_supply(id, words[0], words[1]),
    absent: id => wasm.gba_link_absent(id),
    limit: clock => wasm.gba_link_set_limit(clock),
    pause: () => wasm.set_paused?.(true),
    title: () => wasm.get_rom_title?.() || 'GBA game',
    reset: () => wasm.reset_emulator?.(),
    ...(['gba_local_link_start', 'gba_local_link_input', 'gba_local_link_status', 'gba_local_link_identity', 'export_state', 'export_gba_rom'].every(name => typeof wasm[name] === 'function') ? {
      snapshot: () => wasm.export_state(),
      rom: () => wasm.export_gba_rom(),
      identity: () => JSON.parse(wasm.gba_local_link_identity()),
      localStart: (rom, state, download, player) => wasm.gba_local_link_start(rom, state, download, player),
      localInput: (frame, parent, child) => wasm.gba_local_link_input(frame, parent, child),
      localStatus: () => JSON.parse(wasm.gba_local_link_status()),
    } : {}),
  };
}

export class CableCoordinator {
  constructor({ adapter, player, send, onStatus = () => {}, now = () => performance.now() }) {
    if (player !== 0 && player !== 1) throw new Error('Cable supports two players');
    this.adapter = adapter; this.player = player; this.sendPacket = send;
    this.onStatus = onStatus; this.now = now;
    this.started = false; this.closed = false; this.hello = false;
    this.target = 1; this.sentFrame = 0; this.peerFrame = 0;
    this.local = null; this.incoming = null; this.lastState = '';
    this.lastReceived = this.now(); this.lastHeartbeat = this.now();
    this.transfers = 0; this.capture = null; this.timingSlips = 0; this.maxClockSkew = 0;
  }
  send(packet) {
    if (this.closed) return;
    this.sendPacket(JSON.stringify({ v: LINK_VERSION, ...packet }));
  }
  start() {
    if (this.started || this.closed) return;
    this.adapter.attach(this.player);
    this.started = true;
    this.base = this.adapter.status().clock;
    if (!integer(this.base, Number.MAX_SAFE_INTEGER)) throw new Error('Invalid emulated clock');
    this.adapter.limit(this.base);
    this.send({ type: 'hello', player: this.player, title: String(this.adapter.title?.() || 'GBA game').slice(0, 128) });
    this.onStatus('Connected; waiting for the other emulator');
  }
  receive(raw) {
    if (this.closed) return;
    try {
      if (typeof raw !== 'string' || raw.length > MAX_PACKET) throw new Error('Invalid cable packet');
      const packet = JSON.parse(raw);
      if (packet?.v !== LINK_VERSION || typeof packet.type !== 'string') throw new Error('Incompatible link protocol');
      if (packet.type === 'bye') { this.close('The other player disconnected', true); return; }
      if (packet.type === 'hello') {
        if (!this.started) this.start();
        if (this.hello || packet.player !== 1 - this.player || typeof packet.title !== 'string' || packet.title.length > 128) {
          throw new Error('Invalid cable handshake');
        }
        this.hello = true;
        this.adapter.limit(this.base + (this.player === 0 ? LINK_FRAME_CLOCKS : 0));
        this.onStatus(`Linked as player ${this.player + 1} · peer: ${packet.title}`);
      } else {
        if (!this.hello) throw new Error('Cable handshake has not completed');
        this.handle(packet);
      }
      this.lastReceived = this.now();
      this.poll();
    } catch (error) {
      this.close(`Link stopped: ${error.message || error}`, true);
    }
  }
  handle(packet) {
    switch (packet.type) {
      case 'state': {
        if (!integer(packet.mode, 3) || typeof packet.ready !== 'boolean' || typeof packet.so !== 'boolean') {
          throw new Error('Invalid serial readiness');
        }
        this.adapter.peer(packet);
        break;
      }
      case 'frame': {
        if (!integer(packet.frame) || packet.frame !== this.peerFrame + 1 || packet.frame > this.target + 1) {
          throw new Error('Out-of-order emulated frame');
        }
        this.peerFrame = packet.frame;
        if (this.player === 1) this.adapter.limit(this.base + packet.frame * LINK_FRAME_CLOCKS);
        this.grant();
        break;
      }
      case 'request': {
        const request = packet.request;
        if (!request || !integer(request.id) || !request.id || !integer(request.mode, 2)
            || !integer(request.baud, 3) || !integer(request.value, maskFor(request.mode))) {
          throw new Error('Invalid serial request');
        }
        if (request.mode === 2 && this.player === 0) throw new Error('Multiplayer child cannot initiate');
        if (this.incoming || this.local || this.capture) throw new Error('Overlapping cable transfers');
        if (!integer(packet.clock, Number.MAX_SAFE_INTEGER) || packet.clock > this.target * LINK_FRAME_CLOCKS + LINK_MAX_ATOMIC_OVERRUN
            || packet.clock < (this.target - 1) * LINK_FRAME_CLOCKS) throw new Error('Invalid transfer clock');
        this.capture = { request, clock: this.base + packet.clock };
        // The child follows the parent's announced clock. It cannot run a
        // whole frame ahead and overwrite the send word before we sample it.
        this.adapter.limit(this.capture.clock);
        this.captureIncoming();
        break;
      }
      case 'response': {
        if (!this.local || packet.id !== this.local.id || this.local.phase !== 'request') {
          throw new Error('Stale cable response');
        }
        if (packet.absent === true) {
          this.adapter.absent(packet.id);
          this.local = null;
          this.grant();
          break;
        }
        if (!integer(packet.value, maskFor(this.local.mode))) throw new Error('Invalid cable response word');
        const words = [];
        words[this.player] = this.local.value;
        words[1 - this.player] = packet.value;
        this.local = { ...this.local, words, phase: 'commit' };
        this.send({ type: 'commit', id: packet.id, words });
        break;
      }
      case 'commit': {
        if (!this.incoming || packet.id !== this.incoming.id || !Array.isArray(packet.words) || packet.words.length !== 2
            || packet.words.some(word => !integer(word, maskFor(this.incoming.mode)))
            || packet.words[this.player] !== this.incoming.value || packet.words[1 - this.player] !== this.incoming.sentValue) {
          throw new Error('Cable commit does not match captured words');
        }
        this.adapter.supply(packet.id, packet.words);
        this.incoming = null;
        this.transfers += 1;
        this.send({ type: 'ack', id: packet.id });
        this.grant();
        break;
      }
      case 'ack': {
        if (!this.local || packet.id !== this.local.id || this.local.phase !== 'commit') throw new Error('Stale cable acknowledgement');
        this.adapter.supply(packet.id, this.local.words);
        this.local = null;
        this.transfers += 1;
        this.grant();
        break;
      }
      case 'ping': break;
      default: throw new Error('Unknown cable packet');
    }
  }
  captureIncoming() {
    if (!this.capture) return;
    const { request, clock } = this.capture;
    const state = this.adapter.status();
    if (state.clock < clock) return;
    if (state.clock > clock + LINK_MAX_ATOMIC_OVERRUN) throw new Error('Receiver ran too far ahead of the serial transfer');
    const skew = state.clock - clock;
    this.maxClockSkew = Math.max(this.maxClockSkew, skew);
    if (skew > 64) this.timingSlips += 1;
    this.capture = null;
    if (state.mode !== request.mode || request.mode < 2 && !state.ready) {
      this.send({ type: 'response', id: request.id, absent: true });
      return;
    }
    const value = this.adapter.receive(request);
    if (!integer(value, maskFor(request.mode))) throw new Error('Invalid captured serial word');
    this.incoming = { ...request, value, sentValue: request.value };
    this.send({ type: 'response', id: request.id, value });
  }
  grant() {
    if (this.sentFrame >= this.target && this.peerFrame >= this.target && !this.local && !this.incoming && !this.capture) {
      this.target += 1;
      if (this.player === 0) this.adapter.limit(this.base + this.target * LINK_FRAME_CLOCKS);
    }
  }
  poll() {
    if (this.closed || !this.started) return;
    try {
      const state = this.adapter.status();
      if (!state.connected || state.player !== this.player || state.clock < this.base) {
        throw new Error('Emulator changed; connect both players again');
      }
      if (this.now() - this.lastReceived > 20000) throw new Error('Other player stopped responding');
      if (!this.hello) return;
      this.captureIncoming();
      const signal = { type: 'state', mode: state.mode, ready: state.ready, so: state.so };
      const signature = JSON.stringify(signal);
      if (signature !== this.lastState) { this.lastState = signature; this.send(signal); }
      if (state.request && !this.local) {
        this.local = { ...state.request, phase: 'request' };
        this.send({ type: 'request', request: state.request, clock: state.clock - this.base });
      }
      if (state.clock - this.base >= this.target * LINK_FRAME_CLOCKS && this.sentFrame < this.target) {
        this.sentFrame = this.target;
        this.send({ type: 'frame', frame: this.target });
      }
      this.grant();
      if (this.now() - this.lastHeartbeat >= 1000) {
        this.lastHeartbeat = this.now(); this.send({ type: 'ping' });
      }
    } catch (error) { this.close(`Link stopped: ${error.message || error}`, true); }
  }
  close(reason = 'Link disconnected', pause = false) {
    if (this.closed) return;
    try { this.send({ type: 'bye' }); } catch { /* Already closed transport. */ }
    this.closed = true;
    if (this.started) {
      try { this.adapter.detach(); } catch { /* The ROM may have been replaced. */ }
      if (pause) this.adapter.pause?.();
    }
    this.onStatus(reason);
  }
}

export function gatherIce(pc, timeout = 15000) {
  if (pc.iceGatheringState === 'complete') return Promise.resolve();
  return new Promise((resolve, reject) => {
    let settled = false;
    const finish = error => {
      if (settled) return;
      settled = true;
      clearTimeout(timer); pc.removeEventListener('icegatheringstatechange', changed);
      error ? reject(error) : resolve();
    };
    const changed = () => { if (pc.iceGatheringState === 'complete') finish(); };
    // An unreachable STUN/TURN server can keep gathering pending even after
    // host addresses are present. Preserve those candidates in the manual code
    // rather than declaring the whole connection impossible.
    const timer = setTimeout(() => {
      const hasCandidates = /(?:^|\r?\n)a=candidate:/.test(pc.localDescription?.sdp || '');
      finish(hasCandidates ? undefined : new Error('No WebRTC connection addresses were found. Check whether WebRTC is blocked in browser/privacy settings, or try another network or relay.'));
    }, timeout);
    pc.addEventListener('icegatheringstatechange', changed);
    changed();
  });
}

async function preparePeerDiscovery(pc, PeerConnection) {
  // Firefox advertises its mDNS names only after an answer starts ICE checks.
  // During manual pairing, that answer may arrive after the guest's lookup
  // expires. Complete an empty offer/answer first: this enables address
  // advertising without creating or retiring any data/ICE transport. A
  // rejected data section can leave native teardown queued and destroy the
  // real transport created immediately afterward (Firefox/Windows).
  // The real channel is created only after this empty negotiation.
  let answerBuilder;
  try {
    await pc.setLocalDescription(await pc.createOffer());
    answerBuilder = new PeerConnection({ iceServers: [] });
    await answerBuilder.setRemoteDescription(pc.localDescription);
    await pc.setRemoteDescription(await answerBuilder.createAnswer());
  } finally {
    answerBuilder?.close();
  }
}

// Diagnostic only: temporary native WebRTC peers exchange an echo, without the
// application's transport/pairing code, emulator state or another device.
export async function testBrowserWebRtc({ PeerConnection = globalThis.RTCPeerConnection, timeout = 20000 } = {}) {
  if (!PeerConnection) throw new Error('WebRTC is unavailable in this browser');
  const first = new PeerConnection({ iceServers: [] });
  let second, timer, outgoing;
  try {
    second = new PeerConnection({ iceServers: [] });
    await preparePeerDiscovery(first, PeerConnection);
    const result = new Promise((resolve, reject) => {
      timer = setTimeout(() => reject(new Error(`Native WebRTC self-test timed out (ICE ${first.iceConnectionState || 'unknown'}/${second.iceConnectionState || 'unknown'}).`)), timeout);
      outgoing = first.createDataChannel('rustboy-connection-test', { ordered: true });
      outgoing.onopen = () => outgoing.send('rustboy-test');
      outgoing.onmessage = event => {
        if (event.data === 'rustboy-test-reply') resolve('Native WebRTC self-test passed. This browser can connect to itself; another device or manual code exchange is not tested.');
      };
      outgoing.onerror = () => reject(new Error('Native WebRTC self-test data channel failed'));
      second.ondatachannel = event => {
        event.channel.onmessage = message => {
          if (message.data === 'rustboy-test') event.channel.send('rustboy-test-reply');
        };
      };
      const negotiate = async () => {
        await first.setLocalDescription(await first.createOffer());
        await gatherIce(first, Math.min(timeout, 5000));
        await second.setRemoteDescription(first.localDescription);
        await second.setLocalDescription(await second.createAnswer());
        await gatherIce(second, Math.min(timeout, 5000));
        await first.setRemoteDescription(second.localDescription);
      };
      negotiate().catch(reject);
    });
    return await result;
  } finally {
    clearTimeout(timer); outgoing?.close(); first.close(); second?.close();
  }
}

export const MAX_ROM_BYTES = 32 * 1024 * 1024;
export const ROM_CHUNK_BYTES = 16384;
export function validateLinkRom(bytes, name) {
  if (!ArrayBuffer.isView(bytes) || bytes.BYTES_PER_ELEMENT !== 1 || bytes.length < 192 || bytes.length > MAX_ROM_BYTES || bytes[0xb2] !== 0x96) {
    throw new Error('Choose a valid GBA ROM (192 bytes to 32 MiB, with a GBA header)');
  }
  if (typeof name !== 'string' || !name.length || name.length > 160 || /[\\/\x00-\x1f\x7f]/.test(name) || !/\.gba$/i.test(name)) {
    throw new Error('ROM needs a filename ending in .gba, without directories');
  }
}
const sha256 = async bytes => Array.from(new Uint8Array(await globalThis.crypto.subtle.digest('SHA-256', bytes)),
  byte => byte.toString(16).padStart(2, '0')).join('');

export class RomTransfer {
  constructor({ send, sendBinary, bufferedAmount = () => 0, onUpdate = () => {}, onStatus = () => {},
    loadRom = async () => { throw new Error('ROM loading is unavailable'); } }) {
    this.send = send; this.sendBinary = sendBinary; this.bufferedAmount = bufferedAmount;
    this.onUpdate = onUpdate; this.onStatus = onStatus; this.loadRom = loadRom;
    this.active = null; this.generation = 0; this.closed = false;
    this.retired = new Set();
  }
  update() {
    const active = this.active;
    this.onUpdate(active ? { id: active.id, direction: active.direction, phase: active.phase,
      name: active.name, size: active.size, received: active.offset || 0 } : null);
  }
  async offer(bytes, name) {
    if (this.closed) throw new Error('Connection is closed');
    if (this.active) throw new Error('Finish or cancel the current ROM transfer');
    validateLinkRom(bytes, name);
    if (typeof globalThis.crypto?.subtle?.digest !== 'function') throw new Error('ROM sharing needs an HTTPS page or localhost');
    const generation = ++this.generation;
    const id = globalThis.crypto.getRandomValues(new Uint32Array(1))[0] || 1;
    this.active = { id, direction: 'send', phase: 'hashing', bytes, name, size: bytes.length, offset: 0 };
    this.update();
    try {
      const hash = await sha256(bytes);
      if (this.closed || this.generation !== generation) throw new Error('ROM transfer cancelled');
      this.active.hash = hash; this.active.phase = 'offered';
      const complete = new Promise((resolve, reject) => { Object.assign(this.active, { resolve, reject }); });
      this.send({ type: 'rom-offer', id, name, size: bytes.length, hash });
      this.update();
      this.onStatus('ROM offered; the other player must choose Receive ROM');
      return await complete;
    } catch (error) {
      if (this.generation === generation) { this.active = null; this.update(); }
      throw error;
    }
  }
  async receive(packet) {
    if (this.closed) return;
    if (!integer(packet.id) || !packet.id) throw new Error('Invalid ROM transfer identifier');
    if (this.retired.has(packet.id)) return;
    if (packet.type === 'rom-offer') {
      if (!integer(packet.id) || !packet.id || !integer(packet.size, MAX_ROM_BYTES) || packet.size < 192
          || typeof packet.name !== 'string' || packet.name.length > 160 || /[\\/\x00-\x1f\x7f]/.test(packet.name)
          || !/\.gba$/i.test(packet.name) || typeof packet.hash !== 'string' || !/^[0-9a-f]{64}$/.test(packet.hash)) {
        throw new Error('Invalid ROM offer');
      }
      if (this.active) { this.send({ type: 'rom-reject', id: packet.id, reason: 'Another ROM transfer is active' }); return; }
      this.generation += 1;
      this.active = { ...packet, direction: 'receive', phase: 'offered', offset: 0 };
      this.update(); this.onStatus(`Peer offers ${packet.name}. Choose Receive ROM or Reject.`);
      return;
    }
    const active = this.active;
    if (!active || packet.id !== active.id) {
      // Cancellation can race with packets already buffered in the reliable
      // channel. Ignore packets from completed/cancelled transfer IDs.
      if (['rom-cancel', 'rom-reject', 'rom-verified'].includes(packet.type)) return;
      throw new Error('Unexpected ROM transfer response');
    }
    if (packet.type === 'rom-reject' || packet.type === 'rom-cancel') {
      this.finish(new Error(packet.type === 'rom-reject' ? 'Other player rejected the ROM' : 'Other player cancelled the ROM transfer'));
    } else if (packet.type === 'rom-accept') {
      if (active.direction !== 'send' || active.phase !== 'offered') throw new Error('Unexpected ROM acceptance');
      active.phase = 'sending'; this.update();
      const generation = this.generation;
      try {
        while (active.offset < active.size && this.generation === generation && !this.closed) {
          if (this.bufferedAmount() > 262144) {
            await new Promise(resolve => setTimeout(resolve, 8));
            continue;
          }
          const length = Math.min(ROM_CHUNK_BYTES, active.size - active.offset);
          const frame = new Uint8Array(8 + length);
          const view = new DataView(frame.buffer);
          view.setUint32(0, active.id, true); view.setUint32(4, active.offset, true);
          frame.set(active.bytes.subarray(active.offset, active.offset + length), 8);
          this.sendBinary(frame.buffer); active.offset += length; this.update();
          // Yield between batches so controls/cancellation remain responsive.
          if (active.offset % (ROM_CHUNK_BYTES * 16) === 0) await new Promise(resolve => setTimeout(resolve, 0));
        }
        if (this.generation !== generation || this.closed) return;
        active.phase = 'verifying'; this.update();
        this.send({ type: 'rom-end', id: active.id });
      } catch (error) { this.cancel(error.message || 'ROM transfer failed'); }
    } else if (packet.type === 'rom-end') {
      if (active.direction !== 'receive' || active.phase !== 'receiving' || active.offset !== active.size) {
        throw new Error('ROM transfer ended before all bytes arrived');
      }
      active.phase = 'verifying'; this.update();
      const generation = this.generation;
      const hash = await sha256(active.bytes);
      if (this.generation !== generation || this.closed) return;
      try { validateLinkRom(active.bytes, active.name); } catch (error) { this.cancel(error.message); return; }
      if (hash !== active.hash) { this.cancel('ROM checksum did not match; send the file again'); return; }
      active.phase = 'verified'; this.update();
      this.send({ type: 'rom-verified', id: active.id });
      this.onStatus(`${active.name} received and verified. Choose Load received ROM.`);
    } else if (packet.type === 'rom-verified') {
      if (active.direction !== 'send' || active.phase !== 'verifying') throw new Error('Unexpected ROM verification');
      this.finish(); this.onStatus('ROM received and verified by the other player');
    } else throw new Error('Unknown ROM transfer packet');
  }
  accept() {
    const active = this.active;
    if (!active || active.direction !== 'receive' || active.phase !== 'offered') throw new Error('No ROM offer is waiting');
    // Allocation happens only after this explicit receiver action.
    active.bytes = new Uint8Array(active.size); active.phase = 'receiving';
    this.update(); this.send({ type: 'rom-accept', id: active.id });
  }
  reject() {
    if (!this.active || this.active.direction !== 'receive' || this.active.phase !== 'offered') throw new Error('No ROM offer is waiting');
    this.send({ type: 'rom-reject', id: this.active.id }); this.finish(new Error('ROM offer rejected'));
  }
  receiveBinary(buffer) {
    if (!(buffer instanceof ArrayBuffer) || buffer.byteLength < 9 || buffer.byteLength > ROM_CHUNK_BYTES + 8) {
      throw new Error('Invalid ROM chunk');
    }
    const view = new DataView(buffer);
    const id = view.getUint32(0, true); const offset = view.getUint32(4, true);
    const active = this.active;
    if (!active || id !== active.id) return; // Buffered chunk from a cancelled transfer.
    const length = buffer.byteLength - 8;
    if (active.direction !== 'receive' || active.phase !== 'receiving' || offset !== active.offset
        || offset + length > active.size || length !== Math.min(ROM_CHUNK_BYTES, active.size - offset)) {
      throw new Error('ROM chunks arrived out of sequence');
    }
    active.bytes.set(new Uint8Array(buffer, 8), offset); active.offset += length; this.update();
  }
  async load() {
    const active = this.active;
    if (!active || active.direction !== 'receive' || active.phase !== 'verified') throw new Error('Receive and verify a ROM first');
    active.phase = 'loading'; this.update();
    try { await this.loadRom(active.bytes, active.name); }
    catch (error) { if (this.active === active) { active.phase = 'verified'; this.update(); } throw error; }
    if (this.active === active) { this.finish(); this.onStatus('ROM loaded. Choose Start cable on both devices.'); }
  }
  finish(error) {
    const active = this.active; this.active = null; this.generation += 1; this.update();
    if (active) {
      this.retired.add(active.id);
      if (this.retired.size > 16) this.retired.delete(this.retired.values().next().value);
    }
    if (error) { active?.reject?.(error); this.onStatus(error.message); }
    else active?.resolve?.();
  }
  cancel(reason = 'ROM transfer cancelled') {
    if (!this.active) return;
    this.send({ type: 'rom-cancel', id: this.active.id });
    this.finish(new Error(reason));
  }
  close() {
    this.closed = true;
    if (this.active) this.finish(new Error('Connection closed during ROM transfer'));
  }
}

export class RtcCable {
  constructor({ adapter, onStatus = () => {}, onRom = () => {}, loadRom,
    PeerConnection = globalThis.RTCPeerConnection, iceServers = DEFAULT_ICE, disconnectGraceMs = 15000 }) {
    this.adapter = adapter; this.onStatus = onStatus; this.PeerConnection = PeerConnection;
    this.iceServers = iceServers; this.pc = null; this.coordinator = null; this.timer = null;
    this.onRom = onRom; this.loadRom = loadRom;
    this.disconnectGraceMs = disconnectGraceMs;
    this.connected = false; this.localReady = false; this.peerReady = false; this.epoch = 0;
  }
  setup(player) {
    this.close();
    if (!this.PeerConnection) throw new Error('This browser does not support WebRTC data channels');
    const pc = new this.PeerConnection({ iceServers: this.iceServers });
    this.pc = pc; this.player = player;
    this.discoveryPc = null; this.pendingAnswerPc = null;
    this.pairingId = null;
    this.discoveryErrors = [];
    this.lastFailureDetails = '';
    pc.onicecandidateerror = event => {
      if (this.pc === pc && this.discoveryErrors.length < 8 && Number.isInteger(event.errorCode)) {
        this.discoveryErrors.push(event.errorCode);
      }
    };
    this.connected = false; this.localReady = false; this.peerReady = false; this.epoch = 0;
    pc.onconnectionstatechange = () => {
      if (this.pc !== pc) return;
      // No remote transport exists during initialization or while the offer
      // awaits its reply. Failure handling starts after the real answer.
      if (this.discoveryPc === pc || this.pendingAnswerPc === pc) return;
      if (pc.connectionState === 'failed') {
        this.connectionFailed(pc);
      } else if (pc.connectionState === 'disconnected') {
        // ICE disconnected is recoverable; closing here prevents recovery.
        if (!this.disconnectTimer) {
          this.onStatus('Connection interrupted; waiting for WebRTC to recover…');
          this.disconnectTimer = setTimeout(() => {
            this.disconnectTimer = null;
            if (this.pc === pc && pc.connectionState === 'disconnected') {
              this.close(`Connection did not recover. ${this.connectionDetails(pc)}`, true);
            }
          }, this.disconnectGraceMs);
        }
      } else {
        const recovering = !!this.disconnectTimer;
        clearTimeout(this.disconnectTimer); this.disconnectTimer = null;
        if (recovering && pc.connectionState === 'connected') this.onStatus('Device connection recovered');
      }
    };
    pc.ondatachannel = event => this.channel(event.channel, pc);
    this.connectionTimeout = setTimeout(() => {
      if (this.pc === pc && !this.connected) this.close('Pairing timed out. Check both codes, or configure a TURN relay.', true);
    }, 90000);
    return pc;
  }
  async connectionFailed(pc) {
    if (this.discoveryPc === pc || this.pendingAnswerPc === pc) return;
    const details = this.connectionDetails(pc);
    let stats = '';
    try {
      if (pc.getStats) {
        const report = await Promise.race([pc.getStats(), new Promise(resolve => setTimeout(() => resolve(null), 1000))]);
        if (report) {
          const pairs = [], transports = [];
          report.forEach(item => {
            const count = value => Number.isFinite(value) ? value : 'unavailable';
            if (item.type === 'candidate-pair') pairs.push(`${item.state}: requests ${count(item.requestsSent)}, responses ${count(item.responsesReceived)}`);
            if (item.type === 'transport') transports.push(`DTLS ${item.dtlsState || 'unknown'}`);
          });
          stats = `\nCandidate checks: ${pairs.slice(0, 12).join('; ') || 'none'}. ${transports.join('; ')}`;
        }
      }
    } catch { /* Basic state/candidate diagnostics remain available. */ }
    if (this.pc !== pc || pc.connectionState !== 'failed') return;
    this.lastFailureDetails = `ICE ${pc.iceConnectionState || 'unknown'}; gathering ${pc.iceGatheringState || 'unknown'}.\n${details}${stats}`;
    this.close('WebRTC connection failed. See Connection details below.', true);
  }
  connectionDetails(pc = this.pc) {
    const counts = description => {
      const sdp = description?.sdp || '';
      const types = [...sdp.matchAll(/^a=candidate:.*?\btyp (host|srflx|prflx|relay)\b/gm)].map(match => match[1]);
      return `local ${types.filter(type => type === 'host').length}, public ${types.filter(type => type === 'srflx' || type === 'prflx').length}, relay ${types.filter(type => type === 'relay').length}`;
    };
    // Counts/error codes only: never expose IPs, pairing SDP or relay credentials.
    return `Addresses — this device: ${counts(pc?.localDescription)}; peer: ${counts(pc?.remoteDescription)}.${this.discoveryErrors?.length ? ` Discovery errors: ${[...new Set(this.discoveryErrors)].join(', ')}.` : ''}`;
  }
  channel(channel, pc) {
    if (this.pc !== pc || channel.label !== 'rustboy-gba-cable-v1' || channel.ordered === false
        || channel.maxRetransmits != null || channel.maxPacketLifeTime != null) {
      channel.close(); return;
    }
    if (this.dataChannel) { channel.close(); return; }
    this.dataChannel = channel;
    channel.binaryType = 'arraybuffer';
    const open = () => {
      if (this.pc !== pc || this.rom) return;
      clearTimeout(this.connectionTimeout);
      try {
        this.rom = new RomTransfer({ send: packet => this.send(packet),
          sendBinary: buffer => this.sendBinary(buffer), bufferedAmount: () => channel.bufferedAmount,
          onStatus: this.onStatus, onUpdate: this.onRom, loadRom: this.loadRom });
        this.lastReceived = performance.now(); this.lastHeartbeat = performance.now();
        this.send({ type: 'transport-hello', player: this.player });
        this.timer = setInterval(() => {
          this.coordinator?.poll();
          if (this.pc !== pc) return;
          if (performance.now() - this.lastReceived > 20000) {
            this.close('Connection lost: the other device stopped responding', true); return;
          }
          if (performance.now() - this.lastHeartbeat >= 1000) {
            this.lastHeartbeat = performance.now();
            try { this.send({ type: 'transport-ping' }); }
            catch { this.close('Connection lost', true); }
          }
        }, 8);
      } catch (error) { this.close(`Cannot start link: ${error.message || error}`, true); }
    };
    channel.onopen = open;
    channel.onmessage = event => {
      this.receiveTransport(event.data).catch(error => this.close(`Connection stopped: ${error.message || error}`, true));
    };
    channel.onerror = () => this.close('Data channel failed; both players must reconnect', true);
    channel.onclose = () => { if (this.pc === pc) this.close('Other player disconnected', true); };
    if (channel.readyState === 'open') open();
  }
  send(packet) {
    const channel = this.dataChannel;
    if (!channel || channel.readyState !== 'open' || channel.bufferedAmount > 1048576) throw new Error('Connection is unavailable');
    channel.send(JSON.stringify({ v: LINK_VERSION, ...packet }));
  }
  sendBinary(buffer) {
    if (this.dataChannel?.readyState !== 'open') throw new Error('Connection is unavailable');
    this.dataChannel.send(buffer);
  }
  async receiveTransport(raw) {
    if (!this.rom) throw new Error('Connection has not opened');
    this.lastReceived = performance.now();
    if (raw instanceof ArrayBuffer) {
      if (!this.connected) throw new Error('Connection handshake has not completed');
      if (isLocalLinkBinary(raw)) {
        if (!this.coordinator?.receiveBinary) throw new Error('No local cable session is waiting for state data');
        this.coordinator.receiveBinary(raw);
      } else this.rom.receiveBinary(raw);
      return;
    }
    if (typeof raw !== 'string' || raw.length > MAX_PACKET) throw new Error('Invalid transport packet');
    const packet = JSON.parse(raw);
    if (packet?.v !== LINK_VERSION || typeof packet.type !== 'string') throw new Error('Incompatible connection protocol');
    if (packet.type === 'transport-hello') {
      if (this.connected || packet.player !== 1 - this.player) throw new Error('Invalid connection handshake');
      this.connected = true;
      this.onStatus(`Device connected as player ${this.player + 1}. Send a ROM or choose Start cable on both devices.`);
      return;
    }
    if (!this.connected) throw new Error('Connection handshake has not completed');
    if (packet.type.startsWith('rom-')) {
      if (packet.type === 'rom-offer') this.stopLink('ROM offered; start cable again after loading', false);
      await this.rom.receive(packet);
    } else switch (packet.type) {
      case 'transport-ping': break;
      case 'link-ready':
        if (this.peerReady || this.coordinator) throw new Error('Unexpected cable readiness');
        this.peerReady = true; this.maybeStart(); break;
      case 'link-start':
        if (this.player !== 1 || !this.localReady || !this.peerReady || !integer(packet.epoch)
            || packet.epoch !== this.epoch + 1 || this.coordinator) throw new Error('Invalid cable start');
        this.epoch = packet.epoch; this.beginCable(); break;
      case 'link-stop':
        if (!integer(packet.epoch) || packet.epoch > this.epoch) throw new Error('Invalid cable stop');
        if (packet.epoch === this.epoch) this.stopLink('Other player stopped the cable', false);
        break;
      case 'cable':
        if (!integer(packet.epoch) || packet.epoch > this.epoch) throw new Error('Invalid cable session');
        if (packet.epoch === this.epoch && this.coordinator) this.coordinator.receive(JSON.stringify(packet.packet));
        break;
      default: throw new Error('Unknown connection packet');
    }
  }
  startLink({ restart = true } = {}) {
    if (!this.connected) throw new Error('Pair both devices first');
    if (this.rom?.active) throw new Error('Finish or cancel the ROM transfer before starting the cable');
    if (this.coordinator || this.localReady) throw new Error('Cable already started or waiting for the other player');
    this.adapter.status(); // Validate a GBA is loaded, without attaching it yet.
    if (restart) this.adapter.reset?.();
    this.adapter.pause?.();
    // Attach and freeze at this clock immediately. Closing Settings while the
    // other player is still choosing Start cannot run ahead or initialize with
    // the wrong parent/child cable role.
    this.adapter.attach(this.player);
    this.adapter.limit(this.adapter.status().clock);
    this.localReady = true;
    this.send({ type: 'link-ready' });
    this.onStatus('Ready; choose Start cable on the other device, then close Settings on both to play');
    this.maybeStart();
  }
  maybeStart() {
    if (this.player === 0 && this.localReady && this.peerReady && !this.coordinator) {
      this.epoch += 1; this.send({ type: 'link-start', epoch: this.epoch }); this.beginCable();
    }
  }
  beginCable() {
    const Coordinator = this.adapter.localStart ? LocalLinkCoordinator : CableCoordinator;
    this.coordinator = new Coordinator({ adapter: this.adapter, player: this.player,
      sendBinary: buffer => this.sendBinary(buffer), bufferedAmount: () => this.dataChannel?.bufferedAmount || 0,
      send: raw => this.send({ type: 'cable', epoch: this.epoch, packet: JSON.parse(raw) }),
      onStatus: message => {
        this.onStatus(message);
        if (this.coordinator?.closed) queueMicrotask(() => {
          if (this.coordinator?.closed) this.stopLink(message);
        });
      } });
    this.coordinator.start();
  }
  stopLink(reason = 'Cable stopped; devices remain connected', notify = true) {
    const coordinator = this.coordinator; this.coordinator = null;
    const wasReady = this.localReady || this.peerReady;
    this.localReady = false; this.peerReady = false;
    coordinator?.close(reason);
    if (!coordinator && wasReady) this.adapter.detach();
    if (notify && this.connected && (coordinator || wasReady)) this.send({ type: 'link-stop', epoch: this.epoch });
    if (coordinator || wasReady) this.onStatus(reason);
  }
  async sendRom(bytes, name, { loadLocal = false } = {}) {
    if (!this.connected) throw new Error('Pair both devices before sending a ROM');
    if (this.rom?.active) throw new Error('Finish or cancel the current ROM transfer');
    validateLinkRom(bytes, name);
    this.stopLink('ROM transfer selected; restart the cable after loading');
    if (loadLocal) {
      if (!this.loadRom) throw new Error('ROM loading is unavailable');
      await this.loadRom(bytes, name);
    }
    return this.rom.offer(bytes, name);
  }
  async createOffer() {
    const pc = this.setup(0);
    this.pairingId = globalThis.crypto.randomUUID();
    this.onStatus('Creating player 1 offer…');
    this.discoveryPc = pc; this.pendingAnswerPc = pc;
    try { await preparePeerDiscovery(pc, this.PeerConnection); }
    catch (error) {
      if (this.pc !== pc) throw new Error('Pairing was cancelled');
      this.close(); throw error;
    }
    finally { if (this.discoveryPc === pc) this.discoveryPc = null; }
    if (this.pc !== pc) throw new Error('Pairing was cancelled');
    this.channel(pc.createDataChannel('rustboy-gba-cable-v1', { ordered: true }), pc);
    await pc.setLocalDescription(await pc.createOffer());
    await gatherIce(pc);
    if (this.pc !== pc) throw new Error('Pairing was cancelled');
    this.onStatus('Send the offer code to player 2, then paste their answer');
    return encodePairing({ ...(pc.localDescription.toJSON?.() || pc.localDescription), session: this.pairingId });
  }
  async acceptOffer(code) {
    const description = decodePairing(code, 'offer');
    const pc = this.setup(1);
    this.pairingId = description.session || null;
    this.onStatus('Creating player 2 answer…');
    await pc.setRemoteDescription(description);
    await pc.setLocalDescription(await pc.createAnswer());
    await gatherIce(pc);
    if (this.pc !== pc) throw new Error('Pairing was cancelled');
    this.onStatus('Send the answer code to player 1 to connect devices');
    return encodePairing({ ...(pc.localDescription.toJSON?.() || pc.localDescription), session: this.pairingId });
  }
  async acceptAnswer(code) {
    const description = decodePairing(code, 'answer');
    if (!this.pc || this.player !== 0 || this.pc.signalingState !== 'have-local-offer') {
      throw new Error('Create a player 1 offer before applying an answer');
    }
    if (this.pairingId && description.session !== this.pairingId) {
      throw new Error('This reply belongs to a different or older link code. Send your current code again and connect its new reply. Both pages must use the updated build.');
    }
    const pc = this.pc;
    await pc.setRemoteDescription(description);
    if (this.pc !== pc) throw new Error('Pairing was cancelled');
    this.pendingAnswerPc = null;
    // Check a retained failed state after the answer, even if the browser
    // does not emit another connectionstatechange event.
    clearTimeout(this.initialFailureTimer);
    this.initialFailureTimer = setTimeout(() => {
      if (this.pc === pc && pc.connectionState === 'failed') this.connectionFailed(pc);
    }, 1000);
    this.onStatus('Connecting devices…');
  }
  close(reason = 'Link disconnected', pause = false) {
    clearInterval(this.timer); clearTimeout(this.connectionTimeout);
    clearTimeout(this.initialFailureTimer);
    this.discoveryPc = null; this.pendingAnswerPc = null;
    clearTimeout(this.disconnectTimer); this.disconnectTimer = null;
    this.timer = null;
    const pc = this.pc; const channel = this.dataChannel;
    this.pc = null; this.dataChannel = null;
    const coordinator = this.coordinator;
    coordinator?.close(reason, pause); this.coordinator = null;
    if (!coordinator && this.localReady) { this.adapter.detach(); if (pause) this.adapter.pause?.(); }
    this.rom?.close(); this.rom = null;
    this.connected = false; this.localReady = false; this.peerReady = false;
    channel?.close(); pc?.close();
    this.onStatus(reason);
  }
}

export function installGbaLinkPanel(wasm, root = document, { loadRom = async bytes => wasm.load_gba_data(bytes, new Uint8Array()), loadDownload = async () => wasm.load_gba_download_client(), waitForRom = async () => {} } = {}) {
  const panel = root.querySelector('#gba-link-settings');
  if (!panel) return null;
  const status = root.querySelector('#gba-link-status');
  const input = root.querySelector('#gba-link-remote-code');
  const output = root.querySelector('#gba-link-local-code');
  const relay = root.querySelector('#gba-link-relay');
  const fileInput = root.querySelector('#gba-link-rom-file');
  const romStatus = root.querySelector('#gba-link-rom-status');
  let cable = null; let room = null; let autoLoad = false;
  const report = message => {
    status.textContent = message;
    const details = root.querySelector('#gba-link-diagnostics');
    if (details) {
      details.textContent = cable?.lastFailureDetails || cable?.connectionDetails() || 'No connection attempt yet';
      if (cable?.lastFailureDetails) details.closest('details').open = true;
    }
  };
  const updateRom = transfer => {
    root.querySelector('#gba-link-rom-play').disabled = !(transfer?.direction === 'receive' && transfer.phase === 'offered');
    root.querySelector('#gba-link-rom-current').disabled = !!transfer;
    if (!transfer) autoLoad = false;
    if (autoLoad && transfer?.phase === 'verified') {
      autoLoad = false;
      queueMicrotask(() => cable.rom.load().catch(error => report(error.message || String(error))));
    }
    root.querySelector('#gba-link-rom-receive').disabled = !(transfer?.direction === 'receive' && transfer.phase === 'offered');
    root.querySelector('#gba-link-rom-reject').disabled = !(transfer?.direction === 'receive' && transfer.phase === 'offered');
    root.querySelector('#gba-link-rom-load').disabled = !(transfer?.direction === 'receive' && transfer.phase === 'verified');
    root.querySelector('#gba-link-rom-cancel').disabled = !transfer;
    root.querySelector('#gba-link-rom-send').disabled = !!transfer;
    const text = transfer
      ? `${transfer.name} · ${transfer.phase} · ${Math.floor(100 * transfer.received / transfer.size)}% (${Math.ceil(transfer.size / 1024)} KiB)` : 'No ROM transfer active';
    if (romStatus.textContent !== text) romStatus.textContent = text;
  };
  const create = () => {
    room?.close(); room = null; cable?.close();
    const adapter = wasmLinkAdapter(wasm);
    const iceServers = relayFormServers(root.querySelector('#gba-link-relay-url')?.value,
      root.querySelector('#gba-link-relay-user')?.value, root.querySelector('#gba-link-relay-password')?.value, relay.value);
    return new RtcCable({ adapter, onStatus: report, onRom: updateRom, loadRom, iceServers });
  };
  const run = action => async () => {
    const buttons = panel.querySelectorAll('button[data-link-pair]');
    for (const button of buttons) button.disabled = true;
    try { await waitForRom(); await action(); }
    catch (error) { report(error.message || String(error)); }
    finally { for (const button of buttons) button.disabled = false; }
  };
  const selfTest = root.querySelector('#gba-link-self-test');
  selfTest?.addEventListener('click', async () => {
    selfTest.disabled = true;
    const result = root.querySelector('#gba-link-self-test-result');
    result.textContent = 'Testing native WebRTC in this browser…';
    try { result.textContent = await testBrowserWebRtc(); }
    catch (error) { result.textContent = error.message || String(error); }
    finally { selfTest.disabled = false; }
  });
  const joinRoom = name => {
    cable = create();
    const server = roomServerUrl(root.querySelector('#gba-link-server').value);
    room = new RoomPairing({ cable, server, onStatus: report,
      onInvite: id => { root.querySelector('#gba-link-invite').value = inviteUrl(id, server); } });
    room.connect(name);
  };
  root.querySelector('#gba-link-room-create').addEventListener('click', run(async () => joinRoom(null)));
  root.querySelector('#gba-link-room-copy').addEventListener('click', async () => {
    const invite = root.querySelector('#gba-link-invite');
    if (!invite.value) { report('Create a room first'); return; }
    try { await navigator.clipboard.writeText(invite.value); report('Invite link copied'); }
    catch { invite.focus(); invite.select(); report('Select and copy the invite link'); }
  });
  root.querySelector('#gba-link-rom-current').addEventListener('click', run(async () => {
    if (!cable?.connected) throw new Error('Connect your friend before sending the game');
    const bytes = wasm.export_gba_rom();
    const title = String(wasm.get_rom_title?.() || 'game').replace(/[\\/\x00-\x1f\x7f]/g, '_').slice(0, 150);
    await cable.sendRom(bytes, `${title || 'game'}.gba`);
  }));
  root.querySelector('#gba-link-rom-play').addEventListener('click', run(async () => {
    if (!cable?.rom) throw new Error('No game offer is waiting');
    autoLoad = true; cable.rom.accept();
  }));
  root.querySelector('#gba-link-create').addEventListener('click', run(async () => {
    cable = create(); output.value = ''; output.value = await cable.createOffer();
  }));
  root.querySelector('#gba-link-connect').addEventListener('click', run(async () => {
    if (cable?.player === 0 && cable.pc?.signalingState === 'have-local-offer') {
      await cable.acceptAnswer(input.value);
    } else {
      // Validate before replacing an existing connection.
      decodePairing(input.value, 'offer');
      cable = create(); output.value = ''; output.value = await cable.acceptOffer(input.value);
    }
  }));
  root.querySelector('#gba-link-disconnect').addEventListener('click', () => { room?.close(); room = null; cable?.close(); cable = null; });
  root.querySelector('#gba-link-download').addEventListener('click', run(async () => {
    if (!cable?.connected || cable.player !== 1) throw new Error('The invited player can join Single Pak; player 1 keeps the host cartridge');
    cable.stopLink();
    await loadDownload();
    cable.startLink();
  }));
  root.querySelector('#gba-link-start').addEventListener('click', run(async () => {
    if (!cable) throw new Error('Pair both devices first');
    cable.startLink();
  }));
  root.querySelector('#gba-link-rom-send').addEventListener('click', () => {
    if (!cable?.connected) { report('Pair both devices before sending a ROM'); return; }
    fileInput.click();
  });
  fileInput.addEventListener('change', run(async () => {
    const file = fileInput.files?.[0]; fileInput.value = '';
    if (!file) return;
    if (file.size < 192 || file.size > MAX_ROM_BYTES) throw new Error('GBA files must contain 192 bytes to 32 MiB');
    const bytes = new Uint8Array(await file.arrayBuffer());
    validateLinkRom(bytes, file.name);
    await cable.sendRom(bytes, file.name, { loadLocal: true });
  }));
  root.querySelector('#gba-link-rom-receive').addEventListener('click', run(async () => {
    if (!cable?.rom) throw new Error('Pair both devices first'); cable.rom.accept();
  }));
  root.querySelector('#gba-link-rom-reject').addEventListener('click', run(async () => {
    if (!cable?.rom) throw new Error('No ROM offer is waiting'); cable.rom.reject();
  }));
  root.querySelector('#gba-link-rom-load').addEventListener('click', run(async () => {
    if (!cable?.rom) throw new Error('Receive a ROM first'); await cable.rom.load();
  }));
  root.querySelector('#gba-link-rom-cancel').addEventListener('click', () => cable?.rom?.cancel());
  root.querySelector('#gba-link-copy').addEventListener('click', async () => {
    if (!output.value) { report('Create an offer or answer first'); return; }
    try { await navigator.clipboard.writeText(output.value); report('Pairing code copied'); }
    catch { output.focus(); output.select(); report('Select and copy the pairing code to send it'); }
  });
  updateRom(null);
  const page = new URL(globalThis.location.href);
  const invitation = /^#gba-link=([A-Za-z0-9_-]{12})$/.exec(page.hash);
  if (invitation) {
    root.querySelector('#gba-link-server').value = page.searchParams.get('link-server') || '';
    root.querySelector('[data-open-link]')?.click();
    run(async () => joinRoom(invitation[1]))();
  }
  return { disconnect: reason => { room?.close(); room = null; cable?.close(reason); cable = null; }, stopLink: reason => cable?.stopLink(reason), get cable() { return cable; } };
}

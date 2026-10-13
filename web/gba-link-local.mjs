// Each endpoint runs the same two-GBA session locally. Only initial machine
// states/cartridges and delayed frame inputs cross the network; serial transfers
// stay inside the native paired scheduler.
export const LOCAL_LINK_VERSION = 2;
export const LOCAL_LINK_INPUT_DELAY = 6;
export const LOCAL_LINK_QUEUE_LIMIT = 32;
export const LOCAL_LINK_CHUNK_BYTES = 16384;
export const LOCAL_LINK_HEADER_BYTES = 20;
export const LOCAL_LINK_MAX_STATE = 8 * 1024 * 1024;
export const LOCAL_LINK_MAX_ROM = 32 * 1024 * 1024;
const MAGIC = new Uint8Array([0x52, 0x42, 0x4c, 0x4f, 0x43, 0x41, 0x4c, 0x32]); // RBLOCAL2
const MAX_PACKET = 4096;
const BUFFER_LIMIT = 262144;
const MAX_FRAME = 0xffffffff;
const TIMEOUT = 20000;
const integer = (value, maximum = MAX_FRAME) => Number.isSafeInteger(value) && value >= 0 && value <= maximum;
const byteView = value => ArrayBuffer.isView(value) && value.BYTES_PER_ELEMENT === 1;
const string = (value, maximum, empty = false) => typeof value === 'string' && value.length <= maximum
  && (empty || value.length > 0) && !/[\x00-\x1f\x7f]/.test(value);
const arrayBuffer = value => Object.prototype.toString.call(value) === '[object ArrayBuffer]';

export function isLocalLinkBinary(buffer) {
  if (!arrayBuffer(buffer) || buffer.byteLength < MAGIC.length) return false;
  const bytes = new Uint8Array(buffer, 0, MAGIC.length);
  return MAGIC.every((byte, index) => bytes[index] === byte);
}

function validateIdentity(identity, state, rom) {
  if (!identity || typeof identity.download !== 'boolean' || !string(identity.romKey, 256, identity.download)
      || !string(identity.stateId, 512) || !byteView(state) || state.length === 0 || state.length > LOCAL_LINK_MAX_STATE
      || !byteView(rom) || identity.download && rom.length !== 0
      || !identity.download && (rom.length < 192 || rom.length > LOCAL_LINK_MAX_ROM || rom[0xb2] !== 0x96)) {
    throw new Error('Invalid local GBA session state or cartridge');
  }
}

export class LocalLinkCoordinator {
  constructor({ adapter, player, send, sendBinary, bufferedAmount = () => 0,
    onStatus = () => {}, now = () => performance.now() }) {
    if (player !== 0 && player !== 1) throw new Error('Cable supports two players');
    this.adapter = adapter; this.player = player; this.sendPacket = send;
    this.sendBytes = sendBinary; this.bufferedAmount = bufferedAmount;
    this.onStatus = onStatus; this.now = now;
    this.started = false; this.closed = false; this.hello = false; this.transfers = 0;
    this.peer = null; this.installed = false; this.peerInstalled = false;
    this.ownInputs = new Map(); this.peerInputs = new Map();
    this.nextSend = LOCAL_LINK_INPUT_DELAY; this.nextPeer = LOCAL_LINK_INPUT_DELAY; this.nextQueue = 0;
    this.lastFrame = 0; this.lastReceived = this.now(); this.lastProgress = this.now(); this.lastHeartbeat = this.now();
    this.outgoing = []; this.outgoingIndex = 0; this.outgoingOffset = 0;
  }
  send(packet) {
    if (!this.closed) this.sendPacket(JSON.stringify({ v: LOCAL_LINK_VERSION, ...packet }));
  }
  start() {
    if (this.started || this.closed) return;
    try {
      const identity = this.adapter.identity();
      const state = this.adapter.snapshot();
      const rom = this.adapter.rom();
      validateIdentity(identity, state, rom);
      const title = String(this.adapter.title?.() || 'GBA game').replace(/[\x00-\x1f\x7f]/g, ' ').slice(0, 128);
      this.meta = { player: this.player, download: identity.download, romKey: identity.romKey,
        stateId: identity.stateId, stateSize: state.length, romSize: rom.length, title };
      this.ownState = state; this.ownRom = rom;
      this.started = true;
      this.lastReceived = this.now(); this.lastProgress = this.now(); this.lastHeartbeat = this.now();
      this.send({ type: 'local-hello', ...this.meta });
      this.onStatus('Synchronizing the two GBA sessions…');
      this.poll();
    } catch (error) {
      this.close(`Link stopped: ${error.message || error}`, true);
      throw error;
    }
  }
  receive(raw) {
    if (this.closed) return;
    try {
      if (typeof raw !== 'string' || raw.length > MAX_PACKET) throw new Error('Invalid local-link packet');
      const packet = JSON.parse(raw);
      if (packet?.v !== LOCAL_LINK_VERSION || typeof packet.type !== 'string') throw new Error('Incompatible local-link protocol');
      if (packet.type === 'local-bye') { this.close('The other player disconnected', true); return; }
      if (packet.type === 'local-hello') this.acceptHello(packet);
      else {
        if (!this.peer) throw new Error('Local-link handshake has not completed');
        switch (packet.type) {
          case 'local-installed':
            if (this.peerInstalled || packet.player !== 1 - this.player) throw new Error('Invalid local-link installation acknowledgement');
            this.peerInstalled = true; this.lastProgress = this.now(); this.ready(); break;
          case 'local-input': this.acceptInput(packet); break;
          case 'local-ping': break;
          default: throw new Error('Unknown local-link packet');
        }
      }
      this.lastReceived = this.now();
      this.poll();
    } catch (error) { this.close(`Link stopped: ${error.message || error}`, true); }
  }
  acceptHello(packet) {
    if (!this.started) this.start();
    if (this.peer || packet.player !== 1 - this.player || typeof packet.download !== 'boolean'
        || !string(packet.romKey, 256, packet.download) || !string(packet.stateId, 512)
        || !string(packet.title, 128, true) || !integer(packet.stateSize, LOCAL_LINK_MAX_STATE) || packet.stateSize === 0
        || !integer(packet.romSize, LOCAL_LINK_MAX_ROM) || packet.download && packet.romSize !== 0
        || !packet.download && packet.romSize < 192) throw new Error('Invalid peer GBA session metadata');
    this.peer = { player: packet.player, download: packet.download, romKey: packet.romKey,
      stateId: packet.stateId, stateSize: packet.stateSize, romSize: packet.romSize, title: packet.title };
    this.needPeerRom = !packet.download && (this.meta.download || packet.romKey !== this.meta.romKey);
    this.peerState = new Uint8Array(packet.stateSize); this.peerStateOffset = 0;
    this.peerRom = new Uint8Array(this.needPeerRom ? packet.romSize : 0); this.peerRomOffset = 0;
    this.outgoing = [{ kind: 1, bytes: this.ownState }];
    if (!this.meta.download && (packet.download || packet.romKey !== this.meta.romKey)) {
      this.outgoing.push({ kind: 2, bytes: this.ownRom });
    }
    this.lastProgress = this.now();
  }
  pump() {
    // A bounded batch keeps frame/input processing and Cancel responsive during
    // large initial transfers. Data-channel backpressure bounds queued bytes.
    let budget = 16;
    while (this.outgoingIndex < this.outgoing.length && budget-- > 0) {
      const buffered = this.bufferedAmount();
      if (!integer(buffered, Number.MAX_SAFE_INTEGER)) throw new Error('Invalid transport backpressure');
      if (buffered > BUFFER_LIMIT) break;
      const item = this.outgoing[this.outgoingIndex];
      const length = Math.min(LOCAL_LINK_CHUNK_BYTES, item.bytes.length - this.outgoingOffset);
      const bytes = new Uint8Array(LOCAL_LINK_HEADER_BYTES + length); bytes.set(MAGIC);
      const view = new DataView(bytes.buffer);
      bytes[8] = item.kind; bytes[9] = LOCAL_LINK_VERSION;
      view.setUint32(12, this.outgoingOffset, true); view.setUint32(16, item.bytes.length, true);
      bytes.set(item.bytes.subarray(this.outgoingOffset, this.outgoingOffset + length), LOCAL_LINK_HEADER_BYTES);
      this.sendBytes(bytes.buffer); this.outgoingOffset += length; this.lastProgress = this.now();
      if (this.outgoingOffset === item.bytes.length) {
        this.outgoingIndex += 1; this.outgoingOffset = 0;
      }
    }
    if (this.outgoing.length && this.outgoingIndex === this.outgoing.length) {
      this.outgoing = []; this.outgoingIndex = 0;
      this.ownState = null; this.ownRom = null;
    }
  }
  receiveBinary(buffer) {
    if (this.closed) return;
    try {
      if (!this.peer || this.installed || !isLocalLinkBinary(buffer) || buffer.byteLength <= LOCAL_LINK_HEADER_BYTES
          || buffer.byteLength > LOCAL_LINK_HEADER_BYTES + LOCAL_LINK_CHUNK_BYTES) throw new Error('Invalid initial-session chunk');
      const bytes = new Uint8Array(buffer); const view = new DataView(buffer);
      if (bytes[9] !== LOCAL_LINK_VERSION || bytes[10] !== 0 || bytes[11] !== 0 || ![1, 2].includes(bytes[8])) {
        throw new Error('Invalid initial-session chunk header');
      }
      const kind = bytes[8]; const offset = view.getUint32(12, true); const total = view.getUint32(16, true);
      const target = kind === 1 ? this.peerState : this.peerRom;
      const expected = kind === 1 ? this.peerStateOffset : this.peerRomOffset;
      const length = buffer.byteLength - LOCAL_LINK_HEADER_BYTES;
      if (kind === 2 && (!this.needPeerRom || this.peerStateOffset !== this.peer.stateSize)
          || total !== target.length || offset !== expected || length !== Math.min(LOCAL_LINK_CHUNK_BYTES, total - offset)
          || offset + length > total) throw new Error('Initial-session chunks arrived out of sequence');
      target.set(bytes.subarray(LOCAL_LINK_HEADER_BYTES), offset);
      if (kind === 1) this.peerStateOffset += length; else this.peerRomOffset += length;
      this.lastReceived = this.now(); this.lastProgress = this.now();
      this.install(); this.poll();
    } catch (error) { this.close(`Link stopped: ${error.message || error}`, true); }
  }
  install() {
    if (this.installed || this.peerStateOffset !== this.peer.stateSize
        || this.needPeerRom && this.peerRomOffset !== this.peer.romSize) return;
    if (this.needPeerRom && this.peerRom[0xb2] !== 0x96) throw new Error('Peer cartridge has an invalid GBA header');
    // Native import validates the complete snapshot checksum, cartridge and
    // firmware identity before replacing any running hardware.
    this.adapter.localStart(this.peerRom, this.peerState, this.peer.download, this.player);
    this.installed = true; this.lastProgress = this.now();
    this.peerRom = null; this.peerState = null;
    this.send({ type: 'local-installed', player: this.player }); this.ready();
  }
  ready() {
    if (this.hello || !this.installed || !this.peerInstalled) return;
    this.hello = true;
    for (let frame = 0; frame < LOCAL_LINK_INPUT_DELAY; frame++) {
      this.ownInputs.set(frame, 0); this.peerInputs.set(frame, 0);
    }
    this.onStatus(`Linked as player ${this.player + 1} · peer: ${this.peer.title} · 100 ms input delay`);
  }
  status() {
    const state = this.adapter.localStatus();
    if (!state || !state.connected || !integer(state.frame) || !integer(state.queued, LOCAL_LINK_QUEUE_LIMIT)
        || !integer(state.keys, 0x3ff) || !integer(state.transfers, Number.MAX_SAFE_INTEGER) || typeof state.paused !== 'boolean'
        || state.frame < this.lastFrame || state.frame > this.nextQueue || state.queued !== this.nextQueue - state.frame) {
      throw new Error('Local paired emulator changed or advanced without agreed inputs');
    }
    this.lastFrame = state.frame; this.transfers = state.transfers;
    return state;
  }
  acceptInput(packet) {
    if (!this.hello || !integer(packet.frame) || packet.frame !== this.nextPeer || !integer(packet.keys, 0x3ff)) {
      throw new Error('Invalid or out-of-order frame input');
    }
    const state = this.status();
    if (packet.frame < state.frame || packet.frame >= state.frame + LOCAL_LINK_QUEUE_LIMIT) {
      throw new Error('Frame input lies outside the bounded queue');
    }
    this.peerInputs.set(packet.frame, packet.keys); this.nextPeer += 1;
  }
  queue(state) {
    while (this.nextQueue < state.frame + LOCAL_LINK_QUEUE_LIMIT
        && this.ownInputs.has(this.nextQueue) && this.peerInputs.has(this.nextQueue)) {
      const own = this.ownInputs.get(this.nextQueue); const peer = this.peerInputs.get(this.nextQueue);
      this.adapter.localInput(this.nextQueue, this.player === 0 ? own : peer, this.player === 0 ? peer : own);
      this.ownInputs.delete(this.nextQueue); this.peerInputs.delete(this.nextQueue); this.nextQueue += 1;
    }
  }
  poll() {
    if (this.closed || !this.started) return;
    try {
      if (this.now() - this.lastReceived > TIMEOUT || !this.hello && this.now() - this.lastProgress > TIMEOUT) {
        throw new Error('Other player stopped responding');
      }
      this.pump();
      if (this.hello) {
        const state = this.status();
        const horizon = Math.min(MAX_FRAME, state.frame + LOCAL_LINK_INPUT_DELAY);
        while (this.nextSend <= horizon) {
          this.ownInputs.set(this.nextSend, state.keys);
          this.send({ type: 'local-input', frame: this.nextSend, keys: state.keys });
          this.nextSend += 1;
        }
        this.queue(state);
      }
      if (this.peer && this.now() - this.lastHeartbeat >= 1000) {
        this.lastHeartbeat = this.now(); this.send({ type: 'local-ping' });
      }
    } catch (error) { this.close(`Link stopped: ${error.message || error}`, true); }
  }
  close(reason = 'Link disconnected', pause = false) {
    if (this.closed) return;
    try { this.send({ type: 'local-bye' }); } catch { /* Transport is already unavailable. */ }
    this.closed = true;
    if (this.started) {
      try { this.adapter.detach(); } catch { /* The selected backend may have changed. */ }
      if (pause) this.adapter.pause?.();
    }
    this.ownState = null; this.ownRom = null; this.peerState = null; this.peerRom = null;
    this.outgoing = []; this.ownInputs.clear(); this.peerInputs.clear();
    this.onStatus(reason);
  }
}

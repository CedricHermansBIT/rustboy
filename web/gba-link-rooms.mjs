// Signaling only: cartridge bytes and cable traffic stay on WebRTC.
export function roomServerUrl(value = '', page = globalThis.location) {
  const url = value ? new URL(value) : new URL('/gba-link/rooms', page.href);
  if (url.protocol === 'http:') url.protocol = 'ws:';
  if (url.protocol === 'https:') url.protocol = 'wss:';
  if (!['ws:', 'wss:'].includes(url.protocol) || url.username || url.password || url.hash) {
    throw new Error('Room server must be a ws:// or wss:// address');
  }
  if (page.protocol === 'https:' && url.protocol !== 'wss:') throw new Error('HTTPS pages need a secure wss:// room server');
  return url.href;
}

export function inviteUrl(room, server, page = globalThis.location) {
  if (!/^[A-Za-z0-9_-]{12}$/.test(room)) throw new Error('Invalid room invite');
  const url = new URL(page.href);
  url.hash = `gba-link=${room}`;
  if (server !== roomServerUrl('', page)) url.searchParams.set('link-server', server);
  else url.searchParams.delete('link-server');
  return url.href;
}

export class RoomPairing {
  constructor({ cable, server, onInvite = () => {}, onStatus = () => {}, WebSocket = globalThis.WebSocket }) {
    this.cable = cable; this.server = server; this.onInvite = onInvite; this.onStatus = onStatus;
    this.WebSocket = WebSocket; this.socket = null; this.closed = false;
    this.pending = Promise.resolve();
  }
  connect(room = null) {
    if (room !== null && !/^[A-Za-z0-9_-]{12}$/.test(room)) throw new Error('Invalid room invite');
    const socket = new this.WebSocket(this.server);
    this.socket = socket;
    this.onStatus(room ? 'Joining room…' : 'Creating room…');
    const timer = setTimeout(() => this.fail('Room server did not respond. Check the room server address or use manual pairing.'), 10000);
    socket.onopen = () => {
      clearTimeout(timer);
      this.send(room ? { type: 'join', room } : { type: 'create' });
    };
    socket.onmessage = event => {
      this.pending = this.pending.then(() => this.receive(event.data)).catch(error => this.fail(error.message || String(error)));
    };
    socket.onerror = () => { clearTimeout(timer); this.fail('Room server unavailable. Run scripts/gba_link_server.py or use manual pairing under Connection options.'); };
    socket.onclose = () => { clearTimeout(timer); if (!this.closed && !this.cable.connected) this.fail('Room connection closed; create a new invite.'); };
  }
  send(packet) {
    if (this.socket?.readyState !== 1) throw new Error('Room connection is unavailable');
    this.socket.send(JSON.stringify(packet));
  }
  async receive(raw) {
    if (this.closed) return;
    if (typeof raw !== 'string' || raw.length > 200000) throw new Error('Invalid room response');
    const packet = JSON.parse(raw);
    switch (packet.type) {
      case 'joined':
        if (!/^[A-Za-z0-9_-]{12}$/.test(packet.room) || ![0, 1].includes(packet.player) || this.player !== undefined) throw new Error('Invalid room assignment');
        this.player = packet.player;
        this.onInvite(packet.room);
        this.onStatus(packet.player === 0 ? 'Room created. Send the invite link to your friend.' : 'Joined room; connecting devices…');
        break;
      case 'peer-joined':
        if (this.player !== 0 || this.offered) throw new Error('Invalid room pairing');
        this.offered = true;
        this.send({ type: 'signal', code: await this.cable.createOffer() });
        break;
      case 'signal':
        if (this.player === 1 && !this.answered) {
          this.answered = true;
          this.send({ type: 'signal', code: await this.cable.acceptOffer(packet.code) });
        } else if (this.player === 0 && this.offered && !this.answered) {
          this.answered = true;
          await this.cable.acceptAnswer(packet.code);
        } else throw new Error('Unexpected room pairing signal');
        break;
      case 'peer-left':
        if (!this.cable.connected) this.fail('Your friend left the room; create a new invite.');
        break;
      case 'error': throw new Error(String(packet.message || 'Room unavailable').slice(0, 200));
      default: throw new Error('Unknown room response');
    }
  }
  fail(message) {
    if (this.closed) return;
    this.close();
    if (!this.cable.connected) this.cable.close(message);
    this.onStatus(message);
  }
  close() { this.closed = true; this.socket?.close(); this.socket = null; }
}

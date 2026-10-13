import assert from 'node:assert/strict';
import { spawn } from 'node:child_process';
import { once } from 'node:events';
import http from 'node:http';
import { RoomPairing, roomServerUrl, inviteUrl } from '../web/gba-link-rooms.mjs';

const page = new URL('https://example.test/emulator/');
assert.equal(roomServerUrl('', page), 'wss://example.test/gba-link/rooms');
assert.throws(() => roomServerUrl('ws://example.test/rooms', page), /secure/);
assert.throws(() => inviteUrl('bad', 'wss://example.test/gba-link/rooms', page), /Invalid/);
assert.equal(inviteUrl('abcdefghijkl', 'wss://example.test/gba-link/rooms', page), 'https://example.test/emulator/#gba-link=abcdefghijkl');

const server = spawn('python3', ['scripts/gba_link_server.py', '--port', '0'], { stdio: ['ignore', 'pipe', 'pipe'] });
let first, second;
try {
  const port = await new Promise((resolve, reject) => {
    const timeout = setTimeout(() => reject(new Error('Room server did not start')), 10000);
    server.once('error', reject);
    server.stdout.on('data', bytes => {
      const match = String(bytes).match(/http:\/\/127\.0\.0\.1:(\d+)/);
      if (match) { clearTimeout(timeout); resolve(Number(match[1])); }
    });
  });
  const endpoint = `ws://127.0.0.1:${port}/gba-link/rooms`;
  let invite;
  const statuses = [];
  let offer = 0, answer = 0, accepted = 0;
  const host = { connected: false, async createOffer() { offer++; return 'original offer'; }, async acceptAnswer(code) { assert.equal(code, 'original answer'); accepted++; this.connected = true; }, close() {} };
  const guest = { connected: false, async acceptOffer(code) { assert.equal(code, 'original offer'); answer++; return 'original answer'; }, close() {} };
  first = new RoomPairing({ cable: host, server: endpoint, onInvite: name => { invite = name; }, onStatus: text => statuses.push(text) });
  first.connect();
  const until = async condition => {
    const deadline = Date.now() + 5000;
    while (!condition()) { if (Date.now() > deadline) throw new Error(`Room timed out: ${statuses.join('; ')}`); await new Promise(resolve => setTimeout(resolve, 10)); }
  };
  await until(() => invite);
  second = new RoomPairing({ cable: guest, server: endpoint, onStatus: text => statuses.push(text) });
  second.connect(invite);
  await until(() => host.connected);
  assert.deepEqual([offer, answer, accepted], [1, 1, 1], 'one invite relays the complete offer/answer automatically');
  const third = new WebSocket(endpoint);
  await once(third, 'open');
  third.send(JSON.stringify({ type: 'join', room: invite }));
  const [message] = await once(third, 'message');
  assert.equal(JSON.parse(message.data).type, 'error', 'a full room cannot admit a third device');
  third.close();
  for (const resource of ['/crates/gba/src/machine.rs', '/roms/private.gba', '/.git/config', '/web/../../Cargo.toml']) {
    assert.equal((await fetch(`http://127.0.0.1:${port}${resource}`)).status, 404, 'room server does not expose workspace or cartridge files');
  }
  for (const resource of ['/web/%2e%2e/Cargo.toml', '/web/%2e%2e/roms/romlist.json', '/web/%2e%2e%2froms/private.gba']) {
    const code = await new Promise((resolve, reject) => {
      http.get({ host: '127.0.0.1', port, path: resource }, response => { response.resume(); resolve(response.statusCode); }).on('error', reject);
    });
    assert.equal(code, 404, 'raw encoded paths cannot escape the public asset directory');
  }
  assert.equal((await fetch(`http://127.0.0.1:${port}/web/gba-link.mjs`)).status, 200);
  console.log('GBA rooms: one-invite offer/answer, two-player capacity, URL validation and public-asset isolation passed');
} finally {
  first?.close(); second?.close(); server.kill();
  if (server.exitCode === null) await once(server, 'exit');
}

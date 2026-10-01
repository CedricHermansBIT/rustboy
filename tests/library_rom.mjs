import assert from 'node:assert/strict';
import {webcrypto} from 'node:crypto';
import {fetchLibraryRom} from '../web/library-rom.mjs';

const bytes = new Uint8Array(32768);
const digest = Buffer.from(await webcrypto.subtle.digest('SHA-256', bytes)).toString('hex');
const entry = {name: 'demo.gb', size: bytes.length, sha256: digest};
let requested;
const fetchFile = async (url, options) => {
  requested = url;
  assert.ok(options.signal instanceof AbortSignal);
  return {ok: true, arrayBuffer: async () => bytes.buffer};
};
assert.deepEqual(await fetchLibraryRom(entry, 'homebrew', fetchFile, webcrypto.subtle), bytes);
assert.equal(requested, 'homebrew/demo.gb');
await assert.rejects(fetchLibraryRom({...entry, sha256: '0'.repeat(64)}, 'homebrew', fetchFile), /checksum/);
await assert.rejects(fetchLibraryRom({...entry, sha256: 'garbage'}, 'homebrew', fetchFile), /integrity/);
await assert.rejects(fetchLibraryRom({...entry, size: 1}, 'homebrew', fetchFile), /size mismatch/);
await assert.rejects(fetchLibraryRom(entry, 'homebrew', async () => ({ok: false, status: 404})), /HTTP 404/);
await assert.rejects(fetchLibraryRom(entry, 'homebrew', async () => { throw new Error('offline'); }), /offline/);
for (const name of ['../game.gb', 'folder/game.gb', 'folder\\game.gb', 'game.zip']) {
  await assert.rejects(fetchLibraryRom({...entry, name}, 'homebrew', fetchFile), /filename/);
}
assert.deepEqual(await fetchLibraryRom({name: 'my game.gb'}, 'roms', fetchFile), bytes);
assert.equal(requested, 'roms/my%20game.gb');
console.log('Library ROM loader: pinned size/hash validation, download failures, safe names, and local-library compatibility passed');

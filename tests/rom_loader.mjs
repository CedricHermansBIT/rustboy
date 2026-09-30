import assert from 'node:assert/strict';
import {loadWithBootRom} from '../web/rom-loader.mjs';

for (const flag of [0, 0x80, 0xC0]) {
  const rom = new Uint8Array(32768); rom[0x143] = flag;
  let called = false;
  await loadWithBootRom((data, boot) => {
    assert.equal(data, rom);
    assert.equal(boot.length, flag ? 2304 : 256);
    called = true;
  }, rom, async path => {
    assert.equal(path, flag ? 'roms/cgb_boot.bin' : 'roms/dmg_boot.bin');
    return {ok: true, arrayBuffer: async () => new ArrayBuffer(flag ? 2304 : 256)};
  });
  assert.ok(called);
}
let called = false;
const load = () => { called = true; };
await assert.rejects(loadWithBootRom(load, new Uint8Array(5)), /truncated/);
await assert.rejects(loadWithBootRom(load, new Uint8Array(32768), async () => ({ok: false})), /Boot ROM missing/);
await assert.rejects(loadWithBootRom(load, new Uint8Array(32768), async () => ({ok: true, arrayBuffer: async () => new ArrayBuffer(7)})), /Invalid/);
assert.equal(called, false, 'failed loads must leave the running emulator untouched');
console.log('ROM loader: DMG/CGB boot selection, malformed uploads, missing/invalid boot ROMs passed');

import assert from 'node:assert/strict';
import {loadWithBootRom,loadCartridge,romFormat,validateUploadSize,GBA_MAX_BYTES} from '../web/rom-loader.mjs';

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
await assert.rejects(loadWithBootRom(load, new Uint8Array(32768), async () => ({ok: true, arrayBuffer: async () => new ArrayBuffer(7)})), /Invalid/);
assert.equal(called, false, 'failed loads must leave the running emulator untouched');
for (const flag of [0, 0x80, 0xC0]) {
  const rom = new Uint8Array(32768); rom[0x143] = flag;
  for (const fetchFile of [async () => ({ok: false}), async () => {throw new TypeError('network unavailable');}]) {
    let fallback = false;
    await loadWithBootRom((data, boot) => {
      assert.equal(data, rom);
      assert.equal(boot.length, 0, 'empty override selects the bundled replacement');
      fallback = true;
    }, rom, fetchFile);
    assert.ok(fallback);
  }
}
for (const [model, flag, cgb] of [['sgb', 0x80, false], ['dmg', 0x80, false], ['cgb', 0, true], ['auto', 0x80, true]]) {
  const rom = new Uint8Array(32768); rom[0x143] = flag;
  await loadWithBootRom((data, boot) => assert.equal(boot.length, cgb ? 2304 : 256), rom,
    async path => {
      assert.equal(path, cgb ? 'roms/cgb_boot.bin' : 'roms/dmg_boot.bin');
      return {ok: true, arrayBuffer: async () => new ArrayBuffer(cgb ? 2304 : 256)};
    }, model);
}
const colorOnly = new Uint8Array(32768); colorOnly[0x143] = 0xC0;
for (const [flag, license, cgb] of [[0,0x33,false],[0x80,0x33,false],[0xC0,0x33,true],[0x80,0,true]]) {
  const rom = new Uint8Array(32768); rom[0x143] = flag; rom[0x146] = 3; rom[0x14B] = license;
  await loadWithBootRom((_, boot) => assert.equal(boot.length, cgb ? 2304 : 256), rom,
    async name => {
      assert.equal(name, cgb ? 'roms/cgb_boot.bin' : 'roms/dmg_boot.bin');
      return {ok: true, arrayBuffer: async () => new ArrayBuffer(cgb ? 2304 : 256)};
    });
}
for (const model of ['sgb', 'dmg', 'unknown']) {
  await assert.rejects(loadWithBootRom(() => assert.fail('invalid model must not load'), colorOnly,
    () => assert.fail('invalid model must not fetch firmware'), model), /hardware/);
}
console.log('ROM loader: explicit DMG/CGB/SGB models, malformed uploads, and boot-file-free/network-error fallback passed');

for (const size of [192,16*1024*1024,32*1024*1024]) {
  const rom=new Uint8Array(size);rom[0xb2]=0x96;
  let calls=0;
  await loadCartridge(()=>assert.fail('GBA must not enter GB backend'),(data,bios)=>{
    assert.equal(data,rom,'large cartridge must not be converted/copied in JS');assert.equal(bios.length,0);calls++;
  },rom,'Uploaded.GBA',()=>assert.fail('GBA must not request GB/CGB firmware'),'sgb');
  assert.equal(calls,1);validateUploadSize(size,'game.gba');
}
assert.equal(GBA_MAX_BYTES,32*1024*1024);
assert.throws(()=>validateUploadSize(GBA_MAX_BYTES+1,'game.gba'),/32 MiB/);
assert.throws(()=>validateUploadSize(16*1024*1024,'game.gb'),/8 MiB/);
assert.throws(()=>validateUploadSize(0,'game.gba'),/truncated/);
const invalidGba=new Uint8Array(192);
await assert.rejects(loadCartridge(()=>assert.fail(),()=>assert.fail(),invalidGba,'game.gba'),/header/);
invalidGba[0xb2]=0x96;
await assert.rejects(loadCartridge(()=>assert.fail(),undefined,invalidGba,'game.gba'),/updated WASM/);
let checksum=-0x19;for(let i=0xa0;i<=0xbc;i++)checksum-=invalidGba[i];invalidGba[0xbd]=checksum&255;
assert.equal(romFormat(invalidGba,'game.bin'),'gba');
assert.equal(romFormat(new Uint8Array(32768),'game.gb'),'gb');
await assert.rejects(loadCartridge(()=>assert.fail(),()=>assert.fail(),new Uint8Array(GBA_MAX_BYTES+1),'game.gba'),/32 MiB/);
console.log('GBA uploads: explicit/header detection, 16/32 MiB routing, limits, no GB firmware or JS copies, malformed/old-bundle errors passed');

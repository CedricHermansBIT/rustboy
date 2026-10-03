// Optional external firmware overrides the licensed replacement built into WASM.
export const GBA_MAX_BYTES = 32 * 1024 * 1024;
export function romFormat(data, name = '') {
  if (!(data instanceof Uint8Array)) throw new Error('ROM must be binary data');
  if (/\.gba$/i.test(name)) return 'gba';
  // Header checksum plus fixed marker, without relying on Nintendo logo bytes.
  if (data.length >= 192 && data[0xb2] === 0x96) {
    let checksum = -0x19;
    for (let i = 0xa0; i <= 0xbc; i++) checksum -= data[i];
    if ((checksum & 255) === data[0xbd]) return 'gba';
  }
  return 'gb';
}
export function validateUploadSize(size, name = '') {
  // Before reading a File into memory. Raw .bin files can contain either system.
  const limit = /\.gbc?$/i.test(name) ? 8 * 1024 * 1024 : GBA_MAX_BYTES;
  if (size < 192) throw new Error('ROM is truncated: at least 192 bytes is required');
  if (!Number.isSafeInteger(size) || size > limit) {
    throw new Error(`ROM size must be between 192 bytes and ${limit / 1024 / 1024} MiB`);
  }
}
export async function loadCartridge(loadGb, loadGba, data, name = '', fetchFile = fetch, model = 'auto') {
  const format = romFormat(data, name);
  if (format === 'gba') {
    validateUploadSize(data.length, 'game.gba');
    if (data[0xb2] !== 0x96) throw new Error('Invalid GBA cartridge header marker');
    if (!loadGba) throw new Error('GBA uploads need the updated WASM bundle; refresh the page');
    return loadGba(data, new Uint8Array(0));
  }
  return loadWithBootRom(loadGb, data, fetchFile, model);
}
export async function loadWithBootRom(loadRom, data, fetchFile = fetch, model = 'auto') {
  if (!(data instanceof Uint8Array) || data.length < 32768) {
    throw new Error('ROM is truncated: at least 32 KiB is required');
  }
  if (data.length > 8 * 1024 * 1024) throw new Error('Game Boy ROM exceeds 8 MiB');
  if (!['auto', 'dmg', 'cgb', 'sgb'].includes(model)) throw new Error('Unknown Game Boy hardware model');
  if (['dmg', 'sgb'].includes(model) && data[0x143] === 0xC0) {
    throw new Error('CGB-only cartridge cannot run on DMG/SGB hardware');
  }
  const autoSgb = model === 'auto' && data[0x143] !== 0xC0 && data[0x146] === 3 && data[0x14B] === 0x33;
  const cgb = model === 'cgb' || (model === 'auto' && !autoSgb && (data[0x143] & 0x80) !== 0);
  const path = cgb ? 'roms/cgb_boot.bin' : 'roms/dmg_boot.bin';
  let response;
  try { response = await fetchFile(path, {signal: AbortSignal.timeout(3000)}); }
  catch { return loadRom(data, new Uint8Array(0)); }
  if (!response.ok) return loadRom(data, new Uint8Array(0));
  const boot = new Uint8Array(await response.arrayBuffer());
  if (boot.length !== (cgb ? 2304 : 256)) {
    throw new Error(`Invalid ${path}: expected ${cgb ? 2304 : 256} bytes`);
  }
  return loadRom(data, boot);
}

// Optional external firmware overrides the licensed replacement built into WASM.
export async function loadWithBootRom(loadRom, data, fetchFile = fetch) {
  if (!(data instanceof Uint8Array) || data.length < 32768) {
    throw new Error('ROM is truncated: at least 32 KiB is required');
  }
  const cgb = (data[0x143] & 0x80) !== 0;
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

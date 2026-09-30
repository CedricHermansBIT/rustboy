// Boot ROMs are supplied by the user at runtime, never embedded in the build.
export async function loadWithBootRom(loadRom, data, fetchFile = fetch) {
  if (!(data instanceof Uint8Array) || data.length < 32768) {
    throw new Error('ROM is truncated: at least 32 KiB is required');
  }
  const cgb = (data[0x143] & 0x80) !== 0;
  const path = cgb ? 'roms/cgb_boot.bin' : 'roms/dmg_boot.bin';
  const response = await fetchFile(path);
  if (!response.ok) {
    throw new Error(`Boot ROM missing: provide your own ${path} (${cgb ? 2304 : 256} bytes)`);
  }
  const boot = new Uint8Array(await response.arrayBuffer());
  if (boot.length !== (cgb ? 2304 : 256)) {
    throw new Error(`Invalid ${path}: expected ${cgb ? 2304 : 256} bytes`);
  }
  return loadRom(data, boot);
}

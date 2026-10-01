// Same-origin library downloads, with integrity checks for curated homebrew.
export async function fetchLibraryRom(entry, folder, fetchFile = fetch, subtle = globalThis.crypto.subtle) {
  if (typeof entry.name !== 'string' || /[/\\]/.test(entry.name) || !/\.(gb|gbc)$/i.test(entry.name)) {
    throw new Error('Invalid library ROM filename');
  }
  const response = await fetchFile(`${folder}/${encodeURIComponent(entry.name)}`, {signal: AbortSignal.timeout(15000)});
  if (!response.ok) throw new Error(`ROM download failed (HTTP ${response.status})`);
  const bytes = new Uint8Array(await response.arrayBuffer());
  if (entry.size !== undefined && bytes.length !== entry.size) throw new Error('Homebrew ROM size mismatch');
  if (entry.sha256 !== undefined) {
    if (!/^[0-9a-f]{64}$/.test(entry.sha256)) throw new Error('Invalid homebrew integrity manifest');
    const digest = new Uint8Array(await subtle.digest('SHA-256', bytes));
    const hex = Array.from(digest, byte => byte.toString(16).padStart(2, '0')).join('');
    if (hex !== entry.sha256) throw new Error('Homebrew ROM checksum mismatch; refusing to load');
  }
  return bytes;
}

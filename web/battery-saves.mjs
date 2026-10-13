/* Battery saves keep their established ROM key and raw base64 representation.
 * localStorage.setItem is atomic: failures preserve the previous value. Never
 * replace unreadable data with a new game's empty save. */
const MAX_SAVE_BYTES = 8 * 1024 * 1024;

export class BatterySaveStore {
  constructor(storage, report = () => {}) {
    this.storage = storage;
    this.report = report;
    this.failures = new Map();
  }

  fail(key, message) {
    const detail = `${message} Your saved copy is preserved. Export a backup before closing this page.`;
    if (!this.failures.has(key)) this.report(detail, true);
    this.failures.set(key, detail);
    // Reporting must not make a failed storage operation appear successful.
    return undefined;
  }

  decode(encoded) {
    if (typeof encoded !== 'string' || !encoded.length || encoded.length > MAX_SAVE_BYTES * 4 / 3 + 4
        || encoded.length % 4 !== 0
        || !/^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?$/.test(encoded)) {
      throw new Error('Invalid battery save encoding');
    }
    const raw = atob(encoded);
    if (btoa(raw) !== encoded) throw new Error('Noncanonical battery save encoding');
    return Uint8Array.from(raw, c => c.charCodeAt(0));
  }

  load(key) {
    try {
      const encoded = this.storage.getItem(key);
      if (encoded === null) return undefined;
      return this.decode(encoded);
    } catch (error) {
      return this.fail(key, 'Could not read the cartridge save. It may be corrupted or browser storage is blocked.');
    }
  }

  store(key, data) {
    try {
      // Validate the existing value before overwriting. A corrupt or blocked
      // read is not evidence that no save exists. Deleting/repairing the value
      // explicitly allows subsequent writes again.
      const previous = this.storage.getItem(key);
      if (previous !== null) this.decode(previous);
      if (!data || !Number.isInteger(data.length) || data.length < 1 || data.length > MAX_SAVE_BYTES) {
        throw new Error('Invalid battery save size');
      }
      let raw = '';
      for (let offset = 0; offset < data.length; offset += 8192) {
        const part = Array.from(data.slice(offset, offset + 8192));
        if (part.some(v => !Number.isInteger(v) || v < 0 || v > 255)) throw new Error('Invalid battery save byte');
        raw += String.fromCharCode(...part);
      }
      this.storage.setItem(key, btoa(raw));
      if (this.failures.delete(key)) this.report('Cartridge saving is working again.', false);
      return true;
    } catch (error) {
      const message = error?.name === 'QuotaExceededError'
        ? 'Could not save game: browser storage is full.'
        : 'Could not save game: browser storage is blocked or the existing save is unreadable.';
      this.fail(key, message);
      return false;
    }
  }
}

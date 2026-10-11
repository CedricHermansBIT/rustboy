/* State records remain in IndexedDB. Firmware migrations supply compatible IDs;
 * the emulator validates the binary before restoring it. */
export class StateStore {
  constructor(indexedDB) { this.indexedDB = indexedDB; this.opening = null; }
  open() {
    if (!this.opening) {
      this.opening = new Promise((resolve, reject) => {
        const request = this.indexedDB.open('rustboy_states', 1);
        request.onupgradeneeded = () => request.result.createObjectStore('states', {keyPath: 'key'});
        request.onsuccess = () => {
          const db = request.result;
          db.onversionchange = () => { db.close(); this.opening = null; };
          resolve(db);
        };
        request.onerror = () => reject(request.error);
        request.onblocked = () => reject(new Error('Close other RustBoy tabs to open save storage'));
      }).catch(error => { this.opening = null; throw error; });
    }
    return this.opening;
  }
  async record(mode, value) {
    const db = await this.open();
    return new Promise((resolve, reject) => {
      const tx = db.transaction('states', mode === 'get' ? 'readonly' : 'readwrite');
      const store = tx.objectStore('states');
      const request = mode === 'get' ? store.get(value) : mode === 'put' ? store.put(value) : store.delete(value);
      let result;
      request.onsuccess = () => { result = request.result; };
      // A successful put request can still be rolled back. Report success only
      // after the transaction commits (including quota/disk failures).
      tx.oncomplete = () => resolve(result);
      tx.onabort = tx.onerror = () => reject(tx.error || request.error || new Error('Save transaction aborted'));
    });
  }
  async find(slot, identities) {
    const records = await Promise.all([...new Set(identities)].filter(Boolean)
      .map(id => this.record('get', `${id}:${slot}`)));
    return records.filter(Boolean).sort((a,b) => (b.timestamp || 0) - (a.timestamp || 0))[0] || null;
  }
  async deleteSlot(slot, identities) {
    for (const id of new Set(identities)) if (id) await this.record('delete', `${id}:${slot}`);
  }
}

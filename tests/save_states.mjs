import assert from 'node:assert/strict';
import {StateStore} from '../web/save-states.mjs';

// Requests succeed before their enclosing transaction commits. Simulate a disk
// failure at that boundary to prove we don't announce a save that rolls back.
class Database {
  constructor() { this.records = new Map(); this.failCommit = false; }
  transaction() {
    const tx = {objectStore: () => ({
      get: key => request(() => this.records.get(key)),
      put: value => request(() => undefined, () => this.records.set(value.key, value)),
      delete: key => request(() => undefined, () => this.records.delete(key)),
    })};
    const request = (read, write) => {
      const req = {};
      queueMicrotask(() => {
        req.result = read(); req.onsuccess();
        queueMicrotask(() => {
          if (this.failCommit) {
            tx.error = new Error('Disk quota exceeded'); tx.onabort();
          } else {
            write?.(); tx.oncomplete();
          }
        });
      });
      return req;
    };
    return tx;
  }
}
const db = new Database();
let opens = 0, failOpen = true;
const store = new StateStore({open() {
  opens++;
  const req = {};
  queueMicrotask(() => {
    if (failOpen) { req.error = new Error('Storage temporarily unavailable'); req.onerror(); }
    else { req.result = db; req.onsuccess(); }
  });
  return req;
}});
await assert.rejects(store.open(), /temporarily unavailable/);
failOpen = false;
await Promise.all([store.open(), store.open()]);
assert.equal(opens, 2, 'retry failed opens and share concurrent opens');
const current = {key: 'current:auto', timestamp: 100};
const legacy = {key: 'legacy:auto', timestamp: 200, data: new Uint8Array([1,2]).buffer};
await store.record('put', current);
await store.record('put', legacy);
assert.equal(await store.find('auto', ['current', 'legacy', 'legacy', '']), legacy);
assert.equal(await store.find('1', ['current', 'legacy']), null);
db.failCommit = true;
await assert.rejects(store.record('put', {...legacy, timestamp: 300}), /quota/);
assert.equal(db.records.get(legacy.key), legacy, 'failed replacement preserves the previous save');
await assert.rejects(store.deleteSlot('auto', ['legacy']), /quota/);
assert.equal(db.records.get(legacy.key), legacy);
db.failCommit = false;
await store.deleteSlot('auto', ['current', 'legacy']);
assert.equal(await store.find('auto', ['current', 'legacy']), null);
console.log('Save states: retry, commit/rollback, compatible identity lookup and alias deletion passed');

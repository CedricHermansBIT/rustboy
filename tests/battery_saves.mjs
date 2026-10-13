import assert from 'node:assert/strict';
import { BatterySaveStore } from '../web/battery-saves.mjs';

function fixture() {
  const values = new Map(), reports = [];
  const storage = {
    getItem: key => values.get(key) ?? null,
    setItem: (key, value) => values.set(key, value),
  };
  return { values, reports, storage, store: new BatterySaveStore(storage, (...args) => reports.push(args)) };
}

const key = 'rustboy_save_gba_9de870926981239c';
{
  const f = fixture(), bytes = Uint8Array.from({ length: 131072 }, (_, i) => i & 255);
  assert.equal(f.store.load(key), undefined);
  assert.equal(f.store.store(key, bytes), true);
  assert.equal(f.values.get(key), Buffer.from(bytes).toString('base64'));
  assert.deepEqual(f.store.load(key), bytes);
  assert.equal(f.reports.length, 0);
}
{
  const f = fixture();
  f.values.set(key, btoa('existing progress'));
  const original = f.values.get(key);
  f.storage.setItem = () => { throw new DOMException('Full', 'QuotaExceededError'); };
  for (let i = 0; i < 5; i++) assert.equal(f.store.store(key, new Uint8Array([1, 2])), false);
  assert.equal(f.values.get(key), original);
  assert.equal(f.reports.length, 1, 'Repeated autosaves do not flood the interface');
  assert.match(f.reports[0][0], /storage is full/);
  f.storage.setItem = (key, value) => f.values.set(key, value);
  assert.equal(f.store.store(key, new Uint8Array([3, 4])), true);
  assert.deepEqual(f.store.load(key), new Uint8Array([3, 4]));
  assert.deepEqual(f.reports.at(-1), ['Cartridge saving is working again.', false]);
}
for (const corrupt of ['!', 'AA=', 'AB==', '', 'AA==\n']) {
  const f = fixture();
  f.values.set(key, corrupt);
  assert.equal(f.store.load(key), undefined);
  assert.equal(f.store.store(key, new Uint8Array([5])), false);
  assert.equal(f.values.get(key), corrupt, 'Corrupted saves remain available for recovery');
  assert.match(f.reports[0][0], /saved copy is preserved/);
  f.values.delete(key);
  assert.equal(f.store.store(key, new Uint8Array([6])), true);
}
{
  const f = fixture();
  f.values.set(key, btoa('safe'));
  const original = f.values.get(key);
  for (const bytes of [[], [-1], [256], [NaN], [1.5]]) {
    assert.equal(f.store.store(key, bytes), false);
    assert.equal(f.values.get(key), original);
  }
  f.storage.getItem = () => { throw new DOMException('Denied', 'SecurityError'); };
  assert.equal(f.store.load(key), undefined);
  assert.equal(f.store.store(key, [1]), false);
  assert.equal(f.values.get(key), original);
}
console.log('Battery saves: legacy roundtrip, quota recovery, privacy errors and corruption preservation passed');

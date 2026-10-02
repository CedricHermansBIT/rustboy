import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import os from 'node:os';
import {execFile} from 'node:child_process';
import {promisify} from 'node:util';
import {fileURLToPath} from 'node:url';

const temporary = await fs.mkdtemp(path.join(os.tmpdir(), 'rustboy-inventory-test-'));
try {
  const rom = new Uint8Array(32768); rom[0x147] = 0x19;
  rom.set([0x3E, 0x55, 0xEA, 0, 0x14], 0x200);
  await fs.writeFile(path.join(temporary, 'Bootleg (Unl).gbc'), rom);
  await fs.writeFile(path.join(temporary, 'Demo (Aftermarket) (Unl).gb'), rom);
  await fs.writeFile(path.join(temporary, 'Bad (Unl) [b].gb'), new Uint8Array(8));
  await fs.writeFile(path.join(temporary, 'Licensed.gb'), rom);
  const script = fileURLToPath(new URL('../scripts/cartridge_inventory.mjs', import.meta.url));
  const {stdout} = await promisify(execFile)(process.execPath, [script, temporary]);
  const inventory = JSON.parse(stdout);
  assert.equal(inventory.count, 3);
  const bootleg = inventory.entries.find(e => e.file.startsWith('Bootleg'));
  assert.equal(bootleg.ntActivationCandidate, 0x200);
  assert.equal(bootleg.mapper, '19');
  assert.match(bootleg.sha256, /^[0-9a-f]{64}$/);
  assert.equal(inventory.groups.aftermarket, 1);
  assert.equal(inventory.groups['header-19'], 1);
  assert.equal(inventory.entries.find(e => e.file.startsWith('Bad')).error, 'truncated header');
  assert.deepEqual(await fs.readFile(path.join(temporary, 'Bootleg (Unl).gbc')), Buffer.from(rom));
  console.log('Cartridge inventory: local-only grouping, candidate signatures, hashes, malformed headers and unchanged files passed');
} finally {
  await fs.rm(temporary, {recursive: true, force: true});
}

// Independent CPU vectors, not emulator code. Download into a temporary file;
// nothing is added to the repository. Run with Node 18+ and an online connection.
import { mkdtemp, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { spawnSync } from 'node:child_process';

const directory = await mkdtemp(join(tmpdir(), 'rustboy-spc700-'));
const count = Number(process.env.SPC_TEST_COUNT ?? 256);
if (!Number.isInteger(count) || count < 1 || count > 10000) throw Error('SPC_TEST_COUNT must be 1..10000');
const records = new Array(256);
let next = 0;
await Promise.all(Array.from({ length: 8 }, async () => {
  while (next < 256) {
    const opcode = next++;
    const name = opcode.toString(16).padStart(2, '0');
    const response = await fetch(`https://raw.githubusercontent.com/SingleStepTests/spc700/main/v1/${name}.json`);
    if (!response.ok) throw Error(`${name}: HTTP ${response.status}`);
    const tests = await response.json();
    const chunks = [];
    const u16 = value => { const b = Buffer.alloc(2); b.writeUInt16LE(value); return b; };
    const state = s => Buffer.concat([u16(s.pc), Buffer.from([s.a, s.x, s.y, s.sp, s.psw]), u16(s.ram.length), ...s.ram.map(([address, value]) => Buffer.concat([u16(address), Buffer.from([value])]))]);
    // Evenly sample the entire file rather than only its initial records.
    for (let i = 0; i < Math.min(count, tests.length); ++i) {
      const index = Math.floor(i * tests.length / Math.min(count, tests.length));
      const t = tests[index];
      chunks.push(Buffer.from([opcode]), u16(index), state(t.initial), state(t.final), Buffer.from([t.cycles.length]));
    }
    records[opcode] = Buffer.concat(chunks);
  }
}));
const path = join(directory, 'vectors.bin');
await writeFile(path, Buffer.concat(records));
console.log(`Downloaded independent CPU vectors to ${path}`);
const result = spawnSync('cargo', ['run', '--release', '--no-default-features', '--example', 'spc700_verify', '--target-dir', process.env.CARGO_TARGET_DIR ?? '/tmp/rustboy-verify', '--', path], { stdio: 'inherit' });
process.exitCode = result.status ?? 1;

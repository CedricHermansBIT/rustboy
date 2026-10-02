// Read-only local inventory. Unl includes homebrew, not just pirate cartridges.
import fs from 'node:fs/promises';
import path from 'node:path';
import {createHash} from 'node:crypto';

const directory = path.resolve(process.argv[2] || 'roms');
const entries = [];
for (const file of (await fs.readdir(directory)).sort()) {
  if (!/\.(gb|gbc)$/i.test(file) || !file.includes('(Unl)')) continue;
  const rom = await fs.readFile(path.join(directory, file));
  if (rom.length < 0x150) { entries.push({file, error: 'truncated header'}); continue; }
  const activation = rom.indexOf(Buffer.from([0x3E, 0x55, 0xEA, 0x00, 0x14]));
  entries.push({file, size: rom.length, title: rom.subarray(0x134, 0x143).toString('latin1').replace(/\0.*$/, ''),
    mapper: rom[0x147].toString(16).padStart(2, '0'), cgb: rom[0x143], sgb: rom[0x146],
    license: rom.subarray(0x144, 0x146).toString('latin1'), aftermarket: file.includes('(Aftermarket)'),
    markedBad: file.includes('[b]'), sha256: createHash('sha256').update(rom).digest('hex'),
    // A candidate instruction signature is evidence for investigation, not proof
    // of execution, mapper identity or automatic compatibility selection.
    ntActivationCandidate: activation >= 0 ? activation : null});
}
const groups = {};
for (const entry of entries) {
  const group = entry.aftermarket ? 'aftermarket' : `header-${entry.mapper || 'invalid'}`;
  groups[group] = (groups[group] || 0) + 1;
}
console.log(JSON.stringify({directory, count: entries.length, groups, entries}, null, 2));

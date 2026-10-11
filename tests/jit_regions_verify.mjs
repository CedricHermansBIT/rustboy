// Generated ARM regions versus the independent Rust interpreter.
import assert from 'node:assert/strict';
import {createReadStream} from 'node:fs';
import {createInterface} from 'node:readline';
const memory = new WebAssembly.Memory({initial:1});
const data = new DataView(memory.buffer);
let count=0;
for await (const line of createInterface({input:createReadStream(process.argv[2]),crlfDelay:Infinity})) {
  const f=JSON.parse(line);
  f.initial.forEach((value,n)=>data.setUint32(n*4,value,true));
  data.setUint32(64,f.flags,true); data.setBigUint64(f.countOffset,0n,true);
  const instance=new WebAssembly.Instance(new WebAssembly.Module(Buffer.from(f.wasm,'hex')),{env:{memory}});
  const packed=instance.exports.run(0,512,f.budget);
  assert.equal(Number(packed&0xffffffffn),f.clocks,`clocks case ${count}`);
  assert.equal(Number(packed>>32n),f.last,`last PC case ${count}`);
  assert.deepEqual(Array.from({length:16},(_,n)=>data.getUint32(n*4,true)),f.expected,`registers case ${count}`);
  assert.equal(data.getUint32(64,true),f.expectedFlags,`flags case ${count}`);
  assert.equal(data.getBigUint64(f.countOffset,true),BigInt(f.instructions),`count case ${count}`);
  count++;
}
console.log(`JIT: ${count} ARM cases match registers, flags, instruction counts and exact clocks`);

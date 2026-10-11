// Compare actual generated WASM against the independent Rust interpreter.
// cargo run --release -p rustboy-gba --features jit --example jit_verify -- target/jit-cases.jsonl
// node tests/jit_verify.mjs target/jit-cases.jsonl
import assert from 'node:assert/strict';
import {createReadStream} from 'node:fs';
import {createInterface} from 'node:readline';
const memory = new WebAssembly.Memory({initial:1});
const data = new DataView(memory.buffer);
let events, op, pc, sequential;
const u32 = value => value >>> 0;
const rotate = (value, shift) => u32(value >>> shift | value << ((32-shift)&31));
const env = {
  memory,
  fetch(cpu, bus, address) {
    events.push([0,u32(address),1], [1,u32(address),sequential]);
    pc = u32(address+2); sequential = 1;
  },
  finish() { return 1; },
  fetch_run(cpu, bus, address, count, budget) {
    const completed = Math.min(count, budget);
    for (let n=0;n<completed;n++) env.fetch(cpu,bus,u32(address+n*2));
    return (BigInt(completed)<<32n)|BigInt(completed);
  },
  read(bus, address, kind) {
    address = u32(address);
    let value;
    if (kind === 0) {
      const aligned = u32(address & ~3);
      events.push([4,aligned,0]);
      value = rotate(u32(Math.imul(aligned,0x10203041)) ^ 0x87654321, (address&3)*8);
    } else if (kind === 1) {
      events.push([2,address,0]); value = (address ^ (address>>>8) ^ 0xa5)&255;
    } else {
      const aligned = u32(address & ~1);
      events.push([3,aligned,0]); value = rotate((aligned ^ (aligned>>>16) ^ 0xb579)&65535,(address&1)*8);
    }
    events.push([8,1,0]); return value;
  },
  write(bus, address, value, kind) {
    address = u32(address); value = u32(value);
    if (kind === 0) events.push([7,u32(address&~3),value,0]);
    else if (kind === 1) events.push([5,address,value&255]);
    else events.push([6,u32(address&~1),value&65535,0]);
  },
  internal(bus, clocks) { events.push([8,clocks,0]); },
  branch(cpu, bus, target, condition) {
    const flags = data.getUint32(64,true);
    const n=!!(flags&0x80000000), z=!!(flags&0x40000000), c=!!(flags&0x20000000), v=!!(flags&0x10000000);
    if (![z,!z,c,!c,n,!n,v,!v,c&&!z,!c||z,n===v,n!==v,!z&&n===v,z||n!==v,true][condition]) return;
    pc = u32(target & ~1); sequential = 1;
    events.push([1,pc,0], [1,u32(pc+2),1], [9,u32(pc+2),0]);
  },
};
let count=0;
for await (const line of createInterface({input:createReadStream(process.argv[2]),crlfDelay:Infinity})) {
  const fixture = JSON.parse(line);
  fixture.initial.forEach((value,n) => data.setUint32(n*4,value,true));
  data.setUint32(64,fixture.flags,true);
  events=[]; op=fixture.op; pc=fixture.initial[15]; sequential=0;
  const bytes=Buffer.from(fixture.wasm,'hex');
  const instance = new WebAssembly.Instance(new WebAssembly.Module(bytes), {env});
  assert.equal(instance.exports.run(0,128,fixture.budget ?? 1000),fixture.budget ?? 1);
  const registers = Array.from({length:16},(_,n)=>data.getUint32(n*4,true)); registers[15]=pc;
  assert.deepEqual(registers,fixture.expected,`registers opcode ${op.toString(16)}`);
  assert.equal(data.getUint32(64,true),fixture.expectedFlags,`flags opcode ${op.toString(16)}`);
  assert.deepEqual(events,fixture.events,`bus trace opcode ${op.toString(16)}`);
  count++;
}
console.log(`JIT: ${count} Thumb encodings match interpreter registers, flags and bus events`);

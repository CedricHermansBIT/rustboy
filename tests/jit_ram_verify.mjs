// Generated connected regions versus the independent Rust CPU and real GBA bus.
import assert from 'node:assert/strict';
import {createReadStream} from 'node:fs';
import {createInterface} from 'node:readline';
const memory=new WebAssembly.Memory({initial:6}),data=new DataView(memory.buffer);
const ewram=4096,iwram=ewram+0x40000,context=512,versions=1024,version=2048;
const original=new Uint8Array(0x40000);
for(let n=0;n<original.length;n++)original[n]=(n^(n>>>8)^0xa5)&255;
const put=(n,v)=>data.setUint32(n,v,true),get=n=>data.getUint32(n,true);
function checksum(offset,length){let hash=2166136261;for(let n=0;n<length;n++)hash=Math.imul(hash^data.getUint8(offset+n),16777619);return hash>>>0;}
let count=0;
for await(const line of createInterface({input:createReadStream(process.argv[2]),crlfDelay:Infinity})){
 const f=JSON.parse(line);
 new Uint8Array(memory.buffer,ewram,original.length).set(original);
 new Uint8Array(memory.buffer,iwram,0x8000).set(original.subarray(0,0x8000));
 [...f.ops,0xe12fff1e].forEach((op,n)=>put(iwram+0x100+n*4,op));
 for(let n=0;n<17;n++)put(n*4,f.initial[n]);
 for(let n=0;n<42;n++)put(f.layout[0]+n*4,f.initial[17+n]);
 for(let n=0;n<6;n++)put(f.layout[1]+n*4,f.initial[59+n]);
 data.setUint8(f.layout[2],f.initial[65]);data.setBigUint64(f.layout[3],0n,true);data.setBigUint64(f.layout[4],0n,true);
 [ewram,iwram,(f.control>>>24&15)===15?0:16-(f.control>>>24&15),!(f.control&32)?1:0,versions,version,0].forEach((value,n)=>put(context+n*4,value));
 const instance=new WebAssembly.Instance(new WebAssembly.Module(Buffer.from(f.wasm,'hex')),{env:{memory}});
 const packed=instance.exports.run(0,context,f.budget);
 assert.equal(Number(packed&0xffffffffn),f.clocks,`clocks case ${count}`);
 assert.equal(Number(packed>>32n),f.last,`last PC case ${count}`);
 const actual=Array.from({length:17},(_,n)=>get(n*4));
 for(let n=0;n<42;n++)actual.push(get(f.layout[0]+n*4));for(let n=0;n<6;n++)actual.push(get(f.layout[1]+n*4));
 // IWRAM regions always preserve/set sequential after a completed instruction.
 actual.push(actual[15]===f.initial[15]&&get(f.layout[3])===0?f.initial[65]:1,get(f.layout[3]),get(f.layout[3]+4),get(f.layout[4]),get(f.layout[4]+4));
 assert.deepEqual(actual,f.expected,`CPU case ${count}, ops ${f.ops.map(x=>x.toString(16))}`);
 assert.equal(get(context+24),Number(f.writes.length>0),`write summary case ${count}`);
 for(const [region,index,value] of f.writes)assert.equal(data.getUint8((region?iwram:ewram)+index),value,`RAM case ${count} offset ${index}`);
 if(f.hashes)assert.deepEqual([checksum(ewram,0x40000),checksum(iwram,0x8000)],f.hashes,`complete RAM case ${count}`);
 count++;
}
console.log(`RAM JIT: ${count} cases match complete CPU state, RAM, clocks and guarded exits`);

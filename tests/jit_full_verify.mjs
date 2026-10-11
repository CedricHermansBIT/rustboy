import assert from 'node:assert/strict';
import {createReadStream} from 'node:fs';
import {createInterface} from 'node:readline';
const memory=new WebAssembly.Memory({initial:1}),data=new DataView(memory.buffer);
let f,events,sequential,clocks,count=0;
const u=x=>x>>>0, get=n=>data.getUint32(n,true),put=(n,v)=>data.setUint32(n,v,true);
const rotate=(v,s)=>u(v>>>s|v<<((32-s)&31));
const bank=mode=>({16:0,31:0,17:1,18:2,19:3,23:4,27:5})[mode&31];
function setFlags(flags){
 const next=bank(flags),old=bank(get(64));if(next===undefined)return false;
 if(old!==next){
  const [banks]=f.layout,shared=old===1?1:0;
  for(let r=8;r<13;r++)put(banks+shared*28+(r-8)*4,get(r*4));
  for(let r=13;r<15;r++)put(banks+old*28+(r-8)*4,get(r*4));
  for(let r=8;r<13;r++)put(r*4,get(banks+(next===1?1:0)*28+(r-8)*4));
  for(let r=13;r<15;r++)put(r*4,get(banks+next*28+(r-8)*4));
 }
 put(64,flags&0xf00000ff);return true;
}
function refill(value,exchange=false){
 if(exchange)put(64,u(get(64)&~32|(value&1?32:0)));
 const thumb=!!(get(64)&32),pc=u(value&(thumb?~1:~3));put(60,pc);sequential=1;
 events.push([thumb?1:10,pc,0],[thumb?1:10,u(pc+(thumb?2:4)),1],[9,u(pc+(thumb?2:4)),0]);clocks+=2;
}
const env={memory,
 refill(bus,pc,thumb){pc=u(pc);sequential=1;events.push([thumb?1:10,pc,0],[thumb?1:10,u(pc+(thumb?2:4)),1],[9,u(pc+(thumb?2:4)),0]);clocks+=2;},
 fetch_mode(cpu,bus,pc,thumb){events.push([0,u(pc),thumb],[thumb?1:10,u(pc),sequential]);put(60,u(pc+(thumb?2:4)));sequential=1;clocks++;data.setBigUint64(f.layout[3],data.getBigUint64(f.layout[3],true)+1n,true);},
 finish(){const cost=clocks;clocks=0;return cost;},
 fetch_span(cpu,bus,pc,n,budget,thumb){let done=0,elapsed=0;do{env.fetch_mode(cpu,bus,u(pc+done*(thumb?2:4)),thumb);elapsed+=env.finish();done++;}while(done<n&&done<(f.stopSpan??n)&&elapsed<budget);return BigInt(done)<<32n|BigInt(elapsed)|(f.stopSpan?0x80000000n:0n);},
 internal(bus,n){events.push([8,n,0]);clocks+=n;},
 read_full(bus,address,kind,seq){address=u(address);clocks++;
  if(kind===0||kind===5){const a=u(address&~3);events.push([4,a,seq]);const v=u(Math.imul(a,0x10203041)^0x87654321);return kind===0?rotate(v,(address&3)*8):v;}
  if(kind===1||kind===3||kind===4&&address&1){events.push([2,address,0]);const v=(address^(address>>>8)^0xa5)&255;return kind===1?v:u(v<<24>>24);}
  const a=u(address&~1);events.push([3,a,seq]);const v=(a^(a>>>16)^0xb579)&65535;return kind===4?u(v<<16>>16):rotate(v,(address&1)*8);
 },
 write_full(bus,a,v,kind,seq){a=u(a);v=u(v);clocks++;events.push(kind===1?[5,a,v&255]:kind===2?[6,u(a&~1),v&65535,seq]:[7,u(a&~3),v,seq]);},
 control(cpu,bus,value,kind){
  if(kind===2){const mode=bank(get(64));if(mode!==0&&!setFlags(get(f.layout[1]+mode*4))){env.exception(cpu,bus,0);return;}}
  refill(value,kind===1);
 },
 status(cpu,bus,value,fields,kind){const mode=bank(get(64)),spsr=f.layout[1]+mode*4;
  if(kind===0)return get(64);if(kind===1)return get(spsr);
  if(kind===4){if(mode!==0&&!setFlags(get(spsr)))env.exception(cpu,bus,0);return 0;}
  let mask=fields&8?0xf0000000:0;if(fields&1&&(get(64)&31)!==16)mask|=kind===3?255:0xdf;
  if(kind===3){if(mode!==0)put(spsr,u(get(spsr)&~mask|value&mask));}
  else setFlags(u(get(64)&~mask|value&mask));return 0;
 },
 bank_read(cpu,r){const mode=bank(get(64));if((r>=13&&r<15&&mode!==0)||(r>=8&&r<13&&mode===1))return get(f.layout[0]+(r-8)*4);return r===15?u(get(60)+8):get(r*4);},
 bank_write(cpu,r,value){const mode=bank(get(64));if((r>=13&&r<15&&mode!==0)||(r>=8&&r<13&&mode===1))put(f.layout[0]+(r-8)*4,value);else put(r*4,value);},
 exception(cpu,bus,swi){const old=get(64),lr=get(60),mode=swi?19:27;setFlags(u(old&~63|mode|128));put(f.layout[1]+bank(mode)*4,old);put(56,lr);data.setBigUint64(f.layout[4],data.getBigUint64(f.layout[4],true)+1n,true);refill(swi?8:4);},
};
for await(const line of createInterface({input:createReadStream(process.argv[2]),crlfDelay:Infinity})){
 f=JSON.parse(line);data.setUint8(f.layout[2],f.initial[65]);
 for(let n=0;n<17;n++)put(n*4,f.initial[n]);for(let n=0;n<42;n++)put(f.layout[0]+n*4,f.initial[17+n]);for(let n=0;n<6;n++)put(f.layout[1]+n*4,f.initial[59+n]);
 data.setBigUint64(f.layout[3],0n,true);data.setBigUint64(f.layout[4],0n,true);events=[];sequential=f.initial[65];clocks=0;
 try{
 const instance=new WebAssembly.Instance(new WebAssembly.Module(Buffer.from(f.wasm,'hex')),{env});instance.exports.run(0,512,f.budget??1);
 const actual=Array.from({length:17},(_,n)=>get(n*4));for(let n=0;n<42;n++)actual.push(get(f.layout[0]+n*4));for(let n=0;n<6;n++)actual.push(get(f.layout[1]+n*4));actual.push(sequential,Number(data.getBigUint64(f.layout[3],true)&0xffffffffn),Number(data.getBigUint64(f.layout[3],true)>>32n),Number(data.getBigUint64(f.layout[4],true)&0xffffffffn),Number(data.getBigUint64(f.layout[4],true)>>32n));
 assert.deepEqual(actual,f.expected,`CPU case ${count} ${f.thumb?'Thumb':'ARM'} ${f.op.toString(16)}`);assert.deepEqual(events,f.events,`bus case ${count} ${f.op.toString(16)}`);
 }catch(error){console.error(`Failure case ${count}: ${f.thumb?'Thumb':'ARM'} opcode ${f.op.toString(16)}`);throw error;}
 count++;
}
console.log(`Full JIT: ${count} encodings match complete CPU state and bus events`);

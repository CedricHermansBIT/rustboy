// Optional real Firefox manual-code regression using its built-in WebDriver BiDi.
import assert from 'node:assert/strict';
import {spawn} from 'node:child_process';
import fs from 'node:fs/promises';
import http from 'node:http';
import os from 'node:os';
import path from 'node:path';
const profile=await fs.mkdtemp(path.join(os.tmpdir(),'rustboy-firefox-link-'));
const root=process.cwd();
const mdnsEnabled=process.env.RUSTBOY_DISABLE_MDNS!=='1';
const answerDelay=Number(process.env.RUSTBOY_ANSWER_DELAY_MS??15000);
assert.ok(Number.isFinite(answerDelay)&&answerDelay>=0&&answerDelay<=60000,'answer delay is between 0 and 60000 ms');
await fs.writeFile(path.join(profile,'user.js'),`user_pref("media.peerconnection.ice.obfuscate_host_addresses", ${mdnsEnabled});\n`);
const page=`<!doctype html><script type="module">
import {RtcCable,testBrowserWebRtc} from '/web/gba-link.mjs';
class TracedPeerConnection extends RTCPeerConnection {
  constructor(config){super(config);this.offers=[];this.channels=[];}
  createDataChannel(label,options){this.channels.push(label);return super.createDataChannel(label,options);}
  async setLocalDescription(description){if(description?.type==='offer')this.offers.push(description.sdp);return super.setLocalDescription(description);}
}
window.testBrowserWebRtc=testBrowserWebRtc;window.messages=[];window.cable=new RtcCable({adapter:{detach(){},pause(){}},
PeerConnection:TracedPeerConnection,
${process.env.RUSTBOY_LOCAL_ICE==='1'?'iceServers:[],':''}
onStatus:s=>messages.push(s)});
window.ready=true;
</script>`;
const server=http.createServer(async(req,res)=>{
 try{if(req.url==='/'){res.setHeader('Content-Type','text/html');res.end(page);return;}
 const file=path.resolve(root,'.'+new URL(req.url,'http://localhost').pathname);
 if(!file.startsWith(path.join(root,'web')+path.sep)){res.writeHead(404).end();return;}
 res.setHeader('Content-Type','text/javascript');res.end(await fs.readFile(file));
 }catch{res.writeHead(404).end();}
});
await new Promise(r=>server.listen(0,'127.0.0.1',r));
const browser=spawn(process.env.RUSTBOY_FIREFOX||'firefox',['--headless','--no-remote','--profile',profile,'--remote-debugging-port','0'],{stdio:['ignore','pipe','pipe']});
let socket;
try{
 const address=await new Promise((resolve,reject)=>{
 let output='';const timer=setTimeout(()=>reject(Error('Firefox startup timeout: '+output)),30000);
 const receive=bytes=>{output+=bytes;const match=output.match(/WebDriver BiDi listening on (ws:\/\/[^\s]+)/);if(match){clearTimeout(timer);resolve(match[1]);}};
 browser.stdout.on('data',receive);browser.stderr.on('data',receive);
 browser.once('error',e=>{clearTimeout(timer);reject(e);});browser.once('exit',c=>{clearTimeout(timer);reject(Error('Firefox exited '+c+': '+output));});
 });
 socket=new WebSocket(address+'/session');await new Promise((r,j)=>{socket.onopen=r;socket.onerror=j;});
 const pending=new Map();let id=0;
 socket.onmessage=e=>{const m=JSON.parse(e.data),p=pending.get(m.id);if(p){pending.delete(m.id);clearTimeout(p.timer);m.type==='error'?p.reject(Error(JSON.stringify(m))):p.resolve(m.result);}};
 const command=(method,params={})=>new Promise((resolve,reject)=>{
 const key=++id,timer=setTimeout(()=>{pending.delete(key);reject(Error('BiDi timeout '+method));},30000);
 pending.set(key,{resolve,reject,timer});socket.send(JSON.stringify({id:key,method,params}));
 });
 await command('session.new',{capabilities:{alwaysMatch:{}}});
 const tabs=[];
 const evaluate=async(i,expression)=>{
 const r=await command('script.evaluate',{expression:`(async()=>JSON.stringify(await (${expression})))()`,target:{context:tabs[i]},awaitPromise:true});
 if(r.type!=='success')throw Error(JSON.stringify(r));return r.result.type==='undefined'?undefined:JSON.parse(r.result.value);
 };
 for(let i=0;i<2;i++){
 const {context}=await command('browsingContext.create',{type:'tab'});tabs.push(context);
 await command('browsingContext.navigate',{context,url:`http://127.0.0.1:${server.address().port}/`,wait:'complete'});
 const end=Date.now()+30000;while(!await evaluate(i,'window.ready===true')){assert.ok(Date.now()<end,'Firefox module initializes');await new Promise(r=>setTimeout(r,50));}
 }
 const offer=await evaluate(0,'cable.createOffer()');
 const discovery=await evaluate(0,'({offers:cable.pc.offers.map(sdp=>(sdp.match(/^m=/gm)||[]).length),channels:cable.pc.channels})');
 assert.deepEqual(discovery.offers,[0,1],'empty discovery is followed by the first real data transport, without a rejected transport');
 assert.deepEqual(discovery.channels,['rustboy-gba-cable-v1'],'discovery creates no data channel to tear down');
 const metadata=await evaluate(0,"({mdns: /[.]local\\b/.test(cable.pc.localDescription.sdp),end: /a=end-of-candidates/.test(cable.pc.localDescription.sdp),trickle: /a=ice-options:.*trickle/.test(cable.pc.localDescription.sdp)})");
 assert.equal(metadata.mdns,mdnsEnabled,'the real offer retains mDNS obfuscation when enabled');
 const answer=await evaluate(1,`cable.acceptOffer(${JSON.stringify(offer)})`);
 await new Promise(r=>setTimeout(r,answerDelay));
 assert.ok(await evaluate(1,"cable.pc && !['closed','failed'].includes(cable.pc.connectionState)"),'guest remains alive while the reply code is being returned');
 await evaluate(0,`cable.acceptAnswer(${JSON.stringify(answer)})`);
 const end=Date.now()+30000;
 while(!(await evaluate(0,'cable.connected')&&await evaluate(1,'cable.connected'))){
 if(Date.now()>end){for(let i=0;i<2;i++)console.error('Firefox player '+i,await evaluate(i,'({messages,details:cable.lastFailureDetails,pc:cable.pc?.connectionState})'));throw Error('Firefox manual pairing failed');}
 await new Promise(r=>setTimeout(r,100));
 }
 for(let i=0;i<2;i++){
  assert.equal(await evaluate(i,'cable.dataChannel.readyState'),'open');
  const pair=await evaluate(i,`(async()=>{const report=await cable.pc.getStats();for(const item of report.values()){if(item.type==='transport'&&item.selectedCandidatePairId){const p=report.get(item.selectedCandidatePairId);return {local:report.get(p.localCandidateId)?.candidateType,remote:report.get(p.remoteCandidateId)?.candidateType};}}})()`);
  assert.equal(pair.local,'host','the selected local candidate is a direct host address');
  assert.ok(['host','prflx'].includes(pair.remote),'the selected peer is direct, including peer-reflexive discovery, without a relay');
 }
 await evaluate(0,"(window.romDone=false,window.bytes=Uint8Array.from({length:32768},(_,i)=>i*17&255),bytes[0xb2]=0x96,cable.sendRom(bytes,'Original Firefox test.gba').then(()=>romDone=true).catch(e=>{window.romError=e.message}),true)");
 const wait=async(i,expression)=>{const end=Date.now()+10000;while(!await evaluate(i,expression)){assert.ok(Date.now()<end,'Firefox binary ROM transfer completes');await new Promise(r=>setTimeout(r,50));}};
 await wait(1,"cable.rom.active?.phase==='offered'");
 await evaluate(1,'cable.rom.accept()');
 await wait(1,"cable.rom.active?.phase==='verified'");
 await wait(0,'window.romDone');
 assert.ok(await evaluate(1,"cable.rom.active.bytes.every((value,i)=>value===(i===0xb2?0x96:i*17&255))"),'binary transfer preserves the full original fixture');
 console.log(`Firefox: two top-level tabs, mDNS ${mdnsEnabled?'enabled':'disabled'}, ${answerDelay} ms delayed reply, direct WebRTC and verified binary ROM transfer passed`);
 for(let i=0;i<2;i++)await evaluate(i,'cable.close()');
 if (!process.env.RUSTBOY_SKIP_SELFTEST) console.log(await evaluate(0,'testBrowserWebRtc()'));
}finally{
 socket?.close();if(browser.pid&&browser.exitCode===null){browser.kill();await new Promise(r=>browser.once('exit',r));}
 await new Promise(r=>server.close(r));await fs.rm(profile,{recursive:true,force:true,maxRetries:10,retryDelay:100});
}

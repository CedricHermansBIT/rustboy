// Real Chromium + two independent WASM instances + actual WebRTC. Original
// cartridge verifies received multiplayer words and serial IRQ before painting
// green; ROM transfer pairs before either endpoint has a loaded cartridge.
import assert from 'node:assert/strict';
import { spawn } from 'node:child_process';
import fs from 'node:fs/promises';
import http from 'node:http';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const site = process.env.RUSTBOY_SITE_DIR ? path.resolve(process.env.RUSTBOY_SITE_DIR) : root;
function cartridge() {
  const words = [], literals = [], labels = new Map(), branches = [];
  const emit = word => words.push(word >>> 0);
  const label = name => labels.set(name, words.length);
  const branch = (name, condition = 14) => { branches.push({ index: words.length, name, condition }); emit(0); };
  const load = (register, value) => { literals.push({ index: words.length, register, value }); emit(0); };
  load(2, 0x04000000); emit(0xe3a01000); emit(0xe1c210b0); // Clear forced blank; backdrop displays palette[0].
  load(0, 0x04000120); load(1, 0x6003); emit(0xe1c010b8); // SIOCNT: multiplayer + IRQ.
  emit(0xe1d010b8); emit(0xe3110004); branch('child', 1);
  load(1, 0x1234); emit(0xe1c010ba); load(2, 0x04000130);
  label('key'); emit(0xe1d010b0 | 2 << 16); emit(0xe3110001); branch('key', 1);
  load(1, 0x6083); emit(0xe1c010b8); branch('complete');
  label('child'); load(1, 0xabcd); emit(0xe1c010ba);
  label('busy'); emit(0xe1d010b8); emit(0xe3110080); branch('busy', 0);
  label('complete'); emit(0xe1d010b8); emit(0xe3110080); branch('complete', 1);
  emit(0xe1d010b0); load(3, 0x1234); emit(0xe1510003); branch('failed', 1);
  emit(0xe1d010b2); load(3, 0xabcd); emit(0xe1510003); branch('failed', 1);
  load(2, 0x04000202); emit(0xe1d210b0); emit(0xe3110080); branch('failed', 0);
  load(1, 0x03e0); branch('paint');
  label('failed'); load(1, 0x001f);
  label('paint'); load(2, 0x05000000); emit(0xe1c210b0);
  label('done'); branch('done');
  for (const { index, name, condition } of branches) words[index] = ((condition << 28) | 0x0a000000 | (labels.get(name) - index - 2 & 0xffffff)) >>> 0;
  for (const { index, register, value } of literals) {
    const offset = (words.length - index - 2) * 4;
    words[index] = (0xe59f0000 | register << 12 | offset) >>> 0; words.push(value);
  }
  const rom = new Uint8Array(32768); const view = new DataView(rom.buffer);
  view.setUint32(0, 0xea00003e, true);
  rom.set(new TextEncoder().encode('CABLE CHECK'), 0xa0); rom[0xb2] = 0x96;
  words.forEach((word, index) => view.setUint32(0x100 + index * 4, word, true));
  return rom;
}
// Continuous original guest exercises serial IRQ, Halt, DMA and cartridge writes.
function throughputCartridge() {
  const rom = new Uint8Array(4096), view = new DataView(rom.buffer);
  const assemble = (offset, build) => {
    const words=[], labels=new Map(), branches=[], literals=[];
    const a={emit:w=>words.push(w>>>0),label:n=>labels.set(n,words.length),
      branch:(n,c)=>{branches.push([words.length,n,c]);words.push(0);},
      load:(r,v)=>{literals.push([words.length,r,v]);words.push(0);}};
    build(a);
    for(const [i,n,c] of branches) words[i]=((c<<28)|0x0a000000|((labels.get(n)-i-2)&0xffffff))>>>0;
    for(const [i,r,v] of literals){words[i]=(0xe59f0000|(r<<12)|((words.length-i-2)*4))>>>0;words.push(v);}
    words.forEach((w,i)=>view.setUint32(offset+i*4,w,true));
  };
  assemble(256, a=>{
    a.load(0, 0x03007ffc);
    a.load(1, 0x08000800);
    a.emit(0xe5801000); // IRQ function.
    a.load(0, 0x04000200);
    a.emit(0xe3a01080);
    a.emit(0xe1c010b0); // IE serial.
    a.emit(0xe3a01001);
    a.emit(0xe1c010b8); // IME on.
    a.load(0, 0x04000100);
    a.load(1, 0x0080ff00);
    a.emit(0xe5801000); // Running timer.
    a.load(4, 0x04000120);
    a.load(5, 0x02000000);
    a.load(6, 0x040000d4);
    a.load(7, 0x84000001);
    a.load(8, 0x02000010);
    a.load(9, 0x0e000000);
    a.load(1, 0x6003);
    a.emit(0xe1c410b8); // Multiplayer, IRQ, 115200 baud.
    a.label("loop");
    a.emit(0xe1d411b0); // LDRH r1,[r4,#10h]: KEYINPUT.
    a.emit(0xe1c410ba); // SEND.
    a.emit(0xe5c91000); // Battery byte belongs to this endpoint only.
    a.emit(0xe1d420b8);
    a.emit(0xe3120004);
    a.branch("child", 1);
    a.label("ready");
    a.emit(0xe1d420b8);
    a.emit(0xe3120008);
    a.branch("ready", 0);
    a.load(1, 0x6083);
    a.emit(0xe1c410b8);
    a.branch("halt", 14);
    a.label("child");
    a.emit(0xe1d420b8);
    a.emit(0xe3120080);
    a.branch("child", 0);
    a.label("halt");
    a.emit(0xef020000); // SWI Halt, woken by real serial IRQ.
    a.emit(0xe1d420b8);
    a.emit(0xe3120080);
    a.branch("halt", 1);
    a.emit(0xe1d410b0);
    a.emit(0xe1c510b4); // Received parent keys to RAM+4.
    a.emit(0xe1d410b2);
    a.emit(0xe1c510b6); // Received child keys to RAM+6.
    a.emit(0xe5951000);
    a.emit(0xe2811001);
    a.emit(0xe5851000); // Completion counter.
    a.emit(0xe5864000);
    a.emit(0xe5868004);
    a.emit(0xe5867008); // Immediate DMA3 copies both words.
    a.branch("loop", 14);
  });
  assemble(2048, a=>{
    a.load(0, 0x04000202);
    a.emit(0xe3a01080);
    a.emit(0xe1c010b0); // Clear IF serial.
    a.load(0, 0x0200000c);
    a.emit(0xe5901000);
    a.emit(0xe2811001);
    a.emit(0xe5801000);
    a.emit(0xe12fff1e);
  });
  view.setUint32(0,0xea00003e,true);rom[0xb2]=0x96;
  rom.set(new TextEncoder().encode('LINK SPEED'),0xa0);
  rom.set(new TextEncoder().encode('SRAM_V113'),0x900);return rom;
}
const throughputRom = throughputCartridge();
const rom = cartridge();
const page = `<!doctype html><canvas id="rustboy-canvas" width="240" height="160"></canvas><canvas id="vram-canvas" width="128" height="192" hidden></canvas>
<script>window.toggleVramCanvas=()=>{}; window.storeSaveData=()=>true; window.loadSaveData=()=>null; window.queueAudioSamples=(l,r)=>{if(!l.every(Number.isFinite)||!r.every(Number.isFinite))throw Error('Nonfinite audio');};</script>
<script type="module">
import init,*as wasm from '/out/rustboy.js'; import {RtcCable,wasmLinkAdapter} from '/web/gba-link.mjs';
try { const instance=await init(); wasm.configure_gba_jit?.({env:{memory:instance.memory,fetch:instance.gba_jit_fetch,finish:instance.gba_jit_finish,read:instance.gba_jit_read,write:instance.gba_jit_write,fetch_mode:instance.gba_jit_fetch_mode,read_full:instance.gba_jit_read_full,write_full:instance.gba_jit_write_full,control:instance.gba_jit_control,status:instance.gba_jit_status,bank_read:instance.gba_jit_bank_read,bank_write:instance.gba_jit_bank_write,refill:instance.gba_jit_refill,exception:instance.gba_jit_exception,fetch_span:instance.gba_jit_fetch_span,internal:instance.gba_jit_internal,branch:instance.gba_jit_branch,fetch_run:instance.gba_jit_fetch_run}});
window.api=wasm; window.messages=[]; window.romEvents=[]; window.loaded=false;
window.cable=new RtcCable({adapter:wasmLinkAdapter(wasm),iceServers:[],onStatus:s=>window.messages.push(s),onRom:s=>window.romEvents.push(s),loadRom:async bytes=>{wasm.load_gba_data(bytes,new Uint8Array());wasm.set_paused(true);window.loaded=true;}});
window.pixel=()=>Array.from(document.querySelector('canvas').getContext('2d').getImageData(0,0,1,1).data);window.ready=true;
}catch(error){window.failed=String(error.stack||error);}
</script>`;
const temporary = await fs.mkdtemp(path.join(os.tmpdir(), 'rustboy-gba-link-'));
const server = http.createServer(async (request, response) => {
  try {
    const pathname = new URL(request.url, 'http://localhost').pathname;
    if (pathname === '/') { response.setHeader('Content-Type', 'text/html'); response.end('<iframe id="p0" src="/endpoint" width="260" height="190"></iframe><iframe id="p1" src="/endpoint" width="260" height="190"></iframe>'); return; }
    if (pathname === '/ui') { response.setHeader('Content-Type', 'text/html'); response.end('<iframe id="p0" src="/endpoint-ui" width="500" height="700"></iframe><iframe id="p1" src="/endpoint-ui" width="500" height="700"></iframe>'); return; }
    if (pathname === '/room-ui') { response.setHeader('Content-Type', 'text/html'); response.end('<iframe id="p0" src="/endpoint-ui" width="500" height="700"></iframe><iframe id="p1" src="about:blank" width="500" height="700"></iframe>'); return; }
    if (pathname === '/endpoint-ui') {
      response.setHeader('Content-Type', 'text/html');
      // Offline browser checks use local ICE candidates. Set the initial field
      // before module initialization, including the invite's automatic join.
      const html = (await fs.readFile(path.join(site, 'index.html'))).toString();
      response.end(html.replace(/(<textarea id="gba-link-relay"[^>]*>)(<\/textarea>)/, '$1[]$2'));
      return;
    }
    if (pathname === '/endpoint') { response.setHeader('Content-Type', 'text/html'); response.end(page); return; }
    if (pathname === '/throughput.gba') { response.end(throughputRom); return; }
    if (pathname === '/fixture.gba') { response.end(rom); return; }
    const file = path.resolve(site, '.' + decodeURIComponent(pathname));
    if (!file.startsWith(site + path.sep)) { response.writeHead(403).end(); return; }
    response.setHeader('Content-Type', path.extname(file) === '.wasm' ? 'application/wasm' : 'text/javascript');
    response.end(await fs.readFile(file));
  } catch { response.writeHead(404).end(); }
});
await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
const browser = spawn(process.env.RUSTBOY_BROWSER || 'chromium-browser', ['--headless=new', '--no-sandbox',
  '--disable-dev-shm-usage', '--disable-gpu', '--disable-background-timer-throttling', '--disable-renderer-backgrounding',
  '--disable-backgrounding-occluded-windows', '--remote-debugging-port=0', `--user-data-dir=${temporary}/profile`, 'about:blank'],
  { stdio: ['ignore', 'ignore', 'pipe'] });
let socket, roomServer;
try {
  const endpoint = await new Promise((resolve, reject) => {
    let output = ''; const timer = setTimeout(() => reject(new Error('Chromium did not start')), 30000);
    browser.once('error', error => { clearTimeout(timer); reject(error); });
    browser.once('exit', code => { clearTimeout(timer); reject(new Error(`Chromium exited ${code}: ${output}`)); });
    browser.stderr.on('data', bytes => { output += bytes; const match = output.match(/DevTools listening on (ws:\/\/[^\s]+)/); if (match) { clearTimeout(timer); resolve(match[1]); } });
  });
  socket = new WebSocket(endpoint); await new Promise((resolve, reject) => { socket.onopen = resolve; socket.onerror = reject; });
  let nextId = 0; const pending = new Map(); const exceptions = [];
  socket.onmessage = event => {
    const message = JSON.parse(event.data);
    if (message.method === 'Runtime.exceptionThrown') exceptions.push(message.params.exceptionDetails);
    if (pending.has(message.id)) { const item = pending.get(message.id); pending.delete(message.id); clearTimeout(item.timer); message.error ? item.reject(new Error(JSON.stringify(message.error))) : item.resolve(message.result); }
  };
  const command = (method, params = {}, sessionId) => new Promise((resolve, reject) => {
    const id = ++nextId; const timer = setTimeout(() => { pending.delete(id); reject(new Error(`CDP timeout: ${method}`)); }, 30000);
    pending.set(id, { resolve, reject, timer }); socket.send(JSON.stringify({ id, method, params, ...(sessionId ? { sessionId } : {}) }));
  });
  const { targetId } = await command('Target.createTarget', { url: 'about:blank' });
  const { sessionId } = await command('Target.attachToTarget', { targetId, flatten: true });
  await command('Runtime.enable', {}, sessionId); await command('Page.enable', {}, sessionId);
  const evaluate = async expression => {
    const result = await command('Runtime.evaluate', { expression, awaitPromise: true, returnByValue: true }, sessionId);
    if (result.exceptionDetails) throw new Error(JSON.stringify(result.exceptionDetails)); return result.result.value;
  };
  const until = async expression => {
    const end = Date.now() + 30000;
    while (Date.now() < end) { if (await evaluate(expression)) return; await new Promise(resolve => setTimeout(resolve, 50)); }
    const diagnostics = await evaluate("[...document.querySelectorAll('iframe')].map(f=>({failed:f.contentWindow.failed,messages:f.contentWindow.messages,pixel:f.contentWindow.pixel?.(),transfers:f.contentWindow.cable?.coordinator?.transfers,state:f.contentWindow.api?.gba_link_status?.(),linkStatus:f.contentWindow.document.querySelector('#gba-link-status')?.textContent,romStatus:f.contentWindow.document.querySelector('#gba-link-rom-status')?.textContent}))");
    throw new Error(`Timed out: ${expression}\n${JSON.stringify(diagnostics)}`);
  };
  // Independent top-level tabs, rather than only same-page iframes.
  const tabSessions = [];
  for (let i=0;i<2;i++) {
    const {targetId: tabId}=await command('Target.createTarget',{url:'about:blank'});
    const {sessionId: tabSession}=await command('Target.attachToTarget',{targetId:tabId,flatten:true});
    tabSessions.push({tabId,tabSession});
    await command('Runtime.enable',{},tabSession);
    await command('Page.navigate',{url:`http://127.0.0.1:${server.address().port}/endpoint`},tabSession);
  }
  const inTab=async(i,expression)=>{
    const r=await command('Runtime.evaluate',{expression,awaitPromise:true,returnByValue:true},tabSessions[i].tabSession);
    if(r.exceptionDetails)throw Error(JSON.stringify(r.exceptionDetails));return r.result.value;
  };
  for(let i=0;i<2;i++) {
    const end=Date.now()+30000;
    while(!await inTab(i,'window.ready===true')) {
      assert.ok(Date.now()<end,'top-level WASM tab initializes');await new Promise(r=>setTimeout(r,50));
    }
  }
  const tabsOffer=await inTab(0,'cable.createOffer()');
  const tabsAnswer=await inTab(1,`cable.acceptOffer(${JSON.stringify(tabsOffer)})`);
  await inTab(0,`cable.acceptAnswer(${JSON.stringify(tabsAnswer)})`);
  for(let i=0;i<2;i++) {
    const end=Date.now()+30000;
    while(!await inTab(i,'cable.connected')) {
      assert.ok(Date.now()<end,`top-level pairing failed: ${await inTab(i,'JSON.stringify(messages)')}`);
      await new Promise(r=>setTimeout(r,50));
    }
  }
  for(let i=0;i<2;i++)await inTab(i,'cable.close()');
  for(const {tabId}of tabSessions)await command('Target.closeTarget',{targetId:tabId});
  console.log('GBA top-level tabs: actual WebRTC manual offer/answer pairing passed');
  await command('Page.navigate', { url: `http://127.0.0.1:${server.address().port}/` }, sessionId);
  await until("[...document.querySelectorAll('iframe')].length===2&&[...document.querySelectorAll('iframe')].every(f=>f.contentWindow.ready)");
  await evaluate("window.p0=document.querySelector('#p0').contentWindow;window.p1=document.querySelector('#p1').contentWindow;void 0");
  const offer = await evaluate('p0.cable.createOffer()');
  const answer = await evaluate(`p1.cable.acceptOffer(${JSON.stringify(offer)})`);
  await evaluate(`p0.cable.acceptAnswer(${JSON.stringify(answer)})`);
  await until('p0.cable.connected&&p1.cable.connected');
  assert.deepEqual(await evaluate('[p0.loaded,p1.loaded]'), [false, false], 'devices pair before any ROM is loaded');
  await evaluate("void (async()=>{const bytes=new Uint8Array(await(await fetch('/fixture.gba')).arrayBuffer());await p0.cable.sendRom(bytes,'Original cable.gba',{loadLocal:true});p0.sent=true;})().catch(e=>p0.failed=String(e))");
  await until("p1.cable.rom?.active?.phase==='offered'");
  assert.equal(await evaluate('p1.loaded'), false, 'ROM offer does not load the game');
  await evaluate('p1.cable.rom.accept()');
  await until("p1.cable.rom?.active?.phase==='verified'&&p0.sent");
  assert.equal(await evaluate('p1.loaded'), false, 'checksum verification requires a separate Load action');
  await evaluate('p1.cable.rom.load()');
  assert.deepEqual(await evaluate('[p0.loaded,p1.loaded]'), [true, true]);
  await evaluate('p0.cable.startLink();p1.cable.startLink()');
  await until('p0.cable.coordinator?.hello&&p1.cable.coordinator?.hello');
  await evaluate('p0.api.set_paused(false);p1.api.set_paused(false)');
  await until('[p0,p1].every(p=>JSON.parse(p.api.gba_link_status()).mode===2)');
  await evaluate("p0.api.set_controller_button(0,'a',true)");
  await until('[p0,p1].every(p=>p.pixel()[0]===0&&p.pixel()[1]===255&&p.pixel()[2]===0)');
  assert.deepEqual(await evaluate('[p0.cable.coordinator.transfers,p1.cable.coordinator.transfers]'), [1, 1]);
  assert.deepEqual(await evaluate('[p0,p1].map(p=>{p.api.set_speed(8);return p.api.get_speed();})'), [1, 1], 'linked emulators cannot fast-forward independently');
  await evaluate("p0.api.set_controller_button(0,'a',false);p0.api.import_state(p0.api.export_state())");
  await until('!p0.cable.coordinator&&!p1.cable.coordinator');
  assert.deepEqual(await evaluate('[p0,p1].map(p=>JSON.parse(p.api.gba_link_status()).connected)'), [false, false]);
  assert.deepEqual(await evaluate('[p0.cable.connected,p1.cable.connected]'), [true, true], 'hardware session reset preserves explicit file-sharing transport');
  await evaluate('p0.cable.close()'); await until('!p1.cable.connected');
  // Real WASM throughput with 80 ms added to every outbound WebRTC packet.
  await command('Page.navigate', { url: `http://127.0.0.1:${server.address().port}/` }, sessionId);
  await until("[...document.querySelectorAll('iframe')].length===2&&[...document.querySelectorAll('iframe')].every(f=>f.contentWindow.ready)");
  await evaluate("window.p0=document.querySelector('#p0').contentWindow;window.p1=document.querySelector('#p1').contentWindow;void 0");
  await evaluate(`(async()=>{const bytes=new Uint8Array(await(await fetch('/throughput.gba')).arrayBuffer());for(const p of [p0,p1]){
    const original=p.RTCDataChannel.prototype.send;p.inputPackets=0;p.legacyPackets=0;
    p.RTCDataChannel.prototype.send=function(data){
      if(typeof data==='string'){const packet=JSON.parse(data).packet;if(packet?.type==='local-input')p.inputPackets++;if(['request','commit'].includes(packet?.type))p.legacyPackets++;}
      const copy=typeof data==='string'?data:data.slice(0);
      p.setTimeout(()=>{if(this.readyState==='open')original.call(this,copy);},80);
    };await p.cable.loadRom(bytes);
  }})()`);
  const slowOffer=await evaluate('p0.cable.createOffer()');
  const slowAnswer=await evaluate(`p1.cable.acceptOffer(${JSON.stringify(slowOffer)})`);
  await evaluate(`p0.cable.acceptAnswer(${JSON.stringify(slowAnswer)})`);
  await until('p0.cable.connected&&p1.cable.connected');
  await evaluate('p0.cable.startLink();p1.cable.startLink()');
  await until('p0.cable.coordinator?.hello&&p1.cable.coordinator?.hello');
  await evaluate('p0.api.set_paused(false);p1.api.set_paused(false)');
  await until('[p0,p1].every(p=>JSON.parse(p.api.gba_local_link_status()).frame>60)');
  const sample=()=>evaluate('[p0,p1].map(p=>({...JSON.parse(p.api.gba_local_link_status()),time:p.performance.now(),packets:p.inputPackets,legacy:p.legacyPackets}))');
  const before=await sample();
  await new Promise(resolve=>setTimeout(resolve,5000));
  const after=await sample();
  const rates=after.map((s,i)=>(s.frame-before[i].frame)*1000/(s.time-before[i].time));
  for(let i=0;i<2;i++){
    assert.ok(rates[i]>50, `paired WASM under 160 ms RTT ran at ${rates[i].toFixed(1)} fps`);
    assert.ok(after[i].transfers-before[i].transfers>3000,'continuous guest executes thousands of local serial transfers');
    assert.ok(after[i].packets-before[i].packets<400,'network traffic is bounded to frame inputs');
    assert.equal(after[i].legacy,0,'no network request/commit per serial word');
  }
  console.log(`GBA actual paired WASM: ${rates.map(r=>r.toFixed(1)).join('/')} fps with 80 ms one-way delay; ${after[0].transfers-before[0].transfers} serial transfers, ${after[0].packets-before[0].packets} input packets in five seconds`);
  await evaluate('p0.cable.close();p1.cable.close()');
  // Repeat through the actual Settings panel and standard upload/save/UI paths.
  // In particular, closing player one's Settings before player two is ready
  // must leave its guest clock frozen, then correctly resume both games.
  await command('Page.navigate', { url: `http://127.0.0.1:${server.address().port}/ui` }, sessionId);
  await until("[...document.querySelectorAll('iframe')].length===2&&[...document.querySelectorAll('iframe')].every(f=>typeof f.contentWindow.mem==='function')");
  await evaluate("window.p0=document.querySelector('#p0').contentWindow;window.p1=document.querySelector('#p1').contentWindow;void 0");
  await evaluate("Promise.all([p0,p1].map(async p=>{p.api=await p.eval('import(\"/out/rustboy.js\")');p.document.querySelector('[data-open-link]').click();p.document.querySelector('#gba-link-relay').value='[]';})).then(()=>undefined)");
  await evaluate("(async()=>{const bytes=await(await fetch('/fixture.gba')).arrayBuffer();const transfer=new p0.DataTransfer();transfer.items.add(new p0.File([bytes],'Original cable.gba'));const input=p0.document.querySelector('#rom-file-input');input.files=transfer.files;input.dispatchEvent(new p0.Event('change',{bubbles:true}));})()");
  await until("p0.eval('romLoaded')");
  assert.ok(await evaluate("[p0,p1].every(p=>p.document.querySelector('#gba-link-menu').open && p.document.querySelector('#gba-link-create').getClientRects().length && p.document.querySelector('#gba-link-connect').getClientRects().length)"), 'Link opens directly with visible serverless handshake controls');
  await evaluate("p0.document.querySelector('#gba-link-create').click()");
  await until("p0.document.querySelector('#gba-link-local-code').value.startsWith('rustboy-link-v2:')");
  assert.deepEqual(await evaluate("[p0,p1].map(p=>p.document.querySelector('#emulator-settings').open)"),[false,false],'Link uses its own menu');
  await evaluate("[p0,p1].forEach(p=>p.document.querySelector('#gba-link-self-test').click())");
  await until("[p0,p1].every(p=>p.document.querySelector('#gba-link-self-test-result').textContent.startsWith('Native WebRTC self-test passed'))");
  const uiOffer = await evaluate("p0.document.querySelector('#gba-link-local-code').value");
  await evaluate(`p1.document.querySelector('#gba-link-remote-code').value=${JSON.stringify(uiOffer)};p1.document.querySelector('#gba-link-connect').click()`);
  await until("p1.document.querySelector('#gba-link-local-code').value.startsWith('rustboy-link-v2:')");
  const uiAnswer = await evaluate("p1.document.querySelector('#gba-link-local-code').value");
  await evaluate(`p0.document.querySelector('#gba-link-remote-code').value=${JSON.stringify(uiAnswer)};p0.document.querySelector('#gba-link-connect').click()`);
  await until("[p0,p1].every(p=>p.document.querySelector('#gba-link-status').textContent.includes('Device connected'))");
  await evaluate("p0.document.querySelector('#gba-link-rom-current').click()");
  await until("!p1.document.querySelector('#gba-link-rom-play').disabled");
  assert.equal(await evaluate("p1.eval('romLoaded')"), false);
  await evaluate("p1.document.querySelector('#gba-link-rom-play').click()");
  await until("p1.eval('romLoaded')");
  assert.deepEqual(await evaluate("[p0,p1].map(p=>p.document.querySelector('#gba-link-menu').open)"), [true, true]);
  await evaluate("p0.document.querySelector('#gba-link-start').click();p0.document.querySelector('#gba-link-close').click()");
  const frozen = await evaluate('JSON.parse(p0.api.gba_link_status()).clock');
  await new Promise(resolve => setTimeout(resolve, 150));
  assert.equal(await evaluate('JSON.parse(p0.api.gba_link_status()).clock'), frozen, 'closing Settings early cannot run the first endpoint ahead');
  assert.equal(await evaluate('p0.api.is_paused()'), false, 'hardware barrier is independent of the user pause control');
  await evaluate("p1.document.querySelector('#gba-link-start').click();p1.document.querySelector('#gba-link-close').click()");
  await until('[p0,p1].every(p=>JSON.parse(p.api.gba_link_status()).mode===2)');
  await evaluate("p0.api.set_controller_button(0,'a',true)");
  await until("[p0,p1].every(p=>{const c=p.document.querySelector('#rustboy-canvas');const pixel=c.getContext('2d').getImageData(0,0,1,1).data;return pixel[0]===0&&pixel[1]===255&&pixel[2]===0;})");
  await evaluate("p0.eval('Module._import_state(Module._export_state())')");
  await until('[p0,p1].every(p=>!JSON.parse(p.api.gba_link_status()).connected)');
  await until("/disconnect|connection lost|link stopped|reconnect/i.test(p1.document.querySelector('#gba-link-status').textContent)");
  assert.equal(await evaluate('p1.api.is_paused()'), true, 'peer is paused when the actual app restores a linked snapshot');

  // The optional room/invite flow uses the real Python WebSocket server and
  // actual Settings pages. The frontend is on a separate allowed origin, as
  // with static hosting; the invite carries that server address to the guest.
  const origin = `http://127.0.0.1:${server.address().port}`;
  roomServer = spawn('python3', [path.join(root, 'scripts/gba_link_server.py'), '--port', '0', '--origin', origin],
    { cwd: root, stdio: ['ignore', 'pipe', 'pipe'] });
  const roomPort = await new Promise((resolve, reject) => {
    let output = '', errors = '';
    const timer = setTimeout(() => reject(new Error(`Room server did not start: ${errors}`)), 10000);
    roomServer.once('error', error => { clearTimeout(timer); reject(error); });
    roomServer.once('exit', code => { clearTimeout(timer); reject(new Error(`Room server exited ${code}: ${errors}`)); });
    roomServer.stderr.on('data', bytes => { errors = (errors + bytes).slice(-10000); });
    roomServer.stdout.on('data', bytes => {
      output += bytes;
      const match = output.match(/RustBoy room server: http:\/\/127\.0\.0\.1:(\d+)/);
      if (match) { clearTimeout(timer); resolve(Number(match[1])); }
    });
  });
  await command('Page.navigate', { url: `${origin}/room-ui` }, sessionId);
  await until("typeof document.querySelector('#p0').contentWindow.mem==='function'");
  await evaluate("(async()=>{window.p0=document.querySelector('#p0').contentWindow;p0.api=await p0.eval('import(\"/out/rustboy.js\")');p0.openLink();})()");
  await evaluate("(async()=>{const bytes=await(await fetch('/fixture.gba')).arrayBuffer();const transfer=new p0.DataTransfer();transfer.items.add(new p0.File([bytes],'Room cable.gba'));const input=p0.document.querySelector('#rom-file-input');input.files=transfer.files;input.dispatchEvent(new p0.Event('change',{bubbles:true}));})()");
  await until("p0.eval('romLoaded')");
  await evaluate(`p0.document.querySelector('#gba-link-server').value='ws://127.0.0.1:${roomPort}/gba-link/rooms';p0.document.querySelector('#gba-link-room-create').click()`);
  await until("p0.document.querySelector('#gba-link-invite').value.includes('#gba-link=')");
  const invite = await evaluate("p0.document.querySelector('#gba-link-invite').value");
  assert.equal(new URL(invite).searchParams.get('link-server'), `ws://127.0.0.1:${roomPort}/gba-link/rooms`, 'invite includes the configured room server automatically');
  assert.match(new URL(invite).hash, /^#gba-link=[A-Za-z0-9_-]{12}$/, 'invite contains a short room identifier');
  await evaluate(`document.querySelector('#p1').src=${JSON.stringify(invite)};void 0`);
  await until("typeof document.querySelector('#p1').contentWindow.mem==='function'");
  await evaluate("(async()=>{window.p1=document.querySelector('#p1').contentWindow;p1.api=await p1.eval('import(\"/out/rustboy.js\")');})()");
  await until("[p0,p1].every(p=>p.document.querySelector('#gba-link-status').textContent.includes('Device connected'))");
  assert.equal(await evaluate("p1.document.querySelector('#gba-link-menu').open"), true, 'opening the invite automatically opens the link Settings panel');
  assert.equal(await evaluate("p1.document.querySelector('#gba-link-server').value"), `ws://127.0.0.1:${roomPort}/gba-link/rooms`, 'guest requires no room-server entry');
  assert.deepEqual(await evaluate("[p0,p1].map(p=>[p.document.querySelector('#gba-link-local-code').value,p.document.querySelector('#gba-link-remote-code').value])"), [['', ''], ['', '']], 'room invitation requires no manual offer/answer codes');
  assert.equal(await evaluate("p1.eval('romLoaded')"), false, 'opening an invite does not silently load a game');
  const exportedRom = await evaluate('Array.from(p0.api.export_gba_rom())');
  assert.deepEqual(exportedRom, Array.from(rom), 'current-ROM export preserves the original cartridge bytes');
  await evaluate("p0.document.querySelector('#gba-link-rom-current').click()");
  await until("!p1.document.querySelector('#gba-link-rom-play').disabled");
  await evaluate("p1.document.querySelector('#gba-link-rom-play').click()");
  await until("p1.eval('romLoaded')");
  assert.deepEqual(await evaluate('Array.from(p1.api.export_gba_rom())'), exportedRom, 'Receive and load verifies and loads the exact currently playing ROM');
  await evaluate("[p0,p1].forEach(p=>{p.document.querySelector('#gba-link-start').click();p.document.querySelector('#gba-link-close').click();})");
  await until('[p0,p1].every(p=>JSON.parse(p.api.gba_link_status()).mode===2)');
  await evaluate("p0.api.set_controller_button(0,'a',true)");
  await until("[p0,p1].every(p=>{const c=p.document.querySelector('#rustboy-canvas');const pixel=c.getContext('2d').getImageData(0,0,1,1).data;return pixel[0]===0&&pixel[1]===255&&pixel[2]===0;})");
  await evaluate("p0.document.querySelector('#gba-link-disconnect').click()");
  await until('[p0,p1].every(p=>!JSON.parse(p.api.gba_link_status()).connected)');
  await until("/disconnect|connection lost|link stopped|reconnect/i.test(p1.document.querySelector('#gba-link-status').textContent)");
  assert.equal(await evaluate('p1.api.is_paused()'), true, 'room-peer disconnect pauses the guest');
  assert.deepEqual(exceptions, [], 'browser did not throw during pairing, transfer, emulation or state restoration');
  console.log('GBA real WebRTC: pair before ROM, explicit binary ROM receive/load, two WASM machines, guest multiplayer words/IRQ/rendering, 1× lock and restore/disconnect passed');
  console.log('GBA link Settings UI: pair/share/load, close first panel while peer waits, resume fresh roles and disconnect on actual save-state restore passed');
  console.log('GBA room invite: actual Python signaling, Create room -> open invite -> automatic WebRTC, Send current game -> Receive and load, guest words/IRQ/rendering and disconnect passed');
} finally {
  socket?.close();
  if (browser.exitCode === null) { browser.kill(); await new Promise(resolve => browser.once('exit', resolve)); }
  if (roomServer && roomServer.exitCode === null) { roomServer.kill(); await new Promise(resolve => roomServer.once('exit', resolve)); }
  await new Promise(resolve => server.close(resolve));
  await fs.rm(temporary, { recursive: true, force: true, maxRetries: 10, retryDelay: 100 });
}

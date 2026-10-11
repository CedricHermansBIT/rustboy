// A real-browser smoke test using Chromium's built-in DevTools protocol.
// No npm dependencies. RUSTBOY_BROWSER can override the Chromium executable.
// RUSTBOY_ROM optionally adds a gameplay smoke test with your local ROM/BIOS.
import assert from 'node:assert/strict';
import {spawn, execFile} from 'node:child_process';
import {promisify} from 'node:util';
import fs from 'node:fs/promises';
import http from 'node:http';
import os from 'node:os';
import path from 'node:path';
import {fileURLToPath} from 'node:url';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const site = process.env.RUSTBOY_SITE_DIR ? path.resolve(process.env.RUSTBOY_SITE_DIR) : root;
const basePath = process.env.RUSTBOY_BASE_PATH || '';
const temporary = await fs.mkdtemp(path.join(os.tmpdir(), 'rustboy-browser-test-'));
const libraryRequests = [];
// Original executable fixture, not a copyrighted game: the picker must fetch
// a nested filename, route it to GBA and actually render through WASM.
const pickerGba = new Uint8Array(32768);
const pickerWords = [0xe59f0020, 0xe59f1020, 0xe1c010b0, 0xe59f201c, 0xe59f301c,
  0xe3a0101f, 0xe0c210b2, 0xe2533001, 0x1afffffc, 0xeafffffe,
  0x04000000, 0x403, 0x06000000, 38400];
pickerWords.forEach((word, n) => new DataView(pickerGba.buffer).setUint32(n * 4, word, true));
pickerGba[0xb2] = 0x96;
const pickerName = 'Advance/Original red screen.GBA';
const contentTypes = {'.wasm': 'application/wasm', '.js': 'text/javascript', '.mjs': 'text/javascript', '.json': 'application/json', '.html': 'text/html'};
const server = http.createServer(async (request, response) => {
  try {
    const pathname = decodeURIComponent(new URL(request.url, 'http://localhost').pathname);
    if (!pathname.startsWith(basePath + '/')) { response.writeHead(404).end(); return; }
    const name = pathname.slice(basePath.length);
    if (/^\/(roms|testroms)\/romlist\.json$/.test(name)) libraryRequests.push(name);
    if (process.env.RUSTBOY_GBA_PICKER && name === '/roms/romlist.json') {
      response.setHeader('Content-Type', 'application/json');
      response.end(JSON.stringify([{name: 'local.gb'}, {name: 'local.gbc'}, {name: pickerName}]));
      return;
    }
    if (process.env.RUSTBOY_GBA_PICKER && name === `/roms/${pickerName}`) {
      response.end(pickerGba); return;
    }
    if (process.env.RUSTBOY_PUBLIC_LIBRARY && name === '/homebrew/romlist.json') {
      response.setHeader('Content-Type', 'application/json');
      response.end(JSON.stringify([{name: 'demo.gbc', title: 'Free demo', author: 'Test', size: 32768, sha256: '0'.repeat(64)}]));
      return;
    }
    // Model the deployed site: no user-supplied firmware is present.
    if (process.env.RUSTBOY_NO_BOOT && /^\/roms\/(dmg|cgb)_boot\.bin$/.test(name)) {
      response.writeHead(404).end(); return;
    }
    const file = path.resolve(site, '.' + (name === '/' ? '/index.html' : name));
    if (!file.startsWith(site + path.sep)) { response.writeHead(403).end(); return; }
    response.setHeader('Content-Type', contentTypes[path.extname(file)] || 'application/octet-stream');
    const data = await fs.readFile(file);
    response.end(process.env.RUSTBOY_PUBLIC_LIBRARY && name === '/'
      ? data.toString().replace('data-library="local"', 'data-library="homebrew"') : data);
  } catch { response.writeHead(404).end(); }
});
await new Promise((resolve, reject) => {
  server.once('error', reject);
  server.listen(0, '127.0.0.1', resolve);
});
const browser = spawn(process.env.RUSTBOY_BROWSER || 'chromium-browser', [
  '--headless=new', '--no-sandbox', '--disable-dev-shm-usage', '--disable-gpu',
  '--autoplay-policy=no-user-gesture-required',
  '--remote-debugging-port=0', `--user-data-dir=${temporary}/profile`, 'about:blank',
], {stdio: ['ignore', 'ignore', 'pipe']});
let socket;
try {
  const endpoint = await new Promise((resolve, reject) => {
    let output = '';
    const timeout = setTimeout(() => reject(new Error('Chromium did not start')), 30000);
    browser.once('error', error => { clearTimeout(timeout); reject(error); });
    browser.once('exit', code => { clearTimeout(timeout); reject(new Error(`Chromium exited: ${code}: ${output}`)); });
    browser.stderr.on('data', data => {
      output += data;
      const match = output.match(/DevTools listening on (ws:\/\/[^\s]+)/);
      if (match) { clearTimeout(timeout); resolve(match[1]); }
    });
  });
  socket = new WebSocket(endpoint);
  await new Promise((resolve, reject) => { socket.onopen = resolve; socket.onerror = reject; });
  let nextId = 0;
  const pending = new Map();
  const exceptions = [];
  const consoleErrors = [];
  socket.onmessage = event => {
    const message = JSON.parse(event.data);
    if (message.method === 'Runtime.exceptionThrown') exceptions.push(message.params.exceptionDetails);
    if (message.method === 'Runtime.consoleAPICalled' && message.params.type === 'error') {
      consoleErrors.push(message.params.args.map(arg => arg.description ?? String(arg.value)).join(' '));
    }
    if (message.id && pending.has(message.id)) {
      const {resolve, reject, timeout} = pending.get(message.id);
      clearTimeout(timeout); pending.delete(message.id);
      message.error ? reject(new Error(JSON.stringify(message.error))) : resolve(message.result);
    }
  };
  function command(method, params = {}, sessionId) {
    const id = ++nextId;
    return new Promise((resolve, reject) => {
      const timeout = setTimeout(() => { pending.delete(id); reject(new Error(`CDP timeout: ${method}`)); }, 30000);
      pending.set(id, {resolve, reject, timeout});
      socket.send(JSON.stringify({id, method, params, ...(sessionId ? {sessionId} : {})}));
    });
  }
  const {targetId} = await command('Target.createTarget', {url: 'about:blank'});
  const {sessionId} = await command('Target.attachToTarget', {targetId, flatten: true});
  const call = (method, params) => command(method, params, sessionId);
  await call('Runtime.enable');
  await call('Page.enable');
  // Exercise migration from the previous logging shortcut on a fresh profile.
  await call('Page.addScriptToEvaluateOnNewDocument', {source:
    "if (!localStorage.getItem('rustboy_keybindings')) localStorage.setItem('rustboy_keybindings', JSON.stringify({console_log: 'F5'}));"});
  async function evaluate(expression) {
    const result = await call('Runtime.evaluate', {expression, returnByValue: true, awaitPromise: true});
    if (result.exceptionDetails) throw new Error(JSON.stringify(result.exceptionDetails));
    return result.result.value;
  }
  async function until(expression, timeout = 30000) {
    const end = Date.now() + timeout;
    while (Date.now() < end) {
      if (await evaluate(expression)) return;
      await new Promise(resolve => setTimeout(resolve, 100));
    }
    const diagnostics = [...consoleErrors, ...exceptions.map(error =>
      error.exception?.description ?? error.text)];
    throw new Error(`Browser assertion timed out: ${expression}${diagnostics.length ? '\nBrowser errors:\n' + diagnostics.join('\n') : ''}`);
  }
  async function checkActualSpeed(system) {
    await evaluate("openSettings(); document.querySelector('#show-actual-speed').click()");
    assert.equal(await evaluate("document.querySelector('#actual-speed').hidden"), false);
    await until("document.querySelector('#actual-speed').textContent === 'Paused'");
    assert.equal(await evaluate("localStorage.getItem('rustboy_show_actual_speed')"), 'true');
    await evaluate("document.querySelector('#settings-close').click(); Module._set_speed(8)");
    await until("Number.isFinite(Module._get_actual_speed()) && /^Actual: [0-9]+\\.[0-9]{2}× \\([0-9]+%\\)$/.test(document.querySelector('#actual-speed').textContent)");
    assert.ok(await evaluate("(() => { updateActualSpeed(); const speed = Module._get_actual_speed(); return document.querySelector('#actual-speed').textContent === `Actual: ${speed.toFixed(2)}× (${Math.round(speed * 100)}%)`; })()"), 'badge uses measured speed instead of requested 8×');
    await evaluate("Module._set_speed(1); openSettings(); document.querySelector('#show-actual-speed').click(); document.querySelector('#settings-close').click()");
    assert.equal(await evaluate("document.querySelector('#actual-speed').hidden"), true);
    assert.equal(await evaluate("localStorage.getItem('rustboy_show_actual_speed')"), 'false');
    console.log(`${system}: actual speed measurement, pause label and toggle passed`);
  }
  await call('Page.navigate', {url: `http://127.0.0.1:${server.address().port}${basePath}/`});
  await until("typeof window.mem === 'function'");
  assert.equal(await evaluate("!!document.querySelector('#rustboy-canvas')"), true);
  assert.equal(await evaluate("document.querySelector('#rustboy-canvas').width"), 160);
  await evaluate("document.querySelector('#sgb-user-palette').value = 'warm'; document.querySelector('#sgb-user-palette').dispatchEvent(new Event('change')); document.querySelector('#sgb-user-palette').value = 'game'; document.querySelector('#sgb-user-palette').dispatchEvent(new Event('change'))");
  assert.equal(await evaluate("document.querySelector('#rom-picker #hardware-model') === null && document.querySelector('#emulator-settings #hardware-model') !== null"), true, 'advanced settings do not clutter the ROM picker');
  assert.equal(await evaluate("Array.from(document.querySelectorAll('#emulator-settings > details')).filter(el => el.open).length"), 1, 'only everyday appearance settings start expanded');
  await evaluate("openSettings(); window.settingsSelectionBefore = selectedIdx");
  assert.equal(await evaluate("document.querySelector('#emulator-settings').open"), true);
  await call('Input.dispatchKeyEvent', {type:'keyDown',key:'ArrowDown',code:'ArrowDown',windowsVirtualKeyCode:40});
  await call('Input.dispatchKeyEvent', {type:'keyUp',key:'ArrowDown',code:'ArrowDown',windowsVirtualKeyCode:40});
  assert.equal(await evaluate("selectedIdx === window.settingsSelectionBefore"), true, 'settings keyboard input does not navigate the library behind the dialog');
  await call('Input.dispatchKeyEvent', {type:'keyDown',key:'Escape',code:'Escape',windowsVirtualKeyCode:27});
  await call('Input.dispatchKeyEvent', {type:'keyUp',key:'Escape',code:'Escape',windowsVirtualKeyCode:27});
  await until("!document.querySelector('#emulator-settings').open");
  if (process.env.RUSTBOY_PUBLIC_LIBRARY) {
    await until("ROMS.length === 1");
    assert.deepEqual(await evaluate("Array.from(document.querySelectorAll('.rom-tab')).filter(el => getComputedStyle(el).display !== 'none').map(el => el.dataset.tab)"), ['homebrew']);
    assert.equal(await evaluate("getComputedStyle(document.querySelector('#local-hardware')).display"), 'none');
    assert.deepEqual(libraryRequests, [], 'public picker must not request private libraries');
  }
  if (process.env.RUSTBOY_GBA_PICKER && !process.env.RUSTBOY_PUBLIC_LIBRARY) {
    await until("ROM_TABS.gba.available && ROM_TABS.gb.available && ROM_TABS.gbc.available");
    assert.equal(libraryRequests.filter(name => name === '/roms/romlist.json').length, 1,
      'GB, GBC and GBA share one manifest request');
    assert.equal(await evaluate("getComputedStyle(document.querySelector('[data-tab=gba]')).display !== 'none'"), true);
    await evaluate("document.querySelector('[data-tab=gba]').click()");
    assert.equal(await evaluate("activeTab"), 'gba');
    assert.equal(await evaluate("document.querySelector('#rom-list .rom-name').textContent"), pickerName.replace(/\.gba$/i, ''));
    await evaluate("document.querySelector('#rom-list li').click()");
    await until("currentRomInfo?.tab === 'gba' && document.querySelector('#rustboy-canvas').width === 240");
    await until("(() => { const c=document.querySelector('#rustboy-canvas'); const p=c.getContext('2d').getImageData(239,159,1,1).data; return p[0]===255 && p[1]===0 && p[2]===0; })()");
    assert.equal(await evaluate("Module._get_state_id().startsWith('gba-')"), true);
    await evaluate("openPicker()");
    assert.equal(await evaluate("document.querySelector('#recent-list .recent-name').textContent"), pickerName.replace(/\.gba$/i, ''));
    await evaluate("document.querySelector('#recent-list .recent-item').click()");
    await until("document.querySelector('#rom-picker').classList.contains('hidden') && currentRomInfo?.tab === 'gba'");
    console.log('Local GBA picker: shared manifest, filtered tab, nested download, real WASM rendering and recent-game reopen passed');
  }
  await evaluate('initAudio()');
  await until('audioWorkletNode !== null');
  assert.equal(await evaluate('audioWorkletNode.numberOfOutputs'), 1);
  assert.equal(await evaluate('audioWorkletNode.channelCountMode'), 'max');
  const {root: document} = await call('DOM.getDocument');
  let {nodeId} = await call('DOM.querySelector', {nodeId: document.nodeId, selector: '#rom-file-input'});
  const invalidRom = path.join(temporary, 'truncated.gb');
  await fs.writeFile(invalidRom, new Uint8Array(8));
  await call('DOM.setFileInputFiles', {nodeId, files: [invalidRom]});
  await until("document.body.textContent.includes('ROM is truncated')");
  assert.deepEqual(exceptions, [], 'unhandled browser errors');
  if (process.env.RUSTBOY_GBA_CONTROLS) {
    const inputRom = new Uint8Array(32768);
    // Display KEYINPUT as a pixel, exercising browser input through the real GBA backend.
    const words = [0xe59f0018,0xe59f1018,0xe1c010b0,0xe59f2014,0xe59f3014,
      0xe1d210b0,0xe1c310b0,0xeafffffc,0x04000000,0x403,0x04000130,0x06000000];
    words.forEach((word,n) => new DataView(inputRom.buffer).setUint32(n*4,word,true));
    inputRom[0xb2]=0x96;
    const inputPath=path.join(temporary,'original-shoulder-input.gba');
    await fs.writeFile(inputPath,inputRom);
    await call('DOM.setFileInputFiles',{nodeId,files:[inputPath]});
    await until("controlConsole === 'gba' && !pickerOpen && !Module._is_paused()");
    assert.equal(await evaluate("document.querySelector('#panel-left [data-action=btn_l]').hidden"),false);
    assert.equal(await evaluate("document.querySelector('#panel-right [data-action=color_mode]').hidden"),true);
    const green = "document.querySelector('#rustboy-canvas').getContext('2d').getImageData(0,0,1,1).data[1]";
    await until(`${green} === 255`);
    const key = async (type,code) => call('Input.dispatchKeyEvent',{type,code,key:code.slice(3).toLowerCase()});
    await key('keyDown','KeyQ'); await until(`${green} === 123`);
    await key('keyUp','KeyQ'); await until(`${green} === 255`);
    await key('keyDown','KeyE'); await until(`${green} === 189`);
    await key('keyUp','KeyE'); await until(`${green} === 255`);
    await evaluate("openSettings(); document.querySelector('#settings-game-bindings').closest('details').open=true; document.querySelector('#settings-game-bindings [data-action=btn_l] kbd').click()");
    await key('keyDown','KeyT'); await key('keyUp','KeyT');
    assert.equal(await evaluate("bindings.btn_l"),'KeyT');
    assert.equal(await evaluate("document.querySelector('#panel-left [data-action=btn_l] kbd').textContent"),'T');
    await evaluate("document.querySelector('#settings-close').click()");
    await until('!Module._is_paused()');
    await key('keyDown','KeyT'); await until(`${green} === 123`);
    await key('keyUp','KeyT'); await until(`${green} === 255`);
    await call('Emulation.setDeviceMetricsOverride',{width:390,height:844,deviceScaleFactor:1,mobile:true});
    await call('Emulation.setTouchEmulationEnabled',{enabled:true,maxTouchPoints:5});
    await evaluate('renderTouchControls()');
    assert.ok(await evaluate("document.querySelector('[data-group=shoulder_l]').getBoundingClientRect().width > 0"),'GBA touch shoulder appears on mobile');
    const shoulderPoint=await evaluate("(() => { const r=document.querySelector('.touch-btn[data-action=btn_l]').getBoundingClientRect(); return {x:r.x+r.width/2,y:r.y+r.height/2}; })()");
    await call('Input.dispatchTouchEvent',{type:'touchStart',touchPoints:[shoulderPoint]});
    await until(`${green} === 123`);
    await call('Input.dispatchTouchEvent',{type:'touchEnd',touchPoints:[]});
    await until(`${green} === 255`);
    await evaluate("window.testPad={index:0,id:'Shoulder test',connected:true,mapping:'standard',axes:[0,0],buttons:Array.from({length:16},(_,i)=>({pressed:i===5}))}; navigator.getGamepads=()=>[window.testPad]; pollControllers()");
    await until(`${green} === 189`);
    await evaluate("window.testPad.buttons[5].pressed=false; pollControllers()");
    await until(`${green} === 255`);
    await evaluate("navigator.getGamepads=()=>[]; pollControllers(); Module._load_rom_data(new Uint8Array(32768),'original.gb')");
    await until("controlConsole === 'gb'");
    assert.equal(await evaluate("document.querySelector('#panel-left [data-action=btn_l]').hidden && document.querySelector('[data-group=shoulder_l]').hidden"),true,'GB hides shoulder controls');
    assert.equal(await evaluate("bindings.btn_l"),'KeyQ','GB keeps its own bindings');
    await call('DOM.setFileInputFiles',{nodeId,files:[inputPath]});
    await until("controlConsole === 'gba' && !Module._is_paused()");
    assert.equal(await evaluate("bindings.btn_l"),'KeyT','GBA binding restored when switching back');
    await evaluate('window.controlsReloadToken=true');
    await call('Page.reload');
    await until("typeof window.controlsReloadToken === 'undefined' && typeof Module?._load_rom_data === 'function'");
    const reloaded = await call('DOM.getDocument');
    const upload = await call('DOM.querySelector',{nodeId:reloaded.root.nodeId,selector:'#rom-file-input'});
    nodeId = upload.nodeId;
    await call('DOM.setFileInputFiles',{nodeId:upload.nodeId,files:[inputPath]});
    await until("controlConsole === 'gba'");
    assert.equal(await evaluate("bindings.btn_l"),'KeyT','GBA binding survives page reload');
    await evaluate("openSettings(); document.querySelector('#settings-reset-bindings').click(); document.querySelector('#settings-close').click()");
    assert.equal(await evaluate("bindings.btn_l"),'KeyQ');
    await call('Emulation.clearDeviceMetricsOverride');
    await call('Emulation.setTouchEmulationEnabled',{enabled:false});
    console.log('Console controls: GBA keyboard/touch/gamepad shoulders, Settings rebinding, console switching, persistence and reset passed');
  }
  if (process.env.RUSTBOY_GBA_UPLOAD) {
    const gba=new Uint8Array(32*1024*1024);
    const words=[0xe59f0020,0xe59f1020,0xe1c010b0,0xe59f201c,0xe59f301c,
      0xe3a0101f,0xe0c210b2,0xe2533001,0x1afffffc,0xeafffffe,
      0x04000000,0x403,0x06000000,38400];
    const view=new DataView(gba.buffer);words.forEach((word,n)=>view.setUint32(n*4,word,true));
    // After drawing, program one byte through the real Flash command bus.
    view.setUint32(9*4,0xea000000|((0x100-(9*4+8))>>2),true);
    const flash=[0xe59f0028,0xe59f1028,0xe3a020aa,0xe5c02000,
      0xe3a02055,0xe5c12000,0xe3a020a0,0xe5c02000,0xe59f0010,
      0xe3a0205a,0xe5c02000,0xeafffffe,0x0e005555,0x0e002aaa,0x0e001234];
    flash.forEach((word,n)=>view.setUint32(0x100+n*4,word,true));
    // Continue into an original pulse-channel program after Flash programming.
    // Nonzero browser audio must come from PSG, not host-injected FIFO samples.
    view.setUint32(0x100+11*4,0xea000000|((0x200-(0x100+11*4+8))>>2),true);
    const soundSettings=[[0x84,0x80],[0x80,0x2277],[0x82,2],[0x68,0xf080],[0x6c,0x8600]];
    const soundWords=[0];
    for (const [offset] of soundSettings) soundWords.push(0,0xe1c010b0|((offset&0xf0)<<4)|(offset&15));
    soundWords.push(0xeafffffe);
    soundWords[0]=0xe59f0000|(soundWords.length*4-8);soundWords.push(0x04000000);
    soundSettings.forEach(([,value],n)=>{
      const at=1+n*2;soundWords[at]=0xe59f1000|(soundWords.length*4-(at*4+8));soundWords.push(value);
    });
    soundWords.forEach((word,n)=>view.setUint32(0x200+n*4,word,true));
    gba.set(new TextEncoder().encode('FLASH1M_V103'),0x180);
    gba[0xb2]=0x96;gba.set(new TextEncoder().encode('RUSTBOY TEST'),0xa0);
    const gbaPath=path.join(temporary,'original-32MiB.gba');await fs.writeFile(gbaPath,gba);
    await evaluate("window.gbaPsgSamples=0; window.gbaPsgEnergy=0; window.gbaPsgFinite=true; window.gbaOriginalAudioQueue=queueAudioSamples; queueAudioSamples=(left,right,rate)=>{ for(const channel of [left,right]) for(const sample of channel) { window.gbaPsgSamples++; window.gbaPsgEnergy+=sample*sample; window.gbaPsgFinite &&=Number.isFinite(sample); } window.gbaOriginalAudioQueue(left,right,rate); }");
    await call('DOM.setFileInputFiles',{nodeId,files:[gbaPath]});
    await until("document.querySelector('#rustboy-canvas').width === 240");
    await until("(() => { const c=document.querySelector('#rustboy-canvas'); const p=c.getContext('2d').getImageData(239,159,1,1).data; return p[0]===255 && p[1]===0 && p[2]===0; })()");
    assert.equal(await evaluate("document.querySelector('#rustboy-canvas').height"),160);
    await until("window.gbaPsgSamples > 2000 && window.gbaPsgEnergy/window.gbaPsgSamples > 0.001");
    await checkActualSpeed('GBA');
    assert.equal(await evaluate("window.gbaPsgFinite"),true,'PSG PCM reaches the browser stereo queue');
    await evaluate("queueAudioSamples=window.gbaOriginalAudioQueue");
    assert.ok(await evaluate("Module._get_state_id().startsWith('gba-')"));
    assert.equal(await evaluate("document.querySelector('#backend-status').hidden"),false);
    await evaluate("window.gbaUploadedId=Module._get_state_id(); Module._set_paused(true); window.gbaState=Module._export_state(); Module._import_state(gbaState)");
    assert.ok(await evaluate("gbaState.length > 500000 && Module._get_state_id()===gbaUploadedId"));
    assert.ok(await evaluate("Module._get_save_key().startsWith('rustboy_save_gba_') && Module._export_save_data().length===131072 && Module._export_save_data()[0x1234]===0x5a"));
    await evaluate("(async () => { const wasm=await import('./out/rustboy.js'); wasm.save_game(); window.gbaSaveKey=Module._get_save_key(); window.gbaPersisted=localStorage.getItem(gbaSaveKey); })()");
    assert.ok(await evaluate("gbaPersisted?.length > 100000"));
    await evaluate("localStorage.setItem('rustboy_auto_resume','0'); window.gbaSavedBytes=Module._export_save_data(); gbaSavedBytes[0x1234]=0x33; Module._import_save_data(gbaSavedBytes)");
    await call('DOM.setFileInputFiles',{nodeId,files:[gbaPath]});
    await until("Module._export_save_data()[0x1234]===0x5a");
    assert.ok(await evaluate("Module._get_save_key()===gbaSaveKey && localStorage.getItem(gbaSaveKey)===gbaPersisted"));
    // Model the previous HLE BIOS identity without weakening binary validation.
    // A fresh page must find it, import the checked state, and retain the source.
    const legacyState = await evaluate(`(async () => {
      Module._set_paused(true);
      const ids=compatibleStateIds();
      if(ids.length!==2) throw Error('Expected current and previous HLE firmware identities');
      const bytes=Module._export_state(), view=new DataView(bytes.buffer,bytes.byteOffset,bytes.byteLength);
      view.setBigUint64(16,BigInt('0x'+ids[1].split('-')[2]),true);
      view.setUint32(40,0x1234abcd,true);
      let sum=0xcbf29ce484222325n;
      for(const byte of bytes.subarray(0,-8)) sum=BigInt.asUintN(64,(sum^BigInt(byte))*0x100000001b3n);
      view.setBigUint64(bytes.length-8,sum,true);
      const record={key:ids[1]+':auto',romId:ids[1],slot:'auto',timestamp:1234567890000,data:bytes.slice().buffer};
      await stateRecord('put',record); await stateRecord('delete',ids[0]+':auto');
      localStorage.setItem('rustboy_auto_resume','1');
      blockedAutosaveId=ids[0]; // Preserve the fixture through pagehide autosave.
      window.saveReloadToken=true;
      return {key:record.key,size:bytes.length,batteryKey:Module._get_save_key()};
    })()`);
    await call('Page.reload');
    await until("typeof window.saveReloadToken === 'undefined' && typeof Module !== 'undefined' && typeof Module?._load_rom_data === 'function'");
    const savedDocument=await call('DOM.getDocument');
    nodeId=(await call('DOM.querySelector',{nodeId:savedDocument.root.nodeId,selector:'#rom-file-input'})).nodeId;
    await call('DOM.setFileInputFiles',{nodeId,files:[gbaPath]});
    await until("controlConsole === 'gba' && !pickerOpen && Module._is_paused()");
    assert.equal(await evaluate("new DataView(Module._export_state().buffer).getUint32(40,true)"),0x1234abcd,'legacy exact-position autosave restored after a cold reload');
    assert.equal(await evaluate(`(async () => (await stateRecord('get',${JSON.stringify(legacyState.key)})).data.byteLength)()`),legacyState.size);
    assert.equal(await evaluate('Module._get_save_key()'),legacyState.batteryKey,'cartridge persistence key survives BIOS identity migration');
    await evaluate('openSaveManager()');
    assert.ok(await evaluate("document.querySelector('#battery-save-status').textContent.includes('In-game save available')"));
    assert.notEqual(await evaluate("document.querySelector('#state-slots small').textContent"),'Empty');
    await evaluate('closeSaveManager(); window.gbaUploadedId=Module._get_state_id()');
    const preserved = await evaluate(`(async () => {
      const key=currentStateId()+':auto', bad=new Uint8Array([1,2,3]);
      await stateRecord('put',{key,romId:currentStateId(),slot:'auto',timestamp:Date.now(),data:bad.buffer});
      const restored=await loadState('auto',true), saved=await saveState('auto',true);
      const record=await stateRecord('get',key);
      await stateRecord('delete',key); blockedAutosaveId='';
      return {restored,saved,data:Array.from(new Uint8Array(record.data))};
    })()`);
    assert.deepEqual(preserved,{restored:false,saved:false,data:[1,2,3]},'failed restore preserves the original autosave instead of overwriting it');
    console.log('Save recovery: old BIOS identity resumes after reload, battery save is visible, rejected autosaves remain intact');
    const speedChecks = await evaluate(`(async () => {
      Module._set_paused(true);
      const baseline = Module._export_state();
      const readings = [];
      for (const requested of [1, 4]) {
        Module._import_state(baseline); Module._set_speed(requested);
        const before = Number(new DataView(baseline.buffer, baseline.byteOffset).getBigUint64(301, true));
        const start = performance.now(); Module._set_paused(false);
        await new Promise(resolve => {
          let frames=0; requestAnimationFrame(function sample() {
            if (++frames < 60) requestAnimationFrame(sample); else resolve();
          });
        });
        Module._set_paused(true); const elapsed = performance.now()-start;
        const state = Module._export_state();
        const after = Number(new DataView(state.buffer, state.byteOffset).getBigUint64(301, true));
        readings.push((after-before)/16777216/(elapsed/1000));
      }
      Module._set_speed(1); return readings;
    })()`);
    assert.ok(speedChecks[1] > speedChecks[0] * 1.2,
      `fast-forward must advance more emulated time, not just change the badge: ${speedChecks}`);
    const oversized=path.join(temporary,'oversized.gba');const file=await fs.open(oversized,'w');await file.truncate(gba.length+1);await file.close();
    await call('DOM.setFileInputFiles',{nodeId,files:[oversized]});
    await until("document.body.textContent.includes('Could not read ROM: ROM size must')");
    assert.equal(await evaluate("Module._get_state_id()===gbaUploadedId"),true);
    console.log('GBA browser: 32 MiB upload, real WASM bitmap/PSG audio, Flash persistence, checked states, measured fast-forward and oversized-load preservation passed', speedChecks);
    // Differently colored neighboring pixels catch a zero affine matrix that
    // the preceding uniform-red bitmap could not detect. Every fixture writes
    // its registers/data through executable ARM stores, without a BIOS SWI.
    for (let mode=0;mode<6;mode++) {
      const settings=[[0x05000000,0x7fff],[0x05000002,31],[0x05000004,31<<5],[0x05000006,31<<10]];
      const layer=mode===0?0x100:mode===2?0x800:0x400;
      if (mode<=2) {
        settings.push([mode===0?0x04000008:mode===1?0x0400000c:0x0400000e,mode===0?0x1c80:0x1c00],
          [0x06000000,0x0201],[0x06000008,3]);
      } else if (mode===4) settings.push([0x06000000,0x0201],[0x060000f0,3]);
      else settings.push([0x06000000,31],[0x06000002,31<<5],[0x06000000+(mode===5?320:480),31<<10]);
      settings.push([0x04000000,layer|mode]);
      const words=[];
      for (const _ of settings) words.push(0,0,0xe1c010b0);
      words.push(0xeafffffe);
      settings.forEach(([address,value],n)=>{
        const at=n*3;
        words[at]=0xe59f0000|(words.length*4-(at*4+8));words.push(address);
        words[at+1]=0xe59f1000|(words.length*4-((at+1)*4+8));words.push(value);
      });
      const fixture=new Uint8Array(2048);const image=new DataView(fixture.buffer);
      words.forEach((word,n)=>image.setUint32(n*4,word,true));fixture[0xb2]=0x96;
      const file=path.join(temporary,`original-mode-${mode}.gba`);await fs.writeFile(file,fixture);
      const previous=await evaluate("Module._get_state_id()");
      await call('DOM.setFileInputFiles',{nodeId,files:[file]});
      await until(`Module._get_state_id()!==${JSON.stringify(previous)} && Module._get_state_id().startsWith('gba-')`);
      await until("(() => { const s=Module._export_state(); return new DataView(s.buffer,s.byteOffset).getBigUint64(309,true)>=2n; })()");
      assert.deepEqual(await evaluate("(() => { const c=document.querySelector('#rustboy-canvas').getContext('2d'); return [[0,0],[1,0],[0,1]].map(([x,y])=>Array.from(c.getImageData(x,y,1,1).data)); })()"),
        [[255,0,0,255],[0,255,0,255],[0,0,255,255]],`mode ${mode} preserves distinct source coordinates`);
    }
    console.log('GBA browser: patterned CPU-driven rendering in all six display modes passed');
    for (const prefix of ['A','F']) {
      // Original cartridge probe: execute in a ROM mirror, compare low/high
      // chip addresses, then keep painting green for equal data, red otherwise.
      // Normal boards execute at the base and keep their unmapped ROM values.
      const code=[], literals=[];
      const literal=(reg,value)=>{literals.push({at:code.length,reg,value});code.push(0);};
      literal(0,0x04000000);literal(6,0x403);code.push(0xe1c060b0);
      literal(1,0x08000200);literal(2,0x08300200);literal(3,0x06000000);
      const loop=code.length;
      code.push(0xe5d14000,0xe5d25000,0xe1540005,0xe3a0601f,0x03a06e3e,0xe1c360b0);
      code.push(0xea000000|((loop-code.length-2)&0xffffff));
      for(const {at,reg,value} of literals) {
        code[at]=0xe59f0000|(reg<<12)|(code.length*4-(at*4+8));code.push(value);
      }
      const fixture=new Uint8Array(0x100000),view=new DataView(fixture.buffer);
      view.setUint32(0,prefix==='F'?0xea0c003e:0xea00003e,true);
      fixture[0xac]=prefix.charCodeAt(0);fixture[0xb2]=0x96;fixture[0x200]=0x7b;
      code.forEach((word,n)=>view.setUint32(0x100+n*4,word,true));
      const file=path.join(temporary,`original-cartridge-mapping-${prefix}.gba`);
      await fs.writeFile(file,fixture);
      const previous=await evaluate('Module._get_state_id()');
      await call('DOM.setFileInputFiles',{nodeId,files:[file]});
      await until(`Module._get_state_id()!==${JSON.stringify(previous)} && (() => { const s=Module._export_state(); return new DataView(s.buffer,s.byteOffset).getBigUint64(309,true)>=2n; })()`);
      assert.deepEqual(await evaluate("Array.from(document.querySelector('#rustboy-canvas').getContext('2d').getImageData(0,0,1,1).data)"),prefix==='F'?[0,255,0,255]:[255,0,0,255]);
      const comparison=await evaluate(`(async()=>{
        const wasm=await import('./out/rustboy.js'); Module._set_paused(true);
        const original=Module._export_state();const states=[];const stats=[];
        wasm.set_gba_full_jit_enabled(true);
        for(const enabled of [false,true]) {
          Module._import_state(original);Module._set_paused(true);wasm.set_gba_jit_enabled(enabled);
          wasm.benchmark_gba_clocks(280896*4);states.push(Module._export_state());stats.push(wasm.get_gba_jit_statistics());
        }
        wasm.set_gba_full_jit_enabled(false);wasm.set_gba_jit_enabled(true);Module._set_paused(false);
        return {same:states[0].length===states[1].length&&states[0].every((v,n)=>v===states[1][n]),stats};
      })()`);
      assert.equal(comparison.same,true,`cartridge ${prefix} mapping matches interpreter/JIT state`);
      assert.match(comparison.stats[1],/instructions=[1-9]/,'mirrored ROM executes compiled code');
    }
    console.log('GBA browser: original mirrored-cartridge and normal-ROM-hole fixtures match interpreter/JIT snapshots');
    {
      // This original VRAM program overwrites its third instruction before
      // execution. ARM7 has fetched it already, so the original MOV wins.
      const code=[], literals=[];
      const literal=(reg,value)=>{literals.push({at:code.length,reg,value});code.push(0);};
      literal(3,0x06002000);
      for(const [offset,word] of [[0,0xe5810000],[4,0xe3a0500b],[8,0xe3a06011],[12,0xe12fff12]]) {
        literal(4,word);code.push(0xe5834000|offset);
      }
      literal(0,0xe3a0602a);code.push(0xe2831008);
      const returnAddress=code.length;code.push(0,0xe12fff13);
      code[returnAddress]=0xe28f2000; // ADR r2 points past the following BX.
      literal(4,0x06000000);code.push(0xe5846000);
      literal(4,0x04000000);literal(1,0x403);code.push(0xe1c410b0,0xeafffffe);
      for(const {at,reg,value} of literals) {
        code[at]=0xe59f0000|(reg<<12)|(code.length*4-(at*4+8));code.push(value);
      }
      const fixture=new Uint8Array(512), view=new DataView(fixture.buffer);
      view.setUint32(0,0xea00003e,true);fixture[0xb2]=0x96;
      code.forEach((word,n)=>view.setUint32(0x100+n*4,word,true));
      const file=path.join(temporary,'original-video-pipeline.gba');await fs.writeFile(file,fixture);
      const previous=await evaluate('Module._get_state_id()');
      await call('DOM.setFileInputFiles',{nodeId,files:[file]});
      await until(`Module._get_state_id()!==${JSON.stringify(previous)} && (() => { const s=Module._export_state(); return new DataView(s.buffer,s.byteOffset).getBigUint64(309,true)>=2n; })()`);
      assert.deepEqual(await evaluate("Array.from(document.querySelector('#rustboy-canvas').getContext('2d').getImageData(0,0,1,1).data)"),[140,0,0,255],'CPU executes fetched VRAM opcode after a self-modifying store');
      console.log('GBA browser: original self-modifying VRAM program retains its fetched instruction');
    }
    // Original ARM program checks sound math and BIOS identification through
    // real SWIs. Copy the results to VRAM so snapshot layout changes do not
    // silently turn these assertions into reads of unrelated state.
    {
      const fixture=new Uint8Array(512), image=new DataView(fixture.buffer);
      const words=[0xe59f0030,0xe3a0103c,0xe3a02000,0xef1f0000,
        0xe1a05000,0xef0d0000,0xe59f401c,0xe5845000,0xe5840004,
        0xe59f4014,0xe59f1014,0xe1c410b0,0xeafffffe,
        0,0x08000180,0x06000000,0x04000000,0x403];
      words.forEach((word,n)=>image.setUint32(n*4,word,true));fixture[0xb2]=0x96;
      image.setUint32(0x180,0xdeadbeef,true);image.setUint32(0x184,0x00400000,true);
      const file=path.join(temporary,'original-sound-bios.gba');await fs.writeFile(file,fixture);
      const previous=await evaluate('Module._get_state_id()');
      await call('DOM.setFileInputFiles',{nodeId,files:[file]});
      await until(`Module._get_state_id()!==${JSON.stringify(previous)} && (() => { const s=Module._export_state(); return new DataView(s.buffer,s.byteOffset).getBigUint64(309,true)>=2n; })()`);
      const results=await evaluate(`(() => {
        const s=Module._export_state(), view=new DataView(s.buffer,s.byteOffset,s.byteLength);
        if(s[4]!==14)throw new Error('Update BIOS fixture snapshot layout');
        const vram=567+view.getUint32(563,true)*8+0x40000+0x8000+0x400;
        return [view.getUint32(vram,true),view.getUint32(vram+4,true)];
      })()`);
      assert.deepEqual(results,[4096,0xbaae187f],'MidiKey2Freq reads WaveData +4; GetBiosChecksum identifies a GBA');
      console.log('GBA browser: original SWI cartridge verifies instrument frequency and BIOS checksum');
    }
    // Two CPU-programmed HBlank DMAs reproduce the scroll-register idiom used
    // by Phantasy Star. A zero DMA open-bus value leaves this pixel red.
    {
      const settings=[[0x05000000,31<<16],[0x05000004,31<<5],[0x04000008,0x100],
        [0x06000840,0x00010001],[0x03000100,8],
        [0x040000bc,0x03000100],[0x040000c0,0x04000016],[0x040000c4,0xa3400001],
        [0x040000c8,0x04000016],[0x040000cc,0x04000012],[0x040000d0,0xa3400001],
        [0x04000000,0x100]];
      for(let row=0;row<8;row++) settings.unshift([0x06000000+row*4,0x11111111],[0x06000020+row*4,0x22222222]);
      const words=[];
      for(const _ of settings) words.push(0,0,0xe5801000);
      words.push(0xeafffffe);
      settings.forEach(([address,value],n)=>{
        const at=n*3;
        words[at]=0xe59f0000|(words.length*4-(at*4+8)); words.push(address);
        words[at+1]=0xe59f1000|(words.length*4-((at+1)*4+8)); words.push(value);
      });
      const fixture=new Uint8Array(32768), view=new DataView(fixture.buffer);
      view.setUint32(0,0xea00003e,true); fixture[0xb2]=0x96;
      words.forEach((word,n)=>view.setUint32(0x100+n*4,word,true));
      const file=path.join(temporary,'original-dma-scroll-latch.gba'); await fs.writeFile(file,fixture);
      const previous=await evaluate('Module._get_state_id()');
      await call('DOM.setFileInputFiles',{nodeId,files:[file]});
      await until(`Module._get_state_id()!==${JSON.stringify(previous)} && (()=>{const p=document.querySelector('#rustboy-canvas').getContext('2d').getImageData(0,1,1,1).data; return p[0]===0 && p[1]===255;})()`);
      assert.equal(await evaluate(`(()=>{Module._set_paused(true);const s=Module._export_state();Module._import_state(s);return new DataView(Module._export_state().buffer).getUint32(s.length-12,true);})()`),0x00080008);
      await evaluate('Module._set_paused(false)');
      console.log('GBA browser: chained HBlank DMAs retain write-only scroll data across channels and state restore');
    }
    const dataServices=[
      [0x10,[0xa5],[3,3,0x30,0x30]],
      [0x11,[0x10,4,0,0,0,0xe0,3,31,0],[0xe0,3,31,0]],
      [0x12,[0x10,4,0,0,0,0xe0,3,31,0],[0xe0,3,31,0]],
      [0x13,[0x28,4,0,0,3,0x80,102,0xc0,72,117,0,0,0,0,0,0xb0],[72,117,102,102]],
      [0x14,[0x30,4,0,0,3,0xe0,3,31,0],[0xe0,3,31,0]],
      [0x15,[0x30,4,0,0,3,0xe0,3,31,0],[0xe0,3,31,0]],
      [0x16,[0x81,4,0,0,250,10,255,2],[250,4,3,5]],
      [0x17,[0x81,4,0,0,250,10,255,2],[250,4,3,5]],
      [0x18,[0x82,8,0,0,255,255,2,0,255,255,0,128],[255,255,1,0]],
    ];
    for (const [service,data,expected] of dataServices) {
      const fixture=new Uint8Array(512);const image=new DataView(fixture.buffer);
      // Byte-writing services target WRAM; copy their result to video with LDR/STR.
      const writesBytes=[0x11,0x14,0x16].includes(service);
      const destination=writesBytes?0x03000000:0x06000000;
      const words=[0xe59f0018,0xe59f1018,0xe59f2018,0xef000000|(service<<16),
        0xe59f0014,0xe59f1014,0xe1c010b0,0xea000005,
        0x08000080,destination,0x080000a0,0x04000000,0x403,0];
      if (writesBytes) words.push(0xe59f0010,0xe5902000,0xe59f100c,0xe5812000,0xeafffffe,0,0x03000000,0x06000000);
      else words.push(0xeafffffe);
      // Branch at 1Ch lands at the continuation at 38h.
      if ([0x11,0x12,0x14,0x15].includes(service)) {
        // A null asset must return normally before the valid stream is decoded.
        // Prepending shifts code and its literals together, preserving offsets.
        words.unshift(0xe3a00000,0xe3a01403,0xef000000|(service<<16));
      }
      words.forEach((word,n)=>image.setUint32(n*4,word,true));
      fixture.set(data,128);fixture.set([1,0,1,4,2,0,0,0],160);fixture[0xb2]=0x96;
      const file=path.join(temporary,`original-swi-${service.toString(16)}.gba`);await fs.writeFile(file,fixture);
      const previous=await evaluate('Module._get_state_id()');
      await call('DOM.setFileInputFiles',{nodeId,files:[file]});
      await until(`Module._get_state_id()!==${JSON.stringify(previous)} && Module._get_state_id().startsWith('gba-')`);
      await until('(() => { const s=Module._export_state(); return new DataView(s.buffer,s.byteOffset).getBigUint64(309,true)>=2n; })()');
      const pixels=[];
      for (let n=0;n<4;n+=2) {
        const color=expected[n]|expected[n+1]<<8;
        pixels.push([0,5,10].map(shift=>{const v=(color>>shift)&31;return (v<<3)|(v>>2);}).concat(255));
      }
      assert.deepEqual(await evaluate("(() => { const c=document.querySelector('#rustboy-canvas').getContext('2d'); return [0,1].map(x=>Array.from(c.getImageData(x,0,1,1).data)); })()"),pixels,`SWI ${service.toString(16)} output reaches display`);
    }
    console.log('GBA browser: nine original BIOS data services render their output; null LZ/RL sources return before valid streams');
    {
      const settings=[[0x04000200,0x1000],[0x04000132,0x4001]];
      const words=[];
      for (const _ of settings) words.push(0,0,0xe1c010b0);
      words.push(0xef030000,0xe3a04077,0xeafffffe);
      settings.forEach(([address,value],n)=>{
        const at=n*3;
        words[at]=0xe59f0000|(words.length*4-(at*4+8));words.push(address);
        words[at+1]=0xe59f1000|(words.length*4-((at+1)*4+8));words.push(value);
      });
      const fixture=new Uint8Array(512);const image=new DataView(fixture.buffer);
      words.forEach((word,n)=>image.setUint32(n*4,word,true));fixture[0xb2]=0x96;
      const file=path.join(temporary,'original-stop.gba');await fs.writeFile(file,fixture);
      const previous=await evaluate('Module._get_state_id()');
      await call('DOM.setFileInputFiles',{nodeId,files:[file]});
      await until(`Module._get_state_id()!==${JSON.stringify(previous)} && Module._get_state_id().startsWith('gba-')`);
      await until('Module._export_state()[333]===2');
      await evaluate('window.stoppedGbaState=Module._export_state(); Module._import_state(stoppedGbaState)');
      await new Promise(resolve=>setTimeout(resolve,150));
      assert.equal(await evaluate('(() => { const s=Module._export_state(); return s[4]===14 && s[333]===2 && s.every((byte,n)=>byte===stoppedGbaState[n]); })()'),true,'STOP freezes all serialized clocks/state across browser callbacks and restore');
      await call('Input.dispatchKeyEvent',{type:'keyDown',key:'k',code:'KeyK'});
      await until('(() => { const s=Module._export_state(); return s[333]===0 && new DataView(s.buffer,s.byteOffset).getUint32(40,true)===0x77; })()');
      await call('Input.dispatchKeyEvent',{type:'keyUp',key:'k',code:'KeyK'});
      console.log('GBA browser: STOP freezes hardware, replays state and wakes through keyboard keypad IRQ');
    }
    {
      // Our IRQ shim has a nonzero PUSH opcode at BIOS 100h. Cartridge LDR
      // must see the initial protected latch (zero), not those firmware bytes.
      const words=[0xe59f0018,0xe5902000,0xe59f1018,0xe5812000,
        0xe59f0014,0xe59f1014,0xe1c010b0,0xeafffffe,
        0x100,0,0x06000000,0x04000000,0x403];
      const fixture=new Uint8Array(512);const image=new DataView(fixture.buffer);
      words.forEach((word,n)=>image.setUint32(n*4,word,true));fixture[0xb2]=0x96;
      const file=path.join(temporary,'original-bios-protection.gba');await fs.writeFile(file,fixture);
      const previous=await evaluate('Module._get_state_id()');
      await call('DOM.setFileInputFiles',{nodeId,files:[file]});
      await until(`Module._get_state_id()!==${JSON.stringify(previous)} && Module._get_state_id().startsWith('gba-')`);
      await until('(() => { const s=Module._export_state(); return new DataView(s.buffer,s.byteOffset).getBigUint64(309,true)>=2n; })()');
      assert.equal(await evaluate('(() => { const s=Module._export_state(); return new DataView(s.buffer,s.byteOffset).getUint32(32,true); })()'),0,'cartridge BIOS read is protected');
      assert.deepEqual(await evaluate("Array.from(document.querySelector('#rustboy-canvas').getContext('2d').getImageData(0,0,1,1).data)"),[0,0,0,255]);
      console.log('GBA browser: protected BIOS load executes through CPU/WASM and renders the latch instead of firmware');
    }
    for (const [service,x,y,expected] of [
      [9,0x4000,0,0x2000],[9,-0x4000,0,(-0x2000)>>>0],
      [10,-0x4000,0x4000,0x6000],[10,0,-0x4000,0xc000],
    ]) {
      const words=[0xe59f001c,0xe59f101c,0xef000000|(service<<16),0xe59f1018,
        0xe1c100b0,0xe59f1014,0xe59f2014,0xe1c120b0,0xeafffffe,
        x>>>0,y>>>0,0x06000000,0x04000000,0x403];
      const fixture=new Uint8Array(512);const image=new DataView(fixture.buffer);
      words.forEach((word,n)=>image.setUint32(n*4,word,true));fixture[0xb2]=0x96;
      const file=path.join(temporary,`original-angle-${service}-${x}-${y}.gba`);await fs.writeFile(file,fixture);
      const previous=await evaluate('Module._get_state_id()');
      await call('DOM.setFileInputFiles',{nodeId,files:[file]});
      await until(`Module._get_state_id()!==${JSON.stringify(previous)} && Module._get_state_id().startsWith('gba-')`);
      await until('(() => { const s=Module._export_state(); return new DataView(s.buffer,s.byteOffset).getBigUint64(309,true)>=2n; })()');
      assert.equal(await evaluate('(() => { const s=Module._export_state(); return new DataView(s.buffer,s.byteOffset).getUint32(24,true); })()'),expected);
      const pixel=[0,5,10].map(shift=>{const v=(expected>>>shift)&31;return (v<<3)|(v>>2);}).concat(255);
      assert.deepEqual(await evaluate("Array.from(document.querySelector('#rustboy-canvas').getContext('2d').getImageData(0,0,1,1).data)"),pixel,'angle SWI result is rendered by cartridge');
    }
    console.log('GBA browser: original ArcTan/ArcTan2 cartridges return signed/quadrant results and render them');
    {
      const settings=[[0x04000000,0x403],[0x040000d4,0x08001000],
        [0x040000d8,0x06000000],[0x040000dc,0xb20000f0]];
      const words=[];
      for (const _ of settings) words.push(0,0,0xe5801000);
      words.push(0xeafffffe);
      settings.forEach(([address,value],n)=>{
        const at=n*3;
        words[at]=0xe59f0000|(words.length*4-(at*4+8));words.push(address);
        words[at+1]=0xe59f1000|(words.length*4-((at+1)*4+8));words.push(value);
      });
      const fixture=new Uint8Array(0x14000);const image=new DataView(fixture.buffer);
      words.forEach((word,n)=>image.setUint32(n*4,word,true));fixture[0xb2]=0x96;
      for(let y=0;y<160;y++) for(let x=0;x<240;x++)
        image.setUint16(0x1000+(y*240+x)*2,((y&31)<<10)|(x&31),true);
      const file=path.join(temporary,'original-video-capture.gba');await fs.writeFile(file,fixture);
      const previous=await evaluate('Module._get_state_id()');
      await call('DOM.setFileInputFiles',{nodeId,files:[file]});
      await until(`Module._get_state_id()!==${JSON.stringify(previous)} && Module._get_state_id().startsWith('gba-')`);
      await until('(() => { const s=Module._export_state(); return new DataView(s.buffer,s.byteOffset).getBigUint64(309,true)>=2n; })()');
      assert.deepEqual(await evaluate("(() => { const c=document.querySelector('#rustboy-canvas').getContext('2d'); return [[1,0],[0,1],[239,159]].map(([x,y])=>Array.from(c.getImageData(x,y,1,1).data)); })()"),
        [[8,0,0,255],[0,0,8,255],[123,0,255,255]],'DMA3 capture renders distinct pixels, including the completed last row');
      console.log('GBA browser: original cartridge captures a complete bitmap through 160 scheduled DMA3 bursts');
    }
    for (const hblankFree of [false,true]) {
      const settings=[];
      for(let n=0;n<128;n++) settings.push([0x07000000+n*8,n<15?0:0x200]);
      for(let n=0;n<15;n++) settings.push([0x07000002+n*8,n<14?0xc0f0:0xc000]);
      for(let tile=0;tile<8;tile++) settings.push([0x06010000+tile*32,0x1111],[0x06010002+tile*32,0x1111]);
      settings.push([0x05000202,31],[0x04000000,0x1000|(hblankFree?0x20:0)]);
      const words=[];
      for(const _ of settings) words.push(0,0,0xe1c010b0);
      words.push(0xeafffffe);
      settings.forEach(([address,value],n)=>{
        const at=n*3;
        words[at]=0xe59f0000|(words.length*4-(at*4+8));words.push(address);
        words[at+1]=0xe59f1000|(words.length*4-((at+1)*4+8));words.push(value);
      });
      const fixture=new Uint8Array(4096);const image=new DataView(fixture.buffer);
      image.setUint32(0,0xea00003e,true); // Branch past cartridge header to 100h.
      fixture[0xb2]=0x96;words.forEach((word,n)=>image.setUint32(0x100+n*4,word,true));
      const file=path.join(temporary,`original-obj-budget-${hblankFree}.gba`);await fs.writeFile(file,fixture);
      const previous=await evaluate('Module._get_state_id()');
      await call('DOM.setFileInputFiles',{nodeId,files:[file]});
      await until(`Module._get_state_id()!==${JSON.stringify(previous)} && Module._get_state_id().startsWith('gba-')`);
      await until('(() => { const s=Module._export_state(); return new DataView(s.buffer,s.byteOffset).getBigUint64(309,true)>=2n; })()');
      assert.deepEqual(await evaluate("(() => { const c=document.querySelector('#rustboy-canvas').getContext('2d'); return [57,58,63].map(x=>Array.from(c.getImageData(x,0,1,1).data)); })()"),
        [[255,0,0,255],hblankFree?[0,0,0,255]:[255,0,0,255],hblankFree?[0,0,0,255]:[255,0,0,255]],'offscreen OBJ work consumes HBlank-free rendering budget');
    }
    console.log('GBA browser: CPU-driven sprite crowd exercises both OBJ budgets and partial-object clipping');
    // An original ARM cartridge enables WAITCNT prefetch and supplies four
    // internal multiply clocks per instruction, exercising queued/partial state.
    const prefetchRom=new Uint8Array(512), prefetchView=new DataView(prefetchRom.buffer);
    prefetchView.setUint32(0,0xea00003e,true);prefetchRom[0xb2]=0x96;
    [0xe59f001c,0xe59f101c,0xe1c010b0,0xe3a00001,0xe3a01401,
      0xe0000190,0xe0000190,0xe0000190,0xeafffffb,0x04000204,0x4000]
      .forEach((word,n)=>prefetchView.setUint32(0x100+n*4,word,true));
    const prefetchPath=path.join(temporary,'original-prefetch.gba');await fs.writeFile(prefetchPath,prefetchRom);
    await evaluate('window.beforePrefetchId=Module._get_state_id()');
    await call('DOM.setFileInputFiles',{nodeId,files:[prefetchPath]});
    await until("Module._get_state_id()!==beforePrefetchId && (()=>{ const s=Module._export_state(); return new DataView(s.buffer,s.byteOffset,s.byteLength).getBigUint64(309,true)>=2n; })()");
    assert.equal(await evaluate(`(() => {
      Module._set_paused(true);
      const s=Module._export_state(), at=s.length-45;
      const head=new DataView(s.buffer,s.byteOffset,s.byteLength).getUint32(at,true);
      Module._import_state(s);
      return s[4]===14 && head>=0x08000100 && head<0x08000200 && s[at+4]<=8 && s[at+5]<3
        && Module._export_state().every((byte,n)=>byte===s[n]);
    })()`),true,'CPU-enabled cartridge prefetch state round-trips through WASM');
    console.log('GBA browser: original WAITCNT-enabled multiply cartridge executes and replays prefetch state');
    // LDR from unused memory must see the opcode two ARM instructions ahead,
    // not zero. Store its low halfword into Mode 3 to check the real CPU bus.
    for (const openAddress of [0x10000000,0x04000010,0x040000e0,0x040000b0,0x04000206]) {
      const openRom=new Uint8Array(512), openView=new DataView(openRom.buffer);
      openView.setUint32(0,0xea00003e,true);openRom[0xb2]=0x96;
      [0xe59f001c,0xe59f101c,0xe1c010b0,0xe59f0018,0xe59f1018,
        0xe5912000,0xe1c020b0,0xe3a0401f,0xeafffffe,
        0x04000000,0x403,0x06000000,openAddress]
        .forEach((word,n)=>openView.setUint32(0x100+n*4,word,true));
      const openPath=path.join(temporary,`original-open-bus-${openAddress.toString(16)}.gba`);await fs.writeFile(openPath,openRom);
      await evaluate('window.beforeOpenId=Module._get_state_id()');
      await call('DOM.setFileInputFiles',{nodeId,files:[openPath]});
      await until("Module._get_state_id()!==beforeOpenId && (()=>{ const s=Module._export_state(); return new DataView(s.buffer,s.byteOffset,s.byteLength).getBigUint64(309,true)>=2n; })()");
      const expected=openAddress===0x04000206?[0,0,0,255]:[255,0,132,255];
      assert.deepEqual(await evaluate("Array.from(document.querySelector('#rustboy-canvas').getContext('2d').getImageData(0,0,1,1).data)"),expected, `CPU open/zero bus read at ${openAddress.toString(16)} renders correct bits`);
    }
    console.log('GBA browser: original unmapped/write-only I/O cartridges render ARM prefetch and mixed-word zero lanes');
    for (const swap of [0,1]) {
      const rom=new Uint8Array(512), view=new DataView(rom.buffer);
      view.setUint32(0,0xea00003e,true);rom[0xb2]=0x96;
      [0xe59f0014,0xe59f1014,0xe5801000,0xe59f0010,0xe59f1010,
        0xe5801000,0xeafffffe,0x04000000,0x403|(swap<<16),
        0x06000000,0x7fe0001f]
        .forEach((word,n)=>view.setUint32(0x100+n*4,word,true));
      const file=path.join(temporary,`original-green-swap-${swap}.gba`);await fs.writeFile(file,rom);
      await evaluate('window.beforeGreenId=Module._get_state_id()');
      await call('DOM.setFileInputFiles',{nodeId,files:[file]});
      await until("Module._get_state_id()!==beforeGreenId && (()=>{ const s=Module._export_state(); return new DataView(s.buffer,s.byteOffset,s.byteLength).getBigUint64(309,true)>=2n; })()");
      const expected=swap?[255,255,0,255,0,0,255,255]:[255,0,0,255,0,255,255,255];
      assert.deepEqual(await evaluate("Array.from(document.querySelector('#rustboy-canvas').getContext('2d').getImageData(0,0,2,1).data)"),expected,'original cartridge controls final green swapping');
      await evaluate('Module._set_paused(true); window.greenState=Module._export_state(); Module._import_state(greenState)');
      assert.ok(await evaluate('Module._export_state().every((byte,n)=>byte===greenState[n])'),'green-swap state round-trips in WASM');
    }
    console.log('GBA browser: original cartridges verify Green Swap output and state replay');
    for (const control of [0x0e000020,0x0e000000]) {
      const rom=new Uint8Array(512), view=new DataView(rom.buffer), code=[], literals=[];
      const load=(reg,value)=>{ literals.push([code.length,reg,value]);code.push(0); };
      load(0,0x04ff0800);load(1,control);code.push(0xe5801000);
      load(0,0x02000000);load(1,0x7fe0001f);code.push(0xe5801000);
      load(0,control&0x20?0x02000000:0x03000000);code.push(0xe5901000);
      load(0,0x06000000);code.push(0xe5801000);
      load(0,0x04000000);load(1,0x403);code.push(0xe1c010b0,0xeafffffe);
      for (const [at,reg,value] of literals) {
        code[at]=0xe59f0000|(reg<<12)|(code.length*4-at*4-8);code.push(value);
      }
      view.setUint32(0,0xea00003e,true);rom[0xb2]=0x96;
      code.forEach((word,n)=>view.setUint32(0x100+n*4,word,true));
      const file=path.join(temporary,`original-memory-control-${control.toString(16)}.gba`);await fs.writeFile(file,rom);
      await evaluate('window.beforeMemoryId=Module._get_state_id()');
      await call('DOM.setFileInputFiles',{nodeId,files:[file]});
      await until("Module._get_state_id()!==beforeMemoryId && (()=>{ const s=Module._export_state(); return new DataView(s.buffer,s.byteOffset,s.byteLength).getBigUint64(309,true)>=2n; })()");
      assert.deepEqual(await evaluate("Array.from(document.querySelector('#rustboy-canvas').getContext('2d').getImageData(0,0,2,1).data)"),[255,0,0,255,0,255,255,255],'original cartridge reads normal/remapped RAM');
      assert.equal(await evaluate('(() => { Module._set_paused(true); const s=Module._export_state(); Module._import_state(s); return new DataView(s.buffer,s.byteOffset,s.byteLength).getUint32(s.length-34,true); })()'),control,'memory-control register survives WASM state replay');
    }
    console.log('GBA browser: original memory-control cartridges verify 64K register mirror and RAM mapping');
  }
  if (process.env.RUSTBOY_GBA_JIT_VERIFY && !process.env.RUSTBOY_GBA_PROFILE) {
    const fixture = new Uint8Array(32768);
    const image = new DataView(fixture.buffer);
    [0xe59f0000,0xe12fff10,0x08000101].forEach((word,n) => image.setUint32(n*4,word,true));
    fixture[0xb2]=0x96;
    const code=[], literals=[];
    const literal=(reg,value) => { literals.push({at:code.length,reg,value}); code.push(0); };
    literal(0,0x04000100); literal(1,0x80); code.push(0x8041); // timer 0 enable
    literal(2,0x06000000); literal(3,0x03000101); // VRAM and unaligned RAM
    literal(4,0x04000204); literal(1,0x4000); code.push(0x8021); // prefetch
    const loop=code.length;
    code.push(0x3501,0x006e,0x601d,0x681f,0x407e,0x8016,0x8807,0x2f00);
    code.push(0xd100 | ((loop-code.length-2)&255));
    code.push(0xe000 | ((loop-code.length-2)&0x7ff));
    let end=(0x100+code.length*2+3)&~3;
    for (const {at,reg,value} of literals) {
      code[at]=0x4800|(reg<<8)|((end-((0x100+at*2+4)&~3))>>2);
      image.setUint32(end,value,true); end+=4;
    }
    code.forEach((word,n) => image.setUint16(0x100+n*2,word,true));
    const file=path.join(temporary,'original-jit-timing.gba'); await fs.writeFile(file,fixture);
    await evaluate('window.jitPreviousId=Module._get_state_id()');
    await call('DOM.setFileInputFiles',{nodeId,files:[file]});
    await until("Module._get_state_id() !== jitPreviousId && Module._get_state_id().startsWith('gba-') && !Module._is_paused()");
    await evaluate(`(async () => {
      const wasm=await import('./out/rustboy.js');
      if (!wasm.benchmark_gba_clocks) throw new Error('Build --features gba-jit');
      Module._set_paused(true); const original=Module._export_state();
      wasm.set_gba_jit_enabled(true); wasm.set_gba_full_jit_enabled(true); wasm.benchmark_gba_clocks(280896);
      for (const budget of [1,2,4,17,31,1232,4096,280896,280896*3]) {
        const results=[];
        for (const enabled of [false,true]) {
          Module._import_state(original); Module._set_paused(true);
          wasm.set_gba_jit_enabled(enabled); wasm.benchmark_gba_clocks(budget);
          results.push(Module._export_state());
        }
        if (results[0].length!==results[1].length || results[0].some((byte,n)=>byte!==results[1][n]))
          throw new Error('JIT timer/MMIO/prefetch/RAM/VRAM state differs, budget='+budget);
      }
      if (!/full_instructions=[1-9]/.test(wasm.get_gba_jit_statistics())) throw new Error('No compiled Thumb execution');
      if (!/failures=0/.test(wasm.get_gba_jit_statistics())) throw new Error('Compilation failed');
    })()`);
    console.log('GBA JIT: original Thumb cartridge matches complete hardware state with timer/MMIO/prefetch/unaligned RAM/VRAM across nine budgets');
    // Original ARM cartridge: copies executable code into IWRAM, changes it
    // through a guest store, and runs connected arithmetic/branch/multiply paths.
    const arm = new Uint8Array(32768), armView = new DataView(arm.buffer);
    arm[0xb2]=0x96; armView.setUint32(0,0xea00003e,true);
    const ramProgram=[0xe3a04000,0xe3a01003,0xe2844001,0xe0020194,0xe1b032c2,
      0x1a000000,0xe0255002,0xe2966001,0xe31400ff,0x1a000001,0xe22aa003,0xe589a000,0xeafffff4];
    ramProgram.splice(ramProgram.length-1,0,0xe5874001,0xe5970001,0xe1d700b1,0xe1d700d1,0xe1d700f1,
      0xe5c84001,0xe1c840b2,0xe8870003,0xe8970003);
    ramProgram[ramProgram.length-1]=0xea000000|((2-(ramProgram.length-1)-2)&0xffffff);
    const boot=[], pool=[];
    const armLiteral=(reg,value)=>{pool.push({at:boot.length,reg,value});boot.push(0);};
    armLiteral(0,0x04000102);armLiteral(1,0x80);boot.push(0xe1c010b0);
    armLiteral(9,0x03000108);armLiteral(10,0xe2844002);
    armLiteral(7,0x02004000);armLiteral(8,0x03004000);
    armLiteral(0,0x03000100);armLiteral(1,0x08000400);armLiteral(2,0x08000400+ramProgram.length*4);
    boot.push(0xe4913004,0xe4803004,0xe1510002,0x1afffffb);
    armLiteral(0,0x03000100);boot.push(0xe12fff10);
    for (const {at,reg,value} of pool) {boot[at]=0xe59f0000|(reg<<12)|(boot.length*4-at*4-8);boot.push(value);}
    boot.forEach((word,n)=>armView.setUint32(0x100+n*4,word,true));
    ramProgram.forEach((word,n)=>armView.setUint32(0x400+n*4,word,true));
    const armFile=path.join(temporary,'original-arm-regions.gba');await fs.writeFile(armFile,arm);
    await evaluate('window.jitPreviousId=Module._get_state_id()');
    await call('DOM.setFileInputFiles',{nodeId,files:[armFile]});
    await until("Module._get_state_id() !== jitPreviousId && !Module._is_paused()");
    await evaluate(`(async()=>{
      const wasm=await import('./out/rustboy.js');Module._set_paused(true);
      const original=Module._export_state();
      const invalidations=()=>Number(/invalidations=([0-9]+)/.exec(wasm.get_gba_jit_statistics())[1]);
      const beforeInvalidations=invalidations();
      wasm.set_gba_jit_enabled(true);wasm.benchmark_gba_clocks(280896);
      if(invalidations()<=beforeInvalidations)throw new Error('Guest code rewrite did not invalidate an ARM region');
      for(const budget of [1,2,3,4,5,17,31,1006,1232,4096,280896,842688]) {
        let expected;
        for(const {enabled,ram,render} of [{enabled:false,ram:false,render:false},
          {enabled:true,ram:false,render:false},{enabled:true,ram:true,render:false},{enabled:true,ram:true,render:true}]) {
          Module._import_state(original);Module._set_paused(true);wasm.set_gba_jit_enabled(enabled);
          wasm.set_gba_full_jit_enabled(false);wasm.set_gba_jit_ram_enabled(ram);wasm.set_gba_fast_render_enabled(render);
          wasm.benchmark_gba_clocks(budget);const ending=Module._export_state();
          if(expected&&(ending.length!==expected.length||ending.some((byte,n)=>byte!==expected[n])))
            throw new Error('ARM region hardware state differs, budget='+budget+' offset='+ending.findIndex((byte,n)=>byte!==expected[n]));
          expected=ending;
        }
      }
      if(!/instructions=[1-9]/.test(wasm.get_gba_jit_statistics()))throw new Error('No compiled ARM execution');
      if(!/failures=0/.test(wasm.get_gba_jit_statistics()))throw new Error('ARM compilation failed');
    })()`);
    console.log('GBA ARM JIT: self-modifying code, RAM transfers, connected branches, multiply, rendering and timer state match across twelve budgets and four execution modes');
  }
  // Explicit opt-in only. Never copies the supplied game into public assets.
  if (process.env.RUSTBOY_GBA_PROFILE) {
    const profileFrames=Number(process.env.RUSTBOY_GBA_PROFILE_FRAMES || 120);
    const workFrames=Number(process.env.RUSTBOY_GBA_FIXED_WORK_FRAMES || 120);
    const profileSpeeds=(process.env.RUSTBOY_GBA_PROFILE_SPEEDS || '1,4,8').split(',').map(Number);
    assert.ok(Number.isInteger(profileFrames)&&profileFrames>=120&&profileFrames<=1200,'profile frames must be 120..1200');
    assert.ok(Number.isInteger(workFrames)&&workFrames>=120&&workFrames<=1200,'fixed-work frames must be 120..1200');
    assert.ok(profileSpeeds.length>0&&profileSpeeds.every(speed=>[1,2,4,8].includes(speed)),'invalid profile speed');
    const local = path.resolve(process.env.RUSTBOY_GBA_PROFILE);
    await fs.access(local);
    await call('DOM.setFileInputFiles', {nodeId, files: [local]});
    await until("Module._get_state_id().startsWith('gba-') && !Module._is_paused()");
    await evaluate(`new Promise(resolve => {
      let frames=0; requestAnimationFrame(function warmup() {
        if (++frames < 120) requestAnimationFrame(warmup); else resolve();
      });
    })`);
    const profileState = process.env.RUSTBOY_GBA_PROFILE_STATE
      ? (await fs.readFile(path.resolve(process.env.RUSTBOY_GBA_PROFILE_STATE))).toString('base64')
      : null;
    await evaluate(`(async () => {
      const wasm = await import('./out/rustboy.js');
      const clocks = () => {
        const bytes = Module._export_state();
        if (![5, 6, 7, 8, 9, 10, 11, 12, 13].includes(bytes[4])) throw new Error('Update profile snapshot offsets');
        return Number(new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength).getBigUint64(301, true));
      };
      Module._set_paused(true);
      const saved = ${JSON.stringify(profileState)};
      if (saved !== null) {
        Module._import_state(Uint8Array.from(atob(saved), c => c.charCodeAt(0)));
        Module._set_paused(true);
      }
      const baseline = Module._export_state();
      let before, requested, elapsed, delays;
      globalThis[Symbol.for('rustboy.test.gba.profile')] = {
        prepare(speed) {
          Module._import_state(baseline); Module._set_paused(true);
          Module._set_speed(speed); requested = speed; before = clocks();
        },
        async sample() {
          const start = performance.now(); let previous = start; delays = [];
          Module._set_paused(false);
          await new Promise(resolve => requestAnimationFrame(function sample() {
            const now = performance.now(); delays.push(now-previous); previous=now;
            if (delays.length < ${profileFrames}) requestAnimationFrame(sample); else resolve();
          }));
          elapsed = performance.now()-start; Module._set_paused(true);
        },
        result() {
          const achievedSpeed = (clocks()-before)/16777216/(elapsed/1000);
          delays.sort((a,b) => a-b);
          return {requestedSpeed: requested, achievedSpeed,
            medianCallbackMs: delays[Math.floor(delays.length*.5)], p95CallbackMs: delays[Math.floor(delays.length*.95)],
            maxCallbackMs: delays[delays.length-1], status: wasm.get_backend_status(), jit: wasm.get_gba_jit_statistics?.(), rendering:wasm.get_gba_render_statistics?.()};
        }
      };
    })()`);
    if (process.env.RUSTBOY_GBA_JIT_VERIFY) {
      const results = await evaluate(`(async () => {
        const wasm = await import('./out/rustboy.js');
        if (!wasm.benchmark_gba_clocks) throw new Error('Build --features gba-jit');
        Module._set_paused(true);
        const original = Module._export_state();
        const results = [];
        let expected;
        // Warm the code cache, then restore all hardware state. Compiled ROM
        // code is independent of registers and is revalidated on public run.
        wasm.set_gba_jit_enabled(true);
        wasm.set_gba_full_jit_enabled(true);
        wasm.benchmark_gba_clocks(280896 * ${workFrames});
        if (${Boolean(process.env.RUSTBOY_GBA_RAM_JIT_VERIFY)}) {
          Module._import_state(original);Module._set_paused(true);
          wasm.set_gba_full_jit_enabled(false);wasm.set_gba_jit_ram_enabled(false);
          wasm.benchmark_gba_clocks(280896 * ${workFrames});
        }
        const modes=${process.env.RUSTBOY_GBA_FETCH_VERIFY ? "[1,8,8,1,1,8].map(stride=>({enabled:true,full:false,ram:true,render:false,stride}))" : process.env.RUSTBOY_GBA_RAM_JIT_VERIFY ? "[0,1,2,3,4,4,3,2,1,0,1,3,0,4,2].map(mode=>({enabled:mode!==0,full:false,ram:mode>=2,render:mode===3,stride:mode===4?8:1}))" : process.env.RUSTBOY_GBA_FULL_JIT_VERIFY ? "[{enabled:false,full:false},{enabled:true,full:false},{enabled:true,full:true},{enabled:true,full:true},{enabled:false,full:false},{enabled:true,full:false},{enabled:true,full:false},{enabled:true,full:true},{enabled:false,full:false}]" : "[false,true,true,false,false,true].map(enabled=>({enabled,full:true}))"};
        for (const {enabled,full,ram=true,render=true,stride=1} of modes) {
          Module._import_state(original); Module._set_paused(true);
          wasm.set_gba_jit_enabled(enabled);
          wasm.set_gba_full_jit_enabled(full);
          wasm.set_gba_jit_ram_enabled(ram);
          wasm.set_gba_fast_render_enabled(render);
          wasm.set_gba_video_stride(stride);
          const start = performance.now();
          const clocks = wasm.benchmark_gba_clocks(280896 * ${workFrames});
          const ms = performance.now() - start;
          const ending = Module._export_state();
          const sameHardware = (a,b) => {
            if (a.length!==b.length || a[4]!==14 || b[4]!==14) throw new Error('Update frame-skip snapshot layout');
            const videoStart=567+new DataView(a.buffer,a.byteOffset,a.byteLength).getUint32(563,true)*8+0x60c00;
            const videoEnd=videoStart+240*160*4;
            return a.every((byte,n)=>n>=videoStart&&n<videoEnd || n>=a.length-8 || byte===b[n]);
          };
          if (expected && (stride===1
              ? ending.length !== expected.length || ending.some((byte,n) => byte !== expected[n])
              : !sameHardware(ending,expected))) {
            const offset = ending.findIndex((byte,n) => byte !== expected[n]);
            throw new Error('JIT hardware snapshot differs at byte ' + offset + ' enabled=' + enabled);
          }
          if(stride===1)expected = ending;
          // Compare deterministic hardware across separately built binaries.
          // Omit only the presentation buffer and its containing checksum.
          const videoStart=567+new DataView(ending.buffer,ending.byteOffset,ending.byteLength).getUint32(563,true)*8+0x60c00;
          const hardware = new Uint8Array(ending.length-240*160*4-8);
          hardware.set(ending.subarray(0,videoStart));
          hardware.set(ending.subarray(videoStart+240*160*4,ending.length-8),videoStart);
          const hardwareHash=Array.from(new Uint8Array(await crypto.subtle.digest('SHA-256',hardware)),byte=>byte.toString(16).padStart(2,'0')).join('');
          const snapshotHash=Array.from(new Uint8Array(await crypto.subtle.digest('SHA-256',ending)),byte=>byte.toString(16).padStart(2,'0')).join('');
          results.push({enabled, full, ram, render, stride, hardwareHash, snapshotHash, rendering:wasm.get_gba_render_statistics(), ms, clocks, speed: clocks / 16777216 / (ms / 1000), stats: wasm.get_gba_jit_statistics()});
        }
        Module._import_state(original); Module._set_paused(true);
        wasm.set_gba_jit_enabled(true);
        wasm.set_gba_full_jit_enabled(!${Boolean(process.env.RUSTBOY_GBA_RAM_JIT_VERIFY || process.env.RUSTBOY_GBA_FETCH_VERIFY)});
        wasm.set_gba_jit_ram_enabled(true);
        wasm.set_gba_fast_render_enabled(false);
        return results;
      })()`);
      console.log('GBA JIT fixed-work state comparison (skipping excludes host pixels/checksum):', JSON.stringify(results));
      assert.ok(results.some(row => /instructions=[1-9]/.test(row.stats)), 'JIT must execute compiled instructions');
      assert.ok(results.every(row => /failures=0/.test(row.stats)), 'all hot blocks compile');
      for(const row of results.filter(row=>row.stride>1)) {
        const stats=/stride=(\d+) rendered=(\d+) skipped=(\d+)/.exec(row.rendering);
        assert.equal(Number(stats[1]),8);
        assert.ok(Number(stats[2])<=Math.ceil(workFrames/8)+2&&Number(stats[3])>=Math.floor(workFrames/8)*7-2,'only selected video frames are composed');
      }
    }
    const metrics = [];
    let profiling = false;
    try {
      for (const requested of profileSpeeds) {
        await evaluate(`globalThis[Symbol.for('rustboy.test.gba.profile')].prepare(${requested})`);
        if (requested === 8 && process.env.RUSTBOY_GBA_CPU_PROFILE) {
          await call('Profiler.enable');
          await call('Profiler.setSamplingInterval',{interval:1000});
          await call('Profiler.start');
          profiling = true;
        }
        await evaluate(`globalThis[Symbol.for('rustboy.test.gba.profile')].sample()`);
        if (profiling) {
          const {profile} = await call('Profiler.stop');
          profiling = false;
          await call('Profiler.disable');
          await fs.writeFile(path.resolve(process.env.RUSTBOY_GBA_CPU_PROFILE),JSON.stringify(profile),{flag:'wx'});
          console.log('Local GBA CPU profile written (8x execution interval only; exclusive create):',process.env.RUSTBOY_GBA_CPU_PROFILE);
        }
        metrics.push(await evaluate(`globalThis[Symbol.for('rustboy.test.gba.profile')].result()`));
      }
    } finally {
      if (profiling) {
        await call('Profiler.stop');
        await call('Profiler.disable');
      }
      await evaluate(`Module._set_paused(true); Module._set_speed(1); delete globalThis[Symbol.for('rustboy.test.gba.profile')]`);
    }
    for (const metric of metrics) {
      assert.ok(!metric.status.includes('stopped'), metric.status);
      assert.ok(metric.achievedSpeed > 0, 'GBA continues executing');
      if(metric.rendering&&metric.requestedSpeed>1) {
        const stride=Number(/stride=(\d+)/.exec(metric.rendering)[1]);
        assert.ok(stride>=metric.requestedSpeed&&stride<=16,'UI adapts frame skipping to callback work');
        const stats=/rendered=(\d+) skipped=(\d+)/.exec(metric.rendering);
        assert.ok(Number(stats[2])>Number(stats[1]),'fast-forward skips undisplayed frames');
      }
    }
    console.log('Local GBA fast-forward profile:', JSON.stringify(metrics));
    if (process.env.RUSTBOY_GBA_PROFILE_SCREENSHOT) {
      const data=await evaluate("document.querySelector('#rustboy-canvas').toDataURL('image/png').split(',')[1]");
      await fs.writeFile(path.resolve(process.env.RUSTBOY_GBA_PROFILE_SCREENSHOT),Buffer.from(data,'base64'),{flag:'wx'});
    }
  }
  if (process.env.RUSTBOY_SGB_FIRMWARE) {
    const firmware = path.resolve(process.env.RUSTBOY_SGB_FIRMWARE);
    await fs.access(firmware);
    const {nodeId: firmwareNode} = await call('DOM.querySelector', {nodeId: document.nodeId, selector: '#sgb-firmware-input'});
    await call('DOM.setFileInputFiles', {nodeId: firmwareNode, files: [firmware]});
    await until("document.querySelector('#sgb-firmware-status').textContent.includes('firmware loaded')");
    assert.ok(await evaluate("localStorage.getItem(SGB_FIRMWARE_KEY)?.length > 300000"));
    const saved = await evaluate("localStorage.getItem(SGB_FIRMWARE_KEY)");
    await call('DOM.setFileInputFiles', {nodeId: firmwareNode, files: [invalidRom]});
    await until("document.body.textContent.includes('Expected a 256/512 KiB')");
    assert.equal(await evaluate("localStorage.getItem(SGB_FIRMWARE_KEY)"), saved, 'invalid firmware cannot replace the previous choice');
  }
  let gamePath = process.env.RUSTBOY_ROM && path.resolve(process.env.RUSTBOY_ROM);
  const borderTest = !!process.env.RUSTBOY_SGB_BORDER;
  const paletteTest = !!process.env.RUSTBOY_SGB_PALETTES;
  const multiplayerTest = !!process.env.RUSTBOY_SGB_MULTIPLAYER;
  const audioTest = !!process.env.RUSTBOY_SGB_AUDIO && !gamePath;
  assert.ok([borderTest,paletteTest,audioTest].filter(Boolean).length <= 1, 'select one generated SGB cartridge');
  if (process.env.RUSTBOY_HARDWARE) {
    assert.ok(['auto', 'dmg', 'cgb', 'sgb'].includes(process.env.RUSTBOY_HARDWARE));
    await evaluate(`document.querySelector('#hardware-model').value = ${JSON.stringify(process.env.RUSTBOY_HARDWARE)}; document.querySelector('#hardware-model').dispatchEvent(new Event('change'))`);
  }
  if (borderTest || paletteTest || audioTest) {
    const fixture = borderTest ? 'border' : paletteTest ? 'palette' : 'audio';
    gamePath = path.join(temporary, `sgb-${fixture}.gb`);
    await promisify(execFile)('cargo', ['run', '--locked', '--offline', '--release', '--no-default-features',
      '--example', `sgb_${fixture}_fixture`, '--', gamePath], {cwd: root, timeout: 120000});
    await evaluate("document.querySelector('#hardware-model').value = 'auto'; document.querySelector('#hardware-model').dispatchEvent(new Event('change'))");
  }
  if (!gamePath && process.env.RUSTBOY_NO_BOOT && !process.env.RUSTBOY_HOMEBREW) {
    // Public-domain synthetic program: draw a repeating tile and publish 0x66.
    const rom = new Uint8Array(32768);
    rom.set([0xC3, 0x50, 0x01], 0x100);
    rom.fill(0xAA, 0x104, 0x134);
    rom[0x143] = process.env.RUSTBOY_SYNTHETIC_CGB ? 0x80 : 0;
    if (process.env.RUSTBOY_SYNTHETIC_SGB || multiplayerTest) {
      rom[0x143] = 0x80; // Dual-mode cart explicitly running on the SGB/DMG path.
      rom[0x146] = 3; rom[0x14B] = 0x33;
      await evaluate("document.querySelector('#hardware-model').value = 'auto'; document.querySelector('#hardware-model').dispatchEvent(new Event('change'))");
      assert.ok(await evaluate("!document.querySelector('#sgb-warning').classList.contains('hidden')"));
    }
    const code = [0xF3, 0xAF, 0xE0, 0x40, 0x3E, 0xE4, 0xE0, 0x47,
      0x21, 0x00, 0x80, 0x06, 0x10, 0x3E, 0xAA, 0x22, 0x05, 0x20, 0xFC,
      0x3E, 0x80, 0xE0, 0x68];
    for (const color of [0xFF, 0x7F, 0, 0, 0, 0, 0, 0]) code.push(0x3E, color, 0xE0, 0x69);
    if (process.env.RUSTBOY_SYNTHETIC_SGB || multiplayerTest) {
      // PAL01 through real cartridge instructions, with recommended low/high
      // pulse spacing. White backdrop, shade 3 red instead of handheld black.
      const pulse = value => code.push(0x3E, value, 0xE0, 0, ...Array(5).fill(0), 0x3E, 0x30, 0xE0, 0, ...Array(15).fill(0));
      pulse(0);
      for (const byte of [1, 0xFF, 0x7F, 0xE0, 3, 0, 0x7C, 31, 0, 0, 0, 0, 0, 0, 0, 0]) {
        for (let bit = 0; bit < 8; bit++) pulse(byte & (1 << bit) ? 0x10 : 0x20);
      }
      pulse(0x20);
      if (multiplayerTest) {
        pulse(0);
        for (const byte of [0x89,3,...Array(14).fill(0)]) {
          for (let bit = 0; bit < 8; bit++) pulse(byte & (1 << bit) ? 0x10 : 0x20);
        }
        pulse(0x20);
      }
    }
    code.push(0x3E, 0x91, 0xE0, 0x40, 0x3E, 0x66, 0xEA, 0x00, 0xC0);
    if (multiplayerTest) {
      const loop = 0x150 + code.length;
      // Original cartridge code records each player's action/direction rows.
      // P15 release advances the player only after the action row is sampled.
      code.push(0x3E,0x30,0xE0,0,0xF0,0,0x2F,0xE6,3,0x6F,0x26,0xC1,
        0x3E,0x20,0xE0,0,0xF0,0,0x47,0x7D,0xC6,4,0x6F,0x70,
        0x3E,0x30,0xE0,0,0x7D,0xD6,4,0x6F,
        0x3E,0x10,0xE0,0,0xF0,0,0x77,0x3E,0x30,0xE0,0,
        0xC3,loop&255,loop>>8);
    } else code.push(0x18,0xFE);
    rom.set(code, 0x150);
    gamePath = path.join(temporary, rom[0x143] ? 'synthetic.gbc' : 'synthetic.gb');
    await fs.writeFile(gamePath, rom);
  }
  if (process.env.RUSTBOY_HOMEBREW) {
    const entries = JSON.parse(await fs.readFile(path.join(site, 'homebrew/romlist.json'), 'utf8'));
    assert.equal(entries.length, 3);
    // These pinned cartridges exercise all automatic hardware choices. Tobu
    // supports both CGB and SGB; Auto deliberately prefers its SGB enhancements.
    const homebrewHardware = {'2048':'dmg','tobu-tobu-girl-deluxe':'sgb','ucity':'cgb'};
    for (const entry of entries) {
      await evaluate("openPicker(); searchQuery = ''; document.querySelector('#rom-search').value = ''; document.querySelector('[data-tab=homebrew]').click(); filterRoms()");
      await until("document.querySelectorAll('#rom-list li .rom-name').length === 3");
      assert.ok(await evaluate("!document.querySelector('#homebrew-info').classList.contains('hidden')"));
      const title = JSON.stringify(entry.title);
      await evaluate(`Array.from(document.querySelectorAll('#rom-list li')).find(li => li.querySelector('.rom-name')?.textContent === ${title}).click()`);
      await until(`currentRomInfo?.name === ${JSON.stringify(entry.name)} && document.querySelector('#rom-picker').classList.contains('hidden')`);
      await until("Module._get_debug_state().includes('Boot:0')");
      // Tobu has non-skippable publisher/music logos before its intro/menu.
      await new Promise(resolve => setTimeout(resolve, entry.id === 'tobu-tobu-girl-deluxe' ? 14000 : 3000));
      const colors = await evaluate("new Set(new Uint32Array(document.querySelector('#rustboy-canvas').getContext('2d').getImageData(0,0,160,144).data.buffer)).size");
      assert.ok(colors > 1, `${entry.title} renders a nonblank game screen`);
      const expectedHardware = homebrewHardware[entry.id];
      assert.ok(expectedHardware, `${entry.title}: pinned hardware expectation exists`);
      assert.equal(await evaluate('Module._get_is_cgb()'), expectedHardware === 'cgb', `${entry.title}: automatic CGB selection`);
      assert.equal(await evaluate("Module._get_sgb_status().startsWith('SGB HLE: functions enabled')"), expectedHardware === 'sgb', `${entry.title}: automatic SGB selection`);
      for (const [key, code] of [['Enter', 'Enter'], ['a', 'KeyA'], ['ArrowRight', 'ArrowRight']]) {
        await call('Input.dispatchKeyEvent', {type: 'keyDown', key, code});
        await new Promise(resolve => setTimeout(resolve, 120));
        await call('Input.dispatchKeyEvent', {type: 'keyUp', key, code});
        await new Promise(resolve => setTimeout(resolve, 500));
      }
      await evaluate("document.querySelector('#quick-pause').click()");
      await until("document.querySelector('#quick-pause').textContent.includes('Resume')");
      if (process.env.RUSTBOY_SCREENSHOT_DIR) {
        await fs.mkdir(process.env.RUSTBOY_SCREENSHOT_DIR, {recursive: true});
        const data = await evaluate("document.querySelector('#rustboy-canvas').toDataURL('image/png').split(',')[1]");
        await fs.writeFile(path.join(process.env.RUSTBOY_SCREENSHOT_DIR, `${entry.id}.png`), Buffer.from(data, 'base64'));
      }
      await evaluate("document.querySelector('#quick-pause').click()");
      assert.deepEqual(exceptions, [], `${entry.title}: no unhandled errors`);
      console.log(`Homebrew picker: ${entry.title}, verified ROM download, game rendering/input, pause/resume passed`);
    }
    const credits = await fs.readFile(path.join(site, 'homebrew/credits.html'), 'utf8');
    assert.ok(credits.includes('potato-tan'));
    assert.ok(credits.includes('sources/ucity-v1.3.zip'));
    const current = await evaluate("currentRomInfo.name");
    // Model a corrupted deployment payload. Failed integrity checks must not
    // replace the current game or its save namespace.
    await evaluate("openPicker(); searchQuery = ''; filterRoms(); ROMS.find(r => r.name === '2048.gb').sha256 = '0'.repeat(64); selectedIdx = ROMS.findIndex(r => r.name === '2048.gb'); loadSelectedRom()");
    await until("document.body.textContent.includes('checksum mismatch')");
    assert.equal(await evaluate("currentRomInfo.name"), current);
    assert.ok(await evaluate("!document.querySelector('#rom-picker').classList.contains('hidden')"));
  }
  if (gamePath) {
    // Chromium can silently accept a nonexistent file in its input command.
    await fs.access(gamePath);
    await call('DOM.setFileInputFiles', {nodeId, files: [gamePath]});
    await until("document.querySelector('#quick-pause').textContent.includes('Pause')");
    if (!process.env.RUSTBOY_ROM && process.env.RUSTBOY_NO_BOOT) {
      const cgb = !!process.env.RUSTBOY_SYNTHETIC_CGB;
      const mask = (await fs.readFile(path.join(root, `crates/gameboy/bootroms/logo_${cgb ? 'cgb' : 'dmg'}.txt`), 'utf8')).trim().split('\n');
      await until(`(() => {
        const pixels = document.querySelector('#rustboy-canvas').getContext('2d').getImageData(0,0,160,144).data;
        const mask = ${JSON.stringify(mask)};
        return mask.every((row,y) => [...row].every((bit,x) => {
          const i = ((${cgb ? 48 : 64}+y)*160+${cgb ? 16 : 32}+x)*4;
          const foreground = pixels[i] !== pixels[0] || pixels[i+1] !== pixels[1] || pixels[i+2] !== pixels[2];
          return foreground === (bit === '1');
        }));
      })()`, 5000);
      if (process.env.RUSTBOY_BOOT_SCREENSHOT) {
        // Optional debugging artifact, outside the temporary browser profile.
        const data = await evaluate("document.querySelector('#rustboy-canvas').toDataURL('image/png').split(',')[1]");
        await fs.writeFile(process.env.RUSTBOY_BOOT_SCREENSHOT, Buffer.from(data, 'base64'));
      }
    }
    const waitMs = Number(process.env.RUSTBOY_GAME_WAIT_MS || 6000);
    assert.ok(Number.isFinite(waitMs) && waitMs >= 0 && waitMs <= 60000);
    await new Promise(resolve => setTimeout(resolve, waitMs));
    if (multiplayerTest) {
      await until("Module._get_sgb_status().includes('players 4')");
      assert.equal(await evaluate('Module._get_debug_flags()'),0,'multiplayer needs no debug opt-in');
      const rows = "Array.from({length:8},(_,i) => parseInt(window.mem(0xC100+i),16)&15)";
      await evaluate("window.testPads = []; window.makeTestPad = (index,held=[],axes=[0,0]) => ({index,id:'Simulated pad '+index,connected:true,mapping:'standard',axes,buttons:Array.from({length:16},(_,i)=>({pressed:held.includes(i)}))}); Object.defineProperty(navigator,'getGamepads',{configurable:true,value:()=>window.testPads}); window.testPads = [makeTestPad(0),makeTestPad(1),makeTestPad(2),makeTestPad(3)]; pollControllers()");
      await until(`${rows}.every(value => value === 15)`);
      await evaluate("window.testPads = [makeTestPad(0,[1]),makeTestPad(1,[0]),makeTestPad(2,[9]),makeTestPad(3,[8],[-1,0])]; pollControllers()");
      await until(`JSON.stringify(${rows}) === '[14,13,7,11,15,15,15,13]'`);
      assert.ok(await evaluate("document.querySelector('#controller-status').textContent.includes('Player 4: Simulated pad 3')"));
      await evaluate("dispatchActionPress('btn_a','keyKeyK'); dispatchActionPress('btn_a','touch42'); testPads[0]=makeTestPad(0); pollControllers(); dispatchActionRelease('btn_a','keyKeyK')");
      await until(`${rows}[0] === 14`);
      await evaluate("dispatchActionRelease('btn_a','touch42'); testPads.splice(1,1); pollControllers()");
      await until(`${rows}[0] === 15 && ${rows}[1] === 15 && ${rows}[2] === 7`);
      await evaluate("window.controllerSavedState = Module._export_state(); Module._import_state(window.controllerSavedState); pollControllers()");
      await until(`${rows}.every(value => value === 15)`);
      await evaluate("testPads = [makeTestPad(0),makeTestPad(2),makeTestPad(3)]; pollControllers(); testPads[1] = makeTestPad(2,[1]); pollControllers()");
      await until(`${rows}[2] === 14`);
      await evaluate("window.dispatchEvent(new Event('blur')); window.dispatchEvent(new Event('focus')); pollControllers()");
      await until(`${rows}.every(value => value === 15)`);
      await evaluate("testPads[1]=makeTestPad(2); pollControllers(); testPads[1]=makeTestPad(2,[1]); pollControllers(); Module._reset_emulator(); pollControllers()");
      await until(`Module._get_sgb_status().includes('players 4') && ${rows}.every(value => value === 15)`);
      await evaluate("testPads=[]; pollControllers(); Module._set_controller_button(3,'start',true)");
      await until(`${rows}[3] === 7`);
      await evaluate("Module._release_controller_inputs()");
      await until(`${rows}.every(value => value === 15)`);
      await evaluate("testPads=[makeTestPad(0,[1])]; pollControllers()");
      await until(`${rows}[0] === 14`);
      await evaluate("Module._set_paused(true); Module._set_paused(false); pollControllers()");
      await until(`${rows}.every(value => value === 15)`);
      await evaluate("testPads=[makeTestPad(0)]; pollControllers(); testPads=[makeTestPad(0,[1])]; pollControllers()");
      await until(`${rows}[0] === 14`);
      await call('Input.dispatchKeyEvent', {type:'keyDown',key:' ',code:'Space'});
      await until("Module._is_paused()");
      await call('Input.dispatchKeyEvent', {type:'keyUp',key:' ',code:'Space'});
      await new Promise(resolve => setTimeout(resolve,100));
      await evaluate("Module._set_paused(false); pollControllers()");
      await until(`${rows}.every(value => value === 15)`);
      await evaluate("testPads=[]; pollControllers()");
      const pixel = "Array.from(document.querySelector('#rustboy-canvas').getContext('2d').getImageData(0,0,1,1).data)";
      await evaluate("document.querySelector('#sgb-user-palette').value='warm'; document.querySelector('#sgb-user-palette').dispatchEvent(new Event('change'))");
      await until(`JSON.stringify(${pixel}) === '[74,16,16,255]'`);
      await evaluate("window.warmPaletteState=Module._export_state(); document.querySelector('#sgb-user-palette').value='cool'; document.querySelector('#sgb-user-palette').dispatchEvent(new Event('change'))");
      await until(`JSON.stringify(${pixel}) === '[0,24,66,255]'`);
      await evaluate("Module._import_state(window.warmPaletteState)");
      await until(`JSON.stringify(${pixel}) === '[74,16,16,255]'`);
      await evaluate("Module._reset_emulator()");
      await until(`JSON.stringify(${pixel}) === '[0,24,66,255]'`);
      await evaluate("document.querySelector('#sgb-user-palette').value='game'; document.querySelector('#sgb-user-palette').dispatchEvent(new Event('change'))");
      await until(`JSON.stringify(${pixel}) === '[255,0,0,255]'`);
      console.log('SGB multiplayer browser: four simulated pads, source union, stable disconnect, neutral restore/reset/blur/pause, public controller exports and palette preference/snapshot passed');
    }
    if (process.env.RUSTBOY_SGB_FIRMWARE || process.env.RUSTBOY_SGB_AUDIO) {
      assert.ok(await evaluate("Module._get_sgb_status().includes('SNES audio active')"));
      if (!process.env.RUSTBOY_SGB_FIRMWARE) {
        assert.equal(await evaluate("localStorage.getItem(SGB_FIRMWARE_KEY)"),null);
        assert.ok(await evaluate("Module._get_sgb_status().includes('built-in replacement')"));
      }
      await call('Input.dispatchKeyEvent', {type: 'keyDown', key: 'Enter', code: 'Enter'});
      await new Promise(resolve => setTimeout(resolve, 120));
      await call('Input.dispatchKeyEvent', {type: 'keyUp', key: 'Enter', code: 'Enter'});
      await until("Module._get_sgb_status().includes('sound uploads 1')", 20000);
      if (!process.env.RUSTBOY_SGB_FIRMWARE) {
        assert.ok(await evaluate("Module._get_sgb_status().includes('score errors 0')"));
      }
      await new Promise(resolve => setTimeout(resolve, 1500));
      await evaluate("window.sgbAudioEnergy = 0; window.sgbAudioSamples = 0; window.sgbAudioFinite = true; window.originalAudioQueue = queueAudioSamples; queueAudioSamples = (left,right,rate) => { for (const channel of [left,right]) for (const sample of channel) { window.sgbAudioFinite &&= Number.isFinite(sample); window.sgbAudioEnergy += sample*sample; window.sgbAudioSamples++; } window.originalAudioQueue(left,right,rate); }");
      await new Promise(resolve => setTimeout(resolve, 3000));
      assert.ok(await evaluate("window.sgbAudioFinite && window.sgbAudioSamples > 50000 && window.sgbAudioEnergy/window.sgbAudioSamples > 0.000001"), 'SGB music reaches the browser stereo queue');
      await evaluate("Module._set_paused(true); window.sgbSavedState = Module._export_state(); Module._reset_emulator(); Module._import_state(window.sgbSavedState); window.sgbAudioEnergy = 0; window.sgbAudioSamples = 0; Module._set_paused(false)");
      assert.ok(await evaluate("Module._get_sgb_status().includes('SNES audio active')"));
      await new Promise(resolve => setTimeout(resolve, 2000));
      assert.ok(await evaluate("window.sgbAudioEnergy/window.sgbAudioSamples > 0.000001"), 'enhanced music continues after save/restore');
      console.log(`SGB audio browser: ${process.env.RUSTBOY_SGB_FIRMWARE ? 'firmware override' : 'built-in replacement without firmware'}, ${audioTest ? 'original synthetic score' : 'real-game'} music, reset and save/restore passed`);
    }
    if (paletteTest) {
      await until("window.mem(0xC000) === '0x66'");
      const paletteSamples = "(() => { const c = document.querySelector('#rustboy-canvas'); return [[0,0],[8,0],[8,8],[16,0],[24,0]].map(([x,y]) => Array.from(c.getContext('2d').getImageData(x,y,1,1).data)); })()";
      const expected = [[0,255,0,255],[255,0,0,255],[0,0,255,255],[255,0,0,255],[255,0,255,255]];
      assert.deepEqual(await evaluate(paletteSamples), expected, 'PAL_TRN/PAL_SET/ATTR_TRN reach the canvas');
      const status = await evaluate("Module._get_sgb_status()");
      assert.ok(status.includes('commands 5') && status.includes('unsupported []') && status.includes('screen mask 0') && status.includes('pending transfers 0'), status);
      await evaluate("window.sgbSavedState = Module._export_state(); Module._reset_emulator(); Module._import_state(window.sgbSavedState)");
      await until("Module._get_sgb_status().includes('commands 5')");
      assert.deepEqual(await evaluate(paletteSamples), expected, 'snapshot restores transferred palettes and attributes');
      console.log('SGB palettes browser: all-white palette replacement, LCD table uploads, ATF selection, mask cancellation and snapshot restore passed');
    }
    if (borderTest) {
      await until("document.querySelector('#rustboy-canvas').width === 256 && window.mem(0xC000) === '0x66'");
      const geometry = await evaluate("(() => { const c = document.querySelector('#rustboy-canvas'); return [c.width,c.height,c.style.aspectRatio]; })()");
      assert.deepEqual(geometry, [256,224,'8 / 7']);
      const samples = await evaluate("(() => { const ctx = document.querySelector('#rustboy-canvas').getContext('2d'); return [[0,0],[8,0],[48,40],[49,40],[56,48]].map(([x,y]) => Array.from(ctx.getImageData(x,y,1,1).data)); })()");
      assert.deepEqual(samples, [[0,255,0,255],[255,0,0,255],[173,173,173,255],[255,255,255,255],[255,0,0,255]]);
      assert.equal(await evaluate("document.querySelector('#hardware-model').value"), 'auto');
      await evaluate("document.querySelector('#show-sgb-border').click()");
      await until("document.querySelector('#rustboy-canvas').width === 160");
      assert.equal(await evaluate("document.querySelector('#touch-sgb-border').checked"), false);
      assert.equal(await evaluate("Module._get_is_cgb()"), false, 'border visibility must not change hardware');
      assert.deepEqual(await evaluate("Array.from(document.querySelector('#rustboy-canvas').getContext('2d').getImageData(8,8,1,1).data)"), [173,173,173,255]);
      await evaluate("window.hiddenBorderState = Module._export_state(); Module._import_state(window.hiddenBorderState)");
      assert.equal(await evaluate("document.querySelector('#rustboy-canvas').width"), 160);
      await evaluate("document.querySelector('#touch-sgb-border').click()");
      await until("document.querySelector('#rustboy-canvas').width === 256");
      assert.equal(await evaluate("document.querySelector('#show-sgb-border').checked"), true);
      // Border memory lives outside handheld VRAM and has its own debug views.
      await evaluate("document.querySelector('#debug-tools-toggle').click(); dispatchActionPress('vram'); document.querySelector('#vram-view').value = 'sgb-tiles'; document.querySelector('#vram-view').dispatchEvent(new Event('change'))");
      await until("document.querySelector('#vram-canvas').width === 384 && document.querySelector('#vram-canvas').height === 128");
      await evaluate("dispatchActionRelease('vram')");
      assert.deepEqual(await evaluate("Array.from(document.querySelector('#vram-canvas').getContext('2d').getImageData(128,64,1,1).data)"), [0,255,0,255]);
      await evaluate("document.querySelector('#vram-view').value = 'sgb-border'; document.querySelector('#vram-view').dispatchEvent(new Event('change'))");
      await until("document.querySelector('#vram-canvas').width === 256 && document.querySelector('#vram-canvas').height === 224");
      assert.deepEqual(await evaluate("Array.from(document.querySelector('#vram-canvas').getContext('2d').getImageData(0,0,1,1).data)"), [0,255,0,255]);
      await evaluate("document.querySelector('#debug-tools-toggle').click()");
      // The wider border must remain fully visible beside the desktop panels,
      // as well as in a portrait touch layout. Canvas object-fit preserves the
      // native ratio when the available CSS box is narrower than the frame.
      for (const [width, height, mobile] of [[1100,720,false],[390,844,true]]) {
        await call('Emulation.setDeviceMetricsOverride', {width, height, deviceScaleFactor: 1, mobile});
        await new Promise(resolve => setTimeout(resolve, 150));
        assert.ok(await evaluate("(() => { const c = document.querySelector('#rustboy-canvas'), a = document.querySelector('#screen-area'); const r = c.getBoundingClientRect(), s = a.getBoundingClientRect(); return r.width > 0 && r.height > 0 && r.left >= s.left - 1 && r.right <= s.right + 1 && r.top >= s.top - 1 && r.bottom <= s.bottom + 1 && getComputedStyle(c).objectFit === 'contain'; })()"), 'border canvas must fit the available screen area');
      }
      await call('Emulation.clearDeviceMetricsOverride');
      assert.ok(await evaluate("Module._get_sgb_status().includes('border present')"));
      assert.ok(await evaluate("Module._get_sgb_status().includes('pending transfers 0')"));
      await evaluate("window.sgbSavedState = Module._export_state(); Module._reset_emulator()");
      await until("document.querySelector('#rustboy-canvas').width === 160");
      await evaluate("Module._import_state(window.sgbSavedState)");
      await until("document.querySelector('#rustboy-canvas').width === 256");
      assert.deepEqual(await evaluate("Array.from(document.querySelector('#rustboy-canvas').getContext('2d').getImageData(0,0,1,1).data)"), [0,255,0,255]);
      if (process.env.RUSTBOY_BORDER_SCREENSHOT) {
        const data = await evaluate("document.querySelector('#rustboy-canvas').toDataURL('image/png').split(',')[1]");
        await fs.writeFile(process.env.RUSTBOY_BORDER_SCREENSHOT, Buffer.from(data, 'base64'));
      }
      // Loading a normal GB cartridge afterwards must restore 160x144 output,
      // rather than leaving the previous SGB border/canvas around the next game.
      await evaluate("document.querySelector('#hardware-model').value = 'auto'");
      const ordinary = new Uint8Array(32768); ordinary.set([0xC3,0x50,1], 0x100); ordinary.set([0x18,0xFE], 0x150);
      const ordinaryPath = path.join(temporary, 'ordinary.gb'); await fs.writeFile(ordinaryPath, ordinary);
      await call('DOM.setFileInputFiles', {nodeId, files: [ordinaryPath]});
      await until("Module._get_sgb_status() === 'SGB mode is off' && document.querySelector('#rustboy-canvas').width === 160");
      assert.equal(await evaluate("document.querySelector('#rustboy-canvas').height"), 144);
      console.log('SGB border browser: CHR/PCT uploads, 256x224 canvas, transparency/overlay, snapshot/reset and return to handheld passed');
    }
    const colors = await evaluate("(() => { const c = document.querySelector('#rustboy-canvas'); return new Set(new Uint32Array(c.getContext('2d').getImageData(c.width === 256 ? 48 : 0,c.height === 224 ? 40 : 0,160,144).data.buffer)).size; })()");
    if (!borderTest) assert.ok(colors > 1, 'game canvas must render nonblank content');
    if (process.env.RUSTBOY_GAME_SCREENSHOT) {
      const data = await evaluate("document.querySelector('#rustboy-canvas').toDataURL('image/png').split(',')[1]");
      await fs.writeFile(process.env.RUSTBOY_GAME_SCREENSHOT, Buffer.from(data, 'base64'));
    }
    // Debugging is opt-in; appearance controls and browser refresh are not.
    await evaluate("if (pickerOpen) closePicker()");
    const key = async code => {
      const accepted = await evaluate(`(() => { const event = new KeyboardEvent('keydown', {code: ${JSON.stringify(code)}, bubbles:true, cancelable:true}); return window.dispatchEvent(event); })()`);
      await new Promise(resolve => setTimeout(resolve, 100));
      await evaluate(`window.dispatchEvent(new KeyboardEvent('keyup', {code: ${JSON.stringify(code)}, bubbles:true, cancelable:true}))`);
      return accepted;
    };
    assert.equal(await evaluate("Module._get_debug_flags()"), 0);
    assert.ok(await evaluate("[...document.querySelectorAll('.debug-tools')].every(el => el.classList.contains('hidden'))"));
    for (const code of ['F2','F3','F6','F7','KeyL','F5']) assert.equal(await key(code), true, `${code} must not enable debugging by default`);
    assert.equal(await evaluate("Module._get_debug_flags()"), 0);
    await key('KeyH');
    assert.ok(await evaluate("document.querySelector('#panel-right').classList.contains('panels-hidden')"));
    assert.equal(await evaluate("Module._get_debug_flags()"), 0);
    await key('KeyH');
    await key('Backquote');
    assert.equal(await evaluate("Module._get_debug_flags()"), 1);
    assert.equal(await key('F5'), true, 'F5 remains browser refresh with debug tools enabled');
    // Enable expensive output only while paused, then ensure closing debug tools
    // disables logging/tracing/VRAM together before gameplay resumes.
    await evaluate("Module._set_paused(true)");
    await key('KeyL'); await key('F7'); await key('F2'); await key('F3');
    assert.equal(await evaluate("Module._get_debug_flags()"), 15);
    await key('Backquote');
    assert.equal(await evaluate("Module._get_debug_flags()"), 0);
    assert.ok(await evaluate("document.querySelector('#debug-hud').classList.contains('hidden') && document.querySelector('#vram-canvas').style.display === 'none'"));
    await evaluate("Module._set_paused(false)");
    console.log('Interface: opt-in debug tools, refresh-safe F5, independent H, debug shutdown and automatic cartridge model passed');
    if (!process.env.RUSTBOY_ROM && process.env.RUSTBOY_NO_BOOT && !borderTest) {
      assert.equal(await evaluate("window.mem(0xC000)"), '0x66', 'replacement boot must reach the cartridge');
      assert.ok(await evaluate("(async () => (await import('./out/rustboy.js')).get_boot_rom_license().includes('Lior Halphon'))()"));
      if (process.env.RUSTBOY_SYNTHETIC_SGB && !borderTest) {
        assert.equal(await evaluate("Module._get_is_cgb()"), false, 'SGB uses DMG hardware for a dual-mode cartridge');
        assert.ok(await evaluate("Module._get_sgb_status().includes('commands 1')"));
        const pixel = await evaluate("Array.from(document.querySelector('#rustboy-canvas').getContext('2d').getImageData(0,0,1,1).data)");
        assert.deepEqual(pixel, [255, 0, 0, 255], 'cartridge PAL01 reaches the canvas');
        assert.ok(await evaluate("Module._get_state_id().endsWith('-sgb-hle-v1')"));
        await evaluate("window.sgbSavedState = Module._export_state(); Module._reset_emulator(); Module._import_state(window.sgbSavedState)");
        assert.ok(await evaluate("Module._get_sgb_status().includes('commands 1')"));
        // A rejected CGB-only upload must leave the running SGB session intact.
        const incompatible = new Uint8Array(32768); incompatible[0x143] = 0xC0;
        const incompatiblePath = path.join(temporary, 'color-only.gbc');
        await fs.writeFile(incompatiblePath, incompatible);
        // Auto correctly selects CGB for a color-only upload. Rejection is
        // specifically an explicit SGB/DMG override, not a general limitation.
        await evaluate("document.querySelector('#hardware-model').value = 'sgb'");
        await call('DOM.setFileInputFiles', {nodeId, files: [incompatiblePath]});
        await until("document.body.textContent.includes('CGB-only cartridge')");
        assert.ok(await evaluate("Module._get_sgb_status().includes('commands 1')"));
        console.log('SGB browser: automatic detection, command-driven colors, state restore, explicit incompatible override preservation passed');
      }
    }
    await evaluate("document.querySelector('#quick-pause').click()");
    await until("document.querySelector('#quick-pause').textContent.includes('Resume')");
    await evaluate("document.querySelector('#quick-pause').click()");
    await until("document.querySelector('#quick-pause').textContent.includes('Pause')");
    await evaluate("openSettings()");
    assert.equal(await evaluate("Module._is_paused()"), true, 'settings pause gameplay');
    await evaluate("document.querySelector('#settings-close').click()");
    await until("!Module._is_paused()");
    await evaluate("Module._set_paused(true); openSettings(); document.querySelector('#settings-close').click()");
    assert.equal(await evaluate("Module._is_paused()"), true, 'closing settings preserves a deliberate pause');
    await evaluate("Module._set_paused(false)");
    await checkActualSpeed('Game Boy');
    assert.deepEqual(exceptions, [], 'gameplay browser errors');
  }
  if (process.env.RUSTBOY_SGB_FIRMWARE) {
    await call('Page.reload');
    await until("typeof Module !== 'undefined' && Module?._set_sgb_sound_firmware && document.querySelector('#sgb-firmware-status').textContent.includes('stored in this browser')", 15000);
    const {root: newDocument} = await call('DOM.getDocument');
    const {nodeId: newUpload} = await call('DOM.querySelector', {nodeId: newDocument.nodeId, selector: '#rom-file-input'});
    await call('DOM.setFileInputFiles', {nodeId: newUpload, files: [gamePath]});
    await until("Module._get_sgb_status().includes('SNES audio active')", 15000);
    console.log('SGB firmware browser persistence: page reload and subsequent ROM load passed');
  }
  if (process.env.RUSTBOY_UI_SCREENSHOTS) {
    const capture = async name => {
      const {data} = await call('Page.captureScreenshot', {format:'png'});
      await fs.writeFile(path.join(process.env.RUSTBOY_UI_SCREENSHOTS, name), Buffer.from(data,'base64'), {flag:'wx'});
    };
    await call('Emulation.setDeviceMetricsOverride', {width:1280,height:800,deviceScaleFactor:1,mobile:false});
    await evaluate("openPicker()");
    await capture('library-desktop.png');
    await evaluate("openSettings()");
    await capture('settings-desktop.png');
    await evaluate("document.querySelectorAll('#emulator-settings > details')[2].open = true; document.querySelector('#sgb-audio-settings').open = true");
    await capture('hardware-desktop.png');
    await call('Emulation.setDeviceMetricsOverride', {width:390,height:844,deviceScaleFactor:1,mobile:true});
    // Reload so the viewport meta tag establishes a fresh mobile page scale,
    // rather than retaining the previous desktop layout's scale.
    await call('Page.reload');
    await until("typeof openSettings === 'function' && typeof Module !== 'undefined' && Module?._is_paused");
    await evaluate("openSettings()");
    await capture('settings-mobile.png');
    assert.equal(await evaluate("innerWidth"), 390, 'mobile preview uses device-width viewport');
    assert.ok(await evaluate("(() => { const r = document.querySelector('#emulator-settings').getBoundingClientRect(); return r.left >= 0 && r.right <= innerWidth && r.top >= 0 && r.bottom <= innerHeight; })()"), 'settings fit the mobile viewport');
  }
  await evaluate("openSettings(); document.querySelector('#show-actual-speed').click(); window.actualSpeedReloadPending = true");
  await call('Page.reload');
  await until("!window.actualSpeedReloadPending && typeof Module !== 'undefined' && Module?._get_actual_speed && document.querySelector('#show-actual-speed')");
  assert.equal(await evaluate("document.querySelector('#show-actual-speed').checked && !document.querySelector('#actual-speed').hidden"), true, 'actual speed toggle survives reload');
  await evaluate("openSettings(); document.querySelector('#show-actual-speed').click(); document.querySelector('#settings-close').click()");
  assert.equal(await evaluate("document.querySelector('#actual-speed').hidden"), true);
  assert.deepEqual(exceptions, [], 'settings and initialization browser errors');
  console.log('Browser smoke: WASM initialization, canvas, upload error handling' + (gamePath ? ', rendering, pause/resume' : '') + (process.env.RUSTBOY_NO_BOOT ? (process.env.RUSTBOY_ROM || process.env.RUSTBOY_HOMEBREW ? ', bundled boot without external firmware' : ', RustBoy wordmark and bundled boot without external firmware') : '') + ', grouped settings, keyboard isolation passed');
} finally {
  socket?.close();
  browser.kill();
  await new Promise(resolve => server.close(resolve));
  await fs.rm(temporary, {recursive: true, force: true, maxRetries: 5, retryDelay: 100});
}

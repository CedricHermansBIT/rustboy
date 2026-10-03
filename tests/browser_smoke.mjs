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
const contentTypes = {'.wasm': 'application/wasm', '.js': 'text/javascript', '.mjs': 'text/javascript', '.json': 'application/json', '.html': 'text/html'};
const server = http.createServer(async (request, response) => {
  try {
    const pathname = decodeURIComponent(new URL(request.url, 'http://localhost').pathname);
    if (!pathname.startsWith(basePath + '/')) { response.writeHead(404).end(); return; }
    const name = pathname.slice(basePath.length);
    if (/^\/(roms|testroms)\/romlist\.json$/.test(name)) libraryRequests.push(name);
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
  socket.onmessage = event => {
    const message = JSON.parse(event.data);
    if (message.method === 'Runtime.exceptionThrown') exceptions.push(message.params.exceptionDetails);
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
    throw new Error(`Browser assertion timed out: ${expression}`);
  }
  await call('Page.navigate', {url: `http://127.0.0.1:${server.address().port}${basePath}/`});
  await until("typeof window.mem === 'function'");
  assert.equal(await evaluate("!!document.querySelector('#rustboy-canvas')"), true);
  assert.equal(await evaluate("document.querySelector('#rustboy-canvas').width"), 160);
  await evaluate("document.querySelector('#sgb-user-palette').value = 'warm'; document.querySelector('#sgb-user-palette').dispatchEvent(new Event('change')); document.querySelector('#sgb-user-palette').value = 'game'; document.querySelector('#sgb-user-palette').dispatchEvent(new Event('change'))");
  if (process.env.RUSTBOY_PUBLIC_LIBRARY) {
    await until("ROMS.length === 1");
    assert.deepEqual(await evaluate("Array.from(document.querySelectorAll('.rom-tab')).filter(el => getComputedStyle(el).display !== 'none').map(el => el.dataset.tab)"), ['homebrew']);
    assert.equal(await evaluate("getComputedStyle(document.querySelector('#local-hardware')).display"), 'none');
    assert.deepEqual(libraryRequests, [], 'public picker must not request private libraries');
  }
  await evaluate('initAudio()');
  await until('audioWorkletNode !== null');
  assert.equal(await evaluate('audioWorkletNode.numberOfOutputs'), 1);
  assert.equal(await evaluate('audioWorkletNode.channelCountMode'), 'max');
  const {root: document} = await call('DOM.getDocument');
  const {nodeId} = await call('DOM.querySelector', {nodeId: document.nodeId, selector: '#rom-file-input'});
  const invalidRom = path.join(temporary, 'truncated.gb');
  await fs.writeFile(invalidRom, new Uint8Array(8));
  await call('DOM.setFileInputFiles', {nodeId, files: [invalidRom]});
  await until("document.body.textContent.includes('ROM is truncated')");
  assert.deepEqual(exceptions, [], 'unhandled browser errors');
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
      assert.equal(await evaluate("(async () => (await import('./out/rustboy.js')).get_is_cgb())()"), entry.platform !== 'GB');
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
  console.log('Browser smoke: WASM initialization, canvas, upload error handling' + (gamePath ? ', rendering, pause/resume' : '') + (process.env.RUSTBOY_NO_BOOT ? (process.env.RUSTBOY_ROM || process.env.RUSTBOY_HOMEBREW ? ', bundled boot without external firmware' : ', RustBoy wordmark and bundled boot without external firmware') : '') + ' passed');
} finally {
  socket?.close();
  browser.kill();
  await new Promise(resolve => server.close(resolve));
  await fs.rm(temporary, {recursive: true, force: true, maxRetries: 5, retryDelay: 100});
}

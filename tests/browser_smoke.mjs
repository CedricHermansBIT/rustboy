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
const contentTypes = {'.wasm': 'application/wasm', '.js': 'text/javascript', '.mjs': 'text/javascript', '.json': 'application/json', '.html': 'text/html'};
const server = http.createServer(async (request, response) => {
  try {
    const pathname = decodeURIComponent(new URL(request.url, 'http://localhost').pathname);
    if (!pathname.startsWith(basePath + '/')) { response.writeHead(404).end(); return; }
    const name = pathname.slice(basePath.length);
    // Model the deployed site: no user-supplied firmware is present.
    if (process.env.RUSTBOY_NO_BOOT && /^\/roms\/(dmg|cgb)_boot\.bin$/.test(name)) {
      response.writeHead(404).end(); return;
    }
    const file = path.resolve(site, '.' + (name === '/' ? '/index.html' : name));
    if (!file.startsWith(site + path.sep)) { response.writeHead(403).end(); return; }
    response.setHeader('Content-Type', contentTypes[path.extname(file)] || 'application/octet-stream');
    response.end(await fs.readFile(file));
  } catch { response.writeHead(404).end(); }
});
await new Promise((resolve, reject) => {
  server.once('error', reject);
  server.listen(0, '127.0.0.1', resolve);
});
const browser = spawn(process.env.RUSTBOY_BROWSER || 'chromium-browser', [
  '--headless=new', '--no-sandbox', '--disable-dev-shm-usage', '--disable-gpu',
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
  const {root: document} = await call('DOM.getDocument');
  const {nodeId} = await call('DOM.querySelector', {nodeId: document.nodeId, selector: '#rom-file-input'});
  const invalidRom = path.join(temporary, 'truncated.gb');
  await fs.writeFile(invalidRom, new Uint8Array(8));
  await call('DOM.setFileInputFiles', {nodeId, files: [invalidRom]});
  await until("document.body.textContent.includes('ROM is truncated')");
  assert.deepEqual(exceptions, [], 'unhandled browser errors');
  let gamePath = process.env.RUSTBOY_ROM && path.resolve(process.env.RUSTBOY_ROM);
  const borderTest = !!process.env.RUSTBOY_SGB_BORDER;
  if (borderTest) {
    gamePath = path.join(temporary, 'sgb-border.gb');
    await promisify(execFile)('cargo', ['run', '--locked', '--offline', '--release', '--no-default-features',
      '--example', 'sgb_border_fixture', '--', gamePath], {cwd: root, timeout: 120000});
    await evaluate("document.querySelector('#hardware-model').value = 'sgb'; document.querySelector('#hardware-model').dispatchEvent(new Event('change'))");
  }
  if (!gamePath && process.env.RUSTBOY_NO_BOOT && !process.env.RUSTBOY_HOMEBREW) {
    // Public-domain synthetic program: draw a repeating tile and publish 0x66.
    const rom = new Uint8Array(32768);
    rom.set([0xC3, 0x50, 0x01], 0x100);
    rom.fill(0xAA, 0x104, 0x134);
    rom[0x143] = process.env.RUSTBOY_SYNTHETIC_CGB ? 0x80 : 0;
    if (process.env.RUSTBOY_SYNTHETIC_SGB) {
      rom[0x143] = 0x80; // Dual-mode cart explicitly running on the SGB/DMG path.
      rom[0x146] = 3; rom[0x14B] = 0x33;
      await evaluate("document.querySelector('#hardware-model').value = 'sgb'; document.querySelector('#hardware-model').dispatchEvent(new Event('change'))");
      assert.ok(await evaluate("!document.querySelector('#sgb-warning').classList.contains('hidden')"));
    }
    const code = [0xF3, 0xAF, 0xE0, 0x40, 0x3E, 0xE4, 0xE0, 0x47,
      0x21, 0x00, 0x80, 0x06, 0x10, 0x3E, 0xAA, 0x22, 0x05, 0x20, 0xFC,
      0x3E, 0x80, 0xE0, 0x68];
    for (const color of [0xFF, 0x7F, 0, 0, 0, 0, 0, 0]) code.push(0x3E, color, 0xE0, 0x69);
    if (process.env.RUSTBOY_SYNTHETIC_SGB) {
      // PAL01 through real cartridge instructions, with recommended low/high
      // pulse spacing. White backdrop, shade 3 red instead of handheld black.
      const pulse = value => code.push(0x3E, value, 0xE0, 0, ...Array(5).fill(0), 0x3E, 0x30, 0xE0, 0, ...Array(15).fill(0));
      pulse(0);
      for (const byte of [1, 0xFF, 0x7F, 0xE0, 3, 0, 0x7C, 31, 0, 0, 0, 0, 0, 0, 0, 0]) {
        for (let bit = 0; bit < 8; bit++) pulse(byte & (1 << bit) ? 0x10 : 0x20);
      }
      pulse(0x20);
    }
    code.push(0x3E, 0x91, 0xE0, 0x40, 0x3E, 0x66, 0xEA, 0x00, 0xC0, 0x18, 0xFE);
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
    await new Promise(resolve => setTimeout(resolve, 6000));
    if (borderTest) {
      await until("document.querySelector('#rustboy-canvas').width === 256 && window.mem(0xC000) === '0x66'");
      const geometry = await evaluate("(() => { const c = document.querySelector('#rustboy-canvas'); return [c.width,c.height,c.style.aspectRatio]; })()");
      assert.deepEqual(geometry, [256,224,'8 / 7']);
      const samples = await evaluate("(() => { const ctx = document.querySelector('#rustboy-canvas').getContext('2d'); return [[0,0],[8,0],[48,40],[49,40],[56,48]].map(([x,y]) => Array.from(ctx.getImageData(x,y,1,1).data)); })()");
      assert.deepEqual(samples, [[0,255,0,255],[255,0,0,255],[173,173,173,255],[255,255,255,255],[255,0,0,255]]);
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
    const colors = await evaluate("new Set(new Uint32Array(document.querySelector('#rustboy-canvas').getContext('2d').getImageData(0,0,160,144).data.buffer)).size");
    if (!borderTest) assert.ok(colors > 1, 'game canvas must render nonblank content');
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
        await call('DOM.setFileInputFiles', {nodeId, files: [incompatiblePath]});
        await until("document.body.textContent.includes('CGB-only cartridge')");
        assert.ok(await evaluate("Module._get_sgb_status().includes('commands 1')"));
        console.log('SGB browser: explicit model, command-driven colors, state restore, incompatible upload preservation passed');
      }
    }
    await evaluate("document.querySelector('#quick-pause').click()");
    await until("document.querySelector('#quick-pause').textContent.includes('Resume')");
    await evaluate("document.querySelector('#quick-pause').click()");
    await until("document.querySelector('#quick-pause').textContent.includes('Pause')");
    assert.deepEqual(exceptions, [], 'gameplay browser errors');
  }
  console.log('Browser smoke: WASM initialization, canvas, upload error handling' + (gamePath ? ', rendering, pause/resume' : '') + (process.env.RUSTBOY_NO_BOOT ? (process.env.RUSTBOY_ROM || process.env.RUSTBOY_HOMEBREW ? ', bundled boot without external firmware' : ', RustBoy wordmark and bundled boot without external firmware') : '') + ' passed');
} finally {
  socket?.close();
  browser.kill();
  await new Promise(resolve => server.close(resolve));
  await fs.rm(temporary, {recursive: true, force: true, maxRetries: 5, retryDelay: 100});
}

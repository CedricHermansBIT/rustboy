// A real-browser smoke test using Chromium's built-in DevTools protocol.
// No npm dependencies. RUSTBOY_BROWSER can override the Chromium executable.
// RUSTBOY_ROM optionally adds a gameplay smoke test with your local ROM/BIOS.
import assert from 'node:assert/strict';
import {spawn} from 'node:child_process';
import fs from 'node:fs/promises';
import http from 'node:http';
import os from 'node:os';
import path from 'node:path';
import {fileURLToPath} from 'node:url';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const temporary = await fs.mkdtemp(path.join(os.tmpdir(), 'rustboy-browser-test-'));
const contentTypes = {'.wasm': 'application/wasm', '.js': 'text/javascript', '.mjs': 'text/javascript', '.json': 'application/json', '.html': 'text/html'};
const server = http.createServer(async (request, response) => {
  try {
    const name = decodeURIComponent(new URL(request.url, 'http://localhost').pathname);
    // Model the deployed site: no user-supplied firmware is present.
    if (process.env.RUSTBOY_NO_BOOT && /^\/roms\/(dmg|cgb)_boot\.bin$/.test(name)) {
      response.writeHead(404).end(); return;
    }
    const file = path.resolve(root, '.' + (name === '/' ? '/index.html' : name));
    if (!file.startsWith(root + path.sep)) { response.writeHead(403).end(); return; }
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
  await call('Page.navigate', {url: `http://127.0.0.1:${server.address().port}/`});
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
  if (!gamePath && process.env.RUSTBOY_NO_BOOT) {
    // Public-domain synthetic program: draw a repeating tile and publish 0x66.
    const rom = new Uint8Array(32768);
    rom.set([0xC3, 0x50, 0x01], 0x100);
    rom.fill(0xAA, 0x104, 0x134);
    rom[0x143] = process.env.RUSTBOY_SYNTHETIC_CGB ? 0x80 : 0;
    const code = [0xF3, 0xAF, 0xE0, 0x40, 0x3E, 0xE4, 0xE0, 0x47,
      0x21, 0x00, 0x80, 0x06, 0x10, 0x3E, 0xAA, 0x22, 0x05, 0x20, 0xFC,
      0x3E, 0x80, 0xE0, 0x68];
    for (const color of [0xFF, 0x7F, 0, 0, 0, 0, 0, 0]) code.push(0x3E, color, 0xE0, 0x69);
    code.push(0x3E, 0x91, 0xE0, 0x40, 0x3E, 0x66, 0xEA, 0x00, 0xC0, 0x18, 0xFE);
    rom.set(code, 0x150);
    gamePath = path.join(temporary, rom[0x143] ? 'synthetic.gbc' : 'synthetic.gb');
    await fs.writeFile(gamePath, rom);
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
    const colors = await evaluate("new Set(new Uint32Array(document.querySelector('#rustboy-canvas').getContext('2d').getImageData(0,0,160,144).data.buffer)).size");
    assert.ok(colors > 1, 'game canvas must render nonblank content');
    if (!process.env.RUSTBOY_ROM && process.env.RUSTBOY_NO_BOOT) {
      assert.equal(await evaluate("window.mem(0xC000)"), '0x66', 'replacement boot must reach the cartridge');
      assert.ok(await evaluate("(async () => (await import('./out/rustboy.js')).get_boot_rom_license().includes('Lior Halphon'))()"));
    }
    await evaluate("document.querySelector('#quick-pause').click()");
    await until("document.querySelector('#quick-pause').textContent.includes('Resume')");
    await evaluate("document.querySelector('#quick-pause').click()");
    await until("document.querySelector('#quick-pause').textContent.includes('Pause')");
    assert.deepEqual(exceptions, [], 'gameplay browser errors');
  }
  console.log('Browser smoke: WASM initialization, canvas, upload error handling' + (gamePath ? ', rendering, pause/resume' : '') + (process.env.RUSTBOY_NO_BOOT ? (process.env.RUSTBOY_ROM ? ', bundled boot without external firmware' : ', RustBoy wordmark and bundled boot without external firmware') : '') + ' passed');
} finally {
  socket?.close();
  browser.kill();
  await new Promise(resolve => server.close(resolve));
  await fs.rm(temporary, {recursive: true, force: true, maxRetries: 5, retryDelay: 100});
}

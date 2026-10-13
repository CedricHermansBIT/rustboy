// Imported by the repository's real Chromium/WASM smoke workflow. The same
// checks also run in the collaborative browser, without a mock emulator.
export async function checkGbaSaveFailures(evaluate, assert) {
  await evaluate(`(async () => {
    const rom=new Uint8Array(512);
    new DataView(rom.buffer).setUint32(0,0xeafffffe,true);
    rom[0xb2]=0x96; rom.set(new TextEncoder().encode('SRAM_V110'),0x100);
    window.saveReliabilityRom=rom;
    localStorage.setItem('rustboy_auto_resume','0');
    await Module._load_rom_data(rom,'original-save-reliability.gba'); Module._set_paused(true);
    window.saveReliabilityKey=Module._get_save_key();
    const bytes=new Uint8Array(32768).fill(255);bytes[0]=42;
    Module._import_save_data(bytes); storeSaveData(saveReliabilityKey,bytes);
    window.saveReliabilityOriginal=localStorage.getItem(saveReliabilityKey);
    Module._clear_save_data();
    document.querySelector('#toast-stack').replaceChildren();
    window.saveReliabilitySet=Storage.prototype.setItem;
    Storage.prototype.setItem=function(key,value) {
      if (key===saveReliabilityKey) throw new DOMException('Synthetic quota failure','QuotaExceededError');
      return saveReliabilitySet.call(this,key,value);
    };
    Module._save_game();Module._save_game();
  })()`);
  try {
    assert.equal(await evaluate('localStorage.getItem(saveReliabilityKey)===saveReliabilityOriginal'),true,'quota failure preserves the previous battery save');
    assert.equal(await evaluate("document.querySelectorAll('#toast-stack .toast.error').length"),1,'autosave failures produce one visible notification');
    assert.equal(await evaluate("document.querySelector('#toast-stack').textContent.includes('browser storage is full')"),true);
    assert.equal(await evaluate('Module._export_save_data()[0]'),255,'live game progress survives a failed persistence attempt');
  } finally {
    await evaluate('Storage.prototype.setItem=saveReliabilitySet');
  }
  await evaluate('Module._save_game()');
  assert.equal(await evaluate('atob(localStorage.getItem(saveReliabilityKey)).charCodeAt(0)'),255,'WASM retained dirty state and retries after quota recovery');
  assert.equal(await evaluate("document.querySelector('#toast-stack').textContent.includes('saving is working again')"),true);

  // This is a genuine pending mutation, unlike importing a clean battery blob.
  // Loading the same ROM must flush it BEFORE reading that ROM's stored bytes.
  await evaluate(`(async()=>{
    const old=new Uint8Array(32768).fill(255);old[0]=77;
    Module._import_save_data(old);storeSaveData(saveReliabilityKey,old);
    Module._clear_save_data();
    await Module._load_rom_data(saveReliabilityRom,'original-save-reliability.gba');Module._set_paused(true);
  })()`);
  assert.equal(await evaluate('Module._export_save_data()[0]'),255,'same-ROM reload imports the latest dirty cartridge progress');
  assert.equal(await evaluate('atob(localStorage.getItem(saveReliabilityKey)).charCodeAt(0)'),255);

  await evaluate(`(async()=>{
    localStorage.setItem(saveReliabilityKey,'!corrupted battery data!');
    document.querySelector('#toast-stack').replaceChildren();
    await Module._load_rom_data(saveReliabilityRom,'original-save-reliability.gba');Module._set_paused(true);
    const changed=new Uint8Array(32768).fill(255);changed[0]=11;
    Module._import_save_data(changed);Module._clear_save_data();Module._save_game();
  })()`);
  assert.equal(await evaluate("localStorage.getItem(saveReliabilityKey)"),'!corrupted battery data!','ROM reload and autosave cannot overwrite malformed persisted data');
  assert.equal(await evaluate("document.querySelector('#toast-stack').textContent.includes('Could not read the cartridge save')"),true,'corrupt persisted data is visible in the interface');
  await evaluate("localStorage.removeItem(saveReliabilityKey); Module._save_game()");
  assert.equal(await evaluate('atob(localStorage.getItem(saveReliabilityKey)).charCodeAt(0)'),255,'explicitly removing corrupt data permits the pending save retry');
  console.log('GBA browser saves: quota recovery preserves WASM dirty state, same-ROM reload flushes current progress, and corrupt data remains recoverable');
}

export async function checkGbaSaveOverrides(evaluate, assert) {
  const setup = await evaluate(`(async()=>{
    const wasm=await import('./out/rustboy.js');
    const rom=new Uint8Array(512);new DataView(rom.buffer).setUint32(0,0xeafffffe,true);
    rom[0xb2]=0x96;rom.set(new TextEncoder().encode('FLASH_V123'),0x100);
    window.saveOverrideRom=rom;window.saveOverrideKey=wasm.get_gba_rom_save_key(rom);
    const bytes=new Uint8Array(131072).fill(255);bytes[0]=23;
    storeSaveData(saveOverrideKey,bytes);
    const before=Module._get_state_id();
    let rejected=false;try {await Module._load_rom_data(rom,'original-override.gba');} catch {rejected=true;}
    return {rejected,preserved:Module._get_state_id()===before};
  })()`);
  assert.equal(setup.rejected,true,'a wrong detected save size does not silently discard a larger battery file');
  assert.equal(setup.preserved,true,'a rejected load preserves the running cartridge');
  await evaluate(`(async()=>{
    const select=document.querySelector('#gba-save-type');select.value='flash128';select.dispatchEvent(new Event('change'));
    await Module._load_rom_data(saveOverrideRom,'original-override.gba');Module._set_paused(true);
  })()`);
  assert.equal(await evaluate('Module._export_save_data().length'),131072);
  assert.equal(await evaluate('Module._export_save_data()[0]'),23,'the explicit override is applied before importing saved bytes');
  assert.equal(await evaluate("localStorage.getItem(saveOverrideKey+'_type')"),'flash128');
  assert.equal(await evaluate("document.querySelector('#gba-save-type').value"),'flash128');
  await evaluate("document.querySelector('#gba-save-type').value='none';document.querySelector('#gba-save-type').dispatchEvent(new Event('change'))");
  assert.equal(await evaluate('Module._export_save_data()[0]'),23,'unsafe hardware changes preserve cartridge progress');
  assert.equal(await evaluate("document.querySelector('#gba-save-type').value"),'flash128');
  await evaluate("(async()=>{await Module._load_rom_data(saveOverrideRom,'original-override.gba');Module._set_paused(true);})()");
  assert.equal(await evaluate('Module._export_save_data()[0]'),23,'the remembered per-ROM override permits future loads');
  console.log('GBA browser save overrides: settings repair a wrong detected size before import, preserve data and persist per cartridge');
}

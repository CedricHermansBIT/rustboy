const TYPES = new Set(['auto', 'none', 'sram', 'flash64', 'flash128', 'eeprom']);
export function installGbaSaveSettings(api, document, notify) {
  const select = document.querySelector('#gba-save-type');
  const status = document.querySelector('#gba-save-type-status');
  if (!select) return {load: api.load_gba_data};
  let loadedKey = '', attemptedKey = '', failed = false, next = 'auto';
  const supported = !!api.load_gba_data_with_save_type && !!api.get_gba_rom_save_key;
  select.disabled = !supported;
  const read = key => {
    try { const value = localStorage.getItem(key + '_type'); return TYPES.has(value) ? value : 'auto'; }
    catch { return 'auto'; }
  };
  const remember = (key, type) => {
    try {
      if (type === 'auto') localStorage.removeItem(key + '_type');
      else localStorage.setItem(key + '_type', type);
    } catch { notify('Save hardware changed, but this browser could not remember the setting.', true); }
  };
  const refresh = () => {
    const actual = api.get_gba_save_type?.();
    if (!actual) select.value = next;
    status.textContent = !supported ? 'Refresh the page to use save hardware overrides.'
      : failed ? 'Choose the save hardware for the game that failed to load, then load it again.'
      : actual ? `Current cartridge save hardware: ${actual}. Changes preserve existing save bytes.`
      : 'Your choice applies to the next GBA game you load.';
  };
  select.addEventListener('change', () => {
    const type = select.value;
    if (!TYPES.has(type)) return;
    if (failed || !api.get_gba_save_type?.()) {
      next = type;
      if (failed && attemptedKey) remember(attemptedKey, type);
      refresh(); return;
    }
    try {
      api.set_gba_save_type(type);
      const key = loadedKey || api.get_save_key();
      remember(key, type);
      refresh();
    } catch (error) {
      select.value = read(loadedKey);
      notify(`Could not change save hardware: ${error.message || error}`, true);
    }
  });
  refresh();
  return {
    load(rom, bios) {
      if (!supported) return api.load_gba_data(rom, bios);
      // Native integer hashing avoids a slow JavaScript BigInt pass over ROMs.
      attemptedKey = api.get_gba_rom_save_key(rom);
      const type = next !== 'auto' ? next : read(attemptedKey);
      try {
        const result = api.load_gba_data_with_save_type(rom, bios, type);
        loadedKey = attemptedKey; failed = false; next = 'auto';
        select.value = type;
        if (type !== 'auto') remember(loadedKey, type);
        refresh(); return result;
      } catch (error) {
        failed = true; select.value = type; refresh(); throw error;
      }
    },
    refresh,
  };
}

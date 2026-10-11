//! Derived scanline dependency cache. Guest timing and current pixels are exact.
//! Writes change versions only if storage changes. Host/direct BIOS mutations
//! are reconciled at entry boundaries against a shadow, not ignored.
use crate::{bus::Bus, ppu::Affine};
const PAGES: usize = 392;
const BYTES: usize = PAGES * 256;
pub(crate) struct Storage {
    versions: [u64; PAGES],
    shadow: Vec<u8>,
}
impl Default for Storage {
    fn default() -> Self {
        Self {
            versions: [0; PAGES],
            shadow: vec![0; BYTES],
        }
    }
}
impl Storage {
    pub fn write(&mut self, base: usize, offset: usize, old: u8, value: u8) {
        if old != value {
            self.versions[base + offset / 256] += 1;
        }
        self.shadow[base * 256 + offset] = value;
    }
    fn synchronize(&mut self, base: usize, data: &[u8]) {
        for (page, bytes) in data.as_chunks::<256>().0.iter().enumerate() {
            let start = (base + page) * 256;
            let old = &mut self.shadow[start..start + 256];
            if old != bytes {
                self.versions[base + page] += 1;
                old.copy_from_slice(bytes);
            }
        }
    }
}
impl Bus {
    pub(crate) fn sync_graphics(&mut self) {
        self.graphics.synchronize(0, &self.vram);
        self.graphics.synchronize(384, &self.palette);
        self.graphics.synchronize(388, &self.oam);
    }
}
#[derive(Default)]
struct Row {
    valid: bool,
    misses: u8,
    cooldown: u8,
    // Display registers including scroll, window, blending and mosaic; omit
    // DISPSTAT/VCOUNT, which change with time and are not compositor inputs.
    registers: Vec<u8>,
    affine: Affine,
    dependencies: Vec<(usize, u64)>,
}
pub(crate) struct Cache {
    pub debug_step: bool,
    rows: [Row; 160],
    pub hits: u64,
    pub misses: u64,
}
impl Default for Cache {
    fn default() -> Self {
        Self {
            debug_step: false,
            rows: std::array::from_fn(|_| Row::default()),
            hits: 0,
            misses: 0,
        }
    }
}
impl Cache {
    pub fn clear(&mut self) {
        for row in &mut self.rows {
            row.valid = false;
        }
    }
    pub fn matches(&mut self, bus: &Bus, line: u16) -> bool {
        let row = &mut self.rows[line as usize];
        let valid = row.valid
            && row.affine == bus.affine
            && row.registers[..4] == bus.io[..4]
            && row.registers[4..] == bus.io[8..0x56]
            && row
                .dependencies
                .iter()
                .all(|&(page, version)| bus.graphics.versions[page] == version);
        if valid {
            self.hits += 1;
            row.misses = 0;
            row.cooldown = 0;
        } else {
            self.misses += 1;
        }
        valid
    }
    /// Dynamic scenes back off dependency recording. They still compose exact
    /// pixels through the uninstrumented renderer; a cold row is never reused.
    pub fn track(&mut self, line: u16) -> bool {
        let row = &mut self.rows[line as usize];
        if row.cooldown != 0 {
            row.cooldown -= 1;
            row.valid = false;
            return false;
        }
        row.misses = row.misses.saturating_add(1);
        if row.misses >= 2 {
            row.cooldown = 16;
        }
        true
    }
    pub fn record(&mut self, bus: &Bus, line: u16, reads: [u64; 7]) {
        let row = &mut self.rows[line as usize];
        row.valid = true;
        row.affine = bus.affine;
        row.registers.clear();
        row.registers.extend_from_slice(&bus.io[..4]);
        row.registers.extend_from_slice(&bus.io[8..0x56]);
        row.dependencies.clear();
        for (word, mut bits) in reads.into_iter().enumerate() {
            while bits != 0 {
                let page = word * 64 + bits.trailing_zeros() as usize;
                row.dependencies.push((page, bus.graphics.versions[page]));
                bits &= bits - 1;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cpu::Memory;
    #[test]
    fn cached_pixels_follow_guest_and_host_changes_in_every_display_mode() {
        let mut bus = Bus::new(vec![0; 192], None).unwrap();
        let mut cache = Cache::default();
        let mut cached = vec![255; 240 * 160 * 4];
        let mut reference = cached.clone();
        let mut seed = 0x174ca322u32;
        let mut random = || {
            seed ^= seed << 13;
            seed ^= seed >> 17;
            seed ^= seed << 5;
            seed
        };
        for data in [&mut bus.vram, &mut bus.palette, &mut bus.oam] {
            for byte in data.iter_mut() {
                *byte = random() as u8;
            }
        }
        for mode in 0..=5 {
            bus.set_io(0, mode | 0xff00);
            for frame in 0..12 {
                // Different paths: tracked CPU/DMA writes, direct public memory
                // edits, raster registers, affine/mosaic, forced blank.
                for offset in [
                    8, 10, 12, 14, 0x10, 0x12, 0x40, 0x42, 0x44, 0x46, 0x48, 0x4a, 0x4c, 0x50,
                    0x52, 0x54,
                ] {
                    bus.set_io(offset, random() as u16);
                }
                for region in [5, 6, 7] {
                    let limit = if region == 6 { 0x18000 } else { 0x400 };
                    for _ in 0..24 {
                        bus.write16(
                            (region << 24) | ((random() % limit) & !1),
                            random() as u16,
                            false,
                        );
                    }
                }
                bus.vram[random() as usize % 0x18000] ^= 255;
                bus.palette[random() as usize % 0x400] ^= 255;
                bus.oam[random() as usize % 0x400] ^= 255;
                bus.sync_graphics();
                bus.affine.current = [[random() as i32, random() as i32]; 2];
                bus.affine.mosaic = [[random() as i32, random() as i32]; 2];
                bus.set_io(0, mode | 0xff00 | if frame == 9 { 0x80 } else { 0 });
                for line in [0, 1, 17, 79, 158, 159] {
                    crate::ppu::render_cached(&bus, line, &mut cached, &mut cache);
                    crate::ppu::render_line(&bus, line, &mut reference);
                    assert_eq!(cached, reference, "mode={mode} frame={frame} line={line}");

                    crate::ppu::render_cached(&bus, line, &mut cached, &mut cache);
                    assert_eq!(cached, reference);
                }
            }
        }
    }
    #[test]
    fn public_runs_reconcile_host_memory_and_restores_discard_cached_rows() {
        use crate::{GameBoyAdvance, FRAME_CYCLES};
        use rustboy_emulator_api::Emulator;
        let mut rom = vec![0; 192];
        rom[..4].copy_from_slice(&0xeafffffeu32.to_le_bytes());
        let mut machine = GameBoyAdvance::new(rom, None).unwrap();
        machine.bus.set_io(0, 0);
        machine.run(FRAME_CYCLES * 3);
        assert!(machine.rendering.hits > 0);
        let saved = machine.export_state();
        machine.bus.palette[..2].copy_from_slice(&31u16.to_le_bytes());
        machine.run(FRAME_CYCLES);
        assert_eq!(&machine.pixels[..4], &[255, 0, 0, 255]);
        // Debug stepping does not leave dependencies derived from an
        // unsynchronized public-memory edit. Reverting that edit is safe too.
        machine.bus.halted = true;
        machine.bus.palette[..2].copy_from_slice(&0x3e0u16.to_le_bytes());
        for _ in 0..4 {
            machine.step();
        }
        machine.bus.palette[..2].copy_from_slice(&31u16.to_le_bytes());
        machine.run(FRAME_CYCLES);
        assert_eq!(&machine.pixels[..4], &[255, 0, 0, 255]);
        machine.import_state(&saved).unwrap();
        assert_eq!(machine.rendering.hits, 0);
        machine.run(FRAME_CYCLES);
        assert_eq!(&machine.pixels[..4], &[0, 0, 0, 255]);
    }
    #[test]
    fn unchanged_and_unread_writes_keep_rows_but_sampled_writes_invalidate() {
        let mut bus = Bus::new(vec![0; 192], None).unwrap();
        let mut cache = Cache::default();
        let mut pixels = vec![255; 240 * 160 * 4];
        bus.set_io(0, 0);
        bus.sync_graphics();
        crate::ppu::render_cached(&bus, 0, &mut pixels, &mut cache);
        // Only the backdrop is used with all layers disabled.
        bus.write16(0x06000000, 0xabcd, false);
        bus.write16(0x05000000, 0, false);
        crate::ppu::render_cached(&bus, 0, &mut pixels, &mut cache);
        assert_eq!((cache.hits, cache.misses), (1, 1));
        bus.write16(0x05000000, 0x001f, false);
        crate::ppu::render_cached(&bus, 0, &mut pixels, &mut cache);
        assert_eq!((cache.hits, cache.misses), (1, 2));
        assert_eq!(&pixels[..4], &[255, 0, 0, 255]);
    }
}

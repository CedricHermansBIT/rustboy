#[cfg(target_arch = "wasm32")]
use crate::cpu::CPU;
#[cfg(target_arch = "wasm32")]
use wasm_bindgen::Clamped;

mod fetch;
mod window;
mod objects;
use fetch::{fetch_background_tile, fetch_window_tile};

/// Snapshot of PPU-rendering registers captured at the Mode 2→3 boundary.
#[derive(Copy, Clone, Default)]
pub struct PpuLineSnapshot {
    pub lcdc: u8,
    pub scy:  u8,
    pub scx:  u8,
    pub bgp:  u8,
    pub obp0: u8,
    pub obp1: u8,
    pub wx:   u8,
    pub wy:   u8,
}
/// A single PPU register change that occurred during Mode 3 drawing.
#[derive(Copy, Clone, Default)]
pub struct PpuRegChange {
    /// T-cycle within the scanline (from Mode-2 start) when the write occurred.
    pub dot:   u16,
    /// I/O address (e.g. 0xFF40).
    pub addr:  u16,
    /// Value written.
    pub value: u8,
}

// Default DMG-style green palette (used as fallback)
pub const DEFAULT_GBC_PALETTES: [[[u8; 4]; 4]; 3] =[
    // BG
    [[224, 248, 208, 255],[136, 192, 112, 255],[52, 104, 86, 255],[8, 24, 32, 255]],
    // OBP0
    [[224, 248, 208, 255],[136, 192, 112, 255], [52, 104, 86, 255],[8, 24, 32, 255]],
    // OBP1
    [[224, 248, 208, 255],[136, 192, 112, 255],[52, 104, 86, 255], [8, 24, 32, 255]],
];

// ─── CGB Boot ROM palette data ──────────────────────────────────────────────
type Pal = [[u8; 4]; 4]; // 4 colors, each RGBA

/// Compute the GBC title checksum: sum of bytes 0x134..=0x143
fn title_checksum(rom: &[u8]) -> u8 {
    let mut sum: u8 = 0;
    for i in 0x134..=0x143 {
        sum = sum.wrapping_add(*rom.get(i).unwrap_or(&0));
    }
    sum
}

// Helper to build an RGBA color
const fn c(r: u8, g: u8, b: u8) -> [u8; 4] { [r, g, b, 255] }

const W: [u8; 4] = c(0xFF, 0xFF, 0xFF); // white
const K: [u8; 4] = c(0x00, 0x00, 0x00); // black

// ─── Direct combo palette data ──────────────────────────────────────────────
const COMBO_PALETTES: [[[[u8; 4]; 4]; 3]; 45] = [
    // 0
    [[W, c(0xAD,0xAD,0x84), c(0x42,0x73,0x7B), K],[W, c(0xFF,0x73,0x00), c(0x94,0x42,0x00), K],[W, c(0xFF,0x73,0x00), c(0x94,0x42,0x00), K]],
    // 1
    [[W, c(0xAD,0xAD,0x84), c(0x42,0x73,0x7B), K],[W, c(0xFF,0x73,0x00), c(0x94,0x42,0x00), K],[W, c(0x5A,0xBD,0xFF), c(0xFF,0x00,0x00), c(0x00,0x00,0xFF)]],
    // 2
    [[c(0xFF,0xFF,0x9C), c(0x94,0xB5,0xFF), c(0x63,0x94,0x73), c(0x00,0x3A,0x3A)],[c(0xFF,0xFF,0x9C), c(0x94,0xB5,0xFF), c(0x63,0x94,0x73), c(0x00,0x3A,0x3A)],[c(0xFF,0xFF,0x9C), c(0x94,0xB5,0xFF), c(0x63,0x94,0x73), c(0x00,0x3A,0x3A)]],
    // 3
    [[c(0xFF,0xFF,0x9C), c(0x94,0xB5,0xFF), c(0x63,0x94,0x73), c(0x00,0x3A,0x3A)],[c(0xFF,0xC5,0x42), c(0xFF,0xD6,0x00), c(0x94,0x3A,0x00), c(0x4A,0x00,0x00)],[W, c(0xFF,0x84,0x84), c(0x94,0x3A,0x3A), K]],
    // 4
    [[c(0x6B,0xFF,0x00), W, c(0xFF,0x52,0x4A), K],[W, W, c(0x63,0xA5,0xFF), c(0x00,0x00,0xFF)],[W, c(0xFF,0xAD,0x63), c(0x84,0x31,0x00), K]],
    // 5
    [[c(0x52,0xDE,0x00), c(0xFF,0x84,0x00), c(0xFF,0xFF,0x00), W],[W, W, c(0x63,0xA5,0xFF), c(0x00,0x00,0xFF)],[W, c(0xFF,0x84,0x84), c(0x94,0x3A,0x3A), K]],
    // 6
    [[W, c(0x7B,0xFF,0x00), c(0xB5,0x73,0x00), K],[W, c(0xFF,0x84,0x84), c(0x94,0x3A,0x3A), K],[W, c(0xFF,0x84,0x84), c(0x94,0x3A,0x3A), K]],
    // 7
    [[W, c(0x52,0xFF,0x00), c(0xFF,0x42,0x00), K],[W, c(0xFF,0x84,0x84), c(0x94,0x3A,0x3A), K],[W, c(0xFF,0x84,0x84), c(0x94,0x3A,0x3A), K]],
    // 8
    [[W, c(0x52,0xFF,0x00), c(0xFF,0x42,0x00), K],[W, c(0x52,0xFF,0x00), c(0xFF,0x42,0x00), K],[W, c(0x5A,0xBD,0xFF), c(0xFF,0x00,0x00), c(0x00,0x00,0xFF)]],
    // 9
    [[W, c(0xFF,0x9C,0x00), c(0xFF,0x00,0x00), K],[W, c(0xFF,0x9C,0x00), c(0xFF,0x00,0x00), K],[W, c(0xFF,0x9C,0x00), c(0xFF,0x00,0x00), K]],
    // 10
    [[W, c(0xFF,0x9C,0x00), c(0xFF,0x00,0x00), K],[W, c(0xFF,0x84,0x84), c(0x94,0x3A,0x3A), K],[W, c(0xFF,0x84,0x84), c(0x94,0x3A,0x3A), K]],
    // 11
    [[W, c(0xFF,0x9C,0x00), c(0xFF,0x00,0x00), K],[W, c(0xFF,0x9C,0x00), c(0xFF,0x00,0x00), K],[W, c(0x5A,0xBD,0xFF), c(0xFF,0x00,0x00), c(0x00,0x00,0xFF)]],
    // 12
    [[W, c(0xFF,0xFF,0x00), c(0xFF,0x00,0x00), K],[W, c(0xFF,0xFF,0x00), c(0xFF,0x00,0x00), K],[W, c(0xFF,0xFF,0x00), c(0xFF,0x00,0x00), K]],
    // 13
    [[W, c(0xFF,0xFF,0x00), c(0xFF,0x00,0x00), K],[W, c(0xFF,0xFF,0x00), c(0xFF,0x00,0x00), K],[W, c(0x5A,0xBD,0xFF), c(0xFF,0x00,0x00), c(0x00,0x00,0xFF)]],
    // 14
    [[c(0xA5,0x9C,0xFF), c(0xFF,0xFF,0x00), c(0x00,0x63,0x00), K],[c(0xA5,0x9C,0xFF), c(0xFF,0xFF,0x00), c(0x00,0x63,0x00), K],[c(0xA5,0x9C,0xFF), c(0xFF,0xFF,0x00), c(0x00,0x63,0x00), K]],
    // 15
    [[c(0xA5,0x9C,0xFF), c(0xFF,0xFF,0x00), c(0x00,0x63,0x00), K],[c(0xFF,0x63,0x52), c(0xD6,0x00,0x00), c(0x63,0x00,0x00), K],[c(0xFF,0x63,0x52), c(0xD6,0x00,0x00), c(0x63,0x00,0x00), K]],
    // 16
    [[c(0xA5,0x9C,0xFF), c(0xFF,0xFF,0x00), c(0x00,0x63,0x00), K],[c(0xFF,0x63,0x52), c(0xD6,0x00,0x00), c(0x63,0x00,0x00), K],[c(0x00,0x00,0xFF), W, c(0xFF,0xFF,0x7B), c(0x00,0x84,0xFF)]],
    // 17
    [[c(0xFF,0xFF,0xCE), c(0x63,0xEF,0xEF), c(0x9C,0x84,0x31), c(0x5A,0x5A,0x5A)],[W, c(0xFF,0x73,0x00), c(0x94,0x42,0x00), K],[W, c(0x63,0xA5,0xFF), c(0x00,0x00,0xFF), K]],
    // 18
    [[c(0xB5,0xB5,0xFF), c(0xFF,0xFF,0x94), c(0xAD,0x5A,0x42), K],[K, W, c(0xFF,0x84,0x84), c(0x94,0x3A,0x3A)],[K, W, c(0xFF,0x84,0x84), c(0x94,0x3A,0x3A)]],
    // 19
    [[W, c(0x63,0xA5,0xFF), c(0x00,0x00,0xFF), K],[W, c(0xFF,0x84,0x84), c(0x94,0x3A,0x3A), K],[W, c(0x63,0xA5,0xFF), c(0x00,0x00,0xFF), K]],
    // 20
    [[W, c(0x63,0xA5,0xFF), c(0x00,0x00,0xFF), K],[W, c(0x63,0xA5,0xFF), c(0x00,0x00,0xFF), K],[W, c(0xFF,0x84,0x84), c(0x94,0x3A,0x3A), K]],
    // 21
    [[W, c(0x63,0xA5,0xFF), c(0x00,0x00,0xFF), K],[W, c(0xFF,0x84,0x84), c(0x94,0x3A,0x3A), K],[W, c(0xFF,0xFF,0x7B), c(0x00,0x84,0xFF), c(0xFF,0x00,0x00)]],
    // 22
    [[W, c(0x8C,0x8C,0xDE), c(0x52,0x52,0x8C), K],[W, c(0x8C,0x8C,0xDE), c(0x52,0x52,0x8C), K],[c(0xFF,0xC5,0x42), c(0xFF,0xD6,0x00), c(0x94,0x3A,0x00), c(0x4A,0x00,0x00)]],
    // 23
    [[W, c(0x8C,0x8C,0xDE), c(0x52,0x52,0x8C), K],[c(0xFF,0xC5,0x42), c(0xFF,0xD6,0x00), c(0x94,0x3A,0x00), c(0x4A,0x00,0x00)],[c(0xFF,0xC5,0x42), c(0xFF,0xD6,0x00), c(0x94,0x3A,0x00), c(0x4A,0x00,0x00)]],
    // 24
    [[W, c(0x8C,0x8C,0xDE), c(0x52,0x52,0x8C), K],[c(0xFF,0xC5,0x42), c(0xFF,0xD6,0x00), c(0x94,0x3A,0x00), c(0x4A,0x00,0x00)],[W, c(0x5A,0xBD,0xFF), c(0xFF,0x00,0x00), c(0x00,0x00,0xFF)]],
    // 25
    [[W, c(0x8C,0x8C,0xDE), c(0x52,0x52,0x8C), K],[W, c(0xFF,0x84,0x84), c(0x94,0x3A,0x3A), K],[W, c(0x8C,0x8C,0xDE), c(0x52,0x52,0x8C), K]],
    // 26
    [[W, c(0x8C,0x8C,0xDE), c(0x52,0x52,0x8C), K],[W, c(0xFF,0x84,0x84), c(0x94,0x3A,0x3A), K],[W, c(0xFF,0x84,0x84), c(0x94,0x3A,0x3A), K]],
    // 27
    [[W, c(0x8C,0x8C,0xDE), c(0x52,0x52,0x8C), K],[W, c(0xFF,0x84,0x84), c(0x94,0x3A,0x3A), K],[W, c(0xFF,0xAD,0x63), c(0x84,0x31,0x00), K]],
    // 28
    [[W, c(0x7B,0xFF,0x31), c(0x00,0x84,0x00), K],[W, c(0xFF,0x84,0x84), c(0x94,0x3A,0x3A), K],[W, c(0xFF,0x84,0x84), c(0x94,0x3A,0x3A), K]],
    // 29
    [[W, c(0x7B,0xFF,0x31), c(0x00,0x84,0x00), K],[W, c(0xFF,0x84,0x84), c(0x94,0x3A,0x3A), K],[W, c(0x63,0xA5,0xFF), c(0x00,0x00,0xFF), K]],
    // 30
    [[W, c(0xFF,0xAD,0x63), c(0x84,0x31,0x00), K],[W, c(0x63,0xA5,0xFF), c(0x00,0x00,0xFF), K],[W, c(0x63,0xA5,0xFF), c(0x00,0x00,0xFF), K]],
    // 31
    [[W, c(0xFF,0xAD,0x63), c(0x84,0x31,0x00), K],[W, c(0x63,0xA5,0xFF), c(0x00,0x00,0xFF), K],[W, c(0x7B,0xFF,0x31), c(0x00,0x84,0x00), K]],
    // 32: POKEMON RED
    [[W, c(0xFF,0x84,0x84), c(0x94,0x3A,0x3A), K],[W, c(0xFF,0x84,0x84), c(0x94,0x3A,0x3A), K],[W, c(0xFF,0x84,0x84), c(0x94,0x3A,0x3A), K]],
    // 33
    [[W, c(0xFF,0x84,0x84), c(0x94,0x3A,0x3A), K],[W, c(0x00,0xFF,0x00), c(0x31,0x84,0x00), c(0x00,0x4A,0x00)],[W, c(0x63,0xA5,0xFF), c(0x00,0x00,0xFF), K]],
    // 34
    [[W, c(0xFF,0xAD,0x63), c(0x84,0x31,0x00), K],[W, c(0xFF,0xAD,0x63), c(0x84,0x31,0x00), K],[W, c(0xFF,0xAD,0x63), c(0x84,0x31,0x00), K]],
    // 35
    [[W, c(0xFF,0xAD,0x63), c(0x84,0x31,0x00), K],[W, c(0x7B,0xFF,0x31), c(0x00,0x84,0x00), K],[W, c(0x7B,0xFF,0x31), c(0x00,0x84,0x00), K]],
    // 36
    [[W, c(0xFF,0xAD,0x63), c(0x84,0x31,0x00), K],[W, c(0x7B,0xFF,0x31), c(0x00,0x84,0x00), K],[W, c(0x63,0xA5,0xFF), c(0x00,0x00,0xFF), K]],
    // 37
    [[K, c(0x00,0x84,0x84), c(0xFF,0xDE,0x00), W],[K, c(0x00,0x84,0x84), c(0xFF,0xDE,0x00), W],[K, c(0x00,0x84,0x84), c(0xFF,0xDE,0x00), W]],
    // 38
    [[W, c(0x63,0xA5,0xFF), c(0x00,0x00,0xFF), K],[c(0xFF,0xFF,0x00), c(0xFF,0x00,0x00), c(0x63,0x00,0x00), K],[W, c(0x7B,0xFF,0x31), c(0x00,0x84,0x00), K]],
    // 39
    [[W, c(0xAD,0xAD,0x84), c(0x42,0x73,0x7B), K],[W, c(0xFF,0xAD,0x63), c(0x84,0x31,0x00), K],[W, c(0x63,0xA5,0xFF), c(0x00,0x00,0xFF), K]],
    // 40
    [[W, c(0xA5,0xA5,0xA5), c(0x52,0x52,0x52), K],[W, c(0xA5,0xA5,0xA5), c(0x52,0x52,0x52), K],[W, c(0xA5,0xA5,0xA5), c(0x52,0x52,0x52), K]],
    // 41
    [[W, c(0xFF,0xCE,0x00), c(0x9C,0x63,0x00), K],[W, c(0xFF,0xCE,0x00), c(0x9C,0x63,0x00), K],[W, c(0xFF,0xCE,0x00), c(0x9C,0x63,0x00), K]],
    // 42
    [[W, c(0x7B,0xFF,0x31), c(0x00,0x63,0xC5), K],[W, c(0xFF,0x84,0x84), c(0x94,0x3A,0x3A), K],[W, c(0x7B,0xFF,0x31), c(0x00,0x63,0xC5), K]],
    // 43
    [[W, c(0x7B,0xFF,0x31), c(0x00,0x63,0xC5), K],[W, c(0xFF,0x84,0x84), c(0x94,0x3A,0x3A), K],[W, c(0xFF,0x84,0x84), c(0x94,0x3A,0x3A), K]],
    // 44
    [[W, c(0x7B,0xFF,0x31), c(0x00,0x63,0xC5), K],[W, c(0xFF,0x84,0x84), c(0x94,0x3A,0x3A), K],[W, c(0x63,0xA5,0xFF), c(0x00,0x00,0xFF), K]],
];

const CHECKSUM_TABLE: [(u8, u8); 79] =[
    (0x00, 43), (0x01, 31), (0x0C, 34), (0x0D, 13), (0x10, 31),
    (0x14, 32), (0x15, 12), (0x16, 34), (0x17, 29), (0x18, 24),
    (0x19, 10), (0x1D, 15), (0x27, 16), (0x28, 28), (0x29, 31),
    (0x34, 6),  (0x35, 34), (0x36, 5),  (0x39, 30), (0x3C, 20),
    (0x3D, 7),  (0x3E, 11), (0x3F, 43), (0x43, 30), (0x46, 18),
    (0x49, 16), (0x4B, 28), (0x4E, 21), (0x52, 31), (0x58, 40),
    (0x59, 1),  (0x5C, 16), (0x5D, 31), (0x61, 19), (0x66, 6),
    (0x67, 34), (0x68, 31), (0x69, 13), (0x6A, 7),  (0x6B, 24),
    (0x6D, 31), (0x6F, 41), (0x70, 33), (0x71, 9),  (0x75, 34),
    (0x86, 3),  (0x88, 14), (0x8B, 29), (0x8C, 2),  (0x90, 28),
    (0x92, 34), (0x95, 8),  (0x97, 30), (0x99, 34), (0x9A, 28),
    (0x9C, 22), (0x9D, 27), (0xA2, 36), (0xA5, 35), (0xA8, 3),
    (0xAA, 42), (0xB3, 0),  (0xB7, 34), (0xBD, 28), (0xBF, 4),
    (0xC6, 1),  (0xC9, 17), (0xCE, 4),  (0xD1, 4),  (0xD3, 25),
    (0xDB, 12), (0xE0, 11), (0xE8, 37), (0xF0, 4),  (0xF2, 13),
    (0xF4, 6),  (0xF6, 31), (0xF7, 36), (0xFF, 9),
];

const DISAMBIG_TABLE: [(u8, u8, u8); 26] =[
    (0x0D, 0x45, 23), (0x18, 0x42, 43), (0x27, 0x42, 29), (0x28, 0x41, 37),
    (0x28, 0x42, 37), (0x46, 0x45, 38), (0x61, 0x45, 29), (0x66, 0x45, 43),
    (0x6A, 0x49, 24), (0xA5, 0x42, 37), (0xB3, 0x42, 8),  (0xB3, 0x4E, 16),
    (0xB3, 0x55, 16), (0xBF, 0x20, 26), (0xBF, 0x43, 26), (0xC6, 0x41, 43),
    (0xC6, 0x42, 43), (0xD3, 0x49, 39), (0xF4, 0x42, 44), (0x14, 0x4F, 32),
    (0x3C, 0x4D, 20), (0x46, 0x41, 18), (0x61, 0x41, 19), (0x6A, 0x4B, 7),
    (0xD3, 0x52, 25), (0xF4, 0x41, 6),
];

fn build_combo_palettes(combo_idx: usize) -> [Pal; 3] {
    let raw = &COMBO_PALETTES[combo_idx];
    [raw[0], raw[1], raw[2]]
}

pub fn gbc_palette_for_rom(rom: &[u8]) -> [Pal; 3] {
    let cs = title_checksum(rom);
    let ch4 = *rom.get(0x137).unwrap_or(&0);

    for &(dcs, dch, combo_id) in &DISAMBIG_TABLE {
        if dcs == cs && dch == ch4 {
            return build_combo_palettes(combo_id as usize);
        }
    }

    for &(tcs, combo_id) in &CHECKSUM_TABLE {
        if tcs == cs {
            return build_combo_palettes(combo_id as usize);
        }
    }

    DEFAULT_GBC_PALETTES
}

const PAL_BG: u8 = 0;
const PAL_OBJ0: u8 = 1;
const PAL_OBJ1: u8 = 2;

#[inline]
fn pack_cgb_pixel(r: u8, g: u8, b: u8, raw: u8, bg_priority: bool) -> u32 {
    let meta = (raw & 0x03) | if bg_priority { 0x04 } else { 0 };
    (r as u32) | ((g as u32) << 8) | ((b as u32) << 16) | ((meta as u32) << 24)
}

fn get_cgb_color(palettes: &[u8; 64], pal_num: u8, color_idx: u8) -> (u8, u8, u8) {
    let base = (pal_num as usize * 8) + (color_idx as usize * 2);
    let low = palettes[base] as u16;
    let high = palettes[base + 1] as u16;
    let rgb555 = low | (high << 8);
    let r = (rgb555 & 0x1F) as u8;
    let g = ((rgb555 >> 5) & 0x1F) as u8;
    let b = ((rgb555 >> 10) & 0x1F) as u8;
    ((r << 3) | (r >> 2), (g << 3) | (g >> 2), (b << 3) | (b >> 2))
}

fn get_color_index(b1: u8, b2: u8, x: u8) -> u8 {
    let b1sel = (b1 >> (7 - x)) & 0x01;
    let b2sel = (b2 >> (7 - x)) & 0x01;
    (b2sel << 1) | b1sel
}

#[inline]
fn apply_dmg_palette(color_index: u8, palette_reg: u8) -> u8 {
    (palette_reg >> (color_index * 2)) & 0x03
}

#[cfg(test)]
mod raster_tests {
    use super::*;

    #[test]
    fn palette_edges_follow_visible_output_and_preserve_one_dot_overlap() {
        let mut cpu = crate::cpu::CPU::new();
        cpu.memory[0xFF44] = 1;
        cpu.ppu_mode3_start_dot = 84;
        cpu.ppu_line_snapshot = PpuLineSnapshot { lcdc: 0x91, ..Default::default() };
        cpu.gbc_palettes = [[[255, 255, 255, 255], [170, 170, 170, 255], [85, 85, 85, 255], [0, 0, 0, 255]]; 3];
        cpu.color_mode = 0;
        cpu.ppu_reg_log[0] = PpuRegChange { dot: 100, addr: 0xFF47, value: 1 };
        cpu.ppu_reg_log[1] = PpuRegChange { dot: 104, addr: 0xFF47, value: 0 };
        cpu.ppu_reg_log_len = 2;
        draw_scanline(&mut cpu);
        for x in 0..160 {
            let gray = (cpu.frame_buffer[160 + x] & 0xFF) as u8;
            assert_eq!(gray, if (5..=9).contains(&x) { 170 } else { 255 }, "x={x}");
        }
    }

    #[test]
    fn invisible_objects_still_pause_the_raster_clock() {
        let mut cpu = crate::cpu::CPU::new();
        let snap = PpuLineSnapshot { lcdc: 0x93, scx: 3, ..Default::default() };
        let before = pixel_output_dots(&cpu, snap, 1, 95);
        cpu.memory[0xFE00] = 16;
        cpu.memory[0xFE01] = 0;
        let after = pixel_output_dots(&cpu, snap, 1, 95);
        assert_eq!(before[0], 98);
        for x in 0..160 { assert_eq!(after[x] - before[x], 11); }
    }

    #[test]
    fn fine_scroll_latches_transition_edge_but_not_later_writes() {
        let mut cpu = crate::cpu::CPU::new();
        cpu.memory[0xFF44] = 1;
        cpu.ppu_mode3_start_dot = 84;
        cpu.ppu_line_snapshot = PpuLineSnapshot { lcdc: 0x91, bgp: 0xE4, ..Default::default() };
        cpu.gbc_palettes = [[[255, 255, 255, 255], [170, 170, 170, 255], [85, 85, 85, 255], [0, 0, 0, 255]]; 3];
        cpu.color_mode = 0;
        cpu.memory[0x8002] = 0x80;
        cpu.ppu_reg_log[0] = PpuRegChange { dot: 84, addr: 0xFF43, value: 2 };
        cpu.ppu_reg_log[1] = PpuRegChange { dot: 100, addr: 0xFF43, value: 7 };
        cpu.ppu_reg_log_len = 2;
        draw_scanline(&mut cpu);
        for x in 0..160 {
            assert_eq!(cpu.frame_buffer[160 + x] & 255,
                if x % 8 == 6 { 170 } else { 255 }, "x={x}");
        }
    }

    #[test]
    fn background_bitplanes_sample_scroll_and_tile_selection_separately() {
        let mut cpu = crate::cpu::CPU::new();
        let snap = PpuLineSnapshot { lcdc: 0x91, ..Default::default() };
        cpu.memory[0x9800] = 1;
        cpu.memory[0x8010] = 0xA5;
        cpu.memory[0x9013] = 0x3C;
        // Write between the low-plane (102) and high-plane (104) reads.
        let changes = [PpuRegChange { dot: 103, addr: 0xFF42, value: 1 },
            PpuRegChange { dot: 103, addr: 0xFF40, value: 0x81 }];
        assert_eq!(fetch_background_tile(&cpu, snap, &changes, 0, 0, 100), (0xA5, 0x3C, 0));
    }

    #[test]
    fn window_restart_uses_live_wx_and_does_not_trigger_behind_the_beam() {
        let mut cpu = crate::cpu::CPU::new();
        let snap = PpuLineSnapshot { lcdc: 0xB1, wx: 10, ..Default::default() };
        cpu.ppu_reg_log[0] = PpuRegChange { dot: 84, addr: 0xFF4B, value: 11 };
        cpu.ppu_reg_log_len = 1;
        let dots = pixel_output_dots(&cpu, snap, 1, 95);
        assert_eq!(dots[3], 98);
        assert_eq!(dots[4], 105);
        cpu.ppu_reg_log[0] = PpuRegChange { dot: 110, addr: 0xFF4B, value: 7 };
        let snap = PpuLineSnapshot { wx: 100, ..snap };
        let dots = pixel_output_dots(&cpu, snap, 1, 95);
        assert_eq!(dots[16], 111);
        assert_eq!(dots[159], 254);
    }

    #[test]
    fn wx_zero_with_fine_scroll_delays_window_activation_one_extra_dot() {
        let cpu = crate::cpu::CPU::new();
        let snap = PpuLineSnapshot { lcdc: 0xB1, wx: 0, ..Default::default() };
        assert_eq!(pixel_output_dots(&cpu, snap, 1, 95)[0], 101);
        for fine in 1..8 {
            let snap = PpuLineSnapshot { scx: fine, ..snap };
            assert_eq!(pixel_output_dots(&cpu, snap, 1, 95)[0], 102 + fine as u16);
        }
    }

    #[test]
    fn window_disable_drains_fetched_tile_and_reactivation_advances_row() {
        let mut cpu = crate::cpu::CPU::new();
        cpu.memory[0xFF44] = 0;
        cpu.ppu_mode3_start_dot = 84;
        cpu.ppu_line_snapshot = PpuLineSnapshot { lcdc: 0xF1, wx: 7, bgp: 0xE4, ..Default::default() };
        cpu.gbc_palettes = [[[255, 255, 255, 255], [170, 170, 170, 255], [85, 85, 85, 255], [0, 0, 0, 255]]; 3];
        cpu.color_mode = 0;
        cpu.memory[0x9C00..0x9C20].fill(1);
        cpu.memory[0x8010] = 0xFF;
        cpu.memory[0x8011] = 0xFF;
        cpu.memory[0x8012] = 0xFF;
        // Disable before (not on) the next map bus edge at dot 110.
        cpu.ppu_reg_log[0] = PpuRegChange { dot: 109, addr: 0xFF40, value: 0xD1 };
        cpu.ppu_reg_log[1] = PpuRegChange { dot: 130, addr: 0xFF40, value: 0xF1 };
        cpu.ppu_reg_log[2] = PpuRegChange { dot: 140, addr: 0xFF4B, value: 87 };
        cpu.ppu_reg_log_len = 3;
        draw_scanline(&mut cpu);
        for x in 0..160 {
            let expected = if x < 16 { 0 } else if x < 80 { 255 } else { 170 };
            assert_eq!(cpu.frame_buffer[x] & 255, expected, "x={x}");
        }
        assert_eq!(cpu.window_line_counter, 2);
    }

    #[test]
    fn window_fetch_latches_map_and_ignores_background_scroll() {
        let mut cpu = crate::cpu::CPU::new();
        let snap = PpuLineSnapshot { lcdc: 0xF1, ..Default::default() };
        cpu.memory[0x9C00] = 1;
        cpu.memory[0x9800] = 2;
        cpu.memory[0x8014] = 0xA5;
        cpu.memory[0x9015] = 0x3C;
        let changes = [PpuRegChange { dot: 101, addr: 0xFF40, value: 0xB1 },
            PpuRegChange { dot: 103, addr: 0xFF40, value: 0xA1 },
            PpuRegChange { dot: 104, addr: 0xFF42, value: 7 }];
        assert_eq!(fetch_window_tile(&cpu, snap, &changes, 2, 0, 100), (0xA5, 0x3C, 0));
    }

    #[test]
    fn map_read_precedes_a_register_write_on_the_same_dot() {
        let mut cpu = crate::cpu::CPU::new();
        let snap = PpuLineSnapshot { lcdc: 0x91, ..Default::default() };
        cpu.memory[0x9800] = 1;
        cpu.memory[0x9801] = 2;
        cpu.memory[0x8010] = 0xA5;
        cpu.memory[0x8020] = 0x3C;
        let changes = [PpuRegChange { dot: 100, addr: 0xFF43, value: 8 }];
        assert_eq!(fetch_background_tile(&cpu, snap, &changes, 0, 0, 100).0, 0xA5);
        assert_eq!(fetch_background_tile(&cpu, snap, &changes, 0, 0, 101).0, 0x3C);
    }

    #[test]
    fn clipped_objects_delay_startup_but_visible_objects_interrupt_prefetch() {
        let mut cpu = crate::cpu::CPU::new();
        let snap = PpuLineSnapshot { lcdc: 0x93, ..Default::default() };
        cpu.memory[0xFE00] = 16;
        cpu.memory[0xFE01] = 1;
        let (pauses, prefetch) = object_fetch_pauses(&cpu, snap, 0);
        assert!(pauses[0] >= 6);
        assert_eq!(prefetch[0], 0);
        cpu.memory[0xFE01] = 8;
        let (pauses, prefetch) = object_fetch_pauses(&cpu, snap, 0);
        assert_eq!(pauses[0], 11);
        assert_eq!(prefetch[0], 11);
    }

    #[test]
    fn bitplane_read_precedes_same_dot_tile_selection_write() {
        let mut cpu = crate::cpu::CPU::new();
        let snap = PpuLineSnapshot { lcdc: 0x91, ..Default::default() };
        cpu.memory[0x9800] = 1;
        cpu.memory[0x8010] = 0xA5;
        cpu.memory[0x8011] = 0x5A;
        cpu.memory[0x9011] = 0x3C;
        let changes = [PpuRegChange { dot: 104, addr: 0xFF40, value: 0x81 }];
        assert_eq!(fetch_background_tile(&cpu, snap, &changes, 0, 0, 100), (0xA5, 0x5A, 0));
        assert_eq!(fetch_background_tile(&cpu, snap, &changes, 0, 0, 101), (0xA5, 0x3C, 0));
    }

    #[test]
    fn background_mixer_samples_enable_before_bus_writes() {
        for cgb in [false, true] {
            let mut cpu = crate::cpu::CPU::new();
            cpu.is_cgb = cgb;
            cpu.booting = false;
            cpu.memory[0xFF4C] = 4; // CGB monochrome compatibility mode.
            cpu.memory[0xFF44] = 0;
            cpu.memory[0x8000..0x8010].fill(0xFF);
            cpu.memory[0x9800..0x9C00].fill(0);
            cpu.memory[0xFE00..0xFEA0].fill(0);
            cpu.ppu_line_snapshot = PpuLineSnapshot { lcdc: 0x91, bgp: 0xE4, ..Default::default() };
            cpu.ppu_mode3_start_dot = 84;
            cpu.ppu_reg_log[0] = PpuRegChange { dot: 99, addr: 0xFF40, value: 0x90 };
            cpu.ppu_reg_log_len = 1;
            super::draw_scanline(&mut cpu);
            let raw = |x: usize| (cpu.frame_buffer[x] >> 24) & 3;
            assert_eq!(raw(4), 3, "same-dot write must not change the pixel");
            assert_eq!(raw(5), if cgb { 3 } else { 0 });
            assert_eq!(raw(6), 0, "disabled BG must also clear OBJ-priority metadata");
        }
    }
}

/// Visible pixel clocks include fetcher pauses; raster register writes must
/// not be projected onto a uniform one-pixel-per-dot line across those pauses.
fn object_fetch_pauses(cpu: &crate::cpu::CPU, snap: PpuLineSnapshot, ly: u8) -> ([u16; 160], [u16; 160]) {
    let schedule = objects::fetch_schedule(cpu, snap, ly);
    (schedule.pauses, schedule.prefetch)
}

fn pixel_output_dots(cpu: &crate::cpu::CPU, snap: PpuLineSnapshot, ly: u8, first_dot: u16) -> [u16; 160] {
    let (pauses, _) = object_fetch_pauses(cpu, snap, ly);
    let fine = (snap.scx & 7) as u16;
    let mut dots = [0u16; 160];
    let mut delay = fine;
    let mut window_regs = snap;
    let mut change_index = 0;
    let changes = &cpu.ppu_reg_log[..cpu.ppu_reg_log_len];
    let mut window_origin = window::clipped_initial_origin(
        snap, changes, cpu.ppu_mode3_start_dot, ly, cpu.cgb_native_mode()
    ).map(|origin| origin as i16);
    if window_origin.is_some() { delay += 6; }
    for x in 0..160 {
        delay += pauses[x];
        let dot = first_dot + x as u16 + delay;
        while change_index < changes.len() && changes[change_index].dot <= dot {
            let change = changes[change_index];
            match change.addr {
                0xFF40 => window_regs.lcdc = change.value,
                0xFF4A => window_regs.wy = change.value,
                0xFF4B => window_regs.wx = change.value,
                _ => {}
            }
            change_index += 1;
        }
        // WX is compared against the current horizontal position, not the
        // value from the previous scanline's Mode-3 snapshot. A write before
        // the trigger moves the six-dot fetcher restart to the new position.
        let enabled = window_regs.lcdc & 0x20 != 0 && ly >= window_regs.wy
            && (cpu.cgb_native_mode() || window_regs.lcdc & 1 != 0);
        // Independent WX-change captures place the comparator two dots
        // behind the CPU bus write (inferred from the hardware references).
        let comparison_wx = registers_at(snap, changes, dot.saturating_sub(2)).wx;
        if let Some(origin) = window_origin {
            if (x as i16 - origin) & 7 == 0 {
                let fetch_dot = if x >= 7 { dots[x - 7] }
                    else { dots[0].saturating_sub(7).saturating_add(x as u16) };
                if registers_at(snap, changes, fetch_dot.saturating_sub(1)).lcdc & 0x20 == 0 {
                    window_origin = None;
                }
            }
        }
        if window_origin.is_none() && enabled && comparison_wx <= 166
            && (comparison_wx == 0 || comparison_wx >= 7)
            && x == (comparison_wx as i16 - 7).max(0) as usize {
            // At WX=0, nonzero fine scrolling delays window activation by
            // one dot (the Mealybug WX=0 hardware capture exercises this).
            delay += if comparison_wx == 0 && fine != 0 { 7 } else { 6 };
            window_origin = Some(comparison_wx as i16 - 7);
        }
        if let Some(origin) = window_origin {
            if window::reactivation_zero_pixel(origin as i32, x as u32, comparison_wx) {
                window_origin = Some(origin + 1);
            }
        }
        dots[x] = first_dot + x as u16 + delay;
    }
    dots
}

fn registers_at(mut registers: PpuLineSnapshot, changes: &[PpuRegChange], dot: u16) -> PpuLineSnapshot {
    for change in changes {
        if change.dot > dot { break; }
        match change.addr {
            0xFF40 => registers.lcdc = change.value,
            0xFF42 => registers.scy = change.value,
            0xFF43 => registers.scx = change.value,
            0xFF4A => registers.wy = change.value,
            0xFF4B => registers.wx = change.value,
            _ => {}
        }
    }
    registers
}

pub fn draw_scanline(cpu: &mut crate::cpu::CPU) {
    let ly = cpu.memory[0xFF44];
    if ly >= 144 { return; }

    let mut snap = cpu.ppu_line_snapshot;
    if snap.lcdc & 0x80 == 0 { return; }

    let log       = &cpu.ppu_reg_log[..cpu.ppu_reg_log_len];
    // CPU bus writes at the transition dot are logged after the Mode-3
    // snapshot. Fine scrolling is latched by the first map fetch, so include
    // that edge, but never subsequent writes to the low three bits.
    let first_fetch = registers_at(snap, log, cpu.ppu_mode3_start_dot);
    snap.scx = (snap.scx & !7) | (first_fetch.scx & 7);
    // STAT's Mode-3 transition is sampled at the end of an M-cycle, four
    // dots after the fetcher starts. Output follows the twelve-dot fetch
    // startup, and the LCD samples palette data before the bus write edge.
    let mode3_dot = cpu.ppu_mode3_start_dot + 11;
    let is_cgb    = cpu.cgb_native_mode();
    let buf_base  = ly as usize * 160;
    let pixel_dots = if log.is_empty() { [0; 160] }
        else { pixel_output_dots(cpu, snap, ly, mode3_dot) };
    let object_schedule = if log.is_empty() { None } else {
        Some(objects::fetch_schedule(cpu, snap, ly))
    };

    // In CGB compatibility mode, BGP/OBP select among the colors installed by
    // the real boot ROM. Cartridge writes cannot replace native palettes.
    let mut compatibility_palettes = DEFAULT_GBC_PALETTES;
    if cpu.is_cgb && !is_cgb {
        for group in 0..3 {
            for color in 0..4 {
                let (r, g, b) = if group == 0 {
                    get_cgb_color(&cpu.cgb_bg_palettes, 0, color as u8)
                } else {
                    get_cgb_color(&cpu.cgb_obj_palettes, (group - 1) as u8, color as u8)
                };
                compatibility_palettes[group][color] = [r, g, b, 255];
            }
        }
    }
    let palettes = if cpu.is_cgb && !is_cgb { &compatibility_palettes }
        else if cpu.color_mode == 0 { &cpu.gbc_palettes } else { &DEFAULT_GBC_PALETTES };

    // ── Pre-build per-pixel OBP/LCDC state for sprite pass ──────────────────
    // One forward pass (O(160)) instead of a full log scan (O(log_len)) per
    // sprite pixel. 3×160 = 480 bytes on the stack.
    let mut px_obp0    = [snap.obp0; 160];
    let mut px_obp1    = [snap.obp1; 160];
    let mut px_lcdc_sp = [snap.lcdc; 160];
    if !log.is_empty() {
        let (mut c0, mut c1, mut cl) = (snap.obp0, snap.obp1, snap.lcdc);
        let mut li = 0usize;
        for xi in 0..160usize {
            let dot = pixel_dots[xi];
            while li < log.len() && log[li].dot <= dot {
                match log[li].addr {
                    0xFF40 => cl = log[li].value,
                    0xFF48 => c0 = log[li].value,
                    0xFF49 => c1 = log[li].value,
                    _ => {}
                }
                li += 1;
            }
            px_obp0[xi] = c0; px_obp1[xi] = c1; px_lcdc_sp[xi] = cl;
        }
    }

    // Working rendering registers — start at Mode-2→3 snapshot values.
    let mut lcdc = snap.lcdc;
    let mut scy  = snap.scy;
    let mut scx  = snap.scx;
    let mut bgp  = snap.bgp;
    let mut wx   = snap.wx;
    let wy       = snap.wy;

    let win_line_active = ly >= wy;
    let mut log_idx     = 0usize;
    let mut window_origin = window::clipped_initial_origin(
        snap, log, cpu.ppu_mode3_start_dot, ly, is_cgb
    );
    let mut window_activations = u8::from(window_origin.is_some());
    let mut bg_resume_shift = 0u16;
    let raster_fetch = log.iter().any(|change| matches!(change.addr, 0xFF40 | 0xFF42 | 0xFF43));
    let prefetched_obj_pauses = if raster_fetch { object_fetch_pauses(cpu, snap, ly).1 } else { [0; 160] };
    let mut cached_bg_tile = None;
    let mut cached_window_tile = None;

    // ── BG / Window tile-strided pass ────────────────────────────────────────
    // We cache (b1, b2) tile bytes and render up to 8 pixels from them before
    // fetching the next tile. Log-entry boundaries and the window left edge
    // split runs shorter when needed. In the common case (no log entries) this
    // gives ~20 tile fetches per scanline instead of 160 per-pixel fetches.
    let mut screen_x = 0u32;
    while screen_x < 160 {
        let pixel_dot = pixel_dots[screen_x as usize];
        let mut pixel_bgp = bgp;
        let mut palette_edge = false;

        // Apply any log entries up to the current pixel.
        while log_idx < log.len() && log[log_idx].dot <= pixel_dot {
            let chg = log[log_idx];
            match chg.addr {
                0xFF40 => lcdc = chg.value,
                0xFF42 => scy  = chg.value,
                0xFF43 => scx  = chg.value,
                0xFF47 => {
                    // DMG palette lines overlap at the write edge: the LCD
                    // samples the OR of the old and new register for one dot.
                    pixel_bgp = if !cpu.is_cgb && chg.dot == pixel_dot {
                        palette_edge = true;
                        bgp | chg.value
                    } else { chg.value };
                    bgp = chg.value;
                }
                0xFF4B => wx   = chg.value,
                _ => {}
            }
            log_idx += 1;
        }

        // LCD killed mid-line → white fill for the rest of the scanline.
        if lcdc & 0x80 == 0 {
            for xi in screen_x..160 {
                cpu.frame_buffer[buf_base + xi as usize] = pack_cgb_pixel(0xFF, 0xFF, 0xFF, 0, false);
            }
            break;
        }

        // How many screen-x columns until the next log entry fires?
        let next_log_dot = log.get(log_idx).map(|e| e.dot).unwrap_or(0xFFFF);
        let mut next_log_x: u32 = if palette_edge {
            screen_x + 1
        } else if next_log_dot == 0xFFFF {
            160
        } else {
            pixel_dots.partition_point(|&dot| dot < next_log_dot) as u32
        };
        // A FIFO restart can fall inside a cached tile run without another
        // register write at that pixel. Re-enter the control path at every
        // output-clock discontinuity rather than drawing across that event.
        if !log.is_empty() {
            if let Some(x) = (screen_x as usize + 1..next_log_x as usize)
                .find(|&x| pixel_dots[x] > pixel_dots[x - 1] + 1) {
                next_log_x = x as u32;
            }
        }

        let master_bg = is_cgb || lcdc & 0x01 != 0;
        let win_en    = lcdc & 0x20 != 0 && master_bg && win_line_active;
        let win_left  = wx as i32 - 7;
        // Disabling the window drains its current FIFO tile. Changing WX
        // does not relocate pixels already fetched, and re-enabling after a
        // missed horizontal comparison cannot retroactively start it.
        if let Some(origin) = window_origin {
            let fetch_dot = if screen_x >= 7 { pixel_dots[screen_x as usize - 7] }
                else { pixel_dots[0].saturating_sub(7).saturating_add(screen_x as u16) };
            if (screen_x as i32 - origin) & 7 == 0
                && registers_at(snap, log, fetch_dot.saturating_sub(1)).lcdc & 0x20 == 0 {
                window_origin = None;
                bg_resume_shift = (screen_x as u16 + (snap.scx & 7) as u16) & 7;
                cached_bg_tile = None;
            }
        }
        if window_origin.is_none() {
            let previous_dot = if screen_x > 0 { Some(pixel_dots[screen_x as usize - 1]) } else { None };
            if let Some(origin) = window::restart_trigger_origin(snap, log, ly, is_cgb, screen_x, pixel_dot, previous_dot) {
                window_origin = Some(origin);
                window_activations += 1;
            }
        }
        if window_origin.is_none() && win_en && wx <= 166 && (wx == 0 || wx >= 7)
            && screen_x == win_left.max(0) as u32 {
            window_origin = Some(win_left);
            window_activations += 1;
        }

        if window::insert_reactivation_zero(&mut window_origin, screen_x, wx) {
            let (r, g, b) = if is_cgb {
                get_cgb_color(&cpu.cgb_bg_palettes, 0, 0)
            } else {
                let color = palettes[PAL_BG as usize][apply_dmg_palette(0, pixel_bgp) as usize];
                (color[0], color[1], color[2])
            };
            cpu.frame_buffer[buf_base + screen_x as usize] = pack_cgb_pixel(r, g, b, 0, false);
            screen_x += 1;
            continue;
        }

        let tile_data_base: usize = if lcdc & 0x10 != 0 { 0x8000 } else { 0x8800 };

        if let Some(origin) = window_origin {
            // ── Window tile ─────────────────────────────────────────────────
            let wy_row       = cpu.window_line_counter.wrapping_add(window_activations - 1);
            let tile_map_base: usize = if lcdc & 0x40 != 0 { 0x9C00 } else { 0x9800 };
            let wx_pos_start = (screen_x as i32 - origin).max(0) as u32;
            let tile_map_addr = tile_map_base + (wy_row as usize / 8) * 32 + (wx_pos_start as usize / 8);

            let (tile_index, mut attr) = if is_cgb {
                (cpu.cgb_vram[0][tile_map_addr - 0x8000],
                 cpu.cgb_vram[1][tile_map_addr - 0x8000])
            } else {
                (cpu.memory[tile_map_addr], 0u8)
            };
            let vram_bank = if is_cgb { ((attr >> 3) & 1) as usize } else { 0 };
            let flip_y    = is_cgb && (attr & 0x40) != 0;
            let offset: u16 = if tile_data_base == 0x8000 {
                tile_index as u16 * 16
            } else {
                ((tile_index as i8 as i16 + 128) as u16) * 16
            };
            let t_row = if flip_y { 7 - (wy_row % 8) } else { wy_row % 8 };
            let addr  = tile_data_base + (offset + t_row as u16 * 2) as usize;
            let (mut b1, mut b2) = if is_cgb {
                (cpu.cgb_vram[vram_bank][addr - 0x8000],
                 cpu.cgb_vram[vram_bank][addr - 0x8000 + 1])
            } else {
                (cpu.memory[addr], cpu.memory[addr + 1])
            };

            if raster_fetch {
                let tile = wx_pos_start as u16 / 8;
                let key = (window_activations, tile);
                if cached_window_tile.map(|(k, _, _, _)| k) != Some(key) {
                    let tile_start = screen_x as i32 - (wx_pos_start & 7) as i32;
                    let map_dot = if tile_start >= origin + 8 && tile_start >= 7 {
                        pixel_dots[tile_start as usize - 7]
                    } else {
                        let first = pixel_dots[origin.max(0) as usize];
                        first.saturating_sub(if wx == 0 && snap.scx & 7 != 0 { 7 } else { 6 })
                    };
                    let (low, high, attributes) = fetch_window_tile(cpu, snap, log, wy_row, tile, map_dot);
                    cached_window_tile = Some((key, low, high, attributes));
                }
                let (_, low, high, attributes) = cached_window_tile.unwrap();
                b1 = low; b2 = high; attr = attributes;
            }
            let flip_x = is_cgb && attr & 0x20 != 0;
            let cgb_pal = attr & 7;
            let bg_prio = is_cgb && attr & 0x80 != 0;

            // Run to end of this window tile or next log entry.
            let tile_run = 8 - (wx_pos_start % 8);
            let run_end  = (screen_x + tile_run).min(next_log_x).min(160);

            for xi in screen_x..run_end {
                let wx_pos = (xi as i32 - origin) as u8;
                let px  = if flip_x { 7 - (wx_pos % 8) } else { wx_pos % 8 };
                let raw = get_color_index(b1, b2, px);
                let pixel = if is_cgb {
                    let (r, g, b) = get_cgb_color(&cpu.cgb_bg_palettes, cgb_pal, raw);
                    pack_cgb_pixel(r, g, b, raw, bg_prio)
                } else {
                    let ci    = apply_dmg_palette(raw, pixel_bgp);
                    let color = &palettes[PAL_BG as usize][ci as usize];
                    pack_cgb_pixel(color[0], color[1], color[2], raw, bg_prio)
                };
                cpu.frame_buffer[buf_base + xi as usize] = pixel;
            }
            screen_x = run_end;
        } else {
            // ── Background tile ──────────────────────────────────────────────
            let dy    = ly.wrapping_add(scy);
            let t_row = dy % 8;
            let tile_map_base: usize = if lcdc & 0x08 != 0 { 0x9C00 } else { 0x9800 };
            let dx = ((screen_x as u16 + if raster_fetch { (snap.scx & 7) as u16 } else { scx as u16 }).wrapping_sub(bg_resume_shift) & 0xFF) as u8;
            let tile_map_addr = tile_map_base + (dy as usize / 8) * 32 + (dx as usize / 8);

            let (tile_index, mut attr) = if is_cgb {
                (cpu.cgb_vram[0][tile_map_addr - 0x8000],
                 cpu.cgb_vram[1][tile_map_addr - 0x8000])
            } else {
                (cpu.memory[tile_map_addr], 0u8)
            };
            let vram_bank = if is_cgb { ((attr >> 3) & 1) as usize } else { 0 };
            let flip_y    = is_cgb && (attr & 0x40) != 0;
            let offset: u16 = if tile_data_base == 0x8000 {
                tile_index as u16 * 16
            } else {
                ((tile_index as i8 as i16 + 128) as u16) * 16
            };
            let ty   = if flip_y { 7 - t_row } else { t_row };
            let addr = tile_data_base + (offset + ty as u16 * 2) as usize;
            let (mut b1, mut b2) = if is_cgb {
                (cpu.cgb_vram[vram_bank][addr - 0x8000],
                 cpu.cgb_vram[vram_bank][addr - 0x8000 + 1])
            } else {
                (cpu.memory[addr], cpu.memory[addr + 1])
            };

            if raster_fetch {
                let fetcher_x = (screen_x as u16 + (snap.scx & 7) as u16 - bg_resume_shift) / 8;
                if cached_bg_tile.map(|(x, _, _, _)| x) != Some(fetcher_x) {
                    let tile_start = screen_x as i16 - (dx & 7) as i16;
                    // Fetch the next tile while the preceding tile drains.
                    // A sprite pause between that fetch and this tile's
                    // output must not move an already-completed map read.
                    let mut map_dot = if tile_start >= 7 { pixel_dots[(tile_start - 7) as usize] }
                        else { mode3_dot.saturating_add((snap.scx & 7) as u16).saturating_sub(7).saturating_add(tile_start.max(0) as u16) };
                    // The map read is already in flight while the previous
                    // tile's first pixels drain. An OBJ at those positions
                    // stops output, not that earlier map bus transaction.
                    // Left-clipped objects act during startup instead.
                    if tile_start >= 8 {
                        let previous = (tile_start - 8) as usize;
                        map_dot = map_dot.saturating_sub(prefetched_obj_pauses[previous]
                            + prefetched_obj_pauses[previous + 1]);
                    }
                    let (low, high, attributes) = fetch_background_tile(cpu, snap, log, ly, fetcher_x, map_dot);
                    cached_bg_tile = Some((fetcher_x, low, high, attributes));
                }
                let (_, low, high, attributes) = cached_bg_tile.unwrap();
                b1 = low; b2 = high; attr = attributes;
            }
            let flip_x = is_cgb && attr & 0x20 != 0;
            let cgb_pal = attr & 7;
            let bg_prio = is_cgb && attr & 0x80 != 0;

            // Run to tile boundary, next log entry, window left edge, or screen end.
            let tile_run: u32 = 8 - (dx % 8) as u32;
            let win_boundary: u32 = if win_en && win_left > screen_x as i32 {
                (win_left as u32).min(160)
            } else { 160 };
            let run_end = (screen_x + tile_run).min(next_log_x).min(win_boundary).min(160);

            for xi in screen_x..run_end {
                let dxi = ((xi as u16 + if raster_fetch { (snap.scx & 7) as u16 } else { scx as u16 }).wrapping_sub(bg_resume_shift) & 0xFF) as u8;
                let px  = if flip_x { 7 - (dxi % 8) } else { dxi % 8 };
                let raw = get_color_index(b1, b2, px);
                let pixel = if is_cgb {
                    let (r, g, b) = get_cgb_color(&cpu.cgb_bg_palettes, cgb_pal, raw);
                    pack_cgb_pixel(r, g, b, raw, bg_prio)
                } else {
                    // The LCD mixer samples LCDC.0 before a CPU write on
                    // the same dot, independently of the tile fetcher. CGB
                    // compatibility-mode captures show one more dot of
                    // propagation than DMG (an inferred pipeline phase).
                    let bg_enabled = if log.is_empty() { master_bg } else {
                        registers_at(snap, log, pixel_dots[xi as usize].saturating_sub(if cpu.is_cgb { 2 } else { 1 })).lcdc & 1 != 0
                    };
                    let raw_eff = if bg_enabled { raw } else { 0 };
                    let ci      = apply_dmg_palette(raw_eff, pixel_bgp);
                    let color   = &palettes[PAL_BG as usize][ci as usize];
                    pack_cgb_pixel(color[0], color[1], color[2], raw_eff, bg_prio)
                };
                cpu.frame_buffer[buf_base + xi as usize] = pixel;
            }
            screen_x = run_end;
        }
    }

    cpu.window_line_counter = cpu.window_line_counter.wrapping_add(window_activations);

    // ── Sprite pass ──────────────────────────────────────────────────────────
    if snap.lcdc & 0x02 != 0 {
        let sprite_height: i16 = if snap.lcdc & 0x04 != 0 { 16 } else { 8 };
        let ly_i16 = ly as i16;

        let mut line_sprites: [(u8, i16); 10] = [(0, 0); 10];
        let mut count = 0usize;

        for i in 0..40u8 {
            let base     = 0xFE00 + (i as usize) * 4;
            let sprite_y = cpu.memory[base] as i16 - 16;
            if ly_i16 >= sprite_y && ly_i16 < sprite_y + sprite_height {
                let sprite_x = cpu.memory[base + 1] as i16 - 8;
                line_sprites[count] = (i, sprite_x);
                count += 1;
                if count >= 10 { break; }
            }
        }

        let sprites = &mut line_sprites[..count];
        if !is_cgb {
            sprites.sort_by(|a, b| a.1.cmp(&b.1));
        }

        for idx in (0..count).rev() {
            let (oam_idx, sprite_x) = sprites[idx];
            if sprite_x >= 160 { continue; }
            if object_schedule.as_ref().is_some_and(|schedule| !schedule.fetched[oam_idx as usize]) {
                continue;
            }
            let base        = 0xFE00 + (oam_idx as usize) * 4;
            let attributes  = cpu.memory[base + 3];

            let pal_type     = if attributes & 0x10 != 0 { PAL_OBJ1 } else { PAL_OBJ0 };
            let flip_x       = attributes & 0x20 != 0;
            let obj_priority = (attributes & 0x80) == 0;
            let cgb_pal      = attributes & 0x07;

            let first_dot = object_schedule.as_ref().map_or(0, |schedule| schedule.first_output[oam_idx as usize]);
            let (b1, b2) = objects::fetch_object_planes(cpu, snap, log, ly, oam_idx, first_dot);

            for px in 0..8u8 {
                let sx = sprite_x as i32 + px as i32;
                if sx < 0 || sx >= 160 { continue; }
                let sx = sx as usize;

                let dx  = if flip_x { 7 - px } else { px };
                let raw = get_color_index(b1, b2, dx);
                if raw == 0 { continue; }

                // O(1) lookup via pre-built per-pixel palette state.
                let cur_lcdc = if log.is_empty() { px_lcdc_sp[sx] } else {
                    (px_lcdc_sp[sx] & !2)
                        | (registers_at(snap, log, pixel_dots[sx].saturating_sub(1)).lcdc & 2)
                };
                if cur_lcdc & 0x02 == 0 { continue; }
                let obp = if attributes & 0x10 != 0 { px_obp1[sx] } else { px_obp0[sx] };

                let bg_pixel         = cpu.frame_buffer[buf_base + sx];
                let meta             = (bg_pixel >> 24) as u8;
                let bg_raw           = meta & 0x03;
                let bg_priority_attr = (meta & 0x04) != 0;
                let lcdc_b0          = cur_lcdc & 0x01 != 0;

                let draw = if is_cgb {
                    if !lcdc_b0                            { true  }
                    else if bg_priority_attr || !obj_priority { bg_raw == 0 }
                    else                                   { true  }
                } else {
                    obj_priority || bg_raw == 0
                };

                if draw {
                    let (r, g, b) = if is_cgb {
                        get_cgb_color(&cpu.cgb_obj_palettes, cgb_pal, raw)
                    } else {
                        let ci    = apply_dmg_palette(raw, obp);
                        let color = &palettes[pal_type as usize][ci as usize];
                        (color[0], color[1], color[2])
                    };
                    cpu.frame_buffer[buf_base + sx] = pack_cgb_pixel(r, g, b, raw, false);
                }
            }
        }
    }
}

#[cfg(target_arch = "wasm32")]
pub fn draw_state(context: &web_sys::CanvasRenderingContext2d, cpu: &mut crate::cpu::CPU) {
    let lcd_control = cpu.memory[0xFF40];

    if lcd_control & 0x80 == 0x80 {
        let mut data = [0u8; 160 * 144 * 4];
        for (i, &pixel) in cpu.frame_buffer.iter().enumerate() {
            let off = i * 4;
            data[off] = (pixel & 0xFF) as u8;
            data[off + 1] = ((pixel >> 8) & 0xFF) as u8;
            data[off + 2] = ((pixel >> 16) & 0xFF) as u8;
            data[off + 3] = 255;
        }

        let image_data = web_sys::ImageData::new_with_u8_clamped_array_and_sh(Clamped(&data), 160, 144).unwrap();
        context.put_image_data(&image_data, 0.0, 0.0).unwrap();
    } else {
        context.clear_rect(0.0, 0.0, 160.0, 144.0);
    }
}

#[cfg(target_arch = "wasm32")]
pub fn draw_vram(context: &web_sys::CanvasRenderingContext2d, cpu: &mut CPU) {
    let mut buffer =[0u8; 384 * 8 * 8];
    let tile_data = if cpu.is_cgb { &cpu.cgb_vram[0][0..0x1800] } else { &cpu.memory[0x8000..0x9800] };
    for tile_index in 0..384usize {
        let x = tile_index % 16;
        let y = tile_index / 16;
        let offset: u16 = tile_index as u16 * 16;
        for ty in 0..8usize {
            let b1 = tile_data[(offset + (ty as u16 * 2)) as usize];
            let b2 = tile_data[(offset + (ty as u16 * 2 + 1)) as usize];
            for tx in 0..8usize {
                let raw = get_color_index(b1, b2, tx as u8);
                let index = (y * 8 + ty) * 128 + (x * 8 + tx);
                if index < buffer.len() {
                    buffer[index] = raw;
                }
            }
        }
    }
    let mut data = Vec::with_capacity(128 * 192 * 4);
    for &raw in buffer.iter() {
        let (r, g, b) = if cpu.is_cgb {
            get_cgb_color(&cpu.cgb_bg_palettes, 0, raw)
        } else {
            let palettes = if cpu.color_mode == 0 { &cpu.gbc_palettes } else { &DEFAULT_GBC_PALETTES };
            let ci = apply_dmg_palette(raw, cpu.memory[0xFF47]);
            let color = palettes[0][ci as usize];
            (color[0], color[1], color[2])
        };
        data.push(r);
        data.push(g);
        data.push(b);
        data.push(255);
    }
    let image_data = web_sys::ImageData::new_with_u8_clamped_array_and_sh(Clamped(&data), 128, 192).unwrap();
    context.put_image_data(&image_data, 0.0, 0.0).unwrap();
}

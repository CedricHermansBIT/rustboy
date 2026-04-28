use crate::cpu::CPU;
#[cfg(target_arch = "wasm32")]
use wasm_bindgen::Clamped;

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

pub fn draw_scanline(cpu: &mut crate::cpu::CPU) {
    let ly = cpu.memory[0xFF44];
    if ly >= 144 { return; }

    let lcd_control = cpu.memory[0xFF40];
    let scroll_y = cpu.memory[0xFF42];
    let scroll_x = cpu.memory[0xFF43];
    let bgp = cpu.memory[0xFF47];

    let lcdc_bit0 = lcd_control & 0x01 != 0;
    let master_bg_enable = cpu.is_cgb || lcdc_bit0;

    let palettes = if cpu.color_mode == 0 {
        &cpu.gbc_palettes
    } else {
        &DEFAULT_GBC_PALETTES
    };

    if lcd_control & 0x80 == 0x80 {
        let tile_map_base: usize = if lcd_control & 0x08 == 0x08 { 0x9C00 } else { 0x9800 };
        let tile_data_base: usize = if lcd_control & 0x10 == 0x10 { 0x8000 } else { 0x8800 };

        // 1. DRAW BACKGROUND FOR THIS LINE
        let dy = ly.wrapping_add(scroll_y);
        for x in 0..160u32 {
            let dx = ((x + scroll_x as u32) & 0xFF) as u8;
            let tile_map_addr = tile_map_base + (dy as usize / 8) * 32 + (dx as usize / 8);

            let tile_index = if cpu.is_cgb {
                cpu.cgb_vram[0][tile_map_addr - 0x8000]
            } else {
                cpu.memory[tile_map_addr]
            };

            let attr = if cpu.is_cgb {
                cpu.cgb_vram[1][tile_map_addr - 0x8000]
            } else {
                0
            };

            let vram_bank = if cpu.is_cgb { ((attr >> 3) & 1) as usize } else { 0 };
            let flip_x = cpu.is_cgb && (attr & 0x20) != 0;
            let flip_y = cpu.is_cgb && (attr & 0x40) != 0;
            let cgb_pal = attr & 0x07;
            let bg_priority = cpu.is_cgb && (attr & 0x80) != 0;

            let offset: u16 = if tile_data_base == 0x8000 {
                tile_index as u16 * 16
            } else {
                ((tile_index as i8 as i16 + 128) as u16) * 16
            };

            let ty = if flip_y { 7 - (dy % 8) } else { dy % 8 };
            let line_offset = offset + (ty as u16 * 2);

            let addr = tile_data_base + line_offset as usize;

            let b1 = if cpu.is_cgb {
                let base = addr - 0x8000;
                cpu.cgb_vram[vram_bank][base]
            } else {
                cpu.memory[addr]
            };

            let b2 = if cpu.is_cgb {
                let base = addr - 0x8000;
                cpu.cgb_vram[vram_bank][base + 1]
            } else {
                cpu.memory[addr + 1]
            };

            let px = if flip_x { 7 - (dx % 8) } else { dx % 8 };
            let raw = get_color_index(b1, b2, px);

            let (r, g, b) = if cpu.is_cgb {
                get_cgb_color(&cpu.cgb_bg_palettes, cgb_pal, raw)
            } else {
                let raw_effective = if master_bg_enable { raw } else { 0 };
                let ci = apply_dmg_palette(raw_effective, bgp);
                let color = &palettes[PAL_BG as usize][ci as usize];
                (color[0], color[1], color[2])
            };

            cpu.frame_buffer[(ly as usize * 160) + x as usize] = pack_cgb_pixel(r, g, b, raw, bg_priority);
        }

        // 2. DRAW WINDOW FOR THIS LINE
        if lcd_control & 0x20 != 0 && master_bg_enable {
            let window_y = cpu.memory[0xFF4A];
            let window_x = cpu.memory[0xFF4B] as i32 - 7;
            let window_tile_map_base: usize = if lcd_control & 0x40 != 0 { 0x9C00 } else { 0x9800 };

            if ly >= window_y {
                let wy = cpu.window_line_counter;
                for screen_x in 0..160i32 {
                    if screen_x >= window_x {
                        let wx = (screen_x - window_x) as u8;
                        let tile_map_addr = window_tile_map_base + (wy as usize / 8) * 32 + (wx as usize / 8);

                        let tile_index = if cpu.is_cgb {
                            cpu.cgb_vram[0][tile_map_addr - 0x8000]
                        } else {
                            cpu.memory[tile_map_addr]
                        };

                        let attr = if cpu.is_cgb {
                            cpu.cgb_vram[1][tile_map_addr - 0x8000]
                        } else {
                            0
                        };

                        let vram_bank = if cpu.is_cgb { ((attr >> 3) & 1) as usize } else { 0 };
                        let flip_x = cpu.is_cgb && (attr & 0x20) != 0;
                        let flip_y = cpu.is_cgb && (attr & 0x40) != 0;
                        let cgb_pal = attr & 0x07;
                        let bg_priority = cpu.is_cgb && (attr & 0x80) != 0;

                        let offset: u16 = if tile_data_base == 0x8000 {
                            tile_index as u16 * 16
                        } else {
                            ((tile_index as i8 as i16 + 128) as u16) * 16
                        };

                        let ty = if flip_y { 7 - (wy % 8) } else { wy % 8 };
                        let line_offset = offset + (ty as u16 * 2);

                        let addr = tile_data_base + line_offset as usize;

                        let b1 = if cpu.is_cgb {
                            let base = addr - 0x8000;
                            cpu.cgb_vram[vram_bank][base]
                        } else {
                            cpu.memory[addr]
                        };

                        let b2 = if cpu.is_cgb {
                            let base = addr - 0x8000;
                            cpu.cgb_vram[vram_bank][base + 1]
                        } else {
                            cpu.memory[addr + 1]
                        };

                        let px = if flip_x { 7 - (wx % 8) } else { wx % 8 };
                        let raw = get_color_index(b1, b2, px);

                        let (r, g, b) = if cpu.is_cgb {
                            get_cgb_color(&cpu.cgb_bg_palettes, cgb_pal, raw)
                        } else {
                            let ci = apply_dmg_palette(raw, bgp);
                            let color = &palettes[PAL_BG as usize][ci as usize];
                            (color[0], color[1], color[2])
                        };

                        cpu.frame_buffer[(ly as usize * 160) + screen_x as usize] = pack_cgb_pixel(r, g, b, raw, bg_priority);
                    }
                    cpu.window_line_counter += 1;
                }
            }
        }

        // 3. DRAW SPRITES FOR THIS LINE
        if lcd_control & 0x02 != 0 {
            let sprite_height: i16 = if lcd_control & 0x04 != 0 { 16 } else { 8 };
            let obp0 = cpu.memory[0xFF48];
            let obp1 = cpu.memory[0xFF49];
            let ly_i16 = ly as i16;

            let mut line_sprites: [(u8, i16); 10] =[(0, 0); 10]; // (oam_index, sprite_x)
            let mut count = 0usize;

            for i in 0..40u8 {
                let base = 0xFE00 + (i as usize) * 4;
                let sprite_y = cpu.memory[base] as i16 - 16;
                if ly_i16 >= sprite_y && ly_i16 < sprite_y + sprite_height {
                    let sprite_x = cpu.memory[base + 1] as i16 - 8;
                    line_sprites[count] = (i, sprite_x);
                    count += 1;
                    if count >= 10 { break; }
                }
            }

            let sprites = &mut line_sprites[..count];
            if !cpu.is_cgb {
                sprites.sort_by(|a, b| a.1.cmp(&b.1));
            }

            for idx in (0..count).rev() {
                let (oam_idx, sprite_x) = sprites[idx];
                let base = 0xFE00 + (oam_idx as usize) * 4;
                let sprite_y = cpu.memory[base] as i16 - 16;
                let mut tile_index = cpu.memory[base + 2];
                let attributes = cpu.memory[base + 3];

                let obp = if attributes & 0x10 != 0 { obp1 } else { obp0 };
                let pal_type = if attributes & 0x10 != 0 { PAL_OBJ1 } else { PAL_OBJ0 };
                let flip_x = attributes & 0x20 != 0;
                let flip_y = attributes & 0x40 != 0;
                let obj_priority = (attributes & 0x80) == 0;
                let vram_bank = if cpu.is_cgb { ((attributes >> 3) & 1) as usize } else { 0 };
                let cgb_pal = attributes & 0x07;

                let mut row = (ly_i16 - sprite_y) as u8;
                if sprite_height == 16 {
                    tile_index &= 0xFE;
                    if flip_y { row = 15 - row; }
                    if row >= 8 {
                        tile_index |= 0x01;
                        row -= 8;
                    }
                } else {
                    if flip_y { row = 7 - row; }
                }

                let line_offset = (tile_index as u16) * 16 + (row as u16) * 2;

                let b1 = if cpu.is_cgb { cpu.cgb_vram[vram_bank][line_offset as usize] } else { cpu.memory[0x8000 + line_offset as usize] };
                let b2 = if cpu.is_cgb { cpu.cgb_vram[vram_bank][line_offset as usize + 1] } else { cpu.memory[0x8000 + line_offset as usize + 1] };

                for px in 0..8u8 {
                    let screen_x = sprite_x as i32 + px as i32;
                    if screen_x < 0 || screen_x >= 160 { continue; }

                    let dx = if flip_x { 7 - px } else { px };
                    let raw = get_color_index(b1, b2, dx);
                    if raw == 0 { continue; }

                    let buffer_idx = ly as usize * 160 + screen_x as usize;
                    let bg_pixel = cpu.frame_buffer[buffer_idx];
                    let meta = (bg_pixel >> 24) as u8;
                    let bg_raw = meta & 0x03;
                    let bg_priority_attr = (meta & 0x04) != 0;

                    let mut draw = true;
                    if cpu.is_cgb {
                        if !lcdc_bit0 {
                            draw = true;
                        } else if bg_priority_attr || !obj_priority {
                            if bg_raw != 0 { draw = false; }
                        }
                    } else {
                        if !obj_priority && bg_raw != 0 {
                            draw = false;
                        }
                    }

                    if draw {
                        let (r, g, b) = if cpu.is_cgb {
                            get_cgb_color(&cpu.cgb_obj_palettes, cgb_pal, raw)
                        } else {
                            let ci = apply_dmg_palette(raw, obp);
                            let color = &palettes[pal_type as usize][ci as usize];
                            (color[0], color[1], color[2])
                        };
                        cpu.frame_buffer[buffer_idx] = pack_cgb_pixel(r, g, b, raw, false);
                    }
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
use super::*;
fn bus() -> Bus {
    let mut bus = Bus::new(vec![0; 0xc0], None).unwrap();
    bus.set_io(0, 0);
    // OAM reset contents would enable all sprites. Tests explicitly disable
    // entries they do not use, exactly as software must do on the real device.
    for i in 0..128 {
        put(&mut bus.oam, i * 8, 0x200);
    }
    bus
}
fn put(data: &mut [u8], offset: usize, value: u16) {
    data[offset..offset + 2].copy_from_slice(&value.to_le_bytes());
}
fn pal(bus: &mut Bus, index: usize, color: u16) {
    put(&mut bus.palette, index * 2, color);
}
fn pixel(bus: &Bus, x: usize, y: u16) -> [u8; 4] {
    let mut pixels = vec![0; 240 * 160 * 4];
    render_line(bus, y, &mut pixels);
    pixels[(y as usize * 240 + x) * 4..(y as usize * 240 + x + 1) * 4]
        .try_into()
        .unwrap()
}
fn bg4(bus: &mut Bus, bg: usize, tile: usize, color: u8, priority: u16) {
    bus.set_io(0, bus.io16(0) | (1 << (8 + bg)));
    bus.set_io(8 + bg * 2, priority | (((28 + bg) as u16) << 8));
    bus.vram[tile * 32..tile * 32 + 32].fill(color * 17);
    for entry in 0..1024 {
        put(&mut bus.vram, (28 + bg) * 0x800 + entry * 2, tile as u16);
    }
}
fn obj(bus: &mut Bus, index: usize, a0: u16, a1: u16, a2: u16) {
    put(&mut bus.oam, index * 8, a0);
    put(&mut bus.oam, index * 8 + 2, a1);
    put(&mut bus.oam, index * 8 + 4, a2);
    bus.set_io(0, bus.io16(0) | 0x1000);
}
const RED: [u8; 4] = [255, 0, 0, 255];
const GREEN: [u8; 4] = [0, 255, 0, 255];
const BLUE: [u8; 4] = [0, 0, 255, 255];
const BLACK: [u8; 4] = [0, 0, 0, 255];

#[test]
fn backdrop_does_not_alpha_blend_with_itself() {
    let mut bus = bus();
    pal(&mut bus, 0, 31);
    bus.set_io(0x50, 1 << 5 | 1 << 6 | 1 << 13);
    bus.set_io(0x52, 0);
    assert_eq!(pixel(&bus, 0, 0), RED);
}

#[test]
fn eight_bit_obj_uses_full_palette_and_mapping_alignment() {
    let mut bus = bus();
    pal(&mut bus, 256 + 20, 31);
    pal(&mut bus, 256 + 21, 31 << 5);
    bus.vram[0x10000] = 20;
    bus.vram[0x10020] = 21;
    obj(&mut bus, 0, 0x2000, 0, 1 | 15 << 12);
    assert_eq!(pixel(&bus, 0, 0), RED); // 2D ignores tile-number bit 0
    bus.set_io(0, 0x1040);
    assert_eq!(pixel(&bus, 0, 0), GREEN); // 1D preserves it
}

#[test]
fn double_size_y128_is_not_wrapped_to_top() {
    let mut bus = bus();
    pal(&mut bus, 257, 31);
    bus.vram[0x10000..0x10800].fill(0x11);
    obj(&mut bus, 0, 0x300 | 128, 3 << 14, 0);
    put(&mut bus.oam, 6, 0x80);
    put(&mut bus.oam, 30, 0x80);
    assert_eq!(pixel(&bus, 32, 128), RED);
    assert_eq!(pixel(&bus, 32, 0), BLACK);
}

#[test]
fn forced_blank_and_backdrop_are_opaque() {
    let mut bus = bus();
    pal(&mut bus, 0, 31);
    assert_eq!(pixel(&bus, 0, 0), RED);
    bus.set_io(0, 0x80);
    assert_eq!(pixel(&bus, 239, 159), [255; 4]);
}
#[test]
fn text_nibbles_palettes_and_zero_transparency() {
    let mut bus = bus();
    bg4(&mut bus, 0, 0, 0, 0);
    pal(&mut bus, 0, 31 << 10);
    pal(&mut bus, 3 * 16 + 1, 31);
    pal(&mut bus, 3 * 16 + 2, 31 << 5);
    put(&mut bus.vram, 28 * 0x800, 3 << 12);
    bus.vram[0] = 0x21;
    assert_eq!(pixel(&bus, 0, 0), RED);
    assert_eq!(pixel(&bus, 1, 0), GREEN);
    assert_eq!(pixel(&bus, 2, 0), BLUE);
}
#[test]
fn text_8bpp_flips_and_color_bank_is_ignored() {
    let mut bus = bus();
    bus.set_io(0, 0x100);
    bus.set_io(8, 0x80 | (28 << 8));
    put(&mut bus.vram, 28 * 0x800, 0xfc00); // both flips, palette 15 ignored
    bus.vram[63] = 1;
    pal(&mut bus, 1, 31);
    assert_eq!(pixel(&bus, 0, 0), RED);
    assert_eq!(pixel(&bus, 7, 7), BLACK);
}
#[test]
fn text_large_maps_select_blocks_and_wrap_scroll() {
    let mut bus = bus();
    bus.set_io(0, 0x100);
    bus.set_io(8, 3 << 14 | 24 << 8);
    for t in 0..4 {
        bus.vram[t * 32..t * 32 + 32].fill((t as u8 + 1) * 17);
    }
    for block in 0..4 {
        put(&mut bus.vram, 24 * 0x800 + block * 0x800, block as u16);
    }
    pal(&mut bus, 1, 31);
    pal(&mut bus, 2, 31 << 5);
    pal(&mut bus, 3, 31 << 10);
    pal(&mut bus, 4, 0x7fff);
    assert_eq!(pixel(&bus, 0, 0), RED);
    bus.set_io(0x10, 256);
    assert_eq!(pixel(&bus, 0, 0), GREEN);
    bus.set_io(0x12, 256);
    assert_eq!(pixel(&bus, 0, 0), [255; 4]);
    bus.set_io(0x10, 0);
    assert_eq!(pixel(&bus, 0, 0), BLUE);
    bus.set_io(0x10, 511);
    bus.set_io(0x12, 511);
    assert_eq!(pixel(&bus, 1, 1), RED);
}
#[test]
fn bg_ties_lower_number_wins_and_priority_overrides_number() {
    let mut bus = bus();
    bg4(&mut bus, 0, 0, 1, 1);
    bg4(&mut bus, 1, 1, 2, 1);
    pal(&mut bus, 1, 31);
    pal(&mut bus, 2, 31 << 5);
    assert_eq!(pixel(&bus, 0, 0), RED);
    bus.set_io(10, 29 << 8);
    assert_eq!(pixel(&bus, 0, 0), GREEN);
}
#[test]
fn affine_tiles_signed_reference_and_overflow() {
    let mut bus = bus();
    bus.set_io(0, 2 | 0x400);
    bus.set_io(12, 28 << 8);
    bus.set_io(0x20, 0x100);
    bus.set_io(0x26, 0x100);
    bus.vram[28 * 0x800] = 1;
    bus.vram[64..128].fill(1);
    pal(&mut bus, 1, 31);
    assert_eq!(pixel(&bus, 0, 0), RED);
    assert_eq!(pixel(&bus, 128, 0), BLACK);
    bus.io[0x28..0x2c].copy_from_slice(&(-256i32).to_le_bytes());
    assert_eq!(pixel(&bus, 0, 0), BLACK);
    assert_eq!(pixel(&bus, 1, 0), RED);
    bus.set_io(12, 28 << 8 | 0x2000);
    bus.vram[28 * 0x800 + 15] = 1;
    assert_eq!(pixel(&bus, 0, 0), RED);
    assert_eq!(pixel(&bus, 129, 0), RED);
}
#[test]
fn affine_bitmap_uses_matrix_and_does_not_wrap() {
    let mut bus = bus();
    bus.set_io(0, 3 | 0x400);
    bus.set_io(0x20, 0x200);
    bus.set_io(0x26, 0x100);
    put(&mut bus.vram, 4, 31);
    put(&mut bus.vram, (240 + 2) * 2, 31 << 5);
    assert_eq!(pixel(&bus, 1, 0), RED);
    assert_eq!(pixel(&bus, 1, 1), GREEN);
    bus.set_io(12, 0x2000);
    assert_eq!(pixel(&bus, 120, 0), BLACK);
}
#[test]
fn bitmap_page_and_index_zero_transparency() {
    let mut bus = bus();
    bus.set_io(0, 4 | 0x400);
    bus.set_io(0x20, 0x100);
    bus.set_io(0x26, 0x100);
    pal(&mut bus, 0, 31 << 5);
    pal(&mut bus, 1, 31);
    bus.vram[0] = 1;
    bus.vram[0xa000] = 2;
    pal(&mut bus, 2, 31 << 10);
    assert_eq!(background(&bus, 2, 4, 1, 0).map(|d| d.color), None);
    assert_eq!(pixel(&bus, 0, 0), RED);
    bus.set_io(0, 4 | 0x410);
    assert_eq!(pixel(&bus, 0, 0), BLUE);
    bus.set_io(0, 5 | 0x410);
    put(&mut bus.vram, 0xa000, 31);
    assert_eq!(pixel(&bus, 0, 0), RED);
    assert_eq!(pixel(&bus, 160, 0), GREEN);
}
#[test]
fn sprite_palette_transparency_and_bg_priority_ties() {
    let mut bus = bus();
    bg4(&mut bus, 0, 0, 1, 1);
    pal(&mut bus, 1, 31 << 5);
    pal(&mut bus, 256 + 3 * 16 + 1, 31);
    bus.vram[0x10000] = 0x01;
    obj(&mut bus, 0, 0, 0, 3 << 12 | 1 << 10);
    assert_eq!(pixel(&bus, 0, 0), RED);
    assert_eq!(pixel(&bus, 1, 0), GREEN);
    put(&mut bus.oam, 4, 3 << 12 | 2 << 10);
    assert_eq!(pixel(&bus, 0, 0), GREEN);
}
#[test]
fn oam_order_resolves_objects_before_bg_priority() {
    let mut bus = bus();
    pal(&mut bus, 257, 31);
    pal(&mut bus, 258, 31 << 5);
    bus.vram[0x10000..0x10020].fill(0x11);
    bus.vram[0x10020..0x10040].fill(0x22);
    obj(&mut bus, 0, 0, 0, 3 << 10);
    obj(&mut bus, 1, 0, 0, 1);
    assert_eq!(pixel(&bus, 0, 0), RED);
}
#[test]
fn obj_mapping_flips_and_signed_positions() {
    let mut bus = bus();
    pal(&mut bus, 257, 31);
    pal(&mut bus, 258, 31 << 5);
    obj(&mut bus, 0, 0, 1 << 14, 0); // 16x16, 2D mapping
    bus.vram[0x10000] = 0x11;
    bus.vram[0x10000 + 32 * 32] = 0x22;
    assert_eq!(pixel(&bus, 0, 0), RED);
    assert_eq!(pixel(&bus, 0, 8), GREEN);
    bus.set_io(0, 0x1040);
    bus.vram[0x10000 + 2 * 32] = 0x22;
    assert_eq!(pixel(&bus, 0, 8), GREEN);
    obj(&mut bus, 0, 0, 0x1000 | 511, 0);
    bus.vram[0x10003] = 0x10;
    assert_eq!(pixel(&bus, 0, 0), BLACK);
    bus.vram[0x10003] = 0x11;
    assert_eq!(pixel(&bus, 0, 0), RED);
}
#[test]
fn affine_sprite_double_size_centers_source() {
    let mut bus = bus();
    pal(&mut bus, 257, 31);
    bus.vram[0x10000..0x10020].fill(0x11);
    obj(&mut bus, 0, 0x300, 0, 0);
    put(&mut bus.oam, 6, 0x100);
    put(&mut bus.oam, 30, 0x100);
    assert_eq!(pixel(&bus, 0, 0), BLACK);
    assert_eq!(pixel(&bus, 4, 4), RED);
    assert_eq!(pixel(&bus, 11, 11), RED);
    assert_eq!(pixel(&bus, 12, 12), BLACK);
}
#[test]
fn bitmap_rejects_obj_tiles_in_framebuffer_area() {
    let mut bus = bus();
    pal(&mut bus, 257, 31);
    bus.set_io(0, 3);
    bus.vram[0x10000] = 0x11;
    bus.vram[0x14000] = 0x11;
    obj(&mut bus, 0, 0, 0, 0);
    assert_eq!(pixel(&bus, 0, 0), BLACK);
    put(&mut bus.oam, 4, 512);
    assert_eq!(pixel(&bus, 0, 0), RED);
}
#[test]
fn regular_windows_priority_and_layer_masks() {
    let mut bus = bus();
    bg4(&mut bus, 0, 0, 1, 0);
    pal(&mut bus, 1, 31);
    bus.set_io(0, 0x6100);
    bus.set_io(0x40, 2 << 8 | 6);
    bus.set_io(0x42, 8);
    bus.set_io(0x44, 8);
    bus.set_io(0x46, 8);
    bus.set_io(0x48, 0x0100);
    bus.set_io(0x4a, 0);
    assert_eq!(pixel(&bus, 1, 0), RED);
    assert_eq!(pixel(&bus, 2, 0), BLACK);
    assert_eq!(pixel(&bus, 6, 0), RED);
    assert_eq!(pixel(&bus, 8, 0), BLACK);
    assert_eq!(pixel(&bus, 1, 8), BLACK);
}
#[test]
fn obj_window_masks_bgs_without_displaying_its_color() {
    let mut bus = bus();
    bg4(&mut bus, 0, 0, 1, 0);
    pal(&mut bus, 1, 31);
    bus.vram[0x10000] = 0x01;
    obj(&mut bus, 0, 2 << 10, 0, 0);
    bus.set_io(0, bus.io16(0) | 0x8000);
    bus.set_io(0x4a, 0x0100);
    assert_eq!(pixel(&bus, 0, 0), RED);
    assert_eq!(pixel(&bus, 1, 0), BLACK);
}
#[test]
fn blending_requires_immediately_adjacent_target_and_clamps_coefficients() {
    let mut bus = bus();
    bg4(&mut bus, 0, 0, 1, 0);
    bg4(&mut bus, 1, 1, 2, 1);
    bg4(&mut bus, 2, 2, 3, 2);
    pal(&mut bus, 1, 31);
    pal(&mut bus, 2, 31 << 5);
    pal(&mut bus, 3, 31 << 10);
    bus.set_io(0x50, 1 | 1 << 6 | 1 << 10);
    bus.set_io(0x52, 8 | 8 << 8);
    assert_eq!(pixel(&bus, 0, 0), RED);
    bus.set_io(0x50, 1 | 1 << 6 | 1 << 9);
    assert_eq!(pixel(&bus, 0, 0), [123, 123, 0, 255]);
    bus.set_io(0x52, 31 | 31 << 8);
    assert_eq!(pixel(&bus, 0, 0), [255, 255, 0, 255]);
}
#[test]
fn semi_transparent_obj_forces_alpha_unless_window_disables_effects() {
    let mut bus = bus();
    bg4(&mut bus, 0, 0, 1, 0);
    pal(&mut bus, 1, 31 << 5);
    pal(&mut bus, 257, 31);
    bus.vram[0x10000..0x10020].fill(0x11);
    obj(&mut bus, 0, 1 << 10, 0, 0);
    bus.set_io(0x50, 1 << 8);
    bus.set_io(0x52, 8 | 8 << 8);
    assert_eq!(pixel(&bus, 0, 0), [123, 123, 0, 255]);
    bus.set_io(0, bus.io16(0) | 0x2000);
    bus.set_io(0x4a, 0x1f);
    assert_eq!(pixel(&bus, 0, 0), RED);
}
#[test]
fn brightness_and_mosaic_sample_upper_left() {
    let mut bus = bus();
    bg4(&mut bus, 0, 0, 0, 0);
    bus.vram[0] = 0x21;
    pal(&mut bus, 1, 31);
    pal(&mut bus, 2, 31 << 5);
    bus.set_io(8, bus.io16(8) | 0x40);
    bus.set_io(0x4c, 1 | 1 << 4);
    assert_eq!(pixel(&bus, 0, 0), RED);
    assert_eq!(pixel(&bus, 1, 1), RED);
    bus.set_io(0x50, 1 | 2 << 6);
    bus.set_io(0x54, 16);
    assert_eq!(pixel(&bus, 0, 0), [255; 4]);
    bus.set_io(0x50, 1 | 3 << 6);
    assert_eq!(pixel(&bus, 0, 0), BLACK);
}

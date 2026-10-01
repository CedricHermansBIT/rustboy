//! End-to-end, original synthetic ROMs; no external firmware or copyrighted
//! game fixtures. Commands are sent by executed LR35902 instructions.
use crate::{emulator::*, GameBoy, HardwareModel};

fn rom() -> Vec<u8> {
    let mut rom = vec![0; 32768];
    rom[0x100..0x103].copy_from_slice(&[0xC3, 0x50, 0x01]);
    rom[0x134..0x13B].copy_from_slice(b"SGBTEST");
    rom[0x143] = 0x80; // SGB is DMG hardware even for a dual-mode cart.
    rom[0x146] = 3;
    rom[0x14B] = 0x33;
    rom
}

fn packet(code: &mut Vec<u8>, bytes: &[u8; 16]) {
    let pulse = |code: &mut Vec<u8>, value| {
        code.extend_from_slice(&[0x3E, value, 0xE0, 0]);
        code.extend_from_slice(&[0; 5]);
        code.extend_from_slice(&[0x3E, 0x30, 0xE0, 0]);
        code.extend_from_slice(&[0; 15]);
    };
    pulse(code, 0);
    for &byte in bytes {
        for bit in 0..8 {
            pulse(code, if byte & (1 << bit) == 0 { 0x20 } else { 0x10 });
        }
    }
    pulse(code, 0x20);
}

fn write_packet(gb: &mut GameBoy, data: &[u8; 16]) {
    gb.cpu_mut().write_byte(0xFF00, 0);
    gb.cpu_mut().write_byte(0xFF00, 0x30);
    for &byte in data {
        for bit in 0..8 {
            gb.cpu_mut()
                .write_byte(0xFF00, if byte & (1 << bit) == 0 { 0x20 } else { 0x10 });
            gb.cpu_mut().write_byte(0xFF00, 0x30);
        }
    }
    gb.cpu_mut().write_byte(0xFF00, 0x20);
    gb.cpu_mut().write_byte(0xFF00, 0x30);
}

fn palette() -> [u8; 16] {
    // Shared white; palette 0: green, red, blue; palette 1: blue, green, red.
    [
        1, 0xFF, 0x7F, 0xE0, 3, 31, 0, 0, 0x7C, 0, 0x7C, 0xE0, 3, 31, 0, 0,
    ]
}

fn draw_rom() -> Vec<u8> {
    let mut rom = rom();
    let mut code = vec![0xF3, 0xAF, 0xE0, 0x40];
    packet(&mut code, &palette());
    // Vertical division: left palette 0, line palette 2, right palette 1.
    let mut divide = [0; 16];
    divide[0] = (6 << 3) | 1;
    divide[1] = (2 << 4) | 1;
    divide[2] = 10;
    packet(&mut code, &divide);
    code.extend_from_slice(&[
        0x3E, 0xE8, 0xE0, 0x47, // raw 1 maps to LCD shade 2
        0x3E, 0xFC, 0xE0, 0x48, // object raw 1 maps to LCD shade 3
        0x21, 0, 0x80,
    ]);
    for _ in 0..8 {
        code.extend_from_slice(&[0x3E, 0xFF, 0x22, 0xAF, 0x22]);
    }
    // Sprite at x=8..15, y=0..7, same tile as BG, above background.
    code.extend_from_slice(&[0x21, 0, 0xFE]);
    for value in [16, 16, 0, 0] {
        code.extend_from_slice(&[0x3E, value, 0x22]);
    }
    code.extend_from_slice(&[0x3E, 0x93, 0xE0, 0x40, 0x18, 0xFE]);
    rom[0x150..0x150 + code.len()].copy_from_slice(&code);
    rom
}

fn machine(rom: &[u8]) -> GameBoy {
    let mut gb = GameBoy::load(rom, &[0; 256], HardwareModel::Sgb).unwrap();
    gb.cpu_mut().booting = false;
    gb.cpu_mut().program_counter = 0x150;
    gb
}

#[test]
fn cartridge_instructions_color_post_bgp_and_obp_shades_by_screen_region() {
    let rom = draw_rom();
    let mut gb = machine(&rom);
    assert!(!gb.cpu().is_cgb);
    gb.run(70_224 * 4);
    assert_eq!(gb.cpu().sgb.as_ref().unwrap().commands_received, 2);
    let frame = gb.video_frame();
    assert_eq!(&frame.pixels[..4], &[255, 0, 0, 255]); // BG shade 2, not raw 1 green
    assert_eq!(&frame.pixels[8 * 4..9 * 4], &[0, 0, 255, 255]); // OBP shade 3
    assert_eq!(&frame.pixels[88 * 4..89 * 4], &[0, 255, 0, 255]); // right region palette 1
    assert_eq!((gb.cpu().frame_buffer[0] >> 24) & 3, 1);
    assert_eq!((gb.cpu().frame_buffer[0] >> 27) & 3, 2);
    // The same cartridge on a handheld never consumes its SGB packets.
    let mut handheld = GameBoy::load(&rom, &[0; 256], HardwareModel::Dmg).unwrap();
    handheld.cpu_mut().booting = false;
    handheld.cpu_mut().program_counter = 0x150;
    handheld.run(70_224 * 4);
    assert!(handheld.cpu().sgb.is_none());
    assert_ne!(&handheld.video_frame().pixels[..4], &[255, 0, 0, 255]);
}

#[test]
fn backend_ports_detection_reset_and_header_gating_use_the_cpu_bus() {
    let mut gb = machine(&rom());
    let mut request = [0; 16];
    request[0] = (0x11 << 3) | 1;
    request[1] = 3;
    write_packet(&mut gb, &request);
    assert_eq!(gb.cpu().peek(0xFF00), 0xFF);
    gb.set_button(0, Button::A, true).unwrap();
    gb.set_button(1, Button::B, true).unwrap();
    gb.cpu_mut().write_byte(0xFF00, 0x10);
    assert_eq!(gb.cpu().peek(0xFF00), 0xDE);
    gb.cpu_mut().write_byte(0xFF00, 0x30);
    assert_eq!(gb.cpu().peek(0xFF00), 0xFE);
    gb.cpu_mut().write_byte(0xFF00, 0x10);
    assert_eq!(gb.cpu().peek(0xFF00), 0xDD);
    assert!(gb.set_button(4, Button::A, true).is_err());
    gb.reset();
    assert_eq!(gb.cpu().sgb.as_ref().unwrap().players(), 1);
    assert_eq!(gb.cpu().sgb.as_ref().unwrap().commands_received, 0);
    assert!(!gb.cpu().is_cgb);
    let mut ordinary = rom();
    ordinary[0x146] = 0;
    let mut gb = machine(&ordinary);
    write_packet(&mut gb, &request);
    assert!(!gb.cpu().sgb.as_ref().unwrap().enabled());
    assert_eq!(gb.cpu().peek(0xFF00), 0xFF);
}

#[test]
fn sgb_state_roundtrip_preserves_frame_and_partial_command_and_rejects_cross_mode() {
    let rom = draw_rom();
    let mut gb = machine(&rom);
    gb.run(70_224 * 4);
    let expected = gb.video_frame().pixels.to_vec();
    let mut mask = [0; 16];
    mask[0] = (0x17 << 3) | 1;
    mask[1] = 1;
    write_packet(&mut gb, &mask);
    gb.cpu_mut().write_byte(0xFF00, 0);
    gb.cpu_mut().write_byte(0xFF00, 0x30);
    let saved = gb.export_state();
    let adapter = gb.cpu().sgb.as_deref().unwrap().clone();
    gb.reset();
    gb.import_state(&saved).unwrap();
    assert_eq!(gb.cpu().sgb.as_deref(), Some(&adapter));
    assert_eq!(gb.video_frame().pixels, expected);
    assert_eq!(gb.export_state(), saved);
    let mut handheld = GameBoy::load(&rom, &[], HardwareModel::Auto).unwrap();
    assert_ne!(gb.state_id(), handheld.state_id());
    assert_eq!(gb.save_key(), handheld.save_key()); // same cartridge battery save
    let old = handheld.export_state();
    assert!(handheld.import_state(&saved).is_err());
    assert_eq!(handheld.export_state(), old);
    assert!(gb.import_state(&old).is_err());
    assert_eq!(gb.export_state(), saved);
    let mut corrupt = saved.clone();
    corrupt[100] ^= 1;
    assert!(gb.import_state(&corrupt).is_err());
    assert_eq!(gb.export_state(), saved);
    // A correctly checksummed but malformed CPU payload must also be atomic.
    let mut truncated_cpu = Vec::from(&saved[..10]);
    truncated_cpu[6..10].copy_from_slice(&3u32.to_le_bytes());
    truncated_cpu.extend_from_slice(b"bad");
    truncated_cpu.extend_from_slice(&adapter.export_state());
    let checksum = truncated_cpu.iter().fold(0x811c9dc5u32, |h, b| {
        (h ^ *b as u32).wrapping_mul(0x01000193)
    });
    truncated_cpu.extend_from_slice(&checksum.to_le_bytes());
    assert!(gb.import_state(&truncated_cpu).is_err());
    assert_eq!(gb.export_state(), saved);
}

#[test]
fn bundled_boot_identifies_sgb_only_at_handoff_and_color_only_carts_are_rejected() {
    let mut rom = rom();
    rom[0x150..0x152].copy_from_slice(&[0x18, 0xFE]);
    let mut gb = GameBoy::load(&rom, &[], HardwareModel::Sgb).unwrap();
    assert_eq!(gb.cpu().get_reg_c(), 0);
    gb.run(70_224 * 240);
    assert!(!gb.cpu().booting);
    assert_eq!(gb.cpu().get_reg_a(), 1);
    assert_eq!(gb.cpu().get_reg_c(), 0x14);
    // Later writes to the boot-disable register must not clobber game registers.
    gb.cpu_mut().write_byte(0xFF50, 1);
    assert_eq!(gb.cpu().get_reg_c(), 0x14);
    rom[0x143] = 0xC0;
    assert!(GameBoy::load(&rom, &[], HardwareModel::Sgb).is_err());
}

#[test]
fn lcd_off_keeps_the_adapter_frame_and_handheld_state_format_stays_unchanged() {
    let mut gb = machine(&draw_rom());
    gb.run(70_224 * 4);
    let expected = gb.video_frame().pixels.to_vec();
    gb.cpu_mut().write_byte(0xFF40, 0);
    gb.run(70_224 * 2);
    let frame = gb.video_frame();
    assert!(frame.enabled);
    assert_eq!(frame.pixels, expected);
    let handheld = GameBoy::load(&rom(), &[], HardwareModel::Auto).unwrap();
    assert_eq!(handheld.export_state(), handheld.cpu().export_state());
    assert_eq!(&handheld.export_state()[..6], b"RBST\x02\0");
}

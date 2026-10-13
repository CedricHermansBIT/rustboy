use crate::{cpu::Memory, GameBoyAdvance, SaveType};
use rustboy_emulator_api::Emulator;
fn machine() -> GameBoyAdvance {
    let mut rom = vec![0; 192];
    rom[..4].copy_from_slice(&0xeafffffeu32.to_le_bytes());
    GameBoyAdvance::new(rom, None).unwrap()
}
#[test]
fn visible_palette_change_preserves_displayed_prefix_and_midline_state() {
    let mut m = machine();
    m.bus.set_io(0, 0);
    m.bus.write16(0x05000000, 31, false);
    m.bus.take_cycles();
    m.advance_video(446);
    m.bus.write16(0x05000000, 31 << 10, false);
    let cycles = m.bus.take_cycles();
    m.advance_video(cycles);
    let state = m.export_state();
    m.advance_video(u64::from(1006 - m.line_clocks));
    let expected = m.export_state();
    assert_eq!(&m.pixels[99 * 4..100 * 4], &[255, 0, 0, 255]);
    assert_eq!(&m.pixels[100 * 4..101 * 4], &[0, 0, 255, 255]);
    m.import_state(&state).unwrap();
    m.advance_video(u64::from(1006 - m.line_clocks));
    assert_eq!(m.export_state(), expected);
}
#[test]
fn save_override_and_pending_flash_replay_reset_and_battery_bytes() {
    let mut m = machine();
    m.set_save_type(SaveType::Flash64).unwrap();
    for (a, v) in [
        (0x5555, 0xaa),
        (0x2aaa, 0x55),
        (0x5555, 0xa0),
        (0x1234, 0x25),
    ] {
        m.bus.write8(0x0e000000 + a, v);
    }
    m.bus.take_cycles();
    let pending = m.export_state();
    assert_eq!(m.export_save()[0x1234], 255);
    m.advance_video(crate::backup::PROGRAM_CLOCKS);
    let expected = m.export_state();
    assert_eq!(m.export_save()[0x1234], 0x25);
    m.import_state(&pending).unwrap();
    m.advance_video(crate::backup::PROGRAM_CLOCKS);
    assert_eq!(m.export_state(), expected);
    m.reset();
    assert_eq!(m.save_type(), SaveType::Flash64);
    assert_eq!(m.export_save()[0x1234], 0x25);
    assert!(m.set_save_type(SaveType::None).is_err());
}
#[test]
fn display_register_read_masks_match_independent_hardware_expectations() {
    let mut m = machine();
    // mGBA suite src/io-read.c, BG0/1CNT, WININ/OUT, BLDCNT and BLDALPHA.
    for (offset, expected) in [
        (8, 0xdfff),
        (10, 0xdfff),
        (0x48, 0x3f3f),
        (0x4a, 0x3f3f),
        (0x50, 0x3fff),
        (0x52, 0x1f1f),
    ] {
        m.bus.write16(0x04000000 + offset, 0xffff, false);
        assert_eq!(m.bus.peek16(0x04000000 + offset), expected);
    }
}
#[test]
fn rtc_gpio_halfword_reads_use_device_and_snapshot_phase() {
    let mut m = machine();
    m.bus.rtc.enabled = true;
    m.bus.write16(0x080000c6, 7, false);
    m.bus.write16(0x080000c8, 1, false);
    assert_eq!(m.bus.peek16(0x080000c6), 7);
    assert_eq!(m.bus.read32(0x080000c4, false) >> 16, 7);
    m.bus.take_cycles();
    let state = m.export_state();
    m.advance_video(crate::CLOCK_HZ);
    m.import_state(&state).unwrap();
    assert_eq!(m.export_state(), state);
}
#[test]
fn rtc_battery_record_keeps_offline_time_and_rejects_corruption_atomically() {
    let mut m = machine();
    m.bus.rtc.enabled = true;
    m.set_rtc_time(1700000000).unwrap();
    let record = m.export_rtc(1700000000);
    m.import_rtc(&record, 1700000060).unwrap();
    assert_eq!(m.bus.rtc.unix_seconds, 1700000060);
    let valid = m.export_state();
    let mut bad = record;
    bad[20] ^= 1;
    assert!(m.import_rtc(&bad, 1700000060).is_err());
    assert_eq!(m.export_state(), valid);
}
#[test]
fn guest_hard_reset_retains_battery_and_clock_accounting() {
    let mut rom = vec![0; 192];
    rom[..4].copy_from_slice(&0xef260000u32.to_le_bytes());
    let mut m = GameBoyAdvance::new(rom, None).unwrap();
    m.set_save_type(SaveType::Sram).unwrap();
    m.bus.write8(0x0e000012, 0x5a);
    m.bus.take_cycles();
    m.bus.write8(0x02000000, 0x55);
    m.bus.take_cycles();
    assert!(m.step() > 0);
    assert!(m.clocks > 0);
    assert!(m.fault.is_none());
    assert_eq!(m.cpu.r[15], 0x08000000);
    assert_eq!(m.bus.peek8(0x02000000), 0);
    assert_eq!(m.export_save()[0x12], 0x5a);
    assert_eq!(m.save_type(), SaveType::Sram);
}

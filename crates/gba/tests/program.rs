//! Original synthetic cartridge; no commercial ROM, BIOS or third-party code.
use rustboy_gba::{GameBoyAdvance, FRAME_CYCLES};

#[test]
fn arm_cartridge_paints_bitmap_and_reaches_idle_loop() {
    // LDR display/literals; STRH DISPCNT; fill mode 3 red using a counted loop.
    let words: [u32; 14] = [
        0xe59f0020, 0xe59f1020, 0xe1c010b0, 0xe59f201c, 0xe59f301c, 0xe3a0101f, 0xe0c210b2,
        0xe2533001, 0x1afffffc, 0xeafffffe, 0x04000000, 0x00000403, 0x06000000, 38400,
    ];
    let mut rom = vec![0; 192];
    for (i, word) in words.into_iter().enumerate() {
        rom[i * 4..i * 4 + 4].copy_from_slice(&word.to_le_bytes());
    }
    rom[0xa0..0xac].copy_from_slice(b"RUSTBOY TEST");
    let mut m = GameBoyAdvance::new(rom, None).unwrap();
    m.run(FRAME_CYCLES * 6);
    assert!(m.fault.is_none());
    assert_eq!(m.cpu.exceptions, 0);
    assert_eq!(m.title(), "RUSTBOY TEST");
    assert_eq!(m.cpu.r[15], 0x08000024);
    assert_eq!(m.cpu.r[3], 0);
    assert_eq!(m.cpu.r[2], 0x06012c00);
    assert!(m
        .video_frame()
        .pixels
        .as_chunks::<4>()
        .0
        .iter()
        .all(|p| *p == [255, 0, 0, 255]));
}

#[test]
fn missing_firmware_stops_at_first_swi_without_fake_progress() {
    let mut rom = vec![0; 192];
    rom[..4].copy_from_slice(&0xef000000u32.to_le_bytes());
    let mut m = GameBoyAdvance::new(rom, None).unwrap();
    let clocks = m.run(FRAME_CYCLES);
    assert!(clocks < FRAME_CYCLES);
    assert_eq!(m.cpu.instructions, 1);
    let fault = m.fault.as_ref().unwrap();
    assert_eq!(fault.instruction_address, 0x08000000);
    assert_eq!(fault.vector, 8);
    assert_eq!(m.run(FRAME_CYCLES), 0);
    assert_eq!(m.step(), 0);
}

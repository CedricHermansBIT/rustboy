//! Original synthetic cartridge; no commercial ROM, BIOS or third-party code.
use rustboy_gba::cpu::Memory;
use rustboy_gba::{GameBoyAdvance, FRAME_CYCLES};

fn put_words(machine: &mut GameBoyAdvance, address: u32, words: &[u32]) {
    for (n, word) in words.iter().enumerate() {
        machine.bus.write32(address + n as u32 * 4, *word, false);
    }
    machine.bus.take_cycles();
}

#[test]
fn timer_dma_sound_reaches_frontend_and_replays_after_restore() {
    use rustboy_emulator_api::Emulator;
    let mut rom = vec![0; 192];
    rom[..4].copy_from_slice(&0xeafffffeu32.to_le_bytes());
    let mut m = GameBoyAdvance::new(rom, None).unwrap();
    // Original PCM fixture: alternating positive/negative levels, timer0 at
    // output rate, repeating sound DMA1. No external music or firmware.
    for n in 0..1024 {
        m.bus.write32(0x02000000 + n * 4, 0xc040c040, false);
    }
    m.bus.write16(0x04000084, 0x80, false);
    m.bus.write16(0x04000082, 0x0304, false);
    m.bus.write32(0x040000bc, 0x02000000, false);
    m.bus.write32(0x040000c0, 0x040000a0, false);
    m.bus.write32(0x040000c4, 0xb2000001, false);
    m.bus.write32(0x04000100, 0x0080fe00, false);
    m.bus.take_cycles();
    m.run(4096);
    let saved = m.export_state();
    let sound = m.drain_audio();
    assert_eq!((sound.sample_rate, sound.channels), (32768, 2));
    assert_eq!(sound.samples.len(), (m.clocks / 512) as usize * 2);
    assert!(sound.samples.contains(&0.5));
    assert!(sound.samples.contains(&-0.5));
    assert!(sound
        .samples
        .as_chunks::<2>()
        .0
        .iter()
        .all(|s| s[0] == s[1]));
    assert!(m.drain_audio().samples.is_empty());
    m.run(4096);
    let expected = m.drain_audio().samples;
    m.import_state(&saved).unwrap();
    m.drain_audio();
    m.run(4096);
    assert_eq!(m.drain_audio().samples, expected);
}
fn install_irq_handler(machine: &mut GameBoyAdvance) {
    // Independent handler: set BIOS flags, acknowledge VBlank, return via BX LR.
    put_words(
        machine,
        0x02000000,
        &[
            0xe59f0010, 0xe3a01001, 0xe1c010b0, 0xe59f0008, 0xe1c010b0, 0xe12fff1e, 0x03007ff8,
            0x04000202,
        ],
    );
    machine.bus.write32(0x03007ffc, 0x02000000, false);
    machine.bus.take_cycles();
}

#[test]
fn thumb_object_affine_service_writes_oam_and_returns_to_caller() {
    let mut rom = vec![0; 192];
    rom[..4].copy_from_slice(&[0x0f, 0xdf, 0xfe, 0xe7]);
    let mut m = GameBoyAdvance::new(rom, None).unwrap();
    m.cpu.set_cpsr(0x3f).unwrap();
    m.cpu.r[0] = 0x02000000;
    m.cpu.r[1] = 0x07000006;
    m.cpu.r[2] = 1;
    m.cpu.r[3] = 8;
    m.bus.write16(0x02000000, 256, false);
    m.bus.write16(0x02000002, 512, false);
    m.bus.write16(0x02000004, 0x4000, false);
    m.bus.take_cycles();
    m.step();
    assert!(m.fault.is_none());
    assert!(m.cpu.thumb());
    assert_eq!(m.cpu.r[15], 0x08000002);
    for (n, value) in [0, -256, 512, 0].into_iter().enumerate() {
        assert_eq!(m.bus.peek16(0x07000006 + n as u32 * 8), value as u16);
        assert_eq!(m.bus.peek16(0x07000000 + n as u32 * 8), 0);
    }
}
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
    rom[..4].copy_from_slice(&0xeffe0000u32.to_le_bytes());
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

#[test]
fn original_irq_shim_dispatches_handler_and_restores_cpu() {
    let mut rom = vec![0; 192];
    rom[..4].copy_from_slice(&0xeafffffeu32.to_le_bytes());
    let mut m = GameBoyAdvance::new(rom, None).unwrap();
    install_irq_handler(&mut m);
    for n in 0..13 {
        m.cpu.r[n] = 0x100 + n as u32;
    }
    let saved = m.cpu.r;
    m.bus.write16(0x04000200, 1, false);
    m.bus.write16(0x04000208, 1, false);
    m.bus.request_irq(1);
    m.bus.take_cycles();
    m.step();
    assert_eq!(m.cpu.cpsr() & 31, 0x12);
    for _ in 0..40 {
        m.step();
        if m.cpu.cpsr() & 31 == 0x1f {
            break;
        }
    }
    assert!(m.fault.is_none());
    assert_eq!(m.cpu.cpsr() & 31, 0x1f);
    assert_eq!(m.cpu.r, saved);
    assert_eq!(m.bus.peek16(0x03007ff8), 1);
    assert_eq!(m.bus.io16(0x202) & 1, 0);
}

#[test]
fn vblank_wait_runs_interrupt_handler_before_resuming_caller() {
    let mut rom = vec![0; 192];
    for (n, op) in [0xef050000u32, 0xe3a04077, 0xeafffffe]
        .into_iter()
        .enumerate()
    {
        rom[n * 4..n * 4 + 4].copy_from_slice(&op.to_le_bytes());
    }
    let mut m = GameBoyAdvance::new(rom, None).unwrap();
    install_irq_handler(&mut m);
    m.bus.write16(0x04000200, 1, false);
    m.bus.write16(0x04000004, 8, false);
    m.bus.take_cycles();
    m.run(10000);
    assert_eq!(m.cpu.r[4], 0);
    assert!(m.bus.halted);
    m.run(FRAME_CYCLES);
    assert!(m.fault.is_none());
    assert_eq!(m.cpu.r[4], 0x77);
    assert_eq!(m.cpu.cpsr() & 31, 0x1f);
    assert_eq!(m.bus.peek16(0x03007ff8) & 1, 0);
    assert_eq!(m.cpu.exceptions, 2);
}

#[test]
fn thumb_bios_call_returns_to_thumb_with_results() {
    let mut rom = vec![0; 192];
    rom[..4].copy_from_slice(&[6, 0xdf, 0xfe, 0xe7]);
    let mut m = GameBoyAdvance::new(rom, None).unwrap();
    m.cpu.set_cpsr(0x3f).unwrap();
    m.cpu.r[0] = (-1234i32) as u32;
    m.cpu.r[1] = 10;
    m.step();
    assert!(m.fault.is_none());
    assert!(m.cpu.thumb());
    assert_eq!(m.cpu.r[15], 0x08000002);
    assert_eq!(m.cpu.r[0], (-123i32) as u32);
    assert_eq!(m.cpu.r[1], (-4i32) as u32);
}

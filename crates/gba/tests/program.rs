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
fn original_cartridge_captures_a_bitmap_in_160_dma3_scanline_bursts() {
    use rustboy_emulator_api::Emulator;
    // CPU stores configure display and DMA3. Every pixel comes from scheduled
    // DMA, not host VRAM writes or another emulator's graphics/firmware code.
    let settings = [
        (0x04000000u32, 0x403u32),
        (0x040000d4, 0x08001000),
        (0x040000d8, 0x06000000),
        (0x040000dc, 0xb20000f0),
    ];
    let mut words = vec![];
    for _ in settings {
        words.extend_from_slice(&[0, 0, 0xe5801000]);
    }
    words.push(0xeafffffe);
    for (n, (address, value)) in settings.into_iter().enumerate() {
        let at = n * 3;
        words[at] = 0xe59f0000 | (words.len() as u32 * 4 - (at as u32 * 4 + 8));
        words.push(address);
        words[at + 1] = 0xe59f1000 | (words.len() as u32 * 4 - ((at + 1) as u32 * 4 + 8));
        words.push(value);
    }
    let mut rom = vec![0; 0x14000];
    for (n, word) in words.into_iter().enumerate() {
        rom[n * 4..n * 4 + 4].copy_from_slice(&word.to_le_bytes());
    }
    for row in 0..160usize {
        for col in 0..240usize {
            let color = ((row as u16 & 31) << 10) | (col as u16 & 31);
            let at = 0x1000 + (row * 240 + col) * 2;
            rom[at..at + 2].copy_from_slice(&color.to_le_bytes());
        }
    }
    let mut m = GameBoyAdvance::new(rom, None).unwrap();
    m.run(FRAME_CYCLES * 2);
    assert!(m.fault.is_none());
    assert_eq!(m.bus.peek16(0x040000de) & 0x8000, 0);
    for row in 0..160u32 {
        for col in 0..240u32 {
            let color = ((row as u16 & 31) << 10) | (col as u16 & 31);
            assert_eq!(m.bus.peek16(0x06000000 + (row * 240 + col) * 2), color);
        }
    }
    let last = (159 * 240 + 239) * 4;
    assert_eq!(&m.video_frame().pixels[last..last + 4], &[123, 0, 255, 255]);
    let saved = m.export_state();
    m.run(FRAME_CYCLES);
    let after = m.export_state();
    m.import_state(&saved).unwrap();
    m.run(FRAME_CYCLES);
    assert_eq!(m.export_state(), after);
}
#[test]
fn arm_and_thumb_angle_services_return_signed_and_wrapped_results() {
    use rustboy_emulator_api::Emulator;
    for thumb in [false, true] {
        for (service, x, y, expected) in [
            (9u8, 0i32, 0i32, 0u32),
            (9, 0x4000, 0, 0x2000),
            (9, -0x4000, 0, (-0x2000i32) as u32),
            (10, 0, 0, 0),
            (10, 0x4000, 0, 0),
            (10, 0, 0x4000, 0x4000),
            (10, -0x4000, 0, 0x8000),
            (10, 0, -0x4000, 0xc000),
            (10, -0x4000, 0x4000, 0x6000),
            (10, -0x4000, -0x4000, 0xa000),
            (10, 0x4000, -0x4000, 0xe000),
            (10, i32::MIN, i32::MIN, 0xa000),
        ] {
            let mut rom = vec![0; 192];
            let pool = if thumb {
                for (n, op) in [
                    0x4802u16,
                    0x4903,
                    0xdf00 | u16::from(service),
                    0x2477,
                    0xe7fe,
                ]
                .into_iter()
                .enumerate()
                {
                    rom[n * 2..n * 2 + 2].copy_from_slice(&op.to_le_bytes());
                }
                12
            } else {
                for (n, op) in [
                    0xe59f000cu32,
                    0xe59f100c,
                    0xef000000 | (u32::from(service) << 16),
                    0xe3a04077,
                    0xeafffffe,
                ]
                .into_iter()
                .enumerate()
                {
                    rom[n * 4..n * 4 + 4].copy_from_slice(&op.to_le_bytes());
                }
                20
            };
            rom[pool..pool + 4].copy_from_slice(&(x as u32).to_le_bytes());
            rom[pool + 4..pool + 8].copy_from_slice(&(y as u32).to_le_bytes());
            let mut m = GameBoyAdvance::new(rom, None).unwrap();
            if thumb {
                m.cpu.set_cpsr(m.cpu.cpsr() | 0x20).unwrap();
            }
            m.step();
            m.step();
            let saved = m.export_state();
            m.step();
            m.step();
            assert!(m.fault.is_none());
            assert_eq!(
                m.cpu.r[0], expected,
                "service {service:x}, x {x}, y {y}, thumb {thumb}"
            );
            assert_eq!(m.cpu.r[4], 0x77);
            assert_eq!(m.cpu.cpsr() & 0x3f, if thumb { 0x3f } else { 0x1f });
            assert_eq!(m.cpu.exceptions, 1);
            let after = m.export_state();
            m.import_state(&saved).unwrap();
            m.step();
            m.step();
            assert_eq!(m.export_state(), after);
        }
    }
}
#[test]
fn original_firmware_can_read_bios_but_arm_and_thumb_cartridges_get_opcode_latch() {
    use rustboy_emulator_api::Emulator;
    for thumb in [false, true] {
        let mut bios = vec![0; 0x4000];
        for (n, op) in [
            0xe59f0018u32,
            0xe59f5018,
            0xe5956000,
            0xe12fff10,
            0xe1a00000,
            0xe1a00000,
        ]
        .into_iter()
        .enumerate()
        {
            bios[n * 4..n * 4 + 4].copy_from_slice(&op.to_le_bytes());
        }
        bios[32..36].copy_from_slice(&(0x08000000u32 | u32::from(thumb)).to_le_bytes());
        bios[36..40].copy_from_slice(&0x100u32.to_le_bytes());
        bios[0x100..0x104].copy_from_slice(&0xdeadbeefu32.to_le_bytes());
        let mut rom = vec![0; 192];
        if thumb {
            for (n, op) in [0x4907u16, 0x680a, 0x784b, 0x884c, 0xe7fe]
                .into_iter()
                .enumerate()
            {
                rom[n * 2..n * 2 + 2].copy_from_slice(&op.to_le_bytes());
            }
        } else {
            for (n, op) in [
                0xe59f1018u32,
                0xe5912000,
                0xe5d13001,
                0xe1d140b2,
                0xeafffffe,
            ]
            .into_iter()
            .enumerate()
            {
                rom[n * 4..n * 4 + 4].copy_from_slice(&op.to_le_bytes());
            }
        }
        rom[32..36].copy_from_slice(&0x100u32.to_le_bytes());
        let mut m = GameBoyAdvance::new(rom, Some(bios)).unwrap();
        for _ in 0..4 {
            m.step();
        }
        assert_eq!(m.cpu.r[6], 0xdeadbeef); // BIOS-side data load is permitted.
        let saved = m.export_state();
        for _ in 0..5 {
            m.step();
        }
        assert_eq!(m.cpu.r[2], 0xe1a00000); // Last BIOS instruction's prefetch.
        assert_eq!(m.cpu.r[3], 0);
        assert_eq!(m.cpu.r[4], 0xe1a0);
        assert!(m.fault.is_none());
        let after = m.export_state();
        m.import_state(&saved).unwrap();
        for _ in 0..5 {
            m.step();
        }
        assert_eq!(m.export_state(), after);
    }
}
#[test]
fn arm_block_load_restarts_cartridge_waits_at_page_boundary() {
    let mut rom = vec![0; 0x20008];
    rom[..4].copy_from_slice(&0xe8b0001eu32.to_le_bytes()); // LDMIA r0!, {r1-r4}
    for (n, value) in [0x12345678u32, 0xabcdef01, 0x2468ace0, 0x13579bdf]
        .into_iter()
        .enumerate()
    {
        rom[0x1fff8 + n * 4..0x1fffc + n * 4].copy_from_slice(&value.to_le_bytes());
    }
    let mut m = GameBoyAdvance::new(rom, None).unwrap();
    m.cpu.r[0] = 0x0801fff8;
    // Fetch 8 + data (8 + 6 + 8 + 6) + load's internal clock.
    assert_eq!(m.step(), 37);
    assert_eq!(
        &m.cpu.r[1..5],
        &[0x12345678, 0xabcdef01, 0x2468ace0, 0x13579bdf]
    );
    assert_eq!(m.cpu.r[0], 0x08020008);
    assert_eq!(m.cpu.instructions, 1);
    assert!(m.fault.is_none());
}
#[test]
fn dma3_cartridge_to_oam_observes_page_restart_and_32bit_destination() {
    let mut rom = vec![0; 0x20004];
    rom[0x1fffc..0x20000].copy_from_slice(&0x12345678u32.to_le_bytes());
    rom[0x20000..0x20004].copy_from_slice(&0xabcdef01u32.to_le_bytes());
    let mut m = GameBoyAdvance::new(rom, None).unwrap();
    m.bus.write32(0x040000d4, 0x0801fffc, false);
    m.bus.write32(0x040000d8, 0x07000000, false);
    m.bus.write32(0x040000dc, 0x84000002, false);
    m.bus.take_cycles();
    assert_eq!(m.step(), 11); // startup 2 + ROM N+S 8 + OAM 1.
    assert_eq!(m.step(), 9); // next page ROM N+S 8 + OAM 1.
    assert_eq!(m.bus.peek32(0x07000000), 0x12345678);
    assert_eq!(m.bus.peek32(0x07000004), 0xabcdef01);
    assert_eq!(m.cpu.instructions, 0);
    assert!(m.fault.is_none());
}
#[test]
fn arm_and_thumb_stop_services_resume_at_the_next_instruction() {
    use rustboy_emulator_api::Emulator;
    for thumb in [false, true] {
        for service in [3, 0x27] {
            let mut rom = vec![0; 192];
            if thumb {
                for (n, op) in [0xdf00u16 | service, 0x2477, 0xe7fe]
                    .into_iter()
                    .enumerate()
                {
                    rom[n * 2..n * 2 + 2].copy_from_slice(&op.to_le_bytes());
                }
            } else {
                for (n, op) in [
                    0xef000000u32 | (u32::from(service) << 16),
                    0xe3a04077,
                    0xeafffffe,
                ]
                .into_iter()
                .enumerate()
                {
                    rom[n * 4..n * 4 + 4].copy_from_slice(&op.to_le_bytes());
                }
            }
            let mut m = GameBoyAdvance::new(rom, None).unwrap();
            m.cpu.r[2] = 0x80;
            if thumb {
                m.cpu.set_cpsr(m.cpu.cpsr() | 0x20).unwrap();
            }
            m.run(FRAME_CYCLES);
            assert!(m.fault.is_none());
            assert!(m.bus.stopped);
            assert_eq!(m.cpu.r[4], 0);
            assert_eq!(m.cpu.instructions, 1);
            assert_eq!(m.cpu.exceptions, 1);
            let before = m.export_state();
            assert_eq!(m.run(FRAME_CYCLES * 100), 0);
            assert_eq!(m.export_state(), before);
            m.bus.write16(0x04000200, 0x1000, false);
            m.bus.write16(0x04000132, 0x4001, false);
            m.bus.set_keys(1);
            m.bus.take_cycles();
            m.run(100);
            assert!(!m.bus.stopped);
            assert_eq!(m.cpu.r[4], 0x77);
            assert_eq!(m.cpu.cpsr() & 0x3f, if thumb { 0x3f } else { 0x1f });
        }
    }
}

#[test]
fn original_cartridges_dispatch_data_services_in_arm_and_thumb() {
    let cases: &[(u8, &[u8], &[u8])] = &[
        (0x10, &[0xa5], &[0x03, 0x03, 0x30, 0x30]),
        (
            0x13,
            &[
                0x28, 4, 0, 0, 3, 0x80, b'f', 0xc0, b'H', b'u', 0, 0, 0, 0, 0, 0xb0,
            ],
            b"Huff",
        ),
        (0x16, &[0x81, 4, 0, 0, 250, 10, 255, 2], &[250, 4, 3, 5]),
        (0x17, &[0x81, 4, 0, 0, 250, 10, 255, 2], &[250, 4, 3, 5]),
        (
            0x18,
            &[0x82, 8, 0, 0, 255, 255, 2, 0, 255, 255, 0, 128],
            &[255, 255, 1, 0, 0, 0, 0, 128],
        ),
    ];
    for thumb in [false, true] {
        for &(service, data, expected) in cases {
            let destination = if service == 0x16 {
                0x03000000u32
            } else {
                0x06000000
            };
            let mut rom = vec![0; 256];
            if thumb {
                for (n, op) in [
                    0x4802u16,
                    0x4903,
                    0x4a03,
                    0xdf00 | u16::from(service),
                    0x2477,
                    0xe7fe,
                ]
                .into_iter()
                .enumerate()
                {
                    rom[n * 2..n * 2 + 2].copy_from_slice(&op.to_le_bytes());
                }
            } else {
                for (n, op) in [
                    0xe59f0010u32,
                    0xe59f1010,
                    0xe59f2010,
                    0xef000000 | (u32::from(service) << 16),
                    0xe3a04077,
                    0xeafffffe,
                ]
                .into_iter()
                .enumerate()
                {
                    rom[n * 4..n * 4 + 4].copy_from_slice(&op.to_le_bytes());
                }
            }
            let pool = if thumb { 12 } else { 24 };
            for (n, value) in [0x08000040u32, destination, 0x08000060]
                .into_iter()
                .enumerate()
            {
                rom[pool + n * 4..pool + n * 4 + 4].copy_from_slice(&value.to_le_bytes());
            }
            rom[64..64 + data.len()].copy_from_slice(data);
            rom[96..104].copy_from_slice(&[1, 0, 1, 4, 2, 0, 0, 0]);
            let mut m = GameBoyAdvance::new(rom, None).unwrap();
            if thumb {
                m.cpu.set_cpsr(m.cpu.cpsr() | 0x20).unwrap();
            }
            m.run(2000);
            assert!(m.fault.is_none(), "service {service:x}, thumb {thumb}");
            assert_eq!(m.cpu.r[4], 0x77, "must return to cartridge after SWI");
            assert_eq!(m.cpu.cpsr() & 0x3f, if thumb { 0x3f } else { 0x1f });
            for (n, byte) in expected.iter().enumerate() {
                assert_eq!(m.bus.peek8(destination + n as u32), *byte);
            }
            assert_eq!(m.cpu.exceptions, 1);
        }
    }
}

#[test]
fn direct_boot_all_display_modes_keep_distinct_horizontal_and_vertical_pixels() {
    for mode in [3, 4, 5, 1, 2, 0] {
        let mut rom = vec![0; 192];
        rom[..4].copy_from_slice(&0xeafffffeu32.to_le_bytes());
        let mut m = GameBoyAdvance::new(rom, None).unwrap();
        for (index, color) in [(0, 0x7fff), (1, 31), (2, 31 << 5), (3, 31 << 10)] {
            m.bus.write16(0x05000000 + index * 2, color, false);
        }
        let layer = if mode == 0 {
            0x100
        } else if mode == 2 {
            0x800
        } else {
            0x400
        };
        if mode <= 2 {
            let control = if mode == 0 {
                0x04000008
            } else if mode == 1 {
                0x0400000c
            } else {
                0x0400000e
            };
            m.bus
                .write16(control, if mode == 0 { 0x1c80 } else { 0x1c00 }, false);
            m.bus.vram[0] = 1;
            m.bus.vram[1] = 2;
            m.bus.vram[8] = 3;
        } else if mode == 4 {
            m.bus.vram[0] = 1;
            m.bus.vram[1] = 2;
            m.bus.vram[240] = 3;
        } else {
            m.bus.write16(0x06000000, 31, false);
            m.bus.write16(0x06000002, 31 << 5, false);
            m.bus.write16(
                0x06000000 + if mode == 5 { 320 } else { 480 },
                31 << 10,
                false,
            );
        }
        // No matrix writes or BIOS reset service: direct entry must supply the
        // post-boot identity transform, not collapse all pixels to coordinate 0.
        m.bus.write16(0x04000000, layer | mode, false);
        m.bus.take_cycles();
        m.run(FRAME_CYCLES * 2);
        let frame = m.video_frame();
        assert_eq!(&frame.pixels[..4], &[255, 0, 0, 255], "mode {mode}: origin");
        assert_eq!(
            &frame.pixels[4..8],
            &[0, 255, 0, 255],
            "mode {mode}: horizontal coordinate"
        );
        assert_eq!(
            &frame.pixels[960..964],
            &[0, 0, 255, 255],
            "mode {mode}: vertical coordinate"
        );
    }
}

#[test]
fn original_arm_cartridge_starts_all_psg_channels_and_replays_pcm() {
    use rustboy_emulator_api::Emulator;
    // ARM literal loads and halfword stores perform every register write on the
    // CPU bus. No host-side sound initialization or copyrighted score/sample.
    let settings = [
        (0x84u32, 0x80u32),
        (0x80, 0xff77),
        (0x82, 2),
        (0x60, 0x19),
        (0x62, 0xa280),
        (0x64, 0x8680),
        (0x68, 0x7280),
        (0x6c, 0x8700),
        (0x70, 0),
        (0x90, 0xf0f0),
        (0x92, 0xf0f0),
        (0x94, 0xf0f0),
        (0x96, 0xf0f0),
        (0x98, 0xf0f0),
        (0x9a, 0xf0f0),
        (0x9c, 0xf0f0),
        (0x9e, 0xf0f0),
        (0x70, 0xc0),
        (0x72, 0x2000),
        (0x74, 0x8700),
        (0x78, 0x5900),
        (0x7c, 0x8039),
    ];
    let mut words = vec![0];
    for (offset, _) in settings {
        words.push(0); // LDR r1,[pc,#literal]
        words.push(0xe1c010b0 | ((offset & 0xf0) << 4) | (offset & 15));
    }
    words.push(0xeafffffe);
    let pool = words.len() * 4;
    words[0] = 0xe59f0000 | (pool as u32 - 8);
    words.push(0x04000000);
    for (n, (_, value)) in settings.into_iter().enumerate() {
        let instruction = 1 + n * 2;
        let literal = words.len() * 4;
        words[instruction] = 0xe59f1000 | (literal as u32 - (instruction as u32 * 4 + 8));
        words.push(value);
    }
    let mut rom = vec![0; 1024];
    for (n, word) in words.into_iter().enumerate() {
        rom[n * 4..n * 4 + 4].copy_from_slice(&word.to_le_bytes());
    }
    let mut m = GameBoyAdvance::new(rom, None).unwrap();
    m.run(15000);
    assert!(m.fault.is_none());
    assert_eq!(m.bus.peek8(0x04000084), 0x8f);
    let sound = m.drain_audio();
    assert_eq!((sound.sample_rate, sound.channels), (32768, 2));
    assert!(sound.samples.iter().any(|v| v.abs() > 0.05));
    assert!(sound.samples.windows(2).any(|pair| pair[0] != pair[1]));
    let saved = m.export_state();
    m.run(FRAME_CYCLES * 3);
    let expected = m.drain_audio();
    let final_state = m.export_state();
    m.import_state(&saved).unwrap();
    m.run(FRAME_CYCLES * 3);
    assert_eq!(m.drain_audio().samples, expected.samples);
    assert_eq!(m.export_state(), final_state);
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

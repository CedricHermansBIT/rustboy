use super::*;
use crate::cpu::Memory;
use crate::FRAME_CYCLES;
use rustboy_emulator_api::{Button, Emulator};

fn program(words: &[u32], tag: &[u8]) -> GameBoyAdvance {
    let mut rom = vec![0; 512];
    for (i, word) in words.iter().enumerate() {
        rom[i * 4..i * 4 + 4].copy_from_slice(&word.to_le_bytes());
    }
    rom[256..256 + tag.len()].copy_from_slice(tag);
    GameBoyAdvance::new(rom, None).unwrap()
}

fn reference_run(machine: &mut GameBoyAdvance, budget: u64) -> u64 {
    let start = machine.clocks;
    while machine.clocks.wrapping_sub(start) < budget && machine.fault.is_none() {
        if machine.step() == 0 {
            break;
        }
    }
    machine.clocks.wrapping_sub(start)
}

#[test]
fn slices_exit_on_arm_thumb_interworking_before_the_next_opcode() {
    let mut m = program(&[0xe12fff11, 0xe2844001, 0xeafffffc], b"");
    m.bus.rom[16..20].copy_from_slice(&[0x01, 0x34, 0x10, 0x47]); // ADD r4,1; BX r2
    let mut m = GameBoyAdvance::new(m.bus.rom, None).unwrap();
    m.cpu.r[1] = 0x08000011;
    m.cpu.r[2] = 0x08000004;
    replay_budgets(&mut m, &[1, 7, 19, 101, 1007, FRAME_CYCLES]);
    assert!(m.cpu.r[4] > 100);
}

#[test]
fn deferred_instruction_counts_match_reference_across_wrap_and_idle_proofs() {
    for thumb in [false, true] {
        let mut m = program(&[0xe1a00000, 0xe1a00000, 0xeafffffc], b"");
        if thumb {
            m.bus.rom[..6].copy_from_slice(&[0xc0, 0x46, 0xc0, 0x46, 0xfc, 0xe7]);
            m = GameBoyAdvance::new(m.bus.rom, None).unwrap();
            m.cpu.set_cpsr(0x3f).unwrap();
        }
        m.cpu.instructions = u64::MAX - 2;
        replay_budgets(&mut m, &[1, 17, 1006, 1232, FRAME_CYCLES]);
        assert!(m.cpu.instructions < u64::MAX / 2);
    }
}

#[cfg(feature = "idle-loops")]
#[test]
fn idle_loops_replay_complete_state_across_events_modes_memory_and_prefetch() {
    for thumb in [false, true] {
        for address in [0x08000000, 0x03000000, 0x02000000] {
            for wait in [0, 0x4000, 0x4317] {
                let mut m = program(&[0xe5910000, 0xe3500000, 0x0afffffc], b"");
                let code: Vec<u8> = if thumb {
                    [0x6808u16, 0x2800, 0xd0fc]
                        .into_iter()
                        .flat_map(u16::to_le_bytes)
                        .collect()
                } else {
                    [0xe5910000u32, 0xe3500000, 0x0afffffc]
                        .into_iter()
                        .flat_map(u32::to_le_bytes)
                        .collect()
                };
                if address >> 24 == 8 {
                    m.bus.rom[..code.len()].copy_from_slice(&code);
                    m = GameBoyAdvance::new(m.bus.rom, None).unwrap();
                } else {
                    for (n, byte) in code.into_iter().enumerate() {
                        m.bus.write8(address + n as u32, byte);
                    }
                }
                m.cpu.set_cpsr(if thumb { 0x3f } else { 0x1f }).unwrap();
                m.cpu.r[15] = address;
                m.cpu.r[1] = 0x03000100;
                m.bus.write16(0x04000204, wait, false);
                // Audio, video and timer events must all retain exact state.
                m.bus.write16(0x04000100, 0xff01, false);
                m.bus.write16(0x04000102, 0x80, false);
                replay_budgets(&mut m, &[1, 7, 19, 127, 777, 5000, FRAME_CYCLES * 2 + 1]);
                assert!(
                    m.accelerated_instructions() > 0,
                    "{address:08x} thumb={thumb} wait={wait:04x}"
                );
            }
        }
    }
}

#[cfg(feature = "idle-loops")]
#[test]
fn idle_proofs_cannot_hide_writes_peripheral_reads_dma_or_host_ram_changes() {
    for (opcode, address) in [(0xe5810000, 0x03000100), (0xe5910000, 0x04000100)] {
        let mut m = program(&[opcode, 0xeafffffd], b"");
        m.cpu.r[1] = address;
        m.bus.write16(0x04000102, 0x80, false);
        replay_budgets(&mut m, &[1000, 3000]);
        assert_eq!(m.accelerated_instructions(), 0);
    }
    let mut m = program(&[0xe5910000, 0xe3500000, 0x0afffffc, 0xeafffffe], b"");
    m.cpu.r[1] = 0x03000100;
    m.bus.write32(0x02000000, 1, false);
    m.bus.write32(0x040000d4, 0x02000000, false);
    m.bus.write32(0x040000d8, 0x03000100, false);
    m.bus.write32(0x040000dc, 0xa4000001, false);
    replay_budgets(&mut m, &[500, 2000]);
    assert!(m.accelerated_instructions() > 0);
    assert_eq!(m.cpu.r[0], 1);
    assert_eq!(m.cpu.r[15], 0x0800000c);

    let mut m = program(&[0xe5910000, 0xe3500000, 0x0afffffc, 0xeafffffe], b"");
    m.cpu.r[1] = 0x03000100;
    m.run(4000);
    assert!(m.accelerated_instructions() > 0);
    // The public RAM API can bypass bus write hooks between host calls.
    m.bus.iwram[0x100] = 1;
    replay_budgets(&mut m, &[1000]);
    assert_eq!(m.cpu.r[0], 1);
}

fn assert_state(a: &GameBoyAdvance, b: &GameBoyAdvance, context: &str) {
    let (a, b) = (a.export_state(), b.export_state());
    assert_eq!(a.len(), b.len(), "{context}: snapshot size");
    if let Some(offset) = a.iter().zip(&b).position(|(a, b)| a != b) {
        let end = (offset + 16).min(a.len());
        panic!(
            "{context}: first mismatch offset {offset}: {:?} != {:?}",
            &a[offset..end],
            &b[offset..end]
        );
    }
}

fn replay(mut sliced: GameBoyAdvance) {
    replay_budgets(
        &mut sliced,
        &[1, 3, 31, 511, 2049, FRAME_CYCLES / 2, FRAME_CYCLES * 2 + 1],
    );
}

fn replay_budgets(sliced: &mut GameBoyAdvance, budgets: &[u64]) {
    sliced.bus.take_cycles();
    let bios = (!sliced.bus.replacement_bios).then(|| sliced.bus.bios.clone());
    let mut reference = GameBoyAdvance::new(sliced.bus.rom.clone(), bios).unwrap();
    let baseline = sliced.export_state();
    // Restore both sides, then normalize the frontend persistence hint. A
    // 512-byte EEPROM restore into a fresh 8192-byte default marks it dirty,
    // while restoring identical data into itself does not.
    sliced.import_state(&baseline).unwrap();
    reference.import_state(&baseline).unwrap();
    sliced.mark_save_clean();
    reference.mark_save_clean();
    assert_state(sliced, &reference, "initial state");
    for &budget in budgets {
        assert_eq!(sliced.run(budget), reference_run(&mut reference, budget));
        assert_state(sliced, &reference, &format!("budget={budget}"));
        assert_eq!(sliced.bus.timer_next, reference.bus.timer_next);
        assert_eq!(sliced.bus.dma_active, reference.bus.dma_active);
        assert_eq!(sliced.bus.dma_ready, reference.bus.dma_ready);
        assert_eq!(sliced.bus.execution.pending, 0);
        assert_eq!(sliced.bus.execution.credited, 0);
        assert_eq!(sliced.bus.clocks, 0);
    }
}

#[test]
fn byte_halfword_and_word_register_barriers_flush_time_before_side_effects() {
    for store in [0xe5c10000, 0xe1c100b0, 0xe5810000] {
        for offset in [
            0x62, 0x64, 0x70, 0x74, 0x82, 0x84, 0x88, 0x100, 0x102, 0x104, 0x106, 0x204, 0x208,
        ] {
            let mut m = program(
                &[
                    0xe2888001, 0xe2888001, 0xe2888001, store, 0xe1d320b0, 0xeafffff9,
                ],
                b"",
            );
            m.cpu.r[0] = 0x008080c0;
            m.cpu.r[1] = 0x04000000 + offset;
            m.cpu.r[3] = 0x04000100;
            m.bus.write32(0x04000100, 0x0080ffa1, false);
            m.bus.write32(0x04000104, 0x0084fff0, false);
            m.bus.write16(0x04000084, 0x80, false);
            m.bus.write16(0x04000080, 0x1177, false);
            m.bus.write16(0x04000082, 0x030e, false);
            m.bus.write16(0x04000062, 0x8080, false);
            m.bus.write16(0x04000064, 0x87a1, false);
            replay_budgets(&mut m, &[1, 17, 513, 1007, 8192]);
        }
    }
}

#[test]
fn slices_replay_reference_cpu_video_audio_mmio_dma_and_eeprom_boundaries() {
    // Original ARM loop: ADD, RAM store/load, multiply, timer/EEPROM read,
    // register write, FIFO write, sound-status read, branch to the start.
    let words = [
        0xe2888001, 0xe5898000, 0xe599a000, 0xe00b0a98, 0xe1d3a0b0, 0xe1c5a0b0, 0xe5878000,
        0xe1d6a0b0, 0xeafffff6,
    ];
    for scenario in 0..5 {
        let mut m = program(&words, if scenario == 4 { b"EEPROM_V123" } else { b"" });
        m.cpu.r[9] = 0x03000100;
        m.cpu.r[3] = if scenario == 4 {
            0x0d000000
        } else {
            0x04000100
        };
        m.cpu.r[5] = 0x04000104;
        m.cpu.r[7] = 0x040000a0;
        m.cpu.r[6] = 0x04000084;
        m.bus.write16(0x04000000, 0x0403, false);
        m.bus.write16(0x04000100, 0xffc1, false);
        m.bus.write16(0x04000102, 0x80, false);
        m.bus.write16(0x04000106, 0x80, false);
        if scenario >= 1 {
            m.bus.write16(0x04000084, 0x80, false);
            m.bus.write16(0x04000080, 0x1177, false);
            m.bus.write16(0x04000082, 0x030e, false);
            m.bus.write16(0x04000062, 0x8080, false);
            m.bus.write16(0x04000064, 0x87a1, false);
        }
        if scenario == 2 {
            m.bus.write32(0x040000bc, 0x03000100, false);
            m.bus.write32(0x040000c0, 0x040000a0, false);
            m.bus.write32(0x040000c4, 0xb6400004, false);
            m.bus.write32(0x040000c8, 0x03000100, false);
            m.bus.write32(0x040000cc, 0x05000000, false);
            m.bus.write32(0x040000d0, 0xa2400004, false);
        }
        if scenario == 3 {
            m.bus.write16(0x04000102, 0xc0, false);
            m.bus.write16(0x04000200, 8, false);
            m.bus.write16(0x04000208, 1, false);
            // An enabled, CPU-masked IRQ must keep using the reference path.
            m.cpu.set_cpsr(m.cpu.cpsr() | 0x80).unwrap();
        }
        if scenario == 4 {
            m.bus.backup.eeprom.address_bits = 6;
            m.bus.backup.data.resize(512, 255);
            m.bus.backup.eeprom.busy = 321;
            m.bus.backup.eeprom.pending_data = 0x0123456789abcdef;
        }
        replay(m);
    }
    // A long, ordinary CPU slice with no observation barriers at all.
    replay(program(
        &[0xe2888001, 0xe5898000, 0xe599a000, 0xeafffffb],
        b"",
    ));
}

#[test]
fn slice_exception_helpers_preserve_preceding_time_and_faults() {
    for service in [0, 1, 6, 8, 0x19, 0xff] {
        let mut m = program(
            &[
                0xe2888001,
                0xe2888001,
                0xef000000 | service << 16,
                0xeafffffb,
            ],
            b"",
        );
        m.cpu.r[0] = 5;
        m.cpu.r[1] = 2;
        m.bus.write16(0x04000100, 0xfff0, false);
        m.bus.write16(0x04000102, 0x80, false);
        replay(m);
    }
}

#[test]
fn slice_stop_and_halt_handoff_and_key_wake_match_reference() {
    for service in [2, 3] {
        let mut m = program(
            &[
                0xe2888001,
                0xe2888001,
                0xef000000 | service << 16,
                0xe2888001,
                0xeafffffe,
            ],
            b"",
        );
        m.bus.write16(0x04000200, 0x1008, false);
        m.bus.write16(0x04000132, 0x4001, false);
        m.bus.write16(0x04000100, 0xffe0, false);
        m.bus.write16(0x04000102, 0xc0, false);
        m.bus.take_cycles();
        let mut reference = GameBoyAdvance::new(m.bus.rom.clone(), None).unwrap();
        reference.import_state(&m.export_state()).unwrap();
        assert_eq!(m.run(2048), reference_run(&mut reference, 2048));
        assert_state(&m, &reference, "sleep");
        m.set_button(0, Button::A, true).unwrap();
        reference.set_button(0, Button::A, true).unwrap();
        assert_eq!(m.run(2048), reference_run(&mut reference, 2048));
        assert_state(&m, &reference, "key wake");
    }
}

#[test]
fn thumb_slices_preserve_io_and_iwram_self_modification_and_open_bus_history() {
    let mut m = program(&[], b"");
    let instructions = [
        0x3001u16, 0x6008, 0x680a, 0x4350, 0x881c, 0x802c, 0x6030, 0x883c, 0xe7f6,
    ];
    for (i, op) in instructions.iter().enumerate() {
        m.bus.rom[i * 2..i * 2 + 2].copy_from_slice(&op.to_le_bytes());
    }
    // Reconstruct immutable cartridge identity after preparing the fixture.
    let mut m = GameBoyAdvance::new(m.bus.rom, None).unwrap();
    m.cpu.set_cpsr(m.cpu.cpsr() | 0x20).unwrap();
    for (reg, addr) in [
        (1, 0x03000100),
        (3, 0x04000100),
        (5, 0x04000104),
        (6, 0x040000a0),
        (7, 0x04000084),
    ] {
        m.cpu.r[reg] = addr;
    }
    m.bus.write32(0x04000100, 0x0080ffb1, false);
    m.bus.write32(0x04000104, 0x0084fff0, false);
    m.bus.write16(0x04000084, 0x80, false);
    m.bus.write16(0x04000082, 0x0304, false);
    replay(m);

    let mut m = program(&[0xeafffffe], b"");
    for (i, op) in [0x8008u16, 0x3201, 0x880b, 0x2200, 0x683d, 0xe7f9]
        .iter()
        .enumerate()
    {
        m.bus.iwram[i * 2..i * 2 + 2].copy_from_slice(&op.to_le_bytes());
    }
    m.cpu.set_cpsr(m.cpu.cpsr() | 0x20).unwrap();
    m.cpu.r[15] = 0x03000000;
    m.cpu.r[0] = 0x3201; // Replace the future MOV r2,#0 with ADD r2,#1.
    m.cpu.r[1] = 0x03000006;
    m.cpu.r[7] = 0x10000000; // Open-bus data read after self-modifying writes.
    replay(m);
}

#[test]
fn slice_ram_remapping_and_memory_lock_match_instruction_boundaries() {
    for control in [0x0d000000, 0x0f000020, 0x0d000021] {
        let mut m = program(
            &[
                0xe2888001,
                0xe2888001,
                0xe5810000,
                0xe5932000,
                0xeafffffau32,
            ],
            b"",
        );
        m.cpu.r[0] = control;
        m.cpu.r[1] = 0x04000800;
        m.cpu.r[3] = 0x02000000;
        replay(m);
    }
}

#[test]
fn slice_irq_arrival_unmask_and_bios_wait_replay_reference_dispatch() {
    for waiting in [false, true] {
        let mut m = program(
            if waiting {
                &[0xe2888001, 0xe2888001, 0xef040000, 0xe2888001, 0xeafffffa]
            } else {
                &[0xe2888001, 0xe2888001, 0xe121f000, 0xe2888001, 0xeafffffa]
            },
            b"",
        );
        // Original timer IRQ handler: set BIOS flags, acknowledge timer 0,
        // return through the replacement IRQ shim's saved register context.
        for (i, word) in [
            0xe59f0010u32,
            0xe3a01008,
            0xe1c010b0,
            0xe59f0008,
            0xe1c010b0,
            0xe12fff1e,
            0x03007ff8,
            0x04000202,
        ]
        .iter()
        .enumerate()
        {
            m.bus.write32(0x02000000 + i as u32 * 4, *word, false);
        }
        m.bus.write32(0x03007ffc, 0x02000000, false);
        m.bus.write16(0x04000200, 8, false);
        m.bus.write16(0x04000208, 1, false);
        m.bus.write32(0x04000100, 0x00c0ff00, false);
        if waiting {
            m.cpu.r[0] = 0;
            m.cpu.r[1] = 8;
        } else {
            m.cpu.set_cpsr(m.cpu.cpsr() | 0x80).unwrap();
            m.cpu.r[0] = 0x1f;
            m.bus.request_irq(8);
            assert!(
                m.can_run_cpu_slice(),
                "CPSR.I permits batching while IRQ is pending"
            );
        }
        replay(m);
    }
}

#[test]
fn masked_pending_irq_slice_ends_at_the_unmasking_instruction() {
    let mut m = program(&[0xe2888001, 0xe2888001, 0xe121f000, 0xe2888001], b"");
    m.cpu.set_cpsr(0x9f).unwrap();
    m.cpu.r[0] = 0x1f;
    m.bus.write16(0x04000200, 8, false);
    m.bus.write16(0x04000208, 1, false);
    m.bus.request_irq(8);
    m.bus.take_cycles();
    assert!(m.can_run_cpu_slice());
    m.run_cpu_slice(500);
    assert_eq!(
        m.cpu.r[8], 2,
        "no instruction may execute after MSR unmasks the IRQ"
    );
    assert_eq!(m.cpu.r[15], 0x0800000c);
    assert!(!m.can_run_cpu_slice());
    let before = m.cpu.instructions;
    m.step();
    assert_eq!(
        m.cpu.instructions, before,
        "the next boundary dispatches the IRQ first"
    );
    assert_eq!(m.cpu.exceptions, 1);
}

#[test]
fn slice_user_firmware_exception_return_keeps_instruction_timing() {
    let m = program(&[0xe2888001, 0xe2888001, 0xef060000, 0xeafffffb], b"");
    let mut firmware = vec![0; 0x4000];
    // Original minimal SWI handler: MOVS PC,LR restores SPSR and returns.
    firmware[8..12].copy_from_slice(&0xe1b0f00eu32.to_le_bytes());
    let mut m = GameBoyAdvance::new(m.bus.rom, Some(firmware)).unwrap();
    m.cpu.set_cpsr(0x1f).unwrap();
    m.cpu.r[15] = 0x08000000;
    m.bus.write32(0x04000100, 0x0080ff00, false);
    replay(m);
}

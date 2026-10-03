//! Original serial fixtures, exercising real DMA3, bus, scheduler and frontend.
use rustboy_emulator_api::Emulator;
use rustboy_gba::{cpu::Memory, GameBoyAdvance};
fn machine() -> GameBoyAdvance {
    let mut rom = vec![0; 192];
    rom[..4].copy_from_slice(&0xeafffffeu32.to_le_bytes());
    rom[128..139].copy_from_slice(b"EEPROM_V124");
    GameBoyAdvance::new(rom, None).unwrap()
}
fn command(bits: u8, read: bool, address: u16, data: u64) -> Vec<u16> {
    let mut output = vec![1, u16::from(read)];
    for n in (0..bits).rev() {
        output.push(0xa500 | (address >> n & 1));
    }
    if !read {
        for n in (0..64).rev() {
            output.push(0xaa00 | ((data >> n) & 1) as u16);
        }
    }
    output.push(0);
    output
}
fn dma(m: &mut GameBoyAdvance, source: u32, dest: u32, count: u16) {
    m.bus.write32(0x040000d4, source, false);
    m.bus.write32(0x040000d8, dest, false);
    m.bus
        .write32(0x040000dc, 0x80000000 | u32::from(count), false);
    m.bus.take_cycles();
}
fn send(m: &mut GameBoyAdvance, bits: &[u16]) {
    for (i, bit) in bits.iter().enumerate() {
        m.bus.write16(0x02000000 + i as u32 * 2, *bit, false);
    }
    dma(m, 0x02000000, 0x0dffff00, bits.len() as u16);
}
fn finish(m: &mut GameBoyAdvance) {
    for _ in 0..100 {
        if m.bus.io16(0xde) & 0x8000 == 0 {
            return;
        }
        m.step();
    }
    panic!("DMA did not finish");
}
fn read(m: &mut GameBoyAdvance, address: u16, bits: u8) -> u64 {
    send(m, &command(bits, true, address, 0));
    finish(m);
    for _ in 0..10 {
        assert_eq!(m.bus.peek16(0x0dffff00), 0);
    }
    dma(m, 0x0dffff00, 0x02010000, 68);
    finish(m);
    let mut data = 0;
    for n in 4..68 {
        data = (data << 1) | u64::from(m.bus.peek16(0x02010000 + n * 2));
    }
    data
}
#[test]
fn both_sizes_dma_write_busy_read_and_frontend_persistence() {
    for bits in [6, 14] {
        let mut m = machine();
        let address = if bits == 6 { 63 } else { 1023 };
        let data = 0x8123456789abcdef;
        send(&mut m, &command(bits, false, address, data));
        finish(&mut m);
        assert_eq!(m.cpu.instructions, 0);
        assert_eq!(m.bus.read16(0x0dffff00, false), 0);
        m.bus.take_cycles();
        m.run(108368);
        assert_eq!(m.bus.peek16(0x0dffff00), 1);
        assert_eq!(read(&mut m, address, bits), data);
        let save = m.export_save();
        assert_eq!(save.len(), if bits == 6 { 512 } else { 8192 });
        assert_eq!(&save[save.len() - 8..], &data.to_be_bytes());
        assert!(m.save_info().unwrap().dirty);
        m.reset();
        assert_eq!(m.export_save(), save);
        let mut reloaded = machine();
        reloaded.import_save(&save).unwrap();
        assert_eq!(read(&mut reloaded, address, bits), data);
        let previous = reloaded.export_save();
        assert!(reloaded.import_save(&[0; 513]).is_err());
        assert_eq!(reloaded.export_save(), previous);
    }
}
#[test]
fn partial_command_busy_program_and_output_replay_in_snapshots() {
    let mut m = machine();
    let value = 0xdeadbeef10203040;
    send(&mut m, &command(14, false, 17, value));
    for _ in 0..13 {
        m.step();
    }
    let partial = m.export_state();
    finish(&mut m);
    let busy = m.export_state();
    m.run(108368);
    assert_eq!(read(&mut m, 17, 14), value);
    m.import_state(&partial).unwrap();
    finish(&mut m);
    m.run(108368);
    assert_eq!(read(&mut m, 17, 14), value);
    m.import_state(&busy).unwrap();
    assert_eq!(m.bus.peek16(0x0dffff00), 0);
    m.run(108368);
    send(&mut m, &command(14, true, 17, 0));
    finish(&mut m);
    dma(&mut m, 0x0dffff00, 0x02010000, 68);
    for _ in 0..13 {
        m.step();
    }
    let output = m.export_state();
    finish(&mut m);
    let expected = m.bus.peek32(0x02010000 + 128);
    m.import_state(&output).unwrap();
    finish(&mut m);
    assert_eq!(m.bus.peek32(0x02010000 + 128), expected);
    let position = output.len() - 38;
    let mut invalid = output.clone();
    invalid[position] = 7;
    let length = invalid.len();
    let hash = invalid[..length - 8]
        .iter()
        .fold(0xcbf29ce484222325u64, |h, b| {
            (h ^ u64::from(*b)).wrapping_mul(0x100000001b3)
        });
    invalid[length - 8..].copy_from_slice(&hash.to_le_bytes());
    let before = m.export_state();
    assert!(m.import_state(&invalid).is_err());
    assert_eq!(m.export_state(), before);
}
#[test]
fn large_rom_only_maps_last_256_bytes_and_does_not_add_sram() {
    let mut rom = vec![0x5a; 32 * 1024 * 1024];
    rom[128..139].copy_from_slice(b"EEPROM_V124");
    let mut m = GameBoyAdvance::new(rom, None).unwrap();
    assert_eq!(m.bus.read16(0x0d000000, false), 0x5a5a);
    assert_eq!(m.bus.read16(0x0dfffefe, false), 0x5a5a);
    assert_eq!(m.bus.read16(0x0dffff00, false), 1);
    m.bus.write8(0x0e000000, 0);
    assert_eq!(m.bus.read8(0x0e000000), 255);
}

use super::*;

fn cartridge() -> Mbc {
    let mut rom = vec![0; 512 * 1024];
    for (bank, bytes) in rom.chunks_exact_mut(0x2000).enumerate() {
        bytes.fill(bank as u8);
    }
    rom[0x147] = 1;
    let mut mbc = Mbc::new(rom, 0);
    mbc.kind = MbcKind::NtNew {
        split: false,
        low: 2,
        high: 3,
        ram_enable: false,
    };
    mbc
}

#[test]
fn nt_new_activation_and_independent_half_banks_match_observed_registers() {
    let mut mbc = cartridge();
    assert_eq!((mbc.read(0x4000), mbc.read(0x6000)), (2, 3));
    mbc.write(0x1400, 0x54); // Wrong data cannot enable split mode.
    mbc.write(0x2000, 4);
    assert_eq!((mbc.read(0x4000), mbc.read(0x6000)), (8, 9));
    mbc.write(0x14FF, 0x55);
    mbc.write(0x2000, 0x3E);
    mbc.write(0x24FF, 0x1C);
    assert_eq!((mbc.read(0x4000), mbc.read(0x5FFF)), (62, 62));
    assert_eq!((mbc.read(0x6000), mbc.read(0x7FFF)), (28, 28));
    assert_eq!(mbc.read(0x1000), 0); // Fixed bank is unaffected.
    mbc.write(0x1400, 0x55); // Repeated activation preserves selected halves.
    assert_eq!((mbc.read(0x4000), mbc.read(0x6000)), (62, 28));
    for (value, expected) in [(0, 2), (1, 3), (64, 2), (65, 3), (255, 63)] {
        mbc.write(0x2000, value);
        assert_eq!(mbc.read(0x4000), expected);
    }
    mbc.write(0x2100, 5); // Outside the split-register page: normal 16 KiB banking.
    assert_eq!((mbc.read(0x4000), mbc.read(0x6000)), (10, 11));
}

#[test]
fn nt_new_snapshot_retains_split_banks_and_reset_has_no_filename_heuristic() {
    let mut mbc = cartridge();
    mbc.write(0x1400, 0x55);
    mbc.write(0x2000, 62);
    mbc.write(0x2400, 28);
    let mut state = Vec::new();
    mbc.export_state(&mut state);
    let mut restored = cartridge();
    restored.import_state(&mut state.as_slice()).unwrap();
    assert_eq!((restored.read(0x4000), restored.read(0x6000)), (62, 28));
    let mut again = Vec::new();
    restored.export_state(&mut again);
    assert_eq!(again, state);
    let mut corrupt = state;
    corrupt[3] = 4;
    assert!(restored.import_state(&mut corrupt.as_slice()).is_err());
    let mut rom = mbc.rom.clone();
    rom[0x134..0x142].copy_from_slice(b"POKEMONDIAMOND");
    assert!(!Mbc::detect_nt_new(&rom));
    assert!(matches!(Mbc::new(rom, 0).kind, MbcKind::Mbc1 { .. }));
}

//! Checks at the Game Boy loader boundary; the CPU can still load synthetic
//! ROMs directly for instruction tests.
pub fn validate_rom(data: &[u8]) -> Result<(), String> {
    if data.len() < 0x8000 { return Err("ROM is truncated: at least 32 KiB is required".into()); }
    if data.len() > 8 * 1024 * 1024 { return Err("ROM exceeds the supported 8 MiB cartridge size".into()); }
    if data.len() % 0x4000 != 0 { return Err("ROM must contain complete 16 KiB banks".into()); }
    let kind = data[0x147];
    if !matches!(kind, 0x00..=0x03 | 0x05..=0x06 | 0x08..=0x09 | 0x0F..=0x13 | 0x19..=0x1E) {
        return Err(format!("Unsupported cartridge controller (type 0x{kind:02X}); supported: ROM/RAM, MBC1, MBC2, MBC3 and MBC5"));
    }
    Ok(())
}

pub fn validate_boot_rom(data: &[u8], cgb: bool) -> Result<(), String> {
    let size = if cgb { 0x900 } else { 0x100 };
    if data.len() != size {
        return Err(format!("{} boot ROM must be {size} bytes", if cgb { "CGB" } else { "DMG" }));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn accepts_standard_controllers_and_rejects_invalid_uploads() {
        for kind in [0, 1, 2, 3, 5, 6, 8, 9, 0x0F, 0x10, 0x11, 0x12, 0x13, 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E] {
            let mut rom = vec![0; 0x8000];
            rom[0x147] = kind;
            assert!(validate_rom(&rom).is_ok(), "type={kind}");
        }
        assert!(validate_rom(&[]).is_err());
        assert!(validate_rom(&vec![0; 0x8001]).is_err());
        assert!(validate_rom(&vec![0; 8 * 1024 * 1024 + 0x4000]).is_err());
        let mut rom = vec![0; 0x8000]; rom[0x147] = 0x22;
        assert!(validate_rom(&rom).unwrap_err().contains("Unsupported"));
    }
    #[test]
    fn checks_model_specific_boot_rom_sizes() {
        assert!(validate_boot_rom(&vec![0; 0x100], false).is_ok());
        assert!(validate_boot_rom(&vec![0; 0x900], true).is_ok());
        assert!(validate_boot_rom(&vec![0; 0x100], true).is_err());
        assert!(validate_boot_rom(&[], false).is_err());
    }
}

//! Original cartridge SRAM and Macronix-compatible Flash command hardware.
//! Chip identification uses SDK tags in cartridge data, not a title database.
//! Program/erase currently complete immediately; busy/toggle timing and EEPROM
//! serial transfers require further hardware work.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Kind {
    None,
    Sram,
    Flash64,
    Flash128,
}
impl Kind {
    pub fn size(self) -> usize {
        match self {
            Self::None => 0,
            Self::Sram => 0x8000,
            Self::Flash64 => 0x10000,
            Self::Flash128 => 0x20000,
        }
    }
    pub fn code(self) -> u8 {
        match self {
            Self::None => 0,
            Self::Sram => 1,
            Self::Flash64 => 2,
            Self::Flash128 => 3,
        }
    }
}
pub(crate) struct Backup {
    pub kind: Kind,
    pub data: Vec<u8>,
    pub bank: u8,
    pub phase: u8,
    pub id: bool,
    pub dirty: bool,
    pub rom_hash: u64,
}
impl Backup {
    pub fn detect(rom: &[u8]) -> Self {
        let contains = |tag: &[u8]| {
            rom.windows(tag.len() + 3)
                .any(|w| w.starts_with(tag) && w[tag.len()..].iter().all(u8::is_ascii_digit))
        };
        let kind = if contains(b"FLASH1M_V") {
            Kind::Flash128
        } else if contains(b"FLASH512_V") || contains(b"FLASH_V") {
            Kind::Flash64
        } else if contains(b"SRAM_V") || contains(b"SRAM_F_V") {
            Kind::Sram
        } else {
            Kind::None
        };
        Self {
            kind,
            data: vec![255; kind.size()],
            bank: 0,
            phase: 0,
            id: false,
            dirty: false,
            rom_hash: crate::backend::hash(rom),
        }
    }
    pub fn read(&self, address: u32) -> u8 {
        let offset = address as usize & 0xffff;
        if self.kind == Kind::None {
            return 255;
        }
        if self.id {
            match offset {
                0 => return 0xc2,
                1 => return if self.kind == Kind::Flash128 { 9 } else { 0x1c },
                _ => {}
            }
        }
        self.data[(usize::from(self.bank) * 0x10000 + offset) % self.data.len()]
    }
    fn program(&mut self, offset: usize, value: u8) {
        let index = (usize::from(self.bank) * 0x10000 + offset) % self.data.len();
        let new = if self.kind == Kind::Sram {
            value
        } else {
            self.data[index] & value
        };
        self.dirty |= self.data[index] != new;
        self.data[index] = new;
    }
    fn erase(&mut self, start: usize, len: usize) {
        let bytes = &mut self.data[start..start + len];
        self.dirty |= bytes.iter().any(|v| *v != 255);
        bytes.fill(255);
    }
    pub fn write(&mut self, address: u32, value: u8) {
        if self.kind == Kind::None {
            return;
        }
        let offset = address as usize & 0xffff;
        if self.kind == Kind::Sram {
            self.program(offset, value);
            return;
        }
        if self.phase == 3 {
            self.program(offset, value);
            self.phase = 0;
            return;
        }
        if value == 0xf0 {
            self.id = false;
            self.phase = 0;
            return;
        }
        self.phase = match self.phase {
            0 => {
                if offset == 0x5555 && value == 0xaa {
                    1
                } else {
                    0
                }
            }
            1 => {
                if offset == 0x2aaa && value == 0x55 {
                    2
                } else {
                    0
                }
            }
            2 => {
                if offset != 0x5555 {
                    0
                } else {
                    match value {
                        0x90 => {
                            self.id = true;
                            0
                        }
                        0x80 => 4,
                        0xa0 => 3,
                        0xb0 if self.kind == Kind::Flash128 => 7,
                        _ => 0,
                    }
                }
            }
            4 => {
                if offset == 0x5555 && value == 0xaa {
                    5
                } else {
                    0
                }
            }
            5 => {
                if offset == 0x2aaa && value == 0x55 {
                    6
                } else {
                    0
                }
            }
            6 => {
                if offset == 0x5555 && value == 0x10 {
                    self.erase(0, self.data.len());
                } else if offset & 0xfff == 0 && value == 0x30 {
                    self.erase(usize::from(self.bank) * 0x10000 + offset, 0x1000);
                }
                0
            }
            7 => {
                if offset == 0 && value <= 1 {
                    self.bank = value;
                }
                0
            }
            _ => 0,
        };
    }
    pub fn import(&mut self, data: &[u8]) -> Result<(), String> {
        if self.kind == Kind::None || data.len() != self.data.len() {
            return Err("Incorrect GBA cartridge save size".into());
        }
        self.data.copy_from_slice(data);
        self.dirty = false;
        Ok(())
    }
    pub fn reset_commands(&mut self) {
        self.bank = 0;
        self.phase = 0;
        self.id = false;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn chip(tag: &str) -> Backup {
        Backup::detect(tag.as_bytes())
    }
    fn command(b: &mut Backup, c: u8) {
        b.write(0x5555, 0xaa);
        b.write(0x2aaa, 0x55);
        b.write(0x5555, c);
    }
    fn program(b: &mut Backup, addr: u32, v: u8) {
        command(b, 0xa0);
        b.write(addr, v);
    }
    #[test]
    fn tags_are_versioned_and_larger_flash_takes_precedence() {
        for (tag, kind) in [
            ("FLASH1M_V103", Kind::Flash128),
            ("FLASH512_V130", Kind::Flash64),
            ("FLASH_V123", Kind::Flash64),
            ("SRAM_V110", Kind::Sram),
            ("SRAM_F_V100", Kind::Sram),
            ("FLASH1M_VXXX", Kind::None),
            ("EEPROM_V124", Kind::None),
        ] {
            assert_eq!(chip(tag).kind, kind);
        }
        assert_eq!(
            chip("SRAM_V110 FLASH_V123 FLASH1M_V103").kind,
            Kind::Flash128
        );
    }
    #[test]
    fn id_mode_and_unlocked_commands_do_not_program() {
        let mut b = chip("FLASH_V123");
        b.write(0x5555, 0x90);
        assert_eq!(b.read(0), 255);
        command(&mut b, 0x90);
        assert_eq!(b.read(0), 0xc2);
        assert_eq!(b.read(1), 0x1c);
        assert!(!b.dirty);
        command(&mut b, 0xf0);
        assert_eq!(b.read(0), 255);
        command(&mut b, 0x90);
        b.write(0x1234, 0xf0);
        assert_eq!(b.read(0), 255);
    }
    #[test]
    fn flash_banks_program_only_clear_bits_and_erase_isolated_sectors() {
        let mut b = chip("FLASH1M_V103");
        program(&mut b, 0x1234, 0xa5);
        program(&mut b, 0x1234, 0x5a);
        assert_eq!(b.read(0x1234), 0);
        command(&mut b, 0xb0);
        b.write(0, 1);
        assert_eq!(b.read(0x1234), 255);
        program(&mut b, 0x1234, 0xf0);
        assert_eq!(b.read(0x1234), 0xf0);
        program(&mut b, 0x2234, 0x77);
        command(&mut b, 0x80);
        b.write(0x5555, 0xaa);
        b.write(0x2aaa, 0x55);
        b.write(0x1000, 0x30);
        assert_eq!(b.read(0x1234), 255);
        assert_eq!(b.read(0x2234), 0x77);
        command(&mut b, 0xb0);
        b.write(0, 0);
        assert_eq!(b.read(0x1234), 0);
        command(&mut b, 0x80);
        b.write(0x5555, 0xaa);
        b.write(0x2aaa, 0x55);
        b.write(0x5555, 0x10);
        assert!(b.data.iter().all(|v| *v == 255));
    }
    #[test]
    fn bad_unlock_and_invalid_bank_are_safe() {
        let mut b = chip("FLASH1M_V103");
        b.write(0x5555, 0xaa);
        b.write(0x2aab, 0x55);
        b.write(0x5555, 0xa0);
        b.write(1, 0);
        assert_eq!(b.read(1), 255);
        command(&mut b, 0xb0);
        b.write(0, 255);
        assert_eq!(b.bank, 0);
        assert!(b.import(&[0; 10]).is_err());
        assert!(b.data.iter().all(|v| *v == 255));
    }
    #[test]
    fn sram_mirrors_and_dirty_changes_only_with_data() {
        let mut b = chip("SRAM_V110");
        b.write(0x8123, 255);
        assert!(!b.dirty);
        b.write(0x8123, 0x34);
        assert_eq!(b.read(0x123), 0x34);
        assert!(b.dirty);
        b.dirty = false;
        b.write(0x123, 0x34);
        assert!(!b.dirty);
        b.write(0x123, 255);
        assert_eq!(b.read(0x8123), 255);
    }
}

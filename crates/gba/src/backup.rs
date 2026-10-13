//! Original cartridge SRAM and Macronix-compatible Flash command hardware.
//! Chip identification uses SDK tags in cartridge data, not a title database.
//! Program/erase expose Macronix-style DQ7 polling and DQ6 toggle status.
//! Fixed operation durations are nominal compatibility values, not measured
//! chip-specific timings. Serial EEPROM is implemented separately.
// Command format: https://rust-console.github.io/gbatek-gbaonly/#gba-cart-backup-flash-rom
// Polling behavior: Macronix MX29GL320E datasheet, write-operation status table.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SaveType {
    None,
    Sram,
    Flash64,
    Flash128,
    Eeprom,
}
pub(crate) type Kind = SaveType;
impl SaveType {
    pub fn size(self) -> usize {
        match self {
            Self::None => 0,
            Self::Sram => 0x8000,
            Self::Flash64 => 0x10000,
            Self::Flash128 => 0x20000,
            Self::Eeprom => 8192,
        }
    }
    pub fn code(self) -> u8 {
        match self {
            Self::None => 0,
            Self::Sram => 1,
            Self::Flash64 => 2,
            Self::Flash128 => 3,
            Self::Eeprom => 4,
        }
    }
}
pub(crate) const PROGRAM_CLOCKS: u64 = 336; // nominal 20 microseconds
pub(crate) const SECTOR_ERASE_CLOCKS: u64 = 419_431; // nominal 25 milliseconds
pub(crate) const CHIP_ERASE_CLOCKS: u64 = 1_677_722; // nominal 100 milliseconds
pub(crate) const FLASH_STATE_BYTES: usize = 15;
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub(crate) struct FlashOperation {
    pub clocks: u64,
    // 0: idle, 1: byte program, 2: sector erase, 3: chip erase.
    pub kind: u8,
    pub address: u32,
    pub value: u8,
    pub toggle: bool,
}
impl FlashOperation {
    pub fn encode(self) -> [u8; FLASH_STATE_BYTES] {
        let mut bytes = [0; FLASH_STATE_BYTES];
        bytes[..8].copy_from_slice(&self.clocks.to_le_bytes());
        bytes[8] = self.kind;
        bytes[9..13].copy_from_slice(&self.address.to_le_bytes());
        bytes[13] = self.value;
        bytes[14] = u8::from(self.toggle);
        bytes
    }
    pub fn decode(bytes: &[u8]) -> Result<Self, String> {
        if bytes.len() != FLASH_STATE_BYTES || bytes[14] > 1 {
            return Err("Invalid GBA Flash operation state".into());
        }
        Ok(Self {
            clocks: u64::from_le_bytes(bytes[..8].try_into().unwrap()),
            kind: bytes[8],
            address: u32::from_le_bytes(bytes[9..13].try_into().unwrap()),
            value: bytes[13],
            toggle: bytes[14] != 0,
        })
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
    pub eeprom: crate::eeprom::Eeprom,
    pub flash: FlashOperation,
}
impl Backup {
    pub fn detect(rom: &[u8]) -> Self {
        let contains = |tag: &[u8]| {
            rom.windows(tag.len()).enumerate().any(|(offset, w)| {
                if w != tag {
                    return false;
                }
                let suffix = &rom[offset + tag.len()..];
                // SDK version digits remain preferred. Independent/homebrew
                // cartridges also use bare tags followed by padding or EOF.
                suffix
                    .get(..3)
                    .is_some_and(|v| v.iter().all(u8::is_ascii_digit))
                    || suffix.is_empty()
                    || suffix
                        .first()
                        .is_some_and(|v| *v == 0 || v.is_ascii_whitespace())
            })
        };
        let kind = if contains(b"FLASH1M_V") {
            Kind::Flash128
        } else if contains(b"FLASH512_V") || contains(b"FLASH_V") {
            Kind::Flash64
        } else if contains(b"SRAM_V") || contains(b"SRAM_F_V") {
            Kind::Sram
        } else if contains(b"EEPROM_V") {
            Kind::Eeprom
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
            eeprom: crate::eeprom::Eeprom::default(),
            flash: FlashOperation::default(),
        }
    }
    pub fn peek(&self, address: u32) -> u8 {
        let offset = address as usize & 0xffff;
        if matches!(self.kind, Kind::None | Kind::Eeprom) {
            return 255;
        }
        if self.flash.clocks != 0 {
            // DQ7 is the complement of the programmed datum (zero for erase).
            // DQ5 stays clear: this deterministic chip does not fail internally.
            let polling = if self.flash.kind == 1 {
                !self.flash.value & 0x80
            } else {
                0
            };
            return polling | if self.flash.toggle { 0x40 } else { 0 };
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
    pub fn read(&mut self, address: u32) -> u8 {
        let value = self.peek(address);
        if self.flash.clocks != 0 {
            self.flash.toggle = !self.flash.toggle;
        }
        value
    }
    pub fn clock_flash(&mut self, ticks: u64) {
        if self.flash.clocks == 0 {
            return;
        }
        self.flash.clocks = self.flash.clocks.saturating_sub(ticks);
        if self.flash.clocks != 0 {
            return;
        }
        let operation = std::mem::take(&mut self.flash);
        match operation.kind {
            1 => {
                let index = operation.address as usize;
                let value = self.data[index] & operation.value;
                self.dirty |= self.data[index] != value;
                self.data[index] = value;
            }
            2 => self.erase(operation.address as usize, 0x1000),
            3 => self.erase(0, self.data.len()),
            _ => {}
        }
    }
    pub fn clock(&mut self, ticks: u64) {
        self.clock_eeprom(ticks);
        self.clock_flash(ticks);
    }
    pub fn valid_flash(&self) -> bool {
        let f = self.flash;
        if f.kind == 0 {
            return f == FlashOperation::default();
        }
        if !matches!(self.kind, Kind::Flash64 | Kind::Flash128) || f.clocks == 0 {
            return false;
        }
        match f.kind {
            1 => f.clocks <= PROGRAM_CLOCKS && (f.address as usize) < self.data.len(),
            2 => {
                f.clocks <= SECTOR_ERASE_CLOCKS
                    && f.address & 0xfff == 0
                    && (f.address as usize) < self.data.len()
            }
            3 => f.clocks <= CHIP_ERASE_CLOCKS && f.address == 0,
            _ => false,
        }
    }
    /// Select save hardware without changing the ROM identity or discarding
    /// programmed bytes. A shrinking change is allowed only for erased tails.
    pub fn set_type(&mut self, kind: SaveType) -> Result<(), String> {
        if kind == self.kind {
            return Ok(());
        }
        if self.flash.clocks != 0 || self.eeprom.busy != 0 {
            return Err("Wait for the cartridge save operation to finish".into());
        }
        let size = kind.size();
        if self
            .data
            .get(size..)
            .is_some_and(|tail| tail.iter().any(|v| *v != 255))
        {
            return Err("Save type would discard existing cartridge data".into());
        }
        self.data.resize(size, 255);
        self.kind = kind;
        self.reset_commands();
        self.eeprom = crate::eeprom::Eeprom::default();
        self.dirty |= self.data.iter().any(|v| *v != 255);
        Ok(())
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
        if matches!(self.kind, Kind::None | Kind::Eeprom) {
            return;
        }
        let offset = address as usize & 0xffff;
        if self.kind == Kind::Sram {
            self.program(offset, value);
            return;
        }
        if self.flash.clocks != 0 {
            if value == 0xf0 && offset == 0x5555 {
                self.flash = FlashOperation::default();
                self.phase = 0;
                self.id = false;
            }
            return;
        }
        if self.phase == 3 {
            self.flash = FlashOperation {
                clocks: PROGRAM_CLOCKS,
                kind: 1,
                address: (usize::from(self.bank) * 0x10000 + offset) as u32,
                value,
                toggle: false,
            };
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
                    self.flash = FlashOperation {
                        clocks: CHIP_ERASE_CLOCKS,
                        kind: 3,
                        ..Default::default()
                    };
                } else if offset & 0xfff == 0 && value == 0x30 {
                    self.flash = FlashOperation {
                        clocks: SECTOR_ERASE_CLOCKS,
                        kind: 2,
                        address: (usize::from(self.bank) * 0x10000 + offset) as u32,
                        ..Default::default()
                    };
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
        if self.kind == Kind::None {
            let inferred = match data.len() {
                512 | 8192 => Kind::Eeprom,
                0x8000 => Kind::Sram,
                0x10000 => Kind::Flash64,
                0x20000 => Kind::Flash128,
                _ => return Err("Unrecognized GBA cartridge save size".into()),
            };
            self.set_type(inferred)?;
        }
        if self.kind == Kind::Eeprom {
            let bits = match data.len() {
                512 => 6,
                8192 => 14,
                _ => return Err("EEPROM saves must contain 512 or 8192 bytes".into()),
            };
            self.data.clear();
            self.data.extend_from_slice(data);
            self.dirty = false;
            self.eeprom = crate::eeprom::Eeprom {
                address_bits: bits,
                ..Default::default()
            };
            return Ok(());
        }
        if self.kind == Kind::None || data.len() != self.data.len() {
            return Err("Incorrect GBA cartridge save size".into());
        }
        self.data.copy_from_slice(data);
        self.dirty = false;
        self.reset_commands();
        Ok(())
    }
    pub fn reset_commands(&mut self) {
        self.eeprom = crate::eeprom::Eeprom {
            address_bits: self.eeprom.address_bits,
            ..Default::default()
        };
        self.bank = 0;
        self.phase = 0;
        self.id = false;
        self.flash = FlashOperation::default();
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
        b.clock_flash(PROGRAM_CLOCKS);
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
            ("EEPROM_V124", Kind::Eeprom),
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
        b.clock_flash(SECTOR_ERASE_CLOCKS);
        assert_eq!(b.read(0x1234), 255);
        assert_eq!(b.read(0x2234), 0x77);
        command(&mut b, 0xb0);
        b.write(0, 0);
        assert_eq!(b.read(0x1234), 0);
        command(&mut b, 0x80);
        b.write(0x5555, 0xaa);
        b.write(0x2aaa, 0x55);
        b.write(0x5555, 0x10);
        b.clock_flash(CHIP_ERASE_CLOCKS);
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
    #[test]
    fn unversioned_test_tags_require_padding_or_end() {
        for (tag, kind) in [
            ("SRAM_V      ", Kind::Sram),
            ("FLASH_V\0", Kind::Flash64),
            ("FLASH1M_V", Kind::Flash128),
            ("EEPROM_V\n", Kind::Eeprom),
            ("SRAM_VERBOSE", Kind::None),
            ("FLASH_VXXX", Kind::None),
        ] {
            assert_eq!(chip(tag).kind, kind);
        }
    }
    #[test]
    fn manual_hardware_selection_and_import_never_discard_progress() {
        let mut b = chip("No SDK tags");
        let hash = b.rom_hash;
        let mut save = vec![255; 0x8000];
        save[19] = 42;
        b.import(&save).unwrap();
        assert_eq!(b.kind, Kind::Sram);
        assert_eq!(b.data, save);
        b.set_type(Kind::Flash128).unwrap();
        assert_eq!(b.data[19], 42);
        assert_eq!(b.rom_hash, hash);
        b.data[0x10000] = 7;
        assert!(b.set_type(Kind::Flash64).is_err());
        assert_eq!(b.kind, Kind::Flash128);
        assert_eq!(b.data[0x10000], 7);
        assert!(b.set_type(Kind::None).is_err());
        let before = b.data.clone();
        assert!(b.import(&[0; 513]).is_err());
        assert_eq!(b.data, before);
        let mut unknown = chip("No SDK tags");
        assert!(unknown.import(&[0; 513]).is_err());
        assert_eq!(unknown.kind, Kind::None);
        assert!(unknown.data.is_empty());
    }
    #[test]
    fn flash_polling_toggles_until_clocked_commit_and_reset_aborts() {
        for value in [0x7f, 0x80, 0xf0] {
            let mut b = chip("FLASH_V123");
            command(&mut b, 0xa0);
            b.write(0x1234, value);
            assert_eq!(b.data[0x1234], 255);
            assert!(!b.dirty);
            assert!(b.valid_flash());
            let first = b.read(0x1234);
            let second = b.read(0x1234);
            assert_eq!(first ^ second, 0x40);
            assert_eq!(first & 0x80, !value & 0x80);
            assert_eq!(first & 0x20, 0);
            let peek = b.peek(0x1234);
            assert_eq!(
                b.peek(0x1234),
                peek,
                "Debugger reads leave toggle state unchanged"
            );
            b.write(0x1000, 0);
            assert_eq!(b.data[0x1000], 255);
            b.clock_flash(PROGRAM_CLOCKS - 1);
            assert!(!b.dirty);
            b.clock_flash(1);
            assert_eq!(b.read(0x1234), value);
            assert!(b.dirty);
            assert!(b.valid_flash());
            command(&mut b, 0xa0);
            b.write(0x1234, 0);
            b.write(0x5555, 0xf0);
            b.clock_flash(PROGRAM_CLOCKS);
            assert_eq!(b.read(0x1234), value);
        }
    }
    #[test]
    fn flash_pending_sector_and_chip_erases_are_delayed() {
        let mut b = chip("FLASH1M_V103");
        b.data[0x1200] = 0;
        b.data[0x2200] = 0;
        b.data[0x11200] = 0;
        command(&mut b, 0x80);
        b.write(0x5555, 0xaa);
        b.write(0x2aaa, 0x55);
        b.write(0x1000, 0x30);
        assert_eq!(b.read(0x1000) & 0x80, 0);
        assert!(b.set_type(Kind::Sram).is_err());
        let encoded = b.flash.encode();
        assert_eq!(FlashOperation::decode(&encoded).unwrap(), b.flash);
        b.clock_flash(SECTOR_ERASE_CLOCKS - 1);
        assert_eq!(b.data[0x1200], 0);
        b.clock_flash(1);
        assert_eq!(b.data[0x1200], 255);
        assert_eq!(b.data[0x2200], 0);
        assert_eq!(b.data[0x11200], 0);
        command(&mut b, 0x80);
        b.write(0x5555, 0xaa);
        b.write(0x2aaa, 0x55);
        b.write(0x5555, 0x10);
        b.clock_flash(CHIP_ERASE_CLOCKS - 1);
        assert_eq!(b.data[0x11200], 0);
        b.clock_flash(1);
        assert!(b.data.iter().all(|v| *v == 255));
        assert!(b.valid_flash());
        let mut corrupt = encoded;
        corrupt[14] = 2;
        assert!(FlashOperation::decode(&corrupt).is_err());
        b.flash = FlashOperation {
            kind: 2,
            clocks: 1,
            address: 3,
            ..Default::default()
        };
        assert!(!b.valid_flash());
    }
}

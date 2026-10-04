//! GBA address decoding and instruction-granular wait states. No BIOS is bundled.
use crate::cpu::Memory;
use crate::peripherals::{Dma, Timer};

pub struct Bus {
    pub(crate) bios: Vec<u8>,
    pub(crate) replacement_bios: bool,
    pub(crate) rom: Vec<u8>,
    pub ewram: Vec<u8>,
    pub iwram: Vec<u8>,
    pub palette: Vec<u8>,
    pub vram: Vec<u8>,
    pub oam: Vec<u8>,
    pub(crate) io: Vec<u8>,
    pub(crate) clocks: u64,
    pub halted: bool,
    pub stopped: bool,
    pub(crate) timers: [Timer; 4],
    // Derived scheduling caches: rebuilt after register writes/state restore.
    pub(crate) timer_enabled: u8,
    pub(crate) timer_next: u64,
    pub(crate) dma: [Dma; 4],
    pub(crate) apu: crate::apu::Apu,
    pub(crate) backup: crate::backup::Backup,
    pub(crate) affine: crate::ppu::Affine,
}
impl Bus {
    pub fn new(rom: Vec<u8>, bios: Option<Vec<u8>>) -> Result<Self, String> {
        if rom.len() < 0xc0 || rom.len() > 0x2000000 {
            return Err("GBA cartridge must contain 192 bytes to 32 MiB".into());
        }
        if bios.as_ref().is_some_and(|data| data.len() != 0x4000) {
            return Err("GBA BIOS must be exactly 16 KiB".into());
        }
        let replacement_bios = bios.is_none();
        let backup = crate::backup::Backup::detect(&rom);
        let mut bus = Self {
            bios: bios.unwrap_or_else(crate::bios::irq_firmware),
            replacement_bios,
            rom,
            ewram: vec![0; 0x40000],
            iwram: vec![0; 0x8000],
            palette: vec![0; 0x400],
            vram: vec![0; 0x18000],
            oam: vec![0; 0x400],
            io: vec![0; 0x400],
            clocks: 0,
            halted: false,
            stopped: false,
            timers: [Timer::default(); 4],
            timer_enabled: 0,
            timer_next: u64::MAX,
            dma: [Dma::default(); 4],
            apu: crate::apu::Apu::default(),
            backup,
            affine: crate::ppu::Affine::default(),
        };
        bus.set_io(0x130, 0x3ff);
        bus.set_io(0, 0x80);
        bus.set_io(0x88, 0x200);
        Ok(bus)
    }
    pub fn take_cycles(&mut self) -> u64 {
        std::mem::take(&mut self.clocks)
    }
    pub fn io16(&self, offset: usize) -> u16 {
        u16::from_le_bytes([self.io[offset], self.io[offset + 1]])
    }
    pub(crate) fn set_io(&mut self, offset: usize, value: u16) {
        self.io[offset..offset + 2].copy_from_slice(&value.to_le_bytes());
    }
    pub fn request_irq(&mut self, bits: u16) {
        self.set_io(0x202, self.io16(0x202) | bits);
    }
    pub fn pending_irq(&self) -> bool {
        self.io16(0x200) & self.io16(0x202) != 0
    }
    pub fn irq_enabled(&self) -> bool {
        self.io16(0x208) & 1 != 0 && self.pending_irq()
    }
    pub fn set_keys(&mut self, pressed: u16) {
        self.set_io(0x130, !pressed & 0x3ff);
        self.check_key_irq();
    }
    fn check_key_irq(&mut self) {
        let control = self.io16(0x132);
        let selected = control & 0x3ff;
        let pressed = !self.io16(0x130) & selected;
        let matched = if control & 0x8000 != 0 {
            pressed == selected
        } else {
            pressed != 0
        };
        if control & 0x4000 != 0 && matched {
            self.request_irq(1 << 12);
        }
    }
    pub(crate) fn enter_low_power(&mut self, value: u8) {
        self.stopped = value & 0x80 != 0;
        self.halted = !self.stopped;
    }
    pub(crate) fn stop_wake_pending(&self) -> bool {
        let sources = 0x3000
            | if self.io16(0x134) & 0xc000 == 0x8000 {
                0x80 // Serial IRQ wakes only in general-purpose SIO mode.
            } else {
                0
            };
        self.io16(0x200) & self.io16(0x202) & sources != 0
    }
    fn vram_index(address: u32) -> usize {
        let index = address as usize & 0x1ffff;
        if index >= 0x18000 {
            index - 0x8000
        } else {
            index
        }
    }
    /// Non-mutating debugger read. BIOS protection and open-bus latch are pending.
    pub fn peek8(&self, address: u32) -> u8 {
        if self.eeprom_address(address) {
            return if address & 1 == 0 {
                self.backup.eeprom.peek() as u8
            } else {
                0
            };
        }
        let index = address as usize;
        match address >> 24 {
            0 if address < 0x4000 => self.bios.get(index).copied().unwrap_or(0),
            2 => self.ewram[index & 0x3ffff],
            3 => self.iwram[index & 0x7fff],
            4 if index & 0xffffff < 0x400 => {
                let offset = index & 0x3ff;
                if (0x60..0xa0).contains(&offset) {
                    self.sound_read(offset)
                } else if (0x100..0x110).contains(&offset) && offset & 3 < 2 {
                    (self.timers[(offset - 0x100) / 4].counter >> ((offset & 1) * 8)) as u8
                } else if ((0xb0..0xe0).contains(&offset) && (offset - 0xb0) % 12 < 10)
                    || (0xa0..0xa8).contains(&offset)
                {
                    0
                } else {
                    self.io[offset]
                }
            }
            5 => self.palette[index & 0x3ff],
            6 => self.vram[Self::vram_index(address)],
            7 => self.oam[index & 0x3ff],
            8..=13 => self
                .rom
                .get(index & 0x1ffffff)
                .copied()
                .unwrap_or(((address >> 1) >> ((address & 1) * 8)) as u8),
            14..=15 => self.backup.read(address),
            _ => 0,
        }
    }
    pub fn peek16(&self, address: u32) -> u16 {
        if let Some(bytes) = self.linear_bytes::<2>(address) {
            return u16::from_le_bytes(bytes);
        }
        u16::from_le_bytes([self.peek8(address), self.peek8(address.wrapping_add(1))])
    }
    pub fn peek32(&self, address: u32) -> u32 {
        if let Some(bytes) = self.linear_bytes::<4>(address) {
            return u32::from_le_bytes(bytes);
        }
        u32::from_le_bytes(std::array::from_fn(|i| {
            self.peek8(address.wrapping_add(i as u32))
        }))
    }
    /// Decode ordinary memory once per wide access. Fall back to byte decoding
    /// for I/O, serial devices, absent ROM and mirror/region boundary crossings.
    #[inline]
    fn linear_bytes<const N: usize>(&self, address: u32) -> Option<[u8; N]> {
        let index = address as usize;
        let (bytes, offset): (&[u8], usize) = match address >> 24 {
            0 if address < 0x4000 => (&self.bios, index),
            2 => (&self.ewram, index & 0x3ffff),
            3 => (&self.iwram, index & 0x7fff),
            5 => (&self.palette, index & 0x3ff),
            6 => (&self.vram, Self::vram_index(address)),
            7 => (&self.oam, index & 0x3ff),
            8..=13
                if !(address >> 24 == 13
                    && (self.eeprom_address(address)
                        || self.eeprom_address(address.wrapping_add(N as u32 - 1)))) =>
            {
                (&self.rom, index & 0x1ffffff)
            }
            _ => return None,
        };
        // Cross-region unaligned debugger peeks retain byte-level semantics.
        if address & 0xffffff > 0x1000000 - N as u32 {
            return None;
        }
        bytes.get(offset..offset + N)?.try_into().ok()
    }
    fn charge(&mut self, address: u32, width: u32, sequential: bool) {
        let clocks = match address >> 24 {
            2 => {
                if width == 4 {
                    6
                } else {
                    3
                }
            }
            5..=7 => {
                if width == 4 {
                    2
                } else {
                    1
                }
            }
            8..=13 => {
                let area = ((address >> 25) - 4) as usize;
                let wait = self.io16(0x204);
                let first = [4, 3, 2, 8][(wait >> (2 + area * 3) & 3) as usize] + 1;
                let second = if wait >> (4 + area * 3) & 1 != 0 {
                    2
                } else {
                    [3, 5, 9][area]
                };
                (if sequential { second } else { first }) + if width == 4 { second } else { 0 }
            }
            14 | 15 => [4, 3, 2, 8][(self.io16(0x204) & 3) as usize] + 1,
            _ => 1,
        };
        self.clocks += clocks;
    }
    fn put8(&mut self, address: u32, value: u8) {
        let index = address as usize;
        match address >> 24 {
            2 => self.ewram[index & 0x3ffff] = value,
            3 => self.iwram[index & 0x7fff] = value,
            4 if index & 0xffffff < 0x400 => {
                let offset = index & 0x3ff;
                let old = self.io[offset];
                if (0x60..0xa0).contains(&offset) {
                    self.apu.psg.flush(&mut self.io);
                }
                match offset {
                    0x202 | 0x203 => self.io[offset] &= !value,
                    6 | 7 | 0x130 | 0x131 => {}
                    4 => self.io[4] = (self.io[4] & 7) | (value & 0x38),
                    0x301 => self.enter_low_power(value),
                    0x133 => self.io[offset] = value & 0xc3,
                    _ => self.io[offset] = value,
                }
                self.peripheral_write(offset, old);
                self.sound_write(offset, old);
                self.affine_write(offset);
            }
            5 => self.palette[index & 0x3ff] = value,
            6 => self.vram[Self::vram_index(address)] = value,
            7 => self.oam[index & 0x3ff] = value,
            14..=15 => self.backup.write(address, value),
            _ => {}
        }
    }
}
impl Memory for Bus {
    fn read8(&mut self, address: u32) -> u8 {
        self.charge(address, 1, false);
        self.flush_sound_read(address);
        self.peek8(address)
    }
    fn read16(&mut self, address: u32, sequential: bool) -> u16 {
        self.charge(address, 2, sequential);
        self.flush_sound_read(address);
        if self.eeprom_address(address) {
            return self.backup.eeprom.read();
        }
        if (14..=15).contains(&(address >> 24)) {
            return u16::from(self.backup.read(address)) * 0x101;
        }
        self.peek16(address & !1)
    }
    fn read32(&mut self, address: u32, sequential: bool) -> u32 {
        self.charge(address, 4, sequential);
        self.flush_sound_read(address);
        if (14..=15).contains(&(address >> 24)) {
            return u32::from(self.backup.read(address)) * 0x01010101;
        }
        self.peek32(address & !3)
    }
    fn write8(&mut self, address: u32, value: u8) {
        self.charge(address, 1, false);
        match address >> 24 {
            5 => {
                self.put8(address & !1, value);
                self.put8(address | 1, value);
            }
            6 => {
                let obj = if self.io16(0) & 7 >= 3 {
                    0x14000
                } else {
                    0x10000
                };
                if Self::vram_index(address) < obj {
                    self.put8(address & !1, value);
                    self.put8(address | 1, value);
                }
            }
            7 => {}
            _ => self.put8(address, value),
        }
        if matches!(address, 0x04000132 | 0x04000133) {
            self.check_key_irq();
        }
    }
    fn write16(&mut self, address: u32, value: u16, sequential: bool) {
        self.charge(address, 2, sequential);
        if self.eeprom_address(address) {
            self.backup.eeprom_bit(value);
            return;
        }
        if (14..=15).contains(&(address >> 24)) {
            self.backup
                .write(address, value.rotate_right((address & 1) * 8) as u8);
            return;
        }
        let address = address & !1;
        for (i, byte) in value.to_le_bytes().into_iter().enumerate() {
            self.put8(address + i as u32, byte);
        }
        if address == 0x04000132 {
            self.check_key_irq();
        }
    }
    fn write32(&mut self, address: u32, value: u32, sequential: bool) {
        self.charge(address, 4, sequential);
        if (14..=15).contains(&(address >> 24)) {
            self.backup
                .write(address, value.rotate_right((address & 3) * 8) as u8);
            return;
        }
        let address = address & !3;
        for (i, byte) in value.to_le_bytes().into_iter().enumerate() {
            self.put8(address + i as u32, byte);
        }
        if address == 0x04000130 {
            self.check_key_irq();
        }
    }
    fn internal(&mut self, clocks: u32) {
        self.clocks += u64::from(clocks);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn bus() -> Bus {
        Bus::new(vec![0; 0xc0], None).unwrap()
    }
    #[test]
    fn keypad_irq_or_and_enable_masks_and_control_writes() {
        let mut b = bus();
        b.set_keys(1);
        assert_eq!(b.io16(0x202), 0);
        b.write16(0x04000132, 0x4003, false);
        assert_eq!(b.io16(0x202), 0x1000);
        b.write16(0x04000202, 0x1000, false);
        b.write16(0x04000132, 0xc003, false);
        assert_eq!(b.io16(0x202), 0);
        b.set_keys(3);
        assert_eq!(b.io16(0x202), 0x1000);
        b.write16(0x04000202, 0x1000, false);
        b.set_keys(1);
        assert_eq!(b.io16(0x202), 0);
        b.write8(0x04000133, 0x40);
        assert_eq!(b.io16(0x202), 0x1000);
        b.write16(0x04000202, 0x1000, false);
        b.write32(0x04000130, 0x40020000, false);
        assert_eq!(b.io16(0x202), 0);
        assert_eq!(b.io16(0x130), 0x3fe); // KEYINPUT is read-only.
        b.write16(0x04000132, 0xffff, false);
        assert_eq!(b.io16(0x132), 0xc3ff);
        b.set_keys(0xffff);
        assert_eq!(b.io16(0x130), 0);
        assert_eq!(b.io16(0x202), 0x1000);
    }
    #[test]
    fn halfword_key_control_write_has_no_transient_low_byte_match() {
        let mut b = bus();
        b.set_keys(1);
        b.write16(0x04000132, 0x4002, false);
        // New low byte matches A, but the same halfword disables interrupts.
        b.write16(0x04000132, 1, false);
        assert_eq!(b.io16(0x202), 0);
    }
    #[test]
    fn backup_is_an_eight_bit_bus_not_consecutive_wide_accesses() {
        let mut rom = vec![0; 192];
        rom[80..89].copy_from_slice(b"SRAM_V110");
        let mut b = Bus::new(rom, None).unwrap();
        b.write32(0x0e000003, 0x12345678, false);
        assert_eq!(b.read8(0x0e000003), 0x12);
        assert_eq!(b.read8(0x0e000004), 255);
        assert_eq!(b.read16(0x0f000003, false), 0x1212);
        assert_eq!(b.read32(0x0e000003, false), 0x12121212);
        b.write16(0x0e000005, 0xabcd, false);
        assert_eq!(b.read8(0x0e000005), 0xab);
        assert_eq!(b.read8(0x0e000006), 255);
    }
    #[test]
    fn ram_mirrors_and_alignment() {
        let mut b = bus();
        b.write32(0x02000003, 0x12345678, false);
        assert_eq!(b.peek32(0x02040000), 0x12345678);
        b.write16(0x03008000, 0xabcd, false);
        assert_eq!(b.peek16(0x03000000), 0xabcd);
    }
    #[test]
    fn wide_memory_fast_path_matches_byte_decoder_at_mirrors_and_devices() {
        let mut rom = vec![0x5a; 0x2000000];
        rom[80..91].copy_from_slice(b"EEPROM_V124");
        let mut b = Bus::new(rom, None).unwrap();
        for bytes in [
            &mut b.ewram,
            &mut b.iwram,
            &mut b.palette,
            &mut b.vram,
            &mut b.oam,
        ] {
            for (i, byte) in bytes.iter_mut().enumerate() {
                *byte = i.wrapping_mul(37) as u8;
            }
        }
        for base in [
            0x3ffcu32, 0x0203fffc, 0x03007ffc, 0x04fffffc, 0x050003fc, 0x06017ffc, 0x0601fffc,
            0x070003fc, 0x08000000, 0x09fffffc, 0x0dfffefc, 0x0dfffffc, 0x0efffffc,
        ] {
            for offset in 0..8 {
                let address = base + offset;
                let bytes = std::array::from_fn(|i| b.peek8(address + i as u32));
                assert_eq!(
                    b.peek32(address),
                    u32::from_le_bytes(bytes),
                    "{address:08x}"
                );
                assert_eq!(b.peek16(address), u16::from_le_bytes([bytes[0], bytes[1]]));
            }
        }
        let short = bus();
        for address in [0x080000be, 0x080000bf, 0x080000c0] {
            assert_eq!(
                short.peek32(address),
                u32::from_le_bytes(std::array::from_fn(|i| short.peek8(address + i as u32)))
            );
        }
    }
    #[test]
    fn video_byte_writes() {
        let mut b = bus();
        b.write8(0x05000001, 0x12);
        assert_eq!(b.peek16(0x05000000), 0x1212);
        b.write8(0x06000000, 0x34);
        assert_eq!(b.peek16(0x06000000), 0x3434);
        b.write8(0x06010000, 0xff);
        assert_eq!(b.peek16(0x06010000), 0);
        b.write8(0x07000000, 0xff);
        assert_eq!(b.peek8(0x07000000), 0);
        b.write16(0x06018000, 0xabcd, false);
        assert_eq!(b.peek16(0x06010000), 0xabcd);
    }
    #[test]
    fn interrupt_flags_are_write_one_to_clear() {
        let mut b = bus();
        b.request_irq(7);
        b.write16(0x04000202, 2, false);
        assert_eq!(b.io16(0x202), 5);
        b.write16(0x04000200, 1, false);
        assert!(b.pending_irq());
        assert!(!b.irq_enabled());
        b.write16(0x04000208, 1, false);
        assert!(b.irq_enabled());
    }
    #[test]
    fn waitcnt_and_ram_costs() {
        let mut b = bus();
        b.read32(0x08000000, false);
        assert_eq!(b.take_cycles(), 8);
        b.read32(0x08000004, true);
        assert_eq!(b.take_cycles(), 6);
        b.write16(0x04000204, 0x18, false);
        b.take_cycles();
        b.read32(0x08000000, false);
        assert_eq!(b.take_cycles(), 5);
        b.read32(0x02000000, false);
        assert_eq!(b.take_cycles(), 6);
        b.read32(0x03000000, false);
        assert_eq!(b.take_cycles(), 1);
    }
    #[test]
    fn cartridge_windows_and_bounds() {
        let b = bus();
        assert_eq!(b.peek16(0x0a000000), b.peek16(0x08000000));
        assert_eq!(b.peek16(0x08001000), 0x800);
        assert!(Bus::new(vec![], None).is_err());
        assert!(Bus::new(vec![0; 192], Some(vec![0; 4])).is_err());
    }
}

//! GBA address decoding and instruction-granular wait states. No BIOS is bundled.
use crate::cpu::Memory;

pub struct Bus {
    pub(crate) bios: Vec<u8>,
    pub(crate) rom: Vec<u8>,
    pub ewram: Vec<u8>,
    pub iwram: Vec<u8>,
    pub palette: Vec<u8>,
    pub vram: Vec<u8>,
    pub oam: Vec<u8>,
    pub(crate) io: Vec<u8>,
    clocks: u64,
    pub halted: bool,
}
impl Bus {
    pub fn new(rom: Vec<u8>, bios: Option<Vec<u8>>) -> Result<Self, String> {
        if rom.len() < 0xc0 || rom.len() > 0x2000000 {
            return Err("GBA cartridge must contain 192 bytes to 32 MiB".into());
        }
        if bios.as_ref().is_some_and(|data| data.len() != 0x4000) {
            return Err("GBA BIOS must be exactly 16 KiB".into());
        }
        let mut bus = Self {
            bios: bios.unwrap_or_default(),
            rom,
            ewram: vec![0; 0x40000],
            iwram: vec![0; 0x8000],
            palette: vec![0; 0x400],
            vram: vec![0; 0x18000],
            oam: vec![0; 0x400],
            io: vec![0; 0x400],
            clocks: 0,
            halted: false,
        };
        bus.set_io(0x130, 0x3ff);
        bus.set_io(0, 0x80);
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
        let index = address as usize;
        match address >> 24 {
            0 if address < 0x4000 => self.bios.get(index).copied().unwrap_or(0),
            2 => self.ewram[index & 0x3ffff],
            3 => self.iwram[index & 0x7fff],
            4 if index & 0xffffff < 0x400 => self.io[index & 0x3ff],
            5 => self.palette[index & 0x3ff],
            6 => self.vram[Self::vram_index(address)],
            7 => self.oam[index & 0x3ff],
            8..=13 => self
                .rom
                .get(index & 0x1ffffff)
                .copied()
                .unwrap_or(((address >> 1) >> ((address & 1) * 8)) as u8),
            _ => 0,
        }
    }
    pub fn peek16(&self, address: u32) -> u16 {
        u16::from_le_bytes([self.peek8(address), self.peek8(address.wrapping_add(1))])
    }
    pub fn peek32(&self, address: u32) -> u32 {
        u32::from_le_bytes(std::array::from_fn(|i| {
            self.peek8(address.wrapping_add(i as u32))
        }))
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
                match offset {
                    0x202 | 0x203 => self.io[offset] &= !value,
                    6 | 7 | 0x130 | 0x131 => {}
                    4 => self.io[4] = (self.io[4] & 7) | (value & 0x38),
                    0x301 => self.halted = true,
                    _ => self.io[offset] = value,
                }
            }
            5 => self.palette[index & 0x3ff] = value,
            6 => self.vram[Self::vram_index(address)] = value,
            7 => self.oam[index & 0x3ff] = value,
            _ => {}
        }
    }
}
impl Memory for Bus {
    fn read8(&mut self, address: u32) -> u8 {
        self.charge(address, 1, false);
        self.peek8(address)
    }
    fn read16(&mut self, address: u32, sequential: bool) -> u16 {
        self.charge(address, 2, sequential);
        self.peek16(address & !1)
    }
    fn read32(&mut self, address: u32, sequential: bool) -> u32 {
        self.charge(address, 4, sequential);
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
    }
    fn write16(&mut self, address: u32, value: u16, sequential: bool) {
        self.charge(address, 2, sequential);
        let address = address & !1;
        for (i, byte) in value.to_le_bytes().into_iter().enumerate() {
            self.put8(address + i as u32, byte);
        }
    }
    fn write32(&mut self, address: u32, value: u32, sequential: bool) {
        self.charge(address, 4, sequential);
        let address = address & !3;
        for (i, byte) in value.to_le_bytes().into_iter().enumerate() {
            self.put8(address + i as u32, byte);
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
    fn ram_mirrors_and_alignment() {
        let mut b = bus();
        b.write32(0x02000003, 0x12345678, false);
        assert_eq!(b.peek32(0x02040000), 0x12345678);
        b.write16(0x03008000, 0xabcd, false);
        assert_eq!(b.peek16(0x03000000), 0xabcd);
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

//! GBA address decoding and instruction-granular wait states. No BIOS is bundled.
use crate::cpu::Memory;
use crate::peripherals::{Dma, Timer};

pub struct Bus {
    pub(crate) bios: Vec<u8>,
    pub(crate) replacement_bios: bool,
    pub(crate) bios_access: bool,
    pub(crate) bios_latch: u32,
    pub(crate) prefetch: crate::prefetch::Prefetch,
    // Intra-instruction timing scratch, consumed before a snapshot boundary.
    gamepak_instruction: bool,
    restart_fetch: bool,
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
            bios_access: !replacement_bios,
            bios_latch: 0,
            prefetch: crate::prefetch::Prefetch::default(),
            gamepak_instruction: false,
            restart_fetch: false,
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
    /// Non-mutating debugger read: deliberately bypasses CPU BIOS protection.
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
                    || matches!(offset, 0x206 | 0x207)
                {
                    0
                } else if offset == 0x205 {
                    self.io[offset] & 0x5f
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
            5..=6 => {
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
                // Cartridge /CS restarts at each 128-KiB page even when the
                // CPU/DMA marks this access sequential. Align to the physical
                // bus access before testing the boundary.
                let aligned = address & if width == 4 { !3 } else { !1 };
                let sequential = sequential && aligned & 0x1ffff != 0;
                (if sequential { second } else { first }) + if width == 4 { second } else { 0 }
            }
            14 | 15 => [4, 3, 2, 8][(self.io16(0x204) & 3) as usize] + 1,
            _ => 1,
        };
        self.clocks += clocks;
        if (8..=15).contains(&(address >> 24)) {
            // Data accesses share the cartridge bus; queued opcode timing
            // cannot survive a ROM, EEPROM or backup-memory transaction.
            self.prefetch = crate::prefetch::Prefetch::default();
        } else if self.io16(0x204) & 0x4000 != 0 {
            self.prefetch.idle(clocks as u32, self.io16(0x204));
        }
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
                    0x205 => self.io[offset] = value & 0x5f,
                    0x206 | 0x207 => {}
                    _ => self.io[offset] = value,
                }
                self.peripheral_write(offset, old);
                self.sound_write(offset, old);
                self.affine_write(offset);
                if matches!(offset, 0x204 | 0x205) && self.io[offset] != old {
                    self.prefetch = crate::prefetch::Prefetch::default();
                }
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
    fn instruction_context(&mut self, address: u32, thumb: bool) {
        self.gamepak_instruction = (8..=13).contains(&(address >> 24));
        if !self.gamepak_instruction {
            self.prefetch = crate::prefetch::Prefetch::default();
        }
        self.restart_fetch = false;
        self.bios_access = address < 0x4000;
        if self.bios_access {
            // The interpreter represents the visible ARM/Thumb prefetch without
            // yet maintaining a real pipeline queue. Data loads do not replace
            // the last fetched opcode latch.
            let ahead = address.wrapping_add(if thumb { 4 } else { 8 });
            let fetched = if ahead < 0x4000 { ahead } else { address };
            self.bios_latch = self.peek32(fetched & !3);
        }
    }
    fn refill_complete(&mut self, address: u32) {
        // The refill has already performed its N+S accesses. Internal cycles
        // before a PC write must not restart the new instruction stream.
        self.restart_fetch = false;
        if address < 0x4000 {
            self.bios_latch = self.peek32(address & !3);
        }
    }
    fn finish_instruction(&mut self) -> bool {
        !std::mem::take(&mut self.restart_fetch)
    }
    fn fetch16(&mut self, address: u32, sequential: bool) -> u16 {
        if (8..=13).contains(&(address >> 24))
            && self.io16(0x204) & 0x4000 != 0
            && !self.eeprom_address(address)
        {
            self.clocks +=
                u64::from(
                    self.prefetch
                        .fetch(address & !1, 1, sequential, self.io16(0x204)),
                );
            return self.peek16(address & !1);
        }
        self.prefetch = crate::prefetch::Prefetch::default();
        if address < 0x4000 {
            self.charge(address, 2, sequential);
            self.peek16(address & !1)
        } else {
            self.read16(address, sequential)
        }
    }
    fn fetch32(&mut self, address: u32, sequential: bool) -> u32 {
        if (8..=13).contains(&(address >> 24))
            && self.io16(0x204) & 0x4000 != 0
            && !self.eeprom_address(address)
        {
            self.clocks +=
                u64::from(
                    self.prefetch
                        .fetch(address & !3, 2, sequential, self.io16(0x204)),
                );
            return self.peek32(address & !3);
        }
        self.prefetch = crate::prefetch::Prefetch::default();
        if address < 0x4000 {
            self.charge(address, 4, sequential);
            self.peek32(address & !3)
        } else {
            self.read32(address, sequential)
        }
    }
    fn read8(&mut self, address: u32) -> u8 {
        self.charge(address, 1, false);
        if address < 0x4000 && !self.bios_access {
            return (self.bios_latch >> ((address & 3) * 8)) as u8;
        }
        self.flush_sound_read(address);
        self.peek8(address)
    }
    fn read16(&mut self, address: u32, sequential: bool) -> u16 {
        self.charge(address, 2, sequential);
        if address < 0x4000 && !self.bios_access {
            return (self.bios_latch >> ((address & 2) * 8)) as u16;
        }
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
        if address < 0x4000 && !self.bios_access {
            return self.bios_latch;
        }
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
        if self.io16(0x204) & 0x4000 != 0 {
            self.prefetch.idle(clocks, self.io16(0x204));
        }
        // GBATEK's prefetch-disable bug: ROM opcodes with internal cycles
        // turn the subsequent opcode S access into N. Enabled-prefetch queue
        // timing is a separate, still pending feature.
        if clocks != 0 && self.gamepak_instruction && self.io16(0x204) & 0x4000 == 0 {
            self.restart_fetch = true;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn bus() -> Bus {
        Bus::new(vec![0; 0xc0], None).unwrap()
    }
    #[test]
    fn prefetch_uses_idle_bus_clocks_but_not_cartridge_data_accesses() {
        let mut b = bus();
        b.write16(0x04000204, 0x4000, false);
        b.take_cycles();
        assert_eq!(b.fetch16(0x08000000, false), 0);
        assert_eq!(b.take_cycles(), 5);
        b.read32(0x02000000, false); // Six idle Game Pak clocks: two halfwords.
        assert_eq!((b.prefetch.count, b.prefetch.elapsed), (2, 0));
        b.take_cycles();
        b.fetch32(0x08000002, true); // Physical word alignment is 08000000.
        assert_eq!(b.take_cycles(), 8); // Discontinuity, including page restart.
        b.internal(30);
        assert_eq!(b.prefetch.count, 8);
        b.take_cycles();
        b.fetch32(0x08000004, true);
        assert_eq!(b.take_cycles(), 1);
        b.read16(0x08000040, false);
        assert_eq!(b.prefetch, crate::prefetch::Prefetch::default());
        b.take_cycles();
        b.fetch16(0x08000008, true);
        assert_eq!(b.take_cycles(), 3);
        b.internal(8);
        b.write8(0x04000205, 0); // Disable via the high WAITCNT byte.
        assert_eq!(b.prefetch, crate::prefetch::Prefetch::default());
    }
    #[test]
    fn arm_internal_cycles_restart_rom_fetch_only_when_prefetch_is_disabled() {
        use crate::cpu::Cpu;
        for op in [
            0xe0000190u32,
            0xe5920000,
            0xe1a00110,
            0xe1020091,
            0xe8920003,
        ] {
            for enabled in [false, true] {
                let mut b = bus();
                b.rom[..4].copy_from_slice(&op.to_le_bytes());
                b.rom[4..8].copy_from_slice(&0xe1a00000u32.to_le_bytes());
                b.set_io(0x204, if enabled { 0x4000 } else { 0 });
                let mut c = Cpu::default();
                c.set_cpsr(0x1f).unwrap();
                c.r[0] = 2;
                c.r[1] = 3;
                c.r[2] = 0x03000000;
                c.r[15] = 0x08000000;
                c.step(&mut b);
                assert_eq!(c.sequential, enabled, "{op:08x}");
                b.take_cycles();
                c.step(&mut b);
                // With prefetch enabled, I cycles and RAM bus cycles fill the
                // following opcode: MUL/shift supply 1 clock, LDR 2, SWP/LDM 3.
                let buffered = match op {
                    0xe0000190 | 0xe1a00110 => 5,
                    0xe5920000 => 4,
                    _ => 3,
                };
                assert_eq!(
                    b.take_cycles(),
                    if enabled { buffered } else { 8 },
                    "{op:08x}"
                );
                assert!(c.sequential); // The next plain opcode has no I cycle.
            }
        }
    }
    #[test]
    fn thumb_internal_cycles_restart_rom_fetch_but_ram_code_is_unaffected() {
        use crate::cpu::Cpu;
        for op in [0x4348u16, 0x6808, 0x4088, 0xca03, 0xbc01] {
            for location in [0x08000000, 0x03000000] {
                for enabled in [false, true] {
                    let mut b = bus();
                    let code = if location == 0x08000000 {
                        &mut b.rom
                    } else {
                        &mut b.iwram
                    };
                    code[..2].copy_from_slice(&op.to_le_bytes());
                    code[2..4].copy_from_slice(&0x46c0u16.to_le_bytes());
                    b.set_io(0x204, if enabled { 0x4000 } else { 0 });
                    let mut c = Cpu::default();
                    c.set_cpsr(0x3f).unwrap();
                    c.r[0] = 2;
                    c.r[1] = 0x03000100;
                    c.r[2] = 0x03000100;
                    c.r[13] = 0x03000100;
                    c.r[15] = location;
                    c.step(&mut b);
                    assert_eq!(c.sequential, enabled || location == 0x03000000);
                    b.take_cycles();
                    c.step(&mut b);
                    let expected = if location == 0x03000000 {
                        1
                    } else if enabled {
                        if op == 0x4088 {
                            2
                        } else {
                            1
                        }
                    } else {
                        5
                    };
                    assert_eq!(b.take_cycles(), expected, "{op:04x}");
                    assert!(c.sequential);
                }
            }
        }
    }
    #[test]
    fn rom_refills_and_failed_conditions_do_not_inherit_internal_restart() {
        use crate::cpu::Cpu;
        let mut b = bus();
        b.rom[..4].copy_from_slice(&0xe592f000u32.to_le_bytes()); // LDR pc,[r2]
        b.rom[0x40..0x44].copy_from_slice(&0xe1a00000u32.to_le_bytes());
        b.iwram[..4].copy_from_slice(&0x08000040u32.to_le_bytes());
        let mut c = Cpu::default();
        c.set_cpsr(0x1f).unwrap();
        c.r[2] = 0x03000000;
        c.r[15] = 0x08000000;
        c.step(&mut b);
        assert_eq!(c.r[15], 0x08000040);
        assert!(c.sequential);
        b.take_cycles();
        c.step(&mut b);
        assert_eq!(b.take_cycles(), 6);
        b.rom[0x44..0x48].copy_from_slice(&0x00000190u32.to_le_bytes()); // MULEQ, Z clear
        c.step(&mut b);
        assert!(c.sequential);
        assert_eq!(b.take_cycles(), 6);
    }
    #[test]
    fn thumb_pop_pc_refill_clears_the_load_internal_cycle_restart() {
        use crate::cpu::Cpu;
        let mut b = bus();
        b.rom[..2].copy_from_slice(&0xbd00u16.to_le_bytes()); // POP {pc}
        b.rom[0x40..0x42].copy_from_slice(&0x46c0u16.to_le_bytes());
        b.iwram[..4].copy_from_slice(&0x08000041u32.to_le_bytes());
        let mut c = Cpu::default();
        c.set_cpsr(0x3f).unwrap();
        c.r[13] = 0x03000000;
        c.r[15] = 0x08000000;
        c.step(&mut b);
        assert_eq!(c.r[15], 0x08000040);
        assert!(c.sequential);
        b.take_cycles();
        c.step(&mut b);
        assert_eq!(b.take_cycles(), 3);
    }
    #[test]
    fn bios_protection_uses_opcode_lanes_and_debugger_peeks_do_not_change_it() {
        let mut b = bus();
        b.bios[8..12].copy_from_slice(&0x12345678u32.to_le_bytes());
        b.bios[32..36].copy_from_slice(&0xabcdef01u32.to_le_bytes());
        b.instruction_context(0, false);
        assert_eq!(b.read32(32, false), 0xabcdef01);
        assert_eq!(b.bios_latch, 0x12345678); // Data isn't an opcode fetch.
        b.instruction_context(0x08000000, false);
        assert_eq!(b.read32(32, false), 0x12345678);
        assert_eq!(b.read16(34, false), 0x1234);
        assert_eq!(b.read16(33, false), 0x5678);
        for (n, expected) in [0x78, 0x56, 0x34, 0x12].into_iter().enumerate() {
            assert_eq!(b.read8(32 + n as u32), expected);
        }
        assert_eq!(b.peek32(32), 0xabcdef01);
        assert_eq!(b.bios_latch, 0x12345678);
        assert!(!b.bios_access);
        assert_eq!(b.fetch32(32, false), 0xabcdef01);
        b.refill_complete(32);
        assert_eq!(b.bios_latch, 0xabcdef01);
        b.instruction_context(0x08000000, true);
        assert_eq!(b.read32(0, false), 0xabcdef01);
    }
    #[test]
    fn thumb_bios_prefetch_and_end_of_firmware_do_not_mirror() {
        let mut b = bus();
        b.bios[4..8].copy_from_slice(&0x12345678u32.to_le_bytes());
        b.bios[0x3ffc..].copy_from_slice(&0xabcdef01u32.to_le_bytes());
        b.instruction_context(2, true);
        assert_eq!(b.bios_latch, 0x12345678);
        b.instruction_context(0x3ffe, true);
        assert_eq!(b.bios_latch, 0xabcdef01);
        b.instruction_context(0x4000, true);
        assert_eq!(b.read32(0x3ffc, false), 0xabcdef01);
        assert_eq!(b.peek32(0x4000), 0); // Unused area, not a BIOS mirror.
    }
    #[test]
    fn dma_does_not_inherit_firmware_cpu_read_permissions() {
        let mut b = bus();
        b.bios[0..4].copy_from_slice(&0xdeadbeefu32.to_le_bytes());
        b.bios_access = true;
        b.bios_latch = 0x12345678;
        b.write32(0x040000d4, 0, false);
        b.write32(0x040000d8, 0x02000000, false);
        b.write32(0x040000dc, 0x84000001, false);
        assert!(b.dma_unit());
        assert_eq!(b.peek32(0x02000000), 0x12345678);
        assert!(b.bios_access);
        assert_eq!(b.bios_latch, 0x12345678);
        assert_eq!(b.peek32(0), 0xdeadbeef);
    }
    #[test]
    fn gamepak_boundaries_restart_nonsequential_in_all_wait_areas() {
        let mut b = bus();
        for area in 0..3u32 {
            let base = 0x08000000 + (area << 25);
            let sequential_cost = [3, 5, 9][area as usize];
            for boundary in [0, 0x20000, 0x1000000, 0x1fe0000] {
                b.read16(base + boundary, true);
                assert_eq!(b.take_cycles(), 5, "area {area} boundary {boundary:x}");
                b.read32(base + boundary, true);
                assert_eq!(b.take_cycles(), 5 + sequential_cost);
                b.read16(base + boundary + 2, true);
                assert_eq!(b.take_cycles(), sequential_cost);
            }
        }
    }
    #[test]
    fn oam_has_a_32_bit_bus_unlike_palette_and_video_ram() {
        let mut b = bus();
        for (base, word_cost) in [(0x05000000, 2), (0x06000000, 2), (0x07000000, 1)] {
            b.write32(base, 0x12345678, false);
            assert_eq!(b.take_cycles(), word_cost);
            assert_eq!(b.read32(base, false), 0x12345678);
            assert_eq!(b.take_cycles(), word_cost);
            b.read16(base, false);
            assert_eq!(b.take_cycles(), 1);
        }
    }
    #[test]
    fn waitcnt_reserved_bits_and_gamepak_type_are_not_writable() {
        let mut b = bus();
        b.write32(0x04000204, u32::MAX, false);
        assert_eq!(b.peek32(0x04000204), 0x5fff);
        b.write8(0x04000205, 0xff);
        assert_eq!(b.peek16(0x04000204), 0x5fff);
    }
    #[test]
    fn all_waitcnt_selections_keep_boundary_and_bus_width_costs() {
        let mut b = bus();
        for area in 0..3u32 {
            for first_setting in 0..4u16 {
                for second_setting in 0..2u16 {
                    let wait =
                        (first_setting << (2 + area * 3)) | (second_setting << (4 + area * 3));
                    b.write16(0x04000204, wait, false);
                    b.take_cycles();
                    let first = [5, 4, 3, 9][first_setting as usize];
                    let second = if second_setting == 1 {
                        2
                    } else {
                        [3, 5, 9][area as usize]
                    };
                    let base = 0x08020000 + (area << 25);
                    for sequential in [false, true] {
                        for width in [1, 2, 4] {
                            for offset in [0, 1, 2, 3, 4, 8] {
                                let address = base + offset;
                                let alignment = if width == 4 { 3 } else { 1 };
                                let first_part = if sequential && offset & !alignment != 0 {
                                    second
                                } else {
                                    first
                                };
                                b.charge(address, width, sequential);
                                assert_eq!(
                                    b.take_cycles(),
                                    first_part + if width == 4 { second } else { 0 }
                                );
                            }
                        }
                    }
                }
            }
        }
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

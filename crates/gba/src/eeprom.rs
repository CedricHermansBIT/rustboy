//! Original GBA serial EEPROM, from its documented MSB-first command format.
//! DMA3 command length identifies 6/14 address bits; this is an emulator-side
//! inference, not a physical chip autodetection protocol. Nominal write time is
//! fixed here; real chip variation and sub-access timing remain unverified.
use crate::backup::{Backup, Kind};
impl crate::bus::Bus {
    pub(crate) fn eeprom_address(&self, address: u32) -> bool {
        self.backup.kind == Kind::Eeprom
            && address >> 24 == 13
            && (self.rom.len() <= 0x1000000 || address & 0xffffff >= 0xffff00)
    }
}
pub(crate) const WRITE_CLOCKS: u64 = 108368;
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct Eeprom {
    pub address_bits: u8,
    pub input: [u8; 81],
    pub input_len: u8,
    pub expected: u8,
    pub output: u64,
    pub output_left: u8,
    pub busy: u64,
    pub pending_address: u16,
    pub pending_data: u64,
}
impl Default for Eeprom {
    fn default() -> Self {
        Self {
            address_bits: 0,
            input: [0; 81],
            input_len: 0,
            expected: 0,
            output: 0,
            output_left: 0,
            busy: 0,
            pending_address: 0,
            pending_data: 0,
        }
    }
}
impl Eeprom {
    pub fn peek(&self) -> u16 {
        if self.busy != 0 || self.output_left > 64 {
            0
        } else if self.output_left > 0 {
            (self.output >> 63) as u16
        } else {
            1
        }
    }
    pub fn read(&mut self) -> u16 {
        let bit = self.peek();
        if self.busy == 0 && self.output_left > 0 {
            if self.output_left <= 64 {
                self.output <<= 1;
            }
            self.output_left -= 1;
        }
        bit
    }
    pub fn valid(&self, size: usize) -> bool {
        let size_valid = match self.address_bits {
            0 | 14 => size == 8192,
            6 => size == 512,
            _ => false,
        };
        size_valid
            && self.input.iter().all(|b| *b <= 1)
            && self.input_len <= self.expected
            && (self.expected == 0
                || self.address_bits != 0
                    && (self.expected == self.address_bits + 3
                        || self.expected == self.address_bits + 67))
            && self.output_left <= 68
            && self.busy <= WRITE_CLOCKS
            && usize::from(self.pending_address) * 8 < size
            && !(self.busy != 0 && self.output_left != 0)
            && !(self.address_bits == 0
                && (self.expected != 0 || self.output_left != 0 || self.busy != 0))
    }
}
impl Backup {
    pub fn begin_eeprom(&mut self, count: u32) {
        if self.kind != Kind::Eeprom || self.eeprom.busy != 0 {
            return;
        }
        self.eeprom.input_len = 0;
        self.eeprom.expected = 0;
        let bits = match count {
            9 | 73 => 6,
            17 | 81 => 14,
            _ => return,
        };
        if self.eeprom.address_bits != 0 && self.eeprom.address_bits != bits {
            return;
        }
        self.eeprom.address_bits = bits;
        self.data.resize(if bits == 6 { 512 } else { 8192 }, 255);
        self.eeprom.input.fill(0);
        self.eeprom.expected = count as u8;
        self.eeprom.output_left = 0;
    }
    pub fn eeprom_bit(&mut self, value: u16) {
        let e = &mut self.eeprom;
        if self.kind != Kind::Eeprom || e.busy != 0 || e.expected == 0 {
            return;
        }
        e.input[usize::from(e.input_len)] = (value & 1) as u8;
        e.input_len += 1;
        if e.input_len != e.expected {
            return;
        }
        let count = e.expected;
        e.input_len = 0;
        e.expected = 0;
        let bits = usize::from(e.address_bits);
        if e.input[0] != 1 || e.input[usize::from(count) - 1] != 0 {
            return;
        }
        let read = e.input[1] == 1;
        if usize::from(count) != bits + if read { 3 } else { 67 } {
            return;
        }
        let mut address = 0u16;
        for bit in &e.input[2..2 + bits] {
            address = (address << 1) | u16::from(*bit);
        }
        address &= if bits == 6 { 63 } else { 1023 };
        if read {
            let start = usize::from(address) * 8;
            e.output = u64::from_be_bytes(self.data[start..start + 8].try_into().unwrap());
            e.output_left = 68;
        } else {
            let mut data = 0u64;
            for bit in &e.input[2 + bits..2 + bits + 64] {
                data = (data << 1) | u64::from(*bit);
            }
            e.pending_address = address;
            e.pending_data = data;
            e.busy = WRITE_CLOCKS;
        }
    }
    pub fn clock_eeprom(&mut self, clocks: u64) {
        if self.eeprom.busy == 0 {
            return;
        }
        self.eeprom.busy = self.eeprom.busy.saturating_sub(clocks);
        if self.eeprom.busy == 0 {
            let start = usize::from(self.eeprom.pending_address) * 8;
            let bytes = self.eeprom.pending_data.to_be_bytes();
            self.dirty |= self.data[start..start + 8] != bytes;
            self.data[start..start + 8].copy_from_slice(&bytes);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn command(b: &mut Backup, bits: u8, read: bool, address: u16, data: u64) {
        let count = u32::from(bits) + if read { 3 } else { 67 };
        b.begin_eeprom(count);
        b.eeprom_bit(1);
        b.eeprom_bit(u16::from(read));
        for n in (0..bits).rev() {
            b.eeprom_bit(address >> n);
        }
        if !read {
            for n in (0..64).rev() {
                b.eeprom_bit((data >> n) as u16);
            }
        }
        b.eeprom_bit(0);
    }
    #[test]
    fn both_geometries_write_busy_and_msb_first_read() {
        for bits in [6, 14] {
            let mut b = Backup::detect(b"EEPROM_V124");
            let address = if bits == 6 { 63 } else { 1023 };
            let value = 0x807f0123456789ab;
            command(&mut b, bits, false, address, value);
            assert_eq!(b.eeprom.peek(), 0);
            assert!(b.data.iter().all(|v| *v == 255));
            assert!(!b.dirty);
            b.clock_eeprom(WRITE_CLOCKS - 1);
            assert_eq!(b.eeprom.peek(), 0);
            b.clock_eeprom(1);
            assert_eq!(b.eeprom.peek(), 1);
            assert!(b.dirty);
            command(&mut b, bits, true, address, 0);
            for _ in 0..4 {
                assert_eq!(b.eeprom.read(), 0);
            }
            let mut got = 0;
            for _ in 0..64 {
                got = (got << 1) | u64::from(b.eeprom.read());
            }
            assert_eq!(got, value);
            assert_eq!(b.eeprom.read(), 1);
            assert!(b.eeprom.valid(b.data.len()));
        }
    }
    #[test]
    fn write_replaces_bits_and_busy_rejects_intervening_commands() {
        let mut b = Backup::detect(b"EEPROM_V124");
        command(&mut b, 6, false, 7, 0);
        command(&mut b, 6, true, 0, 0);
        assert_eq!(b.eeprom.output_left, 0);
        b.clock_eeprom(WRITE_CLOCKS);
        b.dirty = false;
        command(&mut b, 6, false, 7, u64::MAX);
        b.clock_eeprom(WRITE_CLOCKS);
        assert_eq!(&b.data[56..64], &[255; 8]);
        assert!(b.dirty);
        b.dirty = false;
        command(&mut b, 6, false, 7, u64::MAX);
        b.clock_eeprom(WRITE_CLOCKS);
        assert!(!b.dirty);
    }
    #[test]
    fn invalid_lengths_headers_stop_bits_and_size_changes_are_safe() {
        let mut b = Backup::detect(b"EEPROM_V124");
        for count in [0, 8, 10, 68, 72, 74, 82, u32::MAX] {
            b.begin_eeprom(count);
            for _ in 0..100 {
                b.eeprom_bit(0);
            }
            assert_eq!(b.eeprom.busy, 0);
        }
        b.begin_eeprom(73);
        for _ in 0..73 {
            b.eeprom_bit(0);
        }
        assert_eq!(b.eeprom.busy, 0);
        b.begin_eeprom(73);
        b.eeprom_bit(1);
        for _ in 0..71 {
            b.eeprom_bit(0);
        }
        b.eeprom_bit(1);
        assert_eq!(b.eeprom.busy, 0);
        b.begin_eeprom(81);
        assert_eq!(b.eeprom.expected, 0);
        assert_eq!(b.data.len(), 512);
        assert!(b.data.iter().all(|v| *v == 255));
    }
    #[test]
    fn large_addresses_mask_unused_upper_bits_and_peek_does_not_consume() {
        let mut b = Backup::detect(b"EEPROM_V124");
        command(&mut b, 14, false, 0x3fff, 0x1122334455667788);
        b.clock_eeprom(WRITE_CLOCKS);
        assert_eq!(&b.data[8184..], &0x1122334455667788u64.to_be_bytes());
        command(&mut b, 14, true, 1023, 0);
        for _ in 0..100 {
            assert_eq!(b.eeprom.peek(), 0);
        }
        assert_eq!(b.eeprom.output_left, 68);
    }
}

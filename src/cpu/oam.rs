//! DMG OAM bus corruption. See Pan Docs' OAM Corruption Bug specification.
use super::CPU;

#[derive(Clone, Copy)]
pub(super) enum OamAccess { Read, Write, ReadIncrement }

impl CPU {
    pub(super) fn corrupt_oam(&mut self, address: usize, access: OamAccess) {
        if self.is_cgb || !(0xFE00..=0xFEFF).contains(&address)
            || self.memory[0xFF40] & 0x80 == 0 || self.scanline >= 144
            || self.ppu_first_line_after_enable || !(4..80).contains(&self.ppu_scanline_dot)
            || self.oam_dma_active {
            return;
        }
        // The scanner advances to the next row as the CPU bus cycle completes.
        let row = self.ppu_scanline_dot as usize / 4;
        if row == 0 || row >= 20 { return; }
        let base = 0xFE00 + row * 8;
        if matches!(access, OamAccess::ReadIncrement) && (4..19).contains(&row) {
            let a = self.oam_word(base - 16);
            let b = self.oam_word(base - 8);
            let c = self.oam_word(base);
            let d = self.oam_word(base - 4);
            self.set_oam_word(base - 8, (b & (a | c | d)) | (a & c & d));
            let previous: [u8; 8] = self.memory[base - 8..base].try_into().unwrap();
            self.memory[base..base + 8].copy_from_slice(&previous);
            self.memory[base - 16..base - 8].copy_from_slice(&previous);
        }
        let a = self.oam_word(base);
        let b = self.oam_word(base - 8);
        let c = self.oam_word(base - 4);
        let first = match access {
            OamAccess::Write => ((a ^ c) & (b ^ c)) ^ c,
            OamAccess::Read | OamAccess::ReadIncrement => b | (a & c),
        };
        self.set_oam_word(base, first);
        let previous: [u8; 6] = self.memory[base - 6..base].try_into().unwrap();
        self.memory[base + 2..base + 8].copy_from_slice(&previous);
    }

    fn oam_word(&self, address: usize) -> u16 {
        u16::from_le_bytes([self.memory[address], self.memory[address + 1]])
    }

    fn set_oam_word(&mut self, address: usize, value: u16) {
        self.memory[address..address + 2].copy_from_slice(&value.to_le_bytes());
    }

    pub(super) fn read_byte_increment(&mut self, address: usize) -> u8 {
        self.tick_timer_4t();
        self.corrupt_oam(address, OamAccess::ReadIncrement);
        self.read_bus_byte(address)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn scanning_cpu(dot: u16) -> CPU {
        let mut cpu = CPU::new();
        cpu.memory[0xFF40] = 0x80;
        cpu.scanline = 1;
        cpu.ppu_first_line_after_enable = false;
        cpu.ppu_scanline_dot = dot;
        for index in 0..160 { cpu.memory[0xFE00 + index] = index as u8; }
        cpu
    }

    #[test]
    fn oam_write_uses_scanned_row_not_cpu_address() {
        let mut cpu = scanning_cpu(4);
        cpu.corrupt_oam(0xFEFF, OamAccess::Write);
        assert_eq!(&cpu.memory[0xFE0A..0xFE10], &[2, 3, 4, 5, 6, 7]);
        assert_eq!(&cpu.memory[0xFE00..0xFE08], &[0, 1, 2, 3, 4, 5, 6, 7]);
    }

    #[test]
    fn oam_corruption_covers_exactly_nineteen_rows() {
        for dot in (0..=84).step_by(4) {
            let mut cpu = scanning_cpu(dot);
            let before = cpu.memory[0xFE00..0xFEA0].to_vec();
            cpu.corrupt_oam(0xFE00, OamAccess::Write);
            assert_eq!(cpu.memory[0xFE00..0xFEA0] != before, (4..80).contains(&dot), "dot={dot}");
        }
    }

    #[test]
    fn oam_corruption_is_absent_on_cgb_and_with_lcd_off() {
        for cgb in [false, true] {
            let mut cpu = scanning_cpu(24);
            cpu.is_cgb = cgb;
            if !cgb { cpu.memory[0xFF40] = 0; }
            let before = cpu.memory[0xFE00..0xFEA0].to_vec();
            cpu.corrupt_oam(0xFE00, OamAccess::ReadIncrement);
            assert_eq!(cpu.memory[0xFE00..0xFEA0], before);
        }
    }
}

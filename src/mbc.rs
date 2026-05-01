/// src/mbc.rs — Memory Bank Controller implementations.
///
/// Each cartridge type gets its own variant in `MbcKind` which carries the
/// variant-specific runtime state (bank registers, RAM-enable flag, RTC, …).
/// `Mbc::read` / `Mbc::write` dispatch to the correct implementation with a
/// single match — no repeated `cartridge_type` checks in the hot path.
///
/// # Address routing (caller's responsibility)
/// The CPU calls `mbc.read(addr)` for:
///   - 0x0000..=0x7FFF  ROM (and ROM-register writes)
///   - 0xA000..=0xBFFF  External cartridge RAM
/// Everything else (VRAM, WRAM, I/O, …) is handled by the CPU directly.

// ──────────────────────────────────────────────────────────────────────────────
// MbcKind — per-variant state
// ──────────────────────────────────────────────────────────────────────────────

/// Runtime state that differs between MBC variants.
/// All inner fields are `Copy` primitives so `match self.kind { Variant { a, b } => … }`
/// binds by value without needing `ref` patterns.
#[derive(Debug, Clone, Copy)]
pub enum MbcKind {
    /// 0x00 – ROM only, no banking.
    None,

    /// 0x01 = MBC1, 0x02 = MBC1+RAM, 0x03 = MBC1+RAM+BATTERY
    Mbc1 {
        /// Upper 2-bit bank register (written to 0x4000..=0x5FFF).
        bank2: u8,
        /// Banking mode: 0 = ROM-banking, 1 = RAM-banking.
        mode: u8,
        ram_enable: bool,
        /// True when this is a multicart (MBC1M): uses a 4-bit lower bank
        /// instead of the standard 5-bit, with bank2 shifting by 4 not 5.
        /// Detected at load time by checking for a Nintendo logo in the
        /// second game slot (ROM bank 0x10, offset 0x40104).
        multicart: bool,
    },

    /// 0x05 = MBC2, 0x06 = MBC2+BATTERY  (512 × 4-bit built-in nibble RAM)
    Mbc2 { ram_enable: bool },

    /// 0x0F..=0x13 – MBC3 (with optional RTC clock hardware)
    Mbc3 { ram_enable: bool },

    /// 0x19..=0x1E – MBC5 (up to 64 Mbit ROM, 1 Mbit RAM, optional rumble)
    Mbc5 { ram_enable: bool },

    /// Catch-all for any unimplemented MBC type.
    Unknown(u8),
}

// ──────────────────────────────────────────────────────────────────────────────
// Mbc — the top-level struct held by CPU
// ──────────────────────────────────────────────────────────────────────────────

pub struct Mbc {
    pub kind: MbcKind,
    /// Cartridge ROM bytes (the full file as loaded).
    pub rom: Vec<u8>,
    /// External RAM: 16 banks × 8 KiB.
    /// MBC2 uses `ram[0][0..=0x1FF]` as 512 nibbles.
    pub ram: [[u8; 0x2000]; 16],
    /// Currently selected ROM bank for the 0x4000..=0x7FFF window.
    pub rombank: u16,
    /// Currently selected RAM bank (or RTC register index for MBC3).
    pub rambank: u8,
    /// Bitmask applied to ROM bank numbers to handle power-of-two sizing.
    pub rom_bank_mask: u16,
    /// Bitmask applied to RAM bank numbers.
    pub ram_bank_mask: usize,
    /// Raw cartridge-type byte from header offset 0x147.
    pub cart_type: u8,
    /// RTC registers (MBC3): [S, M, H, DL, DH] – present for all MBC3 carts.
    pub rtc_registers: [u8; 5],
    /// True when the cartridge has a battery-backed save (set at init time).
    pub has_battery: bool,
}

impl Mbc {
    // ─── Construction ────────────────────────────────────────────────────────

    /// Build an `Mbc` for `rom`, using `ram_size_bytes` (pre-calculated by the
    /// caller who may override the header value, e.g. for MBC2).
    pub fn new(rom: Vec<u8>, ram_size_bytes: usize) -> Self {
        let cart_type = *rom.get(0x147).unwrap_or(&0);

        let num_rom_banks = (rom.len() / 0x4000).max(2) as u16;
        let rom_bank_mask = num_rom_banks.next_power_of_two() - 1;

        let num_ram_banks = if ram_size_bytes == 0 { 0 } else { (ram_size_bytes / 0x2000).max(1) };
        let ram_bank_mask = if num_ram_banks > 0 { num_ram_banks.next_power_of_two() - 1 } else { 0 };

        let kind = match cart_type {
            0x00                => MbcKind::None,
            0x01..=0x03         => MbcKind::Mbc1 {
                bank2: 0, mode: 0, ram_enable: false,
                multicart: Self::detect_mbc1_multicart(&rom),
            },
            0x05..=0x06         => MbcKind::Mbc2 { ram_enable: false },
            0x0F..=0x13         => MbcKind::Mbc3 { ram_enable: false },
            0x19..=0x1E         => MbcKind::Mbc5 { ram_enable: false },
            other               => MbcKind::Unknown(other),
        };

        let has_battery = matches!(
            cart_type,
            0x03 | 0x06 | 0x09 | 0x0D | 0x0F | 0x10 | 0x13 | 0x1B | 0x1E
        );

        Mbc {
            kind,
            rom,
            ram: [[0; 0x2000]; 16],
            rombank: 1,
            rambank: 0,
            rom_bank_mask,
            ram_bank_mask,
            cart_type,
            rtc_registers: [0; 5],
            has_battery,
        }
    }

    /// An empty/no-cart Mbc used before any ROM is loaded.
    pub fn empty() -> Self {
        Mbc {
            kind: MbcKind::None,            rom: Vec::new(),
            ram: [[0; 0x2000]; 16],
            rombank: 1,
            rambank: 0,
            rom_bank_mask: 1,
            ram_bank_mask: 0,
            cart_type: 0,
            rtc_registers: [0; 5],
            has_battery: false,
        }
    }

    // ─── Read / Write dispatch ────────────────────────────────────────────────

    /// Read a byte from cartridge address space.
    /// Called for addresses in 0x0000..=0x7FFF and 0xA000..=0xBFFF.
    #[inline]
    pub fn read(&self, address: usize) -> u8 {
        match self.kind {
            MbcKind::None => {
                self.rom.get(address).copied().unwrap_or(0xFF)
            }
            MbcKind::Mbc1 { bank2, mode, ram_enable, multicart } => {
                self.mbc1_read(address, bank2, mode, ram_enable, multicart)
            }
            MbcKind::Mbc2 { ram_enable } => {
                self.mbc2_read(address, ram_enable)
            }
            MbcKind::Mbc3 { ram_enable } => {
                self.mbc3_read(address, ram_enable)
            }
            MbcKind::Mbc5 { ram_enable } => {
                self.mbc5_read(address, ram_enable)
            }
            MbcKind::Unknown(_) => {
                // Best-effort: expose ROM, ignore RAM
                self.rom.get(address).copied().unwrap_or(0xFF)
            }
        }
    }

    /// Write a byte to cartridge address space (MBC register or external RAM).
    /// Called for addresses in 0x0000..=0x7FFF and 0xA000..=0xBFFF.
    #[inline]
    pub fn write(&mut self, address: usize, data: u8) {
        match self.kind {
            MbcKind::None         => { /* ROM-only carts ignore writes to ROM space */ }
            MbcKind::Mbc1 { .. }  => self.mbc1_write(address, data),
            MbcKind::Mbc2 { .. }  => self.mbc2_write(address, data),
            MbcKind::Mbc3 { .. }  => self.mbc3_write(address, data),
            MbcKind::Mbc5 { .. }  => self.mbc5_write(address, data),
            MbcKind::Unknown(_)   => { /* unknown MBC – treat as ROM only */ }
        }
    }

    // ─── Multicart detection ─────────────────────────────────────────────────

    /// Detect MBC1 multicart ("MBC1M") wiring.
    ///
    /// A multicart packs multiple Game Boy games onto one 8 Mbit (64-bank) ROM
    /// by treating bank2 as a game-slot selector and only using 4 bits for the
    /// in-game bank number (instead of the usual 5).  We detect it the same
    /// way Gambatte does: look for a valid Nintendo logo at the second slot's
    /// header location (bank 0x10, cartridge offset 0x40104).
    fn detect_mbc1_multicart(rom: &[u8]) -> bool {
        // Must be exactly 64 banks (1 MiB = 8 Mbit).
        if rom.len() < 64 * 0x4000 {
            return false;
        }
        // Nintendo logo bytes (48 bytes starting at 0x0104 in every valid GB header).
        const NINTENDO_LOGO: [u8; 48] = [
            0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B,
            0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D,
            0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E,
            0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99,
            0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC,
            0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E,
        ];
        // The second game starts at bank 0x10 (offset 0x40000); its header
        // logo lives 0x104 bytes in.
        let logo_offset = 0x40000 + 0x104;
        rom.get(logo_offset..logo_offset + 48)
            .map_or(false, |slice| slice == NINTENDO_LOGO)
    }

    // ─── MBC1 ────────────────────────────────────────────────────────────────
    //
    // Memory map:
    //   0x0000–0x3FFF  ROM Bank 0  (fixed; shifts with bank2 in RAM-banking mode for large ROMs)
    //   0x4000–0x7FFF  ROM Bank N  (lower bits from rombank, upper 2 from bank2)
    //   0xA000–0xBFFF  External RAM (up to 4 banks, selected by bank2 in RAM mode)
    //
    // Standard MBC1 (5-bit lower bank, bank = (bank2<<5) | lower5):
    //   0x2000–0x3FFF  ROM bank number (lower 5 bits; 0 → 1)
    //
    // MBC1M multicart (4-bit lower bank, bank = (bank2<<4) | lower4):
    //   The "bank2" register selects the game slot (each slot = 16 banks).
    //   0x2000–0x3FFF  ROM bank number (lower 4 bits; 0 → 1)
    //   Bank-0 window in mode 1 uses shift-4 as well.
    //
    // Register writes (both variants):
    //   0x0000–0x1FFF  RAM enable  (0x_A = enable)
    //   0x4000–0x5FFF  bank2 register (upper 2 bits for ROM or RAM bank)
    //   0x6000–0x7FFF  mode: 0 = ROM banking, 1 = RAM banking

    fn mbc1_read(&self, address: usize, bank2: u8, mode: u8, ram_enable: bool, multicart: bool) -> u8 {
        // For MBC1M the lower bank register is 4-bit; for standard MBC1 it's 5-bit.
        let shift = if multicart { 4usize } else { 5usize };
        // Minimum mask value where bank2 makes a difference for the bank-0 window.
        let bank2_threshold = 1u16 << shift;

        if address < 0x4000 {
            // In RAM-banking mode with a large enough ROM, bank2 shifts the
            // base address of the bank-0 window (allows accessing e.g. bank
            // 0x20/0x40/0x60 for standard MBC1, or slot start for MBC1M).
            // MBC1M multicarts keep this window fixed at bank 0.
            if !multicart && mode == 1 && self.rom_bank_mask >= bank2_threshold {
                let bank = ((bank2 as usize) << shift) & (self.rom_bank_mask as usize);
                return self.rom.get(address + bank * 0x4000).copied().unwrap_or(0xFF);
            }
            return self.rom.get(address).copied().unwrap_or(0xFF);
        }

        if address < 0x8000 {
            let lower_mask = (bank2_threshold - 1) as usize; // 0x0F for multicart, 0x1F for MBC1
            let lower = (self.rombank as usize) & lower_mask;
            // Bank number 0 is promoted to 1 (hardware quirk applies to the lower bits only).
            let lower = if lower == 0 { 1 } else { lower };
            let bank = ((bank2 as usize) << shift | lower) & (self.rom_bank_mask as usize);
            return self.rom.get(address - 0x4000 + bank * 0x4000).copied().unwrap_or(0xFF);
        }

        if address >= 0xA000 && address < 0xC000 {
            if ram_enable {
                // RAM bank is bank2 in RAM-banking mode, 0 otherwise.
                let r_bank = (if mode == 1 { bank2 as usize } else { 0 }) & self.ram_bank_mask;
                return self.ram[r_bank][address - 0xA000];
            }
            return 0xFF;
        }

        0xFF
    }

    fn mbc1_write(&mut self, address: usize, data: u8) {
        // Determine whether this is a multicart (needed to pick the right mask).
        let multicart = matches!(self.kind, MbcKind::Mbc1 { multicart: true, .. });
        let lower_mask: u8 = if multicart { 0x0F } else { 0x1F };

        if address < 0x2000 {
            if let MbcKind::Mbc1 { ref mut ram_enable, .. } = self.kind {
                *ram_enable = (data & 0x0F) == 0x0A;
            }
        } else if address < 0x4000 {
            // Lower bank bits; 0 is kept as 0 here and resolved to 1 during reads.
            self.rombank = (data & lower_mask) as u16;
        } else if address < 0x6000 {
            if let MbcKind::Mbc1 { ref mut bank2, .. } = self.kind {
                *bank2 = data & 0x3;
            }
        } else if address < 0x8000 {
            if let MbcKind::Mbc1 { ref mut mode, .. } = self.kind {
                *mode = data & 0x01;
            }
        } else if address >= 0xA000 && address < 0xC000 {
            // Copy-out the fields we need (all Copy types) so we're not holding
            // a borrow on self.kind while mutating self.ram.
            let (ram_enable, bank2, mode) = match self.kind {
                MbcKind::Mbc1 { ram_enable, bank2, mode, .. } => (ram_enable, bank2, mode),
                _ => return,
            };
            if ram_enable {
                let r_bank = (if mode == 1 { bank2 as usize } else { 0 }) & self.ram_bank_mask;
                self.ram[r_bank][address - 0xA000] = data;
            }
        }
    }

    // ─── MBC2 ────────────────────────────────────────────────────────────────
    //
    // 512 × 4-bit built-in RAM (no external RAM chips).
    // Bit 8 of the write address selects RAM-enable vs ROM-bank.
    //
    // Register writes (0x0000–0x3FFF only):
    //   addr & 0x0100 == 0  → RAM enable / disable  (0x_A = enable)
    //   addr & 0x0100 != 0  → ROM bank (lower 4 bits; 0 → 1)
    //
    // RAM (0xA000–0xBFFF):  512-nibble window, mirrored; upper nibble always 0xF.

    fn mbc2_read(&self, address: usize, ram_enable: bool) -> u8 {
        if address < 0x4000 {
            return self.rom.get(address).copied().unwrap_or(0xFF);
        }
        if address < 0x8000 {
            return self.rom
                .get(address - 0x4000 + (self.rombank as usize) * 0x4000)
                .copied()
                .unwrap_or(0xFF);
        }
        if address >= 0xA000 && address < 0xC000 {
            if ram_enable {
                let idx = (address - 0xA000) & 0x1FF; // 512-nibble wrap
                return self.ram[0][idx] | 0xF0;        // upper nibble reads as 1
            }
            return 0xFF;
        }
        0xFF
    }

    fn mbc2_write(&mut self, address: usize, data: u8) {
        if address < 0x4000 {
            if (address & 0x0100) == 0 {
                // RAM enable
                if let MbcKind::Mbc2 { ref mut ram_enable } = self.kind {
                    *ram_enable = (data & 0x0F) == 0x0A;
                }
            } else {
                // ROM bank select (lower 4 bits; 0 → 1)
                let mut bank = (data & 0x0F) as u16;
                if bank == 0 { bank = 1; }
                self.rombank = bank & self.rom_bank_mask;
            }
        } else if address >= 0xA000 && address < 0xC000 {
            let ram_enable = match self.kind {
                MbcKind::Mbc2 { ram_enable } => ram_enable,
                _ => return,
            };
            if ram_enable {
                let idx = (address - 0xA000) & 0x1FF;
                self.ram[0][idx] = data & 0x0F; // only the lower nibble is stored
            }
        }
    }

    // ─── MBC3 ────────────────────────────────────────────────────────────────
    //
    // Similar to MBC1 but:
    //   - Full 7-bit ROM bank number (no upper register, no bank-0 alias to 1 quirk
    //     except 0 → 1 on write).
    //   - RAM bank 0x00–0x03 → 8 KiB RAM banks.
    //   - RAM bank 0x08–0x0C → RTC register R/W.
    //
    // Register writes:
    //   0x0000–0x1FFF  RAM & RTC enable   (0x_A = enable)
    //   0x2000–0x3FFF  ROM bank (7-bit; 0 → 1)
    //   0x4000–0x5FFF  RAM bank / RTC register select
    //   0x6000–0x7FFF  RTC latch (0 → 1 sequence latches current time)

    fn mbc3_read(&self, address: usize, ram_enable: bool) -> u8 {
        if address < 0x4000 {
            return self.rom.get(address).copied().unwrap_or(0xFF);
        }
        if address < 0x8000 {
            return self.rom
                .get(address - 0x4000 + (self.rombank as usize) * 0x4000)
                .copied()
                .unwrap_or(0xFF);
        }
        if address >= 0xA000 && address < 0xC000 {
            if ram_enable {
                if self.rambank <= 0x03 {
                    return self.ram[self.rambank as usize][address - 0xA000];
                } else if self.rambank >= 0x08 && self.rambank <= 0x0C {
                    return self.rtc_registers[(self.rambank - 0x08) as usize];
                }
            }
            return 0xFF;
        }
        0xFF
    }

    fn mbc3_write(&mut self, address: usize, data: u8) {
        if address < 0x2000 {
            if let MbcKind::Mbc3 { ref mut ram_enable } = self.kind {
                *ram_enable = (data & 0x0F) == 0x0A;
            }
        } else if address < 0x4000 {
            let mut bank = (data & 0x7F) as u16;
            if bank == 0 { bank = 1; }
            self.rombank = bank & self.rom_bank_mask;
        } else if address < 0x6000 {
            if data <= 0x03 {
                self.rambank = data & (self.ram_bank_mask as u8);
            } else if data >= 0x08 && data <= 0x0C {
                self.rambank = data; // RTC register select; kept as raw value
            }
        } else if address < 0x8000 {
            // RTC latch: a write of 0 followed by 1 latches the current RTC
            // time into the readable registers.  Full RTC emulation not yet
            // implemented; this slot is here for future work.
        } else if address >= 0xA000 && address < 0xC000 {
            let ram_enable = match self.kind {
                MbcKind::Mbc3 { ram_enable } => ram_enable,
                _ => return,
            };
            if ram_enable {
                if self.rambank <= 0x03 {
                    self.ram[self.rambank as usize][address - 0xA000] = data;
                } else if self.rambank >= 0x08 && self.rambank <= 0x0C {
                    self.rtc_registers[(self.rambank - 0x08) as usize] = data;
                }
            }
        }
    }

    // ─── MBC5 ────────────────────────────────────────────────────────────────
    //
    // Supports up to 64 Mbit ROM (512 banks) via a 9-bit bank register split
    // across two write addresses.  Rumble motor support (carts 0x1C..=0x1E)
    // is signalled by bit 3 of the RAM bank register.
    //
    // Register writes:
    //   0x0000–0x1FFF  RAM enable       (0x_A = enable)
    //   0x2000–0x2FFF  ROM bank low byte  (bits 0–7)
    //   0x3000–0x3FFF  ROM bank high bit  (bit 8 only)
    //   0x4000–0x5FFF  RAM bank / rumble (0x00–0x0F; rumble carts mask to 0–7)
    //   0x6000–0x7FFF  (unused)

    fn mbc5_read(&self, address: usize, ram_enable: bool) -> u8 {
        if address < 0x4000 {
            return self.rom.get(address).copied().unwrap_or(0xFF);
        }
        if address < 0x8000 {
            return self.rom
                .get(address - 0x4000 + (self.rombank as usize) * 0x4000)
                .copied()
                .unwrap_or(0xFF);
        }
        if address >= 0xA000 && address < 0xC000 {
            if ram_enable {
                return self.ram[self.rambank as usize][address - 0xA000];
            }
            return 0xFF;
        }
        0xFF
    }

    fn mbc5_write(&mut self, address: usize, data: u8) {
        if address < 0x2000 {
            if let MbcKind::Mbc5 { ref mut ram_enable } = self.kind {
                *ram_enable = (data & 0x0F) == 0x0A;
            }
        } else if address < 0x3000 {
            // Low byte of ROM bank number.
            let new_bank = (self.rombank & 0x100) | data as u16;
            self.rombank = new_bank & self.rom_bank_mask;
        } else if address < 0x4000 {
            // High bit of ROM bank number (bit 8).
            let new_bank = (self.rombank & 0xFF) | ((data as u16 & 0x01) << 8);
            self.rombank = new_bank & self.rom_bank_mask;
        } else if address < 0x6000 {
            // Rumble carts (0x1C–0x1E) use bit 3 for the rumble motor; mask
            // it out of the RAM bank number.
            let raw_bank = if self.cart_type >= 0x1C { data & 0x07 } else { data & 0x0F };
            self.rambank = raw_bank & (self.ram_bank_mask as u8);
        } else if address >= 0xA000 && address < 0xC000 {
            let ram_enable = match self.kind {
                MbcKind::Mbc5 { ram_enable } => ram_enable,
                _ => return,
            };
            if ram_enable {
                self.ram[self.rambank as usize][address - 0xA000] = data;
            }
        }
    }

    // ─── Save RAM helpers ─────────────────────────────────────────────────────

    /// Returns the external RAM size in bytes as declared in the ROM header,
    /// with special handling for MBC2 (always 512 bytes).
    pub fn get_ram_size(&self) -> usize {
        if matches!(self.kind, MbcKind::Mbc2 { .. }) {
            return 512;
        }
        match self.rom.get(0x149).copied().unwrap_or(0) {
            0x00 => 0,
            0x01 => 0x800,
            0x02 => 0x2000,
            0x03 => 0x8000,
            0x04 => 0x20000,
            0x05 => 0x10000,
            _    => 0x2000,
        }
    }

    /// Serialize all RAM banks for save-game persistence.
    pub fn export_save_ram(&self) -> Vec<u8> {
        let ram_size = self.get_ram_size();
        let num_banks = (ram_size / 0x2000).max(1);
        let mut data = Vec::with_capacity(num_banks * 0x2000);
        for bank in 0..num_banks {
            data.extend_from_slice(&self.ram[bank]);
        }
        data
    }

    /// Restore RAM banks from a previously saved blob.
    pub fn import_save_ram(&mut self, data: &[u8]) {
        let ram_size = self.get_ram_size();
        let num_banks = (ram_size / 0x2000).max(1);
        for bank in 0..num_banks {
            let start = bank * 0x2000;
            let end   = (start + 0x2000).min(data.len());
            if start < data.len() {
                self.ram[bank][..end - start].copy_from_slice(&data[start..end]);
            }
        }
    }
}








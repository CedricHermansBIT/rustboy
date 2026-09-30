#[cfg(not(target_arch = "wasm32"))]
use std::time::{SystemTime, UNIX_EPOCH};

#[cfg(target_arch = "wasm32")]
use js_sys::Date;

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
    /// Latched copy used by MBC3 0->1 latch command.
    rtc_latched_registers: [u8; 5],
    rtc_latch_active: bool,
    rtc_latch_armed: bool,
    /// Unix timestamp used as base for RTC progression.
    rtc_last_update_unix: u64,
    /// True only for MBC3+Timer cartridge types (0x0F, 0x10).
    has_rtc: bool,
    /// True when the cartridge has a battery-backed save (set at init time).
    pub has_battery: bool,
    /// Set only when persistent cartridge state changes. This keeps the web
    /// autosave timer from serializing unchanged RAM every five seconds.
    save_dirty: bool,
}

impl Mbc {
    const RTC_SAVE_MAGIC: [u8; 6] = *b"RBRTC1";

    /// Append the mutable cartridge-controller state to an emulator snapshot.
    /// The ROM itself is deliberately excluded: save states are tied to the
    /// already-loaded cartridge by the CPU-level ROM fingerprint.
    pub(crate) fn export_state(&self, out: &mut Vec<u8>) {
        let (tag, a, b, c) = match self.kind {
            MbcKind::None => (0, 0, 0, 0),
            MbcKind::Mbc1 { bank2, mode, ram_enable, multicart } =>
                (1, bank2, mode, (ram_enable as u8) | ((multicart as u8) << 1)),
            MbcKind::Mbc2 { ram_enable } => (2, ram_enable as u8, 0, 0),
            MbcKind::Mbc3 { ram_enable } => (3, ram_enable as u8, 0, 0),
            MbcKind::Mbc5 { ram_enable } => (5, ram_enable as u8, 0, 0),
            MbcKind::Unknown(value) => (255, value, 0, 0),
        };
        out.extend_from_slice(&[tag, a, b, c]);
        out.extend_from_slice(&self.rombank.to_le_bytes());
        out.push(self.rambank);
        for bank in &self.ram { out.extend_from_slice(bank); }
        out.extend_from_slice(&self.rtc_registers);
        out.extend_from_slice(&self.rtc_latched_registers);
        out.push(self.rtc_latch_active as u8);
        out.push(self.rtc_latch_armed as u8);
        out.extend_from_slice(&self.rtc_last_update_unix.to_le_bytes());
        out.push(self.save_dirty as u8);
    }

    pub(crate) fn import_state(&mut self, input: &mut &[u8]) -> Result<(), &'static str> {
        fn take<'a>(input: &mut &'a [u8], n: usize) -> Result<&'a [u8], &'static str> {
            if input.len() < n { return Err("truncated cartridge state"); }
            let (head, tail) = input.split_at(n); *input = tail; Ok(head)
        }
        let header = take(input, 4)?;
        self.kind = match header[0] {
            0 => MbcKind::None,
            1 => MbcKind::Mbc1 { bank2: header[1], mode: header[2], ram_enable: header[3] & 1 != 0, multicart: header[3] & 2 != 0 },
            2 => MbcKind::Mbc2 { ram_enable: header[1] != 0 },
            3 => MbcKind::Mbc3 { ram_enable: header[1] != 0 },
            5 => MbcKind::Mbc5 { ram_enable: header[1] != 0 },
            255 => MbcKind::Unknown(header[1]),
            _ => return Err("invalid cartridge state"),
        };
        self.rombank = u16::from_le_bytes(take(input, 2)?.try_into().unwrap());
        self.rambank = take(input, 1)?[0];
        for bank in &mut self.ram { bank.copy_from_slice(take(input, 0x2000)?); }
        self.rtc_registers.copy_from_slice(take(input, 5)?);
        self.rtc_latched_registers.copy_from_slice(take(input, 5)?);
        self.rtc_latch_active = take(input, 1)?[0] != 0;
        self.rtc_latch_armed = take(input, 1)?[0] != 0;
        self.rtc_last_update_unix = u64::from_le_bytes(take(input, 8)?.try_into().unwrap());
        self.save_dirty = take(input, 1)?[0] != 0;
        Ok(())
    }

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
            0x00 | 0x08 | 0x09  => MbcKind::None,
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
        let has_rtc = matches!(cart_type, 0x0F | 0x10);
        let rtc_now = Self::now_unix_seconds();

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
            rtc_latched_registers: [0; 5],
            rtc_latch_active: false,
            rtc_latch_armed: false,
            rtc_last_update_unix: rtc_now,
            has_rtc,
            has_battery,
            save_dirty: false,
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
            rtc_latched_registers: [0; 5],
            rtc_latch_active: false,
            rtc_latch_armed: false,
            rtc_last_update_unix: Self::now_unix_seconds(),
            has_rtc: false,
            has_battery: false,
            save_dirty: false,
        }
    }

    #[inline]
    fn now_unix_seconds() -> u64 {
        #[cfg(target_arch = "wasm32")]
        {
            (Date::now() / 1000.0) as u64
        }
        #[cfg(not(target_arch = "wasm32"))]
        {
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .map_or(0, |d| d.as_secs())
        }
    }

    #[inline]
    fn rtc_is_halted(regs: &[u8; 5]) -> bool {
        (regs[4] & 0x40) != 0
    }

    #[inline]
    fn rtc_day(regs: &[u8; 5]) -> u16 {
        regs[3] as u16 | (((regs[4] & 0x01) as u16) << 8)
    }

    #[inline]
    fn rtc_apply_delta(mut regs: [u8; 5], delta_seconds: u64) -> [u8; 5] {
        if delta_seconds == 0 || Self::rtc_is_halted(&regs) {
            return regs;
        }

        let day = Self::rtc_day(&regs) as u64;
        let mut total = regs[0] as u64 + regs[1] as u64 * 60 + regs[2] as u64 * 3600 + day * 86_400;
        total += delta_seconds;

        let new_day = total / 86_400;
        let rem = total % 86_400;

        regs[0] = (rem % 60) as u8;
        regs[1] = ((rem / 60) % 60) as u8;
        regs[2] = ((rem / 3600) % 24) as u8;

        let wrapped_day = (new_day % 512) as u16;
        let mut dh = regs[4] & 0xC0;
        if new_day > 511 {
            dh |= 0x80;
        }
        dh = (dh & !0x01) | (((wrapped_day >> 8) as u8) & 0x01);
        regs[3] = wrapped_day as u8;
        regs[4] = dh;
        regs
    }

    #[inline]
    fn rtc_effective_registers(&self) -> [u8; 5] {
        if !self.has_rtc {
            return self.rtc_registers;
        }
        let now = Self::now_unix_seconds();
        let delta = now.saturating_sub(self.rtc_last_update_unix);
        Self::rtc_apply_delta(self.rtc_registers, delta)
    }

    #[inline]
    fn rtc_commit_now(&mut self) {
        if !self.has_rtc {
            return;
        }
        self.rtc_registers = self.rtc_effective_registers();
        self.rtc_last_update_unix = Self::now_unix_seconds();
    }

    #[inline]
    fn rtc_latch(&mut self) {
        if !self.has_rtc {
            return;
        }
        self.rtc_latched_registers = self.rtc_effective_registers();
        self.rtc_latch_active = true;
    }

    #[inline]
    fn rtc_write_selected_register(&mut self, value: u8) {
        if !self.has_rtc || !(0x08..=0x0C).contains(&self.rambank) {
            return;
        }

        self.rtc_commit_now();

        match self.rambank {
            0x08 => self.rtc_registers[0] = value % 60,
            0x09 => self.rtc_registers[1] = value % 60,
            0x0A => self.rtc_registers[2] = value % 24,
            0x0B => self.rtc_registers[3] = value,
            0x0C => {
                let prev_halt = self.rtc_registers[4] & 0x40;
                let dh = value & 0xC1;
                self.rtc_registers[4] = dh;
                let new_halt = self.rtc_registers[4] & 0x40;
                if prev_halt != new_halt {
                    self.rtc_last_update_unix = Self::now_unix_seconds();
                }
            }
            _ => {}
        }
        if self.has_battery {
            self.save_dirty = true;
        }
    }

    // ─── Read / Write dispatch ────────────────────────────────────────────────

    /// Read a byte from cartridge address space.
    /// Called for addresses in 0x0000..=0x7FFF and 0xA000..=0xBFFF.
    #[inline]
    pub fn read(&self, address: usize) -> u8 {
        match self.kind {
            MbcKind::None => {
                if (0xA000..=0xBFFF).contains(&address) {
                    return if matches!(self.cart_type, 0x08 | 0x09) && self.get_ram_size() != 0 {
                        self.ram[0][self.ram_offset(address)]
                    } else { 0xFF };
                }
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
            MbcKind::None => {
                if matches!(self.cart_type, 0x08 | 0x09) && self.get_ram_size() != 0 && (0xA000..=0xBFFF).contains(&address) {
                    let offset = self.ram_offset(address);
                    let byte = &mut self.ram[0][offset];
                    if *byte != data {
                        *byte = data;
                        if self.has_battery { self.save_dirty = true; }
                    }
                }
            }
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
            // MBC1M uses the same mode-1 remap with a four-bit shift, making
            // each selected 16-bank game visible through its own bank 0.
            if mode == 1 && self.rom_bank_mask >= bank2_threshold {
                let bank = ((bank2 as usize) << shift) & (self.rom_bank_mask as usize);
                return self.rom.get(address + bank * 0x4000).copied().unwrap_or(0xFF);
            }
            return self.rom.get(address).copied().unwrap_or(0xFF);
        }

        if address < 0x8000 {
            let lower_mask = (bank2_threshold - 1) as usize; // 0x0F for multicart, 0x1F for MBC1
            let raw_lower = self.rombank as usize & 0x1F;
            // Bank number 0 is promoted to 1 (hardware quirk applies to the lower bits only).
            let lower = (if raw_lower == 0 { 1 } else { raw_lower }) & lower_mask;
            let bank = ((bank2 as usize) << shift | lower) & (self.rom_bank_mask as usize);
            return self.rom.get(address - 0x4000 + bank * 0x4000).copied().unwrap_or(0xFF);
        }

        if address >= 0xA000 && address < 0xC000 {
            if ram_enable && self.get_ram_size() != 0 {
                // RAM bank is bank2 in RAM-banking mode, 0 otherwise.
                let r_bank = (if mode == 1 { bank2 as usize } else { 0 }) & self.ram_bank_mask;
                return self.ram[r_bank][self.ram_offset(address)];
            }
            return 0xFF;
        }

        0xFF
    }

    fn mbc1_write(&mut self, address: usize, data: u8) {
        // Keep bit 4 for the zero-bank decoder even on MBC1M. Wiring drops
        // that bit only after the raw five-bit zero has been translated.
        let lower_mask: u8 = 0x1F;

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
            if ram_enable && self.get_ram_size() != 0 {
                let r_bank = (if mode == 1 { bank2 as usize } else { 0 }) & self.ram_bank_mask;
                let offset = self.ram_offset(address);
                let slot = &mut self.ram[r_bank][offset];
                if *slot != data {
                    *slot = data;
                    self.save_dirty = self.has_battery;
                }
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
                let value = data & 0x0F; // only the lower nibble is stored
                if self.ram[0][idx] != value {
                    self.ram[0][idx] = value;
                    self.save_dirty = self.has_battery;
                }
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
                    return if self.get_ram_size() != 0 { self.ram[self.rambank as usize][self.ram_offset(address)] } else { 0xFF };
                } else if self.rambank >= 0x08 && self.rambank <= 0x0C {
                    if !self.has_rtc {
                        return 0xFF;
                    }
                    let regs = if self.rtc_latch_active {
                        self.rtc_latched_registers
                    } else {
                        self.rtc_effective_registers()
                    };
                    return regs[(self.rambank - 0x08) as usize];
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
            if self.has_rtc {
                // RTC latch is edge-triggered on a 0 -> 1 write sequence.
                if data == 0 {
                    self.rtc_latch_armed = true;
                } else if data == 1 {
                    if self.rtc_latch_armed {
                        self.rtc_latch();
                    }
                    self.rtc_latch_armed = false;
                } else {
                    self.rtc_latch_armed = false;
                }
            }
        } else if address >= 0xA000 && address < 0xC000 {
            let ram_enable = match self.kind {
                MbcKind::Mbc3 { ram_enable } => ram_enable,
                _ => return,
            };
            if ram_enable {
                if self.rambank <= 0x03 && self.get_ram_size() != 0 {
                    let offset = self.ram_offset(address);
                    let slot = &mut self.ram[self.rambank as usize][offset];
                    if *slot != data {
                        *slot = data;
                        self.save_dirty = self.has_battery;
                    }
                } else if self.rambank >= 0x08 && self.rambank <= 0x0C {
                    self.rtc_write_selected_register(data);
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
            if ram_enable && self.get_ram_size() != 0 {
                return self.ram[self.rambank as usize][self.ram_offset(address)];
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
            if ram_enable && self.get_ram_size() != 0 {
                let offset = self.ram_offset(address);
                let slot = &mut self.ram[self.rambank as usize][offset];
                if *slot != data {
                    *slot = data;
                    self.save_dirty = self.has_battery;
                }
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

    fn ram_offset(&self, address: usize) -> usize {
        (address - 0xA000) & (self.get_ram_size().min(0x2000).saturating_sub(1))
    }

    /// Serialize all RAM banks for save-game persistence.
    pub fn export_save_ram(&self) -> Vec<u8> {
        let ram_size = self.get_ram_size();
        let num_banks = (ram_size / 0x2000).max(1);
        let mut data = Vec::with_capacity(num_banks * 0x2000 + 32);
        for bank in 0..num_banks {
            data.extend_from_slice(&self.ram[bank]);
        }
        data.truncate(ram_size);

        if self.has_rtc {
            // Append optional RTC trailer. Older saves without this trailer
            // remain valid and are still accepted by import_save_ram().
            let regs = self.rtc_effective_registers();
            let now = Self::now_unix_seconds();
            data.extend_from_slice(&Self::RTC_SAVE_MAGIC);
            data.extend_from_slice(&regs);
            data.extend_from_slice(&now.to_le_bytes());
        }

        data
    }

    #[inline]
    pub fn save_ram_is_dirty(&self) -> bool {
        self.has_battery && self.save_dirty
    }

    #[inline]
    pub fn mark_save_ram_clean(&mut self) {
        self.save_dirty = false;
    }

    /// Restore RAM banks from a previously saved blob.
    pub fn import_save_ram(&mut self, data: &[u8]) {
        // Loading an existing save establishes the persisted baseline.
        self.save_dirty = false;
        let ram_size = self.get_ram_size();
        let num_banks = (ram_size / 0x2000).max(1);
        let ram_blob_len = ram_size;
        for bank in 0..num_banks {
            let start = bank * 0x2000;
            let end   = (start + 0x2000).min(data.len()).min(ram_size);
            if start < end {
                self.ram[bank][..end - start].copy_from_slice(&data[start..end]);
            }
        }

        if self.has_rtc {
            self.rtc_latch_active = false;
            self.rtc_latch_armed = false;

            let trailer_len = Self::RTC_SAVE_MAGIC.len() + 5 + 8;
            // Old exports padded even tiny/no-RAM cartridges to an 8 KiB bank.
            let legacy_len = num_banks * 0x2000;
            let trailer_offset = if data.get(ram_blob_len..).is_some_and(|bytes| bytes.starts_with(&Self::RTC_SAVE_MAGIC)) {
                ram_blob_len
            } else { legacy_len };
            if data.len() >= trailer_offset + trailer_len {
                let trailer = &data[trailer_offset..];
                if trailer.starts_with(&Self::RTC_SAVE_MAGIC) {
                    let mut regs = [0u8; 5];
                    regs.copy_from_slice(&trailer[Self::RTC_SAVE_MAGIC.len()..Self::RTC_SAVE_MAGIC.len() + 5]);

                    let mut ts_bytes = [0u8; 8];
                    ts_bytes.copy_from_slice(&trailer[Self::RTC_SAVE_MAGIC.len() + 5..Self::RTC_SAVE_MAGIC.len() + 13]);

                    self.rtc_registers = regs;
                    self.rtc_last_update_unix = u64::from_le_bytes(ts_bytes);
                    self.rtc_registers = self.rtc_effective_registers();
                    self.rtc_last_update_unix = Self::now_unix_seconds();
                    return;
                }
            }

            // Legacy RAM-only save: keep existing RTC registers but resume
            // progression from current wall-clock time.
            self.rtc_last_update_unix = Self::now_unix_seconds();
        }
    }

    pub fn clear_save_ram(&mut self) {
        for bank in &mut self.ram { bank.fill(0); }
        self.rtc_registers = [0; 5];
        self.rtc_latched_registers = [0; 5];
        self.rtc_latch_active = false;
        self.rtc_latch_armed = false;
        self.rtc_last_update_unix = Self::now_unix_seconds();
        self.save_dirty = false;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unbanked_ram_cartridges_persist_battery_writes() {
        let mut rom = vec![0; 0x8000]; rom[0x147] = 0x09; rom[0x149] = 0x02;
        let mut mbc = Mbc::new(rom, 0x2000);
        mbc.write(0xA123, 0xA5);
        assert_eq!(mbc.read(0xA123), 0xA5);
        assert!(mbc.save_ram_is_dirty());
        mbc.mark_save_ram_clean();
        mbc.write(0xA123, 0xA5);
        assert!(!mbc.save_ram_is_dirty());
    }

    #[test]
    fn multicart_zero_bank_remap_precedes_four_bit_mask() {
        let mut rom = vec![0u8; 64 * 0x4000];
        for bank in 0..64 { rom[bank * 0x4000] = bank as u8; }
        rom[0x147] = 1;
        let mut mbc = Mbc::new(rom, 0);
        mbc.kind = MbcKind::Mbc1 { bank2: 0, mode: 0, ram_enable: false, multicart: true };
        mbc.write(0x2000, 0);
        assert_eq!(mbc.read(0x4000), 1);
        mbc.write(0x2000, 0x10);
        assert_eq!(mbc.read(0x4000), 0);
        mbc.write(0x4000, 2);
        assert_eq!(mbc.read(0x4000), 0x20);
    }

    fn make_mbc3_timer() -> Mbc {
        let mut rom = vec![0u8; 0x8000];
        rom[0x147] = 0x10; // MBC3 + Timer + RAM + Battery
        rom[0x149] = 0x02; // 8 KiB RAM
        Mbc::new(rom, 0x2000)
    }

    #[test]
    fn small_ram_mirrors_and_exports_exact_size() {
        for cart in [0x09, 0x03, 0x13, 0x1B] {
            let mut rom = vec![0; 0x8000];
            rom[0x147] = cart;
            rom[0x149] = 0x01;
            let mut mbc = Mbc::new(rom, 0x800);
            mbc.write(0, 0x0A);
            mbc.write(0xA123, 0x5A);
            assert_eq!(mbc.read(0xA923), 0x5A, "cart {cart:02X}");
            assert_eq!(mbc.export_save_ram().len(), 0x800);
        }
        let mut rom = vec![0; 0x8000];
        rom[0x147] = 0x06;
        assert_eq!(Mbc::new(rom, 512).export_save_ram().len(), 512);
    }

    #[test]
    fn absent_ram_cannot_be_written_or_exported() {
        for cart in [0x00, 0x01, 0x11, 0x19] {
            let mut rom = vec![0; 0x8000];
            rom[0x147] = cart;
            let mut mbc = Mbc::new(rom, 0);
            mbc.write(0, 0x0A);
            mbc.write(0xA123, 0x5A);
            assert_eq!(mbc.read(0xA123), 0xFF);
            assert!(mbc.export_save_ram().is_empty());
        }
    }

    #[test]
    fn rtc_carry_can_be_cleared_by_software() {
        let mut mbc = make_mbc3_timer();
        mbc.rtc_registers[4] = 0xC0;
        mbc.write(0, 0x0A);
        mbc.write(0x4000, 0x0C);
        mbc.write(0xA000, 0x40);
        assert_eq!(mbc.rtc_registers[4], 0x40);
    }

    #[test]
    fn timer_only_saves_accept_compact_and_legacy_trailers() {
        let mut rom = vec![0; 0x8000];
        rom[0x147] = 0x0F;
        let mut mbc = Mbc::new(rom.clone(), 0);
        mbc.rtc_registers = [12, 34, 5, 0xAB, 0x41];
        let compact = mbc.export_save_ram();
        assert_eq!(compact.len(), Mbc::RTC_SAVE_MAGIC.len() + 13);
        let mut legacy = vec![0; 0x2000];
        legacy.extend_from_slice(&compact);
        for blob in [compact, legacy] {
            let mut restored = Mbc::new(rom.clone(), 0);
            restored.import_save_ram(&blob);
            assert_eq!(restored.rtc_registers, mbc.rtc_registers);
        }
    }

    #[test]
    fn rtc_apply_delta_advances_clock() {
        let regs = [58, 59, 23, 0x00, 0x00];
        let out = Mbc::rtc_apply_delta(regs, 3);
        assert_eq!(out[0], 1);
        assert_eq!(out[1], 0);
        assert_eq!(out[2], 0);
        assert_eq!(out[3], 1);
    }

    #[test]
    fn rtc_save_trailer_roundtrip_for_halted_clock() {
        let mut mbc = make_mbc3_timer();
        mbc.rtc_registers = [12, 34, 5, 0xAB, 0x41]; // halted + day high bit
        mbc.rtc_last_update_unix = Mbc::now_unix_seconds();

        let blob = mbc.export_save_ram();
        assert!(blob.windows(Mbc::RTC_SAVE_MAGIC.len()).any(|w| w == Mbc::RTC_SAVE_MAGIC));

        let mut mbc2 = make_mbc3_timer();
        mbc2.import_save_ram(&blob);

        assert_eq!(mbc2.rtc_registers[0], 12);
        assert_eq!(mbc2.rtc_registers[1], 34);
        assert_eq!(mbc2.rtc_registers[2], 5);
        assert_eq!(mbc2.rtc_registers[3], 0xAB);
        assert_eq!(mbc2.rtc_registers[4] & 0xC1, 0x41);
    }

    #[test]
    fn battery_ram_is_only_dirty_after_a_changed_write() {
        let mut rom = vec![0u8; 0x8000];
        rom[0x147] = 0x03; // MBC1 + RAM + Battery
        rom[0x149] = 0x02; // 8 KiB RAM
        let mut mbc = Mbc::new(rom, 0x2000);

        assert!(!mbc.save_ram_is_dirty());
        mbc.write(0x2000, 2); // Banking state is not persistent.
        assert!(!mbc.save_ram_is_dirty());

        mbc.write(0x0000, 0x0A);
        mbc.write(0xA123, 0x5A);
        assert!(mbc.save_ram_is_dirty());

        mbc.mark_save_ram_clean();
        mbc.write(0xA123, 0x5A); // Rewriting the same byte changes nothing.
        assert!(!mbc.save_ram_is_dirty());
        mbc.write(0xA123, 0xA5);
        assert!(mbc.save_ram_is_dirty());

        let blob = mbc.export_save_ram();
        mbc.import_save_ram(&blob);
        assert!(!mbc.save_ram_is_dirty());
    }

    #[test]
    fn rtc_register_write_marks_save_dirty() {
        let mut mbc = make_mbc3_timer();
        mbc.write(0x0000, 0x0A);
        mbc.write(0x4000, 0x08);
        mbc.write(0xA000, 42);
        assert!(mbc.save_ram_is_dirty());
    }
}

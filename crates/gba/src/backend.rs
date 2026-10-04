//! Shared frontend adapter, cartridge persistence and original checked snapshots.
use crate::{cpu::bank, GameBoyAdvance, CLOCK_HZ};
use rustboy_emulator_api::*;
use std::any::Any;
pub const MAX_ROM_BYTES: usize = 32 * 1024 * 1024;
// Version 1: fixed CPU/scheduler/peripheral prefix, RAM, RGBA frame, checksum.
const SNAPSHOT_BYTES: usize =
    491 + 0x40000 + 0x8000 + 0x400 + 0x18000 + 0x400 + 0x400 + 240 * 160 * 4 + 8;
pub fn validate_cartridge(data: &[u8]) -> Result<(), String> {
    if data.len() < 192 || data.len() > MAX_ROM_BYTES {
        return Err("GBA ROM must contain 192 bytes to 32 MiB".into());
    }
    if data[0xb2] != 0x96 {
        return Err("Invalid GBA cartridge header marker".into());
    }
    Ok(())
}
pub(crate) fn hash(bytes: &[u8]) -> u64 {
    bytes.iter().fold(0xcbf29ce484222325, |h, b| {
        (h ^ u64::from(*b)).wrapping_mul(0x100000001b3)
    })
}
impl Emulator for GameBoyAdvance {
    fn system_name(&self) -> &'static str {
        "Game Boy Advance (development)"
    }
    fn clock_hz(&self) -> u64 {
        CLOCK_HZ
    }
    fn title(&self) -> String {
        GameBoyAdvance::title(self)
    }
    fn run(&mut self, ticks: u64) -> RunResult {
        if self.paused() {
            return RunResult {
                ticks: 0,
                reason: StopReason::Paused,
            };
        }
        let ticks = GameBoyAdvance::run(self, ticks);
        RunResult {
            ticks,
            reason: if self.fault.is_some() {
                StopReason::Paused
            } else {
                StopReason::BudgetExhausted
            },
        }
    }
    fn step(&mut self) -> RunResult {
        let ticks = GameBoyAdvance::step(self);
        RunResult {
            ticks,
            reason: StopReason::Stepped,
        }
    }
    fn paused(&self) -> bool {
        self.paused || self.fault.is_some()
    }
    fn set_paused(&mut self, value: bool) {
        self.paused = value;
    }
    fn reset(&mut self) {
        let backup_data = std::mem::take(&mut self.bus.backup.data);
        let dirty = self.bus.backup.dirty;
        let address_bits = self.bus.backup.eeprom.address_bits;
        let rom = std::mem::take(&mut self.bus.rom);
        let bios = if self.bus.replacement_bios {
            None
        } else {
            Some(std::mem::take(&mut self.bus.bios))
        };
        *self = Self::new(rom, bios).expect("Previously validated cartridge and BIOS");
        self.bus.backup.data = backup_data;
        self.bus.backup.dirty = dirty;
        self.bus.backup.eeprom.address_bits = address_bits;
    }
    fn set_button(&mut self, port: usize, button: Button, pressed: bool) -> Result<(), String> {
        if port != 0 {
            return Err("GBA supports one local controller".into());
        }
        let bit = match button {
            Button::A => 0,
            Button::B => 1,
            Button::Select => 2,
            Button::Start => 3,
            Button::Right => 4,
            Button::Left => 5,
            Button::Up => 6,
            Button::Down => 7,
            Button::R => 8,
            Button::L => 9,
            _ => return Err("Unsupported GBA button".into()),
        };
        let mut keys = !self.bus.io16(0x130) & 0x3ff;
        if pressed {
            keys |= 1 << bit;
        } else {
            keys &= !(1 << bit);
        }
        self.bus.set_keys(keys);
        Ok(())
    }
    fn video_frame(&mut self) -> VideoFrame<'_> {
        GameBoyAdvance::video_frame(self)
    }
    fn drain_audio(&mut self) -> AudioChunk {
        AudioChunk {
            sample_rate: 32768,
            channels: 2,
            samples: self.bus.apu.drain(),
        }
    }
    fn state_id(&self) -> String {
        format!(
            "gba-{:016x}-{:016x}",
            hash(&self.bus.rom),
            hash(&self.bus.bios)
        )
    }
    fn save_info(&self) -> Option<SaveInfo> {
        (!self.bus.backup.data.is_empty()).then(|| SaveInfo {
            key: format!("rustboy_save_gba_{:016x}", self.bus.backup.rom_hash),
            dirty: self.bus.backup.dirty,
        })
    }
    fn export_save(&self) -> Vec<u8> {
        self.bus.backup.data.clone()
    }
    fn import_save(&mut self, data: &[u8]) -> Result<(), String> {
        self.bus.backup.import(data)
    }
    fn mark_save_clean(&mut self) {
        self.bus.backup.dirty = false;
    }
    fn clear_save(&mut self) {
        self.bus.backup.dirty |= self.bus.backup.data.iter().any(|v| *v != 255);
        self.bus.backup.data.fill(255);
        self.bus.backup.reset_commands();
    }
    fn export_state(&self) -> Vec<u8> {
        self.snapshot()
    }
    fn import_state(&mut self, data: &[u8]) -> Result<(), String> {
        self.restore(data)
    }
    fn debug_extension(&self) -> Option<&dyn Any> {
        Some(self)
    }
    fn debug_extension_mut(&mut self) -> Option<&mut dyn Any> {
        Some(self)
    }
}
struct Reader<'a> {
    bytes: &'a [u8],
    position: usize,
}
impl Reader<'_> {
    fn bytes(&mut self, n: usize) -> Result<&[u8], String> {
        let end = self
            .position
            .checked_add(n)
            .ok_or("State length overflow")?;
        let bytes = self
            .bytes
            .get(self.position..end)
            .ok_or("Truncated GBA state")?;
        self.position = end;
        Ok(bytes)
    }
    fn int(&mut self, n: usize) -> Result<u64, String> {
        let mut bytes = [0; 8];
        bytes[..n].copy_from_slice(self.bytes(n)?);
        Ok(u64::from_le_bytes(bytes))
    }
    fn bool(&mut self) -> Result<bool, String> {
        match self.int(1)? {
            0 => Ok(false),
            1 => Ok(true),
            _ => Err("Invalid GBA boolean".into()),
        }
    }
}
fn put(data: &mut Vec<u8>, value: u64, n: usize) {
    data.extend_from_slice(&value.to_le_bytes()[..n]);
}
impl GameBoyAdvance {
    fn snapshot(&self) -> Vec<u8> {
        let mut data = b"RBGA\x06\0\0\0".to_vec();
        put(&mut data, hash(&self.bus.rom), 8);
        put(&mut data, hash(&self.bus.bios), 8);
        for register in self
            .cpu
            .r
            .into_iter()
            .chain(self.cpu.banks.into_iter().flatten())
            .chain(self.cpu.spsr)
        {
            put(&mut data, u64::from(register), 4);
        }
        put(&mut data, u64::from(self.cpu.cpsr), 4);
        put(&mut data, u64::from(self.cpu.sequential), 1);
        for value in [
            self.cpu.instructions,
            self.cpu.exceptions,
            self.clocks,
            self.frames,
            self.bus.clocks,
        ] {
            put(&mut data, value, 8);
        }
        put(&mut data, u64::from(self.line), 2);
        put(&mut data, u64::from(self.line_clocks), 2);
        put(&mut data, u64::from(self.wait_mask.is_some()), 1);
        put(&mut data, u64::from(self.wait_mask.unwrap_or(0)), 2);
        put(&mut data, u64::from(self.paused), 1);
        put(&mut data, u64::from(self.bus.halted), 1);
        put(&mut data, u64::from(self.fault.is_some()), 1);
        put(
            &mut data,
            u64::from(self.fault.as_ref().map_or(0, |f| f.instruction_address)),
            4,
        );
        put(
            &mut data,
            u64::from(self.fault.as_ref().map_or(0, |f| f.vector)),
            4,
        );
        for timer in self.bus.timers {
            put(&mut data, u64::from(timer.counter), 2);
            put(&mut data, u64::from(timer.reload), 2);
            put(&mut data, u64::from(timer.control), 1);
            put(&mut data, timer.phase, 8);
        }
        for dma in self.bus.dma {
            for value in [
                dma.source,
                dma.destination,
                dma.initial_destination,
                dma.count,
                dma.remaining,
            ] {
                put(&mut data, u64::from(value), 4);
            }
            put(&mut data, u64::from(dma.control), 2);
            put(&mut data, u64::from(dma.active), 1);
            put(&mut data, u64::from(dma.first), 1);
        }
        for fifo in self.bus.apu.fifo {
            for value in fifo.data {
                data.push(value as u8);
            }
            data.extend_from_slice(&[fifo.head, fifo.len, fifo.held as u8]);
        }
        put(&mut data, u64::from(self.bus.apu.phase), 2);
        put(&mut data, self.bus.apu.samples.len() as u64, 4);
        for sample in &self.bus.apu.samples {
            for value in sample {
                put(&mut data, u64::from(value.to_bits()), 4);
            }
        }
        for bytes in [
            &self.bus.ewram,
            &self.bus.iwram,
            &self.bus.palette,
            &self.bus.vram,
            &self.bus.oam,
            &self.bus.io,
            &self.pixels,
        ] {
            data.extend_from_slice(bytes);
        }
        let backup = &self.bus.backup;
        data.extend_from_slice(&[
            backup.kind.code(),
            backup.bank,
            backup.phase,
            u8::from(backup.id),
            u8::from(backup.dirty),
        ]);
        put(&mut data, backup.data.len() as u64, 4);
        data.extend_from_slice(&backup.data);
        for value in self
            .bus
            .affine
            .current
            .into_iter()
            .flatten()
            .chain(self.bus.affine.mosaic.into_iter().flatten())
        {
            put(&mut data, u64::from(value as u32), 4);
        }
        let e = &backup.eeprom;
        data.extend_from_slice(&e.input);
        data.extend_from_slice(&[e.address_bits, e.input_len, e.expected, e.output_left]);
        put(&mut data, e.output, 8);
        put(&mut data, e.busy, 8);
        put(&mut data, u64::from(e.pending_address), 2);
        put(&mut data, e.pending_data, 8);
        data.extend_from_slice(&self.bus.apu.psg.encode());
        let checksum = hash(&data);
        put(&mut data, checksum, 8);
        data
    }
    fn restore(&mut self, data: &[u8]) -> Result<(), String> {
        if data.len() < SNAPSHOT_BYTES
            || data.len()
                > SNAPSHOT_BYTES
                    + 228
                    + crate::psg::STATE_BYTES
                    + crate::apu::MAX_SAMPLES * 8
                    + 0x20000
        {
            return Err("Incorrect GBA state size".into());
        }
        let checksum = u64::from_le_bytes(data[data.len() - 8..].try_into().unwrap());
        if hash(&data[..data.len() - 8]) != checksum {
            return Err("Corrupted GBA state checksum".into());
        }
        let mut reader = Reader {
            bytes: &data[..data.len() - 8],
            position: 0,
        };
        let version = match reader.bytes(8)? {
            b"RBGA\x01\0\0\0" => 1,
            b"RBGA\x02\0\0\0" => 2,
            b"RBGA\x03\0\0\0" => 3,
            b"RBGA\x04\0\0\0" => 4,
            b"RBGA\x05\0\0\0" => 5,
            b"RBGA\x06\0\0\0" => 6,
            _ => return Err("Unsupported GBA state version".into()),
        };
        if reader.int(8)? != hash(&self.bus.rom) || reader.int(8)? != hash(&self.bus.bios) {
            return Err("GBA state belongs to another ROM/firmware".into());
        }
        let bios = if self.bus.replacement_bios {
            None
        } else {
            Some(self.bus.bios.clone())
        };
        let mut next = Self::new(self.bus.rom.clone(), bios)?;
        for register in &mut next.cpu.r {
            *register = reader.int(4)? as u32;
        }
        for register in next.cpu.banks.iter_mut().flatten() {
            *register = reader.int(4)? as u32;
        }
        for register in &mut next.cpu.spsr {
            *register = reader.int(4)? as u32;
        }
        next.cpu.cpsr = reader.int(4)? as u32;
        if bank(next.cpu.cpsr).is_none() {
            return Err("Invalid GBA CPU mode".into());
        }
        next.cpu.sequential = reader.bool()?;
        next.cpu.instructions = reader.int(8)?;
        next.cpu.exceptions = reader.int(8)?;
        next.clocks = reader.int(8)?;
        next.frames = reader.int(8)?;
        next.bus.clocks = reader.int(8)?;
        next.line = reader.int(2)? as u16;
        next.line_clocks = reader.int(2)? as u16;
        let waiting = reader.bool()?;
        let mask = reader.int(2)? as u16;
        next.wait_mask = waiting.then_some(mask);
        next.paused = reader.bool()?;
        next.bus.halted = reader.bool()?;
        let fault = reader.bool()?;
        let address = reader.int(4)? as u32;
        let vector = reader.int(4)? as u32;
        next.fault = fault.then_some(crate::machine::MissingBiosException {
            instruction_address: address,
            vector,
            reason: "Stopped state restored; reset to resume",
        });
        for timer in &mut next.bus.timers {
            timer.counter = reader.int(2)? as u16;
            timer.reload = reader.int(2)? as u16;
            timer.control = reader.int(1)? as u8;
            timer.phase = reader.int(8)?;
            if timer.phase >= [1, 64, 256, 1024][usize::from(timer.control & 3)] {
                return Err("Invalid GBA timer phase".into());
            }
        }
        for dma in &mut next.bus.dma {
            dma.source = reader.int(4)? as u32;
            dma.destination = reader.int(4)? as u32;
            dma.initial_destination = reader.int(4)? as u32;
            dma.count = reader.int(4)? as u32;
            dma.remaining = reader.int(4)? as u32;
            dma.control = reader.int(2)? as u16;
            dma.active = reader.bool()?;
            dma.first = reader.bool()?;
            if dma.count > 65536
                || dma.remaining > dma.count
                || (dma.active && dma.remaining == 0)
                || (dma.control & 0x8000 != 0 && dma.count == 0)
            {
                return Err("Invalid GBA DMA count".into());
            }
        }
        if version >= 2 {
            for fifo in &mut next.bus.apu.fifo {
                for value in &mut fifo.data {
                    *value = reader.int(1)? as i8;
                }
                fifo.head = reader.int(1)? as u8;
                fifo.len = reader.int(1)? as u8;
                fifo.held = reader.int(1)? as i8;
                if fifo.head >= 32 || fifo.len > 32 {
                    return Err("Invalid GBA FIFO state".into());
                }
            }
            next.bus.apu.phase = reader.int(2)? as u16;
            let count = reader.int(4)? as usize;
            if next.bus.apu.phase >= 512
                || count > crate::apu::MAX_SAMPLES
                || version < 5
                    && data.len()
                        != SNAPSHOT_BYTES
                            + 76
                            + count * 8
                            + if version >= 3 {
                                5 + if next.bus.backup.kind == crate::backup::Kind::Eeprom {
                                    0
                                } else {
                                    next.bus.backup.data.len()
                                }
                            } else {
                                0
                            }
                            + if version >= 4 { 32 } else { 0 }
            {
                return Err("Invalid GBA audio state".into());
            }
            for _ in 0..count {
                let sample = [
                    f32::from_bits(reader.int(4)? as u32),
                    f32::from_bits(reader.int(4)? as u32),
                ];
                if sample.iter().any(|s| !s.is_finite() || s.abs() > 1.) {
                    return Err("Invalid GBA audio sample".into());
                }
                next.bus.apu.samples.push_back(sample);
            }
        }
        for bytes in [
            &mut next.bus.ewram,
            &mut next.bus.iwram,
            &mut next.bus.palette,
            &mut next.bus.vram,
            &mut next.bus.oam,
            &mut next.bus.io,
            &mut next.pixels,
        ] {
            let length = bytes.len();
            bytes.copy_from_slice(reader.bytes(length)?);
        }
        if version >= 3 {
            let encoded_kind = reader.int(1)? as u8;
            let legacy_eeprom = version < 5
                && encoded_kind == 0
                && next.bus.backup.kind == crate::backup::Kind::Eeprom;
            if encoded_kind != next.bus.backup.kind.code() && !legacy_eeprom {
                return Err("GBA save hardware mismatch".into());
            }
            next.bus.backup.bank = reader.int(1)? as u8;
            next.bus.backup.phase = reader.int(1)? as u8;
            next.bus.backup.id = reader.bool()?;
            next.bus.backup.dirty = reader.bool()?;
            let backup = &mut next.bus.backup;
            if backup.phase > 7
                || backup.bank > u8::from(backup.kind == crate::backup::Kind::Flash128)
                || (matches!(
                    backup.kind,
                    crate::backup::Kind::None
                        | crate::backup::Kind::Sram
                        | crate::backup::Kind::Eeprom
                ) && (backup.phase != 0 || backup.id))
            {
                return Err("Invalid GBA Flash command state".into());
            }
            let length = if version >= 5 {
                reader.int(4)? as usize
            } else if legacy_eeprom {
                0
            } else {
                backup.kind.size()
            };
            if legacy_eeprom {
                backup.data.clone_from(&self.bus.backup.data);
                backup.eeprom.address_bits = self.bus.backup.eeprom.address_bits;
            } else {
                if (backup.kind == crate::backup::Kind::Eeprom && !matches!(length, 512 | 8192))
                    || (backup.kind != crate::backup::Kind::Eeprom && length != backup.kind.size())
                {
                    return Err("Invalid GBA backup storage size".into());
                }
                backup.data = reader.bytes(length)?.to_vec();
            }
            backup.dirty |= backup.data != self.bus.backup.data;
        } else {
            // Old states predate cartridge backup emulation. Keep persistent
            // storage rather than silently erasing the user's current save.
            next.bus.backup.data.clone_from(&self.bus.backup.data);
            next.bus.backup.dirty = self.bus.backup.dirty;
            next.bus.backup.eeprom.address_bits = self.bus.backup.eeprom.address_bits;
        }
        if version >= 4 {
            for value in next
                .bus
                .affine
                .current
                .iter_mut()
                .flatten()
                .chain(next.bus.affine.mosaic.iter_mut().flatten())
            {
                *value = reader.int(4)? as u32 as i32;
                if *value != crate::ppu::wrap_reference(*value) {
                    return Err("Invalid GBA affine reference state".into());
                }
            }
        } else {
            next.bus.reconstruct_affine(next.line);
        }
        if version >= 5 {
            let e = &mut next.bus.backup.eeprom;
            e.input.copy_from_slice(reader.bytes(81)?);
            e.address_bits = reader.int(1)? as u8;
            e.input_len = reader.int(1)? as u8;
            e.expected = reader.int(1)? as u8;
            e.output_left = reader.int(1)? as u8;
            e.output = reader.int(8)?;
            e.busy = reader.int(8)?;
            e.pending_address = reader.int(2)? as u16;
            e.pending_data = reader.int(8)?;
            if (next.bus.backup.kind == crate::backup::Kind::Eeprom
                && !e.valid(next.bus.backup.data.len()))
                || (next.bus.backup.kind != crate::backup::Kind::Eeprom
                    && *e != crate::eeprom::Eeprom::default())
            {
                return Err("Invalid GBA EEPROM serial state".into());
            }
        }
        if version >= 6 {
            next.bus.apu.psg = crate::psg::Psg::decode(reader.bytes(crate::psg::STATE_BYTES)?)?;
            if !next.bus.apu.psg.valid_io(&next.bus.io, next.bus.apu.phase) {
                return Err("Inconsistent GBA PSG registers/sample phase".into());
            }
        } else {
            // Legacy states predate PSG: retain their single exposed wave bank,
            // but do not invent oscillator phases or active voices.
            let bank = crate::psg::Psg::wave_index(&next.bus.io, 0x90);
            next.bus.apu.psg.wave[bank..bank + 16].copy_from_slice(&next.bus.io[0x90..0xa0]);
        }
        if next.line >= 228
            || next.line_clocks >= 1232
            || next.bus.clocks > 1_000_000_000
            || reader.position != reader.bytes.len()
        {
            return Err("Invalid GBA scheduler state".into());
        }
        next.bus.set_keys(0);
        next.bus.refresh_timer_schedule();
        *self = next;
        Ok(())
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    fn machine() -> GameBoyAdvance {
        let mut rom = vec![0; 192];
        rom[..4].copy_from_slice(&0xeafffffeu32.to_le_bytes());
        GameBoyAdvance::new(rom, None).unwrap()
    }
    #[test]
    fn snapshot_rebuilds_derived_timer_deadlines() {
        use crate::cpu::Memory;
        let mut m = machine();
        m.bus.write32(0x04000100, 0x0081fffa, false);
        m.bus.take_cycles();
        m.run(17);
        let deadline = m.bus.timer_deadline();
        let state = m.export_state();
        m.run(1000);
        let expected = m.export_state();
        m.import_state(&state).unwrap();
        assert_eq!(m.bus.timer_deadline(), deadline);
        m.run(1000);
        assert_eq!(m.export_state(), expected);
        let legacy = legacy_none_state(&state, 4);
        m.import_state(&legacy).unwrap();
        assert_eq!(m.bus.timer_deadline(), deadline);
    }
    #[test]
    fn version_five_state_migrates_wave_and_invalid_psg_rejects_atomically() {
        use crate::cpu::Memory;
        let mut m = machine();
        m.bus.write8(0x04000090, 0xab);
        let saved = m.export_state();
        let mut old = saved[..saved.len() - crate::psg::STATE_BYTES - 8].to_vec();
        old[4] = 5;
        let checksum = hash(&old);
        put(&mut old, checksum, 8);
        m.import_state(&old).unwrap();
        assert_eq!(m.bus.peek8(0x04000090), 0xab);
        assert_eq!(m.bus.apu.psg.active, 0);
        let before = m.export_state();
        let mut corrupt = before.clone();
        let position = corrupt.len() - crate::psg::STATE_BYTES - 8;
        corrupt[position + 56] = 1; // Active voice with a zero period counter.
        let length = corrupt.len();
        let checksum = hash(&corrupt[..length - 8]);
        corrupt[length - 8..].copy_from_slice(&checksum.to_le_bytes());
        assert!(m.import_state(&corrupt).is_err());
        assert_eq!(m.export_state(), before);
    }
    fn legacy_none_state(state: &[u8], version: u8) -> Vec<u8> {
        let count = u32::from_le_bytes(state[563..567].try_into().unwrap()) as usize;
        let memory = 567 + count * 8;
        let backup = memory + SNAPSHOT_BYTES - 491 - 8;
        assert_eq!(state[backup], 0);
        let mut old = state[..491].to_vec();
        old[4] = version;
        if version >= 2 {
            old.extend_from_slice(&state[491..memory]);
        }
        old.extend_from_slice(&state[memory..backup]);
        if version >= 3 {
            old.extend_from_slice(&state[backup..backup + 5]);
        }
        if version >= 4 {
            old.extend_from_slice(&state[backup + 9..backup + 41]);
        }
        let checksum = hash(&old);
        put(&mut old, checksum, 8);
        old
    }
    #[test]
    fn affine_snapshots_replay_distinct_latches_and_migrate_v3() {
        use crate::cpu::Memory;
        let mut m = machine();
        m.bus.write16(0x04000026, 0x100, false);
        m.bus.write16(0x0400004c, 0x30, false);
        m.bus.take_cycles();
        m.run(1232 * 9);
        m.bus.write32(0x0400002c, 0x4000, false);
        m.bus.take_cycles();
        let origin = m.bus.affine;
        assert_ne!(origin.current, origin.mosaic);
        let state = m.export_state();
        m.run(10000);
        let expected = m.export_state();
        m.import_state(&state).unwrap();
        assert_eq!(m.bus.affine, origin);
        m.run(10000);
        assert_eq!(m.export_state(), expected);
        let mut invalid = state.clone();
        let position = invalid.len() - 151 - crate::psg::STATE_BYTES;
        invalid[position..position + 4].copy_from_slice(&i32::MAX.to_le_bytes());
        let length = invalid.len();
        let checksum = hash(&invalid[..length - 8]);
        invalid[length - 8..].copy_from_slice(&checksum.to_le_bytes());
        assert!(m.import_state(&invalid).is_err());
        assert_eq!(m.export_state(), expected);
        let old = legacy_none_state(&state, 3);
        m.import_state(&old).unwrap();
        assert_eq!(
            m.bus.affine.current[0][1],
            0x4000 + i32::from(m.line) * 0x100
        );
        let old = legacy_none_state(&state, 2);
        m.import_state(&old).unwrap();
        assert_eq!(
            m.bus.affine.current[0][1],
            0x4000 + i32::from(m.line) * 0x100
        );
    }
    #[test]
    fn cartridge_saves_reset_state_restore_and_identity() {
        use crate::cpu::Memory;
        let mut m = machine();
        m.bus.rom[80..92].copy_from_slice(b"FLASH1M_V103");
        let mut m = GameBoyAdvance::new(m.bus.rom, None).unwrap();
        let key = m.save_info().unwrap().key;
        assert_eq!(m.export_save().len(), 0x20000);
        let command = |m: &mut GameBoyAdvance, value| {
            m.bus.write8(0x0e005555, 0xaa);
            m.bus.write8(0x0e002aaa, 0x55);
            m.bus.write8(0x0e005555, value);
        };
        command(&mut m, 0xb0);
        m.bus.write8(0x0e000000, 1);
        command(&mut m, 0xa0);
        // Snapshot in the middle of a command, then restore to finish it.
        let pending = m.export_state();
        m.bus.write8(0x0e001234, 0x5a);
        assert!(m.save_info().unwrap().dirty);
        m.import_state(&pending).unwrap();
        m.bus.write8(0x0e001234, 0xa5);
        let data = m.export_save();
        assert_eq!(data[0x11234], 0xa5);
        assert_eq!(data[0x1234], 255);
        m.reset();
        assert_eq!(m.export_save(), data);
        assert_eq!(m.bus.peek8(0x0e001234), 255);
        assert_eq!(m.save_info().unwrap().key, key);
        m.mark_save_clean();
        assert!(!m.save_info().unwrap().dirty);
        assert!(m.import_save(&data[..100]).is_err());
        assert_eq!(m.export_save(), data);
        m.clear_save();
        assert!(m.save_info().unwrap().dirty);
        assert!(m.export_save().iter().all(|v| *v == 255));
        m.import_save(&data).unwrap();
        assert_eq!(m.export_save(), data);
        assert!(!m.save_info().unwrap().dirty);
        let mut other = m.bus.rom.clone();
        other[30] = 1;
        let other = GameBoyAdvance::new(other, None).unwrap();
        assert_ne!(key, other.save_info().unwrap().key);
    }
    #[test]
    fn snapshots_preserve_sound_and_accept_legacy_v1() {
        use crate::cpu::Memory;
        let mut m = machine();
        m.bus.write16(0x04000084, 0x80, false);
        m.bus.write16(0x04000082, 0x0304, false);
        m.bus.write32(0x040000a0, 0x7f00ff80, false);
        m.bus.apu.fifo[0].held = 64;
        m.run(1500);
        let state = m.export_state();
        let expected = m.drain_audio();
        assert!(expected.samples.iter().any(|s| *s != 0.));
        m.import_state(&state).unwrap();
        assert_eq!(m.drain_audio().samples, expected.samples);
        assert_eq!(m.bus.apu.fifo[0].len, 4);
        let legacy = legacy_none_state(&state, 1);
        m.import_state(&legacy).unwrap();
        assert!(m.drain_audio().samples.is_empty());
        assert_eq!(m.bus.apu.fifo[0].len, 0);
        for (offset, value) in [(523, 32), (524, 33), (562, 255), (566, 255)] {
            let mut invalid = state.clone();
            invalid[offset] = value;
            let length = invalid.len();
            let checksum = hash(&invalid[..length - 8]);
            invalid[length - 8..].copy_from_slice(&checksum.to_le_bytes());
            assert!(m.import_state(&invalid).is_err());
        }
    }
    #[test]
    fn snapshots_replay_and_reject_corruption_atomically() {
        let mut m = machine();
        m.run(1000);
        let saved = m.export_state();
        m.run(4000);
        let expected = m.export_state();
        m.import_state(&saved).unwrap();
        m.run(4000);
        assert_eq!(m.export_state(), expected);
        let mut broken = saved.clone();
        broken[200] ^= 1;
        assert!(m.import_state(&broken).is_err());
        assert_eq!(m.export_state(), expected);
        assert!(m.import_state(&saved[..100]).is_err());
        let mut other = machine();
        other.bus.rom[100] = 1;
        assert!(other.import_state(&saved).is_err());
    }
    #[test]
    fn frontend_controls_and_release_on_restore() {
        let mut m = machine();
        m.set_button(0, Button::L, true).unwrap();
        assert_eq!(m.bus.io16(0x130) & 0x200, 0);
        let state = m.export_state();
        m.import_state(&state).unwrap();
        assert_eq!(m.bus.io16(0x130), 0x3ff);
        m.set_paused(true);
        assert_eq!(Emulator::run(&mut m, 1000).ticks, 0);
        assert!(Emulator::step(&mut m).ticks > 0);
        m.reset();
        assert!(!m.paused());
    }
    #[test]
    fn snapshots_reject_checksum_valid_impossible_peripherals() {
        let mut m = machine();
        let saved = m.export_state();
        assert_eq!(saved.len(), SNAPSHOT_BYTES + 228 + crate::psg::STATE_BYTES);
        for (offset, bytes) in [
            (348, &64u64.to_le_bytes()[..]),
            (415, &0x8000u16.to_le_bytes()[..]),
        ] {
            let mut broken = saved.clone();
            broken[offset..offset + bytes.len()].copy_from_slice(bytes);
            let len = broken.len();
            let checksum = hash(&broken[..len - 8]);
            broken[len - 8..].copy_from_slice(&checksum.to_le_bytes());
            assert!(m.import_state(&broken).is_err());
            assert_eq!(m.export_state(), saved);
        }
    }
    #[test]
    fn browser_cartridge_validation_accepts_32_mib_and_rejects_larger() {
        let mut rom = vec![0; MAX_ROM_BYTES];
        rom[0xb2] = 0x96;
        assert!(validate_cartridge(&rom).is_ok());
        rom.push(0);
        assert!(validate_cartridge(&rom).is_err());
        rom.truncate(192);
        assert!(validate_cartridge(&rom).is_ok());
        rom[0xb2] = 0;
        assert!(validate_cartridge(&rom).is_err());
    }
}

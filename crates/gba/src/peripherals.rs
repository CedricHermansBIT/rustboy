//! Independently authored timer and DMA hardware. Scheduler dispatches complete
//! transfer units; exact DMA startup/bus arbitration phase remains future work.
use crate::{bus::Bus, cpu::Memory};

#[derive(Clone, Copy, Debug, Default)]
pub(crate) struct Timer {
    pub counter: u16,
    pub reload: u16,
    pub control: u8,
    pub phase: u64,
}
#[derive(Clone, Copy, Debug, Default)]
pub(crate) struct Dma {
    pub(crate) source: u32,
    pub(crate) destination: u32,
    pub(crate) initial_destination: u32,
    pub(crate) count: u32,
    pub(crate) remaining: u32,
    pub(crate) control: u16,
    pub(crate) active: bool,
    pub(crate) first: bool,
}
impl Bus {
    pub(crate) fn peripheral_write(&mut self, offset: usize, old: u8) {
        if (0x100..0x110).contains(&offset) {
            let n = (offset - 0x100) / 4;
            let base = 0x100 + n * 4;
            if offset & 3 < 2 {
                self.timers[n].reload = self.io16(base);
            } else if offset & 3 == 2 {
                let control = self.io[offset] & if n == 0 { 0xc3 } else { 0xc7 };
                self.io[offset] = control;
                let timer = &mut self.timers[n];
                timer.control = control;
                if (old ^ control) & 7 != 0 {
                    timer.phase = 0;
                }
                if old & 0x80 == 0 && control & 0x80 != 0 {
                    timer.counter = timer.reload;
                    timer.phase = 0;
                }
            } else {
                self.io[offset] = 0;
            }
            self.refresh_timer_schedule();
        }
        if (0xb0..0xe0).contains(&offset) && (offset - 0xb0) % 12 >= 10 {
            let n = (offset - 0xb0) / 12;
            let base = 0xb0 + n * 12;
            self.io[offset] &= if offset == base + 10 {
                0xe0 // Bits 0-4 are unused on every channel.
            } else if n == 3 {
                0xff // Only DMA3 implements the Game Pak DRQ bit.
            } else {
                0xf7
            };
            let control = self.io16(base + 10);
            let enabled_before = self.dma[n].control & 0x8000 != 0;
            if control & 0x8000 == 0 {
                self.dma[n].active = false;
                self.dma[n].control = control;
            } else if !enabled_before {
                let read_reg =
                    |index| u32::from_le_bytes(self.io[index..index + 4].try_into().unwrap());
                let sound = (n == 1 || n == 2) && control >> 12 & 3 == 3;
                let width = if sound || control & 0x400 != 0 { 4 } else { 2 };
                let source =
                    read_reg(base) & if n == 0 { 0x07ffffff } else { 0x0fffffff } & !(width - 1);
                let destination = read_reg(base + 4)
                    & if n == 3 { 0x0fffffff } else { 0x07ffffff }
                    & !(width - 1);
                let count = u32::from(self.io16(base + 8)) & if n == 3 { 0xffff } else { 0x3fff };
                let count = if sound {
                    4
                } else if count == 0 {
                    if n == 3 {
                        0x10000
                    } else {
                        0x4000
                    }
                } else {
                    count
                };
                self.dma[n] = Dma {
                    source,
                    destination,
                    initial_destination: destination,
                    count,
                    remaining: count,
                    control,
                    active: control >> 12 & 3 == 0,
                    first: true,
                };
            } else {
                self.dma[n].control = control;
            }
            self.refresh_dma_channel(n);
        }
    }
    /// Advance by arithmetic, not one host iteration per hardware clock. Return
    /// overflow counts for future Direct Sound timer consumers.
    pub(crate) fn advance_timers(&mut self, clocks: u64) -> [u64; 4] {
        let mut overflows = [0; 4];
        if self.timer_enabled == 0 {
            return overflows;
        }
        if clocks < self.timer_next {
            // No independently clocked timer can overflow before the cached
            // deadline, so cascade timers and IRQ/FIFO consumers cannot change.
            // Advance only active counters, avoiding overflow/division work on
            // every CPU instruction while preserving immediate register reads.
            let mut enabled = self.timer_enabled;
            while enabled != 0 {
                let n = enabled.trailing_zeros() as usize;
                enabled &= enabled - 1;
                let timer = &mut self.timers[n];
                if n != 0 && timer.control & 4 != 0 {
                    continue;
                }
                let shift = [0, 6, 8, 10][usize::from(timer.control & 3)];
                let elapsed = timer.phase + clocks;
                timer.counter = timer.counter.wrapping_add((elapsed >> shift) as u16);
                timer.phase = elapsed & ((1 << shift) - 1);
            }
            if self.timer_next != u64::MAX {
                self.timer_next -= clocks;
            }
            return overflows;
        }
        let mut irqs = 0;
        for n in 0..4 {
            let timer = &mut self.timers[n];
            if self.timer_enabled & (1 << n) == 0 {
                continue;
            }
            let ticks = if n > 0 && timer.control & 4 != 0 {
                overflows[n - 1]
            } else {
                let shift = [0, 6, 8, 10][usize::from(timer.control & 3)];
                timer.phase += clocks;
                let ticks = timer.phase >> shift;
                timer.phase &= (1 << shift) - 1;
                ticks
            };
            let to_overflow = 0x10000 - u64::from(timer.counter);
            if ticks < to_overflow {
                timer.counter = timer.counter.wrapping_add(ticks as u16);
            } else {
                let period = 0x10000 - u64::from(timer.reload);
                let remaining = ticks - to_overflow;
                overflows[n] = 1 + remaining / period;
                timer.counter = timer.reload.wrapping_add((remaining % period) as u16);
                if timer.control & 0x40 != 0 {
                    irqs |= 1 << (3 + n);
                }
            }
        }
        if irqs != 0 {
            self.request_irq(irqs);
        }
        if self.timer_next == u64::MAX {
            // Enabled cascade-only timers have no independent deadline.
        } else if clocks < self.timer_next {
            self.timer_next -= clocks;
        } else {
            self.refresh_timer_schedule();
        }
        overflows
    }
    pub(crate) fn timer_deadline(&self) -> u64 {
        self.timer_next
    }
    pub(crate) fn refresh_timer_schedule(&mut self) {
        self.timer_enabled = self.timers.iter().enumerate().fold(0, |mask, (n, timer)| {
            mask | (u8::from(timer.control & 0x80 != 0) << n)
        });
        self.timer_next = self
            .timers
            .iter()
            .enumerate()
            .filter(|(n, t)| t.control & 0x80 != 0 && (*n == 0 || t.control & 4 == 0))
            .map(|(_, t)| {
                ((0x10000 - u64::from(t.counter)) << [0, 6, 8, 10][usize::from(t.control & 3)])
                    - t.phase
            })
            .min()
            .unwrap_or(u64::MAX);
    }
    /// 1=VBlank, 2=HBlank. Repeated DMA reloads count, and destination only in
    /// increment/reload mode; source continues from its internal latched address.
    pub(crate) fn trigger_dma(&mut self, event: u16) {
        for (n, channel) in self.dma.iter_mut().enumerate() {
            if channel.control & 0x8000 != 0
                && channel.control >> 12 & 3 == event
                && !channel.active
            {
                channel.remaining = channel.count;
                channel.active = true;
                channel.first = true;
                self.dma_active |= 1 << n;
            }
        }
    }
    /// DMA3 special timing captures 160 scanlines, including the two lines
    /// after visible display. Functional HBlank scheduling; exact request phase
    /// and camera/DRQ electrical timing are not yet modeled.
    pub(crate) fn trigger_capture_dma(&mut self, line: u16) {
        if !(2..162).contains(&line) {
            return;
        }
        let d = &mut self.dma[3];
        if d.control & 0xb000 == 0xb000 && !d.active {
            d.remaining = d.count;
            d.active = true;
            d.first = true;
            self.dma_active |= 8;
        }
    }
    pub(crate) fn finish_capture_dma(&mut self) {
        let d = &mut self.dma[3];
        if d.control & 0xb000 == 0xb000 {
            // Prevent another scanline request, but let the last burst drain.
            // A row may legitimately require more than the HBlank interval.
            d.control &= !0x8000;
            let control = d.control;
            self.set_io(0xde, control);
        }
    }
    pub(crate) fn dma_unit(&mut self) -> bool {
        if self.memory_locked || self.dma_active | self.dma_ready == 0 {
            return false;
        }
        self.request_fifo_dma();
        if self.dma_active == 0 {
            return false;
        }
        let n = self.dma_active.trailing_zeros() as usize;
        let mut d = self.dma[n];
        // DMA takes ownership of the bus. Do not let its setup/internal RAM
        // cycles manufacture prefetched CPU opcodes while the CPU is stopped.
        self.prefetch = crate::prefetch::Prefetch::default();
        let source_mask = if n == 0 { 0x07ffffff } else { 0x0fffffff };
        let destination_mask = if n == 3 { 0x0fffffff } else { 0x07ffffff };
        // Apply the physical address width to every transfer, not just enable.
        // This also handles older snapshots whose counters crossed the limit.
        d.source &= source_mask;
        d.destination &= destination_mask;
        let sound = (n == 1 || n == 2) && d.control >> 12 & 3 == 3;
        let width = if sound || d.control & 0x400 != 0 {
            4
        } else {
            2
        };
        if d.first {
            if n == 3 && width == 2 && self.eeprom_address(d.destination) {
                self.backup.begin_eeprom(d.remaining);
            }
            self.internal(if d.source >> 24 >= 8 && d.destination >> 24 >= 8 {
                4
            } else {
                2
            });
        }
        // DMA cannot grant BIOS read access merely because the interrupted CPU
        // happens to be executing firmware.
        let bios_access = self.bios_access;
        self.bios_access = false;
        if width == 4 {
            let value = self.read32(d.source, !d.first);
            self.write32(d.destination, value, !d.first);
        } else {
            let value = self.read16(d.source, !d.first);
            self.write16(d.destination, value, !d.first);
        }
        self.bios_access = bios_access;
        if self.memory_locked {
            // The transfer did not complete: retain its counters and do not
            // signal completion or dispatch another channel.
            return true;
        }
        let move_address = |a: u32, mode: u16| match mode {
            0 | 3 => a.wrapping_add(width),
            1 => a.wrapping_sub(width),
            _ => a,
        };
        d.source = move_address(d.source, d.control >> 7 & 3) & source_mask;
        if !sound {
            d.destination = move_address(d.destination, d.control >> 5 & 3) & destination_mask;
        }
        d.first = false;
        d.remaining -= 1;
        if d.remaining == 0 {
            d.active = false;
            if d.control & 0x4000 != 0 {
                self.request_irq(1 << (8 + n));
            }
            if d.control & 0x200 == 0 || d.control >> 12 & 3 == 0 {
                d.control &= !0x8000;
                self.set_io(0xba + n * 12, d.control);
            } else if d.control >> 5 & 3 == 3 {
                d.destination = d.initial_destination & destination_mask;
            }
        }
        self.dma[n] = d;
        self.refresh_dma_channel(n);
        true
    }
    fn refresh_dma_channel(&mut self, n: usize) {
        let bit = 1 << n;
        self.dma_active = (self.dma_active & !bit) | if self.dma[n].active { bit } else { 0 };
        for mask in &mut self.dma_fifo {
            *mask &= !bit;
        }
        let d = self.dma[n];
        if (n == 1 || n == 2) && d.control & 0xb000 == 0xb000 {
            match d.destination {
                0x040000a0 => self.dma_fifo[0] |= bit,
                0x040000a4 => self.dma_fifo[1] |= bit,
                _ => {}
            }
        }
        self.refresh_fifo_requests();
    }
    pub(crate) fn refresh_dma_schedule(&mut self) {
        for n in 0..4 {
            self.refresh_dma_channel(n);
        }
    }
    pub(crate) fn refresh_fifo_requests(&mut self) {
        self.dma_ready = 0;
        if self.io[0x84] & 0x80 == 0 {
            return;
        }
        for fifo in 0..2 {
            if self.apu.fifo[fifo].len <= 16 {
                self.dma_ready |= self.dma_fifo[fifo];
            }
        }
    }
    fn request_fifo_dma(&mut self) {
        let mut channels = self.dma_ready & !self.dma_active;
        while channels != 0 {
            let n = channels.trailing_zeros() as usize;
            channels &= channels - 1;
            let d = &mut self.dma[n];
            d.count = 4;
            d.remaining = 4;
            d.active = true;
            d.first = true;
            self.dma_active |= 1 << n;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn fifo_request_cache_tracks_sound_reset_master_and_bios_dma_reset() {
        let mut b = bus();
        configure(&mut b, 1, 0x03000000, 0x040000a0, 1, 0xb200);
        assert_eq!(b.dma_ready, 0);
        b.write16(0x04000084, 0x80, false);
        assert_eq!(b.dma_ready, 2);
        for _ in 0..5 {
            b.write32(0x040000a0, 0, false);
        }
        assert_eq!(b.dma_ready, 0);
        b.write16(0x04000082, 0x0800, false); // FIFO A reset.
        assert_eq!(b.dma_ready, 2);
        b.write16(0x04000084, 0, false);
        assert_eq!(b.dma_ready, 0);
        b.write16(0x04000084, 0x80, false);
        let mut cpu = crate::cpu::Cpu::default();
        cpu.r[0] = 0x40;
        crate::bios::service(1, &mut cpu, &mut b).unwrap();
        assert_eq!(b.dma_ready, 0);
        assert!(!b.dma_unit());
        b.write16(0x04000084, 0x80, false);
        assert_eq!(b.dma_ready, 2);
        configure(&mut b, 0, 0x03000000, 0x02000000, 1, 0x8000);
        assert_eq!(b.dma_active, 1);
        cpu.r[0] = 0x80;
        crate::bios::service(1, &mut cpu, &mut b).unwrap();
        assert_eq!(b.dma_active, 0);
        assert_eq!(b.dma_fifo, [0; 2]);
        assert_eq!(b.dma_ready, 0);
        assert!(!b.dma_unit());
    }
    #[test]
    fn dma_derived_masks_track_enable_fifo_thresholds_completion_and_disable() {
        let mut b = bus();
        let check = |b: &Bus| {
            let active = b
                .dma
                .iter()
                .enumerate()
                .fold(0, |mask, (n, d)| mask | (u8::from(d.active) << n));
            assert_eq!(b.dma_active, active);
            for fifo in 0..2 {
                let expected = (1..=2).fold(0, |mask, n| {
                    let d = b.dma[n];
                    mask | (u8::from(
                        d.control & 0x8000 != 0
                            && d.control >> 12 & 3 == 3
                            && d.destination == 0x040000a0 + fifo as u32 * 4,
                    ) << n)
                });
                assert_eq!(b.dma_fifo[fifo], expected);
            }
        };
        check(&b);
        b.write16(0x04000084, 0x80, false);
        b.apu.fifo[0].len = 32;
        b.apu.fifo[1].len = 32;
        configure(&mut b, 0, 0x03000000, 0x02000000, 2, 0x8000);
        configure(&mut b, 1, 0x03000000, 0x040000a0, 1, 0xb200);
        configure(&mut b, 2, 0x03000000, 0x040000a4, 1, 0xb200);
        check(&b);
        while b.dma_unit() {
            check(&b);
        }
        assert_eq!(b.dma_active, 0);
        b.apu.fifo[0].len = 16;
        b.refresh_fifo_requests();
        b.request_fifo_dma();
        assert_eq!(b.dma_active, 2);
        check(&b);
        while b.dma_unit() {
            check(&b);
        }
        assert_eq!(b.apu.fifo[0].len, 32);
        configure(&mut b, 3, 0x03000000, 0x02000000, 1, 0xb200);
        b.trigger_capture_dma(2);
        check(&b);
        assert_eq!(b.dma_active, 8);
        for n in 0..4 {
            b.write16(0x040000ba + n * 12, 0, false);
            check(&b);
        }
        assert_eq!(b.dma_active, 0);
        assert_eq!(b.dma_fifo, [0; 2]);
        b.dma_active = 15;
        b.dma_fifo = [15; 2];
        b.refresh_dma_schedule();
        check(&b);
    }
    #[test]
    fn timer_deadline_fast_path_matches_full_overflow_path_at_each_boundary() {
        for controls in [
            [0xc0, 0xc1, 0xc2, 0xc3],
            [0xc0, 0xc4, 0xc4, 0xc4],
            [0, 0xc4, 0xc2, 0xc4],
        ] {
            let make = || {
                let mut b = Bus::new(vec![0; 192], None).unwrap();
                for (n, control) in controls.into_iter().enumerate() {
                    b.write16(0x04000100 + n as u32 * 4, 0xffc0 + n as u16 * 8, false);
                    b.write16(0x04000102 + n as u32 * 4, control, false);
                }
                b
            };
            let mut fast = make();
            let mut full = make();
            for clocks in [1, 3, 5, 7, 17, 31, 1, 64, 128, 4096, 1, 10000] {
                // Force the reference bus through the pre-existing generic
                // overflow path, then let it rebuild the derived deadline.
                full.timer_next = 0;
                assert_eq!(fast.advance_timers(clocks), full.advance_timers(clocks));
                for n in 0..4 {
                    assert_eq!(fast.timers[n].counter, full.timers[n].counter);
                    assert_eq!(fast.timers[n].phase, full.timers[n].phase);
                }
                assert_eq!(fast.io16(0x202), full.io16(0x202));
                assert_eq!(fast.timer_next, full.timer_next);
            }
        }
    }
    fn bus() -> Bus {
        Bus::new(vec![0; 192], None).unwrap()
    }
    #[test]
    fn dma_discards_cpu_prefetch_without_refilling_it_on_ram_transfers() {
        let mut b = bus();
        b.write16(0x04000204, 0x4000, false);
        b.fetch16(0x08000000, false);
        b.internal(30);
        assert_eq!(b.prefetch.count, 8);
        configure(&mut b, 0, 0x03000000, 0x02000000, 2, 0x8000);
        while b.dma_unit() {
            assert_eq!(b.prefetch, crate::prefetch::Prefetch::default());
        }
        b.take_cycles();
        b.fetch16(0x08000002, true);
        assert_eq!(b.take_cycles(), 3);
    }
    #[test]
    fn capture_copies_exactly_160_rows_including_vblank_and_then_disables() {
        let mut b = bus();
        for row in 0..160 {
            b.write16(0x03000000 + row * 2, (0x100 + row) as u16, false);
        }
        configure(&mut b, 3, 0x03000000, 0x06000000, 1, 0xf200);
        for line in 0..162 {
            b.trigger_capture_dma(line);
            if line < 2 {
                assert!(!b.dma_unit());
            } else {
                assert!(b.dma_unit());
                assert!(!b.dma[3].active);
                assert_eq!(
                    b.peek16(0x06000000 + u32::from(line - 2) * 2),
                    0x100 + line - 2
                );
                assert_eq!(b.io16(0x202) & 0x800, 0x800);
                b.write16(0x04000202, 0x800, false);
            }
        }
        assert_eq!(b.dma[3].source, 0x03000140);
        assert_eq!(b.dma[3].destination, 0x06000140);
        b.finish_capture_dma();
        assert_eq!(b.io16(0xde), 0x7200);
        assert_eq!(b.io16(0x202) & 0x800, 0); // No additional completion IRQ.
        for line in 162..228 {
            b.trigger_capture_dma(line);
            assert!(!b.dma_unit());
        }
        b.trigger_capture_dma(2);
        assert!(!b.dma_unit()); // Software must re-enable capture next frame.
    }
    #[test]
    fn capture_obeys_nonrepeat_and_destination_reload() {
        let mut b = bus();
        for (n, value) in [10u16, 11, 12, 13].into_iter().enumerate() {
            b.write16(0x03000000 + n as u32 * 2, value, false);
        }
        configure(&mut b, 3, 0x03000000, 0x06000000, 2, 0xb000);
        b.trigger_capture_dma(2);
        while b.dma_unit() {}
        assert_eq!(b.io16(0xde) & 0x8000, 0);
        b.trigger_capture_dma(3);
        assert!(!b.dma_unit());
        assert_eq!(b.peek16(0x06000004), 0);
        configure(&mut b, 3, 0x03000000, 0x06000000, 2, 0xb260);
        for line in [2, 3] {
            b.trigger_capture_dma(line);
            while b.dma_unit() {}
            assert_eq!(b.dma[3].destination, 0x06000000);
        }
        assert_eq!(b.peek32(0x06000000), 0x000d000c);
        assert_eq!(b.dma[3].source, 0x03000008);
    }
    #[test]
    fn capture_trigger_and_shutdown_only_affect_dma3_special_mode() {
        let mut b = bus();
        for n in 0..4 {
            configure(&mut b, n, 0x03000000, 0x06000000, 2, 0xb200);
        }
        b.trigger_capture_dma(2);
        assert_eq!(b.dma.map(|d| d.active), [false, false, false, true]);
        b.finish_capture_dma(); // Stop requests, but drain the queued burst.
        assert!(b.dma[3].active);
        assert!(b.dma_unit());
        assert!(b.dma_unit());
        assert!(b.dma.iter().all(|d| !d.active));
        assert_eq!(b.io16(0xde), 0x3200);
        assert_eq!(b.dma[0].control, 0xb200);
        configure(&mut b, 3, 0x03000000, 0x06000000, 2, 0x9200);
        b.finish_capture_dma();
        assert_eq!(b.dma[3].control, 0x9200); // Don't disable VBlank DMA3.
    }
    #[test]
    fn dma_control_masks_reserved_bits_and_only_dma3_has_drq() {
        let mut b = bus();
        for n in 0..4 {
            let base = 0xb0 + n * 12;
            b.write16(0x04000000 + base as u32 + 10, 0x7fff, false);
            assert_eq!(b.io16(base + 10), if n == 3 { 0x7fe0 } else { 0x77e0 });
            b.write8(0x04000000 + base as u32 + 10, 0xff);
            assert_eq!(b.io[base + 10], 0xe0);
            assert_eq!(b.dma[n].control, b.io16(base + 10));
        }
    }
    #[test]
    fn dma_low_control_byte_updates_waiting_channel_without_relatching() {
        let mut b = bus();
        b.write16(0x03000000, 0x1234, false);
        b.write16(0x03000002, 0x5678, false);
        configure(&mut b, 0, 0x03000000, 0x02000000, 2, 0xa000);
        // Change destination to fixed through STRB while waiting for HBlank.
        b.write8(0x040000ba, 0x40);
        b.write32(0x040000b0, 0x03000100, false); // Programmed, not latched.
        b.trigger_dma(2);
        while b.dma_unit() {}
        assert_eq!(b.peek16(0x02000000), 0x5678);
        assert_eq!(b.peek16(0x02000002), 0);
        assert_eq!(b.dma[0].source, 0x03000004);
        assert_eq!(b.dma[0].destination, 0x02000000);
    }
    #[test]
    fn dma_counters_wrap_to_each_channels_physical_address_width() {
        let mut b = bus();
        for n in 0..4 {
            let source_mask = if n == 0 { 0x07ffffff } else { 0x0fffffff };
            let destination_mask = if n == 3 { 0x0fffffff } else { 0x07ffffff };
            for decrement in [false, true] {
                let source = if decrement { 0 } else { source_mask & !3 };
                let destination = if decrement { 0 } else { destination_mask & !3 };
                configure(
                    &mut b,
                    n,
                    source,
                    destination,
                    1,
                    if decrement { 0x84a0 } else { 0x8400 },
                );
                assert!(b.dma_unit());
                assert_eq!(
                    b.dma[n].source,
                    if decrement { source_mask & !3 } else { 0 }
                );
                assert_eq!(
                    b.dma[n].destination,
                    if decrement { destination_mask & !3 } else { 0 }
                );
            }
        }
    }
    #[test]
    fn dma0_wrap_does_not_escape_into_cartridge_rom() {
        let mut b = bus();
        b.rom[..4].copy_from_slice(&0xdeadbeefu32.to_le_bytes());
        b.write32(0x070003fc, 0x12345678, false);
        configure(&mut b, 0, 0x07fffffc, 0x02000000, 2, 0x8400);
        assert!(b.dma_unit());
        assert_eq!(b.dma[0].source, 0);
        assert!(b.dma_unit());
        assert_eq!(b.peek32(0x02000000), 0x12345678);
        assert_eq!(b.peek32(0x02000004), 0); // Protected BIOS latch, not ROM.
    }
    fn configure(b: &mut Bus, n: usize, source: u32, dest: u32, count: u16, control: u16) {
        let base = 0x040000b0 + n as u32 * 12;
        b.write32(base, source, false);
        b.write32(base + 4, dest, false);
        b.write32(
            base + 8,
            u32::from(count) | (u32::from(control) << 16),
            false,
        );
        b.take_cycles();
    }
    #[test]
    fn sound_dma_forces_words_fixed_destination_and_four_transfers() {
        let mut b = bus();
        b.write16(0x04000084, 0x80, false);
        for i in 0..12 {
            b.write32(0x02000000 + i * 4, 0x03020100 + i * 0x04040404, false);
        }
        configure(&mut b, 1, 0x02000000, 0x040000a0, 1, 0xb200);
        for _ in 0..8 {
            assert!(b.dma_unit());
        }
        assert!(!b.dma_unit());
        assert_eq!(b.apu.fifo[0].len, 32);
        assert_eq!(b.apu.fifo[0].data, std::array::from_fn(|i| i as i8));
        assert_eq!(b.dma[1].source, 0x02000020);
        assert_eq!(b.dma[1].destination, 0x040000a0);
        b.clock_direct_sound([16, 0, 0, 0]);
        for _ in 0..4 {
            assert!(b.dma_unit());
        }
        assert!(!b.dma_unit());
        assert_eq!(b.apu.fifo[0].len, 32);
        assert_eq!(b.dma[1].source, 0x02000030);
    }
    #[test]
    fn timer_reload_read_and_enable_edge() {
        let mut b = bus();
        b.write16(0x04000100, 0xfffe, false);
        assert_eq!(b.peek16(0x04000100), 0);
        b.write16(0x04000102, 0xc0, false);
        assert_eq!(b.peek16(0x04000100), 0xfffe);
        assert_eq!(b.advance_timers(1), [0; 4]);
        assert_eq!(b.peek16(0x04000100), 0xffff);
        assert_eq!(b.advance_timers(1), [1, 0, 0, 0]);
        assert_eq!(b.peek16(0x04000100), 0xfffe);
        assert_eq!(b.io16(0x202), 8);
        b.write16(0x04000100, 0xfffc, false);
        assert_eq!(b.peek16(0x04000100), 0xfffe);
        assert_eq!(b.advance_timers(6), [2, 0, 0, 0]);
        assert_eq!(b.peek16(0x04000100), 0xfffc);
    }
    #[test]
    fn timer_prescalers_and_cascade_chain() {
        for (prescaler, divisor) in [1, 64, 256, 1024].into_iter().enumerate() {
            let mut b = bus();
            b.write32(
                0x04000100,
                ((0x80 | prescaler as u32) << 16) | 0xffff,
                false,
            );
            b.advance_timers(divisor - 1);
            assert_eq!(b.peek16(0x04000100), 0xffff);
            assert_eq!(b.timer_deadline(), 1);
            b.advance_timers(1);
            assert_eq!(b.peek16(0x04000100), 0xffff);
        }
        let mut b = bus();
        for n in 0..4 {
            b.write32(
                0x04000100 + n * 4,
                ((if n == 0 { 0xc0 } else { 0xc4 }) << 16) | 0xfffe,
                false,
            );
        }
        assert_eq!(b.advance_timers(16), [8, 4, 2, 1]);
        assert_eq!(b.io16(0x202), 0x78);
    }
    #[test]
    fn stopped_timer_and_atomic_reload_start() {
        let mut b = bus();
        b.write32(0x04000100, 0x00801234, false);
        assert_eq!(b.peek16(0x04000100), 0x1234);
        b.advance_timers(12);
        b.write16(0x04000102, 0, false);
        b.advance_timers(1000);
        assert_eq!(b.peek16(0x04000100), 0x1240);
        b.write16(0x04000102, 0x80, false);
        assert_eq!(b.peek16(0x04000100), 0x1234);
    }
    #[test]
    fn timer_batch_matches_small_steps() {
        let mut a = bus();
        let mut b = bus();
        for n in 0..4 {
            let value = ((if n == 0 { 0xc1 } else { 0xc4 }) << 16) | 0xfffd;
            a.write32(0x04000100 + n * 4, value, false);
            b.write32(0x04000100 + n * 4, value, false);
        }
        a.advance_timers(200000);
        for _ in 0..200000 {
            b.advance_timers(1);
        }
        for n in 0..4 {
            assert_eq!(a.timers[n].counter, b.timers[n].counter);
            assert_eq!(a.timers[n].phase, b.timers[n].phase);
        }
        assert_eq!(a.io16(0x202), b.io16(0x202));
    }
    #[test]
    fn timer_schedule_cache_matches_canonical_state_after_writes_and_batches() {
        let mut b = bus();
        let check = |b: &Bus| {
            let expected = b
                .timers
                .iter()
                .enumerate()
                .filter(|(n, t)| t.control & 0x80 != 0 && (*n == 0 || t.control & 4 == 0))
                .map(|(_, t)| {
                    (65536 - u64::from(t.counter)) * [1, 64, 256, 1024][usize::from(t.control & 3)]
                        - t.phase
                })
                .min()
                .unwrap_or(u64::MAX);
            assert_eq!(b.timer_deadline(), expected);
        };
        for control in [0x80, 0xc1, 0x82, 0x83, 0xc4, 0, 0x80] {
            for n in 0..4 {
                b.write32(0x04000100 + n * 4, (control << 16) | 0xffed, false);
                check(&b);
                for clocks in [1, 3, 17, 64, 1025, 99999] {
                    b.advance_timers(clocks);
                    check(&b);
                }
                b.write16(0x04000100 + n * 4, 0xfffa, false);
                check(&b);
            }
        }
        for n in 0..4 {
            b.write16(0x04000102 + n * 4, if n == 0 { 0 } else { 0x84 }, false);
        }
        b.advance_timers(65536);
        check(&b);
        assert_eq!(b.timer_deadline(), u64::MAX);
    }
    #[test]
    fn immediate_word_dma_and_irq_and_write_only_registers() {
        let mut b = bus();
        b.write32(0x02000000, 0xdeadbeef, false);
        b.write32(0x02000004, 0x12345678, false);
        configure(&mut b, 3, 0x02000000, 0x03000000, 2, 0xc400);
        assert_eq!(b.peek32(0x040000d4), 0);
        assert!(b.dma_unit());
        assert_eq!(b.take_cycles(), 9);
        assert_eq!(b.peek32(0x03000000), 0xdeadbeef);
        assert!(b.dma_unit());
        assert!(!b.dma_unit());
        assert_eq!(b.peek32(0x03000004), 0x12345678);
        assert_eq!(b.io16(0xde) & 0x8000, 0);
        assert_eq!(b.io16(0x202), 1 << 11);
    }
    #[test]
    fn halfword_fixed_source_decrement_destination() {
        let mut b = bus();
        b.write16(0x03000000, 0xabcd, false);
        configure(&mut b, 0, 0x03000000, 0x02000004, 3, 0x8120);
        while b.dma_unit() {}
        for address in [0x02000000, 0x02000002, 0x02000004] {
            assert_eq!(b.peek16(address), 0xabcd);
        }
    }
    #[test]
    fn hblank_repeat_reload_and_latched_addresses() {
        let mut b = bus();
        for i in 0..4 {
            b.write16(0x03000000 + i * 2, (10 + i) as u16, false);
        }
        configure(&mut b, 0, 0x03000000, 0x02000000, 2, 0xa260);
        assert!(!b.dma_unit());
        b.trigger_dma(1);
        assert!(!b.dma_unit());
        b.trigger_dma(2);
        while b.dma_unit() {}
        assert_eq!(b.peek16(0x02000000), 10);
        assert_eq!(b.peek16(0x02000002), 11);
        b.write32(0x040000b0, 0x03000100, false);
        b.trigger_dma(2);
        while b.dma_unit() {}
        assert_eq!(b.peek16(0x02000000), 12);
        assert_eq!(b.peek16(0x02000002), 13);
        assert_ne!(b.io16(0xba) & 0x8000, 0);
        b.write16(0x040000ba, 0, false);
        b.trigger_dma(2);
        assert!(!b.dma_unit());
    }
    #[test]
    fn dma_priority_and_zero_count() {
        let mut b = bus();
        configure(&mut b, 3, 0x03000000, 0x02000000, 0, 0x8000);
        assert_eq!(b.dma[3].remaining, 65536);
        configure(&mut b, 0, 0x03000000, 0x02000000, 0, 0x8000);
        assert_eq!(b.dma[0].remaining, 16384);
        b.dma_unit();
        assert_eq!(b.dma[0].remaining, 16383);
        assert_eq!(b.dma[3].remaining, 65536);
    }
}

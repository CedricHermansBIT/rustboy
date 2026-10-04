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
        if (0xb0..0xe0).contains(&offset) && (offset - 0xb0) % 12 == 11 {
            let n = (offset - 0xb0) / 12;
            let base = 0xb0 + n * 12;
            let control = self.io16(base + 10);
            if control & 0x8000 == 0 {
                self.dma[n].active = false;
                self.dma[n].control = control;
            } else if old & 0x80 == 0 {
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
        }
    }
    /// Advance by arithmetic, not one host iteration per hardware clock. Return
    /// overflow counts for future Direct Sound timer consumers.
    pub(crate) fn advance_timers(&mut self, clocks: u64) -> [u64; 4] {
        let mut overflows = [0; 4];
        if self.timer_enabled == 0 {
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
        for channel in &mut self.dma {
            if channel.control & 0x8000 != 0
                && channel.control >> 12 & 3 == event
                && !channel.active
            {
                channel.remaining = channel.count;
                channel.active = true;
                channel.first = true;
            }
        }
    }
    pub(crate) fn dma_unit(&mut self) -> bool {
        self.request_fifo_dma();
        let Some(n) = self.dma.iter().position(|d| d.active) else {
            return false;
        };
        let mut d = self.dma[n];
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
        let move_address = |a: u32, mode: u16| match mode {
            0 | 3 => a.wrapping_add(width),
            1 => a.wrapping_sub(width),
            _ => a,
        };
        d.source = move_address(d.source, d.control >> 7 & 3);
        if !sound {
            d.destination = move_address(d.destination, d.control >> 5 & 3);
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
                d.destination = d.initial_destination;
            }
        }
        self.dma[n] = d;
        true
    }
    fn request_fifo_dma(&mut self) {
        if self.io16(0x84) & 0x80 == 0 {
            return;
        }
        for n in 1..=2 {
            let d = &mut self.dma[n];
            let fifo = match d.destination {
                0x040000a0 => 0,
                0x040000a4 => 1,
                _ => continue,
            };
            if d.control & 0x8000 != 0
                && d.control >> 12 & 3 == 3
                && !d.active
                && self.apu.fifo[fifo].len <= 16
            {
                d.count = 4;
                d.remaining = 4;
                d.active = true;
                d.first = true;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn bus() -> Bus {
        Bus::new(vec![0; 192], None).unwrap()
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
        b.apu.fifo[0].len = 16;
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

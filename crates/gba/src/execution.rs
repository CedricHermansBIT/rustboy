//! Event-bounded CPU execution with explicit peripheral-observation barriers.
//!
//! This keeps the reference instruction-boundary timing model: previous
//! instructions' peripheral time is synchronized BEFORE an MMIO/EEPROM access,
//! and the current instruction's clocks are applied AFTER its side effects.
//! CPU fetch timing, prefetch, DMA units and every hardware event remain real.
use crate::{bus::Bus, machine::GameBoyAdvance};

#[derive(Default)]
pub(crate) struct DeferredClock {
    pub pending: u64,
    pub credited: u64,
    pub barrier: bool,
}

impl Bus {
    /// Synchronize preceding instructions, never the partially executed one.
    /// Called before peripheral observations/mutations and exception helpers.
    pub(crate) fn synchronize_deferred(&mut self) {
        let mut clocks = std::mem::take(&mut self.execution.pending);
        self.execution.credited += clocks;
        while clocks > 0 {
            let advance = clocks.min(self.timer_deadline()).min(self.apu.deadline());
            let overflow = self.advance_timers(advance);
            if overflow[0] != 0 || overflow[1] != 0 {
                self.clock_direct_sound(overflow);
            }
            self.clock_audio(advance);
            self.backup.clock_eeprom(advance);
            clocks -= advance;
        }
    }
}

impl GameBoyAdvance {
    pub(crate) fn can_run_cpu_slice(&self) -> bool {
        self.fault.is_none()
            && !self.bus.memory_locked
            && !self.bus.stopped
            && !self.bus.halted
            && self.wait_mask.is_none()
            && (self.bus.dma_active | self.bus.dma_ready) == 0
            // Even a masked CPU interrupt can become unmasked by an MSR.
            // Pending enabled IRQs therefore use the reference path until
            // serviced/acknowledged, rather than eliding per-op IRQ checks.
            && !self.bus.irq_enabled()
    }

    pub(crate) fn run_cpu_slice(&mut self, budget: u64) -> u64 {
        debug_assert_eq!(self.bus.execution.pending, 0);
        debug_assert_eq!(self.bus.execution.credited, 0);
        let start = self.clocks;
        let video = u64::from(if self.line_clocks < 1006 {
            1006 - self.line_clocks
        } else {
            1232 - self.line_clocks
        });
        // No preceding instruction in this slice can have crossed an event.
        // Its last instruction may overshoot, exactly as reference step does.
        let horizon = budget
            .min(video)
            .min(self.bus.timer_deadline())
            .min(self.bus.apu.deadline())
            .max(1);
        self.bus.execution.barrier = false;
        let mut elapsed = 0;
        #[cfg(feature = "decoded-blocks")]
        let mut cursor = None;
        loop {
            let address = self.cpu.r[15];
            let thumb = self.cpu.thumb();
            let exceptions = self.cpu.exceptions;
            #[cfg(feature = "decoded-blocks")]
            self.step_decoded_slice(&mut cursor, address, thumb);
            #[cfg(not(feature = "decoded-blocks"))]
            self.cpu.step(&mut self.bus);
            let exception = self.cpu.exceptions != exceptions;
            if exception {
                // BIOS helpers may reset peripherals or use direct peeks and
                // internal I/O setters. Finish preceding instructions first.
                self.finish_cpu_slice();
                self.handle_replacement_exception(address, thumb);
            }
            let clocks = self.bus.take_cycles();
            elapsed += clocks;
            self.bus.execution.pending += clocks;
            if elapsed >= horizon
                || self.bus.execution.barrier
                || exception
                || self.bus.memory_locked
                || clocks == 0
            {
                break;
            }
        }
        self.finish_cpu_slice();
        self.clocks.wrapping_sub(start)
    }

    fn finish_cpu_slice(&mut self) {
        let credited = std::mem::take(&mut self.bus.execution.credited);
        // An observation barrier's preceding instructions are strictly before
        // the next video event. Only peripherals were synchronized by Bus;
        // account for this prefix without advancing them a second time.
        if credited != 0 {
            let boundary = if self.line_clocks < 1006 { 1006 } else { 1232 };
            debug_assert!(credited < u64::from(boundary - self.line_clocks));
            self.clocks = self.clocks.wrapping_add(credited);
            self.line_clocks += credited as u16;
        }
        let pending = std::mem::take(&mut self.bus.execution.pending);
        self.advance_video(pending);
        debug_assert_eq!(self.bus.execution.pending, 0);
        debug_assert_eq!(self.bus.execution.credited, 0);
    }
}

#[cfg(test)]
mod tests;

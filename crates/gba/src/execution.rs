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
    #[cfg(feature = "idle-loops")]
    pub idle_unsafe: bool,
    #[cfg(all(feature = "idle-loops", not(target_arch = "wasm32")))]
    pub monitor_idle: bool,
}

impl Bus {
    /// Synchronize preceding instructions, never the partially executed one.
    /// Called before peripheral observations/mutations and exception helpers.
    pub(crate) fn synchronize_deferred(&mut self) {
        let mut clocks = std::mem::take(&mut self.execution.pending);
        self.execution.credited += clocks;
        while clocks > 0 {
            let advance = clocks
                .min(self.timer_deadline())
                .min(self.apu.deadline())
                .min(self.serial.deadline());
            let overflow = self.advance_timers(advance);
            if overflow[0] != 0 || overflow[1] != 0 {
                self.clock_direct_sound(overflow);
            }
            self.clock_audio(advance);
            self.backup.clock(advance);
            if self.serial.deadline() != u64::MAX {
                if self.serial.advance(advance) {
                    self.request_irq(0x80);
                }
                self.serial.store_registers(&mut self.io);
            }
            if self.rtc.advance(advance) {
                self.request_irq(0x2000);
            }
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
            && !self.bus.serial.waiting()
            && self.wait_mask.is_none()
            && (self.bus.dma_active | self.bus.dma_ready) == 0
            // A pending IRQ cannot dispatch while CPSR.I masks it. Run the
            // handler in slices too, but exit on the instruction that unmasks.
            && (!self.bus.irq_enabled() || self.cpu.cpsr() & crate::cpu::I != 0)
    }

    pub(crate) fn run_cpu_slice(&mut self, budget: u64) -> u64 {
        if self.cpu.thumb() {
            self.run_cpu_slice_mode::<true>(budget)
        } else {
            self.run_cpu_slice_mode::<false>(budget)
        }
    }

    fn run_cpu_slice_mode<const THUMB: bool>(&mut self, budget: u64) -> u64 {
        #[cfg(all(feature = "idle-loops", not(target_arch = "wasm32")))]
        let monitor = self.idle_policy.enabled(self.clocks);
        #[cfg(all(feature = "idle-loops", target_arch = "wasm32"))]
        let monitor = true;
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
            .min(self.bus.serial.deadline())
            .max(1);
        self.bus.execution.barrier = false;
        let mut elapsed = 0;
        #[cfg(not(feature = "decoded-blocks"))]
        let mut instructions = 0u32;
        let pending_irq = self.bus.irq_enabled();
        // Frontend JIT policy cannot change during a synchronous slice. Avoid
        // probing the general compiler on every Thumb op when it is disabled.
        #[cfg(all(feature = "jit", target_arch = "wasm32"))]
        let try_jit = self.jit.enabled && (!THUMB || self.jit.full_enabled);
        #[cfg(feature = "idle-loops")]
        {
            #[cfg(not(target_arch = "wasm32"))]
            {
                self.bus.execution.monitor_idle = monitor;
            }
            if monitor {
                self.idle.start_slice(&mut self.bus);
            } else {
                self.idle = crate::idle::Detector::default();
            }
        }
        #[cfg(all(feature = "idle-loops", not(target_arch = "wasm32")))]
        let mut skipped_clocks = 0;
        #[cfg(feature = "decoded-blocks")]
        let mut cursor = None;
        loop {
            let address = self.cpu.r[15];
            #[cfg(all(feature = "jit", target_arch = "wasm32"))]
            {
                #[cfg(not(feature = "decoded-blocks"))]
                if try_jit && instructions != 0 {
                    self.cpu.instructions =
                        self.cpu.instructions.wrapping_add(u64::from(instructions));
                    instructions = 0;
                }
                let before_exceptions = self.cpu.exceptions;
                if let Some(clocks) = try_jit
                    .then(|| self.run_jit_block(horizon - elapsed, THUMB))
                    .flatten()
                {
                    if self.cpu.exceptions != before_exceptions {
                        #[cfg(feature = "idle-loops")]
                        {
                            self.idle = crate::idle::Detector::default();
                        }
                        self.finish_cpu_slice();
                        self.handle_replacement_exception(
                            self.bus.jit_instruction_address(),
                            THUMB,
                        );
                        let extra = self.bus.take_cycles();
                        self.bus.execution.pending += extra;
                        break;
                    }
                    elapsed += clocks;
                    if self.cpu.thumb() != THUMB
                        || pending_irq && self.cpu.cpsr() & crate::cpu::I == 0
                    {
                        break;
                    }
                    if elapsed >= horizon || self.bus.execution.barrier || self.bus.memory_locked {
                        break;
                    }
                    // Branches still participate in exact idle-loop detection.
                    #[cfg(feature = "idle-loops")]
                    if monitor && self.cpu.r[15] <= address {
                        let skipped = self.idle.observe(
                            &mut self.cpu,
                            &mut self.bus,
                            address,
                            elapsed,
                            horizon,
                        );
                        elapsed += skipped.clocks;
                        self.bus.execution.pending += skipped.clocks;
                        self.accelerated_instructions += skipped.instructions;
                    }
                    continue;
                }
            }
            let exceptions = self.cpu.exceptions;
            #[cfg(feature = "decoded-blocks")]
            self.step_decoded_slice(&mut cursor, address, THUMB);
            #[cfg(not(feature = "decoded-blocks"))]
            {
                self.cpu.step_mode_deferred_count::<_, THUMB>(&mut self.bus);
                instructions += 1;
            }
            let exception = self.cpu.exceptions != exceptions;
            if exception {
                #[cfg(not(feature = "decoded-blocks"))]
                {
                    self.cpu.instructions =
                        self.cpu.instructions.wrapping_add(u64::from(instructions));
                    instructions = 0;
                }
                #[cfg(feature = "idle-loops")]
                {
                    self.idle = crate::idle::Detector::default();
                }
                // BIOS helpers may reset peripherals or use direct peeks and
                // internal I/O setters. Finish preceding instructions first.
                self.finish_cpu_slice();
                self.handle_replacement_exception(address, THUMB);
            }
            let clocks = self.bus.take_cycles();
            elapsed += clocks;
            self.bus.execution.pending += clocks;
            if elapsed >= horizon
                || self.bus.execution.barrier
                || exception
                || self.cpu.thumb() != THUMB
                || pending_irq && self.cpu.cpsr() & crate::cpu::I == 0
                || self.bus.memory_locked
                || clocks == 0
            {
                break;
            }
            #[cfg(feature = "idle-loops")]
            if monitor && self.cpu.r[15] <= address {
                #[cfg(not(feature = "decoded-blocks"))]
                {
                    self.cpu.instructions =
                        self.cpu.instructions.wrapping_add(u64::from(instructions));
                    instructions = 0;
                }
                let skipped =
                    self.idle
                        .observe(&mut self.cpu, &mut self.bus, address, elapsed, horizon);
                #[cfg(not(target_arch = "wasm32"))]
                {
                    skipped_clocks += skipped.clocks;
                }
                elapsed += skipped.clocks;
                self.bus.execution.pending += skipped.clocks;
                self.accelerated_instructions += skipped.instructions;
            }
        }
        #[cfg(not(feature = "decoded-blocks"))]
        {
            self.cpu.instructions = self.cpu.instructions.wrapping_add(u64::from(instructions));
        }
        self.finish_cpu_slice();
        #[cfg(all(feature = "idle-loops", not(target_arch = "wasm32")))]
        {
            self.bus.execution.monitor_idle = false;
            if monitor {
                self.idle_policy
                    .record(self.clocks, elapsed, skipped_clocks);
            }
        }
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

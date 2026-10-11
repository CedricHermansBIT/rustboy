//! Exact fixed-point loop acceleration inside one event-bounded CPU slice.
//!
//! Learn two actual iterations. Proofs survive ordinary scheduler events, but
//! never DMA/exception dispatch, unsafe accesses, restore or host calls.
//! A loop must have no writes and
//! only ordinary storage reads, with identical CPU and transient bus state at
//! its back edge. Its transition can then repeat without changing storage.
//! Whole iterations are batched strictly BEFORE the event horizon, retaining
//! the reference runner's final instruction and overshoot behavior.
use crate::{bus::Bus, cpu::Cpu, prefetch::Prefetch};

#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) struct BusState {
    pub prefetch: Prefetch,
    pub bios_latch: u32,
    pub previous_data: u32,
    pub previous_data_valid: bool,
    pub instruction_address: u32,
    pub instruction_thumb: bool,
    pub context_data: Option<u32>,
    pub cpu_data_access: bool,
    pub gamepak_instruction: bool,
    pub restart_fetch: bool,
    pub bios_access: bool,
}

struct Anchor {
    source: u32,
    cpu: Cpu,
    bus: BusState,
    elapsed: u64,
}

struct Proof {
    source: u32,
    cpu: Cpu,
    bus: BusState,
    period: u64,
    instructions: u64,
}

#[derive(Default)]
pub(crate) struct Detector {
    anchor: Option<Anchor>,
    proof: Option<Proof>,
}

#[derive(Default)]
pub(crate) struct Skipped {
    pub clocks: u64,
    pub instructions: u64,
}

impl Detector {
    pub fn start_slice(&mut self, bus: &mut Bus) {
        if std::mem::take(&mut bus.execution.idle_unsafe) {
            self.proof = None;
        }
        self.anchor = None;
    }

    pub fn observe(
        &mut self,
        cpu: &mut Cpu,
        bus: &mut Bus,
        source: u32,
        elapsed: u64,
        horizon: u64,
    ) -> Skipped {
        let unsafe_access = std::mem::take(&mut bus.execution.idle_unsafe);
        if unsafe_access {
            self.proof = None;
            self.anchor = None;
            return Skipped::default();
        }
        let mut skipped = Skipped::default();
        if let Some(proof) = &self.proof {
            if proof.source == source
                && cpu.same_execution_state(&proof.cpu)
                && bus.idle_state() == proof.bus
            {
                let rounds = (horizon - elapsed - 1) / proof.period;
                skipped.clocks = rounds * proof.period;
                skipped.instructions = rounds * proof.instructions;
                cpu.instructions = cpu.instructions.wrapping_add(skipped.instructions);
                return skipped;
            }
        }
        if let Some(anchor) = &self.anchor {
            if anchor.source == source
                && cpu.same_execution_state(&anchor.cpu)
                && bus.idle_state() == anchor.bus
            {
                let period = elapsed - anchor.elapsed;
                let instructions = cpu.instructions.wrapping_sub(anchor.cpu.instructions);
                if period != 0 && instructions != 0 {
                    self.proof = Some(Proof {
                        source,
                        cpu: cpu.clone(),
                        bus: bus.idle_state(),
                        period,
                        instructions,
                    });
                    let rounds = (horizon - elapsed - 1) / period;
                    skipped.clocks = rounds * period;
                    skipped.instructions = rounds * instructions;
                    cpu.instructions = cpu.instructions.wrapping_add(skipped.instructions);
                }
            }
        }
        self.anchor = Some(Anchor {
            source,
            cpu: cpu.clone(),
            bus: bus.idle_state(),
            elapsed: elapsed + skipped.clocks,
        });
        skipped
    }
}

/// Sample CPU-active work, then back off when it contains too little eligible
/// idle time to pay for detection. This changes only optimization policy.
#[cfg(not(target_arch = "wasm32"))]
#[derive(Default)]
pub(crate) struct Policy {
    observed: u64,
    skipped: u64,
    next_probe: u64,
}
#[cfg(not(target_arch = "wasm32"))]
impl Policy {
    pub fn enabled(&self, clocks: u64) -> bool {
        clocks >= self.next_probe
    }
    pub fn record(&mut self, clocks: u64, observed: u64, skipped: u64) {
        self.observed += observed;
        self.skipped += skipped;
        if self.observed >= crate::FRAME_CYCLES {
            if self.skipped < self.observed / 8 {
                self.next_probe = clocks.saturating_add(crate::FRAME_CYCLES * 30);
            }
            self.observed = 0;
            self.skipped = 0;
        }
    }
}
#[cfg(all(test, not(target_arch = "wasm32")))]
mod policy_tests {
    use super::*;
    #[test]
    fn low_yield_samples_back_off_and_later_reprobe() {
        let mut policy = Policy::default();
        assert!(policy.enabled(0));
        policy.record(crate::FRAME_CYCLES, crate::FRAME_CYCLES, 0);
        assert!(!policy.enabled(crate::FRAME_CYCLES * 2));
        assert!(policy.enabled(crate::FRAME_CYCLES * 31));
        policy.record(
            crate::FRAME_CYCLES * 32,
            crate::FRAME_CYCLES,
            crate::FRAME_CYCLES / 2,
        );
        assert!(policy.enabled(crate::FRAME_CYCLES * 33));
    }
}

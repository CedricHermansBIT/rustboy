//! Browser display refresh rates must not change the emulated clock rate.
#[derive(Default)]
pub struct FrameClock {
    previous_ms: Option<f64>,
    budget: f64,
}

impl FrameClock {
    pub fn target(&mut self, timestamp_ms: f64, speed: u32, paused: bool) -> u32 {
        let elapsed = self.previous_ms.replace(timestamp_ms)
            .map(|previous| (timestamp_ms - previous).clamp(0.0, 100.0)).unwrap_or(0.0);
        if paused { self.budget = 0.0; return 0; }
        // Cap catch-up after backgrounding; do not run a minute of missed work.
        self.budget += elapsed * 4194.304 * speed.clamp(1, 16) as f64;
        self.budget.max(0.0) as u32
    }

    pub fn consume(&mut self, ppu_t_cycles: u32) {
        // Keep instruction overshoot as negative debt for the next callback.
        self.budget -= ppu_t_cycles as f64;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn emulation_rate_does_not_depend_on_display_refresh() {
        for hz in [30, 60, 120, 144, 240] {
            let mut clock = FrameClock::default();
            let mut cycles = 0u64;
            for frame in 0..=hz {
                let target = clock.target(frame as f64 * 1000.0 / hz as f64, 1, false);
                let executed = target.div_ceil(4) * 4;
                clock.consume(executed);
                cycles += executed as u64;
            }
            assert!((4_194_300..=4_194_308).contains(&cycles), "refresh={hz}, cycles={cycles}");
        }
    }

    #[test]
    fn pause_and_backgrounding_do_not_accumulate_unbounded_work() {
        let mut clock = FrameClock::default();
        clock.target(0.0, 1, false);
        assert_eq!(clock.target(60_000.0, 1, true), 0);
        assert_eq!(clock.target(60_000.0, 1, false), 0);
        assert!(clock.target(120_000.0, 1, false) <= 419_431);
    }
}

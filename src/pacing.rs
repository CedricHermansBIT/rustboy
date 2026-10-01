//! Browser display refresh rates must not change the emulated clock rate.
#[derive(Default)]
pub struct FrameClock {
    previous_ms: Option<f64>,
    budget: f64,
}

impl FrameClock {
    pub fn target(&mut self, timestamp_ms: f64, speed: u32, paused: bool, clock_hz: u64) -> u64 {
        if !timestamp_ms.is_finite() {
            return 0;
        }
        let elapsed = self
            .previous_ms
            .replace(timestamp_ms)
            .map(|previous| (timestamp_ms - previous).clamp(0.0, 100.0))
            .unwrap_or(0.0);
        if paused {
            self.budget = 0.0;
            return 0;
        }
        // Cap catch-up after backgrounding; do not run a minute of missed work.
        self.budget += elapsed * (clock_hz as f64 / 1000.0) * speed.clamp(1, 16) as f64;
        self.budget.max(0.0) as u64
    }

    pub fn consume(&mut self, ticks: u64) {
        // Keep instruction overshoot as negative debt for the next callback.
        self.budget -= ticks as f64;
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
                let target = clock.target(frame as f64 * 1000.0 / hz as f64, 1, false, 4_194_304);
                let executed = target.div_ceil(4) * 4;
                clock.consume(executed);
                cycles += executed as u64;
            }
            assert!(
                (4_194_300..=4_194_308).contains(&cycles),
                "refresh={hz}, cycles={cycles}"
            );
        }
    }

    #[test]
    fn pause_and_backgrounding_do_not_accumulate_unbounded_work() {
        let mut clock = FrameClock::default();
        clock.target(0.0, 1, false, 4_194_304);
        assert_eq!(clock.target(60_000.0, 1, true, 4_194_304), 0);
        assert_eq!(clock.target(60_000.0, 1, false, 4_194_304), 0);
        assert!(clock.target(120_000.0, 1, false, 4_194_304) <= 419_431);
    }

    #[test]
    fn accepts_other_backend_clocks_and_retains_overshoot_debt() {
        let mut clock = FrameClock::default();
        assert_eq!(clock.target(0.0, 1, false, 1_000_000), 0);
        assert_eq!(clock.target(10.0, 1, false, 1_000_000), 10_000);
        clock.consume(10_004);
        assert_eq!(clock.target(20.0, 1, false, 1_000_000), 9_996);
        clock.consume(9_996);
        assert_eq!(clock.target(f64::NAN, 1, false, 1_000_000), 0);
        assert_eq!(clock.target(30.0, 2, false, 1_000_000), 20_000);
    }
}

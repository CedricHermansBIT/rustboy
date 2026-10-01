//! Frontend policy: wall-clock pacing and speed controls, not console timing.
use crate::emulator::{Emulator, RunResult, StopReason};
use crate::pacing::FrameClock;

pub struct Session {
    pub backend: Box<dyn Emulator>,
    pub speed: u32,
    clock: FrameClock,
    step_requested: bool,
}

impl Session {
    pub fn new(backend: Box<dyn Emulator>) -> Self {
        Self {
            backend,
            speed: 1,
            clock: FrameClock::default(),
            step_requested: false,
        }
    }

    pub fn tick(&mut self, timestamp_ms: f64) -> RunResult {
        let paused = self.backend.paused();
        let budget = self
            .clock
            .target(timestamp_ms, self.speed, paused, self.backend.clock_hz());
        if paused {
            if std::mem::take(&mut self.step_requested) {
                return self.backend.step();
            }
            return RunResult {
                ticks: 0,
                reason: StopReason::Paused,
            };
        }
        self.step_requested = false;
        let result = self.backend.run(budget);
        self.clock.consume(result.ticks);
        result
    }

    pub fn request_step(&mut self) {
        self.step_requested = true;
    }
    pub fn set_speed(&mut self, speed: u32) {
        self.speed = match speed {
            1 | 2 | 4 | 8 => speed,
            _ => 1,
        };
    }
    pub fn cycle_speed(&mut self) {
        self.set_speed(if self.speed == 8 { 1 } else { self.speed * 2 });
    }
    pub fn reset(&mut self) {
        self.backend.reset();
        self.clock = FrameClock::default();
        self.step_requested = false;
    }
    pub fn import_state(&mut self, data: &[u8]) -> Result<(), String> {
        self.backend.import_state(data)?;
        self.clock = FrameClock::default();
        self.step_requested = false;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::emulator::{AudioChunk, Button, PixelFormat, VideoFrame, VideoGeometry};

    // Test double only: intentionally not Game Boy-shaped and no debug hook.
    struct TestBackend {
        ticks: u64,
        paused: bool,
        pixels: Vec<u8>,
    }
    impl Emulator for TestBackend {
        fn system_name(&self) -> &'static str {
            "test double"
        }
        fn clock_hz(&self) -> u64 {
            1_000
        }
        fn title(&self) -> String {
            "test".into()
        }
        fn run(&mut self, budget: u64) -> RunResult {
            let ticks = budget.div_ceil(7) * 7;
            self.ticks += ticks;
            RunResult {
                ticks,
                reason: StopReason::BudgetExhausted,
            }
        }
        fn step(&mut self) -> RunResult {
            self.ticks += 7;
            RunResult {
                ticks: 7,
                reason: StopReason::Stepped,
            }
        }
        fn paused(&self) -> bool {
            self.paused
        }
        fn set_paused(&mut self, paused: bool) {
            self.paused = paused;
        }
        fn reset(&mut self) {
            self.ticks = 0;
            self.paused = false;
        }
        fn set_button(
            &mut self,
            port: usize,
            button: Button,
            _pressed: bool,
        ) -> Result<(), String> {
            if port == 3 && button == Button::X {
                Ok(())
            } else {
                Err("unsupported test input".into())
            }
        }
        fn video_frame(&mut self) -> VideoFrame<'_> {
            VideoFrame {
                geometry: VideoGeometry {
                    width: 320,
                    height: 200,
                    aspect_width: 4,
                    aspect_height: 3,
                },
                format: PixelFormat::Rgba8888,
                pixels: &self.pixels,
                enabled: true,
            }
        }
        fn drain_audio(&mut self) -> AudioChunk {
            AudioChunk {
                sample_rate: 32_000,
                channels: 1,
                samples: vec![0.25],
            }
        }
        fn state_id(&self) -> String {
            "test-state-v1".into()
        }
        fn export_state(&self) -> Vec<u8> {
            self.ticks.to_le_bytes().to_vec()
        }
        fn import_state(&mut self, data: &[u8]) -> Result<(), String> {
            self.ticks = u64::from_le_bytes(data.try_into().map_err(|_| "invalid test state")?);
            Ok(())
        }
    }
    fn session() -> Session {
        Session::new(Box::new(TestBackend {
            ticks: 0,
            paused: false,
            pixels: vec![0; 320 * 200 * 4],
        }))
    }

    #[test]
    fn generic_session_uses_backend_clock_and_actual_execution_debt() {
        let mut session = session();
        assert_eq!(session.tick(0.0).ticks, 0);
        assert_eq!(session.tick(10.0).ticks, 14);
        assert_eq!(session.tick(20.0).ticks, 7);
        session.set_speed(2);
        assert_eq!(session.tick(30.0).ticks, 21);
        session.set_speed(99);
        assert_eq!(session.speed, 1);
    }

    #[test]
    fn pause_and_single_step_do_not_accumulate_wall_clock_catchup() {
        let mut session = session();
        session.tick(0.0);
        session.backend.set_paused(true);
        assert_eq!(session.tick(60_000.0).reason, StopReason::Paused);
        session.request_step();
        assert_eq!(session.tick(60_010.0).reason, StopReason::Stepped);
        assert_eq!(session.tick(60_020.0).ticks, 0);
        session.backend.set_paused(false);
        assert_eq!(session.tick(60_030.0).ticks, 14);
    }

    #[test]
    fn reset_and_state_restore_rebase_pacing_and_clear_requested_steps() {
        let mut session = session();
        session.tick(0.0);
        session.tick(20.0);
        let saved = session.backend.export_state();
        session.request_step();
        session.import_state(&saved).unwrap();
        assert_eq!(session.tick(50_000.0).ticks, 0);
        assert_eq!(session.backend.export_state(), saved);
        session.reset();
        assert_eq!(session.tick(100_000.0).ticks, 0);
        assert_eq!(session.backend.export_state(), 0u64.to_le_bytes());
    }

    #[test]
    fn media_input_and_optional_debugging_are_not_game_boy_specific() {
        let mut session = session();
        assert!(session.backend.debug_extension().is_none());
        session.backend.set_button(3, Button::X, true).unwrap();
        let frame = session.backend.video_frame();
        assert_eq!((frame.geometry.width, frame.geometry.height), (320, 200));
        assert_eq!(
            (frame.geometry.aspect_width, frame.geometry.aspect_height),
            (4, 3)
        );
        let audio = session.backend.drain_audio();
        assert_eq!((audio.sample_rate, audio.channels), (32_000, 1));
        assert!(session.backend.save_info().is_none());
        assert!(session.backend.import_save(&[]).is_err());
    }
}

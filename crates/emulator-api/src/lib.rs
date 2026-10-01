//! Small, platform-independent contract between consoles and their frontends.
//!
//! Ticks belong to each backend's reported presentation clock, not its CPU.
//! Internal bus/CPU/PPU scheduling is deliberately outside this interface.
use std::any::Any;

/// Host-owned services, injected per machine. Callbacks are never serialized.
#[derive(Clone, Copy)]
pub struct HostServices {
    pub now_unix_seconds: fn() -> u64,
    pub log: Option<fn(&str)>,
}

impl Default for HostServices {
    fn default() -> Self {
        Self {
            now_unix_seconds: default_unix_seconds,
            log: None,
        }
    }
}

fn default_unix_seconds() -> u64 {
    #[cfg(not(target_arch = "wasm32"))]
    {
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map_or(0, |time| time.as_secs())
    }
    // WASM hosts inject a wall clock. Unconfigured headless hosts use a frozen
    // deterministic clock rather than making an unsupported SystemTime call.
    #[cfg(target_arch = "wasm32")]
    {
        0
    }
}

impl HostServices {
    pub fn log(&self, arguments: std::fmt::Arguments<'_>) {
        if let Some(log) = self.log {
            log(&arguments.to_string());
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct VideoGeometry {
    pub width: u32,
    pub height: u32,
    pub aspect_width: u32,
    pub aspect_height: u32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PixelFormat {
    /// Four bytes per pixel in R, G, B, A order (independent of host endianness).
    Rgba8888,
}

pub struct VideoFrame<'a> {
    pub geometry: VideoGeometry,
    pub format: PixelFormat,
    pub pixels: &'a [u8],
    pub enabled: bool,
}

pub struct AudioChunk {
    pub sample_rate: u32,
    pub channels: u8,
    /// Interleaved samples; complete sample frames only.
    pub samples: Vec<f32>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Button {
    Up,
    Down,
    Left,
    Right,
    A,
    B,
    X,
    Y,
    Start,
    Select,
    L,
    R,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StopReason {
    BudgetExhausted,
    Paused,
    Breakpoint,
    Stepped,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RunResult {
    /// Actual elapsed presentation ticks, including instruction overshoot.
    pub ticks: u64,
    pub reason: StopReason,
}

pub struct SaveInfo {
    pub key: String,
    pub dirty: bool,
}

/// Implementations must remain usable without a window, browser or audio device.
/// Frames and audio are polled; persistence is performed by the host.
/// No NES/SNES support is implied by the existence of this interface.
pub trait Emulator {
    fn system_name(&self) -> &'static str;
    fn clock_hz(&self) -> u64;
    fn title(&self) -> String;
    /// Stable persistence namespace, even for games without battery RAM.
    fn save_key(&self) -> String {
        self.save_info()
            .map(|info| info.key)
            .unwrap_or_else(|| self.state_id())
    }
    fn run(&mut self, ticks: u64) -> RunResult;
    /// Advance one backend-defined debug step, even while paused.
    fn step(&mut self) -> RunResult;
    fn paused(&self) -> bool;
    fn set_paused(&mut self, paused: bool);
    fn reset(&mut self);
    fn set_button(&mut self, port: usize, button: Button, pressed: bool) -> Result<(), String>;
    fn video_frame(&mut self) -> VideoFrame<'_>;
    fn drain_audio(&mut self) -> AudioChunk;
    fn save_info(&self) -> Option<SaveInfo> {
        None
    }
    fn export_save(&self) -> Vec<u8> {
        Vec::new()
    }
    fn import_save(&mut self, _data: &[u8]) -> Result<(), String> {
        Err("Backend has no battery save storage".into())
    }
    fn mark_save_clean(&mut self) {}
    fn clear_save(&mut self) {}
    /// Versioning, validation and hardware/ROM identity are backend-owned.
    fn state_id(&self) -> String;
    fn export_state(&self) -> Vec<u8>;
    fn import_state(&mut self, data: &[u8]) -> Result<(), String>;
    /// Optional, explicitly console-specific debug extension. The normal
    /// execution/rendering/input paths must not downcast through this hook.
    fn debug_extension(&self) -> Option<&dyn Any> {
        None
    }
    fn debug_extension_mut(&mut self) -> Option<&mut dyn Any> {
        None
    }
}

//! Small, platform-independent contract between a console and its frontend.
//!
//! Ticks belong to each backend's reported presentation clock, not its CPU.
//! Internal bus/CPU/PPU scheduling is deliberately outside this interface.
use std::any::Any;

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
    Up, Down, Left, Right, A, B, X, Y, Start, Select, L, R,
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
    fn run(&mut self, ticks: u64) -> RunResult;
    /// Advance one backend-defined debug step, even while paused.
    fn step(&mut self) -> RunResult;
    fn paused(&self) -> bool;
    fn set_paused(&mut self, paused: bool);
    fn reset(&mut self);
    fn set_button(&mut self, port: usize, button: Button, pressed: bool) -> Result<(), String>;
    fn video_frame(&mut self) -> VideoFrame<'_>;
    fn drain_audio(&mut self) -> AudioChunk;
    fn save_info(&self) -> Option<SaveInfo>;
    fn export_save(&self) -> Vec<u8>;
    fn import_save(&mut self, data: &[u8]) -> Result<(), String>;
    fn mark_save_clean(&mut self);
    fn clear_save(&mut self);
    /// Versioning, validation and hardware/ROM identity are backend-owned.
    fn state_id(&self) -> String;
    fn export_state(&self) -> Vec<u8>;
    fn import_state(&mut self, data: &[u8]) -> Result<(), String>;
    /// Optional, explicitly console-specific debug extension. The normal
    /// execution/rendering/input paths must not downcast through this hook.
    fn debug_extension(&self) -> Option<&dyn Any> { None }
    fn debug_extension_mut(&mut self) -> Option<&mut dyn Any> { None }
}

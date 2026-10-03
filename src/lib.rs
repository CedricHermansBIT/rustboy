pub mod pacing;
pub mod session;

pub use rustboy_emulator_api as emulator;
pub use rustboy_gameboy as gameboy;
pub use rustboy_gba as gba;
// Preserve the existing native test/debug API while relocating the core.
pub use rustboy_gameboy::{apu, boot_roms, cartridge, cpu, debug_tracer, mbc, ppu};

#[cfg(target_arch = "wasm32")]
mod web;

#[cfg(target_arch = "wasm32")]
pub use web::*;

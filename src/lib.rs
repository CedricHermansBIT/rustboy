pub mod cpu;
pub mod cartridge;
pub mod pacing;
pub mod mbc;
pub mod ppu;
pub mod apu;
pub mod debug_tracer;
pub mod emulator;
pub mod gameboy;
pub mod session;

#[cfg(target_arch = "wasm32")]
mod web;

#[cfg(target_arch = "wasm32")]
pub use web::*;

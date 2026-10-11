//! Original GBA implementation, based on hardware documentation, not emulator
//! source. Still under development: this is not a claim of game compatibility.
mod apu;
mod backend;
mod backup;
mod bios;
pub mod bus;
pub mod cpu;
#[cfg(any(test, feature = "decoded-blocks"))]
mod decoded;
mod eeprom;
mod execution;
#[cfg(feature = "idle-loops")]
mod idle;
#[cfg(feature = "jit")]
pub mod jit;
pub mod machine;
mod peripherals;
mod ppu;
mod prefetch;
mod psg;
#[cfg(feature = "render-cache")]
mod render_cache;
pub use backend::validate_cartridge;
pub use machine::GameBoyAdvance;

pub const CLOCK_HZ: u64 = 16_777_216;
pub const FRAME_CYCLES: u64 = 280_896;

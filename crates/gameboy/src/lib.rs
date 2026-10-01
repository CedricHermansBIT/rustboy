//! Game Boy hardware and its portable frontend adapter.
//! No DOM, Web Audio, window or persistence APIs belong in this crate.
pub mod apu;
mod backend;
pub mod boot_roms;
pub mod cartridge;
pub mod cpu;
pub mod debug_tracer;
pub mod mbc;
pub mod ppu;
pub mod sgb;
#[cfg(test)]
mod sgb_integration_tests;

pub use backend::{GameBoy, HardwareModel};
pub use rustboy_emulator_api as emulator;

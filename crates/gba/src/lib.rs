//! Original GBA implementation, based on hardware documentation, not emulator
//! source. Still under development: this is not a claim of game compatibility.
pub mod bus;
pub mod cpu;
pub mod machine;
pub use machine::GameBoyAdvance;

pub const CLOCK_HZ: u64 = 16_777_216;
pub const FRAME_CYCLES: u64 = 280_896;

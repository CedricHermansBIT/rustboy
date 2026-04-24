pub mod cpu;
pub mod ppu;
pub mod apu;
pub mod debug_tracer;

#[cfg(target_arch = "wasm32")]
mod web;

#[cfg(target_arch = "wasm32")]
pub use web::*;

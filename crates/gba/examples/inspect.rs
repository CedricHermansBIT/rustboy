//! Bounded diagnostic, not a game player. No game or proprietary BIOS bundled.
use rustboy_gba::{GameBoyAdvance, CLOCK_HZ, FRAME_CYCLES};
use std::{error::Error, time::Instant};
fn main() -> Result<(), Box<dyn Error>> {
    let mut args = std::env::args().skip(1);
    let path=args.next().ok_or("usage: cargo run --release -p rustboy-gba --example inspect -- <rom.gba> [frames=60] [bios.bin]")?;
    let frames: u64 = args.next().map(|s| s.parse()).transpose()?.unwrap_or(60);
    if frames > 36000 {
        return Err("diagnostic is limited to 36,000 frames".into());
    }
    let bios = args.next().map(std::fs::read).transpose()?;
    let mut machine = GameBoyAdvance::new(std::fs::read(path)?, bios)?;
    println!(
        "GBA development backend: {} (graphics/audio/BIOS services incomplete)",
        machine.title()
    );
    let start = Instant::now();
    machine.run(FRAME_CYCLES * frames);
    let elapsed = start.elapsed().as_secs_f64();
    println!(
        "{} clocks, {} instructions, {} exceptions in {:.3}s ({:.2}x clock throughput)",
        machine.clocks,
        machine.cpu.instructions,
        machine.cpu.exceptions,
        elapsed,
        machine.clocks as f64 / CLOCK_HZ as f64 / elapsed
    );
    println!(
        "PC={:08X}, CPSR={:08X}, display={:04X}, VCOUNT={}, frames={}",
        machine.cpu.r[15],
        machine.cpu.cpsr(),
        machine.bus.io16(0),
        machine.bus.io16(6),
        machine.frames
    );
    if let Some(fault) = machine.fault {
        println!("Stopped at {:08X}: exception vector {:02X} needs BIOS services (not implemented/bundled yet).",fault.instruction_address,fault.vector);
    }
    Ok(())
}

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
    let trace_range = std::env::var("GBA_TRACE_PC")
        .ok()
        .map(|value| {
            let (start, end) = value
                .split_once("..")
                .ok_or("GBA_TRACE_PC expects hexadecimal start..end")?;
            Ok::<_, Box<dyn Error>>((
                u32::from_str_radix(start.trim_start_matches("0x"), 16)?,
                u32::from_str_radix(end.trim_start_matches("0x"), 16)?,
            ))
        })
        .transpose()?;
    if std::env::var_os("GBA_TRACE_IRQ").is_some() || trace_range.is_some() {
        let mut traced = false;
        let mut remaining = 0u32;
        let mut window_lines = 128;
        while machine.clocks < FRAME_CYCLES * frames && machine.fault.is_none() {
            let old_exceptions = machine.cpu.exceptions;
            let in_window = window_lines > 0
                && trace_range
                    .is_some_and(|(start, end)| (start..end).contains(&machine.cpu.r[15]));
            if remaining > 0 || in_window {
                let pc = machine.cpu.r[15];
                println!("trace PC={pc:08X} op={:08X} CPSR={:08X} r0..5={:08X?} r12={:08X} SP={:08X} LR={:08X}",machine.bus.peek32(pc),machine.cpu.cpsr(),&machine.cpu.r[..6],machine.cpu.r[12],machine.cpu.r[13],machine.cpu.r[14]);
                remaining = remaining.saturating_sub(1);
                if in_window {
                    window_lines -= 1;
                }
            }
            machine.step();
            if trace_range.is_none()
                && !traced
                && old_exceptions != machine.cpu.exceptions
                && machine.cpu.cpsr() & 31 == 0x12
            {
                remaining = 128;
                traced = true;
            }
        }
    } else {
        machine.run(FRAME_CYCLES * frames);
    }
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
    let handler = machine.bus.peek32(0x03007ffc);
    println!(
        "IRQ IE={:04X} IF={:04X} IME={} DISPSTAT={:04X} handler={:08X}",
        machine.bus.io16(0x200),
        machine.bus.io16(0x202),
        machine.bus.io16(0x208),
        machine.bus.io16(4),
        handler
    );
    println!(
        "handler words: {:08X} {:08X} {:08X}; regs={:08X?}",
        machine.bus.peek32(handler),
        machine.bus.peek32(handler.wrapping_add(4)),
        machine.bus.peek32(handler.wrapping_add(8)),
        machine.cpu.r
    );
    if let Some(fault) = machine.fault {
        println!(
            "Stopped at {:08X}: exception vector {:02X}: {}.",
            fault.instruction_address, fault.vector, fault.reason
        );
    }
    Ok(())
}

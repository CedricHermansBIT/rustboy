//! Bounded diagnostic, not a game player. No game or proprietary BIOS bundled.
use rustboy_emulator_api::Emulator;
use rustboy_gba::{GameBoyAdvance, CLOCK_HZ, FRAME_CYCLES};
use std::{error::Error, io::Write, time::Instant};

/// Relative emulated frame : complete pressed-key mask (GBA KEYINPUT order).
/// Complete masks make overlapping/released buttons explicit and deterministic.
fn input_sequence(value: &str, frames: u64) -> Result<Vec<(u64, u16)>, Box<dyn Error>> {
    let mut events = Vec::new();
    for entry in value.split(',') {
        if events.len() >= 256 {
            return Err("GBA_INPUT_SEQUENCE is limited to 256 events".into());
        }
        let (frame, mask) = entry.split_once(':').ok_or("Use GBA_INPUT_SEQUENCE=frame:hexmask,...")?;
        let frame: u64 = frame.parse()?;
        let mask = u16::from_str_radix(mask.trim_start_matches("0x"), 16)?;
        if frame >= frames || mask > 0x3ff || events.last().is_some_and(|&(previous, _)| previous >= frame) {
            return Err("Input frames must be increasing and within the run; key mask must be <=03ff".into());
        }
        events.push((frame, mask));
    }
    Ok(events)
}
fn run_inputs(machine: &mut GameBoyAdvance, budget: u64, events: &[(u64, u16)]) -> Result<(), Box<dyn Error>> {
    let start = machine.clocks;
    for &(frame, mask) in events {
        let target = frame * FRAME_CYCLES;
        let elapsed = machine.clocks.wrapping_sub(start);
        if elapsed < target {
            machine.run(target - elapsed);
        }
        if machine.fault.is_some() || machine.clocks.wrapping_sub(start) < target {
            return Err("Input sequence stopped before its next event".into());
        }
        machine.bus.set_keys(mask);
    }
    let elapsed = machine.clocks.wrapping_sub(start);
    if elapsed < budget {
        machine.run(budget - elapsed);
    }
    if machine.fault.is_some() || machine.clocks.wrapping_sub(start) < budget {
        return Err("Input sequence did not finish its requested workload".into());
    }
    Ok(())
}
fn main() -> Result<(), Box<dyn Error>> {
    let mut args = std::env::args().skip(1);
    let path=args.next().ok_or("usage: cargo run --release -p rustboy-gba --example inspect -- <rom.gba> [frames=60] [bios.bin]")?;
    let frames: u64 = args.next().map(|s| s.parse()).transpose()?.unwrap_or(60);
    if frames > 36000 {
        return Err("diagnostic is limited to 36,000 frames".into());
    }
    let bios = args.next().map(std::fs::read).transpose()?;
    if args.next().is_some() {
        return Err("Unexpected argument after BIOS path".into());
    }
    let mut machine = GameBoyAdvance::new(std::fs::read(path)?, bios)?;
    if let Some(path) = std::env::var_os("GBA_STATE") {
        machine.import_state(&std::fs::read(path)?)?;
    }
    println!(
        "GBA development backend: {} (graphics/audio/BIOS services incomplete)",
        machine.title()
    );
    let start = Instant::now();
    let initial_clocks = machine.clocks;
    let initial_instructions = machine.cpu.instructions;
    let initial_exceptions = machine.cpu.exceptions;
    let budget = FRAME_CYCLES * frames;
    let start_at = std::env::var("GBA_START_AT")
        .ok()
        .map(|v| v.parse::<u64>())
        .transpose()?;
    let trace_irq = std::env::var_os("GBA_TRACE_IRQ").is_some();
    let watch = std::env::var("GBA_TRACE_MEMORY")
        .ok()
        .map(|v| u32::from_str_radix(v.trim_start_matches("0x"), 16))
        .transpose()?;
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
    let sequence = std::env::var("GBA_INPUT_SEQUENCE").ok()
        .map(|value| input_sequence(&value, frames)).transpose()?;
    if sequence.is_some() && (trace_irq || trace_range.is_some() || watch.is_some() || start_at.is_some()) {
        return Err("Input sequences cannot be combined with tracing or GBA_START_AT".into());
    }
    if let Some(events) = sequence {
        run_inputs(&mut machine, budget, &events)?;
    } else if trace_irq || trace_range.is_some() || watch.is_some() || start_at.is_some() {
        let mut traced = false;
        let mut remaining = 0u32;
        let mut window_lines = 128;
        let mut watch_lines = 128;
        while machine.clocks.wrapping_sub(initial_clocks) < budget && machine.fault.is_none() {
            if let Some(frame) = start_at {
                let pressed = (frame.saturating_mul(FRAME_CYCLES)
                    ..frame.saturating_add(5).saturating_mul(FRAME_CYCLES))
                    .contains(&machine.clocks);
                machine.bus.set_keys(if pressed { 1 << 3 } else { 0 });
            }
            let old_pc = machine.cpu.r[15];
            let old_value = watch.map(|addr| machine.bus.peek16(addr));
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
            if machine.step() == 0 {
                break;
            }
            if let Some(addr) = watch {
                let value = machine.bus.peek16(addr);
                if watch_lines > 0 && old_value != Some(value) {
                    println!(
                        "watch {addr:08X}: {:04X}->{value:04X} PC={old_pc:08X} clocks={}",
                        old_value.unwrap(),
                        machine.clocks
                    );
                    watch_lines -= 1;
                }
            }
            if trace_irq
                && trace_range.is_none()
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
        machine.clocks.wrapping_sub(initial_clocks),
        machine.cpu.instructions.wrapping_sub(initial_instructions),
        machine.cpu.exceptions.wrapping_sub(initial_exceptions),
        elapsed,
        machine.clocks.wrapping_sub(initial_clocks) as f64 / CLOCK_HZ as f64 / elapsed
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
    println!("PPU BGCNT={:04X?} WINH={:04X?} WINV={:04X?} WININ={:04X} WINOUT={:04X} BLDCNT={:04X} BLDALPHA={:04X} BLDY={:04X}",
        [8, 10, 12, 14].map(|offset| machine.bus.io16(offset)),
        [0x40, 0x42].map(|offset| machine.bus.io16(offset)),
        [0x44, 0x46].map(|offset| machine.bus.io16(offset)),
        machine.bus.io16(0x48), machine.bus.io16(0x4a), machine.bus.io16(0x50),
        machine.bus.io16(0x52), machine.bus.io16(0x54));
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
    if let Some(path) = std::env::var_os("GBA_FRAME_PNG") {
        let file = std::fs::OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(path)?;
        let mut encoder = png::Encoder::new(file, 240, 160);
        encoder.set_color(png::ColorType::Rgba);
        encoder.set_depth(png::BitDepth::Eight);
        encoder
            .write_header()?
            .write_image_data(machine.video_frame().pixels)?;
    }
    if let Some(fault) = machine.fault {
        println!(
            "Stopped at {:08X}: exception vector {:02X}: {}.",
            fault.instruction_address, fault.vector, fault.reason
        );
        return Err("Diagnostic stopped on a CPU/BIOS fault".into());
    }
    if machine.clocks.wrapping_sub(initial_clocks) < budget {
        return Err("Diagnostic did not finish its requested workload".into());
    }
    if let Some(path) = std::env::var_os("GBA_SAVE_STATE") {
        std::fs::OpenOptions::new().write(true).create_new(true).open(path)?
            .write_all(&machine.export_state())?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn input_masks_have_bounded_increasing_frame_times() {
        assert_eq!(input_sequence("0:0008,5:0000,120:0x0001,125:0000", 130).unwrap(),
            [(0, 8), (5, 0), (120, 1), (125, 0)]);
        for value in ["", "0", "a:0", "0:xyz", "0:400", "130:0", "5:0,5:1", "5:0,4:1"] {
            assert!(input_sequence(value, 130).is_err(), "{value}");
        }
        let many = (0..257).map(|frame| format!("{frame}:0")).collect::<Vec<_>>().join(",");
        assert!(input_sequence(&many, 300).is_err());
    }
    #[test]
    fn input_timeline_replays_instruction_boundaries_keypad_irq_and_clock_wrap() {
        use rustboy_gba::cpu::Memory;
        for start in [0, u64::MAX - 100] {
            let mut rom = vec![0; 192];
            for (index, op) in [0xe1d100b0u32, 0xe5820000, 0xeafffffc].into_iter().enumerate() {
                rom[index * 4..index * 4 + 4].copy_from_slice(&op.to_le_bytes());
            }
            let mut sliced = GameBoyAdvance::new(rom.clone(), None).unwrap();
            let mut reference = GameBoyAdvance::new(rom, None).unwrap();
            for machine in [&mut sliced, &mut reference] {
                machine.clocks = start;
                machine.cpu.r[1] = 0x04000130;
                machine.cpu.r[2] = 0x03000100;
                machine.bus.write16(0x04000132, 0x4001, false);
                machine.bus.take_cycles();
            }
            let events = input_sequence("0:0001,5:0000,8:0100", 9).unwrap();
            run_inputs(&mut sliced, FRAME_CYCLES * 9, &events).unwrap();
            for &(frame, mask) in &events {
                while reference.clocks.wrapping_sub(start) < frame * FRAME_CYCLES {
                    assert_ne!(reference.step(), 0);
                }
                reference.bus.set_keys(mask);
            }
            while reference.clocks.wrapping_sub(start) < FRAME_CYCLES * 9 {
                assert_ne!(reference.step(), 0);
            }
            assert_eq!(sliced.export_state(), reference.export_state());
            assert_eq!(sliced.bus.peek16(0x03000100), 0x02ff);
            assert_ne!(sliced.bus.io16(0x202) & 0x1000, 0);
        }
    }
}

//! Local instruction mix/hot-PC diagnostic. Uses reference stepping, never
//! reports its instrumented runtime as an emulation benchmark.
use rustboy_emulator_api::Emulator;
use rustboy_gba::{GameBoyAdvance, FRAME_CYCLES};
use std::{collections::HashMap, error::Error};

fn hash(bytes: &[u8]) -> u64 {
    bytes.iter().fold(0xcbf29ce484222325, |h, b| {
        (h ^ u64::from(*b)).wrapping_mul(0x100000001b3)
    })
}
fn family(thumb: bool, op: u32) -> &'static str {
    if thumb {
        match op >> 13 {
            0 if op & 0x1800 == 0x1800 => "Thumb add/subtract",
            0 => "Thumb immediate shift",
            1 => "Thumb immediate ALU",
            2 if op & 0xfc00 == 0x4000 => "Thumb register ALU",
            2 if op & 0xfc00 == 0x4400 => "Thumb high-register/BX",
            2 if op & 0xf800 == 0x4800 => "Thumb literal load",
            2 => "Thumb register-offset memory",
            3 => "Thumb immediate-offset word/byte memory",
            4 if op & 0x1000 == 0 => "Thumb immediate-offset halfword memory",
            4 => "Thumb SP-relative memory",
            5 if op & 0x1000 == 0 => "Thumb address calculation",
            5 if op & 0xff00 == 0xb000 => "Thumb SP adjustment",
            5 if op & 0xf600 == 0xb400 => "Thumb push/pop",
            5 => "Thumb reserved",
            6 if op & 0x1000 == 0 => "Thumb multiple transfer",
            6 if op >> 8 & 15 == 15 => "Thumb SWI",
            6 if op >> 8 & 15 == 14 => "Thumb reserved",
            6 => "Thumb conditional branch",
            _ => "Thumb branch/link/reserved",
        }
    } else {
        // Major encoding classes, not a complete ARM special-op decoder.
        match op >> 25 & 7 {
            0 | 1 => "ARM ALU/PSR/multiply/halfword",
            2 | 3 => "ARM single transfer",
            4 => "ARM multiple transfer",
            5 => "ARM branch",
            _ => "ARM coprocessor/SWI/reserved",
        }
    }
}
fn main() -> Result<(), Box<dyn Error>> {
    let mut args = std::env::args().skip(1);
    let path = args
        .next()
        .ok_or("usage: instruction_profile <rom.gba> <checked.state> [frames=120] [bios.bin]")?;
    let state_path = args
        .next()
        .ok_or("Supply a checked state from the selected ROM/firmware")?;
    let frames: u64 = args
        .next()
        .map(|arg| arg.parse())
        .transpose()?
        .unwrap_or(120);
    let bios = args.next().map(std::fs::read).transpose()?;
    if args.next().is_some() || !(1..=36000).contains(&frames) {
        return Err("Use 1..36000 frames".into());
    }
    let rom = std::fs::read(path)?;
    let mut machine = GameBoyAdvance::new(rom, bios)?;
    machine.import_state(&std::fs::read(state_path)?)?;
    if machine.fault.is_some() || machine.paused() {
        return Err("Cannot profile a faulted or paused state".into());
    }
    let baseline = hash(&machine.export_state());
    let start_clocks = machine.clocks;
    let start_instructions = machine.cpu.instructions;
    let budget = FRAME_CYCLES * frames;
    let mut families = HashMap::<&str, u64>::new();
    let mut pcs = HashMap::<(u32, bool, u32), (u64, [u32; 4])>::new();
    let mut not_decoded = 0;
    let mut unrecorded_pc_samples = 0;
    while machine.clocks.wrapping_sub(start_clocks) < budget && machine.fault.is_none() {
        let pc = machine.cpu.r[15];
        let thumb = machine.cpu.thumb();
        // BIOS protection, open bus and EEPROM instruction reads need real bus
        // context/side effects. Do not pretend a pre-step peek decodes them.
        let decodable = matches!(pc >> 24, 2 | 3 | 5..=12);
        let op = if !decodable {
            0
        } else if thumb {
            u32::from(machine.bus.peek16(pc & !1))
        } else {
            machine.bus.peek32(pc & !3)
        };
        let before = machine.cpu.instructions;
        let registers: [u32; 4] = machine.cpu.r[..4].try_into().unwrap();
        let clocks = machine.step();
        if machine.cpu.instructions != before {
            if decodable {
                *families.entry(family(thumb, op)).or_default() += 1;
                let key = (pc, thumb, op);
                if let Some(entry) = pcs.get_mut(&key) {
                    entry.0 += 1;
                } else if pcs.len() < 65536 {
                    pcs.insert(key, (1, registers));
                } else {
                    unrecorded_pc_samples += 1;
                }
            } else {
                not_decoded += 1;
            }
        }
        if clocks == 0 {
            break;
        }
    }
    let instructions = machine.cpu.instructions.wrapping_sub(start_instructions);
    let clocks = machine.clocks.wrapping_sub(start_clocks);
    if clocks < budget || machine.fault.is_some() {
        return Err(format!("Workload stopped early: {:?}", machine.fault).into());
    }
    if families.values().sum::<u64>() + not_decoded != instructions {
        return Err("Instruction accounting did not match reference execution".into());
    }
    println!("instruction profile (instrumented reference stepping, NOT a speed benchmark)");
    println!("baseline={baseline:016x} clocks={clocks} instructions={instructions} result={:016x} undecoded={not_decoded}", hash(&machine.export_state()));
    let mut families: Vec<_> = families.into_iter().collect();
    families.sort_by(|a, b| b.1.cmp(&a.1).then(a.0.cmp(b.0)));
    for (family, count) in families {
        println!(
            "{:.2}% {count:>10} {family}",
            100.0 * count as f64 / instructions as f64
        );
    }
    println!("Hot instruction PCs (mode and encoding included):");
    println!("Distinct-PC map capped at 65536 entries; unrecorded samples={unrecorded_pc_samples}");
    let mut pcs: Vec<_> = pcs.into_iter().collect();
    pcs.sort_by(|a, b| b.1 .0.cmp(&a.1 .0).then(a.0.cmp(&b.0)));
    for ((pc, thumb, op), (count, registers)) in pcs.into_iter().take(20) {
        println!(
            "{:.2}% {count:>10} {} {pc:08x} {op:08x}",
            100.0 * count as f64 / instructions as f64,
            if thumb { "Thumb" } else { "ARM" }
        );
        println!("  first-hit r0..r3={:08x?}", &registers[..4]);
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn instruction_mix_labels_match_known_encodings_and_cover_all_thumb_words() {
        for (op, expected) in [
            (0x8b91, "Thumb immediate-offset halfword memory"),
            (0x1c18, "Thumb add/subtract"),
            (0x4008, "Thumb register ALU"),
            (0x2800, "Thumb immediate ALU"),
            (0xd0fa, "Thumb conditional branch"),
            (0xde00, "Thumb reserved"),
            (0xdf00, "Thumb SWI"),
            (0x4800, "Thumb literal load"),
        ] {
            assert_eq!(family(true, op), expected);
        }
        for op in 0..=u16::MAX {
            assert!(family(true, u32::from(op)).starts_with("Thumb "));
        }
        assert_eq!(family(false, 0xe00e0199), "ARM ALU/PSR/multiply/halfword");
        assert_eq!(family(false, 0x0a000007), "ARM branch");
    }
}

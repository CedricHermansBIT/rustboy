//! Opt-in regression of locally supplied games/checkpoints, not compatibility
//! proof. Scene labels are supplied by the manifest, never inferred from boot.
use rustboy_emulator_api::Emulator;
use rustboy_gba::{GameBoyAdvance, FRAME_CYCLES};
use std::{error::Error, fs, path::Path};
fn hash(bytes: &[u8]) -> u64 {
    bytes.iter().fold(0xcbf29ce484222325, |h, b| {
        (h ^ u64::from(*b)).wrapping_mul(0x100000001b3)
    })
}
fn timeline(input: &str, frames: u64) -> Result<Vec<(u64, u16)>, Box<dyn Error>> {
    if input == "-" {
        return Ok(Vec::new());
    }
    let mut result = Vec::new();
    for entry in input.split(',') {
        let (frame, mask) = entry
            .split_once(':')
            .ok_or("Input timeline requires frame:hexmask")?;
        let frame: u64 = frame.parse()?;
        let mask = u16::from_str_radix(mask, 16)?;
        if frame >= frames || mask > 0x3ff || result.last().is_some_and(|(last, _)| *last >= frame)
        {
            return Err("Input timeline is unordered or out of range".into());
        }
        result.push((frame, mask));
    }
    Ok(result)
}
fn exercise(
    machine: &mut GameBoyAdvance,
    frames: u64,
    inputs: &[(u64, u16)],
) -> Result<(), Box<dyn Error>> {
    let mut cursor = 0;
    for frame in 0..frames {
        if inputs.get(cursor).is_some_and(|(at, _)| *at == frame) {
            machine.bus.set_keys(inputs[cursor].1);
            cursor += 1;
        }
        if machine.run(FRAME_CYCLES) < FRAME_CYCLES || machine.fault.is_some() {
            return Err(format!("Game stopped at frame {frame}: {:?}", machine.fault).into());
        }
        let audio = machine.drain_audio();
        if audio.sample_rate != 32768
            || audio.channels != 2
            || audio.samples.len() % 2 != 0
            || audio
                .samples
                .iter()
                .any(|s| !s.is_finite() || s.abs() > 1.0)
        {
            return Err(format!("Invalid audio at frame {frame}").into());
        }
    }
    Ok(())
}
fn run() -> Result<(), Box<dyn Error>> {
    let mut args = std::env::args().skip(1);
    let rom_path = args.next().ok_or("usage: gameplay_regression <rom.gba> <frames> <frame:hexmask,... or -> [checkpoint or -] [output-directory]")?;
    let frames: u64 = args.next().ok_or("Frames missing")?.parse()?;
    if !(1..=36000).contains(&frames) {
        return Err("Run must contain 1–36000 frames".into());
    }
    let inputs = timeline(&args.next().ok_or("Input timeline missing")?, frames)?;
    let checkpoint = args.next().unwrap_or_else(|| "-".into());
    let output = args.next();
    if args.next().is_some() {
        return Err("Unexpected gameplay argument".into());
    }
    let rom = fs::read(rom_path)?;
    let mut m = GameBoyAdvance::new(rom.clone(), None)?;
    if checkpoint != "-" {
        m.import_state(&fs::read(checkpoint)?)?;
    }
    m.mark_save_clean();
    let original = m.export_state();
    exercise(&mut m, frames, &inputs)?;
    let expected = m.export_state();
    let pixels = m.video_frame().pixels.to_vec();
    let battery = m.export_save();
    let save_type = m.save_type();
    let key = m.save_key();
    // Restore the same checkpoint and replay the exact inputs: compare hardware,
    // displayed pixels and battery data, not only CPU registers.
    m.import_state(&original)?;
    m.mark_save_clean();
    exercise(&mut m, frames, &inputs)?;
    if m.export_state() != expected || m.export_save() != battery {
        return Err("Checkpoint/input replay is nondeterministic".into());
    }
    let mut fresh = GameBoyAdvance::new(rom, None)?;
    fresh.set_save_type(save_type)?;
    if !battery.is_empty() {
        fresh.import_save(&battery)?;
    }
    if fresh.export_save() != battery || fresh.save_key() != key {
        return Err("Battery export/reload changed cartridge progress or identity".into());
    }
    if let Ok(expected_hash) = std::env::var("GBA_EXPECT_FRAME_HASH") {
        if hash(&pixels) != u64::from_str_radix(&expected_hash, 16)? {
            return Err("Final frame differs from the supplied scene reference".into());
        }
    }
    if let Some(directory) = output {
        fs::create_dir_all(&directory)?;
        let path = Path::new(&directory);
        fs::write(path.join("state.bin"), &expected)?;
        fs::write(path.join("battery.sav"), &battery)?;
        let mut encoder = png::Encoder::new(fs::File::create(path.join("frame.png"))?, 240, 160);
        encoder.set_color(png::ColorType::Rgba);
        encoder.set_depth(png::BitDepth::Eight);
        encoder.write_header()?.write_image_data(&pixels)?;
    }
    println!("RESULT\tgameplay\tPASS\t{frames} frames; deterministic checkpoint/input replay; finite audio; battery reload; frame={:016x}; scene coverage requires manifest/reference review", hash(&pixels));
    Ok(())
}
fn main() {
    if let Err(error) = run() {
        println!("RESULT\tgameplay\tFAIL\t{error}");
        std::process::exit(1);
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn input_timeline_rejects_ambiguous_or_out_of_run_inputs() {
        assert_eq!(
            timeline("0:8,20:1,21:0", 100).unwrap(),
            vec![(0, 8), (20, 1), (21, 0)]
        );
        for input in ["20:1,10:0", "0:1,0:0", "100:1", "0:400", "0"] {
            assert!(timeline(input, 100).is_err());
        }
    }
}

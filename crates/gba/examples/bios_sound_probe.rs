//! Optional local-ROM diagnostic. No ROM or proprietary firmware is bundled.
use rustboy_emulator_api::Emulator;
use rustboy_gba::{GameBoyAdvance, FRAME_CYCLES};
use std::{error::Error, io::Write};
fn main() -> Result<(), Box<dyn Error>> {
    let mut args = std::env::args().skip(1);
    let path = args
        .next()
        .ok_or("usage: bios_sound_probe <local-rom> [frames=1800]")?;
    let frames: u64 = args.next().map(|s| s.parse()).transpose()?.unwrap_or(1800);
    if frames > 3600 || args.next().is_some() {
        return Err("bounded diagnostic accepts up to 3600 frames".into());
    }
    let mut m = GameBoyAdvance::new(std::fs::read(path)?, None)?;
    let wav = std::env::var_os("GBA_AUDIO_WAV");
    let mut pcm = Vec::new();
    let mut samples = 0;
    let mut nonzero = 0;
    let mut peak = 0f32;
    let mut first = None;
    let mut voice_frames = 0;
    for frame in 0..frames {
        m.run(FRAME_CYCLES);
        if let Some(f) = &m.fault {
            return Err(format!("frame {frame}: {f:?}").into());
        }
        let a = m.bus.peek32(0x03007ff0);
        if matches!(a >> 24, 2 | 3) && (0..12).any(|i| m.bus.peek8(a + 0x50 + i * 0x40) != 0) {
            voice_frames += 1;
        }
        let chunk = m.drain_audio();
        for value in chunk.samples {
            samples += 1;
            peak = peak.max(value.abs());
            if value != 0. {
                nonzero += 1;
                first.get_or_insert(frame);
            }
            if wav.is_some() {
                pcm.extend_from_slice(&((value.clamp(-1., 1.) * 32767.) as i16).to_le_bytes());
            }
        }
    }
    println!(
        "{}: {} frames, {} samples, {} nonzero, peak {:.3}, first {:?}, BIOS voice frames {}",
        m.title(),
        m.frames,
        samples,
        nonzero,
        peak,
        first,
        voice_frames
    );
    if let Some(path) = wav {
        let mut out = std::fs::OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(path)?;
        let length = pcm.len() as u32;
        out.write_all(b"RIFF")?;
        out.write_all(&(length + 36).to_le_bytes())?;
        out.write_all(b"WAVEfmt ")?;
        out.write_all(&16u32.to_le_bytes())?;
        out.write_all(&1u16.to_le_bytes())?;
        out.write_all(&2u16.to_le_bytes())?;
        out.write_all(&32768u32.to_le_bytes())?;
        out.write_all(&(32768u32 * 4).to_le_bytes())?;
        out.write_all(&4u16.to_le_bytes())?;
        out.write_all(&16u16.to_le_bytes())?;
        out.write_all(b"data")?;
        out.write_all(&length.to_le_bytes())?;
        out.write_all(&pcm)?;
    }
    Ok(())
}

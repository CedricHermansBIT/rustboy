//! Diagnose sound commands from a locally supplied SGB game.
use rustboy_gameboy::{
    emulator::{Button, Emulator},
    GameBoy, HardwareModel,
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let path = std::env::args()
        .nth(1)
        .ok_or("Usage: sgb_audio_inspect ROM")?;
    let mut gb = GameBoy::load(&std::fs::read(path)?, &[], HardwareModel::Sgb)?;
    let mut previous = None;
    let mut music_frames = 0;
    let mut music_samples = 0;
    let mut music_energy = 0.0f64;
    for frame in 0..2400 {
        if frame == 900 {
            gb.set_button(0, Button::Start, true)?;
        }
        if frame == 906 {
            gb.set_button(0, Button::Start, false)?;
        }
        if frame == 1200 {
            gb.set_button(0, Button::A, true)?;
        }
        if frame == 1206 {
            gb.set_button(0, Button::A, false)?;
        }
        gb.run(70_224);
        let sgb = gb.cpu().sgb.as_ref().unwrap();
        let state = (
            sgb.sound_uploads(),
            sgb.sound_upload_rejections(),
            sgb.sound_request(),
        );
        if previous != Some(state) {
            let occupied: Vec<_> = sgb
                .sound_ram()
                .chunks(256)
                .enumerate()
                .filter(|(_, bytes)| bytes.iter().any(|&b| b != 0))
                .map(|(index, _)| format!("{:04X}", index * 256))
                .collect();
            println!(
                "frame={frame} uploads={} rejected={} request={:02X?} occupied pages={}",
                state.0,
                state.1,
                state.2,
                occupied.join(",")
            );
            previous = Some(state);
        }
        let music_requested = sgb.sound_request()[3] != 0;
        let audio = gb.drain_audio();
        if music_requested {
            music_frames += 1;
            music_samples += audio.samples.len();
            music_energy += audio
                .samples
                .iter()
                .map(|&sample| f64::from(sample).powi(2))
                .sum::<f64>();
        }
    }
    if let Some(path) = std::env::var_os("RUSTBOY_AUDIO_RAM") {
        std::fs::write(path, gb.cpu().sgb.as_ref().unwrap().sound_ram())?;
    }
    println!(
        "music-request frames={music_frames} mixed-audio samples={music_samples} RMS={:.6}",
        (music_energy / music_samples.max(1) as f64).sqrt()
    );
    Ok(())
}

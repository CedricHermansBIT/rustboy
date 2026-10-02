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
    if let Some(path) = std::env::var_os("RUSTBOY_SGB_FIRMWARE") {
        gb.load_sgb_sound_firmware(&std::fs::read(path)?)?;
    }
    let mut previous = None;
    let mut music_frames = 0;
    let mut music_samples = 0;
    let mut music_energy = 0.0f64;
    let verify = std::env::var("RUSTBOY_VERIFY_SGB_AUDIO").as_deref()==Ok("1");
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
        if verify && frame==1500 {
            let saved=gb.export_state();
            gb.run(70_224*2); gb.drain_audio();
            let expected=gb.cpu().sgb.as_ref().unwrap().clone();
            gb.import_state(&saved)?;
            gb.run(70_224*2); gb.drain_audio();
            assert_eq!(gb.cpu().sgb.as_ref().unwrap(),&expected,"SNES execution, DSP, LCD state and transfer timing must replay after restore");
        }
    }
    if let Some(path) = std::env::var_os("RUSTBOY_AUDIO_RAM") {
        std::fs::write(path, gb.cpu().sgb.as_ref().unwrap().sound_ram())?;
    }
    println!(
        "music-request frames={music_frames} mixed-audio samples={music_samples} RMS={:.6}",
        (music_energy / music_samples.max(1) as f64).sqrt()
    );
    if verify {
        let sgb=gb.cpu().sgb.as_ref().unwrap();
        assert!(sgb.sound_playback_available(),"Provide RUSTBOY_SGB_FIRMWARE");
        assert!(sgb.sound_uploads()>0);
        assert_eq!(sgb.sound_upload_rejections(),0);
        assert!(music_frames>600 && music_energy/music_samples.max(1) as f64>0.000001,"Expected sustained, non-silent enhanced music");
        assert_eq!(sgb.unsupported[8],0); assert_eq!(sgb.unsupported[9],0);
        gb.reset();
        assert!(gb.cpu().sgb.as_ref().unwrap().sound_playback_available(),"Reset must retain host-supplied firmware");
        println!("SGB enhanced audio, snapshot replay and firmware retention on reset passed");
    }
    Ok(())
}

//! Explicit opt-in smoke check for downloaded homebrew; never silently skips
//! missing ROMs and does not need external firmware.
use rustboy::emulator::{Button, Emulator, StopReason};
use rustboy::gameboy::{GameBoy, HardwareModel};
use std::collections::HashSet;

fn frames(gb: &mut GameBoy, count: usize) {
    for _ in 0..count {
        let result = gb.run(70_224);
        assert_eq!(result.reason, StopReason::BudgetExhausted);
        assert!(result.ticks >= 70_224);
        assert!(gb
            .drain_audio()
            .samples
            .iter()
            .all(|sample| sample.is_finite()));
    }
}

fn main() {
    let paths: Vec<_> = std::env::args().skip(1).collect();
    assert!(
        !paths.is_empty(),
        "Pass paths to the prepared homebrew ROMs"
    );
    for path in paths {
        let rom = std::fs::read(&path).expect("homebrew ROM must exist");
        let mut gb = GameBoy::load(&rom, &[], HardwareModel::Auto).expect("supported cartridge");
        frames(&mut gb, 240);
        assert!(!gb.cpu().booting, "boot handoff: {path}");
        frames(&mut gb, 180);
        let title_frame = gb.video_frame().pixels.to_vec();
        // Move beyond title/menu screens and exercise game input, not only boot.
        for button in [
            Button::Start,
            Button::A,
            Button::A,
            Button::Right,
            Button::Down,
        ] {
            gb.set_button(0, button, true).unwrap();
            frames(&mut gb, 6);
            gb.set_button(0, button, false).unwrap();
            frames(&mut gb, 90);
        }
        let frame = gb.video_frame();
        let colors: HashSet<_> = frame.pixels.chunks_exact(4).collect();
        assert!(
            frame.enabled && colors.len() > 1,
            "nonblank gameplay: {path}"
        );
        assert!(
            frame.pixels != title_frame,
            "input must change the game screen: {path}"
        );
        let state = gb.export_state();
        let pc = gb.cpu().program_counter;
        frames(&mut gb, 3);
        let expected_frame = gb.video_frame().pixels.to_vec();
        gb.import_state(&state).expect("state restore");
        assert_eq!(gb.cpu().program_counter, pc);
        // Framebuffers are presentation caches, not serialized state. Compare
        // replay after complete frames rather than expecting an immediate image.
        frames(&mut gb, 3);
        assert!(
            gb.video_frame().pixels == expected_frame,
            "state replay: {path}"
        );
        let battery_bytes = if gb.save_info().is_some() {
            let data = gb.export_save();
            gb.import_save(&data).expect("battery restore");
            assert_eq!(gb.export_save(), data);
            data.len()
        } else {
            0
        };
        println!("{}: boot, input/rendering, finite audio, state restore, battery ({battery_bytes} bytes) passed", gb.title());
        gb.reset();
        assert!(gb.cpu().booting, "reset must replay startup");
    }
}

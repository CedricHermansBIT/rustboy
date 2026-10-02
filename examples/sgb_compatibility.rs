//! Optional checks of user-owned SGB games. No cartridge data is bundled.
use rustboy_gameboy::{
    emulator::{Button, Emulator},
    GameBoy, HardwareModel,
};
use std::collections::BTreeSet;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    for path in std::env::args().skip(1) {
        let rom = std::fs::read(&path)?;
        let mut gb = GameBoy::load(&rom, &[], HardwareModel::Auto)?;
        if gb.cpu().sgb.is_none() {
            return Err(format!("{path}: not an automatically selected SGB cartridge").into());
        }
        for frame in 0..1200 {
            for (when, button, pressed) in [
                (600, Button::Start, true),
                (606, Button::Start, false),
                (800, Button::A, true),
                (806, Button::A, false),
            ] {
                if frame == when {
                    gb.set_button(0, button, pressed)?;
                }
            }
            gb.run(70_224);
            assert!(
                gb.drain_audio()
                    .samples
                    .iter()
                    .all(|sample| sample.is_finite()),
                "{path}: invalid GB audio"
            );
        }
        let sgb = gb.cpu().sgb.as_ref().unwrap();
        assert!(!gb.cpu().booting, "{path}: no cartridge handoff");
        assert_eq!(sgb.screen_mask(), 0, "{path}: game still masked");
        assert_eq!(sgb.transfers_pending(), 0, "{path}: stuck transfer");
        assert_eq!(sgb.transfer_drops, 0, "{path}: transfer queue overflow");
        assert_eq!(sgb.rejected_pulses, 0, "{path}: malformed packet timing");
        assert!(sgb.commands_received > 0, "{path}: no enhancement commands");
        let commands = sgb.commands_received;
        let border = sgb.has_border();
        let unsupported: Vec<_> = sgb
            .unsupported
            .iter()
            .enumerate()
            .filter(|(_, n)| **n != 0)
            .map(|(code, n)| format!("{code:02X}:{n}"))
            .collect();
        gb.set_border_visible(false); // Do not mistake decoration for a working game.
        let colors: BTreeSet<_> = gb
            .video_frame()
            .pixels
            .chunks_exact(4)
            .map(|p| p.to_vec())
            .collect();
        assert!(colors.len() > 1, "{path}: blank game window");
        let saved = gb.export_state();
        gb.run(70_224 * 2);
        let expected = gb.video_frame().pixels.to_vec();
        gb.reset();
        gb.import_state(&saved)?;
        gb.run(70_224 * 2);
        assert_eq!(
            gb.video_frame().pixels,
            expected,
            "{path}: state replay differs"
        );
        println!("PASS {path}: {} game colors, border={border}, commands={commands}, unsupported=[{}], finite GB audio and state replay", colors.len(), unsupported.join(", "));
    }
    Ok(())
}

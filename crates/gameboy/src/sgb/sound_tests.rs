use super::*;

fn adapter() -> (Vec<u8>, Sgb) {
    let mut rom = vec![0; 32768];
    rom[0x146] = 3;
    rom[0x14B] = 0x33;
    let sgb = Sgb::new(&rom);
    (rom, sgb)
}

fn payload() -> [u8; 4096] {
    let mut data = [0; 4096];
    data[..12].copy_from_slice(&[4, 0, 0xFE, 0xFF, 1, 2, 3, 4, 0, 0, 0, 4]);
    data
}

#[test]
fn sound_upload_packet_lists_write_wrapping_ram_and_retain_jump_without_execution() {
    let mut sound = sound::Sound::default();
    sound.upload(&payload());
    assert_eq!(
        [
            sound.ram.read(0xFFFE),
            sound.ram.read(0xFFFF),
            sound.ram.read(0),
            sound.ram.read(1)
        ],
        [1, 2, 3, 4]
    );
    assert_eq!(sound.jump, Some(0x0400));
    assert_eq!(sound.uploads, 1);
    let mut invalid = [0; 4096];
    invalid[..9].copy_from_slice(&[1, 0, 0, 0x20, 99, 0xFF, 0xFF, 0, 4]);
    let original = sound.ram.clone();
    sound.upload(&invalid);
    assert_eq!(sound.ram, original); // Even the valid first write is not committed.
    assert_eq!(sound.jump, Some(0x0400));
    assert_eq!((sound.uploads, sound.rejected), (1, 1));
}

#[test]
fn sound_command_transport_is_retained_but_playback_is_honestly_unsupported() {
    let (_, mut sgb) = adapter();
    let mut command = [0; 16];
    command[0] = (8 << 3) | 1;
    command[1..5].copy_from_slice(&[0x17, 0x04, 0xD2, 0x03]);
    super::tests::send(&mut sgb, &command);
    assert_eq!(sgb.sound_request(), [0x17, 0x04, 0xD2, 0x03]);
    assert_eq!(sgb.unsupported[8], 1);
}

#[test]
fn sou_trn_uses_lcd_pipeline_five_frame_window_and_snapshot_migration() {
    let (rom, mut sgb) = adapter();
    let mut command = [0; 16];
    command[0] = (9 << 3) | 1;
    super::tests::send(&mut sgb, &command);
    let pixels = border_tests::signal(&payload());
    for _ in 0..4 {
        sgb.start_frame();
        sgb.capture_frame(&pixels);
    }
    assert_eq!(sgb.sound_uploads(), 0);
    let state = sgb.export_state();
    let mut restored = Sgb::import_state(&rom, &state, 5).unwrap();
    restored.start_frame();
    restored.capture_frame(&pixels);
    assert_eq!(restored.sound_uploads(), 1);
    assert_eq!(restored.sound_ram()[..2], [3, 4]);
    assert_eq!(restored.unsupported[9], 1);
    assert_eq!(restored.transfers_pending(), 0);
    let state = restored.export_state();
    assert_eq!(Sgb::import_state(&rom, &state, 5).unwrap(), restored);
    let mut corrupt = state.clone();
    *corrupt.get_mut(state.len() - 3).unwrap() = 2;
    assert!(Sgb::import_state(&rom, &corrupt, 5).is_err());
    // Old v4 saves have no audio RAM or uploads, but retain pulse timing.
    let mut legacy = adapter().1;
    legacy.tick_joyp(4);
    legacy.write_joyp_timed(0x20);
    let state = legacy.export_state();
    let old = Sgb::import_state(&rom, &state[..state.len() - sound::STATE_BYTES], 4).unwrap();
    assert_eq!(old.pulse_lines, legacy.pulse_lines);
    assert_eq!(old.sound, sound::Sound::default());
}

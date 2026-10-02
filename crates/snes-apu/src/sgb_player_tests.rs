use super::sgb_player::*;
use crate::{apu::Apu, SpcRam};

fn word(ram: &mut SpcRam, address: u16, value: u16) {
    ram.write_wrapping(address, &value.to_le_bytes());
}
fn score(data: &[u8]) -> (Apu, Player) {
    let mut apu = Apu::default();
    let player = Player::initialize(&mut apu);
    word(&mut apu.bus.ram, 0x2b00, 0x2b20);
    word(&mut apu.bus.ram, 0x2b20, 0x2b30);
    word(&mut apu.bus.ram, 0x2b22, 255);
    word(&mut apu.bus.ram, 0x2b24, 0x2b20);
    word(&mut apu.bus.ram, 0x2b30, 0x2b50);
    apu.bus.ram.write_wrapping(0x2b50, data);
    (apu, player)
}
fn run(apu: &mut Apu, player: &mut Player, clocks: u32) -> f64 {
    let mut energy = 0.;
    for _ in 0..clocks {
        player.clock(apu);
        apu.run(1);
        if let Some(sample) = apu.pop_sample() {
            energy += f64::from(sample[0]).powi(2) + f64::from(sample[1]).powi(2);
        }
    }
    energy
}

#[test]
fn scores_play_notes_rests_ties_and_loop_with_original_instruments() {
    let (mut apu, mut player) = score(&[
        0xe7, 82, 0xe0, 16, 0xe1, 0, 6, 0x7f, 0xa4, 0xc8, 0xc9, 0xa7, 0,
    ]);
    player.command(&mut apu, [0, 0, 0, 1]);
    assert!(run(&mut apu, &mut player, 1_024_000) > 1e9);
    assert!(player.notes() > 10);
    assert_eq!(player.errors(), 0);
    assert_eq!(apu.bus.dsp.read(1), 0, "hard-left panning");
    assert_eq!(apu.bus.dsp.read(4), 16, "documented instrument ID");
    let notes = player.notes();
    player.command(&mut apu, [0, 0, 0, 0]);
    run(&mut apu, &mut player, 200_000);
    assert!(player.notes() > notes, "dummy flag retains the song");
    player.command(&mut apu, [0, 0, 0, 0x80]);
    run(&mut apu, &mut player, 50_000);
    assert_eq!(
        run(&mut apu, &mut player, 100_000),
        0.,
        "stop releases all voices"
    );
}

#[test]
fn special_commands_and_repeated_subroutines_keep_score_alignment() {
    let data = [
        0xe7, 82, 0xe5, 200, 0xe6, 3, 180, 0xe8, 3, 80, 0xe9, 1, 0xea, 255, 0xed, 190, 0xee, 4,
        170, 0xe0, 7, 0xe1, 10, 0xe2, 3, 14, 0xe3, 1, 8, 16, 0xeb, 1, 8, 16, 0xf0, 4, 0xf1, 1, 4,
        2, 0xf4, 20, 0xf5, 1, 20, 30, 0xf7, 2, 50, 2, 0xf8, 4, 0, 0, 0xef, 0, 0x2c, 3, 0xe4, 0xec,
        0xf2, 1, 4, 255, 6, 0x7f, 0xaa, 0xf3, 0xf6, 0xf9, 1, 3, 0xac, 6, 0xc8, 0xfa, 49, 0xca,
        0xfb, 0, 0, 0xfc, 0xa4, 0,
    ];
    let (mut apu, mut player) = score(&data);
    apu.bus
        .ram
        .write_wrapping(0x2c00, &[6, 0x7f, 0xa4, 0xa7, 0]);
    player.command(&mut apu, [0, 0, 0, 1]);
    assert!(run(&mut apu, &mut player, 1_024_000) > 1e8);
    assert!(player.notes() >= 6);
    assert_eq!(player.errors(), 0);
}

#[test]
fn all_documented_effect_ids_sound_without_a_score_and_mute_fades() {
    for (group, max) in [(0, 48), (1, 25)] {
        for code in 1..=max {
            let mut apu = Apu::default();
            let mut player = Player::initialize(&mut apu);
            let mut request = [0; 4];
            request[group] = code;
            player.command(&mut apu, request);
            let energy = run(&mut apu, &mut player, 100_000);
            assert!(energy > 1e7, "group {group} effect {code}: energy {energy}");
            assert_eq!(player.errors(), 0);
            player.command(&mut apu, [0, 0, 12, 0]);
            run(&mut apu, &mut player, 100_000);
            assert_eq!(run(&mut apu, &mut player, 20_000), 0.);
        }
    }
}

#[test]
fn player_and_dsp_snapshot_replay_and_malformed_loops_are_bounded() {
    let (mut apu, mut player) = score(&[0xe7, 82, 0xe0, 27, 6, 0x7f, 0xa4, 0xa7, 0]);
    player.command(&mut apu, [3, 4, 0, 1]);
    run(&mut apu, &mut player, 320_003);
    let state = player.export_state();
    let mut restored = Player::import_state(&state).unwrap();
    let mut apu2 = Apu::import_state(&apu.export_state()).unwrap();
    assert_eq!(
        run(&mut apu, &mut player, 240_009),
        run(&mut apu2, &mut restored, 240_009)
    );
    assert_eq!(player, restored);
    assert_eq!(apu, apu2);
    // Version 1 ended before startup_ticks was added; completed startup must
    // resume identically rather than rejecting previously saved scores.
    let mut legacy = state.clone();
    legacy[4] = 1;
    legacy.pop();
    assert_eq!(
        Player::import_state(&legacy).unwrap(),
        Player::import_state(&state).unwrap()
    );
    assert!(Player::import_state(&state[..state.len() - 1]).is_err());
    let mut trailing = state.clone();
    trailing.push(0);
    assert!(Player::import_state(&trailing).is_err());
    let (mut apu, mut player) = score(&[0xef, 0x50, 0x2b, 2]); // Recursive subroutine.
    player.command(&mut apu, [0, 0, 0, 1]);
    run(&mut apu, &mut player, 100_000);
    assert!(player.errors() > 0);
    word(&mut apu.bus.ram, 0x2b20, 255);
    word(&mut apu.bus.ram, 0x2b22, 0x2b20);
    player.command(&mut apu, [0, 0, 0, 1]);
    assert!(player.errors() > 1);
}

#[test]
fn score_startup_and_phrase_repeats_do_not_add_a_tempo_tick() {
    let (mut apu, mut player) = score(&[0xe7, 29, 0xe0, 7, 6, 0x7f, 0xa4, 0xa7, 0xab, 0]);
    player.command(&mut apu, [0, 0, 0, 1]);
    let mut transitions = Vec::new();
    let mut previous = 0;
    for clock in 0..850_000 {
        player.clock(&mut apu);
        apu.run(1);
        apu.pop_sample();
        let pitch = u16::from_le_bytes([apu.bus.dsp.read(2), apu.bus.dsp.read(3)]);
        if pitch != previous {
            transitions.push(clock as f64 / 1024.);
            previous = pitch;
        }
    }
    // Firmware comparison measured ~85 ms startup and ~106 ms per six-tick
    // note. Repeating phrases must not introduce an extra ~18 ms silence.
    assert!(transitions.len() >= 7);
    assert!((transitions[0] - 85.).abs() < 3.);
    for pair in transitions.windows(2) {
        assert!((pair[1] - pair[0] - 106.).abs() < 3., "{pair:?}");
    }
}

#[test]
fn uploaded_instrument_samples_and_program_detection_are_not_overwritten() {
    let (mut apu, mut player) = score(&[0xe7, 82, 0xe0, 7, 6, 0x7f, 0xa4, 0]);
    apu.bus.ram.write_wrapping(0x4b1c, &[0, 0x60, 0, 0x60]);
    apu.bus.ram.write_wrapping(
        0x6000,
        &[0xc3, 0x77, 0x77, 0x77, 0x77, 0x88, 0x88, 0x88, 0x88],
    );
    player.command(&mut apu, [0, 0, 0, 1]);
    assert!(run(&mut apu, &mut player, 100_000) > 1e8);
    assert_eq!(apu.bus.ram.read(0x6001), 0x77);
    player.mark_upload(0x6000, 9);
    assert!(!player.use_uploaded_program(0x400));
    player.mark_upload(0xfffe, 6);
    assert!(!player.use_uploaded_program(0x400));
    player.mark_upload(0x03ff, 2);
    assert!(player.use_uploaded_program(0x400));
    assert!(Player::default().use_uploaded_program(0x1000));
}

#[test]
fn effect_voices_survive_music_stop_and_gate_events_and_uploaded_tuning_is_respected() {
    let (mut apu, mut player) = score(&[0xe7, 82, 0xe0, 7, 6, 0x7f, 0xa4, 0]);
    apu.bus.ram.write_wrapping(0x4c5e, &[4, 0]); // Instrument 7 tuning $0400.
    player.command(&mut apu, [0, 4, 0, 1]);
    run(&mut apu, &mut player, 100_000);
    assert_eq!(
        u16::from_le_bytes([apu.bus.dsp.read(2), apu.bus.dsp.read(3)]),
        0x085c
    );
    player.command(&mut apu, [0, 0, 0, 0x80]);
    run(&mut apu, &mut player, 50_000);
    assert!(
        run(&mut apu, &mut player, 100_000) > 1e8,
        "stopping music does not stop effect B"
    );
    player.command(&mut apu, [0x80, 0x80, 0, 0]);
    run(&mut apu, &mut player, 50_000);
    assert_eq!(run(&mut apu, &mut player, 100_000), 0.);
}

#[test]
fn resident_sine_strings_and_flute_keep_their_measured_natural_octaves() {
    for (instrument, fine, expected) in [(7, 0, 261.2), (30, 80, 522.3), (48, 53, 523.2)] {
        let (mut apu, mut player) =
            score(&[0xe7, 40, 0xe0, instrument, 0xf4, fine, 96, 0x7f, 0xa4, 0]);
        player.command(&mut apu, [0, 0, 0, 1]);
        for _ in 0..250_000 {
            player.clock(&mut apu);
            apu.run(1);
        }
        let audio = apu.drain_samples();
        let window = &audio[3000..7096]; // Skip the resident song initialization.
        let rising = window
            .windows(2)
            .filter(|s| s[0][0] < 0 && s[1][0] >= 0)
            .count();
        let measured = rising as f64 * 32000. / 4096.;
        assert!(
            (measured - expected).abs() < 16.,
            "instrument {instrument}: {measured} Hz, expected {expected}"
        );
        assert!(window.iter().any(|s| s[0].abs() > 100));
    }
}

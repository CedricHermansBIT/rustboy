use super::*;

fn adapter() -> (Vec<u8>, Sgb) {
    let mut rom = vec![0; 32768];
    rom[0x146] = 3;
    rom[0x14B] = 0x33;
    let sgb = Sgb::new(&rom);
    (rom, sgb)
}

fn pulse(sgb: &mut Sgb, lines: u8, low: u8, space: u8) {
    sgb.write_joyp_timed(lines);
    sgb.tick_joyp(low);
    sgb.write_joyp_timed(0x30);
    sgb.tick_joyp(space);
}

fn bits(sgb: &mut Sgb, low: u8, space: u8) {
    for byte in [(0x17 << 3) | 1, 1].into_iter().chain([0; 14]) {
        for bit in 0..8 {
            pulse(
                sgb,
                if byte & (1 << bit) == 0 { 0x20 } else { 0x10 },
                low,
                space,
            );
        }
    }
}

#[test]
fn timed_transport_accepts_two_machine_cycles_but_not_short_pulses_or_spaces() {
    for (low, space, accepted) in [(8, 8, true), (20, 60, true), (4, 8, false), (8, 4, false)] {
        let (_, mut sgb) = adapter();
        pulse(&mut sgb, 0, low, space);
        bits(&mut sgb, low, space);
        pulse(&mut sgb, 0x20, low, space);
        assert_eq!(sgb.commands_received, accepted as u64);
        assert_eq!(sgb.screen_mask(), accepted as u8);
        assert_eq!(sgb.rejected_pulses == 0, accepted);
        // A valid reset resynchronizes a receiver after a damaged packet.
        sgb.tick_joyp(8);
        pulse(&mut sgb, 0, 8, 8);
        bits(&mut sgb, 8, 8);
        pulse(&mut sgb, 0x20, 8, 8);
        assert_eq!(sgb.commands_received, accepted as u64 + 1);
    }
}

#[test]
fn stop_bit_is_not_dispatched_until_release_and_its_width_survives_snapshots() {
    let (rom, mut sgb) = adapter();
    pulse(&mut sgb, 0, 8, 8);
    bits(&mut sgb, 8, 8);
    sgb.write_joyp_timed(0x20);
    sgb.tick_joyp(4);
    assert_eq!(sgb.commands_received, 0);
    let state = sgb.export_state();
    let mut restored = Sgb::import_state(&rom, &state, 4).unwrap();
    restored.tick_joyp(4);
    restored.write_joyp_timed(0x30);
    assert_eq!(restored.commands_received, 1);
    sgb.write_joyp_timed(0x30); // Releasing at only one M-cycle must reject.
    assert_eq!(sgb.commands_received, 0);
    assert_eq!(sgb.rejected_pulses, 1);
    let mut old = Sgb::import_state(&rom, &state[..state.len() - TIMING_STATE_BYTES], 3).unwrap();
    assert_eq!(old.receiver.bits, 128); // Receiver held 128 bits before the stop.
                                        // A v3 snapshot can resume via its untimed debug transport.
    old.write_joyp(0x20);
    assert_eq!(old.commands_received, 1);
}

#[test]
fn fast_normal_joypad_polling_is_not_reported_as_bad_packet_timing() {
    let (_, mut sgb) = adapter();
    for _ in 0..10 {
        sgb.write_joyp_timed(0x20); sgb.tick_joyp(4);
        sgb.write_joyp_timed(0x10); sgb.tick_joyp(4);
        sgb.write_joyp_timed(0x30); sgb.tick_joyp(8);
    }
    assert_eq!(sgb.rejected_pulses, 0);
    pulse(&mut sgb, 0, 8, 8); bits(&mut sgb, 8, 8); pulse(&mut sgb, 0x20, 8, 8);
    assert_eq!(sgb.commands_received, 1);
}

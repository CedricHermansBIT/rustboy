use super::{border_tests::signal, tests::send, *};

fn rom() -> Vec<u8> {
    let mut rom = vec![0; 32768];
    rom[0x146] = 3;
    rom[0x14B] = 0x33;
    rom
}

fn command(sgb: &mut Sgb, code: u8, payload: &[u8]) {
    let mut packet = [0; 16];
    packet[0] = (code << 3) | 1;
    packet[1..1 + payload.len()].copy_from_slice(payload);
    send(sgb, &packet);
}

fn frames(sgb: &mut Sgb, data: &[u8; 4096], count: usize) {
    let pixels = signal(data);
    for _ in 0..count {
        sgb.start_frame();
        sgb.capture_frame(&pixels);
    }
}

fn table_data() -> [u8; 4096] {
    let mut data = [0; 4096];
    for (id, colors) in [
        (0, [0x7FFFu16, 31, 0x3E0, 0x7C00]),
        (1, [0, 0x3E0, 0x7C00, 31]),
        (256, [0x1234, 0x7C00, 31, 0x3E0]),
        (511, [0xFFFF, 0x83FF, 0x7C1F, 0x7FE0]),
    ] {
        for (i, color) in colors.iter().enumerate() {
            data[id * 8 + i * 2..id * 8 + i * 2 + 2].copy_from_slice(&color.to_le_bytes());
        }
    }
    data
}

fn select(sgb: &mut Sgb, flags: u8) {
    command(sgb, 0x0A, &[0, 0, 1, 0, 0, 1, 0xFF, 0xFF, flags]);
}

#[test]
fn palette_transfer_is_deferred_and_does_not_change_visible_palettes() {
    let mut sgb = Sgb::new(&rom());
    let old = sgb.palettes;
    command(&mut sgb, 0x17, &[1]);
    command(&mut sgb, 0x0B, &[]);
    sgb.capture_frame(&signal(&table_data())); // ineligible partial frame
    frames(&mut sgb, &table_data(), 4);
    assert_eq!(sgb.tables.palette(0), old[0]);
    frames(&mut sgb, &table_data(), 1);
    assert_eq!(sgb.palettes, old);
    assert_eq!(sgb.tables.palette(0), [0x7FFF, 31, 0x3E0, 0x7C00]);
    assert_eq!(sgb.transfers_pending(), 0);
    assert!(!sgb.has_border());
    assert_eq!(sgb.unsupported, [0; 32]);
}

#[test]
fn pal_set_selects_all_512_palettes_masks_high_bits_and_shares_backdrop() {
    let mut sgb = Sgb::new(&rom());
    command(&mut sgb, 0x0B, &[]);
    frames(&mut sgb, &table_data(), 5);
    command(&mut sgb, 0x17, &[3]);
    select(&mut sgb, 0);
    assert_eq!(
        sgb.palettes,
        [
            [0x7FFF, 31, 0x3E0, 0x7C00],
            [0x7FFF, 0x3E0, 0x7C00, 31],
            [0x7FFF, 0x7C00, 31, 0x3E0],
            [0x7FFF, 0x3FF, 0x7C1F, 0x7FE0],
        ]
    );
    assert_eq!(sgb.screen_mask(), 3); // flags, not selecting palettes, unmask
    select(&mut sgb, 0x40);
    assert_eq!(sgb.screen_mask(), 0);
}

#[test]
fn attribute_transfer_decodes_first_and_last_files_msb_first() {
    let mut sgb = Sgb::new(&rom());
    let mut data = [0xFF; 4096];
    data[..90].fill(0x1B); // 0,1,2,3 in screen row order
    data[44 * 90..45 * 90].fill(0xE4); // 3,2,1,0
    command(&mut sgb, 0x15, &[]);
    frames(&mut sgb, &data, 5);
    assert_eq!(sgb.attributes, [0; 360]); // table transfer alone is invisible
    command(&mut sgb, 0x16, &[0]);
    assert_eq!(&sgb.attributes[..8], &[0, 1, 2, 3, 0, 1, 2, 3]);
    assert_eq!(&sgb.attributes[356..], &[0, 1, 2, 3]);
    command(&mut sgb, 0x17, &[2]);
    command(&mut sgb, 0x16, &[44 | 0x40]);
    assert_eq!(&sgb.attributes[..8], &[3, 2, 1, 0, 3, 2, 1, 0]);
    assert_eq!(&sgb.attributes[356..], &[3, 2, 1, 0]);
    assert_eq!(sgb.screen_mask(), 0);
    let old = sgb.attributes;
    for id in 45..64 {
        command(&mut sgb, 0x16, &[id]);
        assert_eq!(sgb.attributes, old);
    }
    command(&mut sgb, 0x17, &[1]);
    command(&mut sgb, 0x16, &[63 | 0x40]);
    assert_eq!(sgb.attributes, old);
    assert_eq!(sgb.screen_mask(), 0); // invalid ATF must not prevent mask cancel
}

#[test]
fn pal_set_applies_attributes_only_when_requested_and_unmasks_independently() {
    let mut sgb = Sgb::new(&rom());
    command(&mut sgb, 0x15, &[]);
    frames(&mut sgb, &[0x1B; 4096], 5);
    select(&mut sgb, 44);
    assert_eq!(sgb.attributes, [0; 360]);
    select(&mut sgb, 0x80 | 44);
    assert_eq!(&sgb.attributes[..4], &[0, 1, 2, 3]);
    sgb.attributes.fill(3);
    command(&mut sgb, 0x17, &[3]);
    select(&mut sgb, 0xC0 | 63);
    assert_eq!(sgb.attributes, [3; 360]);
    assert_eq!(sgb.screen_mask(), 0);
}

#[test]
fn replacing_a_white_palette_recolors_frozen_lcd_shades_and_attributes() {
    let mut sgb = Sgb::new(&rom());
    let white = [0xFF, 0x7F].repeat(7);
    command(&mut sgb, 0, &white);
    sgb.capture_frame(&vec![1 << 27; PIXELS]);
    assert!(sgb.frame.iter().all(|&v| v == 255));
    command(&mut sgb, 0x17, &[1]);
    command(&mut sgb, 0x0B, &[]);
    frames(&mut sgb, &table_data(), 5);
    select(&mut sgb, 0); // retain freeze, recolor the same LCD shade 1
    assert_eq!(&sgb.frame[..4], &[255, 0, 0, 255]);
    assert_eq!(sgb.screen_mask(), 1);
    sgb.lcd_off();
    command(&mut sgb, 5, &[1, 1 << 5]); // column 0 -> palette 1 green
    assert_eq!(&sgb.frame[..4], &[0, 255, 0, 255]);
    assert_eq!(&sgb.frame[8 * 4..9 * 4], &[255, 0, 0, 255]);
    let restored = Sgb::import_state(&rom(), &sgb.export_state(), 7).unwrap();
    assert_eq!(restored, sgb);
}

#[test]
fn snapshots_preserve_pending_tables_shades_and_reject_malformed_extensions() {
    let mut sgb = Sgb::new(&rom());
    sgb.capture_frame(&vec![2 << 27; PIXELS]);
    command(&mut sgb, 0x17, &[1]);
    command(&mut sgb, 0x0B, &[]);
    frames(&mut sgb, &table_data(), 2);
    command(&mut sgb, 0x15, &[]);
    let state = sgb.export_state();
    let mut restored = Sgb::import_state(&rom(), &state, 7).unwrap();
    assert_eq!(restored, sgb);
    frames(&mut restored, &[0x1B; 4096], 5);
    select(&mut restored, 0x80);
    assert_eq!(&restored.frame[..4], &[0, 255, 0, 255]);
    for offset in [
        BORDER_STATE_BYTES + 1,
        BORDER_STATE_BYTES + tables::STATE_BYTES,
    ] {
        let mut bad = state.clone();
        bad[offset] = 0xFF;
        assert!(Sgb::import_state(&rom(), &bad, 7).is_err());
    }
    assert!(Sgb::import_state(&rom(), &state[..state.len() - 1], 7).is_err());
}

#[test]
fn legacy_rgba_frames_remain_intact_until_a_new_unmasked_lcd_frame() {
    let mut sgb = Sgb::new(&rom());
    sgb.capture_frame(&vec![1 << 27; PIXELS]);
    command(&mut sgb, 0x17, &[1]);
    let bytes = sgb.export_state();
    for (version, length) in [(1, LEGACY_STATE_BYTES), (2, BORDER_STATE_BYTES)] {
        let mut restored = Sgb::import_state(&rom(), &bytes[..length], version).unwrap();
        assert_eq!(restored.frame, sgb.frame);
        let expected = restored.frame.clone();
        command(&mut restored, 0, &[0; 14]);
        assert_eq!(restored.frame, expected); // no guessed shade from equal RGB
        command(&mut restored, 0x17, &[0]);
        restored.capture_frame(&vec![1 << 27; PIXELS]);
        assert_eq!(&restored.frame[..4], &[0, 0, 0, 255]);
        assert!(restored.shades_valid);
    }
}

fn user_colors() -> [[u16; 4]; 4] {
    [[0x7C00, 0x3E0, 31, 0]; 4]
}

#[test]
fn user_override_preserves_game_colors_and_validates_atomically() {
    let mut sgb = Sgb::new(&rom());
    let game = sgb.palettes;
    let mut user = user_colors();
    user[1][0] = 31;
    sgb.set_user_palette(user).unwrap();
    assert_eq!(sgb.visible_palettes(), user_colors());
    assert_eq!(sgb.palettes, game);
    assert!(sgb.user_palette_active());
    let before = sgb.clone();
    user[3][3] = 0x8000;
    assert!(sgb.set_user_palette(user).is_err());
    assert_eq!(sgb, before);
    command(&mut sgb, 0, &[0; 14]);
    assert_eq!(sgb.visible_palettes(), user_colors());
    sgb.clear_user_palette();
    assert!(!sgb.user_palette_active());
    assert_eq!(sgb.visible_palettes()[0], [0; 4]);
    assert_eq!(sgb.visible_palettes()[2][1], game[2][1]);
}

#[test]
fn pal_pri_only_cancels_override_on_subsequent_visible_palette_commands() {
    for code in [0, 1, 2, 3, 0x0A] {
        let mut sgb = Sgb::new(&rom());
        sgb.set_user_palette(user_colors()).unwrap();
        command(&mut sgb, 0x19, &[0xFE]); // only bit zero controls priority
        assert!(!sgb.palette_priority());
        command(&mut sgb, code, &[0; 14]);
        assert!(sgb.user_palette_active());
        command(&mut sgb, 0x19, &[0xFF]);
        assert!(sgb.palette_priority());
        assert!(sgb.user_palette_active());
        command(&mut sgb, 0x0B, &[]);
        frames(&mut sgb, &table_data(), 5);
        assert!(sgb.user_palette_active());
        command(&mut sgb, 0x16, &[0]);
        command(&mut sgb, 0x17, &[1]);
        assert!(sgb.user_palette_active());
        command(&mut sgb, code, &[0; 14]);
        assert!(!sgb.user_palette_active());
        assert_eq!(sgb.unsupported[0x19], 0);
        assert!(sgb.palette_priority());
        sgb.set_user_palette(user_colors()).unwrap();
        command(&mut sgb, 0x19, &[0]);
        command(&mut sgb, code, &[0; 14]);
        assert!(sgb.user_palette_active());
    }
}

#[test]
fn user_colors_recolor_frozen_shades_and_obey_black_and_backdrop_masks() {
    let mut sgb = Sgb::new(&rom());
    sgb.capture_frame(&vec![1 << 27; PIXELS]);
    let original = sgb.frame.clone();
    command(&mut sgb, 0x17, &[1]);
    sgb.lcd_off();
    sgb.set_user_palette(user_colors()).unwrap();
    assert_eq!(&sgb.frame[..4], &[0, 255, 0, 255]);
    sgb.capture_frame(&vec![2 << 27; PIXELS]);
    assert_eq!(&sgb.frame[..4], &[0, 255, 0, 255]);
    let mut out = vec![0; PIXELS * 4];
    command(&mut sgb, 0x17, &[2]);
    sgb.copy_game_frame(&mut out);
    assert_eq!(&out[..4], &[0, 0, 0, 255]);
    command(&mut sgb, 0x17, &[3]);
    sgb.copy_game_frame(&mut out);
    assert_eq!(&out[..4], &[0, 0, 255, 255]);
    sgb.clear_user_palette();
    assert_eq!(sgb.frame, original);
    sgb.copy_game_frame(&mut out);
    assert_eq!(&out[..4], &rgb(sgb.palettes[0][0]));
}

#[test]
fn palette_snapshot_extension_roundtrips_and_rejects_bad_flags_colors_and_lengths() {
    let mut sgb = Sgb::new(&rom());
    sgb.set_user_palette(user_colors()).unwrap();
    command(&mut sgb, 0x19, &[1]);
    let state = sgb.export_state();
    assert_eq!(Sgb::import_state(&rom(), &state, 7).unwrap(), sgb);
    let offset = state.len() - 34;
    for (index, value) in [(0, 2), (1, 2), (3, 0x80), (11, 0)] {
        let mut bad = state.clone();
        bad[offset + index] = value;
        assert!(Sgb::import_state(&rom(), &bad, 7).is_err());
    }
    let mut bad = state.clone();
    bad[offset + 1] = 0; // inactive palette storage must be empty
    assert!(Sgb::import_state(&rom(), &bad, 7).is_err());
    let mut bad = state.clone();
    bad.push(0);
    assert!(Sgb::import_state(&rom(), &bad, 7).is_err());
    for length in [0, 33, state.len() - 1] {
        assert!(Sgb::import_state(&rom(), &state[..length], 7).is_err());
    }
    let legacy = Sgb::import_state(&rom(), &state[..offset], 6).unwrap();
    assert!(!legacy.palette_priority());
    assert!(!legacy.user_palette_active());
    assert_eq!(legacy.visible_palettes(), sgb.palettes);
}

#[test]
fn user_palette_keeps_attribute_mapping_and_disabled_commands_cannot_change_priority() {
    let mut sgb = Sgb::new(&rom());
    let mut colors = user_colors();
    colors[1][1] = 31;
    sgb.set_user_palette(colors).unwrap();
    sgb.capture_frame(&vec![1 << 27; PIXELS]);
    command(&mut sgb, 0x17, &[1]);
    command(&mut sgb, 5, &[1, 1 << 5]); // column zero uses user palette one
    assert_eq!(&sgb.frame[..4], &[255, 0, 0, 255]);
    assert_eq!(&sgb.frame[8 * 4..9 * 4], &[0, 255, 0, 255]);
    command(&mut sgb, 0x0E, &[4]);
    command(&mut sgb, 0x19, &[1]);
    assert!(!sgb.palette_priority());
    assert!(sgb.user_palette_active());
}

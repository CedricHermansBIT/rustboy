use super::*;

fn adapter() -> Sgb {
    let mut rom = vec![0; 32768];
    rom[0x146] = 3;
    rom[0x14B] = 0x33;
    Sgb::new(&rom)
}

fn command(sgb: &mut Sgb, code: u8, argument: u8) {
    let mut packet = [0; 16];
    packet[0] = (code << 3) | 1;
    packet[1] = argument;
    super::tests::send(sgb, &packet);
}

pub(super) fn signal(data: &[u8; 4096]) -> Vec<u32> {
    let mut pixels = vec![0; PIXELS];
    for tile in 0..256 {
        for y in 0..8 {
            for x in 0..8 {
                let offset = tile * 16 + y * 2;
                let shade =
                    ((data[offset] >> (7 - x)) & 1) | (((data[offset + 1] >> (7 - x)) & 1) << 1);
                // Deliberately different raw tile metadata: transport uses LCD shade.
                pixels[(tile / 20 * 8 + y) * 160 + tile % 20 * 8 + x] =
                    3 << 24 | (shade as u32) << 27;
            }
        }
    }
    pixels
}

fn frame(sgb: &mut Sgb, pixels: &[u32]) {
    sgb.start_frame();
    sgb.capture_frame(pixels);
}

#[test]
fn lcd_transfer_reconstructs_all_4096_bytes_in_visible_tile_order() {
    let data = std::array::from_fn(|i| (i.wrapping_mul(37) + i / 16) as u8);
    let mut reconstructed = [0; 4096];
    decode_transfer(&signal(&data), &mut reconstructed);
    assert_eq!(reconstructed, data);
}

#[test]
fn transfers_ignore_the_partial_frame_work_while_frozen_and_publish_on_frame_five() {
    let mut sgb = adapter();
    let mut data = [0; 4096];
    data[0x802..0x804].copy_from_slice(&31u16.to_le_bytes());
    let pixels = signal(&data);
    command(&mut sgb, 0x17, 1);
    command(&mut sgb, 0x14, 0);
    sgb.capture_frame(&pixels); // current partial frame is not eligible
    assert_eq!(sgb.transfers[0].remaining, 5);
    assert!(!sgb.has_border());
    for _ in 0..4 {
        frame(&mut sgb, &pixels);
        assert!(!sgb.has_border());
    }
    frame(&mut sgb, &pixels);
    assert!(sgb.has_border());
    assert_eq!(sgb.transfers_pending(), 0);
    assert!(sgb.frame.iter().all(|&byte| byte == 255)); // freeze never shows payload garbage
}

#[test]
fn overlapping_transfers_keep_their_sampled_data_and_queue_is_bounded() {
    let mut sgb = adapter();
    let low = [0x12; 4096];
    let high = [0x89; 4096];
    command(&mut sgb, 0x13, 0);
    frame(&mut sgb, &signal(&low));
    command(&mut sgb, 0x13, 1);
    for _ in 0..5 {
        frame(&mut sgb, &signal(&high));
    }
    let mut expected = border::Border::default();
    expected.transfer(1, &low);
    expected.transfer(2, &high);
    assert_eq!(sgb.border, expected);
    for _ in 0..20 {
        command(&mut sgb, 0x13, 0);
    }
    assert_eq!(sgb.transfers_pending(), MAX_TRANSFERS);
    assert_eq!(sgb.transfer_drops, 16);
}

#[test]
fn lcd_disable_cannot_commit_an_incomplete_payload() {
    let mut sgb = adapter();
    command(&mut sgb, 0x14, 0);
    sgb.start_frame();
    sgb.lcd_off();
    for _ in 0..8 {
        sgb.capture_frame(&vec![0; PIXELS]);
    }
    assert!(!sgb.has_border());
    assert_eq!(sgb.transfers[0].remaining, 5);
    for _ in 0..5 {
        frame(&mut sgb, &vec![0; PIXELS]);
    }
    assert!(sgb.has_border());
}

#[test]
fn masks_affect_only_the_game_window_and_border_can_overlay_it() {
    let mut sgb = adapter();
    let mut tiles = [0; 4096];
    for y in 0..8 {
        tiles[32 + y * 2] = 255;
    }
    let mut map = [0; 4096];
    for i in 0..32 * 28 {
        map[i * 2..i * 2 + 2].copy_from_slice(&0x1001u16.to_le_bytes());
    }
    for y in 5..23 {
        for x in 6..26 {
            map[(y * 32 + x) * 2..(y * 32 + x) * 2 + 2].copy_from_slice(&0x1000u16.to_le_bytes());
        }
    }
    // One opaque tile inside the game window is intentionally allowed.
    map[(6 * 32 + 7) * 2..(6 * 32 + 7) * 2 + 2].copy_from_slice(&0x1001u16.to_le_bytes());
    map[0x802..0x804].copy_from_slice(&31u16.to_le_bytes());
    sgb.border.transfer(1, &tiles);
    sgb.border.transfer(3, &map);
    sgb.capture_frame(&vec![1 << 27; PIXELS]);
    let mut out = vec![0; 256 * 224 * 4];
    for mask in [0, 1, 2, 3] {
        command(&mut sgb, 0x17, mask);
        sgb.copy_frame(&mut out);
        let pixel = |x: usize, y: usize| &out[(y * 256 + x) * 4..(y * 256 + x + 1) * 4];
        assert_eq!(pixel(0, 0), [255, 0, 0, 255]);
        assert_eq!(pixel(56, 48), [255, 0, 0, 255]); // opaque border over any mask
        let expected = match mask {
            2 => [0, 0, 0, 255],
            3 => [255; 4],
            _ => [173, 173, 173, 255],
        };
        assert_eq!(pixel(48, 40), expected);
        assert_eq!(pixel(207, 183), expected);
        assert_eq!(pixel(208, 184), [255, 0, 0, 255]);
    }
}

#[test]
fn old_snapshots_load_without_borders_and_new_snapshots_preserve_pending_transfers() {
    let mut sgb = adapter();
    let mut rom = vec![0; 32768];
    rom[0x146] = 3;
    rom[0x14B] = 0x33;
    command(&mut sgb, 0x13, 1);
    frame(&mut sgb, &signal(&[0x52; 4096]));
    command(&mut sgb, 0x14, 0);
    let bytes = sgb.export_state();
    assert_eq!(Sgb::import_state(&rom, &bytes, 3).unwrap(), sgb);
    let migrated = Sgb::import_state(&rom, &bytes[..BORDER_STATE_BYTES], 2).unwrap();
    assert_eq!(migrated.border, sgb.border);
    assert_eq!(migrated.transfers, sgb.transfers);
    assert_eq!(migrated.frame, sgb.frame);
    assert!(!migrated.shades_valid);
    let old = Sgb::import_state(&rom, &bytes[..LEGACY_STATE_BYTES], 1).unwrap();
    assert!(!old.has_border());
    assert_eq!(old.transfers_pending(), 0);
    assert!(Sgb::import_state(&rom, &bytes, 1).is_err());
    let queue_offset = LEGACY_STATE_BYTES + border::STATE_BYTES + 8;
    for (offset, value) in [
        (queue_offset, 5),
        (queue_offset + 1, 6),
        (queue_offset + 2, 0),
        (queue_offset + 3, 2),
    ] {
        let mut corrupt = bytes.clone();
        corrupt[offset] = value;
        assert!(Sgb::import_state(&rom, &corrupt, 3).is_err());
    }
}

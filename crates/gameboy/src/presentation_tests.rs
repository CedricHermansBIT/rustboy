use crate::{emulator::Emulator, GameBoy, HardwareModel};

#[test]
fn automatic_model_prefers_valid_sgb_enhancements_but_never_color_only_carts() {
    for (flag, sgb_flag, license, expected) in [
        (0, 0, 0, HardwareModel::Dmg),
        (0x80, 0, 0, HardwareModel::Cgb),
        (0, 3, 0x33, HardwareModel::Sgb),
        (0x80, 3, 0x33, HardwareModel::Sgb),
        (0xC0, 3, 0x33, HardwareModel::Cgb),
        (0x80, 3, 0, HardwareModel::Cgb),
    ] {
        let mut rom = vec![0; 32768];
        rom[0x143] = flag;
        rom[0x146] = sgb_flag;
        rom[0x14B] = license;
        let automatic = GameBoy::load(&rom, &[], HardwareModel::Auto).unwrap();
        assert_eq!(
            automatic.cpu().sgb.is_some(),
            expected == HardwareModel::Sgb
        );
        assert_eq!(automatic.cpu().is_cgb, expected == HardwareModel::Cgb);
        let forced = GameBoy::load(&rom, &[], HardwareModel::Cgb).unwrap();
        assert!(forced.cpu().sgb.is_none());
        assert!(forced.cpu().is_cgb);
    }
}

#[path = "../tests/fixtures/border_rom.rs"]
mod border_rom;

#[test]
fn hiding_borders_changes_presentation_only_and_preserves_transfer_and_state() {
    let mut gb = GameBoy::load(&border_rom::make_rom(), &[0; 256], HardwareModel::Auto).unwrap();
    gb.cpu_mut().booting = false;
    gb.cpu_mut().program_counter = 0x150;
    gb.run(70_224 * 40);
    assert!(gb.cpu().sgb.as_ref().unwrap().has_border());
    let bordered = gb.video_frame().pixels.to_vec();
    let state = gb.export_state();
    gb.set_border_visible(false);
    assert_eq!(gb.export_state(), state);
    let frame = gb.video_frame();
    assert_eq!((frame.geometry.width, frame.geometry.height), (160, 144));
    assert_eq!(&frame.pixels[..4], &[173, 173, 173, 255]);
    // The intentionally opaque border tile no longer covers the game.
    assert_eq!(
        &frame.pixels[(8 * 160 + 8) * 4..(8 * 160 + 9) * 4],
        &[173, 173, 173, 255]
    );
    gb.import_state(&state).unwrap();
    assert_eq!(gb.video_frame().geometry.width, 160);
    gb.set_border_visible(true);
    assert_eq!(gb.video_frame().pixels, bordered);
    gb.set_border_visible(false);
    gb.reset();
    gb.import_state(&state).unwrap();
    assert_eq!(gb.video_frame().geometry.width, 160);
}

#[test]
fn border_debug_views_show_the_full_snes_tile_store_and_map_independently() {
    let mut gb = GameBoy::load(&border_rom::make_rom(), &[0; 256], HardwareModel::Auto).unwrap();
    gb.cpu_mut().booting = false;
    gb.cpu_mut().program_counter = 0x150;
    gb.run(70_224 * 40);
    let sgb = gb.cpu().sgb.as_ref().unwrap();
    let tiles = sgb.debug_border_tiles();
    assert_eq!(tiles.len(), 384 * 128 * 4);
    assert_eq!(&tiles[8 * 4..9 * 4], &[255, 0, 0, 255]); // low-half tile 1 / palette 4
    let offset = (64 * 384 + 128) * 4;
    assert_eq!(&tiles[offset..offset + 4], &[0, 255, 0, 255]); // high-half tile128 / palette5
    let map = sgb.debug_border_map();
    assert_eq!(map.len(), 256 * 224 * 4);
    assert_eq!(&map[..4], &[0, 255, 0, 255]);
    assert_eq!(&map[(40 * 256 + 48) * 4..(40 * 256 + 49) * 4], &[255; 4]);
}

//! Pixel-for-pixel comparisons against upstream reference images.
#[path = "support/graphics_harness.rs"]
mod graphics_harness;
use graphics_harness::check_graphics;

#[test]
fn dmg_acid2_reference() {
    check_graphics("testroms/artifacts/dmg-acid2/dmg-acid2.gb", "testroms/artifacts/dmg-acid2/dmg-acid2-dmg.png", false, false);
}

#[test]
fn cgb_acid2_reference() {
    check_graphics("testroms/artifacts/cgb-acid2/cgb-acid2.gbc", "testroms/artifacts/cgb-acid2/cgb-acid2.png", true, false);
}

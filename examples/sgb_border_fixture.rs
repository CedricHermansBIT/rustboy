//! Generate the original synthetic border ROM used in regression tests.
#[path = "../crates/gameboy/tests/fixtures/border_rom.rs"]
mod border_rom;

fn main() {
    let path = std::env::args()
        .nth(1)
        .expect("Pass a new output ROM filename");
    // Refuse to overwrite user cartridges. The fixture is generated, not stored
    // in Git or included in the public game catalog.
    let mut file = std::fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(&path)
        .expect("output must be a new file");
    std::io::Write::write_all(&mut file, &border_rom::make_rom()).unwrap();
    println!("Generated synthetic SGB border cartridge: {path}");
}

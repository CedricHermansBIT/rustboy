//! Generate the original synthetic palette/attribute ROM used in tests.
#[path = "../crates/gameboy/tests/fixtures/palette_rom.rs"]
mod palette_rom;

fn main() {
    let path = std::env::args()
        .nth(1)
        .expect("Pass a new output ROM filename");
    let mut file = std::fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(&path)
        .expect("output must be a new file");
    std::io::Write::write_all(&mut file, &palette_rom::make_rom()).unwrap();
    println!("Generated synthetic SGB palette cartridge: {path}");
}

//! Generate our firmware-free synthetic SGB music cartridge for browser tests.
#[path = "../crates/gameboy/tests/fixtures/audio_rom.rs"]
mod audio_rom;
fn main() {
    let path = std::env::args()
        .nth(1)
        .expect("Pass a new output ROM filename");
    let mut file = std::fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(&path)
        .expect("output must be a new file");
    std::io::Write::write_all(&mut file, &audio_rom::make_rom()).unwrap();
    println!("Generated original SGB audio cartridge: {path}");
}

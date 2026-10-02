//! Original two-note timing probe against optional, locally supplied firmware.
use rustboy_snes_apu::{apu::Apu, firmware::load_sgb_firmware, sgb_player::Player};
fn score(apu: &mut Apu) {
    apu.bus.ram.write_wrapping(0x2b00, &[0x20, 0x2b]);
    apu.bus
        .ram
        .write_wrapping(0x2b20, &[0x30, 0x2b, 0xff, 0, 0x20, 0x2b]);
    apu.bus.ram.write_wrapping(
        0x2b30,
        &[0x50, 0x2b, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    );
    apu.bus
        .ram
        .write_wrapping(0x2b50, &[0xe7, 29, 0xe0, 7, 6, 0x7f, 0xa4, 0xa7, 0xab, 0]);
}
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let path = std::env::args().nth(1).ok_or("Supply local SGB firmware")?;
    let mut original = load_sgb_firmware(&std::fs::read(path)?)?;
    original.run(262144);
    original.drain_samples();
    println!("original timer targets={:?}", original.timer_targets());
    score(&mut original);
    original.bus.input = [1, 0, 0, 0];
    let mut own = Apu::default();
    let mut player = Player::initialize(&mut own);
    score(&mut own);
    player.command(&mut own, [0, 0, 0, 1]);
    let mut before = [0; 2];
    for clock in 0..1_024_000 {
        original.run(1);
        player.clock(&mut own);
        own.run(1);
        for (index, apu) in [&original, &own].into_iter().enumerate() {
            let pitch = u16::from_le_bytes([apu.bus.dsp.read(2), apu.bus.dsp.read(3)]);
            if pitch != before[index] {
                println!(
                    "{} {:.3} ms pitch={pitch:04x}",
                    if index == 0 { "original" } else { "own" },
                    clock as f64 / 1024.
                );
                before[index] = pitch;
            }
        }
        original.pop_sample();
        own.pop_sample();
    }
    Ok(())
}

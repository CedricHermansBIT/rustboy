//! Optional local-firmware oracle for the documented instrument tuning interface.
//! The score is ours; no firmware or extracted samples are saved or bundled.
use rustboy_snes_apu::{firmware::load_sgb_firmware, sgb_player::Player};
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let path = std::env::args()
        .nth(1)
        .ok_or("Usage: sgb_pitch_probe LOCAL_SGB_FIRMWARE")?;
    for instrument in [2, 7, 48] {
        let mut apu = load_sgb_firmware(&std::fs::read(&path)?)?;
        apu.run(262144);
        apu.drain_samples();
        apu.bus.ram.write_wrapping(0x2b00, &[0x20, 0x2b]);
        apu.bus.ram.write_wrapping(0x2b20, &[0x30, 0x2b, 0, 0]);
        apu.bus.ram.write_wrapping(
            0x2b30,
            &[0x50, 0x2b, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        );
        apu.bus
            .ram
            .write_wrapping(0x2b50, &[0xe7, 40, 0xe0, instrument, 96, 0x7f, 0xa4, 0]);
        apu.bus.input = [1, 0, 0, 0];
        apu.run(200000);
        let descriptor = &apu.bus.ram.bytes()
            [0x4c30 + instrument as usize * 6..0x4c36 + instrument as usize * 6];
        println!(
            "instrument {instrument} descriptor={descriptor:02x?} C3 pitch={:04x}",
            u16::from_le_bytes([apu.bus.dsp.read(2), apu.bus.dsp.read(3)])
        );
        let mut replacement = rustboy_snes_apu::apu::Apu::default();
        let mut player = Player::initialize(&mut replacement);
        replacement
            .bus
            .ram
            .write_wrapping(0x2b00, &apu.bus.ram.bytes()[0x2b00..0x4b00]);
        replacement
            .bus
            .ram
            .write_wrapping(0x4c30 + instrument as u16 * 6, descriptor);
        player.command(&mut replacement, [0, 0, 0, 1]);
        for _ in 0..200000 {
            player.clock(&mut replacement);
            replacement.run(1);
            replacement.pop_sample();
        }
        println!(
            "same descriptor replacement C3 pitch={:04x}",
            u16::from_le_bytes([replacement.bus.dsp.read(2), replacement.bus.dsp.read(3)])
        );
    }
    Ok(())
}

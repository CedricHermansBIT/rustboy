//! Inspect a locally supplied cartridge without adding commercial test data.
use rustboy_gameboy::{emulator::Emulator, GameBoy, HardwareModel};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut args = std::env::args().skip(1);
    let path = args.next().ok_or("Usage: sgb_inspect ROM [frames]")?;
    let frames: usize = args.next().map(|v| v.parse()).transpose()?.unwrap_or(1200);
    let rom = std::fs::read(path)?;
    let mut gb = GameBoy::load(&rom, &[], HardwareModel::Sgb)?;
    let mut previous = None;
    for frame in 0..frames {
        gb.run(70_224);
        let adapter = gb.cpu().sgb.as_ref().unwrap();
        let status = (
            adapter.commands_received,
            adapter.screen_mask(),
            adapter.has_border(),
            adapter.transfers_pending(),
            adapter.unsupported,
        );
        if previous.as_ref() != Some(&status) {
            println!("frame={frame} pc={:04X} lcdc={:02X} commands={} mask={} border={} pending={} drops={}",
                gb.cpu().program_counter, gb.cpu().peek(0xFF40), status.0, status.1, status.2, status.3, adapter.transfer_drops);
            for (command, count) in status
                .4
                .iter()
                .enumerate()
                .filter(|(_, count)| **count != 0)
            {
                println!("  unsupported {command:02X}: {count}");
            }
            previous = Some(status);
        }
    }
    let packed = &gb.cpu().frame_buffer;
    let shades: [usize; 4] = std::array::from_fn(|s| {
        packed
            .iter()
            .filter(|p| ((*p >> 27) & 3) as usize == s)
            .count()
    });
    let frame = gb.video_frame();
    let mut colors = std::collections::BTreeSet::new();
    let x0 = if frame.geometry.width == 256 { 48 } else { 0 };
    let y0 = if frame.geometry.height == 224 { 40 } else { 0 };
    for y in y0..y0 + 144 {
        for x in x0..x0 + 160 {
            let offset = (y * frame.geometry.width as usize + x) * 4;
            colors.insert(frame.pixels[offset..offset + 4].to_vec());
        }
    }
    println!("Final LCD shade counts: {shades:?}; game-window RGBA colors: {colors:?}");
    Ok(())
}

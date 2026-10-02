//! Read-only diagnostics for locally supplied cartridges, without bundled ROMs.
use rustboy_gameboy::{
    emulator::{Button, Emulator},
    GameBoy, HardwareModel,
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    for path in std::env::args().skip(1) {
        let mut rom = std::fs::read(&path)?;
        if rom.len() < 0x150 {
            return Err(format!("{path}: cartridge header is truncated").into());
        }
        if let Ok(mapper) = std::env::var("RUSTBOY_MAPPER") {
            let value = u8::from_str_radix(&mapper, 16)?;
            println!(
                "Diagnostic in-memory mapper override {:02X} -> {value:02X}; file unchanged",
                rom[0x147]
            );
            rom[0x147] = value;
        }
        let boot = std::env::var("RUSTBOY_BOOT_ROM")
            .ok()
            .map(std::fs::read)
            .transpose()?
            .unwrap_or_default();
        let mut gb = GameBoy::load(&rom, &boot, HardwareModel::Auto)?;
        println!(
            "{path}: {} bytes, header mapper {:02X}, {}",
            rom.len(),
            rom[0x147],
            gb.system_name()
        );
        for frame in 0..1200 {
            if frame == 600 {
                gb.set_button(0, Button::Start, true)?;
            }
            if frame == 606 {
                gb.set_button(0, Button::Start, false)?;
            }
            if frame == 800 {
                gb.set_button(0, Button::A, true)?;
            }
            if frame == 806 {
                gb.set_button(0, Button::A, false)?;
            }
            gb.run(70_224);
            if frame % 200 == 199 {
                println!("frame {}: {}", frame + 1, gb.cpu().get_debug_state());
            }
        }
        println!(
            "mapper {:?}, ROM bank {}, C20C={:02X}",
            gb.cpu().mbc.kind,
            gb.cpu().mbc.rombank,
            gb.cpu().peek(0xC20C)
        );
        println!(
            "CGB native {}, double speed {}, VRAM nonzero {}, BG palettes {:?}, C200..C21F {:?}",
            gb.cpu().cgb_native_mode(),
            gb.cpu().double_speed,
            gb.cpu().cgb_vram[0].iter().filter(|&&b| b != 0).count(),
            &gb.cpu().cgb_bg_palettes[..32],
            (0xC200..0xC220)
                .map(|address| gb.cpu().peek(address))
                .collect::<Vec<_>>()
        );
        let frame = gb.video_frame();
        let colors: std::collections::BTreeSet<_> =
            frame.pixels.chunks_exact(4).map(|p| p.to_vec()).collect();
        println!(
            "{}x{}, {} distinct RGBA colors",
            frame.geometry.width,
            frame.geometry.height,
            colors.len()
        );
        gb.cpu_mut().set_instruction_tracing(true);
        gb.run(70_224);
        println!("Recent instructions: {:?}", gb.cpu().get_last_traces(12));
    }
    Ok(())
}

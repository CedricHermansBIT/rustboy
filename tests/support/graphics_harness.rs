use rustboy::cpu::CPU;
use std::{fs::File, io::BufReader};

fn reference(path: &str) -> Vec<[u8; 3]> {
    let mut decoder = png::Decoder::new(BufReader::new(File::open(path).unwrap()));
    decoder.set_transformations(png::Transformations::EXPAND | png::Transformations::STRIP_16);
    let mut reader = decoder.read_info().unwrap();
    let mut bytes = vec![0; reader.output_buffer_size().unwrap()];
    let info = reader.next_frame(&mut bytes).unwrap();
    assert_eq!((info.width, info.height), (160, 144));
    bytes[..info.buffer_size()].chunks_exact(info.color_type.samples()).map(|pixel| {
        match info.color_type {
            png::ColorType::Grayscale | png::ColorType::GrayscaleAlpha => [pixel[0]; 3],
            png::ColorType::Rgb | png::ColorType::Rgba => [pixel[0], pixel[1], pixel[2]],
            _ => panic!("PNG expansion failed"),
        }
    }).collect()
}

pub fn check_graphics(rom: &str, image: &str, cgb: bool, software_breakpoint: bool) {
    let mut cpu = CPU::new();
    cpu.bootload(std::fs::read(if cgb { "roms/cgb_boot.bin" } else { "roms/dmg_boot.bin" }).unwrap());
    cpu.load_rom(std::fs::read(rom).unwrap());
    cpu.is_cgb = cgb;
    cpu.apu.set_cgb_mode(cgb);
    if !cgb {
        let gray = [[255, 255, 255, 255], [170, 170, 170, 255], [85, 85, 85, 255], [0, 0, 0, 255]];
        cpu.gbc_palettes = [gray; 3];
        cpu.color_mode = 0;
    }
    // Include the entire boot animation and allow the test image to settle.
    let trace_line = std::env::var("GRAPHICS_TRACE_LINE").ok().map(|value|
        value.parse::<u8>().expect("GRAPHICS_TRACE_LINE must be a scanline number"));
    let mut breakpoint_seen = false;
    'frames: for _ in 0..600 {
        let mut dots = 0;
        while dots < 70_224 {
            if software_breakpoint && !cpu.booting && cpu.peek_byte(cpu.program_counter as usize) == 0x40 {
                breakpoint_seen = true;
                break 'frames;
            }
            let old_line = cpu.memory[0xFF44];
            cpu.execute();
            if !cpu.booting && trace_line == Some(old_line) && cpu.memory[0xFF44] != old_line {
                eprintln!("{rom}: line {old_line}, LCDC={:02X}, SCX={}, WX={}",
                    cpu.ppu_line_snapshot.lcdc, cpu.ppu_line_snapshot.scx, cpu.ppu_line_snapshot.wx);
                for change in &cpu.ppu_reg_log[..cpu.ppu_reg_log_len] {
                    eprintln!("  dot {}: {:04X} <- {:02X}", change.dot, change.addr, change.value);
                }
            }
            dots += cpu.cycles * if cpu.double_speed { 2 } else { 4 };
            cpu.total_cycles += cpu.cycles as u64;
            cpu.cycles = 0;
            if cpu.booting && cpu.program_counter == 0x100 { cpu.check_boot_finish(); }
        }
        cpu.get_audio_buffer();
    }
    assert!(!software_breakpoint || breakpoint_seen, "software breakpoint not reached: {rom}");
    assert!(!cpu.booting, "boot did not finish: {rom}");
    if std::env::var_os("GRAPHICS_TRACE").is_some() {
        eprintln!("{rom}: breakpoint PC={:04X}, LY={}, dot={}, palettes={:?}", cpu.program_counter, cpu.memory[0xFF44], cpu.ppu_scanline_dot, &cpu.cgb_bg_palettes[..24]);
    }
    let expected = reference(image);
    let actual: Vec<[u8; 3]> = cpu.frame_buffer.iter().map(|pixel| {
        [*pixel as u8, (*pixel >> 8) as u8, (*pixel >> 16) as u8]
    }).collect();
    let mismatches: Vec<_> = actual.iter().zip(&expected).enumerate()
        .filter(|(_, (a, e))| a != e).collect();
    if !mismatches.is_empty() {
        eprintln!("{rom}: {} differing pixels; first mismatches:", mismatches.len());
        for (index, (a, e)) in mismatches.iter().take(12) {
            eprintln!("({}, {}): actual {a:?}, expected {e:?}", index % 160, index / 160);
        }
        if let Ok(directory) = std::env::var("GRAPHICS_ARTIFACT_DIR") {
            std::fs::create_dir_all(&directory).unwrap();
            let name = std::path::Path::new(rom).file_stem().unwrap().to_string_lossy();
            let mut encoder = png::Encoder::new(File::create(format!("{directory}/{name}.png")).unwrap(), 160, 144);
            encoder.set_color(png::ColorType::Rgb);
            encoder.set_depth(png::BitDepth::Eight);
            encoder.write_header().unwrap().write_image_data(&actual.iter().flatten().copied().collect::<Vec<_>>()).unwrap();
        }
    }
    assert!(mismatches.is_empty(), "reference image mismatch: {rom}");
}

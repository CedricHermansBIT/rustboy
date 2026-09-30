use rustboy::cpu::CPU;

#[test]
#[ignore = "Requires a locally supplied commercial ROM"]
fn pinball_gameplay_stack_stays_bounded() {
    let mut cpu = CPU::new();
    cpu.bootload(std::fs::read("roms/dmg_boot.bin").unwrap());
    cpu.load_rom(std::fs::read("roms/Pinball Mania (Europe).gb").unwrap());
    let mut minimum_sp = u16::MAX;
    for frame in 0..2800u32 {
        let start = frame >= 300 && frame % 180 < 3;
        let a = frame >= 390 && frame % 180 < 3;
        cpu.set_keys(13, start);
        cpu.set_keys(65, a);
        if start || a {
            cpu.request_interrupt(4);
        }
        let target = cpu.total_cycles + 17_556;
        while cpu.total_cycles < target {
            cpu.execute();
            let cycles = cpu.cycles as u64;
            cpu.handle_timer((cycles * 4) as u32);
            cpu.total_cycles += cycles;
            cpu.cycles = 0;
        }
        if frame >= 2630 {
            minimum_sp = minimum_sp.min(cpu.get_sp());
        }
    }
    assert!(
        minimum_sp >= 0xDF00,
        "gameplay interrupt handlers leaked stack into WRAM: {minimum_sp:04X}"
    );
}

#[test]
#[ignore = "Requires a locally supplied commercial ROM"]
fn cgb_pinball_menu_screenshot() {
    let path = "roms/Little Mermaid II, The - Pinball Frenzy (Europe) (En,Fr,De,Es,It).gbc";
    let mut cpu = CPU::new();
    cpu.bootload(std::fs::read("roms/cgb_boot.bin").unwrap());
    cpu.load_rom(std::fs::read(path).unwrap());
    for frame in 0..1800u32 {
        let start = frame > 300 && frame % 180 < 4;
        cpu.set_keys(13, start);
        cpu.set_keys(65, start);
        if start { cpu.request_interrupt(4); }
        let target = cpu.total_cycles + 17_556;
        while cpu.total_cycles < target {
            cpu.handle_interrupts();
            cpu.execute();
            let cycles = cpu.cycles as u64;
            cpu.handle_timer((cycles * 4) as u32);
            if cpu.booting && cpu.program_counter == 0x100 { cpu.check_boot_finish(); }
            cpu.total_cycles += cycles;
            cpu.cycles = 0;
        }
    }
    // Compare displayed RGB, not internal BG-priority metadata. The title's
    // eight-pixel sparkle at x=158..159,y=76..79 animates with DIV phase; it
    // is not part of the static HDMA menu regression. The reference hash was
    // computed from the original known-good menu with this same tiny mask.
    let frame_hash = cpu.frame_buffer.iter().enumerate()
        .filter(|(index, _)| !(index % 160 >= 158 && (76..80).contains(&(index / 160))))
        .fold(0xcbf29ce484222325u64, |hash, (_, pixel)| {
            pixel.to_le_bytes()[..3].iter().fold(hash, |h, byte| (h ^ *byte as u64).wrapping_mul(0x100000001b3))
        });
    eprintln!("{}\nframe hash: {frame_hash:016x}", cpu.get_debug_state());
    let mut ppm = format!("P6\n160 144\n255\n").into_bytes();
    for pixel in cpu.frame_buffer {
        ppm.extend_from_slice(&[(pixel & 255) as u8, ((pixel >> 8) & 255) as u8, ((pixel >> 16) & 255) as u8]);
    }
    std::fs::write("/tmp/rustboy-cgb-menu.ppm", ppm).unwrap();
    let mut encoder = png::Encoder::new(std::fs::File::create("/tmp/rustboy-cgb-menu.png").unwrap(), 160, 144);
    encoder.set_color(png::ColorType::Rgb);
    encoder.set_depth(png::BitDepth::Eight);
    let rgb: Vec<u8> = cpu.frame_buffer.iter().flat_map(|pixel| [*pixel as u8, (*pixel >> 8) as u8, (*pixel >> 16) as u8]).collect();
    encoder.write_header().unwrap().write_image_data(&rgb).unwrap();
    assert_eq!(frame_hash, 0x5060cd45a77a7de2, "CGB HDMA regression corrupted the title menu");
}

use rustboy::cpu::CPU;

#[test]
#[ignore]
fn pinball_gameplay_stack_stays_bounded() {
    if !std::path::Path::new("roms/Pinball Mania (Europe).gb").exists()
        || !std::path::Path::new("roms/dmg_boot.bin").exists()
    {
        return;
    }
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
#[ignore]
fn cgb_pinball_menu_screenshot() {
    let path = "roms/Little Mermaid II, The - Pinball Frenzy (Europe) (En,Fr,De,Es,It).gbc";
    if !std::path::Path::new(path).exists() { return; }
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
    let frame_hash = cpu.frame_buffer.iter().fold(0xcbf29ce484222325u64, |hash, pixel| {
        pixel.to_le_bytes().iter().fold(hash, |h, byte| (h ^ *byte as u64).wrapping_mul(0x100000001b3))
    });
    assert_eq!(frame_hash, 0x209b647ee9b88c45, "CGB HDMA regression corrupted the title menu");
    eprintln!("{}\nframe hash: {frame_hash:016x}", cpu.get_debug_state());
    let mut ppm = format!("P6\n160 144\n255\n").into_bytes();
    for pixel in cpu.frame_buffer {
        ppm.extend_from_slice(&[(pixel & 255) as u8, ((pixel >> 8) & 255) as u8, ((pixel >> 16) & 255) as u8]);
    }
    std::fs::write("/tmp/rustboy-cgb-menu.ppm", ppm).unwrap();
}

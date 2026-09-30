use rustboy::cpu::CPU;
use std::path::Path;
use std::env;

/// Check if tracing should be enabled for this ROM path
fn should_trace(path: &str) -> bool {
    if let Ok(trace_test) = env::var("TRACE_TEST") {
        path.contains(&trace_test)
    } else {
        false
    }
}

/// Run a ROM for up to `max_cycles` M-cycles, stopping early if the
/// Mooneye finish signature is detected or a Blargg magic string appears.
fn run_rom(path: &str, max_cycles: u64) -> CPU {
    run_rom_with_protocol(path, max_cycles, true)
}

fn run_rom_with_protocol(path: &str, max_cycles: u64, detect_protocol: bool) -> CPU {
    let rom = std::fs::read(path).unwrap_or_else(|e| panic!("Failed to read ROM {}: {}", path, e));

    // Run the matching hardware boot ROM, including its DIV/APU setup.
    // A dual-mode cartridge flag is not a hardware requirement: DMG-only
    // suites must run on DMG even if their generic test shell supports CGB.
    let use_cgb = rom.get(0x143).copied().unwrap_or(0) & 0x80 != 0
        && !path.contains("/oam_bug/") && !path.contains("/dmg_sound/");
    let boot_path = if use_cgb {
        "roms/cgb_boot.bin"
    } else {
        "roms/dmg_boot.bin"
    };
    let boot_rom = std::fs::read(boot_path).expect("Failed to read boot ROM");

    let mut cpu = CPU::new();

    // Enable tracing if requested (but delay actual tracing until after boot ROM)
    let enable_trace = should_trace(path);
    let mut tracing_started = false;
    if enable_trace {
        eprintln!("\n🟢 Instruction tracing ENABLED for: {} (will start after boot ROM)", path);
    }

    cpu.bootload(boot_rom);
    cpu.load_rom(rom);
    cpu.is_cgb = use_cgb;
    cpu.apu.set_cgb_mode(use_cgb);

    let mut total: u64 = 0;
    let mut next_audio_drain = 17_556;
    while total < max_cycles {
        cpu.handle_interrupts();
        cpu.execute();

        let cycles = cpu.cycles as u64;
        let t_cycles = (cycles * 4) as u32;
        cpu.handle_timer(t_cycles);
        total += cycles;
        cpu.total_cycles += cycles;
        cpu.cycles = 0;
        if total >= next_audio_drain {
            cpu.get_audio_buffer();
            next_audio_drain = total + 17_556;
        }

        // Start tracing once we reach the cartridge entry point (0x0100 or higher)
        // This skips the 180,000+ cycle boot ROM execution and focuses on the cart ROM
        if enable_trace && !tracing_started && cpu.program_counter >= 0x0100 {
            cpu.set_instruction_tracing(true);
            tracing_started = true;
            eprintln!("  ✓ Tracing started at PC=0x{:04X}", cpu.program_counter);
        }

        // GBMicrotest publishes a terminal result byte.
        if detect_protocol && path.contains("/gbmicrotest/") && cpu.peek_byte(0xFF82) != 0 {
            break;
        }

        // Detect JR -2 infinite loop (0x18 0xFE). Use the bus-facing ROM
        // view: `memory` only mirrors the first ROM window at load time.
        let addr = cpu.program_counter as usize;
        if detect_protocol && addr + 1 < 0x1_0000
            && cpu.peek_byte(addr) == 0x18
            && cpu.peek_byte(addr + 1) == 0xFE
        {
            break;
        }

        // Detect LD B,B (0x40) followed by JR -2 (Mooneye finish)
        if detect_protocol && addr + 2 < 0x1_0000
            && cpu.peek_byte(addr) == 0x40
            && cpu.peek_byte(addr + 1) == 0x18
            && cpu.peek_byte(addr + 2) == 0xFE
        {
            cpu.execute();
            cpu.cycles = 0;
            break;
        }
    }

    // Export trace if requested
    if enable_trace {
        export_trace_if_needed(&cpu, path);
    }

    cpu
}

/// Export CPU trace to file or stdout
fn export_trace_if_needed(cpu: &CPU, path: &str) {
    let trace_count = cpu.trace_count();
    eprintln!("\n📊 Trace Statistics:");
    eprintln!("  Total instructions: {}", trace_count);

    // Print statistics
    let stats = cpu.get_trace_statistics();
    for line in stats.lines() {
        eprintln!("  {}", line);
    }

    // Print last 20 traces
    eprintln!("\n📍 Last 20 Instructions:");
    for trace_line in cpu.get_last_traces(20).iter() {
        eprintln!("  {}", trace_line);
    }

    // Optionally export as CSV
    if env::var("TRACE_CSV").is_ok() {
        let test_name = Path::new(path)
            .file_stem()
            .unwrap_or_default()
            .to_string_lossy();
        let csv_path = format!("trace_{}.csv", test_name);
        match std::fs::write(&csv_path, cpu.export_instruction_traces_csv()) {
            Ok(_) => eprintln!("\n✅ Trace exported to: {}", csv_path),
            Err(e) => eprintln!("\n⚠️  Failed to export trace: {}", e),
        }
    }
}

/// Check if a Mooneye test passed (B=3,C=5,D=8,E=13,H=21,L=34).
fn mooneye_passed(cpu: &CPU) -> bool {
    cpu.get_reg_b() == 3
        && cpu.get_reg_c() == 5
        && cpu.get_reg_d() == 8
        && cpu.get_reg_e() == 13
        && cpu.get_reg_h() == 21
        && cpu.get_reg_l() == 34
}

fn mooneye_state(cpu: &CPU) -> String {
    format!(
        "B={:02X} C={:02X} D={:02X} E={:02X} H={:02X} L={:02X} A={:02X} F={:02X} PC={:04X}",
        cpu.get_reg_b(), cpu.get_reg_c(), cpu.get_reg_d(), cpu.get_reg_e(),
        cpu.get_reg_h(), cpu.get_reg_l(), cpu.get_reg_a(), cpu.get_reg_f(),
        cpu.program_counter
    )
}

fn run_micro_test(rom_path: &str) {
    // These tests usually finish very quickly (a few hundred cycles)
    let cpu = run_rom(rom_path, 10_000_000);

    let test_result = cpu.peek_byte(0xFF80);
    let expected    = cpu.peek_byte(0xFF81);
    let pass_flag   = cpu.peek_byte(0xFF82);

    println!("Test: {}", rom_path);
    println!("  Memory: [0xFF80]={:02X}, [0xFF81]={:02X}, [0xFF82]={:02X}",
             test_result, expected, pass_flag);

    // Pass condition: 0xFF82 must be 0x01
    assert_eq!(pass_flag, 0x01,
               "FAIL: {} - Got {:02X}, Expected {:02X}",
               rom_path, test_result, expected);
}

/// Older micro-ROMs display a raw byte instead of publishing FF82. Their
/// expected byte and store instruction are recorded from the original source.
/// Run past boot/setup and check both the result and the repeating publisher;
/// an untouched zero-initialized byte alone must never count as a pass.
fn run_legacy_test(rom_path: &str, address: usize, expected: u8, publisher: usize) {
    let cpu = run_rom_with_protocol(rom_path, 10_000_000, false);
    assert!(!cpu.booting, "boot did not finish: {rom_path}");
    let pc = cpu.program_counter as usize;
    assert!((publisher..publisher + 8).contains(&pc),
        "publisher loop not reached: {rom_path}, PC={pc:04X}");
    assert_eq!(cpu.get_reg_a(), expected, "raw result register: {rom_path}");
    assert_eq!(cpu.peek_byte(address), expected, "raw result at {address:04X}: {rom_path}");
}
fn run_mooneye_test(rom_path: &str) {
    let cpu = run_rom(rom_path, 50_000_000);
    let state = mooneye_state(&cpu);
    let serial = String::from_utf8_lossy(&cpu.serial_output);
    println!("Test: {}", rom_path);
    println!("  Registers: {}", state);
    if !serial.is_empty() {
        println!("  Serial: {}", serial.replace('\n', " | "));
    }
    assert!(mooneye_passed(&cpu), "FAIL: {} — {}", rom_path, state);
}

fn run_blargg_test(rom_path: &str) {
    let cpu = run_rom(rom_path, 200_000_000);
    let serial = String::from_utf8_lossy(&cpu.serial_output).to_string();
    println!("Test: {}", rom_path);

    // Blargg tests output results to BOTH serial AND RAM at 0xA004+
    // (with magic signature DE B0 61 at 0xA001-0xA003).
    // "rom_singles" variants only write to RAM, not serial.
    let has_ram_signature = cpu.peek_byte(0xA001) == 0xDE
        && cpu.peek_byte(0xA002) == 0xB0
        && cpu.peek_byte(0xA003) == 0x61;

    let mut ram_text = String::new();
    if has_ram_signature {
        for i in 0xA004..0xC000 {
            let b = cpu.peek_byte(i);
            if b == 0 { break; }
            ram_text.push(b as char);
        }
    }

    // Some original Blargg cartridges declare no external RAM at all. Their
    // LCD console is still an ASCII result channel; do not invent RAM solely
    // to make the harness work. Inverse text uses bit 7 of its tile index.
    let mut screen_text = String::new();
    if !has_ram_signature && serial.is_empty() && cpu.mbc.get_ram_size() == 0 {
        let vram = if cpu.cgb_native_mode() { &cpu.cgb_vram[0][..] }
            else { &cpu.memory[0x8000..0xA000] };
        for row in 0..32 {
            for column in 0..20 {
                screen_text.push((vram[0x1800 + row * 32 + column] & 0x7F) as char);
            }
            screen_text.push('\n');
        }
    }
    let output = if !serial.is_empty() { &serial }
        else if has_ram_signature { &ram_text } else { &screen_text };

    if !serial.is_empty() {
        println!("  Serial: {}", serial.replace('\n', " | "));
    }
    if !ram_text.is_empty() {
        println!("  RAM: {}", ram_text.replace('\n', " | "));
    }
    if !screen_text.is_empty() { println!("  LCD console: {}", screen_text.replace('\n', " | ")); }

    // The RAM protocol's status byte is authoritative. Long diagnostics can
    // fill its text buffer before the final "Passed" string is appended.
    let passed = if has_ram_signature { cpu.peek_byte(0xA000) == 0 }
        else { output.contains("Passed") };
    assert!(
        passed,
        "FAIL: {} — RAM status={:02X}, PC={:04X}, output: {}",
        rom_path,
        cpu.peek_byte(0xA000), cpu.program_counter,
        output.replace('\n', " | ")
    );
}

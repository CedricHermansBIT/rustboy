use rustboy::cpu::CPU;

#[test]
fn debug_boot_rom_milestones() {
    let rom = std::fs::read("testroms/boot_regs-dmgABC.gb").unwrap();
    let boot = std::fs::read("roms/dmg_boot.bin").unwrap();
    let mut cpu = CPU::new();
    cpu.bootload(boot);
    cpu.load_rom(rom);

    // Phase-correct instruction PCs (avoid immediate-byte addresses like 0x0068+1).
    let milestones = [
        0x0000, 0x000C, 0x001D, 0x0027, 0x0034, 0x003E, 0x0055,
        0x0064, 0x0066, 0x0068, 0x006A, 0x006B, 0x006D, 0x006E,
        0x0070, 0x0074, 0x0090, 0x00E0, 0x00E9, 0x00FA, 0x00FC,
    ];

    let mut hit_counts = std::collections::HashMap::new();
    let mut last_milestone_cycles: Option<u64> = None;
    let mut last_milestone_sys: Option<u16> = None;

    println!("PC   | Total Cycles | Sys | dM      | SysErr | LY  | M | LCD | IF | Note");
    println!("--------------------------------------------------------------------------------");

    while cpu.booting {
        let pc = cpu.program_counter;
        let count = hit_counts.entry(pc).or_insert(0);
        *count += 1;

        if milestones.contains(&pc) && *count == 1 {
            let ly = cpu.peek_byte(0xFF44);
            let stat = cpu.peek_byte(0xFF41);
            let lcdc = cpu.peek_byte(0xFF40);
            let iflags = cpu.peek_byte(0xFF0F);
            let mode = stat & 0x03;
            let lcd_on = (lcdc >> 7) & 1;

            let (delta_m, sys_err) = if let (Some(prev_cycles), Some(prev_sys)) =
                (last_milestone_cycles, last_milestone_sys)
            {
                let delta_m = cpu.total_cycles - prev_cycles;
                let expected_sys = prev_sys.wrapping_add(((delta_m * 4) & 0xFFFF) as u16);
                let raw = cpu.sys_counter.wrapping_sub(expected_sys);
                let signed = if raw >= 0x8000 {
                    (raw as i32) - 0x10000
                } else {
                    raw as i32
                };
                (format!("{:7}", delta_m), format!("{:6}", signed))
            } else {
                ("      -".to_string(), "     -".to_string())
            };

            let note = match pc {
                0x0000 => "Start of boot ROM",
                0x000C => "VRAM clear done",
                0x001D => "Audio init done",
                0x0027 => "BG palette done",
                0x0034 => "Tile data decompressed",
                0x003E => "Logo scroll prep",
                0x0055 => "LCD turned on",
                0x0064 => "VBlank wait #1 read LY",
                0x0066 => "VBlank wait #1 compare 0x90",
                0x0068 => "VBlank wait #1 JR C",
                0x006A => "Delay loop DEC C",
                0x006B => "Delay loop JR NZ (C)",
                0x006D => "Delay loop DEC E",
                0x006E => "Delay loop JR NZ (E)",
                0x0070 => "Scroll update",
                0x0074 => "Audio setup",
                0x0090 => "Audio delay section",
                0x00E0 => "Logo checksum done",
                0x00E9 => "Fade-out loops started",
                0x00FA => "Fade-out loops done",
                0x00FC => "Disabling boot ROM",
                _ => "",
            };

            println!(
                "{:04X} | {:12} | {:04X} | {} | {} | {:02X} | {} | {}   | {:02X} | {}",
                pc,
                cpu.total_cycles,
                cpu.sys_counter,
                delta_m,
                sys_err,
                ly,
                mode,
                lcd_on,
                iflags,
                note
            );

            last_milestone_cycles = Some(cpu.total_cycles);
            last_milestone_sys = Some(cpu.sys_counter);
        }

        cpu.execute();
        cpu.total_cycles += cpu.cycles as u64;
        cpu.cycles = 0;

        if !cpu.booting {
            println!("0100 | {:12} | {:11X} | Cartridge Entry", cpu.total_cycles, cpu.sys_counter);
            break;
        }
    }

    println!("--------------------------------------------------------------------------------");
    println!("VBlank wait loops (PC=0x0064): {}", hit_counts.get(&0x0064).unwrap_or(&0));
    println!("Expected Sys Counter at 0x0100: ABC8");
}

#[test]
fn debug_boot_vblank_wait_profile() {
    let rom = std::fs::read("testroms/boot_regs-dmgABC.gb").unwrap();
    let boot = std::fs::read("roms/dmg_boot.bin").unwrap();
    let mut cpu = CPU::new();
    cpu.bootload(boot);
    cpu.load_rom(rom);

    let mut hits_0064: u64 = 0;
    let mut hits_0068: u64 = 0;
    let mut hits_006e: u64 = 0;

    let mut lcd_on_at: Option<(u64, u8, u8)> = None;
    let mut first_ly_change_after_lcd_on: Option<(u64, u8, u8)> = None;
    let mut first_0064_hit: Option<(u64, u8)> = None;
    let mut first_exit_0068_to_006a: Option<(u64, u8)> = None;
    let mut first_exit_006e_to_0070: Option<(u64, u8)> = None;

    let mut prev_lcdc_on = (cpu.peek_byte(0xFF40) & 0x80) != 0;
    let mut ly_at_lcd_on: Option<u8> = None;
    let mut safety_steps: u64 = 0;
    let max_steps: u64 = 15_000_000;

    while cpu.booting && safety_steps < max_steps {
        safety_steps += 1;

        let pc = cpu.program_counter;
        let ly_before = cpu.peek_byte(0xFF44);
        let lcdc = cpu.peek_byte(0xFF40);
        let lcd_on = (lcdc & 0x80) != 0;

        if !prev_lcdc_on && lcd_on && lcd_on_at.is_none() {
            let stat = cpu.peek_byte(0xFF41);
            lcd_on_at = Some((cpu.total_cycles, ly_before, stat & 0x03));
            ly_at_lcd_on = Some(ly_before);
        }
        prev_lcdc_on = lcd_on;

        if lcd_on {
            if let Some(initial_ly) = ly_at_lcd_on {
                if first_ly_change_after_lcd_on.is_none() && ly_before != initial_ly {
                    first_ly_change_after_lcd_on = Some((cpu.total_cycles, initial_ly, ly_before));
                }
            }
        }

        if pc == 0x0064 {
            hits_0064 += 1;
            if first_0064_hit.is_none() {
                first_0064_hit = Some((cpu.total_cycles, ly_before));
            }
        }

        if pc == 0x0068 {
            hits_0068 += 1;
            cpu.execute();
            cpu.total_cycles += cpu.cycles as u64;
            cpu.cycles = 0;

            if cpu.program_counter == 0x006A && first_exit_0068_to_006a.is_none() {
                let ly_after = cpu.peek_byte(0xFF44);
                first_exit_0068_to_006a = Some((cpu.total_cycles, ly_after));
            }
            continue;
        }

        if pc == 0x006E {
            hits_006e += 1;
            cpu.execute();
            cpu.total_cycles += cpu.cycles as u64;
            cpu.cycles = 0;

            if cpu.program_counter == 0x0070 && first_exit_006e_to_0070.is_none() {
                let ly_after = cpu.peek_byte(0xFF44);
                first_exit_006e_to_0070 = Some((cpu.total_cycles, ly_after));
            }
            continue;
        }

        cpu.execute();
        cpu.total_cycles += cpu.cycles as u64;
        cpu.cycles = 0;
    }

    println!("=== Boot VBlank Wait Profile ===");
    println!("steps: {}", safety_steps);
    println!("PC 0064 hits: {}", hits_0064);
    println!("PC 0068 hits: {}", hits_0068);
    println!("PC 006E hits: {}", hits_006e);
    if let Some((cycles, ly, mode)) = lcd_on_at {
        println!("LCD on at cycles={} LY={:02X} mode={}", cycles, ly, mode);
    }
    if let Some((cycles, from_ly, to_ly)) = first_ly_change_after_lcd_on {
        println!("First LY change after LCD on at cycles={} LY {:02X}->{:02X}", cycles, from_ly, to_ly);
    }
    if let Some((cycles, ly)) = first_0064_hit {
        println!("First PC=0064 at cycles={} LY={:02X}", cycles, ly);
    }
    if let Some((cycles, ly)) = first_exit_0068_to_006a {
        println!("First exit 0068->006A at cycles={} LY={:02X}", cycles, ly);
    }
    if let Some((cycles, ly)) = first_exit_006e_to_0070 {
        println!("First exit 006E->0070 at cycles={} LY={:02X}", cycles, ly);
    }

    assert!(safety_steps < max_steps, "profiling loop hit safety cap before boot ROM exit");
}

#[test]
fn debug_boot_cycle_bisect() {
    let rom = std::fs::read("testroms/boot_regs-dmgABC.gb").unwrap();
    let boot = std::fs::read("roms/dmg_boot.bin").unwrap();
    let mut cpu = CPU::new();
    cpu.bootload(boot);
    cpu.load_rom(rom);

    let checkpoints = [
        0x0000, 0x000C, 0x001D, 0x0027, 0x0034, 0x003E, 0x0055,
        0x0064, 0x006A, 0x006B, 0x006D, 0x006E, 0x0070, 0x0074, 0x0090, 0x00E0, 0x00E9, 0x00FA, 0x00FC,
    ];
    let mut checkpoint_cycles: std::collections::BTreeMap<u16, u64> = std::collections::BTreeMap::new();

    let mut m_pre_lcd = 0_u64;
    let mut m_lcd_to_wait = 0_u64;
    let mut m_wait_ly90 = 0_u64;
    let mut m_wait_delay = 0_u64;
    let mut m_scroll_audio = 0_u64;
    let mut m_tail = 0_u64;
    let mut m_unbucketed = 0_u64;
    let mut unbucketed_pc_hits: std::collections::BTreeMap<u16, u64> = std::collections::BTreeMap::new();

    let mut hits_wait_ly90 = 0_u64;
    let mut hits_wait_delay = 0_u64;

    let mut br_0068_taken = 0_u64;
    let mut br_0068_fall = 0_u64;
    let mut br_0068_other = 0_u64;
    let mut br_006b_taken = 0_u64;
    let mut br_006b_fall = 0_u64;
    let mut br_006b_other = 0_u64;
    let mut br_006e_taken = 0_u64;
    let mut br_006e_fall = 0_u64;
    let mut br_006e_other = 0_u64;

    let mut cp90_ly_hist = [0_u64; 256];
    let mut loop_c_values = [0_u64; 256];
    let mut loop_e_values = [0_u64; 256];

    let mut steps = 0_u64;
    let max_steps = 15_000_000_u64;

    while cpu.booting && steps < max_steps {
        steps += 1;

        let pc = cpu.program_counter;
        let cycles_before = cpu.total_cycles;
        let ly_before = cpu.peek_byte(0xFF44);

        if checkpoints.contains(&pc) {
            checkpoint_cycles.entry(pc).or_insert(cycles_before);
        }

        if pc == 0x0066 {
            cp90_ly_hist[ly_before as usize] += 1;
        }
        if pc == 0x006A {
            loop_c_values[cpu.get_reg_c() as usize] += 1;
        }
        if pc == 0x006D {
            loop_e_values[cpu.get_reg_e() as usize] += 1;
        }

        cpu.execute();
        let delta_m = cpu.cycles as u64;
        cpu.total_cycles += delta_m;
        cpu.cycles = 0;

        match pc {
            0x0000..=0x0054 => m_pre_lcd += delta_m,
            0x0055..=0x0061 => m_lcd_to_wait += delta_m,
            0x0064 | 0x0066 | 0x0068 => {
                m_wait_ly90 += delta_m;
                hits_wait_ly90 += 1;
            }
            0x0062 | 0x0063 | 0x006A | 0x006B | 0x006D | 0x006E => {
                m_wait_delay += delta_m;
                hits_wait_delay += 1;
            }
            0x0070..=0x00DF => m_scroll_audio += delta_m,
            0x00E0..=0x00FC => m_tail += delta_m,
            _ => {
                m_unbucketed += delta_m;
                *unbucketed_pc_hits.entry(pc).or_insert(0) += 1;
            }
        }

        if pc == 0x0068 {
            match cpu.program_counter {
                0x0064 => br_0068_taken += 1,
                0x006A => br_0068_fall += 1,
                _ => br_0068_other += 1,
            }
        }

        if pc == 0x006B {
            match cpu.program_counter {
                0x0064 => br_006b_taken += 1,
                0x006D => br_006b_fall += 1,
                _ => br_006b_other += 1,
            }
        }

        if pc == 0x006E {
            match cpu.program_counter {
                0x0062 => br_006e_taken += 1,
                0x0070 => br_006e_fall += 1,
                _ => br_006e_other += 1,
            }
        }
    }

    let total_accounted =
        m_pre_lcd + m_lcd_to_wait + m_wait_ly90 + m_wait_delay + m_scroll_audio + m_tail + m_unbucketed;

    println!("=== Boot Cycle Bisect ===");
    println!("steps: {}", steps);
    println!("boot total m-cycles: {}", cpu.total_cycles);
    println!("accounted m-cycles:  {}", total_accounted);
    println!("phase pre_lcd         : {:12}", m_pre_lcd);
    println!("phase lcd_to_wait     : {:12}", m_lcd_to_wait);
    println!("phase wait_ly_0x90    : {:12} (hits={})", m_wait_ly90, hits_wait_ly90);
    println!("phase wait_delay      : {:12} (hits={})", m_wait_delay, hits_wait_delay);
    println!("phase scroll_audio    : {:12}", m_scroll_audio);
    println!("phase tail_00E0_00FC  : {:12}", m_tail);
    println!("phase unbucketed      : {:12}", m_unbucketed);

    println!(
        "branch 0068 JR C: taken={} fallthrough={} other={}",
        br_0068_taken, br_0068_fall, br_0068_other
    );
    println!(
        "branch 006B JR NZ: taken={} fallthrough={} other={}",
        br_006b_taken, br_006b_fall, br_006b_other
    );
    println!(
        "branch 006E JR NZ: taken={} fallthrough={} other={}",
        br_006e_taken, br_006e_fall, br_006e_other
    );

    println!("-- first-hit checkpoints (m-cycles) --");
    for &pc in checkpoints.iter() {
        if let Some(c) = checkpoint_cycles.get(&pc) {
            println!("  {:04X}: {}", pc, c);
        }
    }
    println!("  0100: {}", cpu.total_cycles);

    let expected_sys_0100 = 0xABC8_u16;
    let final_sys = cpu.sys_counter;
    let sys_diff = final_sys.wrapping_sub(expected_sys_0100);
    let m_offset = (sys_diff as u32) / 4;
    println!(
        "sys @0100: {:04X} expected {:04X} diff {:04X} (~{} M-cycles)",
        final_sys, expected_sys_0100, sys_diff, m_offset
    );

    let mut cp90_nonzero: Vec<(usize, u64)> = cp90_ly_hist
        .iter()
        .enumerate()
        .filter_map(|(ly, &count)| if count > 0 { Some((ly, count)) } else { None })
        .collect();
    cp90_nonzero.sort_by(|a, b| b.1.cmp(&a.1));
    let mut c_nonzero: Vec<(usize, u64)> = loop_c_values
        .iter()
        .enumerate()
        .filter_map(|(ly, &count)| if count > 0 { Some((ly, count)) } else { None })
        .collect();
    c_nonzero.sort_by(|a, b| b.1.cmp(&a.1));
    let mut e_nonzero: Vec<(usize, u64)> = loop_e_values
        .iter()
        .enumerate()
        .filter_map(|(ly, &count)| if count > 0 { Some((ly, count)) } else { None })
        .collect();
    e_nonzero.sort_by(|a, b| b.1.cmp(&a.1));

    println!("top LY values at CP 0x90 (PC=0066):");
    for (ly, count) in cp90_nonzero.into_iter().take(8) {
        println!("  LY={:02X} count={}", ly, count);
    }
    println!("top C values entering DEC C (PC=006A):");
    for (value, count) in c_nonzero.into_iter().take(8) {
        println!("  C={:02X} count={}", value, count);
    }
    println!("top E values entering DEC E (PC=006D):");
    for (value, count) in e_nonzero.into_iter().take(8) {
        println!("  E={:02X} count={}", value, count);
    }

    if !unbucketed_pc_hits.is_empty() {
        println!("unbucketed PCs (up to 16 shown):");
        for (pc, hits) in unbucketed_pc_hits.into_iter().take(16) {
            println!("  PC={:04X} hits={}", pc, hits);
        }
    }

    assert!(steps < max_steps, "bisect loop hit safety cap before boot ROM exit");
    assert_eq!(total_accounted, cpu.total_cycles, "phase accounting mismatch");
}


// ══════════════════════════════════════════════════════════════════


#[test]
fn timer_falling_edge_basic() {
    let mut cpu = CPU::new();
    cpu.program_counter = 0x100;
    cpu.booting = false;
    cpu.memory[0xFF05] = 0x00;
    cpu.memory[0xFF06] = 0x00;
    cpu.memory[0xFF07] = 0x00;
    cpu.write_byte(0xFF07, 0x05); // 262144 Hz
    for _ in 0..4 { cpu.tick_timer_4t(); }
    assert_eq!(cpu.memory[0xFF05], 1, "TIMA should be 1 after 4 M-cycles at 262144 Hz");
}

#[test]
fn timer_div_reset_causes_tick() {
    let mut cpu = CPU::new();
    cpu.sys_counter = 0;
    cpu.program_counter = 0x100;
    cpu.booting = false;
    cpu.memory[0xFF05] = 0x00;
    cpu.memory[0xFF06] = 0x00;
    cpu.write_byte(0xFF07, 0x04); // 4096 Hz, monitors bit 9
    for _ in 0..128 { cpu.tick_timer_4t(); } // bit 9 is now 1
    let tima_before = cpu.memory[0xFF05];
    cpu.write_byte(0xFF04, 0x00); // DIV reset → falling edge
    assert_eq!(cpu.memory[0xFF05], tima_before + 1, "DIV reset should cause TIMA tick");
}

use rustboy::cpu::CPU;

#[test]
fn strict_ppu_transitions_at_dmg_dot_boundaries() {
    let mut cpu = CPU::new();

    cpu.write_byte(0xFF40, 0x91);
    cpu.tick_timer_4t();
    assert_eq!(cpu.ppu_scanline_dot, 4);
    assert_eq!(cpu.memory[0xFF41] & 0x03, 2);

    for _ in 0..19 {
        cpu.tick_timer_4t();
    }
    assert_eq!(cpu.ppu_scanline_dot, 80);
    assert_eq!(cpu.memory[0xFF41] & 0x03, 3);

    for _ in 0..43 {
        cpu.tick_timer_4t();
    }
    assert_eq!(cpu.ppu_scanline_dot, 252);
    assert_eq!(cpu.memory[0xFF41] & 0x03, 0);

    // The first line after LCD enable is four dots shorter.
    for _ in 0..50 {
        cpu.tick_timer_4t();
    }
    assert_eq!(cpu.ppu_scanline_dot, 0);
    assert_eq!(cpu.memory[0xFF44], 1);
    assert_eq!(cpu.memory[0xFF41] & 0x03, 2);
}

#[test]
fn mode3_register_log_reports_overflow_without_allocating() {
    let mut cpu = CPU::new();
    cpu.memory[0xFF41] = (cpu.memory[0xFF41] & !0x03) | 3;
    cpu.ppu_scanline_dot = 100;

    for value in 0..35u8 {
        cpu.write_byte(0xFF42, value);
    }

    assert_eq!(cpu.ppu_reg_log_len, 32);
    assert_eq!(cpu.ppu_reg_log_dropped, 3);
    assert!(cpu.get_debug_state().contains("PPU log:32/32 dropped:3"));
}

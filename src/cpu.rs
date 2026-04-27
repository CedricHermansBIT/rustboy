use std::sync::atomic::{AtomicBool, Ordering};

#[cfg(target_arch = "wasm32")]
use web_sys::console;

use crate::apu::APU;
use crate::debug_tracer::InstructionTracer;

#[cfg(target_arch = "wasm32")]
macro_rules! console_log {
    ($($t:tt)*) => { console::log_1(&format!($($t)*).into()) }
}

#[cfg(not(target_arch = "wasm32"))]
macro_rules! console_log {
    ($($t:tt)*) => { }
}

#[derive(Clone, Debug)]
pub enum Breakpoint {
    Pc(u16),
    Reg(String, u16),
    Mem(u16, u8),
    Opcode(u8),
    CbOpcode(u8),
}

pub struct CPU {
    reg_a: u8,
    reg_b: u8,
    reg_c: u8,
    reg_d: u8,
    reg_e: u8,
    reg_h: u8,
    reg_l: u8,
    reg_f: u8,
    stackpointer: u16,
    pub program_counter: u16,
    pub interrupt_master_enable: bool,
    boot_mem: [u8; 0x10000],
    boot_rom_len: usize,
    pub booting: bool,
    pub memory: [u8; 0x10000],
    pub frame_buffer:[u32; 160 * 144],
    rom: Vec<u8>,
    ram: [[u8; 0x2000]; 16],
    pub halt: bool,
    pub cycles: u32,
    pub total_cycles: u64,
    pub is_paused: AtomicBool,
    pub sys_counter: u16,
    prev_timer_bit: bool,
    consolelog: bool,
    pub show_vram: bool,
    pub keys: [bool; 256],
    ppu_cycles: u32,
    ppu_mcycle_countdown: u32,
    scanline: u8,
    pub go_next: AtomicBool,
    cartridge_type: u8,
    rombank: u16,
    rambank: u8,
    mbc_rom_mode: u8,
    mbc_ram_enable: bool,
    mbc1_bank2: u8,
    rom_bank_mask: u16,
    ram_bank_mask: usize,
    rtc_registers:[u8; 5],
    ei_pending: bool,
    pub apu: APU,
    pub gbc_palettes: [[[u8; 4]; 4]; 3],
    pub color_mode: u8,
    pub speed_multiplier: u32,
    pub breakpoints: Vec<Breakpoint>,
    pub tracing: bool,
    pub trace_buffer: Vec<String>,
    pub instruction_tracer: InstructionTracer,
    pub oam_dma_active: bool,
    pub oam_dma_remaining: u32,
    pub oam_dma_delay: u8,
    oam_dma_source: u16,
    oam_dma_index: u8,
    halt_bug: bool,
    prev_stat_line: bool,
    timer_ticks_this_instr: u32,
    pub serial_output: Vec<u8>,
    pub tima_reload_delay: u8,
    pub tima_reloaded_this_cycle: bool,
    pub is_lcd_turning_on: bool,

    // CGB specific fields
    pub is_cgb: bool,
    pub double_speed: bool,
    pub key1: u8,
    pub vram_bank: u8,
    pub wram_bank: u8,
    pub cgb_wram: [[u8; 0x1000]; 8],
    pub cgb_vram: [[u8; 0x2000]; 2],
    pub bgpi: u8,
    pub cgb_bg_palettes:[u8; 64],
    pub obpi: u8,
    pub cgb_obj_palettes:[u8; 64],
    pub hdma_src: u16,
    pub hdma_dst: u16,
    pub hdma_len: u8,
    pub hdma_active: bool,
    pub ppu_t_cycle_accum: u32,
    pub dma_pause_mcycles: u32,
    pub window_line_counter: u8,
}

impl CPU {
    pub fn get_reg_a(&self) -> u8 { self.reg_a }
    pub fn get_reg_b(&self) -> u8 { self.reg_b }
    pub fn get_reg_c(&self) -> u8 { self.reg_c }
    pub fn get_reg_d(&self) -> u8 { self.reg_d }
    pub fn get_reg_e(&self) -> u8 { self.reg_e }
    pub fn get_reg_h(&self) -> u8 { self.reg_h }
    pub fn get_reg_l(&self) -> u8 { self.reg_l }
    pub fn get_reg_f(&self) -> u8 { self.reg_f }
    pub fn get_sp(&self) -> u16 { self.stackpointer }

    pub fn new() -> CPU {
        CPU {
            reg_a: 0,
            reg_b: 0,
            reg_c: 0,
            reg_d: 0,
            reg_e: 0,
            reg_h: 0,
            reg_l: 0,
            reg_f: 0,
            stackpointer: 0,
            program_counter: 0,
            interrupt_master_enable: false,
            boot_mem:[0; 0x10000],
            boot_rom_len: 0,
            booting: true,
            memory: [0; 0x10000],
            frame_buffer: [0; 160*144],
            rom: Vec::new(),
            ram: [[0; 0x2000]; 16],
            halt: false,
            cycles: 0,
            total_cycles: 0,
            is_paused: AtomicBool::new(false),
            sys_counter: 0,
            prev_timer_bit: false,
            consolelog: false,
            show_vram: false,
            keys:[false; 256],
            ppu_cycles: 0,
            ppu_mcycle_countdown: 0,
            scanline: 0,
            go_next: AtomicBool::new(false),
            cartridge_type: 0,
            rombank: 1,
            rambank: 0,
            mbc_rom_mode: 0,
            mbc_ram_enable: false,
            mbc1_bank2: 0,
            rom_bank_mask: 1,
            ram_bank_mask: 0,
            rtc_registers:[0; 5],
            ei_pending: false,
            apu: APU::new(),
            gbc_palettes: crate::ppu::DEFAULT_GBC_PALETTES,
            color_mode: 0,
            speed_multiplier: 1,
            breakpoints: Vec::new(),
            tracing: false,
            trace_buffer: Vec::new(),
            instruction_tracer: InstructionTracer::new(),
            oam_dma_active: false,
            oam_dma_remaining: 0,
            oam_dma_delay: 0,
            oam_dma_source: 0,
            oam_dma_index: 0,
            halt_bug: false,
            prev_stat_line: false,
            timer_ticks_this_instr: 0,
            serial_output: Vec::new(),
            tima_reload_delay: 0,
            tima_reloaded_this_cycle: false,
            is_lcd_turning_on: false,
            is_cgb: false,
            double_speed: false,
            key1: 0,
            vram_bank: 0,
            wram_bank: 1,
            cgb_wram: [[0; 0x1000]; 8],
            cgb_vram: [[0; 0x2000]; 2],
            bgpi: 0,
            cgb_bg_palettes: [0; 64],
            obpi: 0,
            cgb_obj_palettes: [0; 64],
            hdma_src: 0,
            hdma_dst: 0,
            hdma_len: 0xFF,
            hdma_active: false,
            ppu_t_cycle_accum: 0,
            dma_pause_mcycles: 0,
            window_line_counter: 0,
        }
    }

    pub fn check_boot_finish(&mut self) {
        if self.booting && self.boot_rom_len <= 0x100 && self.program_counter == 0x100 {
            self.booting = false;
            if self.is_cgb {
                self.reg_a = 0x11;
            }
        }
    }

    pub fn toggle_consolelog(&mut self) {
        self.consolelog = !self.consolelog;
    }

    pub fn toggle_trace(&mut self) {
        self.tracing = !self.tracing;
        if self.tracing {
            console_log!(" Trace recording started");
        } else {
            console_log!("⏹ Trace recording stopped ({} lines)", self.trace_buffer.len());
        }
    }

    pub fn is_tracing(&self) -> bool {
        self.tracing
    }

    pub fn get_trace(&self) -> String {
        self.trace_buffer.join("\n")
    }

    pub fn clear_trace(&mut self) {
        let _count = self.trace_buffer.len();
        self.trace_buffer.clear();
        console_log!("Trace cleared ({} lines)", _count);
    }

    pub fn set_instruction_tracing(&mut self, enabled: bool) {
        self.instruction_tracer.set_enabled(enabled);
        if enabled {
            console_log!(" Instruction-level tracing enabled");
        } else {
            console_log!("⏹ Instruction-level tracing disabled ({} traces)", self.instruction_tracer.traces.len());
        }
    }

    pub fn get_last_traces(&self, n: usize) -> Vec<String> {
        self.instruction_tracer.get_last_n_traces(n)
    }

    pub fn export_instruction_traces(&self) -> String {
        self.instruction_tracer.export_as_string()
    }

    pub fn export_instruction_traces_csv(&self) -> String {
        self.instruction_tracer.export_as_csv()
    }

    pub fn get_trace_statistics(&self) -> String {
        self.instruction_tracer.get_statistics()
    }

    pub fn trace_count(&self) -> usize {
        self.instruction_tracer.traces.len()
    }

    fn capture_instruction_trace(&mut self, opcode: u8, is_cb: bool) {
        if !self.instruction_tracer.enabled {
            return;
        }

        let mnemonic = if is_cb {
            self.get_extra_opcode_name(opcode).to_string()
        } else {
            self.get_opcode_name(opcode).to_string()
        };

        let pc = self.program_counter;
        let regs =[
            self.get_reg_a(), self.get_reg_b(), self.get_reg_c(), self.get_reg_d(),
            self.get_reg_e(), self.get_reg_h(), self.get_reg_l(), self.get_reg_f(),
        ];
        let sp = self.get_sp();
        let m_cycles = self.cycles;
        let ime = self.interrupt_master_enable;

        self.instruction_tracer.capture_instruction_final(
            self.instruction_tracer.instr_count,
            self.instruction_tracer.prev_cycles,
            pc, opcode, is_cb, &mnemonic, regs, sp, m_cycles, ime,
        );

        self.instruction_tracer.instr_count += 1;
        self.instruction_tracer.prev_cycles += m_cycles as u64;
    }

    fn opcode_size(opcode: u8) -> u8 {
        match opcode {
            0x00 | 0x02 | 0x03..=0x05 | 0x07 | 0x09..=0x0B | 0x0C..=0x0D | 0x0F |
            0x12..=0x15 | 0x17 | 0x19..=0x1D | 0x1F |
            0x22..=0x25 | 0x27 | 0x29..=0x2D | 0x2F |
            0x32..=0x35 | 0x37 | 0x39..=0x3D | 0x3F |
            0x40..=0x7F | 0x80..=0xBF |
            0xC0 | 0xC1 | 0xC5 | 0xC7 | 0xC8 | 0xC9 | 0xCF |
            0xD0 | 0xD1 | 0xD5 | 0xD7 | 0xD8 | 0xD9 | 0xDF |
            0xE1 | 0xE2 | 0xE5 | 0xE7 | 0xE9 | 0xEF |
            0xF1 | 0xF2 | 0xF3 | 0xF5 | 0xF7 | 0xF9 | 0xFB | 0xFF => 1,
            0x06 | 0x0E | 0x10 | 0x16 | 0x18 | 0x1E |
            0x20 | 0x26 | 0x28 | 0x2E | 0x30 | 0x36 | 0x38 | 0x3E |
            0xC6 | 0xCE | 0xD6 | 0xDE | 0xE0 | 0xE6 | 0xE8 | 0xEE |
            0xF0 | 0xF6 | 0xF8 | 0xFE => 2,
            0xCB => 2,
            0x01 | 0x08 | 0x11 | 0x21 | 0x31 |
            0xC2 | 0xC3 | 0xC4 | 0xCA | 0xCC | 0xCD |
            0xD2 | 0xD4 | 0xDA | 0xDC | 0xEA | 0xFA => 3,
            _ => 1,
        }
    }

    fn format_trace_line(&self) -> String {
        let pc = self.program_counter;
        let opcode = self.peek_byte(pc as usize);
        let size = Self::opcode_size(opcode);

        let flags = format!("{}{}{}{}",
                            if self.reg_f & 0x80 != 0 { 'Z' } else { '-' },
                            if self.reg_f & 0x40 != 0 { 'N' } else { '-' },
                            if self.reg_f & 0x20 != 0 { 'H' } else { '-' },
                            if self.reg_f & 0x10 != 0 { 'C' } else { '-' },
        );

        let bank: u8 = if pc < 0x4000 { 0x00 } else if pc < 0x8000 { self.rombank as u8 } else { 0x00 };

        let mut hex_bytes = String::new();
        for i in 0..size {
            let b = self.peek_byte(pc.wrapping_add(i as u16) as usize);
            if i > 0 { hex_bytes.push(' '); }
            hex_bytes.push_str(&format!("{:02x}", b));
        }

        let imm8 = if size >= 2 { self.peek_byte(pc.wrapping_add(1) as usize) } else { 0 };
        let imm16 = if size >= 3 {
            let lo = self.peek_byte(pc.wrapping_add(1) as usize) as u16;
            let hi = self.peek_byte(pc.wrapping_add(2) as usize) as u16;
            (hi << 8) | lo
        } else { 0 };

        let name = if opcode == 0xCB {
            let cb_op = self.peek_byte(pc.wrapping_add(1) as usize);
            Self::cb_opcode_name_static(cb_op).to_string()
        } else {
            Self::resolve_operands(opcode, imm8, imm16)
        };

        format!(
            "A:{:02X} F:{} BC:{:04x} DE:{:04x} HL:{:04x} SP:{:04x} PC:{:04x} |[{:02x}]0x{:04x}: {:10}{}",
            self.reg_a, flags, self.bc(), self.de(), self.hl(),
            self.stackpointer, pc, bank, pc, hex_bytes, name,
        )
    }

    fn resolve_operands(opcode: u8, imm8: u8, imm16: u16) -> String {
        let rel = imm8 as i8;
        match opcode {
            0x06 => format!("ld b,{}", imm8), 0x0e => format!("ld c,{}", imm8),
            0x16 => format!("ld d,{}", imm8), 0x1e => format!("ld e,{}", imm8),
            0x26 => format!("ld h,{}", imm8), 0x2e => format!("ld l,{}", imm8),
            0x36 => format!("ld [hl],{}", imm8), 0x3e => format!("ld a,{}", imm8),
            0xc6 => format!("add a,{}", imm8), 0xce => format!("adc a,{}", imm8),
            0xd6 => format!("sub a,{}", imm8), 0xde => format!("sbc a,{}", imm8),
            0xe6 => format!("and {}", imm8), 0xee => format!("xor {}", imm8),
            0xf6 => format!("or {}", imm8), 0xfe => format!("cp a,{}", imm8),
            0xe0 => format!("ldh [${:02x}],a", 0xFF00u16 | imm8 as u16),
            0xf0 => format!("ldh a,[${:02x}]", 0xFF00u16 | imm8 as u16),
            0x18 => format!("jr {}{}", if rel >= 0 { "+" } else { "" }, rel),
            0x20 => format!("jr nz,{}{}", if rel >= 0 { "+" } else { "" }, rel),
            0x28 => format!("jr z,{}{}", if rel >= 0 { "+" } else { "" }, rel),
            0x30 => format!("jr nc,{}{}", if rel >= 0 { "+" } else { "" }, rel),
            0x38 => format!("jr c,{}{}", if rel >= 0 { "+" } else { "" }, rel),
            0xe8 => format!("add sp,{}{}", if rel >= 0 { "+" } else { "" }, rel),
            0xf8 => format!("ld hl,sp{}{}", if rel >= 0 { "+" } else { "" }, rel),
            0x10 => "stop".to_string(),
            0x01 => format!("ld bc,{}", imm16), 0x11 => format!("ld de,{}", imm16),
            0x21 => format!("ld hl,{}", imm16), 0x31 => format!("ld sp,{}", imm16),
            0x08 => format!("ld [${:04x}],sp", imm16),
            0xc2 => format!("jp nz,${:04x}", imm16), 0xc3 => format!("jp ${:04x}", imm16),
            0xc4 => format!("call nz,${:04x}", imm16), 0xca => format!("jp z,${:04x}", imm16),
            0xcc => format!("call z,${:04x}", imm16), 0xcd => format!("call ${:04x}", imm16),
            0xd2 => format!("jp nc,${:04x}", imm16), 0xd4 => format!("call nc,${:04x}", imm16),
            0xda => format!("jp c,${:04x}", imm16), 0xdc => format!("call c,${:04x}", imm16),
            0xea => format!("ld[${:04x}],a", imm16), 0xfa => format!("ld a,[${:04x}]", imm16),
            _ => Self::opcode_name_static(opcode).to_string(),
        }
    }

    fn opcode_name_static(opcode: u8) -> &'static str {
        match opcode {
            0x00 => "nop", 0x01 => "ld bc,d16", 0x02 => "ld [bc],a",
            0x03 => "inc bc", 0x04 => "inc b", 0x05 => "dec b",
            0x06 => "ld b,d8", 0x07 => "rlca", 0x08 => "ld [a16],sp",
            0x09 => "add hl,bc", 0x0a => "ld a,[bc]", 0x0b => "dec bc",
            0x0c => "inc c", 0x0d => "dec c", 0x0e => "ld c,d8", 0x0f => "rrca",
            0x10 => "stop", 0x11 => "ld de,d16", 0x12 => "ld [de],a",
            0x13 => "inc de", 0x14 => "inc d", 0x15 => "dec d",
            0x16 => "ld d,d8", 0x17 => "rla", 0x18 => "jr r8",
            0x19 => "add hl,de", 0x1a => "ld a,[de]", 0x1b => "dec de",
            0x1c => "inc e", 0x1d => "dec e", 0x1e => "ld e,d8", 0x1f => "rra",
            0x20 => "jr nz,r8", 0x21 => "ld hl,d16", 0x22 => "ld [hl+],a",
            0x23 => "inc hl", 0x24 => "inc h", 0x25 => "dec h",
            0x26 => "ld h,d8", 0x27 => "daa", 0x28 => "jr z,r8",
            0x29 => "add hl,hl", 0x2a => "ld a,[hl+]", 0x2b => "dec hl",
            0x2c => "inc l", 0x2d => "dec l", 0x2e => "ld l,d8", 0x2f => "cpl",
            0x30 => "jr nc,r8", 0x31 => "ld sp,d16", 0x32 => "ld[hl-],a",
            0x33 => "inc sp", 0x34 => "inc [hl]", 0x35 => "dec [hl]",
            0x36 => "ld[hl],d8", 0x37 => "scf", 0x38 => "jr c,r8",
            0x39 => "add hl,sp", 0x3a => "ld a,[hl-]", 0x3b => "dec sp",
            0x3c => "inc a", 0x3d => "dec a", 0x3e => "ld a,d8", 0x3f => "ccf",
            0x40 => "ld b,b", 0x41 => "ld b,c", 0x42 => "ld b,d", 0x43 => "ld b,e",
            0x44 => "ld b,h", 0x45 => "ld b,l", 0x46 => "ld b,[hl]", 0x47 => "ld b,a",
            0x48 => "ld c,b", 0x49 => "ld c,c", 0x4a => "ld c,d", 0x4b => "ld c,e",
            0x4c => "ld c,h", 0x4d => "ld c,l", 0x4e => "ld c,[hl]", 0x4f => "ld c,a",
            0x50 => "ld d,b", 0x51 => "ld d,c", 0x52 => "ld d,d", 0x53 => "ld d,e",
            0x54 => "ld d,h", 0x55 => "ld d,l", 0x56 => "ld d,[hl]", 0x57 => "ld d,a",
            0x58 => "ld e,b", 0x59 => "ld e,c", 0x5a => "ld e,d", 0x5b => "ld e,e",
            0x5c => "ld e,h", 0x5d => "ld e,l", 0x5e => "ld e,[hl]", 0x5f => "ld e,a",
            0x60 => "ld h,b", 0x61 => "ld h,c", 0x62 => "ld h,d", 0x63 => "ld h,e",
            0x64 => "ld h,h", 0x65 => "ld h,l", 0x66 => "ld h,[hl]", 0x67 => "ld h,a",
            0x68 => "ld l,b", 0x69 => "ld l,c", 0x6a => "ld l,d", 0x6b => "ld l,e",
            0x6c => "ld l,h", 0x6d => "ld l,l", 0x6e => "ld l,[hl]", 0x6f => "ld l,a",
            0x70 => "ld [hl],b", 0x71 => "ld [hl],c", 0x72 => "ld [hl],d", 0x73 => "ld[hl],e",
            0x74 => "ld [hl],h", 0x75 => "ld [hl],l", 0x76 => "halt", 0x77 => "ld[hl],a",
            0x78 => "ld a,b", 0x79 => "ld a,c", 0x7a => "ld a,d", 0x7b => "ld a,e",
            0x7c => "ld a,h", 0x7d => "ld a,l", 0x7e => "ld a,[hl]", 0x7f => "ld a,a",
            0x80 => "add a,b", 0x81 => "add a,c", 0x82 => "add a,d", 0x83 => "add a,e",
            0x84 => "add a,h", 0x85 => "add a,l", 0x86 => "add a,[hl]", 0x87 => "add a,a",
            0x88 => "adc a,b", 0x89 => "adc a,c", 0x8a => "adc a,d", 0x8b => "adc a,e",
            0x8c => "adc a,h", 0x8d => "adc a,l", 0x8e => "adc a,[hl]", 0x8f => "adc a,a",
            0x90 => "sub b", 0x91 => "sub c", 0x92 => "sub d", 0x93 => "sub e",
            0x94 => "sub h", 0x95 => "sub l", 0x96 => "sub [hl]", 0x97 => "sub a",
            0x98 => "sbc a,b", 0x99 => "sbc a,c", 0x9a => "sbc a,d", 0x9b => "sbc a,e",
            0x9c => "sbc a,h", 0x9d => "sbc a,l", 0x9e => "sbc a,[hl]", 0x9f => "sbc a,a",
            0xa0 => "and b", 0xa1 => "and c", 0xa2 => "and d", 0xa3 => "and e",
            0xa4 => "and h", 0xa5 => "and l", 0xa6 => "and[hl]", 0xa7 => "and a",
            0xa8 => "xor b", 0xa9 => "xor c", 0xaa => "xor d", 0xab => "xor e",
            0xac => "xor h", 0xad => "xor l", 0xae => "xor [hl]", 0xaf => "xor a",
            0xb0 => "or b", 0xb1 => "or c", 0xb2 => "or d", 0xb3 => "or e",
            0xb4 => "or h", 0xb5 => "or l", 0xb6 => "or [hl]", 0xb7 => "or a",
            0xb8 => "cp b", 0xb9 => "cp c", 0xba => "cp d", 0xbb => "cp e",
            0xbc => "cp h", 0xbd => "cp l", 0xbe => "cp [hl]", 0xbf => "cp a",
            0xc0 => "ret nz", 0xc1 => "pop bc", 0xc2 => "jp nz,a16", 0xc3 => "jp a16",
            0xc4 => "call nz,a16", 0xc5 => "push bc", 0xc6 => "add a,d8", 0xc7 => "rst 00h",
            0xc8 => "ret z", 0xc9 => "ret", 0xca => "jp z,a16", 0xcb => "CB",
            0xcc => "call z,a16", 0xcd => "call a16", 0xce => "adc a,d8", 0xcf => "rst 08h",
            0xd0 => "ret nc", 0xd1 => "pop de", 0xd2 => "jp nc,a16",
            0xd4 => "call nc,a16", 0xd5 => "push de", 0xd6 => "sub d8", 0xd7 => "rst 10h",
            0xd8 => "ret c", 0xd9 => "reti", 0xda => "jp c,a16",
            0xdc => "call c,a16", 0xde => "sbc a,d8", 0xdf => "rst 18h",
            0xe0 => "ldh [a8],a", 0xe1 => "pop hl", 0xe2 => "ld [c],a",
            0xe5 => "push hl", 0xe6 => "and d8", 0xe7 => "rst 20h",
            0xe8 => "add sp,r8", 0xe9 => "jp [hl]", 0xea => "ld [a16],a",
            0xee => "xor d8", 0xef => "rst 28h",
            0xf0 => "ldh a,[a8]", 0xf1 => "pop af", 0xf2 => "ld a,[c]",
            0xf3 => "di", 0xf5 => "push af", 0xf6 => "or d8", 0xf7 => "rst 30h",
            0xf8 => "ld hl,sp+r8", 0xf9 => "ld sp,hl", 0xfa => "ld a,[a16]",
            0xfb => "ei", 0xfe => "cp d8", 0xff => "rst 38h",
            _ => "UNKNOWN",
        }
    }

    fn cb_opcode_name_static(opcode: u8) -> &'static str {
        match opcode {
            0x00 => "rlc b", 0x01 => "rlc c", 0x02 => "rlc d", 0x03 => "rlc e",
            0x04 => "rlc h", 0x05 => "rlc l", 0x06 => "rlc [hl]", 0x07 => "rlc a",
            0x08 => "rrc b", 0x09 => "rrc c", 0x0a => "rrc d", 0x0b => "rrc e",
            0x0c => "rrc h", 0x0d => "rrc l", 0x0e => "rrc [hl]", 0x0f => "rrc a",
            0x10 => "rl b", 0x11 => "rl c", 0x12 => "rl d", 0x13 => "rl e",
            0x14 => "rl h", 0x15 => "rl l", 0x16 => "rl [hl]", 0x17 => "rl a",
            0x18 => "rr b", 0x19 => "rr c", 0x1a => "rr d", 0x1b => "rr e",
            0x1c => "rr h", 0x1d => "rr l", 0x1e => "rr [hl]", 0x1f => "rr a",
            0x20 => "sla b", 0x21 => "sla c", 0x22 => "sla d", 0x23 => "sla e",
            0x24 => "sla h", 0x25 => "sla l", 0x26 => "sla [hl]", 0x27 => "sla a",
            0x28 => "sra b", 0x29 => "sra c", 0x2a => "sra d", 0x2b => "sra e",
            0x2c => "sra h", 0x2d => "sra l", 0x2e => "sra [hl]", 0x2f => "sra a",
            0x30 => "swap b", 0x31 => "swap c", 0x32 => "swap d", 0x33 => "swap e",
            0x34 => "swap h", 0x35 => "swap l", 0x36 => "swap [hl]", 0x37 => "swap a",
            0x38 => "srl b", 0x39 => "srl c", 0x3a => "srl d", 0x3b => "srl e",
            0x3c => "srl h", 0x3d => "srl l", 0x3e => "srl [hl]", 0x3f => "srl a",
            0x40 => "bit 0,b", 0x41 => "bit 0,c", 0x42 => "bit 0,d", 0x43 => "bit 0,e",
            0x44 => "bit 0,h", 0x45 => "bit 0,l", 0x46 => "bit 0,[hl]", 0x47 => "bit 0,a",
            0x48 => "bit 1,b", 0x49 => "bit 1,c", 0x4a => "bit 1,d", 0x4b => "bit 1,e",
            0x4c => "bit 1,h", 0x4d => "bit 1,l", 0x4e => "bit 1,[hl]", 0x4f => "bit 1,a",
            0x50 => "bit 2,b", 0x51 => "bit 2,c", 0x52 => "bit 2,d", 0x53 => "bit 2,e",
            0x54 => "bit 2,h", 0x55 => "bit 2,l", 0x56 => "bit 2,[hl]", 0x57 => "bit 2,a",
            0x58 => "bit 3,b", 0x59 => "bit 3,c", 0x5a => "bit 3,d", 0x5b => "bit 3,e",
            0x5c => "bit 3,h", 0x5d => "bit 3,l", 0x5e => "bit 3,[hl]", 0x5f => "bit 3,a",
            0x60 => "bit 4,b", 0x61 => "bit 4,c", 0x62 => "bit 4,d", 0x63 => "bit 4,e",
            0x64 => "bit 4,h", 0x65 => "bit 4,l", 0x66 => "bit 4,[hl]", 0x67 => "bit 4,a",
            0x68 => "bit 5,b", 0x69 => "bit 5,c", 0x6a => "bit 5,d", 0x6b => "bit 5,e",
            0x6c => "bit 5,h", 0x6d => "bit 5,l", 0x6e => "bit 5,[hl]", 0x6f => "bit 5,a",
            0x70 => "bit 6,b", 0x71 => "bit 6,c", 0x72 => "bit 6,d", 0x73 => "bit 6,e",
            0x74 => "bit 6,h", 0x75 => "bit 6,l", 0x76 => "bit 6,[hl]", 0x77 => "bit 6,a",
            0x78 => "bit 7,b", 0x79 => "bit 7,c", 0x7a => "bit 7,d", 0x7b => "bit 7,e",
            0x7c => "bit 7,h", 0x7d => "bit 7,l", 0x7e => "bit 7,[hl]", 0x7f => "bit 7,a",
            0x80 => "res 0,b", 0x81 => "res 0,c", 0x82 => "res 0,d", 0x83 => "res 0,e",
            0x84 => "res 0,h", 0x85 => "res 0,l", 0x86 => "res 0,[hl]", 0x87 => "res 0,a",
            0x88 => "res 1,b", 0x89 => "res 1,c", 0x8a => "res 1,d", 0x8b => "res 1,e",
            0x8c => "res 1,h", 0x8d => "res 1,l", 0x8e => "res 1,[hl]", 0x8f => "res 1,a",
            0x90 => "res 2,b", 0x91 => "res 2,c", 0x92 => "res 2,d", 0x93 => "res 2,e",
            0x94 => "res 2,h", 0x95 => "res 2,l", 0x96 => "res 2,[hl]", 0x97 => "res 2,a",
            0x98 => "res 3,b", 0x99 => "res 3,c", 0x9a => "res 3,d", 0x9b => "res 3,e",
            0x9c => "res 3,h", 0x9d => "res 3,l", 0x9e => "res 3,[hl]", 0x9f => "res 3,a",
            0xa0 => "res 4,b", 0xa1 => "res 4,c", 0xa2 => "res 4,d", 0xa3 => "res 4,e",
            0xa4 => "res 4,h", 0xa5 => "res 4,l", 0xa6 => "res 4,[hl]", 0xa7 => "res 4,a",
            0xa8 => "res 5,b", 0xa9 => "res 5,c", 0xaa => "res 5,d", 0xab => "res 5,e",
            0xac => "res 5,h", 0xad => "res 5,l", 0xae => "res 5,[hl]", 0xaf => "res 5,a",
            0xb0 => "res 6,b", 0xb1 => "res 6,c", 0xb2 => "res 6,d", 0xb3 => "res 6,e",
            0xb4 => "res 6,h", 0xb5 => "res 6,l", 0xb6 => "res 6,[hl]", 0xb7 => "res 6,a",
            0xb8 => "res 7,b", 0xb9 => "res 7,c", 0xba => "res 7,d", 0xbb => "res 7,e",
            0xbc => "res 7,h", 0xbd => "res 7,l", 0xbe => "res 7,[hl]", 0xbf => "res 7,a",
            0xc0 => "set 0,b", 0xc1 => "set 0,c", 0xc2 => "set 0,d", 0xc3 => "set 0,e",
            0xc4 => "set 0,h", 0xc5 => "set 0,l", 0xc6 => "set 0,[hl]", 0xc7 => "set 0,a",
            0xc8 => "set 1,b", 0xc9 => "set 1,c", 0xca => "set 1,d", 0xcb => "set 1,e",
            0xcc => "set 1,h", 0xcd => "set 1,l", 0xce => "set 1,[hl]", 0xcf => "set 1,a",
            0xd0 => "set 2,b", 0xd1 => "set 2,c", 0xd2 => "set 2,d", 0xd3 => "set 2,e",
            0xd4 => "set 2,h", 0xd5 => "set 2,l", 0xd6 => "set 2,[hl]", 0xd7 => "set 2,a",
            0xd8 => "set 3,b", 0xd9 => "set 3,c", 0xda => "set 3,d", 0xdb => "set 3,e",
            0xdc => "set 3,h", 0xdd => "set 3,l", 0xde => "set 3,[hl]", 0xdf => "set 3,a",
            0xe0 => "set 4,b", 0xe1 => "set 4,c", 0xe2 => "set 4,d", 0xe3 => "set 4,e",
            0xe4 => "set 4,h", 0xe5 => "set 4,l", 0xe6 => "set 4,[hl]", 0xe7 => "set 4,a",
            0xe8 => "set 5,b", 0xe9 => "set 5,c", 0xea => "set 5,d", 0xeb => "set 5,e",
            0xec => "set 5,h", 0xed => "set 5,l", 0xee => "set 5,[hl]", 0xef => "set 5,a",
            0xf0 => "set 6,b", 0xf1 => "set 6,c", 0xf2 => "set 6,d", 0xf3 => "set 6,e",
            0xf4 => "set 6,h", 0xf5 => "set 6,l", 0xf6 => "set 6,[hl]", 0xf7 => "set 6,a",
            0xf8 => "set 7,b", 0xf9 => "set 7,c", 0xfa => "set 7,d", 0xfb => "set 7,e",
            0xfc => "set 7,h", 0xfd => "set 7,l", 0xfe => "set 7,[hl]", 0xff => "set 7,a",
        }
    }

    pub fn set_next(&mut self) {
        self.go_next.store(true, Ordering::Relaxed);
    }

    pub fn toggle_pause(&mut self) {
        let current = self.is_paused.load(Ordering::Relaxed);
        self.is_paused.store(!current, Ordering::Relaxed);
    }

    pub fn toggle_showvram(&mut self) {
        self.show_vram = !self.show_vram;
    }

    pub fn toggle_color_mode(&mut self) {
        self.color_mode = if self.color_mode == 0 { 1 } else { 0 };
        console_log!("Color mode: {}", if self.color_mode == 0 { "GBC Color" } else { "DMG Green" });
    }

    pub fn cycle_speed(&mut self) {
        self.speed_multiplier = match self.speed_multiplier {
            1 => 2,
            2 => 4,
            4 => 8,
            _ => 1,
        };
    }

    pub fn _set_speed(&mut self, multiplier: u32) {
        self.speed_multiplier = multiplier.max(1).min(8);
    }

    pub fn get_speed(&self) -> u32 {
        self.speed_multiplier
    }

    pub fn add_breakpoint_pc(&mut self, addr: u16) {
        self.breakpoints.push(Breakpoint::Pc(addr));
        console_log!("Breakpoint added: PC == {:04X}  [#{}]", addr, self.breakpoints.len() - 1);
    }

    pub fn add_breakpoint_reg(&mut self, reg: &str, value: u16) {
        self.breakpoints.push(Breakpoint::Reg(reg.to_ascii_lowercase(), value));
        console_log!("Breakpoint added: {} == {:04X}[#{}]", reg, value, self.breakpoints.len() - 1);
    }

    pub fn add_breakpoint_mem(&mut self, addr: u16, value: u8) {
        self.breakpoints.push(Breakpoint::Mem(addr, value));
        console_log!("Breakpoint added:[${:04X}] == {:02X}  [#{}]", addr, value, self.breakpoints.len() - 1);
    }

    pub fn remove_breakpoint(&mut self, index: usize) {
        if index < self.breakpoints.len() {
            let _removed = self.breakpoints.remove(index);
            console_log!("Removed breakpoint #{}: {:?}", index, _removed);
        } else {
            console_log!("Invalid breakpoint index: {}", index);
        }
    }

    pub fn add_breakpoint_opcode(&mut self, opcode: u8) {
        self.breakpoints.push(Breakpoint::Opcode(opcode));
    }

    pub fn add_breakpoint_cb_opcode(&mut self, opcode: u8) {
        self.breakpoints.push(Breakpoint::CbOpcode(opcode));
    }

    pub fn clear_breakpoints(&mut self) {
        let _count = self.breakpoints.len();
        self.breakpoints.clear();
        console_log!("Cleared {} breakpoint(s)", _count);
    }

    pub fn list_breakpoints(&self) -> String {
        if self.breakpoints.is_empty() {
            return "No breakpoints set".to_string();
        }
        let mut out = String::new();
        for (i, bp) in self.breakpoints.iter().enumerate() {
            let _desc = match bp {
                Breakpoint::Pc(addr) => format!("#{}: PC == ${:04X}", i, addr),
                Breakpoint::Reg(reg, val) => format!("#{}: {} == ${:04X}", i, reg.to_uppercase(), val),
                Breakpoint::Mem(addr, val) => format!("#{}: [${:04X}] == ${:02X}", i, addr, val),
                Breakpoint::Opcode(op) => format!("#{}: Opcode == ${:02X}", i, op),
                Breakpoint::CbOpcode(op) => format!("#{}: CbOpcode == ${:02X}", i, op),
            };
            if i > 0 { out.push('\n'); }
            out.push_str(&_desc);
        }
        out
    }

    fn get_reg_value(&self, name: &str) -> Option<u16> {
        match name {
            "a" => Some(self.reg_a as u16),
            "b" => Some(self.reg_b as u16),
            "c" => Some(self.reg_c as u16),
            "d" => Some(self.reg_d as u16),
            "e" => Some(self.reg_e as u16),
            "h" => Some(self.reg_h as u16),
            "l" => Some(self.reg_l as u16),
            "f" => Some(self.reg_f as u16),
            "af" => Some(self.af()),
            "bc" => Some(self.bc()),
            "de" => Some(self.de()),
            "hl" => Some(self.hl()),
            "sp" => Some(self.stackpointer),
            "pc" => Some(self.program_counter),
            _ => None,
        }
    }

    pub fn check_breakpoints(&mut self) -> bool {
        if self.breakpoints.is_empty() {
            return false;
        }
        for bp in &self.breakpoints {
            let hit = match bp {
                Breakpoint::Pc(addr) => self.program_counter == *addr,
                Breakpoint::Reg(reg, val) => {
                    self.get_reg_value(reg).map_or(false, |v| v == *val)
                }
                Breakpoint::Mem(addr, val) => {
                    self.memory[*addr as usize] == *val
                }
                Breakpoint::Opcode(op) => {
                    let current_op = self.peek_byte(self.program_counter as usize);
                    current_op == *op
                }
                Breakpoint::CbOpcode(op) => {
                    let current_op = self.peek_byte(self.program_counter as usize);
                    if current_op == 0xCB {
                        let next_op = self.peek_byte(self.program_counter.wrapping_add(1) as usize);
                        next_op == *op
                    } else {
                        false
                    }
                }
            };
            if hit {
                let desc = match bp {
                    Breakpoint::Pc(addr) => format!("PC == ${:04X}", addr),
                    Breakpoint::Reg(reg, val) => format!("{} == ${:04X}", reg.to_uppercase(), val),
                    Breakpoint::Mem(addr, val) => format!("[${:04X}] == ${:02X}", addr, val),
                    Breakpoint::Opcode(op) => format!("Opcode == ${:02X}", op),
                    Breakpoint::CbOpcode(op) => format!("CbOpcode == ${:02X}", op),
                };
                console_log!(" Breakpoint hit: {}  (PC=${:04X})", desc, self.program_counter);
                self.consolelog = true;
                self.is_paused.store(true, Ordering::Relaxed);
                return true;
            }
        }
        false
    }

    pub fn peek(&self, addr: u16) -> u8 {
        self.peek_byte(addr as usize)
    }

    pub fn peek_slice(&self, start: u16, len: u16) -> String {
        let len = len.min(256);
        let mut out = String::new();
        let mut addr = start;
        let end = start.wrapping_add(len);
        while addr != end {
            out.push_str(&format!("{:04X} | ", addr));
            let row_end = addr.wrapping_add(16).min(end);
            let row_len = row_end.wrapping_sub(addr);
            let row_start = addr;
            for i in 0..16u16 {
                if i < row_len {
                    out.push_str(&format!("{:02X} ", self.peek_byte(addr.wrapping_add(i) as usize)));
                } else {
                    out.push_str("   ");
                }
            }
            out.push_str("| ");
            for i in 0..row_len {
                let b = self.peek_byte(row_start.wrapping_add(i) as usize);
                out.push(if b >= 0x20 && b < 0x7F { b as char } else { '.' });
            }
            out.push('\n');
            addr = row_end;
        }
        out
    }

    pub fn peek_regs(&self) -> String {
        let flags = format!("{}{}{}{}",
                            if self.reg_f & 0x80 != 0 { 'Z' } else { '-' },
                            if self.reg_f & 0x40 != 0 { 'N' } else { '-' },
                            if self.reg_f & 0x20 != 0 { 'H' } else { '-' },
                            if self.reg_f & 0x10 != 0 { 'C' } else { '-' },
        );
        format!(
            "A:{:02X}  F:{:02X} [{}]\nB:{:02X}  C:{:02X}  BC:{:04X}\nD:{:02X}  E:{:02X}  DE:{:04X}\nH:{:02X}  L:{:02X}  HL:{:04X}\nSP:{:04X}  PC:{:04X}\nIME:{}  HALT:{}  ROM:{:03X}  RAM:{:02X}",
            self.reg_a, self.reg_f, flags,
            self.reg_b, self.reg_c, self.bc(),
            self.reg_d, self.reg_e, self.de(),
            self.reg_h, self.reg_l, self.hl(),
            self.stackpointer, self.program_counter,
            self.interrupt_master_enable as u8, self.halt as u8, self.rombank, self.rambank,
        )
    }

    pub fn bootload(&mut self, data: Vec<u8>) {
        self.boot_rom_len = data.len().min(self.boot_mem.len());
        for i in 0..data.len() {
            self.boot_mem[i] = data[i];
        }
    }

    #[inline]
    fn boot_rom_maps_address(&self, address: usize) -> bool {
        if !self.booting {
            return false;
        }

        if self.boot_rom_len <= 0x100 {
            return address < self.boot_rom_len;
        }

        address < 0x100 || (0x200..self.boot_rom_len).contains(&address)
    }

    pub fn load_rom(&mut self, data: Vec<u8>) {
        let copy_len = data.len().min(0x8000);
        for i in 0..copy_len {
            self.memory[i] = data[i];
        }
        self.rom = data;
        let cgb_flag = self.rom.get(0x143).copied().unwrap_or(0);
        self.is_cgb = cgb_flag == 0x80 || cgb_flag == 0xC0;

        self.apu.set_cgb_mode(self.is_cgb);
        self.cartridge_type = self.memory[0x147];
        let num_rom_banks = (self.rom.len() / 0x4000).max(2) as u16;
        self.rom_bank_mask = num_rom_banks.next_power_of_two() - 1;

        let mut ram_size = self.get_ram_size();
        if self.cartridge_type == 0x05 || self.cartridge_type == 0x06 {
            ram_size = 512;
        }
        let num_ram_banks = if ram_size == 0 { 0 } else { (ram_size / 0x2000).max(1) };
        self.ram_bank_mask = if num_ram_banks > 0 { num_ram_banks.next_power_of_two() - 1 } else { 0 };

        console_log!("Cartridge type: {:02X}, ROM size: {} KB, ROM banks: {}, ROM mask: {:03X}, RAM size: {} KB, RAM banks: {}, RAM mask: {:02X}, CGB: {}",
            self.cartridge_type, self.rom.len() / 1024, num_rom_banks, self.rom_bank_mask, ram_size, num_ram_banks, self.ram_bank_mask, self.is_cgb);

        self.gbc_palettes = crate::ppu::gbc_palette_for_rom(&self.rom);
        console_log!("GBC palette assigned for title: '{}'", self.rom_title());
    }

    pub fn has_battery(&self) -> bool {
        matches!(self.cartridge_type, 0x03 | 0x06 | 0x09 | 0x0D | 0x0F | 0x10 | 0x13 | 0x1B | 0x1E)
    }

    pub fn rom_title(&self) -> String {
        let mut title = String::new();
        for i in 0x134..=0x143 {
            let c = self.rom.get(i).copied().unwrap_or(0);
            if c == 0 { break; }
            title.push(c as char);
        }
        title.trim().to_string()
    }

    pub fn export_save_ram(&self) -> Vec<u8> {
        let ram_size = self.get_ram_size();
        let num_banks = (ram_size / 0x2000).max(1);
        let mut data = Vec::with_capacity(num_banks * 0x2000);
        for bank in 0..num_banks {
            data.extend_from_slice(&self.ram[bank]);
        }
        data
    }

    pub fn import_save_ram(&mut self, data: &[u8]) {
        let ram_size = self.get_ram_size();
        let num_banks = (ram_size / 0x2000).max(1);
        for bank in 0..num_banks {
            let start = bank * 0x2000;
            let end = (start + 0x2000).min(data.len());
            if start < data.len() {
                let len = end - start;
                self.ram[bank][..len].copy_from_slice(&data[start..end]);
            }
        }
        console_log!("Loaded save RAM: {} bytes into {} banks", data.len(), num_banks);
    }

    fn get_ram_size(&self) -> usize {
        match self.rom.get(0x149).copied().unwrap_or(0) {
            0x00 => 0,
            0x01 => 0x800,
            0x02 => 0x2000,
            0x03 => 0x8000,
            0x04 => 0x20000,
            0x05 => 0x10000,
            _ => 0x2000,
        }
    }

    fn set_flag_bit(&mut self, bit: u8, value: bool) {
        if value {
            self.reg_f |= 1 << bit;
        } else {
            self.reg_f &= !(1 << bit);
        }
    }

    fn get_flag_bit(&self, bit: u8) -> bool {
        self.reg_f & (1 << bit) != 0
    }

    pub fn write_byte(&mut self, address: usize, data: u8) {
        self.tick_timer_4t();

        if address >= 0xFE00 && address < 0xFEA0 {
            if self.oam_dma_active && self.oam_dma_delay == 0 {
                return;
            }
            if (self.memory[0xFF40] & 0x80) != 0 {
                let mode = self.memory[0xFF41] & 0x03;
                if mode == 2 || mode == 3 {
                    return;
                }
            }
        } else if address >= 0x8000 && address <= 0x9FFF {
            if (self.memory[0xFF40] & 0x80) != 0 {
                let mode = self.memory[0xFF41] & 0x03;
                if mode == 3 {
                    return;
                }
            }
        }

        if self.is_cgb {
            match address {
                0xFF4D => {
                    self.key1 = (self.key1 & 0x80) | (data & 0x01);
                    return;
                }
                0xFF4F => {
                    self.vram_bank = data & 0x01;
                    return;
                }
                0xFF51 => {
                    self.hdma_src = (self.hdma_src & 0x00FF) | ((data as u16) << 8);
                    return;
                }
                0xFF52 => {
                    self.hdma_src = (self.hdma_src & 0xFF00) | (data as u16 & 0xF0);
                    return;
                }
                0xFF53 => {
                    self.hdma_dst = (self.hdma_dst & 0x00FF) | (((data & 0x1F) as u16) << 8);
                    return;
                }
                0xFF54 => {
                    self.hdma_dst = (self.hdma_dst & 0xFF00) | (data as u16 & 0xF0);
                    return;
                }
                0xFF55 => {
                    self.start_hdma(data);
                    return;
                }
                0xFF68 => {
                    self.bgpi = data;
                    return;
                }
                0xFF69 => {
                    let idx = (self.bgpi & 0x3F) as usize;
                    self.cgb_bg_palettes[idx] = data;
                    if self.bgpi & 0x80 != 0 {
                        self.bgpi = (self.bgpi & 0x80) | ((self.bgpi + 1) & 0x3F);
                    }
                    return;
                }
                0xFF6A => {
                    self.obpi = data;
                    return;
                }
                0xFF6B => {
                    let idx = (self.obpi & 0x3F) as usize;
                    self.cgb_obj_palettes[idx] = data;
                    if self.obpi & 0x80 != 0 {
                        self.obpi = (self.obpi & 0x80) | ((self.obpi + 1) & 0x3F);
                    }
                    return;
                }
                0xFF70 => {
                    let mut b = data & 0x07;
                    if b == 0 { b = 1; }
                    self.wram_bank = b;
                    return;
                }
                _ => {}
            }

            if address >= 0x8000 && address < 0xA000 {
                self.cgb_vram[self.vram_bank as usize][address - 0x8000] = data;
                return;
            }
            if address >= 0xC000 && address < 0xD000 {
                self.cgb_wram[0][address - 0xC000] = data;
                return;
            }
            if address >= 0xD000 && address < 0xE000 {
                self.cgb_wram[self.wram_bank as usize][address - 0xD000] = data;
                return;
            }
            if address >= 0xE000 && address < 0xFE00 {
                let echo_addr = address - 0x2000;
                if echo_addr < 0xD000 {
                    self.cgb_wram[0][echo_addr - 0xC000] = data;
                } else {
                    self.cgb_wram[self.wram_bank as usize][echo_addr - 0xD000] = data;
                }
                return;
            }
        }

        if address >= 0xE000 && address < 0xFE00 {
            self.memory[address - 0x2000] = data;
            self.memory[address] = data;
            return;
        }
        if address >= 0xC000 && address < 0xDE00 {
            self.memory[address] = data;
            self.memory[address + 0x2000] = data;
            return;
        }
        match address {
            0xFF50 => {
                if data != 0 {
                    self.booting = false;
                }
            }
            0xFF10..=0xFF3F => {
                self.apu.write_register(address as u16, data);
            }
            0xFF00 => self.handle_joypad(data),
            0xFF46 => {
                self.memory[0xFF46] = data;
                self.dma_transfer(data);
                self.oam_dma_active = true;
                self.oam_dma_remaining = 161;
                self.oam_dma_delay = 2;
            }
            0xFF04 => {
                let old_bit = self.timer_output_bit();
                self.sys_counter = 0;
                if old_bit {
                    self.tick_tima();
                }
                self.prev_timer_bit = false;
            }
            0xFF05 => {
                if self.tima_reloaded_this_cycle {
                } else if self.tima_reload_delay > 0 {
                    self.tima_reload_delay = 0;
                    self.memory[0xFF05] = data;
                } else {
                    self.memory[0xFF05] = data;
                }
            }
            0xFF06 => {
                self.memory[0xFF06] = data;
                if self.tima_reloaded_this_cycle {
                    self.memory[0xFF05] = data;
                }
            }
            0xFF07 => {
                let old_tac = self.memory[0xFF07];
                let old_bit = if (old_tac & 0x04) != 0 {
                    let bit_mask = Self::timer_bit_for_tac(old_tac);
                    (self.sys_counter & bit_mask) != 0
                } else {
                    false
                };

                self.memory[0xFF07] = data;

                let new_bit = self.timer_output_bit();
                if old_bit && !new_bit {
                    self.tick_tima();
                }
                self.prev_timer_bit = new_bit;
            }
            0xFF40 => {
                let was_on = (self.memory[0xFF40] & 0x80) != 0;
                let is_on = (data & 0x80) != 0;
                self.memory[0xFF40] = data;

                if was_on && !is_on {
                    self.handle_lcd_off();
                } else if !was_on && is_on {
                    self.scanline = 0;
                    self.memory[0xFF44] = 0;
                    self.window_line_counter = 0;
                    self.ppu_cycles = 0;
                    self.ppu_mcycle_countdown = 1;
                    self.is_lcd_turning_on = true;
                    self.set_ppu_mode(0);
                    self.check_lyc();
                }
            }
            0xFF41 => {
                let lcd_on = (self.memory[0xFF40] & 0x80) != 0;
                let old_stat = self.memory[0xFF41];
                let read_only = old_stat & 0x07;

                if lcd_on {
                    let mode = old_stat & 0x03;
                    let ly = self.memory[0xFF44];
                    let lyc = self.memory[0xFF45];
                    let any_condition_active = mode != 3 || ly == lyc;

                    if any_condition_active && !self.prev_stat_line {
                        self.request_interrupt(1);
                    }
                    self.prev_stat_line = any_condition_active;
                }

                self.memory[0xFF41] = (data & 0xF8) | read_only;
                self.update_stat_irq_line();
            }
            0xFF44 => {
                self.memory[0xFF44] = 0;
                self.check_lyc();
            }
            0xFF45 => {
                self.memory[0xFF45] = data;
                self.check_lyc();
            }
            0xFF02 => {
                self.memory[0xFF02] = data;
                if data == 0x81 {
                    let sb = self.memory[0xFF01];
                    self.serial_output.push(sb);
                    console_log!("{}", sb as char);
                    self.memory[0xFF02] &= 0x7F;
                    self.request_interrupt(3);
                }
            }

            _ => {
                match self.cartridge_type {
                    0x0 => {
                        if address >= 0x8000 {
                            self.memory[address] = data;
                        }
                    }
                    0x1 | 0x2 | 0x3 => {
                        if address < 0x2000 {
                            self.mbc_ram_enable = (data & 0x0F) == 0x0A;
                        } else if address < 0x4000 {
                            self.rombank = (data & 0x1F) as u16;
                        } else if address < 0x6000 {
                            self.mbc1_bank2 = data & 0x3;
                        } else if address < 0x8000 {
                            self.mbc_rom_mode = data & 0x01;
                        } else if address >= 0xA000 && address < 0xC000 {
                            if self.mbc_ram_enable {
                                let r_bank = if self.mbc_rom_mode == 1 { self.mbc1_bank2 as usize } else { 0 };
                                let ram_bank = r_bank & self.ram_bank_mask;
                                self.ram[ram_bank][address - 0xA000] = data;
                            }
                        } else {
                            self.memory[address] = data;
                        }
                    }                    0x05 | 0x06 => {
                        if address < 0x4000 {
                            if (address & 0x0100) == 0 {
                                self.mbc_ram_enable = (data & 0x0F) == 0x0A;
                            } else {
                                let mut bank = (data & 0x0F) as u16;
                                if bank == 0 { bank = 1; }
                                self.rombank = bank & self.rom_bank_mask;
                            }
                        } else if address < 0x8000 {
                        } else if address >= 0xA000 && address < 0xC000 {
                            if self.mbc_ram_enable {
                                let idx = (address - 0xA000) & 0x1FF;
                                self.ram[0][idx] = data & 0x0F;
                            }
                        } else {
                            self.memory[address] = data;
                        }
                    }
                    0x0F..=0x13 => {
                        if address < 0x2000 {
                            self.mbc_ram_enable = (data & 0x0F) == 0x0A;
                        } else if address < 0x4000 {
                            let mut bank = (data & 0x7F) as u16;
                            if bank == 0 { bank = 1; }
                            self.rombank = bank & self.rom_bank_mask;
                        } else if address < 0x6000 {
                            if data <= 0x03 {
                                self.rambank = data & (self.ram_bank_mask as u8);
                            } else if data >= 0x08 && data <= 0x0C {
                                self.rambank = data;
                            }
                        } else if address < 0x8000 {
                            // RTC latch register
                        } else if address >= 0xA000 && address < 0xC000 {
                            if self.mbc_ram_enable {
                                if self.rambank <= 0x03 {
                                    self.ram[self.rambank as usize][address - 0xA000] = data;
                                } else if self.rambank >= 0x08 && self.rambank <= 0x0C {
                                    self.rtc_registers[(self.rambank - 0x08) as usize] = data;
                                }
                            }
                        } else {
                            self.memory[address] = data;
                        }
                    }
                    0x19..=0x1E => {
                        if address < 0x2000 {
                            self.mbc_ram_enable = (data & 0x0F) == 0x0A;
                        } else if address < 0x3000 {
                            let new_bank = (self.rombank & 0x100) | data as u16;
                            self.rombank = new_bank & self.rom_bank_mask;
                        } else if address < 0x4000 {
                            let new_bank = (self.rombank & 0xFF) | ((data as u16 & 0x01) << 8);
                            self.rombank = new_bank & self.rom_bank_mask;
                        } else if address < 0x6000 {
                            let raw_bank = if self.cartridge_type >= 0x1C { data & 0x07 } else { data & 0x0F };
                            self.rambank = raw_bank & (self.ram_bank_mask as u8);
                        } else if address >= 0xA000 && address < 0xC000 {
                            if self.mbc_ram_enable {
                                self.ram[self.rambank as usize][address - 0xA000] = data;
                            }
                        } else {
                            self.memory[address] = data;
                        }
                    }
                    _ => {
                        if address >= 0x8000 {
                            self.memory[address] = data;
                        }
                    }
                }
            }
        }
    }

    fn handle_joypad(&mut self, data: u8) {
        self.memory[0xFF00] = (data & 0x30) | 0xC0;
    }

    fn dma_transfer(&mut self, data: u8) {
        let start = (data as u16) << 8;
        self.oam_dma_source = start;
        self.oam_dma_index = 0;
    }

    fn start_hdma(&mut self, data: u8) {
        if !self.hdma_active && (data & 0x80) == 0 {
            // General Purpose DMA
            let blocks = (data & 0x7F) as u16 + 1;
            for _ in 0..blocks {
                self.do_hdma_block();
            }
            self.hdma_active = false;
            self.hdma_len = 0xFF;
        } else if (data & 0x80) != 0 {
            // HBlank DMA Start
            self.hdma_len = data & 0x7F;
            self.hdma_active = true;
        } else if self.hdma_active && (data & 0x80) == 0 {
            // Cancel HBlank DMA
            self.hdma_active = false;
            self.hdma_len |= 0x80;
        }
    }

    fn do_hdma_block(&mut self) {
        let src = (self.hdma_src & 0xFFF0) as usize;
        let dst = ((self.hdma_dst & 0x1FF0) | 0x8000) as usize;

        for i in 0..16 {
            let val = self.peek_byte(src + i);
            self.cgb_vram[self.vram_bank as usize][(dst + i) - 0x8000] = val;
        }

        self.hdma_src = self.hdma_src.wrapping_add(16);
        self.hdma_dst = self.hdma_dst.wrapping_add(16);
        if self.hdma_dst >= 0xA000 {
            self.hdma_dst = 0x8000;
        }

        self.dma_pause_mcycles += if self.double_speed { 16 } else { 8 };
    }

    pub fn peek_byte(&self, address: usize) -> u8 {
        if address >= 0xFE00 && address < 0xFEA0 {
            if self.oam_dma_active && self.oam_dma_delay == 0 {
                return 0xFF;
            }
            if (self.memory[0xFF40] & 0x80) != 0 {
                let mode = self.memory[0xFF41] & 0x03;
                if mode == 2 || mode == 3 {
                    return 0xFF;
                }
            }
        } else if address >= 0x8000 && address <= 0x9FFF {
            if (self.memory[0xFF40] & 0x80) != 0 {
                let mode = self.memory[0xFF41] & 0x03;
                if mode == 3 {
                    return 0xFF;
                }
            }
        }

        if address == 0xFF02 { return self.memory[0xFF02] | 0x7E; }
        if address == 0xFF07 { return self.memory[0xFF07] | 0xF8; }
        if address == 0xFF0F { return self.memory[0xFF0F] | 0xE0; }
        if address == 0xFF41 { return self.memory[0xFF41] | 0x80; }

        if self.is_cgb {
            if address >= 0xFF4C && address <= 0xFF7F && address != 0xFF50 {
                match address {
                    0xFF4D => return (self.key1 & 0x7E) | (if self.double_speed { 0x80 } else { 0 }) | (self.key1 & 0x01) | 0x7E,
                    0xFF4F => return self.vram_bank | 0xFE,
                    0xFF51..=0xFF54 => return 0xFF,
                    0xFF55 => return if self.hdma_active { self.hdma_len & 0x7F } else { 0xFF },
                    0xFF68 => return self.bgpi | 0x40,
                    0xFF69 => return self.cgb_bg_palettes[(self.bgpi & 0x3F) as usize],
                    0xFF6A => return self.obpi | 0x40,
                    0xFF6B => return self.cgb_obj_palettes[(self.obpi & 0x3F) as usize],
                    0xFF70 => return self.wram_bank | 0xF8,
                    _ => {}
                }
            }

            if address >= 0x8000 && address < 0xA000 {
                return self.cgb_vram[self.vram_bank as usize][address - 0x8000];
            }
            if address >= 0xC000 && address < 0xD000 {
                return self.cgb_wram[0][address - 0xC000];
            }
            if address >= 0xD000 && address < 0xE000 {
                return self.cgb_wram[self.wram_bank as usize][address - 0xD000];
            }
            if address >= 0xE000 && address < 0xFE00 {
                let echo_addr = address - 0x2000;
                if echo_addr < 0xD000 {
                    return self.cgb_wram[0][echo_addr - 0xC000];
                } else {
                    return self.cgb_wram[self.wram_bank as usize][echo_addr - 0xD000];
                }
            }
        }

        if address >= 0xE000 && address < 0xFE00 {
            return self.memory[address - 0x2000];
        }
        if self.boot_rom_maps_address(address) {
            return self.boot_mem[address];
        }
        if address == 0xFF00 {
            let select = self.memory[0xFF00];
            let p14 = select & 0x10 == 0;
            let p15 = select & 0x20 == 0;

            let mut result: u8 = 0x0F;

            if p14 {
                let right = !self.keys[39] as u8;
                let left = !self.keys[37] as u8;
                let up = !self.keys[38] as u8;
                let down = !self.keys[40] as u8;
                result &= (down << 3) | (up << 2) | (left << 1) | right;
            }
            if p15 {
                let a = !self.keys[65] as u8;
                let b = !self.keys[66] as u8;
                let select_btn = !self.keys[16] as u8;
                let start = !self.keys[13] as u8;
                result &= (start << 3) | (select_btn << 2) | (b << 1) | a;
            }

            return (select & 0x30) | 0xC0 | result;
        }
        if address == 0xFF04 {
            return (self.sys_counter >> 8) as u8;
        }
        if address >= 0xFF10 && address <= 0xFF3F {
            return self.apu.read_register(address as u16);
        }
        match self.cartridge_type {
            0x0 => {
                if address < 0x8000 {
                    return self.rom.get(address).copied().unwrap_or(0xFF);
                }
                return self.memory[address];
            }
            0x1 | 0x2 | 0x3 => {
                if address < 0x4000 {
                    if self.mbc_rom_mode == 1 && self.rom_bank_mask >= 0x20 {
                        let bank = ((self.mbc1_bank2 as usize) << 5) & (self.rom_bank_mask as usize);
                        return self.rom.get(address + bank * 0x4000).copied().unwrap_or(0xFF);
                    }
                    return self.rom.get(address).copied().unwrap_or(0xFF);
                } else if address < 0x8000 {
                    let mut lower_5_bits = self.rombank as usize;
                    if lower_5_bits == 0 {
                        lower_5_bits = 1;
                    }
                    let bank2 = (self.mbc1_bank2 as usize) << 5;
                    let bank_raw = bank2 | lower_5_bits;
                    let bank = bank_raw & (self.rom_bank_mask as usize);

                    return self.rom.get(address - 0x4000 + bank * 0x4000).copied().unwrap_or(0xFF);
                } else if address >= 0xA000 && address < 0xC000 {
                    if self.mbc_ram_enable {
                        let r_bank = if self.mbc_rom_mode == 1 { self.mbc1_bank2 as usize } else { 0 };
                        let ram_bank = r_bank & self.ram_bank_mask;
                        return self.ram[ram_bank][address - 0xA000];
                    }
                    return 0xFF;
                }
                self.memory[address]
            }
            0x05 | 0x06 => {
                if address < 0x4000 {
                    return self.rom.get(address).copied().unwrap_or(0xFF);
                } else if address < 0x8000 {
                    return self.rom.get(address - 0x4000 + (self.rombank as usize) * 0x4000).copied().unwrap_or(0xFF);
                } else if address >= 0xA000 && address < 0xC000 {
                    if self.mbc_ram_enable {
                        let idx = (address - 0xA000) & 0x1FF;
                        return self.ram[0][idx] | 0xF0;
                    }
                    return 0xFF;
                }
                self.memory[address]
            }
            0x0F..=0x13 => {
                if address < 0x4000 {
                    return self.rom.get(address).copied().unwrap_or(0xFF);
                } else if address < 0x8000 {
                    return self.rom.get(address - 0x4000 + (self.rombank as usize) * 0x4000).copied().unwrap_or(0xFF);
                } else if address >= 0xA000 && address < 0xC000 {
                    if self.mbc_ram_enable {
                        if self.rambank <= 0x03 {
                            return self.ram[self.rambank as usize][address - 0xA000];
                        } else if self.rambank >= 0x08 && self.rambank <= 0x0C {
                            return self.rtc_registers[(self.rambank - 0x08) as usize];
                        }
                    }
                    return 0xFF;
                }
                self.memory[address]
            }
            0x19..=0x1E => {
                if address < 0x4000 {
                    return self.rom.get(address).copied().unwrap_or(0xFF);
                } else if address < 0x8000 {
                    return self.rom.get(address - 0x4000 + (self.rombank as usize) * 0x4000).copied().unwrap_or(0xFF);
                } else if address >= 0xA000 && address < 0xC000 {
                    if self.mbc_ram_enable {
                        return self.ram[self.rambank as usize][address - 0xA000];
                    }
                    return 0xFF;
                }
                self.memory[address]
            }
            _ => self.memory[address]
        }
    }

    pub fn read_byte(&mut self, address: usize) -> u8 {
        self.tick_timer_4t();
        self.peek_byte(address)
    }

    fn read(&mut self) -> u8 {
        let data = self.read_byte(self.program_counter as usize);
        self.program_counter = self.program_counter.wrapping_add(1);
        data
    }

    fn read16(&mut self) -> u16 {
        let data1 = self.read_byte(self.program_counter as usize);
        let data2 = self.read_byte(self.program_counter.wrapping_add(1) as usize);
        let data = (data2 as u16) << 8 | data1 as u16;
        self.program_counter = self.program_counter.wrapping_add(2);
        data
    }

    pub fn reset(&mut self) {
        self.reg_a = if self.is_cgb { 0x11 } else { 0x01 };
        self.reg_b = 0;
        self.reg_c = 0;
        self.reg_d = 0;
        self.reg_e = 0;
        self.reg_h = 0;
        self.reg_l = 0;
        self.reg_f = 0;
        self.stackpointer = 0x100;
        self.interrupt_master_enable = false;
        self.ei_pending = false;
        self.sys_counter = 0;
        self.prev_timer_bit = false;
        self.program_counter = 0;
        self.oam_dma_remaining = 0;
        self.oam_dma_active = false;
        self.oam_dma_delay = 0;
        self.oam_dma_source = 0;
        self.oam_dma_index = 0;
        self.prev_stat_line = false;
        self.ppu_mcycle_countdown = 0;
        self.halt = false;
        self.tima_reload_delay = 0;
        self.tima_reloaded_this_cycle = false;
        self.is_lcd_turning_on = false;
        self.double_speed = false;
        self.key1 = 0;
        self.vram_bank = 0;
        self.wram_bank = 1;
        self.hdma_active = false;
        self.ppu_t_cycle_accum = 0;
        self.dma_pause_mcycles = 0;
        self.window_line_counter = 0;
    }

    fn hl(&self) -> u16 {
        (self.reg_h as u16) << 8 | self.reg_l as u16
    }

    fn de(&self) -> u16 {
        (self.reg_d as u16) << 8 | self.reg_e as u16
    }

    fn bc(&self) -> u16 {
        (self.reg_b as u16) << 8 | self.reg_c as u16
    }

    fn af(&self) -> u16 {
        (self.reg_a as u16) << 8 | self.reg_f as u16
    }

    pub fn get_debug_state(&self) -> String {
        let opcode = self.peek_byte(self.program_counter as usize);
        let ie = self.peek_byte(0xFFFF);
        let iflag = self.peek_byte(0xFF0F);
        let ly = self.memory[0xFF44];
        let stat = self.peek_byte(0xFF41);
        let lcdc = self.peek_byte(0xFF40);
        let tac = self.peek_byte(0xFF07);
        let tima = self.peek_byte(0xFF05);
        let flags = format!("{}{}{}{}",
                            if self.reg_f & 0x80 != 0 { 'Z' } else { '-' },
                            if self.reg_f & 0x40 != 0 { 'N' } else { '-' },
                            if self.reg_f & 0x20 != 0 { 'H' } else { '-' },
                            if self.reg_f & 0x10 != 0 { 'C' } else { '-' },
        );
        let mut mem_dump = String::new();
        let pc = self.program_counter as usize;
        for i in 0..8 {
            let addr = pc.wrapping_add(i);
            if addr <= 0xFFFF {
                mem_dump.push_str(&format!("{:02X} ", self.peek_byte(addr)));
            }
        }
        format!(
            "PC:{:04X} OP:{:02X} SP:{:04X}\nAF:{:04X} BC:{:04X}\nDE:{:04X} HL:{:04X}\nF:[{}] IME:{} HALT:{}\nLY:{:02X} STAT:{:02X} LCDC:{:02X}\nIE:{:02X} IF:{:02X}\nTAC:{:02X} TIMA:{:02X}\nROM:{:03X} RAM:{:02X} Cart:{:02X} Boot:{}\nMem@PC: {}\nCycles:{}",
            self.program_counter, opcode, self.stackpointer,
            self.af(), self.bc(),
            self.de(), self.hl(),
            flags, self.interrupt_master_enable as u8, self.halt as u8,
            ly, stat, lcdc,
            ie, iflag,
            tac, tima,
            self.rombank, self.rambank, self.cartridge_type, self.booting as u8,
            mem_dump.trim(),
            self.total_cycles,
        )
    }

    pub fn get_tile_data(&self, base: usize) -> &[u8] {
        if self.is_cgb {
            &self.cgb_vram[self.vram_bank as usize][base - 0x8000 .. base - 0x8000 + 0x2000]
        } else {
            &self.memory[base..base + 0x2000]
        }
    }

    pub fn handle_interrupts(&mut self) {
    }

    pub fn set_keys(&mut self, key: u32, value: bool) {
        if (key as usize) < self.keys.len() {
            self.keys[key as usize] = value;
        }
    }

    pub fn request_interrupt(&mut self, interrupt: u8) {
        self.memory[0xFF0F] |= 1 << interrupt;
    }

    #[inline(always)]
    fn timer_bit_for_tac(tac: u8) -> u16 {
        match tac & 0x03 {
            0 => 1 << 9,
            1 => 1 << 3,
            2 => 1 << 5,
            3 => 1 << 7,
            _ => unreachable!(),
        }
    }

    fn timer_output_bit(&self) -> bool {
        let tac = self.memory[0xFF07];
        let enabled = tac & 0x04 != 0;
        if !enabled {
            return false;
        }
        let bit_mask = Self::timer_bit_for_tac(tac);
        (self.sys_counter & bit_mask) != 0
    }

    #[inline(always)]
    fn tick_tima(&mut self) {
        let tima = self.memory[0xFF05];
        if tima == 0xFF {
            self.memory[0xFF05] = 0x00;
            self.tima_reload_delay = 1;
        } else {
            self.memory[0xFF05] = tima.wrapping_add(1);
        }
    }

    fn check_timer_falling_edge(&mut self) {
        let current_bit = self.timer_output_bit();
        if self.prev_timer_bit && !current_bit {
            self.tick_tima();
        }
        self.prev_timer_bit = current_bit;
    }

    #[inline(always)]
    pub fn tick_timer_4t(&mut self) {
        self.tima_reloaded_this_cycle = false;
        if self.tima_reload_delay > 0 {
            self.tima_reload_delay -= 1;
            if self.tima_reload_delay == 0 {
                self.memory[0xFF05] = self.memory[0xFF06];
                self.request_interrupt(2);
                self.tima_reloaded_this_cycle = true;
            }
        }

        self.sys_counter = self.sys_counter.wrapping_add(4);
        self.timer_ticks_this_instr += 1;

        let tac = self.memory[0xFF07];
        if tac & 0x04 != 0 {
            let bit_mask = Self::timer_bit_for_tac(tac);
            let new_bit = (self.sys_counter & bit_mask) != 0;
            if self.prev_timer_bit && !new_bit {
                self.tick_tima();
            }
            self.prev_timer_bit = new_bit;
        } else if self.prev_timer_bit {
            self.prev_timer_bit = false;
        }

        let tick_ppu_apu = if self.double_speed {
            self.ppu_t_cycle_accum += 1;
            if self.ppu_t_cycle_accum >= 2 {
                self.ppu_t_cycle_accum -= 2;
                true
            } else {
                false
            }
        } else {
            true
        };

        if tick_ppu_apu {
            self.ppu_tick_mcycle();
            self.apu.tick(4);
        }

        if self.oam_dma_active {
            if self.oam_dma_delay > 0 {
                self.oam_dma_delay -= 1;
            }
            if self.oam_dma_delay == 0 && self.oam_dma_remaining > 0 {
                if self.oam_dma_remaining > 1 {
                    let source_addr = self.oam_dma_source.wrapping_add(self.oam_dma_index as u16);
                    let byte = self.peek_byte(source_addr as usize);
                    self.memory[0xFE00 + self.oam_dma_index as usize] = byte;
                    self.oam_dma_index += 1;
                }
                self.oam_dma_remaining -= 1;
                if self.oam_dma_remaining == 0 {
                    self.oam_dma_active = false;
                }
            }
        }
    }

    fn ppu_tick_mcycle(&mut self) {
        let lcd_on = (self.memory[0xFF40] & 0x80) != 0;
        if !lcd_on {
            return;
        }

        if self.ppu_mcycle_countdown > 0 {
            self.ppu_mcycle_countdown -= 1;
        }

        if self.scanline == 153 && self.ppu_mcycle_countdown == 113 {
            self.memory[0xFF44] = 0;
            self.check_lyc();
        }

        let current_mode = self.memory[0xFF41] & 0b11;

        if self.ppu_mcycle_countdown == 1 && current_mode == 3 {
            self.update_stat_irq_line();
        }

        if self.ppu_mcycle_countdown > 0 {
            return;
        }

        match current_mode {
            2 => {
                let scroll_adjust = self.ppu_scroll_adjust();
                self.ppu_mcycle_countdown = 43 + scroll_adjust;
                self.set_ppu_mode(3);
            }
            3 => {
                let scroll_adjust = self.ppu_scroll_adjust();
                self.ppu_mcycle_countdown = 50 - scroll_adjust;
                crate::ppu::draw_scanline(self);
                self.set_ppu_mode(0);
            }
            0 => {
                if self.is_lcd_turning_on {
                    self.is_lcd_turning_on = false;
                    self.ppu_mcycle_countdown = 20;
                    self.set_ppu_mode(2);
                    return;
                }
                self.scanline += 1;
                self.memory[0xFF44] = self.scanline;
                self.check_lyc();

                if self.scanline >= 144 {
                    self.ppu_mcycle_countdown = 114;
                    self.set_ppu_mode(1);
                    self.request_interrupt(0);
                } else {
                    self.ppu_mcycle_countdown = 21;
                    self.set_ppu_mode(2);
                }
            }
            1 => {
                self.scanline += 1;

                if self.scanline > 153 {
                    self.scanline = 0;
                    self.memory[0xFF44] = 0;
                    self.window_line_counter = 0;
                    self.check_lyc();
                    self.ppu_mcycle_countdown = 21;
                    self.set_ppu_mode(2);
                } else {
                    if self.scanline == 153 {
                        self.memory[0xFF44] = 153;
                    } else {
                        self.memory[0xFF44] = self.scanline;
                    }
                    self.check_lyc();
                    self.ppu_mcycle_countdown = 114;
                }
            }
            _ => unreachable!(),
        }
    }

    fn ppu_scroll_adjust(&self) -> u32 {
        let scx = self.memory[0xFF43] % 8;
        match scx {
            5..=7 => 2,
            1..=4 => 1,
            _ => 0,
        }
    }

    pub fn handle_timer(&mut self, _t_cycles: u32) {
    }

    pub fn update_apu(&mut self, _t_cycles: u32) {
    }

    pub fn get_audio_buffer(&mut self) -> Vec<f32> {
        self.apu.drain_samples()
    }

    pub fn update_ppu(&mut self, _cycles: u32) {
    }

    fn check_lyc(&mut self) {
        let ly = self.memory[0xFF44];
        let lyc = self.memory[0xFF45];
        if ly == lyc {
            self.memory[0xFF41] |= 0x04;
        } else {
            self.memory[0xFF41] &= !0x04;
        }
        self.update_stat_irq_line();
    }

    fn handle_lcd_off(&mut self) {
        self.memory[0xFF44] = 0;
        self.memory[0xFF41] &= 0xFC;
        self.ppu_cycles = 0;
        self.ppu_mcycle_countdown = 0;
        self.scanline = 0;
        self.window_line_counter = 0;
        self.prev_stat_line = false;
    }

    fn set_ppu_mode(&mut self, mode: u8) {
        let old_mode = self.memory[0xFF41] & 0x03;
        self.memory[0xFF41] = (self.memory[0xFF41] & 0b11111100) | (mode & 0x03);

        if (self.memory[0xFF40] & 0x80) != 0 {
            self.update_stat_irq_line();
        }

        if self.is_cgb && mode == 0 && old_mode != 0 && self.hdma_active {
            self.do_hdma_block();
            if self.hdma_len == 0 {
                self.hdma_active = false;
                self.hdma_len = 0xFF;
            } else {
                self.hdma_len -= 1;
            }
        }
    }

    fn update_stat_irq_line(&mut self) {
        let stat = self.memory[0xFF41];
        let mode = stat & 0x03;
        let ly = self.memory[0xFF44];
        let lyc = self.memory[0xFF45];

        let hblank_trigger = mode == 0 && (stat & 0x08 != 0);
        let vblank_trigger = mode == 1 && ((stat & 0x10 != 0) || (stat & 0x20 != 0));

        let line = hblank_trigger
            || vblank_trigger
            || (mode == 2 && stat & 0x20 != 0)
            || (ly == lyc && stat & 0x40 != 0);

        if line && !self.prev_stat_line {
            self.request_interrupt(1);
        }
        self.prev_stat_line = line;
    }

    #[allow(dead_code)]
    fn increment_ly(&mut self) {
        if (self.memory[0xFF40] & 0x80) != 0 {
            let ly = self.memory[0xFF44].wrapping_add(1);
            self.memory[0xFF44] = ly;
            self.check_lyc();
        }
    }

    fn push(&mut self, data: u16) {
        self.stackpointer = self.stackpointer.wrapping_sub(1);
        self.write_byte(self.stackpointer as usize, (data >> 8) as u8);
        self.stackpointer = self.stackpointer.wrapping_sub(1);
        self.write_byte(self.stackpointer as usize, (data & 0xFF) as u8);
    }

    pub fn get_opcode_name(&mut self, opcode: u8) -> &'static str {
        Self::opcode_name_static(opcode)
    }

    pub fn get_extra_opcode_name(&mut self, opcode: u8) -> &'static str {
        Self::cb_opcode_name_static(opcode)
    }

    pub fn execute(&mut self) {
        if self.dma_pause_mcycles > 0 {
            for _ in 0..self.dma_pause_mcycles {
                self.tick_timer_4t();
            }
            self.cycles += self.dma_pause_mcycles;
            self.dma_pause_mcycles = 0;
        }

        let was_ei_pending = self.ei_pending;
        self.timer_ticks_this_instr = 0;
        let cycles_before = self.cycles;

        let pending_interrupts = self.memory[0xFF0F] & self.memory[0xFFFF] & 0x1F;
        if self.interrupt_master_enable && pending_interrupts != 0 && !self.halt {
            self.interrupt_master_enable = false;
            self.halt_bug = false;
            self.ei_pending = false;

            self.tick_timer_4t();
            self.tick_timer_4t();
            self.tick_timer_4t();
            self.push(self.program_counter);

            let current_valid = self.memory[0xFF0F] & self.memory[0xFFFF] & 0x1F;
            let mut final_i = 0;
            for j in 0..5 {
                if current_valid & (1 << j) != 0 {
                    final_i = j;
                    break;
                }
            }

            self.memory[0xFF0F] &= !(1u8 << final_i);
            self.program_counter = 0x0040 + (final_i as u16) * 0x0008;
            self.cycles += 5;

            let instr_m_cycles = self.cycles - cycles_before;
            if instr_m_cycles > self.timer_ticks_this_instr {
                let remaining = instr_m_cycles - self.timer_ticks_this_instr;
                for _ in 0..remaining {
                    self.tick_timer_4t();
                }
            }
            return;
        }

        if self.halt {
            self.tick_timer_4t();
            self.cycles += 1;

            let valid_interrupts = self.memory[0xFF0F] & self.memory[0xFFFF] & 0x1F;
            if valid_interrupts != 0 {
                self.halt = false;
            }

            if was_ei_pending && self.ei_pending {
                self.interrupt_master_enable = true;
                self.ei_pending = false;
            }
            return;
        }

        let opcode = self.read_byte(self.program_counter as usize);

        if self.tracing {
            let line = self.format_trace_line();
            self.trace_buffer.push(line);
        }

        self.program_counter = self.program_counter.wrapping_add(1);
        if self.halt_bug {
            self.halt_bug = false;
            self.program_counter = self.program_counter.wrapping_sub(1);
        }

        if self.consolelog {
            let _opcode_name = self.get_opcode_name(opcode);
            console_log!("Pointer: {:x}, opcode: {:x}, name: {}, AF: {:x}, BC: {:x}, DE: {:x}, HL: {:x}, SP: {:x}", self.program_counter-1, opcode, _opcode_name, self.af(), self.bc(), self.de(), self.hl(), self.stackpointer);
        }

        match opcode {
            0x00 => self.nop(),
            0x01 => self.ldbca16(),
            0x02 => self.ldhbca(),
            0x3 => self.incbc(),
            0x4 => self.incb(),
            0x5 => self.decb(),
            0x6 => self.ldb(),
            0x7 => self.rlca(),
            0x8 => self.lda16sp(),
            0x9 => self.addhlbc(),
            0xa => self.ldabc(),
            0xb => self.decbc(),
            0xc => self.incc(),
            0xd => self.decc(),
            0xe => self.ldc(),
            0xf => self.rrca(),
            0x10 => self.stop(),
            0x11 => self.ldded16(),
            0x12 => self.ldhdea(),
            0x13 => self.incde(),
            0x14 => self.incd(),
            0x15 => self.decd(),
            0x16 => self.ldd(),
            0x17 => self.rla(),
            0x18 => self.jr(),
            0x19 => self.addhlde(),
            0x1a => self.ldade(),
            0x1b => self.decde(),
            0x1c => self.ince(),
            0x1d => self.dece(),
            0x1e => self.lde(),
            0x1f => self.rra(),
            0x20 => self.jrnz(),
            0x21 => self.ldhl16(),
            0x22 => self.ldhlplusa(),
            0x23 => self.inchl(),
            0x24 => self.inch(),
            0x25 => self.dech(),
            0x26 => self.ldha8(),
            0x27 => self.daa(),
            0x28 => self.jrz(),
            0x29 => self.addhlhl(),
            0x2a => self.ldahlplus(),
            0x2b => self.dechl(),
            0x2c => self.incl(),
            0x2d => self.decl(),
            0x2e => self.ldl(),
            0x2f => self.cplm(),
            0x30 => self.jrncr8(),
            0x31 => self.ldsp16(),
            0x32 => self.ldhlmina(),
            0x33 => self.incsp(),
            0x34 => self.inchhl(),
            0x35 => self.dechhl(),
            0x36 => self.ldhla8(),
            0x37 => self.scf(),
            0x38 => self.jrcr8(),
            0x39 => self.addhlsp(),
            0x3a => self.ldahlmin(),
            0x3b => self.decsp(),
            0x3c => self.inca(),
            0x3d => self.deca(),
            0x3e => self.lda(),
            0x3f => self.ccf(),
            0x40 => self.ldbb(),
            0x41 => self.ldbc(),
            0x42 => self.ldbd(),
            0x43 => self.ldbe(),
            0x44 => self.ldbh(),
            0x45 => self.ldbl(),
            0x46 => self.ldbhl(),
            0x47 => self.ldba(),
            0x48 => self.ldcb(),
            0x49 => self.ldcc(),
            0x4a => self.ldcd(),
            0x4b => self.ldce(),
            0x4c => self.ldch(),
            0x4d => self.ldcl(),
            0x4e => self.ldchl(),
            0x4f => self.ldca(),
            0x50 => self.lddb(),
            0x51 => self.lddc(),
            0x52 => self.lddd(),
            0x53 => self.ldde(),
            0x54 => self.lddh(),
            0x55 => self.lddl(),
            0x56 => self.lddhl(),
            0x57 => self.ldda(),
            0x58 => self.ldeb(),
            0x59 => self.ldec(),
            0x5a => self.lded(),
            0x5b => self.ldee(),
            0x5c => self.ldeh(),
            0x5d => self.ldel(),
            0x5e => self.ldehl(),
            0x5f => self.ldea(),
            0x60 => self.ldhb(),
            0x61 => self.ldhc(),
            0x62 => self.ldhd(),
            0x63 => self.ldhe(),
            0x64 => self.ldhh(),
            0x65 => self.ldhl(),
            0x66 => self.ldhhl(),
            0x67 => self.ldha(),
            0x68 => self.ldlb(),
            0x69 => self.ldlc(),
            0x6a => self.ldld(),
            0x6b => self.ldle(),
            0x6c => self.ldlh(),
            0x6d => self.ldll(),
            0x6e => self.ldlhl(),
            0x6f => self.ldla(),
            0x70 => self.ldhlb(),
            0x71 => self.ldhlc(),
            0x72 => self.ldhld(),
            0x73 => self.ldhle(),
            0x74 => self.ldhlh(),
            0x75 => self.ldhll(),
            0x76 => self.halt(),
            0x77 => self.ldhla(),
            0x78 => self.ldab(),
            0x79 => self.ldac(),
            0x7a => self.ldad(),
            0x7b => self.ldae(),
            0x7c => self.ldah(),
            0x7d => self.ldal(),
            0x7e => self.ldahl(),
            0x7f => self.ldaa(),
            0x80 => self.addab(),
            0x81 => self.addac(),
            0x82 => self.addad(),
            0x83 => self.addae(),
            0x84 => self.addah(),
            0x85 => self.addal(),
            0x86 => self.addahl(),
            0x87 => self.addaa(),
            0x88 => self.adcab(),
            0x89 => self.adcac(),
            0x8a => self.adcad(),
            0x8b => self.adcae(),
            0x8c => self.adcah(),
            0x8d => self.adcal(),
            0x8e => self.adcahl(),
            0x8f => self.adcaa(),
            0x90 => self.subb(),
            0x91 => self.subc(),
            0x92 => self.subd(),
            0x93 => self.sube(),
            0x94 => self.subh(),
            0x95 => self.subl(),
            0x96 => self.subhl(),
            0x97 => self.suba(),
            0x98 => self.sbcab(),
            0x99 => self.sbcac(),
            0x9a => self.sbcad(),
            0x9b => self.sbcae(),
            0x9c => self.sbcah(),
            0x9d => self.sbcal(),
            0x9e => self.sbcahl(),
            0x9f => self.sbcaa(),
            0xa0 => self.andb(),
            0xa1 => self.andc(),
            0xa2 => self.andd(),
            0xa3 => self.ande(),
            0xa4 => self.andh(),
            0xa5 => self.andl(),
            0xa6 => self.andhl(),
            0xa7 => self.anda(),
            0xa8 => self.xorb(),
            0xa9 => self.xorc(),
            0xaa => self.xord(),
            0xab => self.xore(),
            0xac => self.xorh(),
            0xad => self.xorl(),
            0xae => self.xorhl(),
            0xaf => self.xora(),
            0xb0 => self.orb(),
            0xb1 => self.orc(),
            0xb2 => self.ord(),
            0xb3 => self.ore(),
            0xb4 => self.orh(),
            0xb5 => self.orl(),
            0xb6 => self.orhl(),
            0xb7 => self.ora(),
            0xb8 => self.cpb(),
            0xb9 => self.cpc(),
            0xba => self.cpd(),
            0xbb => self.cpe(),
            0xbc => self.cph(),
            0xbd => self.cpl(),
            0xbe => self.cphl(),
            0xbf => self.cpa(),
            0xc0 => self.retnz(),
            0xc1 => self.popbc(),
            0xc2 => self.jpnza16(),
            0xc3 => self.jp(),
            0xc4 => self.callnza16(),
            0xc5 => self.pushbc(),
            0xc6 => self.adda8(),
            0xc7 => self.rst00h(),
            0xc8 => self.retz(),
            0xc9 => self.ret(),
            0xca => self.jpza16(),
            0xcb => {
                let cb_opcode = self.read();
                self.execute_cb_with_opcode(cb_opcode);
                self.capture_instruction_trace(cb_opcode, true);
            },
            0xcc => self.callza16(),
            0xcd => self.calla16(),
            0xce => self.adcaa8(),
            0xcf => self.rst08h(),
            0xd0 => self.retnc(),
            0xd1 => self.popde(),
            0xd2 => self.jpnca16(),
            0xd4 => self.callnca16(),
            0xd5 => self.pushde(),
            0xd6 => self.suba8(),
            0xd7 => self.rst10h(),
            0xd8 => self.retc(),
            0xd9 => self.reti(),
            0xda => self.jpca16(),
            0xdc => self.callca16(),
            0xde => self.sbca8(),
            0xdf => self.rst18h(),
            0xe0 => self.ldha8a(),
            0xe1 => self.pophl(),
            0xe2 => self.ldhca(),
            0xe5 => self.pushhl(),
            0xe6 => self.anda8(),
            0xe7 => self.rst20h(),
            0xe8 => self.addspr8(),
            0xe9 => self.jphl(),
            0xea => self.lda16a(),
            0xee => self.xora8(),
            0xef => self.rst28h(),
            0xf0 => self.ldhaa8(),
            0xf1 => self.popaf(),
            0xf2 => self.ldahc(),
            0xf3 => self.di(),
            0xf5 => self.pushaf(),
            0xf6 => self.ora8(),
            0xf7 => self.rst30h(),
            0xf8 => self.ldhlspr8(),
            0xf9 => self.ldsphl(),
            0xfa => self.ldaa16(),
            0xfb => self.ei(),
            0xfe => self.cp(),
            0xff => self.rst38h(),
            _ => {
                self.cycles += 1;
            }
        }

        if opcode != 0xCB {
            self.capture_instruction_trace(opcode, false);
        }

        let instr_m_cycles = self.cycles - cycles_before;
        if instr_m_cycles > self.timer_ticks_this_instr {
            let remaining = instr_m_cycles - self.timer_ticks_this_instr;
            for _ in 0..remaining {
                self.tick_timer_4t();
            }
        }

        if was_ei_pending && self.ei_pending {
            self.interrupt_master_enable = true;
            self.ei_pending = false;
        }
    }

    fn execute_cb(&mut self) {
        let opcode = self.read();
        self.execute_cb_with_opcode(opcode);
    }

    fn execute_cb_with_opcode(&mut self, opcode: u8) {
        if self.consolelog {
            let _opcode_name = self.get_extra_opcode_name(opcode);
            console_log!("Pointer: {:x}, opcode: {:x}, name: {}, AF: {:x}, BC: {:x}, DE: {:x}, HL: {:x}, SP: {:x}", self.program_counter-1, opcode, _opcode_name, self.af(), self.bc(), self.de(), self.hl(), self.stackpointer);
        }

        match opcode {
            0x00 => self.rlcb(),
            0x01 => self.rlcc(),
            0x02 => self.rlcd(),
            0x03 => self.rlce(),
            0x04 => self.rlch(),
            0x05 => self.rlcl(),
            0x06 => self.rlchl(),
            0x07 => self.cb_rlca(),
            0x08 => self.rrcb(),
            0x09 => self.rrcc(),
            0x0a => self.rrcd(),
            0x0b => self.rrce(),
            0x0c => self.rrch(),
            0x0d => self.rrcl(),
            0x0e => self.rrchl(),
            0x0f => self.cb_rrca(),
            0x10 => self.rlb(),
            0x11 => self.rlc(),
            0x12 => self.rld(),
            0x13 => self.rle(),
            0x14 => self.rlh(),
            0x15 => self.rll(),
            0x16 => self.rlhl(),
            0x17 => self.cb_rla(),
            0x18 => self.rrb(),
            0x19 => self.rrc(),
            0x1a => self.rrd(),
            0x1b => self.rre(),
            0x1c => self.rrh(),
            0x1d => self.rrl(),
            0x1e => self.rrhl(),
            0x1f => self.cb_rra(),
            0x20 => self.slab(),
            0x21 => self.slac(),
            0x22 => self.slad(),
            0x23 => self.slae(),
            0x24 => self.slah(),
            0x25 => self.slal(),
            0x26 => self.slahl(),
            0x27 => self.slaa(),
            0x28 => self.srab(),
            0x29 => self.srac(),
            0x2a => self.srad(),
            0x2b => self.srae(),
            0x2c => self.srah(),
            0x2d => self.sral(),
            0x2e => self.srahl(),
            0x2f => self.sraa(),
            0x30 => self.swapb(),
            0x31 => self.swapc(),
            0x32 => self.swapd(),
            0x33 => self.swape(),
            0x34 => self.swaph(),
            0x35 => self.swapl(),
            0x36 => self.swaphl(),
            0x37 => self.swapa(),
            0x38 => self.srlb(),
            0x39 => self.srlc(),
            0x3a => self.srld(),
            0x3b => self.srle(),
            0x3c => self.srlh(),
            0x3d => self.srll(),
            0x3e => self.srlhl(),
            0x3f => self.srla(),
            0x40 => self.bit0b(),
            0x41 => self.bit0c(),
            0x42 => self.bit0d(),
            0x43 => self.bit0e(),
            0x44 => self.bit0h(),
            0x45 => self.bit0l(),
            0x46 => self.bit0hl(),
            0x47 => self.bit0a(),
            0x48 => self.bit1b(),
            0x49 => self.bit1c(),
            0x4a => self.bit1d(),
            0x4b => self.bit1e(),
            0x4c => self.bit1h(),
            0x4d => self.bit1l(),
            0x4e => self.bit1hl(),
            0x4f => self.bit1a(),
            0x50 => self.bit2b(),
            0x51 => self.bit2c(),
            0x52 => self.bit2d(),
            0x53 => self.bit2e(),
            0x54 => self.bit2h(),
            0x55 => self.bit2l(),
            0x56 => self.bit2hl(),
            0x57 => self.bit2a(),
            0x58 => self.bit3b(),
            0x59 => self.bit3c(),
            0x5a => self.bit3d(),
            0x5b => self.bit3e(),
            0x5c => self.bit3h(),
            0x5d => self.bit3l(),
            0x5e => self.bit3hl(),
            0x5f => self.bit3a(),
            0x60 => self.bit4b(),
            0x61 => self.bit4c(),
            0x62 => self.bit4d(),
            0x63 => self.bit4e(),
            0x64 => self.bit4h(),
            0x65 => self.bit4l(),
            0x66 => self.bit4hl(),
            0x67 => self.bit4a(),
            0x68 => self.bit5b(),
            0x69 => self.bit5c(),
            0x6a => self.bit5d(),
            0x6b => self.bit5e(),
            0x6c => self.bit5h(),
            0x6d => self.bit5l(),
            0x6e => self.bit5hl(),
            0x6f => self.bit5a(),
            0x70 => self.bit6b(),
            0x71 => self.bit6c(),
            0x72 => self.bit6d(),
            0x73 => self.bit6e(),
            0x74 => self.bit6h(),
            0x75 => self.bit6l(),
            0x76 => self.bit6hl(),
            0x77 => self.bit6a(),
            0x78 => self.bit7b(),
            0x79 => self.bit7c(),
            0x7a => self.bit7d(),
            0x7b => self.bit7e(),
            0x7c => self.bit7h(),
            0x7d => self.bit7l(),
            0x7e => self.bit7hl(),
            0x7f => self.bit7a(),
            0x80 => self.res0b(),
            0x81 => self.res0c(),
            0x82 => self.res0d(),
            0x83 => self.res0e(),
            0x84 => self.res0h(),
            0x85 => self.res0l(),
            0x86 => self.res0hl(),
            0x87 => self.res0a(),
            0x88 => self.res1b(),
            0x89 => self.res1c(),
            0x8a => self.res1d(),
            0x8b => self.res1e(),
            0x8c => self.res1h(),
            0x8d => self.res1l(),
            0x8e => self.res1hl(),
            0x8f => self.res1a(),
            0x90 => self.res2b(),
            0x91 => self.res2c(),
            0x92 => self.res2d(),
            0x93 => self.res2e(),
            0x94 => self.res2h(),
            0x95 => self.res2l(),
            0x96 => self.res2hl(),
            0x97 => self.res2a(),
            0x98 => self.res3b(),
            0x99 => self.res3c(),
            0x9a => self.res3d(),
            0x9b => self.res3e(),
            0x9c => self.res3h(),
            0x9d => self.res3l(),
            0x9e => self.res3hl(),
            0x9f => self.res3a(),
            0xa0 => self.res4b(),
            0xa1 => self.res4c(),
            0xa2 => self.res4d(),
            0xa3 => self.res4e(),
            0xa4 => self.res4h(),
            0xa5 => self.res4l(),
            0xa6 => self.res4hl(),
            0xa7 => self.res4a(),
            0xa8 => self.res5b(),
            0xa9 => self.res5c(),
            0xaa => self.res5d(),
            0xab => self.res5e(),
            0xac => self.res5h(),
            0xad => self.res5l(),
            0xae => self.res5hl(),
            0xaf => self.res5a(),
            0xb0 => self.res6b(),
            0xb1 => self.res6c(),
            0xb2 => self.res6d(),
            0xb3 => self.res6e(),
            0xb4 => self.res6h(),
            0xb5 => self.res6l(),
            0xb6 => self.res6hl(),
            0xb7 => self.res6a(),
            0xb8 => self.res7b(),
            0xb9 => self.res7c(),
            0xba => self.res7d(),
            0xbb => self.res7e(),
            0xbc => self.res7h(),
            0xbd => self.res7l(),
            0xbe => self.res7hl(),
            0xbf => self.res7a(),
            0xc0 => self.set0b(),
            0xc1 => self.set0c(),
            0xc2 => self.set0d(),
            0xc3 => self.set0e(),
            0xc4 => self.set0h(),
            0xc5 => self.set0l(),
            0xc6 => self.set0hl(),
            0xc7 => self.set0a(),
            0xc8 => self.set1b(),
            0xc9 => self.set1c(),
            0xca => self.set1d(),
            0xcb => self.set1e(),
            0xcc => self.set1h(),
            0xcd => self.set1l(),
            0xce => self.set1hl(),
            0xcf => self.set1a(),
            0xd0 => self.set2b(),
            0xd1 => self.set2c(),
            0xd2 => self.set2d(),
            0xd3 => self.set2e(),
            0xd4 => self.set2h(),
            0xd5 => self.set2l(),
            0xd6 => self.set2hl(),
            0xd7 => self.set2a(),
            0xd8 => self.set3b(),
            0xd9 => self.set3c(),
            0xda => self.set3d(),
            0xdb => self.set3e(),
            0xdc => self.set3h(),
            0xdd => self.set3l(),
            0xde => self.set3hl(),
            0xdf => self.set3a(),
            0xe0 => self.set4b(),
            0xe1 => self.set4c(),
            0xe2 => self.set4d(),
            0xe3 => self.set4e(),
            0xe4 => self.set4h(),
            0xe5 => self.set4l(),
            0xe6 => self.set4hl(),
            0xe7 => self.set4a(),
            0xe8 => self.set5b(),
            0xe9 => self.set5c(),
            0xea => self.set5d(),
            0xeb => self.set5e(),
            0xec => self.set5h(),
            0xed => self.set5l(),
            0xee => self.set5hl(),
            0xef => self.set5a(),
            0xf0 => self.set6b(),
            0xf1 => self.set6c(),
            0xf2 => self.set6d(),
            0xf3 => self.set6e(),
            0xf4 => self.set6h(),
            0xf5 => self.set6l(),
            0xf6 => self.set6hl(),
            0xf7 => self.set6a(),
            0xf8 => self.set7b(),
            0xf9 => self.set7c(),
            0xfa => self.set7d(),
            0xfb => self.set7e(),
            0xfc => self.set7h(),
            0xfd => self.set7l(),
            0xfe => self.set7hl(),
            0xff => self.set7a(),
        }
    }

    fn nop(&mut self) {
        self.cycles += 1;
    }

    fn ldbca16(&mut self) {
        self.reg_c = self.read();
        self.reg_b = self.read();
        self.cycles += 3;
    }

    fn ldhbca(&mut self) {
        self.write_byte(((self.reg_b as u16) << 8 | self.reg_c as u16 )as usize, self.reg_a);
        self.cycles += 2;
    }

    fn incbc(&mut self) {
        let data = ((self.reg_b as u16) << 8 | self.reg_c as u16).wrapping_add(1);
        self.reg_b = (data >> 8) as u8;
        self.reg_c = data as u8;
        self.tick_timer_4t();
        self.cycles += 2;
    }

    fn incb(&mut self) {
        self.set_flag_bit(5, (self.reg_b & 0x0F) + 1 > 0x0F);
        self.reg_b = self.reg_b.wrapping_add(1);
        self.set_flag_bit(7, self.reg_b==0);
        self.set_flag_bit(6, false);
        self.cycles += 1;
    }

    fn decb(&mut self) {
        self.set_flag_bit(5, (self.reg_b & 0x0F) == 0x00);
        self.reg_b = self.dec(self.reg_b);
        self.set_flag_bit(7, self.reg_b==0);
        self.set_flag_bit(6, true);
        self.cycles += 1;
    }

    fn ldb(&mut self) {
        self.reg_b = self.read();
        self.cycles += 2;
    }

    fn rlca(&mut self) {
        self.reg_a = self.cb_rlc(self.reg_a);
        self.set_flag_bit(7, false);
        self.cycles += 1;
    }

    fn lda16sp(&mut self) {
        let low = self.read() as u16;
        let high = self.read() as u16;
        self.write_byte((high << 8 | low) as usize, self.stackpointer as u8);
        self.write_byte(((high << 8 | low) + 1) as usize, (self.stackpointer >> 8) as u8);
        self.cycles += 5;
    }

    fn addhlbc(&mut self) {
        let hl = self.hl();
        let bc = self.bc();
        let result = hl as u32 + bc as u32;
        self.set_flag_bit(5, (hl & 0xfff) + (bc & 0xfff) > 0xfff);
        self.set_flag_bit(6, false);
        self.set_flag_bit(4, result > 0xffff);
        self.reg_h = (result >> 8) as u8;
        self.reg_l = result as u8;
        self.tick_timer_4t();
        self.cycles += 2;
    }

    fn ldabc(&mut self) {
        self.reg_a = self.read_byte(((self.reg_b as u16) << 8 | self.reg_c as u16) as usize);
        self.cycles += 2;
    }

    fn decbc(&mut self) {
        let bc = self.bc().wrapping_sub(1);
        self.reg_b = (bc >> 8) as u8;
        self.reg_c = bc as u8;
        self.tick_timer_4t();
        self.cycles += 2;
    }

    fn incc(&mut self) {
        self.set_flag_bit(5, (self.reg_c & 0x0F) + 1 > 0x0F);
        self.reg_c = self.reg_c.wrapping_add(1);
        self.set_flag_bit(7, self.reg_c==0);
        self.set_flag_bit(6, false);
        self.cycles += 1;
    }

    fn decc(&mut self) {
        self.set_flag_bit(5, (self.reg_c & 0x0F) == 0x00);
        self.reg_c = self.dec(self.reg_c);
        (*self).set_flag_bit(7, self.reg_c==0);
        self.set_flag_bit(6, true);
        self.cycles += 1;
    }

    fn ldc(&mut self) {
        self.reg_c = self.read();
        self.cycles += 2;
    }

    fn rrca(&mut self) {
        self.reg_a = self.cb_rrc(self.reg_a);
        self.set_flag_bit(7, false);
        self.cycles += 1;
    }

    fn stop(&mut self) {
        self.read();
        if self.is_cgb && (self.key1 & 0x01) != 0 {
            self.double_speed = !self.double_speed;
            self.key1 &= 0xFE;
            self.cycles += 2050; // Switch delay
        } else {
            self.sys_counter = 0;
            self.memory[0xFF04] = 0;
            self.check_timer_falling_edge();
        }
        self.cycles += 1;
    }

    fn ldded16(&mut self) {
        self.reg_e = self.read();
        self.reg_d = self.read();
        self.cycles += 3;
    }

    fn ldhdea(&mut self) {
        self.write_byte((self.de()) as usize, self.reg_a);
        self.cycles += 2;
    }

    fn incde(&mut self) {
        let de = self.de().wrapping_add(1);
        self.reg_d = (de >> 8) as u8;
        self.reg_e = de as u8;
        self.tick_timer_4t();
        self.cycles += 2;
    }

    fn incd(&mut self) {
        self.set_flag_bit(5, (self.reg_d & 0x0F) + 1 > 0x0F);
        self.reg_d = self.reg_d.wrapping_add(1);
        self.set_flag_bit(7, self.reg_d==0);
        self.set_flag_bit(6, false);
        self.cycles += 1;
    }

    fn decd(&mut self) {
        self.set_flag_bit(5, (self.reg_d & 0x0F) == 0x00);
        self.reg_d = self.dec(self.reg_d);
        self.set_flag_bit(7, self.reg_d==0);
        self.set_flag_bit(6, true);
        self.cycles += 1;
    }

    fn ldd(&mut self) {
        self.reg_d = self.read();
        self.cycles += 2;
    }

    fn rla(&mut self) {
        let carry = self.get_flag_bit(4);
        self.set_flag_bit(4, self.reg_a & 0x80 == 0x80);
        self.reg_a = (self.reg_a << 1) | carry as u8;
        self.set_flag_bit(5, false);
        self.set_flag_bit(6, false);
        self.set_flag_bit(7, false);
        self.cycles += 1;
    }

    fn jr(&mut self) {
        let offset = self.read() as i8;
        self.tick_timer_4t();
        self.program_counter = (self.program_counter as i32 + offset as i32) as u16;
        self.cycles += 3;
    }

    fn addhlde(&mut self) {
        let hl = self.hl();
        let de = self.de();
        let result = hl as u32 + de as u32;
        self.set_flag_bit(5, (hl & 0xfff) + (de & 0xfff) > 0xfff);
        self.set_flag_bit(6, false);
        self.set_flag_bit(4, result > 0xffff);
        self.reg_h = (result >> 8) as u8;
        self.reg_l = result as u8;
        self.tick_timer_4t();
        self.cycles += 2;
    }

    fn ldade(&mut self) {
        self.reg_a = self.read_byte((self.de()) as usize);
        self.cycles += 2;
    }

    fn decde(&mut self) {
        let de = self.de();
        let result = de.wrapping_sub(1);
        self.reg_d = (result >> 8) as u8;
        self.reg_e = result as u8;
        self.tick_timer_4t();
        self.cycles += 2;
    }

    fn ince(&mut self) {
        self.set_flag_bit(5, (self.reg_e & 0x0F) + 1 > 0x0F);
        self.reg_e = self.reg_e.wrapping_add(1);
        self.set_flag_bit(7, self.reg_e==0);
        self.set_flag_bit(6, false);
        self.cycles += 1;
    }

    fn dece(&mut self) {
        self.set_flag_bit(5, (self.reg_e & 0x0F) == 0x00);
        self.reg_e = self.dec(self.reg_e);
        self.set_flag_bit(7, self.reg_e==0);
        self.set_flag_bit(6, true);
        self.cycles += 1;
    }

    fn lde(&mut self) {
        self.reg_e = self.read();
        self.cycles += 2;
    }

    fn rra(&mut self) {
        let carry = self.get_flag_bit(4);
        self.set_flag_bit(4, self.reg_a & 0x01 == 0x01);
        self.reg_a = (self.reg_a >> 1) | ((carry as u8) << 7);
        self.set_flag_bit(5, false);
        self.set_flag_bit(6, false);
        self.set_flag_bit(7, false);
        self.cycles += 1;
    }

    fn jrnz(&mut self) {
        let offset = self.read() as i8;
        if !self.get_flag_bit(7) {
            self.tick_timer_4t();
            self.program_counter = (self.program_counter as i32 + offset as i32) as u16;
            self.cycles += 3;
        } else {
            self.cycles += 2;
        }
    }

    fn ldhl16(&mut self) {
        self.reg_l = self.read();
        self.reg_h = self.read();
        self.cycles += 3;
    }

    fn ldhlplusa(&mut self) {
        self.write_byte(self.hl() as usize, self.reg_a);
        let mut hl = self.hl();
        hl = hl.wrapping_add(1);
        self.reg_h = (hl >> 8) as u8;
        self.reg_l = hl as u8;
        self.cycles += 2;
    }

    fn inchl(&mut self) {
        let mut hl = self.hl();
        hl = hl.wrapping_add(1);
        self.reg_h = (hl >> 8) as u8;
        self.reg_l = hl as u8;
        self.tick_timer_4t();
        self.cycles += 2;
    }

    fn inch(&mut self) {
        self.set_flag_bit(5, (self.reg_h & 0x0F) + 1 > 0x0F);
        self.reg_h = self.reg_h.wrapping_add(1);
        self.set_flag_bit(7, self.reg_h==0);
        self.set_flag_bit(6, false);
        self.cycles += 1;
    }

    fn dech(&mut self) {
        self.reg_h = self.dec(self.reg_h);
        self.cycles += 1;
    }

    fn ldha8(&mut self) {
        self.reg_h = self.read();
        self.cycles += 2;
    }

    fn daa(&mut self) {
        let nflag = self.get_flag_bit(6);
        let cflag = self.get_flag_bit(4);
        let hflag = self.get_flag_bit(5);
        let mut a = self.reg_a;
        if !nflag {
            if cflag || a > 0x99 {
                a = a.wrapping_add(0x60);
                self.set_flag_bit(4, true);
            }
            if hflag || a & 0x0f > 0x09 {
                a = a.wrapping_add(0x06);
            }
        } else {
            if cflag {
                a = a.wrapping_sub(0x60);
            }
            if hflag {
                a = a.wrapping_sub(0x06);
            }
        }
        self.set_flag_bit(5, false);
        self.set_flag_bit(7, a == 0);
        self.reg_a = a;
        self.cycles += 1;
    }

    fn jrz(&mut self) {
        let offset = self.read() as i8;
        if self.get_flag_bit(7) {
            self.tick_timer_4t();
            self.program_counter = (self.program_counter as i32 + offset as i32) as u16;
            self.cycles += 3;
        } else {
            self.cycles += 2;
        }
    }

    fn addhlhl(&mut self) {
        let hl = self.hl();
        let result = hl as u32 + hl as u32;
        self.set_flag_bit(5, (hl & 0xfff) + (hl & 0xfff) > 0xfff);
        self.set_flag_bit(6, false);
        self.set_flag_bit(4, result > 0xffff);
        self.reg_h = (result >> 8) as u8;
        self.reg_l = result as u8;
        self.tick_timer_4t();
        self.cycles += 2;
    }

    fn ldahlplus(&mut self) {
        self.reg_a = self.read_byte(self.hl() as usize);
        let hl = self.hl().wrapping_add(1);
        self.reg_h = (hl >> 8) as u8;
        self.reg_l = hl as u8;
        self.cycles += 2;
    }

    fn dechl(&mut self) {
        let mut hl = self.hl();
        hl = hl.wrapping_sub(1);
        self.reg_h = (hl >> 8) as u8;
        self.reg_l = hl as u8;
        self.tick_timer_4t();
        self.cycles += 2;
    }

    fn incl(&mut self) {
        self.set_flag_bit(5, (self.reg_l & 0x0F) + 1 > 0x0F);
        self.reg_l = self.reg_l.wrapping_add(1);
        self.set_flag_bit(7, self.reg_l==0);
        self.set_flag_bit(6, false);
        self.cycles += 1;
    }

    fn decl(&mut self) {
        self.reg_l = self.dec(self.reg_l);
        self.cycles += 1;
    }

    fn ldl(&mut self) {
        self.reg_l = self.read();
        self.cycles += 2;
    }

    fn cplm(&mut self) {
        self.reg_a = !self.reg_a;
        self.set_flag_bit(5, true);
        self.set_flag_bit(6, true);
        self.cycles += 1;
    }

    fn jrncr8(&mut self) {
        let offset = self.read() as i8;
        if !self.get_flag_bit(4) {
            self.tick_timer_4t();
            self.program_counter = (self.program_counter as i32 + offset as i32) as u16;
            self.cycles += 3;
        } else {
            self.cycles += 2;
        }
    }

    fn ldsp16(&mut self) {
        self.stackpointer = self.read16();
        self.cycles += 3;
    }

    fn ldhlmina(&mut self) {
        let mut hl = self.hl();
        self.write_byte(hl as usize, self.reg_a);
        hl = hl.wrapping_sub(1);
        self.reg_h = (hl >> 8) as u8;
        self.reg_l = hl as u8;
        self.cycles += 2;
    }

    fn incsp(&mut self) {
        self.stackpointer = self.stackpointer.wrapping_add(1);
        self.tick_timer_4t();
        self.cycles += 2;
    }

    fn inchhl(&mut self) {
        let hl = self.hl();
        let mut data = self.read_byte(hl as usize);
        self.set_flag_bit(5, ((data & 0x0F) + 1) > 0x0F);
        data = data.wrapping_add(1);
        self.set_flag_bit(7, data==0);
        self.set_flag_bit(6, false);
        self.write_byte(hl as usize, data);
        self.cycles += 3;
    }

    fn dechhl(&mut self) {
        let v = self.read_byte(self.hl() as usize); let val = self.dec(v);
        self.write_byte(self.hl() as usize, val);
        self.cycles += 3;
    }

    fn ldhla8(&mut self) {
        let val = self.read();
        self.write_byte(self.hl() as usize, val);
        self.cycles += 3;
    }

    fn scf(&mut self) {
        self.set_flag_bit(4, true);
        self.set_flag_bit(5, false);
        self.set_flag_bit(6, false);
        self.cycles += 1;
    }

    fn jrcr8(&mut self) {
        let offset = self.read() as i8;
        if self.get_flag_bit(4) {
            self.tick_timer_4t();
            self.program_counter = (self.program_counter as i32 + offset as i32) as u16;
            self.cycles += 3;
        } else {
            self.cycles += 2;
        }
    }

    fn addhlsp(&mut self) {
        let hl = self.hl();
        let sp = self.stackpointer;
        let result = hl as u32 + sp as u32;

        self.set_flag_bit(5, (hl & 0x0FFF) + (sp & 0x0FFF) > 0x0FFF);
        self.set_flag_bit(6, false);
        self.set_flag_bit(4, result > 0xFFFF);

        self.reg_h = (result >> 8) as u8;
        self.reg_l = result as u8;
        self.tick_timer_4t();
        self.cycles += 2;
    }

    fn ldahlmin(&mut self) {
        self.reg_a = self.read_byte(self.hl() as usize);
        let hl = self.hl().wrapping_sub(1);
        self.reg_h = (hl >> 8) as u8;
        self.reg_l = hl as u8;
        self.cycles += 2;
    }

    fn decsp(&mut self) {
        self.stackpointer = self.stackpointer.wrapping_sub(1);
        self.tick_timer_4t();
        self.cycles += 2;
    }

    fn inca(&mut self) {
        self.set_flag_bit(5, (self.reg_a & 0x0F) + 1 > 0x0F);
        self.reg_a = self.reg_a.wrapping_add(1);
        self.set_flag_bit(7, self.reg_a==0);
        self.set_flag_bit(6, false);
        self.cycles += 1;
    }

    fn dec(&mut self,mut val:u8) -> u8 {
        self.set_flag_bit(5, (val & 0x0F) < 0x01);
        val = val.wrapping_sub(1);
        self.set_flag_bit(7, val==0);
        self.set_flag_bit(6, true);
        val
    }

    fn deca(&mut self) {
        self.set_flag_bit(5, (self.reg_a & 0x0F) == 0x00);
        self.reg_a = self.dec(self.reg_a);
        self.set_flag_bit(7, self.reg_a==0);
        self.set_flag_bit(6, true);
        self.cycles += 1;
    }

    fn lda(&mut self) {
        self.reg_a = self.read();
        self.cycles += 2;
    }

    fn ccf(&mut self) {
        self.set_flag_bit(4, !self.get_flag_bit(4));
        self.set_flag_bit(5, false);
        self.set_flag_bit(6, false);
        self.cycles += 1;
    }

    fn ldbb(&mut self) {
        self.cycles += 1;
    }

    fn ldbc(&mut self) {
        self.reg_b = self.reg_c;
        self.cycles += 1;
    }

    fn ldbd(&mut self) {
        self.reg_b = self.reg_d;
        self.cycles += 1;
    }

    fn ldbe(&mut self) {
        self.reg_b = self.reg_e;
        self.cycles += 1;
    }

    fn ldbh(&mut self) {
        self.reg_b = self.reg_h;
        self.cycles += 1;
    }

    fn ldbl(&mut self) {
        self.reg_b = self.reg_l;
        self.cycles += 1;
    }

    fn ldbhl(&mut self) {
        self.reg_b = self.read_byte(self.hl() as usize);
        self.cycles += 2;
    }

    fn ldba(&mut self) {
        self.reg_b = self.reg_a;
        self.cycles += 1;
    }

    fn ldcb(&mut self) {
        self.reg_c = self.reg_b;
        self.cycles += 1;
    }

    fn ldcc(&mut self) {
        self.cycles += 1;
    }

    fn ldcd(&mut self) {
        self.reg_c = self.reg_d;
        self.cycles += 1;
    }

    fn ldce(&mut self) {
        self.reg_c = self.reg_e;
        self.cycles += 1;
    }

    fn ldch(&mut self) {
        self.reg_c = self.reg_h;
        self.cycles += 1;
    }

    fn ldcl(&mut self) {
        self.reg_c = self.reg_l;
        self.cycles += 1;
    }

    fn ldchl(&mut self) {
        self.reg_c = self.read_byte(self.hl() as usize);
        self.cycles += 2;
    }

    fn ldca(&mut self) {
        self.reg_c = self.reg_a;
        self.cycles += 1;
    }

    fn lddb(&mut self) {
        self.reg_d = self.reg_b;
        self.cycles += 1;
    }

    fn lddc(&mut self) {
        self.reg_d = self.reg_c;
        self.cycles += 1;
    }

    fn lddd(&mut self) {
        self.cycles += 1;
    }

    fn ldde(&mut self) {
        self.reg_d = self.reg_e;
        self.cycles += 1;
    }

    fn lddh(&mut self) {
        self.reg_d = self.reg_h;
        self.cycles += 1;
    }

    fn lddl(&mut self) {
        self.reg_d = self.reg_l;
        self.cycles += 1;
    }

    fn lddhl(&mut self) {
        self.reg_d = self.read_byte(self.hl() as usize);
        self.cycles += 2;
    }

    fn ldda(&mut self) {
        self.reg_d = self.reg_a;
        self.cycles += 1;
    }

    fn ldeb(&mut self) {
        self.reg_e = self.reg_b;
        self.cycles += 1;
    }

    fn ldec(&mut self) {
        self.reg_e = self.reg_c;
        self.cycles += 1;
    }

    fn lded(&mut self) {
        self.reg_e = self.reg_d;
        self.cycles += 1;
    }

    fn ldee(&mut self) {
        self.cycles += 1;
    }

    fn ldeh(&mut self) {
        self.reg_e = self.reg_h;
        self.cycles += 1;
    }

    fn ldel(&mut self) {
        self.reg_e = self.reg_l;
        self.cycles += 1;
    }

    fn ldehl(&mut self) {
        self.reg_e = self.read_byte(self.hl() as usize);
        self.cycles += 2;
    }

    fn ldea(&mut self) {
        self.reg_e = self.reg_a;
        self.cycles += 1;
    }

    fn ldhb(&mut self) {
        self.reg_h = self.reg_b;
        self.cycles += 1;
    }

    fn ldhc(&mut self) {
        self.reg_h = self.reg_c;
        self.cycles += 1;
    }

    fn ldhd(&mut self) {
        self.reg_h = self.reg_d;
        self.cycles += 1;
    }

    fn ldhe(&mut self) {
        self.reg_h = self.reg_e;
        self.cycles += 1;
    }

    fn ldhh(&mut self) {
        self.cycles += 1;
    }

    fn ldhl(&mut self) {
        self.reg_h = self.reg_l;
        self.cycles += 1;
    }

    fn ldhhl(&mut self) {
        self.reg_h = self.read_byte(self.hl() as usize);
        self.cycles += 2;
    }

    fn ldha(&mut self) {
        self.reg_h = self.reg_a;
        self.cycles += 1;
    }

    fn ldlb(&mut self) {
        self.reg_l = self.reg_b;
        self.cycles += 1;
    }

    fn ldlc(&mut self) {
        self.reg_l = self.reg_c;
        self.cycles += 1;
    }

    fn ldld(&mut self) {
        self.reg_l = self.reg_d;
        self.cycles += 1;
    }

    fn ldle(&mut self) {
        self.reg_l = self.reg_e;
        self.cycles += 1;
    }

    fn ldlh(&mut self) {
        self.reg_l = self.reg_h;
        self.cycles += 1;
    }

    fn ldll(&mut self) {
        self.cycles += 1;
    }

    fn ldlhl(&mut self) {
        self.reg_l = self.read_byte(self.hl() as usize);
        self.cycles += 2;
    }

    fn ldla(&mut self) {
        self.reg_l = self.reg_a;
        self.cycles += 1;
    }

    fn ldhlb(&mut self) {
        self.write_byte(self.hl() as usize, self.reg_b);
        self.cycles += 2;
    }

    fn ldhlc(&mut self) {
        self.write_byte(self.hl() as usize, self.reg_c);
        self.cycles += 2;
    }

    fn ldhld(&mut self) {
        self.write_byte(self.hl() as usize, self.reg_d);
        self.cycles += 2;
    }

    fn ldhle(&mut self) {
        self.write_byte(self.hl() as usize, self.reg_e);
        self.cycles += 2;
    }

    fn ldhlh(&mut self) {
        self.write_byte(self.hl() as usize, self.reg_h);
        self.cycles += 2;
    }

    fn ldhll(&mut self) {
        self.write_byte(self.hl() as usize, self.reg_l);
        self.cycles += 2;
    }
    fn halt(&mut self) {
        let ie = self.memory[0xFFFF];
        let iflag = self.memory[0xFF0F];

        if (ie & iflag & 0x1F) != 0 {
            if !self.interrupt_master_enable {
                self.halt_bug = true;
            }
            self.cycles += 1;
            return;
        }

        self.halt = true;
        self.cycles += 1;
    }
    fn ldhla(&mut self) {
        self.write_byte((self.hl() )as usize, self.reg_a);
        self.cycles += 2;
    }

    fn ldab(&mut self) {
        self.reg_a = self.reg_b;
        self.cycles += 1;
    }

    fn ldac(&mut self) {
        self.reg_a = self.reg_c;
        self.cycles += 1;
    }

    fn ldad(&mut self) {
        self.reg_a = self.reg_d;
        self.cycles += 1;
    }

    fn ldae(&mut self) {
        self.reg_a = self.reg_e;
        self.cycles += 1;
    }

    fn ldah(&mut self) {
        self.reg_a = self.reg_h;
        self.cycles += 1;
    }

    fn ldal(&mut self) {
        self.reg_a = self.reg_l;
        self.cycles += 1;
    }

    fn ldahl(&mut self) {
        self.reg_a = self.read_byte(self.hl() as usize);
        self.cycles += 2;
    }

    fn ldaa(&mut self) {
        self.cycles += 1;
    }

    fn cb_addar8(&mut self, val: u8) {
        let tmp = self.reg_a as i16 + val as i16;
        self.set_flag_bit(7, (tmp & 0xFF) == 0);
        self.set_flag_bit(6, false);
        self.set_flag_bit(5, (self.reg_a & 0x0F) + (val & 0x0F) > 0x0F);
        self.set_flag_bit(4, tmp > 0xFF);
        self.reg_a = tmp as u8;
    }

    fn addab(&mut self) {
        self.cb_addar8(self.reg_b);
        self.cycles += 1;
    }

    fn addac(&mut self) {
        self.cb_addar8(self.reg_c);
        self.cycles += 1;
    }

    fn addad(&mut self) {
        self.cb_addar8(self.reg_d);
        self.cycles += 1;
    }

    fn addae(&mut self) {
        self.cb_addar8(self.reg_e);
        self.cycles += 1;
    }

    fn addah(&mut self) {
        self.cb_addar8(self.reg_h);
        self.cycles += 1;
    }

    fn addal(&mut self) {
        self.cb_addar8(self.reg_l);
        self.cycles += 1;
    }

    fn addahl(&mut self) {
        let v = self.read_byte(self.hl() as usize); self.cb_addar8(v);
        self.cycles += 2;
    }

    fn addaa(&mut self) {
        self.cb_addar8(self.reg_a);
        self.cycles += 1;
    }

    fn cb_adcar8(&mut self, val: u8) {
        let carry = self.get_flag_bit(4) as u8;
        let tmp = self.reg_a as i16 + val as i16 + carry as i16;
        self.set_flag_bit(7, (tmp & 0xFF) == 0);
        self.set_flag_bit(6, false);
        self.set_flag_bit(5, ((self.reg_a & 0x0F) + (val & 0x0F) + carry) > 0x0F);
        self.set_flag_bit(4, tmp > 0xFF);
        self.reg_a = tmp as u8;
    }

    fn adcab(&mut self) {
        self.cb_adcar8(self.reg_b);
        self.cycles += 1;
    }

    fn adcac(&mut self) {
        self.cb_adcar8(self.reg_c);
        self.cycles += 1;
    }

    fn adcad(&mut self) {
        self.cb_adcar8(self.reg_d);
        self.cycles += 1;
    }

    fn adcae(&mut self) {
        self.cb_adcar8(self.reg_e);
        self.cycles += 1;
    }

    fn adcah(&mut self) {
        self.cb_adcar8(self.reg_h);
        self.cycles += 1;
    }

    fn adcal(&mut self) {
        self.cb_adcar8(self.reg_l);
        self.cycles += 1;
    }

    fn adcahl(&mut self) {
        let v = self.read_byte(self.hl() as usize); self.cb_adcar8(v);
        self.cycles += 2;
    }

    fn adcaa(&mut self) {
        self.cb_adcar8(self.reg_a);
        self.cycles += 1;
    }

    fn sub(&mut self, b: u8) {
        let tmp = self.reg_a as i16 - b as i16;
        self.set_flag_bit(7, (tmp & 0xFF) == 0);
        self.set_flag_bit(6, true);
        self.set_flag_bit(5, (self.reg_a & 0x0F) < (b & 0x0F));
        self.set_flag_bit(4, tmp < 0);
        self.reg_a = tmp as u8;
    }

    fn subb(&mut self) {
        self.sub(self.reg_b);
        self.cycles += 1;
    }

    fn subc(&mut self) {
        self.sub(self.reg_c);
        self.cycles += 1;
    }

    fn subd(&mut self) {
        self.sub(self.reg_d);
        self.cycles += 1;
    }

    fn sube(&mut self) {
        self.sub(self.reg_e);
        self.cycles += 1;
    }

    fn subh(&mut self) {
        self.sub(self.reg_h);
        self.cycles += 1;
    }

    fn subl(&mut self) {
        self.sub(self.reg_l);
        self.cycles += 1;
    }

    fn subhl(&mut self) {
        let v = self.read_byte(self.hl() as usize); self.sub(v);
        self.cycles += 2;
    }

    fn suba(&mut self) {
        self.sub(self.reg_a);
        self.cycles += 1;
    }

    fn subca(&mut self, b: u8) {
        let carry = self.get_flag_bit(4) as i16;
        let tmp = self.reg_a as i16 - b as i16 - carry;
        self.set_flag_bit(7, (tmp & 0xFF) == 0);
        self.set_flag_bit(6, true);
        self.set_flag_bit(5, ((self.reg_a & 0x0F) as i16 - (b & 0x0F) as i16 - carry) < 0);
        self.set_flag_bit(4, tmp < 0);
        self.reg_a = tmp as u8;
    }

    fn sbcab(&mut self) {
        self.subca(self.reg_b);
        self.cycles += 1;
    }

    fn sbcac(&mut self) {
        self.subca(self.reg_c);
        self.cycles += 1;
    }

    fn sbcad(&mut self) {
        self.subca(self.reg_d);
        self.cycles += 1;
    }

    fn sbcae(&mut self) {
        self.subca(self.reg_e);
        self.cycles += 1;
    }

    fn sbcah(&mut self) {
        self.subca(self.reg_h);
        self.cycles += 1;
    }

    fn sbcal(&mut self) {
        self.subca(self.reg_l);
        self.cycles += 1;
    }

    fn sbcahl(&mut self) {
        let v = self.read_byte(self.hl() as usize); self.subca(v);
        self.cycles += 2;
    }

    fn sbcaa(&mut self) {
        self.subca(self.reg_a);
        self.cycles += 1;
    }

    fn and(&mut self, b: u8) {
        self.reg_a &= b;
        self.set_flag_bit(7, self.reg_a == 0);
        self.set_flag_bit(4, false);
        self.set_flag_bit(5, true);
        self.set_flag_bit(6, false);
    }

    fn andb(&mut self) {
        self.and(self.reg_b);
        self.cycles += 1;
    }

    fn andc(&mut self) {
        self.and(self.reg_c);
        self.cycles += 1;
    }

    fn andd(&mut self) {
        self.and(self.reg_d);
        self.cycles += 1;
    }

    fn ande(&mut self) {
        self.and(self.reg_e);
        self.cycles += 1;
    }

    fn andh(&mut self) {
        self.and(self.reg_h);
        self.cycles += 1;
    }

    fn andl(&mut self) {
        self.and(self.reg_l);
        self.cycles += 1;
    }

    fn andhl(&mut self) {
        let v = self.read_byte(self.hl() as usize); self.and(v);
        self.cycles += 2;
    }

    fn anda(&mut self) {
        self.and(self.reg_a);
        self.cycles += 1;
    }

    fn xor(&mut self, b: u8) {
        self.reg_a ^= b;
        self.set_flag_bit(7, self.reg_a == 0);
        self.set_flag_bit(4, false);
        self.set_flag_bit(5, false);
        self.set_flag_bit(6, false);
    }

    fn xorb(&mut self) {
        self.xor(self.reg_b);
        self.cycles += 1;
    }

    fn xorc(&mut self) {
        self.xor(self.reg_c);
        self.cycles += 1;
    }

    fn xord(&mut self) {
        self.xor(self.reg_d);
        self.cycles += 1;
    }

    fn xore(&mut self) {
        self.xor(self.reg_e);
        self.cycles += 1;
    }

    fn xorh(&mut self) {
        self.xor(self.reg_h);
        self.cycles += 1;
    }

    fn xorl(&mut self) {
        self.xor(self.reg_l);
        self.cycles += 1;
    }

    fn xorhl(&mut self) {
        let v = self.read_byte(self.hl() as usize); self.xor(v);
        self.cycles += 2;
    }

    fn xora(&mut self) {
        self.xor(self.reg_a);
        self.cycles += 1;
    }

    fn or(&mut self, b: u8) {
        self.reg_a |= b;
        self.set_flag_bit(7, self.reg_a == 0);
        self.set_flag_bit(4, false);
        self.set_flag_bit(5, false);
        self.set_flag_bit(6, false);
    }

    fn orb(&mut self) {
        self.or(self.reg_b);
        self.cycles += 1;
    }

    fn orc(&mut self) {
        self.or(self.reg_c);
        self.cycles += 1;
    }

    fn ord(&mut self) {
        self.or(self.reg_d);
        self.cycles += 1;
    }

    fn ore(&mut self) {
        self.or(self.reg_e);
        self.cycles += 1;
    }

    fn orh(&mut self) {
        self.or(self.reg_h);
        self.cycles += 1;
    }

    fn orl(&mut self) {
        self.or(self.reg_l);
        self.cycles += 1;
    }

    fn orhl(&mut self) {
        let v = self.read_byte(self.hl() as usize); self.or(v);
        self.cycles += 2;
    }

    fn ora(&mut self) {
        self.or(self.reg_a);
        self.cycles += 1;
    }

    fn cpr8(&mut self, b: u8) {
        self.set_flag_bit(7, self.reg_a == b);
        self.set_flag_bit(6, true);
        self.set_flag_bit(5, (self.reg_a & 0x0F) < (b & 0x0F));
        self.set_flag_bit(4, self.reg_a < b);
    }

    fn cpb(&mut self) {
        self.cpr8(self.reg_b);
        self.cycles += 1;
    }

    fn cpc(&mut self) {
        self.cpr8(self.reg_c);
        self.cycles += 1;
    }

    fn cpd(&mut self) {
        self.cpr8(self.reg_d);
        self.cycles += 1;
    }

    fn cpe(&mut self) {
        self.cpr8(self.reg_e);
        self.cycles += 1;
    }

    fn cph(&mut self) {
        self.cpr8(self.reg_h);
        self.cycles += 1;
    }

    fn cpl(&mut self) {
        self.cpr8(self.reg_l);
        self.cycles += 1;
    }

    fn cphl(&mut self) {
        let v = self.read_byte(self.hl() as usize); self.cpr8(v);
        self.cycles += 2;
    }

    fn cpa(&mut self) {
        self.cpr8(self.reg_a);
        self.cycles += 1;
    }

    fn retnz(&mut self) {
        self.tick_timer_4t();
        if !self.get_flag_bit(7) {
            self.ret();
            self.cycles += 1;
        } else {
            self.cycles += 2;
        }
    }

    fn popbc(&mut self) {
        self.reg_c = self.read_byte(self.stackpointer as usize);
        self.stackpointer = self.stackpointer.wrapping_add(1);
        self.reg_b = self.read_byte(self.stackpointer as usize);
        self.stackpointer = self.stackpointer.wrapping_add(1);
        self.cycles += 3;
    }

    fn jpnza16(&mut self) {
        let addr = self.read16();
        if !self.get_flag_bit(7) {
            self.tick_timer_4t();
            self.program_counter = addr;
            self.cycles += 4;
        } else {
            self.cycles += 3;
        }
    }

    fn jp(&mut self) {
        self.program_counter = self.read16();
        self.tick_timer_4t();
        self.cycles += 4;
    }

    fn callnza16(&mut self) {
        if !self.get_flag_bit(7) {
            self.calla16();
        } else {
            self.read16();
            self.cycles += 3;
        }
    }

    fn pushbc(&mut self) {
        self.tick_timer_4t();
        self.stackpointer = self.stackpointer.wrapping_sub(1);
        self.write_byte(self.stackpointer as usize, self.reg_b);
        self.stackpointer = self.stackpointer.wrapping_sub(1);
        self.write_byte(self.stackpointer as usize, self.reg_c);
        self.cycles += 4;
    }

    fn adda8(&mut self) {
        let b = self.read();
        self.set_flag_bit(5, (self.reg_a & 0x0F) + (b & 0x0F) > 0x0F);
        self.set_flag_bit(4, self.reg_a as u16 + b as u16 > 0xFF);
        self.reg_a = self.reg_a.wrapping_add(b);
        self.set_flag_bit(7, self.reg_a == 0);
        self.set_flag_bit(6, false);
        self.cycles += 2;
    }

    fn rst00h(&mut self) {
        self.tick_timer_4t();
        self.stackpointer = self.stackpointer.wrapping_sub(1);
        self.write_byte(self.stackpointer as usize, (self.program_counter >> 8) as u8);
        self.stackpointer = self.stackpointer.wrapping_sub(1);
        self.write_byte(self.stackpointer as usize, (self.program_counter) as u8);
        self.program_counter = 0x00;
        self.cycles += 4;
    }

    fn retz(&mut self) {
        self.tick_timer_4t();
        if self.get_flag_bit(7) {
            self.ret();
            self.cycles += 1;
        } else {
            self.cycles += 2;
        }
    }

    fn ret(&mut self) {
        let l = self.read_byte(self.stackpointer as usize);
        self.stackpointer = self.stackpointer.wrapping_add(1);
        let h = self.read_byte(self.stackpointer as usize);
        self.stackpointer = self.stackpointer.wrapping_add(1);
        self.program_counter = (h as u16) << 8 | l as u16;
        self.tick_timer_4t();
        self.cycles += 4;
    }

    fn jpza16(&mut self) {
        let addr = self.read16();
        if self.get_flag_bit(7) {
            self.tick_timer_4t();
            self.program_counter = addr;
            self.cycles += 4;
        } else {
            self.cycles += 3;
        }
    }

    fn callza16(&mut self) {
        if self.get_flag_bit(7) {
            self.calla16();
        } else {
            self.read16();
            self.cycles += 3;
        }
    }

    fn calla16(&mut self) {
        let target = self.read16();
        let return_addr = self.program_counter;
        self.tick_timer_4t();
        self.stackpointer = self.stackpointer.wrapping_sub(1);
        self.write_byte(self.stackpointer as usize, (return_addr >> 8) as u8);
        self.stackpointer = self.stackpointer.wrapping_sub(1);
        self.write_byte(self.stackpointer as usize, return_addr as u8);
        self.program_counter = target;
        self.cycles += 6;
    }

    fn adcaa8(&mut self) {
        let v= self.read();
        let carry = self.get_flag_bit(4) as u16;
        let tmp = self.reg_a as u16 + v as u16 + carry;
        self.set_flag_bit(7, tmp as u8 == 0);
        self.set_flag_bit(6, false);
        self.set_flag_bit(5, (self.reg_a as u16 & 0x0F) + (v as u16 & 0x0F) + carry > 0x0F);
        self.set_flag_bit(4, tmp > 0xFF);
        self.reg_a = tmp as u8;
        self.cycles += 2;
    }

    fn rst08h(&mut self) {
        self.tick_timer_4t();
        self.stackpointer = self.stackpointer.wrapping_sub(1);
        self.write_byte(self.stackpointer as usize, (self.program_counter >> 8) as u8);
        self.stackpointer = self.stackpointer.wrapping_sub(1);
        self.write_byte(self.stackpointer as usize, (self.program_counter) as u8);
        self.program_counter = 0x08;
        self.cycles += 4;
    }

    fn retnc(&mut self) {
        self.tick_timer_4t();
        if !self.get_flag_bit(4) {
            self.ret();
            self.cycles += 1;
        } else {
            self.cycles += 2;
        }
    }

    fn popde(&mut self) {
        self.reg_e = self.read_byte(self.stackpointer as usize);
        self.stackpointer = self.stackpointer.wrapping_add(1);
        self.reg_d = self.read_byte(self.stackpointer as usize);
        self.stackpointer = self.stackpointer.wrapping_add(1);
        self.cycles += 3;
    }

    fn jpnca16(&mut self) {
        let addr = self.read16();
        if !self.get_flag_bit(4) {
            self.tick_timer_4t();
            self.program_counter = addr;
            self.cycles += 4;
        } else {
            self.cycles += 3;
        }
    }

    fn callnca16(&mut self) {
        if !self.get_flag_bit(4) {
            self.calla16();
        } else {
            self.read16();
            self.cycles += 3;
        }
    }

    fn pushde(&mut self) {
        self.tick_timer_4t();
        self.stackpointer = self.stackpointer.wrapping_sub(1);
        self.write_byte(self.stackpointer as usize, self.reg_d);
        self.stackpointer = self.stackpointer.wrapping_sub(1);
        self.write_byte(self.stackpointer as usize, self.reg_e);
        self.cycles += 4;
    }

    fn suba8(&mut self) {
        let b = self.read();
        self.set_flag_bit(7, self.reg_a == b);
        self.set_flag_bit(6, true);
        self.set_flag_bit(5, (self.reg_a & 0x0F) < (b & 0x0F));
        self.set_flag_bit(4, self.reg_a < b);
        self.reg_a = self.reg_a.wrapping_sub(b);
        self.cycles += 2;
    }

    fn rst10h(&mut self) {
        self.tick_timer_4t();
        self.stackpointer = self.stackpointer.wrapping_sub(1);
        self.write_byte(self.stackpointer as usize, (self.program_counter >> 8) as u8);
        self.stackpointer = self.stackpointer.wrapping_sub(1);
        self.write_byte(self.stackpointer as usize, (self.program_counter) as u8);
        self.program_counter = 0x10;
        self.cycles += 4;
    }

    fn retc(&mut self) {
        self.tick_timer_4t();
        if self.get_flag_bit(4) {
            self.ret();
            self.cycles += 1;
        } else {
            self.cycles += 2;
        }
    }

    fn reti(&mut self) {
        self.interrupt_master_enable = true;
        self.ret();
    }

    fn jpca16(&mut self) {
        let addr = self.read16();
        if self.get_flag_bit(4) {
            self.tick_timer_4t();
            self.program_counter = addr;
            self.cycles += 4;
        } else {
            self.cycles += 3;
        }
    }

    fn callca16(&mut self) {
        if self.get_flag_bit(4) {
            self.calla16();
        } else {
            self.read16();
            self.cycles += 3;
        }
    }

    fn sbca8(&mut self) {
        let b = self.read();
        let carry = self.get_flag_bit(4) as i16;
        let tmp = self.reg_a as i16 - b as i16 - carry;
        self.set_flag_bit(7, tmp as u8 == 0);
        self.set_flag_bit(6, true);
        self.set_flag_bit(5, ((self.reg_a & 0x0F) as i16 - (b & 0x0F) as i16 - carry) < 0);
        self.set_flag_bit(4, tmp < 0);
        self.reg_a = tmp as u8;
        self.cycles += 2;
    }

    fn rst18h(&mut self) {
        self.tick_timer_4t();
        self.stackpointer = self.stackpointer.wrapping_sub(1);
        self.write_byte(self.stackpointer as usize, (self.program_counter >> 8) as u8);
        self.stackpointer = self.stackpointer.wrapping_sub(1);
        self.write_byte(self.stackpointer as usize, (self.program_counter) as u8);
        self.program_counter = 0x18;
        self.cycles += 4;
    }

    fn ldha8a(&mut self) {
        let addr = self.read();
        self.write_byte(0xff00+addr as usize, self.reg_a);
        self.cycles += 3;
    }

    fn pophl(&mut self) {
        self.reg_l = self.read_byte(self.stackpointer as usize);
        self.stackpointer = self.stackpointer.wrapping_add(1);
        self.reg_h = self.read_byte(self.stackpointer as usize);
        self.stackpointer = self.stackpointer.wrapping_add(1);
        self.cycles += 3;
    }

    fn ldhca(&mut self) {
        self.write_byte(0xFF00 + self.reg_c as usize, self.reg_a);
        self.cycles += 2;
    }

    fn pushhl(&mut self) {
        self.tick_timer_4t();
        self.stackpointer = self.stackpointer.wrapping_sub(1);
        self.write_byte(self.stackpointer as usize, self.reg_h);
        self.stackpointer = self.stackpointer.wrapping_sub(1);
        self.write_byte(self.stackpointer as usize, self.reg_l);
        self.cycles += 4;
    }

    fn anda8(&mut self) {
        let tmp = self.read();
        self.and(tmp);
        self.cycles += 2;
    }

    fn rst20h(&mut self) {
        self.tick_timer_4t();
        self.stackpointer = self.stackpointer.wrapping_sub(1);
        self.write_byte(self.stackpointer as usize, (self.program_counter >> 8) as u8);
        self.stackpointer = self.stackpointer.wrapping_sub(1);
        self.write_byte(self.stackpointer as usize, (self.program_counter) as u8);
        self.program_counter = 0x20;
        self.cycles += 4;
    }

    fn addspr8(&mut self) {
        let b = self.read();
        let offset = b as i8 as i16 as u16;

        self.set_flag_bit(6, false);
        self.set_flag_bit(7, false);

        self.set_flag_bit(4, (self.stackpointer & 0xFF) + (b as u16) > 0xFF);
        self.set_flag_bit(5, (self.stackpointer & 0xF) + (b as u16 & 0xF) > 0xF);

        self.stackpointer = self.stackpointer.wrapping_add(offset);
        self.tick_timer_4t();
        self.tick_timer_4t();
        self.cycles += 4;
    }

    fn jphl(&mut self) {
        self.program_counter = self.hl();
        self.cycles += 1;
    }

    fn lda16a(&mut self) {
        let address = self.read16() as usize;
        self.write_byte(address, self.reg_a);
        self.cycles += 4;
    }

    fn xora8(&mut self) {
        let tmp = self.read();
        self.xor(tmp);
        self.cycles += 2;
    }

    fn rst28h(&mut self) {
        self.tick_timer_4t();
        self.stackpointer = self.stackpointer.wrapping_sub(1);
        self.write_byte(self.stackpointer as usize, (self.program_counter >> 8) as u8);
        self.stackpointer = self.stackpointer.wrapping_sub(1);
        self.write_byte(self.stackpointer as usize, (self.program_counter) as u8);
        self.program_counter = 0x28;
        self.cycles += 4;
    }

    fn ldhaa8(&mut self) {
        let addr = self.read();
        self.reg_a = self.read_byte(0xff00+addr as usize);
        self.cycles += 3;
    }

    fn popaf(&mut self) {
        self.reg_f = self.read_byte(self.stackpointer as usize);
        self.reg_f &= 0xF0;
        self.stackpointer = self.stackpointer.wrapping_add(1);
        self.reg_a = self.read_byte(self.stackpointer as usize);
        self.stackpointer = self.stackpointer.wrapping_add(1);
        self.cycles += 3;
    }

    fn ldahc(&mut self) {
        self.reg_a = self.read_byte(0xFF00 + self.reg_c as usize);
        self.cycles += 2;
    }

    fn di(&mut self) {
        self.interrupt_master_enable = false;
        self.ei_pending = false;
        self.cycles += 1;
    }

    fn pushaf(&mut self) {
        self.tick_timer_4t();
        self.stackpointer = self.stackpointer.wrapping_sub(1);
        self.write_byte(self.stackpointer as usize, self.reg_a);
        self.stackpointer = self.stackpointer.wrapping_sub(1);
        self.write_byte(self.stackpointer as usize, self.reg_f);
        self.cycles += 4;
    }

    fn ora8(&mut self) {
        let tmp = self.read();
        self.or(tmp);
        self.cycles += 2;
    }

    fn rst30h(&mut self) {
        self.tick_timer_4t();
        self.stackpointer = self.stackpointer.wrapping_sub(1);
        self.write_byte(self.stackpointer as usize, (self.program_counter >> 8) as u8);
        self.stackpointer = self.stackpointer.wrapping_sub(1);
        self.write_byte(self.stackpointer as usize, (self.program_counter) as u8);
        self.program_counter = 0x30;
        self.cycles += 4;
    }

    fn ldhlspr8(&mut self) {
        let b = self.read();
        let offset = b as i8 as i16 as u16;

        self.set_flag_bit(6, false);
        self.set_flag_bit(7, false);

        self.set_flag_bit(4, (self.stackpointer & 0xFF) + (b as u16) > 0xFF);
        self.set_flag_bit(5, (self.stackpointer & 0xF) + (b as u16 & 0xF) > 0xF);

        let hl = self.stackpointer.wrapping_add(offset);
        self.reg_h = (hl >> 8) as u8;
        self.reg_l = hl as u8;
        self.tick_timer_4t();
        self.cycles += 3;
    }

    fn ldsphl(&mut self) {
        self.stackpointer = self.hl();
        self.tick_timer_4t();
        self.cycles += 2;
    }

    fn ldaa16(&mut self) {
        let address = self.read16() as usize;
        self.reg_a = self.read_byte(address);
        self.cycles += 4;
    }

    fn ei(&mut self) {
        self.ei_pending = true;
        self.cycles += 1;
    }

    fn cp(&mut self) {
        let b = self.read();
        self.set_flag_bit(7, self.reg_a == b);
        self.set_flag_bit(6, true);
        self.set_flag_bit(5, (self.reg_a & 0x0F) < (b & 0x0F));
        self.set_flag_bit(4, self.reg_a < b);
        self.cycles += 2;
    }

    fn rst38h(&mut self) {
        self.tick_timer_4t();
        self.stackpointer = self.stackpointer.wrapping_sub(1);
        self.write_byte(self.stackpointer as usize, (self.program_counter >> 8) as u8);
        self.stackpointer = self.stackpointer.wrapping_sub(1);
        self.write_byte(self.stackpointer as usize, self.program_counter as u8);
        self.program_counter = 0x38;
        self.cycles += 4;
    }

    fn cb_rlc(&mut self, n: u8) -> u8 {
        let carry = n & 0x80;
        let n = (n << 1) & 0xff | carry >> 7;
        self.set_flag_bit(4, carry != 0);
        self.set_flag_bit(7, n == 0);
        self.set_flag_bit(5, false);
        self.set_flag_bit(6, false);
        n
    }

    fn rlcb(&mut self) {
        self.reg_b=self.cb_rlc(self.reg_b);
        self.cycles+=2;
    }

    fn rlcc(&mut self) {
        self.reg_c=self.cb_rlc(self.reg_c);
        self.cycles+=2;
    }

    fn rlcd(&mut self) {
        self.reg_d=self.cb_rlc(self.reg_d);
        self.cycles+=2;
    }

    fn rlce(&mut self) {
        self.reg_e=self.cb_rlc(self.reg_e);
        self.cycles+=2;
    }

    fn rlch(&mut self) {
        self.reg_h=self.cb_rlc(self.reg_h);
        self.cycles+=2;
    }

    fn rlcl(&mut self) {
        self.reg_l=self.cb_rlc(self.reg_l);
        self.cycles+=2;
    }

    fn rlchl(&mut self) {
        let v = self.read_byte(self.hl() as usize); let val = self.cb_rlc(v);
        self.write_byte(self.hl() as usize, val);
        self.cycles+=4;
    }

    fn cb_rlca(&mut self) {
        self.reg_a=self.cb_rlc(self.reg_a);
        self.cycles+=2;
    }

    fn cb_rrc(&mut self, n: u8) -> u8 {
        let carry = n & 1;
        let n = n >> 1 | carry << 7;
        self.set_flag_bit(4, carry == 1);
        self.set_flag_bit(7, n == 0);
        self.set_flag_bit(5, false);
        self.set_flag_bit(6, false);
        n
    }

    fn rrcb(&mut self) {
        self.reg_b=self.cb_rrc(self.reg_b);
        self.cycles+=2;
    }

    fn rrcc(&mut self) {
        self.reg_c=self.cb_rrc(self.reg_c);
        self.cycles+=2;
    }

    fn rrcd(&mut self) {
        self.reg_d=self.cb_rrc(self.reg_d);
        self.cycles+=2;
    }

    fn rrce(&mut self) {
        self.reg_e=self.cb_rrc(self.reg_e);
        self.cycles+=2;
    }

    fn rrch(&mut self) {
        self.reg_h=self.cb_rrc(self.reg_h);
        self.cycles+=2;
    }

    fn rrcl(&mut self) {
        self.reg_l=self.cb_rrc(self.reg_l);
        self.cycles+=2;
    }

    fn rrchl(&mut self) {
        let v = self.read_byte(self.hl() as usize); let val = self.cb_rrc(v);
        self.write_byte(self.hl() as usize, val);
        self.cycles+=4;
    }

    fn cb_rrca(&mut self) {
        self.reg_a=self.cb_rrc(self.reg_a);
        self.cycles+=2;
    }

    fn cb_rl(&mut self, n: u8) -> u8 {
        let carry = n & 0x80;
        let n = (n << 1) & 0xff | self.get_flag_bit(4) as u8;
        self.set_flag_bit(4, carry != 0);
        self.set_flag_bit(7, n == 0);
        self.set_flag_bit(5, false);
        self.set_flag_bit(6, false);
        n
    }

    fn rlb(&mut self) {
        self.reg_b=self.cb_rl(self.reg_b);
        self.cycles+=2;
    }

    fn rlc(&mut self) {
        self.reg_c=self.cb_rl(self.reg_c);
        self.cycles+=2;
    }

    fn rld(&mut self) {
        self.reg_d=self.cb_rl(self.reg_d);
        self.cycles+=2;
    }

    fn rle(&mut self) {
        self.reg_e=self.cb_rl(self.reg_e);
        self.cycles+=2;
    }

    fn rlh(&mut self) {
        self.reg_h=self.cb_rl(self.reg_h);
        self.cycles+=2;
    }

    fn rll(&mut self) {
        self.reg_l=self.cb_rl(self.reg_l);
        self.cycles+=2;
    }

    fn rlhl(&mut self) {
        let v = self.read_byte(self.hl() as usize); let val = self.cb_rl(v);
        self.write_byte(self.hl() as usize, val);
        self.cycles+=4;
    }

    fn cb_rla(&mut self) {
        self.reg_a=self.cb_rl(self.reg_a);
        self.cycles+=2;
    }

    fn cb_rr(&mut self, n: u8) -> u8 {
        let carry = n & 1;
        let n = (n >> 1) | ((self.get_flag_bit(4) as u8) << 7);
        self.set_flag_bit(4, carry == 1);
        self.set_flag_bit(7, n == 0);
        self.set_flag_bit(5, false);
        self.set_flag_bit(6, false);
        n
    }

    fn rrb(&mut self) {
        self.reg_b=self.cb_rr(self.reg_b);
        self.cycles+=2;
    }

    fn rrc(&mut self) {
        self.reg_c=self.cb_rr(self.reg_c);
        self.cycles+=2;
    }

    fn rrd(&mut self) {
        self.reg_d=self.cb_rr(self.reg_d);
        self.cycles+=2;
    }

    fn rre(&mut self) {
        self.reg_e=self.cb_rr(self.reg_e);
        self.cycles+=2;
    }

    fn rrh(&mut self) {
        self.reg_h=self.cb_rr(self.reg_h);
        self.cycles+=2;
    }

    fn rrl(&mut self) {
        self.reg_l=self.cb_rr(self.reg_l);
        self.cycles+=2;
    }

    fn rrhl(&mut self) {
        let v = self.read_byte(self.hl() as usize); let val = self.cb_rr(v);
        self.write_byte(self.hl() as usize, val);
        self.cycles+=4;
    }

    fn cb_rra(&mut self) {
        self.reg_a=self.cb_rr(self.reg_a);
        self.cycles+=2;
    }

    fn sla(&mut self, n: u8) -> u8 {
        let carry = n & 0x80;
        let n = (n << 1) & 0xff;
        self.set_flag_bit(4, carry != 0);
        self.set_flag_bit(7, n == 0);
        self.set_flag_bit(5, false);
        self.set_flag_bit(6, false);
        n
    }

    fn slab(&mut self) {
        self.reg_b=self.sla(self.reg_b);
        self.cycles+=2;
    }

    fn slac(&mut self) {
        self.reg_c=self.sla(self.reg_c);
        self.cycles+=2;
    }

    fn slad(&mut self) {
        self.reg_d=self.sla(self.reg_d);
        self.cycles+=2;
    }

    fn slae(&mut self) {
        self.reg_e=self.sla(self.reg_e);
        self.cycles+=2;
    }

    fn slah(&mut self) {
        self.reg_h=self.sla(self.reg_h);
        self.cycles+=2;
    }

    fn slal(&mut self) {
        self.reg_l=self.sla(self.reg_l);
        self.cycles+=2;
    }

    fn slahl(&mut self) {
        let v = self.read_byte(self.hl() as usize); let val = self.sla(v);
        self.write_byte(self.hl() as usize, val);
        self.cycles+=4;
    }

    fn slaa(&mut self) {
        self.reg_a=self.sla(self.reg_a);
        self.cycles+=2;
    }

    fn sra(&mut self, n: u8) -> u8 {
        let carry = n & 1;
        let n = n >> 1 | n & 0x80;
        self.set_flag_bit(4, carry == 1);
        self.set_flag_bit(7, n == 0);
        self.set_flag_bit(5, false);
        self.set_flag_bit(6, false);
        n
    }

    fn srab(&mut self) {
        self.reg_b=self.sra(self.reg_b);
        self.cycles+=2;
    }

    fn srac(&mut self) {
        self.reg_c=self.sra(self.reg_c);
        self.cycles+=2;
    }

    fn srad(&mut self) {
        self.reg_d=self.sra(self.reg_d);
        self.cycles+=2;
    }

    fn srae(&mut self) {
        self.reg_e=self.sra(self.reg_e);
        self.cycles+=2;
    }

    fn srah(&mut self) {
        self.reg_h=self.sra(self.reg_h);
        self.cycles+=2;
    }

    fn sral(&mut self) {
        self.reg_l=self.sra(self.reg_l);
        self.cycles+=2;
    }

    fn srahl(&mut self) {
        let v = self.read_byte(self.hl() as usize); let val = self.sra(v);
        self.write_byte(self.hl() as usize, val);
        self.cycles+=4;
    }

    fn sraa(&mut self) {
        self.reg_a=self.sra(self.reg_a);
        self.cycles+=2;
    }

    fn swap(&mut self, n: u8) -> u8 {
        let tmp = (n & 0x0F) << 4 | n >> 4;
        self.set_flag_bit(7, tmp == 0);
        self.set_flag_bit(4, false);
        self.set_flag_bit(5, false);
        self.set_flag_bit(6, false);
        tmp
    }

    fn swapb(&mut self) {
        self.reg_b=self.swap(self.reg_b);
        self.cycles+=2;
    }

    fn swapc(&mut self) {
        self.reg_c=self.swap(self.reg_c);
        self.cycles+=2;
    }

    fn swapd(&mut self) {
        self.reg_d=self.swap(self.reg_d);
        self.cycles+=2;
    }

    fn swape(&mut self) {
        self.reg_e=self.swap(self.reg_e);
        self.cycles+=2;
    }

    fn swaph(&mut self) {
        self.reg_h=self.swap(self.reg_h);
        self.cycles+=2;
    }

    fn swapl(&mut self) {
        self.reg_l=self.swap(self.reg_l);
        self.cycles+=2;
    }

    fn swaphl(&mut self) {
        let v = self.read_byte(self.hl() as usize); let val = self.swap(v);
        self.write_byte(self.hl() as usize, val);
        self.cycles+=4;
    }

    fn swapa(&mut self) {
        self.reg_a=self.swap(self.reg_a);
        self.cycles+=2;
    }

    fn srl(&mut self, n: u8) -> u8 {
        let carry = n & 1;
        let n = n >> 1;
        self.set_flag_bit(4, carry == 1);
        self.set_flag_bit(7, n == 0);
        self.set_flag_bit(5, false);
        self.set_flag_bit(6, false);
        n
    }

    fn srlb(&mut self) {
        self.reg_b=self.srl(self.reg_b);
        self.cycles+=2;
    }

    fn srlc(&mut self) {
        self.reg_c=self.srl(self.reg_c);
        self.cycles+=2;
    }

    fn srld(&mut self) {
        self.reg_d=self.srl(self.reg_d);
        self.cycles+=2;
    }

    fn srle(&mut self) {
        self.reg_e=self.srl(self.reg_e);
        self.cycles+=2;
    }

    fn srlh(&mut self) {
        self.reg_h=self.srl(self.reg_h);
        self.cycles+=2;
    }

    fn srll(&mut self) {
        self.reg_l=self.srl(self.reg_l);
        self.cycles+=2;
    }

    fn srlhl(&mut self) {
        let v = self.read_byte(self.hl() as usize); let val = self.srl(v);
        self.write_byte(self.hl() as usize, val);
        self.cycles+=4;
    }

    fn srla(&mut self) {
        self.reg_a=self.srl(self.reg_a);
        self.cycles+=2;
    }

    fn bit(&mut self, n: u8, b: u8) {
        self.set_flag_bit(7, (n & (1 << b)) == 0);
        self.set_flag_bit(6, false);
        self.set_flag_bit(5, true);
    }

    fn bit0b(&mut self) {
        self.bit(self.reg_b,0);
        self.cycles+=2;
    }

    fn bit0c(&mut self) {
        self.bit(self.reg_c,0);
        self.cycles+=2;
    }

    fn bit0d(&mut self) {
        self.bit(self.reg_d,0);
        self.cycles+=2;
    }

    fn bit0e(&mut self) {
        self.bit(self.reg_e,0);
        self.cycles+=2;
    }

    fn bit0h(&mut self) {
        self.bit(self.reg_h,0);
        self.cycles+=2;
    }

    fn bit0l(&mut self) {
        self.bit(self.reg_l,0);
        self.cycles+=2;
    }

    fn bit0hl(&mut self) {
        let v = self.read_byte(self.hl() as usize);
        self.bit(v,0);
        self.cycles+=3;
    }

    fn bit0a(&mut self) {
        self.bit(self.reg_a,0);
        self.cycles+=2;
    }

    fn bit1b(&mut self) {
        self.bit(self.reg_b,1);
        self.cycles+=2;
    }

    fn bit1c(&mut self) {
        self.bit(self.reg_c,1);
        self.cycles+=2;
    }

    fn bit1d(&mut self) {
        self.bit(self.reg_d,1);
        self.cycles+=2;
    }

    fn bit1e(&mut self) {
        self.bit(self.reg_e,1);
        self.cycles+=2;
    }

    fn bit1h(&mut self) {
        self.bit(self.reg_h,1);
        self.cycles+=2;
    }

    fn bit1l(&mut self) {
        self.bit(self.reg_l,1);
        self.cycles+=2;
    }

    fn bit1hl(&mut self) {
        let v = self.read_byte(self.hl() as usize);
        self.bit(v,1);
        self.cycles+=3;
    }

    fn bit1a(&mut self) {
        self.bit(self.reg_a,1);
        self.cycles+=2;
    }

    fn bit2b(&mut self) {
        self.bit(self.reg_b,2);
        self.cycles+=2;
    }

    fn bit2c(&mut self) {
        self.bit(self.reg_c,2);
        self.cycles+=2;
    }

    fn bit2d(&mut self) {
        self.bit(self.reg_d,2);
        self.cycles+=2;
    }

    fn bit2e(&mut self) {
        self.bit(self.reg_e,2);
        self.cycles+=2;
    }

    fn bit2h(&mut self) {
        self.bit(self.reg_h,2);
        self.cycles+=2;
    }

    fn bit2l(&mut self) {
        self.bit(self.reg_l,2);
        self.cycles+=2;
    }

    fn bit2hl(&mut self) {
        let v = self.read_byte(self.hl() as usize);
        self.bit(v,2);
        self.cycles+=3;
    }

    fn bit2a(&mut self) {
        self.bit(self.reg_a,2);
        self.cycles+=2;
    }

    fn bit3b(&mut self) {
        self.bit(self.reg_b,3);
        self.cycles+=2;
    }

    fn bit3c(&mut self) {
        self.bit(self.reg_c,3);
        self.cycles+=2;
    }

    fn bit3d(&mut self) {
        self.bit(self.reg_d,3);
        self.cycles+=2;
    }

    fn bit3e(&mut self) {
        self.bit(self.reg_e,3);
        self.cycles+=2;
    }

    fn bit3h(&mut self) {
        self.bit(self.reg_h,3);
        self.cycles+=2;
    }

    fn bit3l(&mut self) {
        self.bit(self.reg_l,3);
        self.cycles+=2;
    }

    fn bit3hl(&mut self) {
        let v = self.read_byte(self.hl() as usize);
        self.bit(v,3);
        self.cycles+=3;
    }

    fn bit3a(&mut self) {
        self.bit(self.reg_a,3);
        self.cycles+=2;
    }

    fn bit4b(&mut self) {
        self.bit(self.reg_b,4);
        self.cycles+=2;
    }

    fn bit4c(&mut self) {
        self.bit(self.reg_c,4);
        self.cycles+=2;
    }

    fn bit4d(&mut self) {
        self.bit(self.reg_d,4);
        self.cycles+=2;
    }

    fn bit4e(&mut self) {
        self.bit(self.reg_e,4);
        self.cycles+=2;
    }

    fn bit4h(&mut self) {
        self.bit(self.reg_h,4);
        self.cycles+=2;
    }

    fn bit4l(&mut self) {
        self.bit(self.reg_l,4);
        self.cycles+=2;
    }

    fn bit4hl(&mut self) {
        let v = self.read_byte(self.hl() as usize);
        self.bit(v,4);
        self.cycles+=3;
    }

    fn bit4a(&mut self) {
        self.bit(self.reg_a,4);
        self.cycles+=2;
    }

    fn bit5b(&mut self) {
        self.bit(self.reg_b,5);
        self.cycles+=2;
    }

    fn bit5c(&mut self) {
        self.bit(self.reg_c,5);
        self.cycles+=2;
    }

    fn bit5d(&mut self) {
        self.bit(self.reg_d,5);
        self.cycles+=2;
    }

    fn bit5e(&mut self) {
        self.bit(self.reg_e,5);
        self.cycles+=2;
    }

    fn bit5h(&mut self) {
        self.bit(self.reg_h,5);
        self.cycles+=2;
    }

    fn bit5l(&mut self) {
        self.bit(self.reg_l,5);
        self.cycles+=2;
    }

    fn bit5hl(&mut self) {
        let v = self.read_byte(self.hl() as usize);
        self.bit(v,5);
        self.cycles+=3;
    }

    fn bit5a(&mut self) {
        self.bit(self.reg_a,5);
        self.cycles+=2;
    }

    fn bit6b(&mut self) {
        self.bit(self.reg_b,6);
        self.cycles+=2;
    }

    fn bit6c(&mut self) {
        self.bit(self.reg_c,6);
        self.cycles+=2;
    }

    fn bit6d(&mut self) {
        self.bit(self.reg_d,6);
        self.cycles+=2;
    }

    fn bit6e(&mut self) {
        self.bit(self.reg_e,6);
        self.cycles+=2;
    }

    fn bit6h(&mut self) {
        self.bit(self.reg_h,6);
        self.cycles+=2;
    }

    fn bit6l(&mut self) {
        self.bit(self.reg_l,6);
        self.cycles+=2;
    }

    fn bit6hl(&mut self) {
        let v = self.read_byte(self.hl() as usize);
        self.bit(v,6);
        self.cycles+=3;
    }

    fn bit6a(&mut self) {
        self.bit(self.reg_a,6);
        self.cycles+=2;
    }

    fn bit7b(&mut self) {
        self.bit(self.reg_b,7);
        self.cycles+=2;
    }

    fn bit7c(&mut self) {
        self.bit(self.reg_c,7);
        self.cycles+=2;
    }

    fn bit7d(&mut self) {
        self.bit(self.reg_d,7);
        self.cycles+=2;
    }

    fn bit7e(&mut self) {
        self.bit(self.reg_e,7);
        self.cycles+=2;
    }

    fn bit7h(&mut self) {
        self.bit(self.reg_h,7);
        self.cycles+=2;
    }

    fn bit7l(&mut self) {
        self.bit(self.reg_l,7);
        self.cycles+=2;
    }

    fn bit7hl(&mut self) {
        let v = self.read_byte(self.hl() as usize);
        self.bit(v,7);
        self.cycles+=3;
    }

    fn bit7a(&mut self) {
        self.bit(self.reg_a,7);
        self.cycles+=2;
    }

    fn res(&mut self, n: u8, b: u8) -> u8 {
        n & !(1 << b)
    }

    fn res0b(&mut self) {
        self.reg_b=self.res(self.reg_b,0);
        self.cycles+=2;
    }

    fn res0c(&mut self) {
        self.reg_c=self.res(self.reg_c,0);
        self.cycles+=2;
    }

    fn res0d(&mut self) {
        self.reg_d=self.res(self.reg_d,0);
        self.cycles+=2;
    }

    fn res0e(&mut self) {
        self.reg_e=self.res(self.reg_e,0);
        self.cycles+=2;
    }

    fn res0h(&mut self) {
        self.reg_h=self.res(self.reg_h,0);
        self.cycles+=2;
    }

    fn res0l(&mut self) {
        self.reg_l=self.res(self.reg_l,0);
        self.cycles+=2;
    }

    fn res0hl(&mut self) {
        let v = self.read_byte(self.hl() as usize);
        let val=self.res(v,0);
        self.write_byte(self.hl() as usize, val);
        self.cycles+=4;
    }

    fn res0a(&mut self) {
        self.reg_a=self.res(self.reg_a,0);
        self.cycles+=2;
    }

    fn res1b(&mut self) {
        self.reg_b=self.res(self.reg_b,1);
        self.cycles+=2;
    }

    fn res1c(&mut self) {
        self.reg_c=self.res(self.reg_c,1);
        self.cycles+=2;
    }

    fn res1d(&mut self) {
        self.reg_d=self.res(self.reg_d,1);
        self.cycles+=2;
    }

    fn res1e(&mut self) {
        self.reg_e=self.res(self.reg_e,1);
        self.cycles+=2;
    }

    fn res1h(&mut self) {
        self.reg_h=self.res(self.reg_h,1);
        self.cycles+=2;
    }

    fn res1l(&mut self) {
        self.reg_l=self.res(self.reg_l,1);
        self.cycles+=2;
    }

    fn res1hl(&mut self) {
        let v = self.read_byte(self.hl() as usize);
        let val=self.res(v,1);
        self.write_byte(self.hl() as usize, val);
        self.cycles+=4;
    }

    fn res1a(&mut self) {
        self.reg_a=self.res(self.reg_a,1);
        self.cycles+=2;
    }

    fn res2b(&mut self) {
        self.reg_b=self.res(self.reg_b,2);
        self.cycles+=2;
    }

    fn res2c(&mut self) {
        self.reg_c=self.res(self.reg_c,2);
        self.cycles+=2;
    }

    fn res2d(&mut self) {
        self.reg_d=self.res(self.reg_d,2);
        self.cycles+=2;
    }

    fn res2e(&mut self) {
        self.reg_e=self.res(self.reg_e,2);
        self.cycles+=2;
    }

    fn res2h(&mut self) {
        self.reg_h=self.res(self.reg_h,2);
        self.cycles+=2;
    }

    fn res2l(&mut self) {
        self.reg_l=self.res(self.reg_l,2);
        self.cycles+=2;
    }

    fn res2hl(&mut self) {
        let v = self.read_byte(self.hl() as usize);
        let val=self.res(v,2);
        self.write_byte(self.hl() as usize, val);
        self.cycles+=4;
    }

    fn res2a(&mut self) {
        self.reg_a=self.res(self.reg_a,2);
        self.cycles+=2;
    }

    fn res3b(&mut self) {
        self.reg_b=self.res(self.reg_b,3);
        self.cycles+=2;
    }

    fn res3c(&mut self) {
        self.reg_c=self.res(self.reg_c,3);
        self.cycles+=2;
    }

    fn res3d(&mut self) {
        self.reg_d=self.res(self.reg_d,3);
        self.cycles+=2;
    }

    fn res3e(&mut self) {
        self.reg_e=self.res(self.reg_e,3);
        self.cycles+=2;
    }

    fn res3h(&mut self) {
        self.reg_h=self.res(self.reg_h,3);
        self.cycles+=2;
    }

    fn res3l(&mut self) {
        self.reg_l=self.res(self.reg_l,3);
        self.cycles+=2;
    }

    fn res3hl(&mut self) {
        let v = self.read_byte(self.hl() as usize);
        let val=self.res(v,3);
        self.write_byte(self.hl() as usize, val);
        self.cycles+=4;
    }

    fn res3a(&mut self) {
        self.reg_a=self.res(self.reg_a,3);
        self.cycles+=2;
    }

    fn res4b(&mut self) {
        self.reg_b=self.res(self.reg_b,4);
        self.cycles+=2;
    }

    fn res4c(&mut self) {
        self.reg_c=self.res(self.reg_c,4);
        self.cycles+=2;
    }

    fn res4d(&mut self) {
        self.reg_d=self.res(self.reg_d,4);
        self.cycles+=2;
    }

    fn res4e(&mut self) {
        self.reg_e=self.res(self.reg_e,4);
        self.cycles+=2;
    }

    fn res4h(&mut self) {
        self.reg_h=self.res(self.reg_h,4);
        self.cycles+=2;
    }

    fn res4l(&mut self) {
        self.reg_l=self.res(self.reg_l,4);
        self.cycles+=2;
    }

    fn res4hl(&mut self) {
        let v = self.read_byte(self.hl() as usize);
        let val=self.res(v,4);
        self.write_byte(self.hl() as usize, val);
        self.cycles+=4;
    }

    fn res4a(&mut self) {
        self.reg_a=self.res(self.reg_a,4);
        self.cycles+=2;
    }

    fn res5b(&mut self) {
        self.reg_b=self.res(self.reg_b,5);
        self.cycles+=2;
    }

    fn res5c(&mut self) {
        self.reg_c=self.res(self.reg_c,5);
        self.cycles+=2;
    }

    fn res5d(&mut self) {
        self.reg_d=self.res(self.reg_d,5);
        self.cycles+=2;
    }

    fn res5e(&mut self) {
        self.reg_e=self.res(self.reg_e,5);
        self.cycles+=2;
    }

    fn res5h(&mut self) {
        self.reg_h=self.res(self.reg_h,5);
        self.cycles+=2;
    }

    fn res5l(&mut self) {
        self.reg_l=self.res(self.reg_l,5);
        self.cycles+=2;
    }

    fn res5hl(&mut self) {
        let v = self.read_byte(self.hl() as usize);
        let val=self.res(v,5);
        self.write_byte(self.hl() as usize, val);
        self.cycles+=4;
    }

    fn res5a(&mut self) {
        self.reg_a=self.res(self.reg_a,5);
        self.cycles+=2;
    }

    fn res6b(&mut self) {
        self.reg_b=self.res(self.reg_b,6);
        self.cycles+=2;
    }

    fn res6c(&mut self) {
        self.reg_c=self.res(self.reg_c,6);
        self.cycles+=2;
    }

    fn res6d(&mut self) {
        self.reg_d=self.res(self.reg_d,6);
        self.cycles+=2;
    }

    fn res6e(&mut self) {
        self.reg_e=self.res(self.reg_e,6);
        self.cycles+=2;
    }

    fn res6h(&mut self) {
        self.reg_h=self.res(self.reg_h,6);
        self.cycles+=2;
    }

    fn res6l(&mut self) {
        self.reg_l=self.res(self.reg_l,6);
        self.cycles+=2;
    }

    fn res6hl(&mut self) {
        let v = self.read_byte(self.hl() as usize);
        let val=self.res(v,6);
        self.write_byte(self.hl() as usize, val);
        self.cycles+=4;
    }

    fn res6a(&mut self) {
        self.reg_a=self.res(self.reg_a,6);
        self.cycles+=2;
    }

    fn res7b(&mut self) {
        self.reg_b=self.res(self.reg_b,7);
        self.cycles+=2;
    }

    fn res7c(&mut self) {
        self.reg_c=self.res(self.reg_c,7);
        self.cycles+=2;
    }

    fn res7d(&mut self) {
        self.reg_d=self.res(self.reg_d,7);
        self.cycles+=2;
    }

    fn res7e(&mut self) {
        self.reg_e=self.res(self.reg_e,7);
        self.cycles+=2;
    }

    fn res7h(&mut self) {
        self.reg_h=self.res(self.reg_h,7);
        self.cycles+=2;
    }

    fn res7l(&mut self) {
        self.reg_l=self.res(self.reg_l,7);
        self.cycles+=2;
    }

    fn res7hl(&mut self) {
        let v = self.read_byte(self.hl() as usize);
        let val=self.res(v,7);
        self.write_byte(self.hl() as usize, val);
        self.cycles+=4;
    }

    fn res7a(&mut self) {
        self.reg_a=self.res(self.reg_a,7);
        self.cycles+=2;
    }

    fn set(&mut self, n: u8, b: u8) -> u8 {
        n | (1 << b)
    }

    fn set0b(&mut self) {
        self.reg_b=self.set(self.reg_b,0);
        self.cycles+=2;
    }

    fn set0c(&mut self) {
        self.reg_c=self.set(self.reg_c,0);
        self.cycles+=2;
    }

    fn set0d(&mut self) {
        self.reg_d=self.set(self.reg_d,0);
        self.cycles+=2;
    }

    fn set0e(&mut self) {
        self.reg_e=self.set(self.reg_e,0);
        self.cycles+=2;
    }

    fn set0h(&mut self) {
        self.reg_h=self.set(self.reg_h,0);
        self.cycles+=2;
    }

    fn set0l(&mut self) {
        self.reg_l=self.set(self.reg_l,0);
        self.cycles+=2;
    }

    fn set0hl(&mut self) {
        let v = self.read_byte(self.hl() as usize);
        let val=self.set(v,0);
        self.write_byte(self.hl() as usize, val);
        self.cycles+=4;
    }

    fn set0a(&mut self) {
        self.reg_a=self.set(self.reg_a,0);
        self.cycles+=2;
    }

    fn set1b(&mut self) {
        self.reg_b=self.set(self.reg_b,1);
        self.cycles+=2;
    }

    fn set1c(&mut self) {
        self.reg_c=self.set(self.reg_c,1);
        self.cycles+=2;
    }

    fn set1d(&mut self) {
        self.reg_d=self.set(self.reg_d,1);
        self.cycles+=2;
    }

    fn set1e(&mut self) {
        self.reg_e=self.set(self.reg_e,1);
        self.cycles+=2;
    }

    fn set1h(&mut self) {
        self.reg_h=self.set(self.reg_h,1);
        self.cycles+=2;
    }

    fn set1l(&mut self) {
        self.reg_l=self.set(self.reg_l,1);
        self.cycles+=2;
    }

    fn set1hl(&mut self) {
        let v = self.read_byte(self.hl() as usize);
        let val=self.set(v,1);
        self.write_byte(self.hl() as usize, val);
        self.cycles+=4;
    }

    fn set1a(&mut self) {
        self.reg_a=self.set(self.reg_a,1);
        self.cycles+=2;
    }

    fn set2b(&mut self) {
        self.reg_b=self.set(self.reg_b,2);
        self.cycles+=2;
    }

    fn set2c(&mut self) {
        self.reg_c=self.set(self.reg_c,2);
        self.cycles+=2;
    }

    fn set2d(&mut self) {
        self.reg_d=self.set(self.reg_d,2);
        self.cycles+=2;
    }

    fn set2e(&mut self) {
        self.reg_e=self.set(self.reg_e,2);
        self.cycles+=2;
    }

    fn set2h(&mut self) {
        self.reg_h=self.set(self.reg_h,2);
        self.cycles+=2;
    }

    fn set2l(&mut self) {
        self.reg_l=self.set(self.reg_l,2);
        self.cycles+=2;
    }

    fn set2hl(&mut self) {
        let v = self.read_byte(self.hl() as usize);
        let val=self.set(v,2);
        self.write_byte(self.hl() as usize, val);
        self.cycles+=4;
    }

    fn set2a(&mut self) {
        self.reg_a=self.set(self.reg_a,2);
        self.cycles+=2;
    }

    fn set3b(&mut self) {
        self.reg_b=self.set(self.reg_b,3);
        self.cycles+=2;
    }

    fn set3c(&mut self) {
        self.reg_c=self.set(self.reg_c,3);
        self.cycles+=2;
    }

    fn set3d(&mut self) {
        self.reg_d=self.set(self.reg_d,3);
        self.cycles+=2;
    }

    fn set3e(&mut self) {
        self.reg_e=self.set(self.reg_e,3);
        self.cycles+=2;
    }

    fn set3h(&mut self) {
        self.reg_h=self.set(self.reg_h,3);
        self.cycles+=2;
    }

    fn set3l(&mut self) {
        self.reg_l=self.set(self.reg_l,3);
        self.cycles+=2;
    }

    fn set3hl(&mut self) {
        let v = self.read_byte(self.hl() as usize);
        let val=self.set(v,3);
        self.write_byte(self.hl() as usize, val);
        self.cycles+=4;
    }

    fn set3a(&mut self) {
        self.reg_a=self.set(self.reg_a,3);
        self.cycles+=2;
    }

    fn set4b(&mut self) {
        self.reg_b=self.set(self.reg_b,4);
        self.cycles+=2;
    }

    fn set4c(&mut self) {
        self.reg_c=self.set(self.reg_c,4);
        self.cycles+=2;
    }

    fn set4d(&mut self) {
        self.reg_d=self.set(self.reg_d,4);
        self.cycles+=2;
    }

    fn set4e(&mut self) {
        self.reg_e=self.set(self.reg_e,4);
        self.cycles+=2;
    }

    fn set4h(&mut self) {
        self.reg_h=self.set(self.reg_h,4);
        self.cycles+=2;
    }

    fn set4l(&mut self) {
        self.reg_l=self.set(self.reg_l,4);
        self.cycles+=2;
    }

    fn set4hl(&mut self) {
        let v = self.read_byte(self.hl() as usize);
        let val=self.set(v,4);
        self.write_byte(self.hl() as usize, val);
        self.cycles+=4;
    }

    fn set4a(&mut self) {
        self.reg_a=self.set(self.reg_a,4);
        self.cycles+=2;
    }

    fn set5b(&mut self) {
        self.reg_b=self.set(self.reg_b,5);
        self.cycles+=2;
    }

    fn set5c(&mut self) {
        self.reg_c=self.set(self.reg_c,5);
        self.cycles+=2;
    }

    fn set5d(&mut self) {
        self.reg_d=self.set(self.reg_d,5);
        self.cycles+=2;
    }

    fn set5e(&mut self) {
        self.reg_e=self.set(self.reg_e,5);
        self.cycles+=2;
    }

    fn set5h(&mut self) {
        self.reg_h=self.set(self.reg_h,5);
        self.cycles+=2;
    }

    fn set5l(&mut self) {
        self.reg_l=self.set(self.reg_l,5);
        self.cycles+=2;
    }

    fn set5hl(&mut self) {
        let v = self.read_byte(self.hl() as usize);
        let val=self.set(v,5);
        self.write_byte(self.hl() as usize, val);
        self.cycles+=4;
    }

    fn set5a(&mut self) {
        self.reg_a=self.set(self.reg_a,5);
        self.cycles+=2;
    }

    fn set6b(&mut self) {
        self.reg_b=self.set(self.reg_b,6);
        self.cycles+=2;
    }

    fn set6c(&mut self) {
        self.reg_c=self.set(self.reg_c,6);
        self.cycles+=2;
    }

    fn set6d(&mut self) {
        self.reg_d=self.set(self.reg_d,6);
        self.cycles+=2;
    }

    fn set6e(&mut self) {
        self.reg_e=self.set(self.reg_e,6);
        self.cycles+=2;
    }

    fn set6h(&mut self) {
        self.reg_h=self.set(self.reg_h,6);
        self.cycles+=2;
    }

    fn set6l(&mut self) {
        self.reg_l=self.set(self.reg_l,6);
        self.cycles+=2;
    }

    fn set6hl(&mut self) {
        let v = self.read_byte(self.hl() as usize);
        let val=self.set(v,6);
        self.write_byte(self.hl() as usize, val);
        self.cycles+=4;
    }

    fn set6a(&mut self) {
        self.reg_a=self.set(self.reg_a,6);
        self.cycles+=2;
    }

    fn set7b(&mut self) {
        self.reg_b=self.set(self.reg_b,7);
        self.cycles+=2;
    }

    fn set7c(&mut self) {
        self.reg_c=self.set(self.reg_c,7);
        self.cycles+=2;
    }

    fn set7d(&mut self) {
        self.reg_d=self.set(self.reg_d,7);
        self.cycles+=2;
    }

    fn set7e(&mut self) {
        self.reg_e=self.set(self.reg_e,7);
        self.cycles+=2;
    }

    fn set7h(&mut self) {
        self.reg_h=self.set(self.reg_h,7);
        self.cycles+=2;
    }

    fn set7l(&mut self) {
        self.reg_l=self.set(self.reg_l,7);
        self.cycles+=2;
    }

    fn set7hl(&mut self) {
        let v = self.read_byte(self.hl() as usize);
        let val=self.set(v,7);
        self.write_byte(self.hl() as usize, val);
        self.cycles+=4;
    }

    fn set7a(&mut self) {
        self.reg_a=self.set(self.reg_a,7);
        self.cycles+=2;
    }

}

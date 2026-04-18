use std::{cell::RefCell, rc::Rc};
use std::sync::{Mutex, atomic::Ordering};

use wasm_bindgen::prelude::*;
use web_sys::console;
use wasm_bindgen::JsCast;

use crate::{cpu, ppu};

lazy_static::lazy_static! {
    static ref KEYS: Mutex<[bool; 256]> = Mutex::new([false; 256]);
    static ref PREVIOUS_KEYS: Mutex<[bool; 256]> = Mutex::new([false; 256]);
}

// call JS function to toggle vram canvas
#[wasm_bindgen]
extern "C" {
    fn toggleVramCanvas(v: bool);
    fn queueAudioSamples(left: &[f32], right: &[f32]);
    fn storeSaveData(key: &str, data: &[u8]);
    fn loadSaveData(key: &str) -> JsValue;
}

const TOGGLE_KEYS: [u32; 8] = [32, 67, 76, 78, 86, 106, 27, 9]; // 9 = Tab for speed cycle

thread_local! {
    static CPU: RefCell<Option<Rc<RefCell<cpu::CPU>>>> = RefCell::new(None);
    static EMULATION_RUNNING: RefCell<bool> = RefCell::new(false);
}

#[wasm_bindgen]
pub fn set_key_state(key_code: u32, is_pressed: bool) {
    if key_code as usize >= 256 { return; }
    let mut keys = KEYS.lock().unwrap();
    if !TOGGLE_KEYS.contains(&key_code) {
        keys[key_code as usize] = is_pressed;
    }
    else if is_pressed {
        keys[key_code as usize] = true;
    }
    else {
        keys[key_code as usize] = false;
    }
}

/// Called from JS with the ROM bytes to load and run a new ROM
#[wasm_bindgen]
pub fn load_rom_data(rom_data: &[u8]) {
    // Preserve speed from previous session
    let prev_speed = CPU.with(|c| {
        c.borrow().as_ref().map(|rc| rc.borrow().speed_multiplier).unwrap_or(1)
    });

    let bootrom = include_bytes!("../roms/DMG_ROM.bin");
    let mut cpu = cpu::CPU::new();
    cpu.speed_multiplier = prev_speed;
    cpu.bootload(bootrom.to_vec());
    cpu.load_rom(rom_data.to_vec());

    // Restore save data if this cartridge has a battery
    if cpu.has_battery() {
        let key = format!("rustboy_save_{}", cpu.rom_title());
        let save_js = loadSaveData(&key);
        if !save_js.is_undefined() && !save_js.is_null() {
            if let Ok(arr) = save_js.dyn_into::<js_sys::Uint8Array>() {
                let data = arr.to_vec();
                cpu.import_save_ram(&data);
                console::log_1(&format!("Restored save for '{}'", key).into());
            }
        }
    }

    let cpu = Rc::new(RefCell::new(cpu));

    CPU.with(|c| {
        *c.borrow_mut() = Some(cpu.clone());
    });

    // Only start the loop if it's not already running
    EMULATION_RUNNING.with(|running| {
        if !*running.borrow() {
            *running.borrow_mut() = true;
            start_emulation_loop();
        }
    });
}

/// Save the current game's external RAM to persistent storage
#[wasm_bindgen]
pub fn save_game() {
    CPU.with(|c| {
        let cpu_opt = c.borrow();
        if let Some(cpu_rc) = cpu_opt.as_ref() {
            let cpu = cpu_rc.borrow();
            if cpu.has_battery() {
                let key = format!("rustboy_save_{}", cpu.rom_title());
                let data = cpu.export_save_ram();
                storeSaveData(&key, &data);
            }
        }
    });
}

/// Returns current CPU debug state as a string (called from JS)
#[wasm_bindgen]
pub fn get_debug_state() -> String {
    CPU.with(|c| {
        let cpu_opt = c.borrow();
        match cpu_opt.as_ref() {
            Some(cpu_rc) => cpu_rc.borrow().get_debug_state(),
            None => "No CPU loaded".to_string(),
        }
    })
}

/// Returns the current emulation speed multiplier (1, 2, 4, or 8)
#[wasm_bindgen]
pub fn get_speed() -> u32 {
    CPU.with(|c| {
        let cpu_opt = c.borrow();
        match cpu_opt.as_ref() {
            Some(cpu_rc) => cpu_rc.borrow().get_speed(),
            None => 1,
        }
    })
}

// ── Breakpoint API ───────────────────────────────────────────────────────

/// Add a breakpoint that triggers when PC reaches the given address.
/// Address is in decimal; use JS `0x1234` for hex.
#[wasm_bindgen]
pub fn add_breakpoint_pc(addr: u16) {
    CPU.with(|c| {
        let cpu_opt = c.borrow();
        if let Some(cpu_rc) = cpu_opt.as_ref() {
            cpu_rc.borrow_mut().add_breakpoint_pc(addr);
        }
    });
}

/// Add a breakpoint that triggers when a register equals a value.
/// reg: "a","b","c","d","e","h","l","f","af","bc","de","hl","sp","pc"
#[wasm_bindgen]
pub fn add_breakpoint_reg(reg: &str, value: u16) {
    CPU.with(|c| {
        let cpu_opt = c.borrow();
        if let Some(cpu_rc) = cpu_opt.as_ref() {
            cpu_rc.borrow_mut().add_breakpoint_reg(reg, value);
        }
    });
}

/// Add a breakpoint that triggers when memory at `addr` equals `value`.
#[wasm_bindgen]
pub fn add_breakpoint_mem(addr: u16, value: u8) {
    CPU.with(|c| {
        let cpu_opt = c.borrow();
        if let Some(cpu_rc) = cpu_opt.as_ref() {
            cpu_rc.borrow_mut().add_breakpoint_mem(addr, value);
        }
    });
}

/// Add a breakpoint based on instruction opcode (e.g., 0xDA for JP C,a16)
#[wasm_bindgen]
pub fn add_breakpoint_opcode(opcode: u8) {
    CPU.with(|c| {
        let cpu_opt = c.borrow();
        if let Some(cpu_rc) = cpu_opt.as_ref() {
            cpu_rc.borrow_mut().add_breakpoint_opcode(opcode);
        }
    });
}

/// Add a breakpoint based on CB-prefixed instruction opcode (e.g., 0x7C for BIT 7,H)
#[wasm_bindgen]
pub fn add_breakpoint_cb_opcode(opcode: u8) {
    CPU.with(|c| {
        let cpu_opt = c.borrow();
        if let Some(cpu_rc) = cpu_opt.as_ref() {
            cpu_rc.borrow_mut().add_breakpoint_cb_opcode(opcode);
        }
    });
}

/// Remove a breakpoint by index.
#[wasm_bindgen]
pub fn remove_breakpoint(index: usize) {
    CPU.with(|c| {
        let cpu_opt = c.borrow();
        if let Some(cpu_rc) = cpu_opt.as_ref() {
            cpu_rc.borrow_mut().remove_breakpoint(index);
        }
    });
}

/// Clear all breakpoints.
#[wasm_bindgen]
pub fn clear_breakpoints() {
    CPU.with(|c| {
        let cpu_opt = c.borrow();
        if let Some(cpu_rc) = cpu_opt.as_ref() {
            cpu_rc.borrow_mut().clear_breakpoints();
        }
    });
}

/// List all breakpoints (returns string for console display).
#[wasm_bindgen]
pub fn list_breakpoints() -> String {
    CPU.with(|c| {
        let cpu_opt = c.borrow();
        match cpu_opt.as_ref() {
            Some(cpu_rc) => cpu_rc.borrow().list_breakpoints(),
            None => "No CPU loaded".to_string(),
        }
    })
}

// ── Memory / register inspection API ─────────────────────────────────

/// Read a single byte at the given address (respects MBC banking).
#[wasm_bindgen]
pub fn peek(addr: u16) -> u8 {
    CPU.with(|c| {
        let cpu_opt = c.borrow();
        match cpu_opt.as_ref() {
            Some(cpu_rc) => cpu_rc.borrow().peek(addr),
            None => 0,
        }
    })
}

/// Hex-dump `len` bytes starting at `start`. Max 256 bytes.
#[wasm_bindgen]
pub fn peek_slice(start: u16, len: u16) -> String {
    CPU.with(|c| {
        let cpu_opt = c.borrow();
        match cpu_opt.as_ref() {
            Some(cpu_rc) => cpu_rc.borrow().peek_slice(start, len),
            None => "No CPU loaded".to_string(),
        }
    })
}

/// Print all register values.
#[wasm_bindgen]
pub fn peek_regs() -> String {
    CPU.with(|c| {
        let cpu_opt = c.borrow();
        match cpu_opt.as_ref() {
            Some(cpu_rc) => cpu_rc.borrow().peek_regs(),
            None => "No CPU loaded".to_string(),
        }
    })
}

// ── Trace API ────────────────────────────────────────────────────────

/// Toggle CPU instruction tracing on/off.
#[wasm_bindgen]
pub fn toggle_trace() {
    CPU.with(|c| {
        let cpu_opt = c.borrow();
        if let Some(cpu_rc) = cpu_opt.as_ref() {
            cpu_rc.borrow_mut().toggle_trace();
        }
    });
}

/// Returns true if tracing is currently active.
#[wasm_bindgen]
pub fn is_tracing() -> bool {
    CPU.with(|c| {
        let cpu_opt = c.borrow();
        match cpu_opt.as_ref() {
            Some(cpu_rc) => cpu_rc.borrow().is_tracing(),
            None => false,
        }
    })
}

/// Get the full trace buffer as a single string (lines separated by \n).
#[wasm_bindgen]
pub fn get_trace() -> String {
    CPU.with(|c| {
        let cpu_opt = c.borrow();
        match cpu_opt.as_ref() {
            Some(cpu_rc) => cpu_rc.borrow().get_trace(),
            None => String::new(),
        }
    })
}

/// Clear the trace buffer.
#[wasm_bindgen]
pub fn clear_trace() {
    CPU.with(|c| {
        let cpu_opt = c.borrow();
        if let Some(cpu_rc) = cpu_opt.as_ref() {
            cpu_rc.borrow_mut().clear_trace();
        }
    });
}

/// Get the number of lines currently in the trace buffer.
#[wasm_bindgen]
pub fn trace_len() -> usize {
    CPU.with(|c| {
        let cpu_opt = c.borrow();
        match cpu_opt.as_ref() {
            Some(cpu_rc) => cpu_rc.borrow().trace_buffer.len(),
            None => 0,
        }
    })
}

// This is like the `main` function, except for JavaScript.
#[wasm_bindgen(start)]
pub fn main_js() -> Result<(), JsValue> {
    console_error_panic_hook::set_once();
    // Setup canvases but don't start emulation yet — wait for ROM load
    // We still call setup so the canvases are sized
    setup_canvas_and_get_context("rustboy-canvas");
    setup_canvas_and_get_context("vram-canvas");
    Ok(())
}

fn start_emulation_loop() {
    let context = setup_canvas_and_get_context("rustboy-canvas");
    let vram_context = setup_canvas_and_get_context("vram-canvas");

    let f = Rc::new(RefCell::new(None));
    let g = f.clone();

    const CYCLES_PER_FRAME: u32 = 70224;

    let mut cycles_this_frame: u32 = 0;

    *g.borrow_mut() = Some(Closure::wrap(Box::new(move || {
        let cpu_rc = CPU.with(|c| c.borrow().clone());
        let Some(cpu_rc) = cpu_rc else {
            request_animation_frame(f.borrow().as_ref().unwrap());
            return;
        };
        let mut cpu = cpu_rc.borrow_mut();

        if cpu.is_paused.load(Ordering::Relaxed) {
            if cpu.go_next.load(Ordering::Relaxed) {
                cpu.go_next.store(false, Ordering::Relaxed);
                // Single-step mode: execute exactly one instruction
                cpu.handle_interrupts();
                if !cpu.halt {
                    cpu.execute();
                } else {
                    cpu.cycles += 1;
                    cpu.tick_timer_4t(); // Tick timer for HALT's 1 M-cycle
                    if cpu.oam_dma_active {
                        if cpu.oam_dma_remaining <= 1 {
                            cpu.oam_dma_remaining = 0;
                            cpu.oam_dma_active = false;
                        } else {
                            cpu.oam_dma_remaining -= 1;
                        }
                    }
                }
                let cycles = cpu.cycles;
                let t_cycles = cycles * 4;
                cpu.handle_timer(t_cycles);
                cpu.update_ppu(t_cycles as u32);
                cpu.update_apu(t_cycles as u32);
                cpu.total_cycles += cycles as u64;
                cpu.cycles = 0;
            }
            check_keys(&mut cpu);
            request_animation_frame(f.borrow().as_ref().unwrap());
            return;
        }

        let target_cycles = CYCLES_PER_FRAME * cpu.speed_multiplier;

        while cycles_this_frame < target_cycles {

            // Check breakpoints before executing
            if cpu.check_breakpoints() {
                break;
            }

            // Handle interrupts BEFORE executing the next instruction
            cpu.handle_interrupts();

            if !cpu.halt {
                cpu.execute();
            } else {
                cpu.cycles += 1; // HALT consumes 1 M-cycle per iteration
                cpu.tick_timer_4t(); // Tick timer for HALT's 1 M-cycle
                // Tick OAM DMA during halt
                if cpu.oam_dma_active {
                    if cpu.oam_dma_remaining <= 1 {
                        cpu.oam_dma_remaining = 0;
                        cpu.oam_dma_active = false;
                    } else {
                        cpu.oam_dma_remaining -= 1;
                    }
                }
            }
            let cycles = cpu.cycles;

            // Convert M-cycles to T-cycles (1 M-cycle = 4 T-cycles)
            let t_cycles = cycles * 4;

            cpu.handle_timer(t_cycles);

            cycles_this_frame += t_cycles as u32;

            // PPU logic
            cpu.update_ppu(t_cycles as u32);

            // APU logic
            cpu.update_apu(t_cycles as u32);

            if cpu.booting && cpu.program_counter == 0x100 {
                cpu.booting = false;
            }

            cpu.total_cycles += cycles as u64;
            cpu.cycles = 0;
        }

        check_keys(&mut cpu);
        ppu::draw_state(&context, &mut cpu);
        if cpu.show_vram {
            ppu::draw_vram(&vram_context, &mut cpu);
        }

        // Flush audio samples to JS
        let samples = cpu.get_audio_buffer();
        if !samples.is_empty() {
            let (left, right): (Vec<f32>, Vec<f32>) = samples
                .chunks_exact(2)
                .map(|chunk| (chunk[0], chunk[1]))
                .unzip();
            queueAudioSamples(&left, &right);
        }

        // Handle excess cycles, only if we're not paused
        if !cpu.is_paused.load(Ordering::Relaxed) {
            cycles_this_frame %= target_cycles;
        }

        request_animation_frame(f.borrow().as_ref().unwrap());
    }) as Box<dyn FnMut()>));

    request_animation_frame(g.borrow().as_ref().unwrap());
}


fn request_animation_frame(f: &Closure<dyn FnMut()>) {
    web_sys::window()
        .unwrap()
        .request_animation_frame(f.as_ref().unchecked_ref())
        .expect("should register `requestAnimationFrame` OK");
}

fn check_keys(cpu: &mut cpu::CPU) {
    let keys = KEYS.lock().unwrap();
    let mut previous_keys = PREVIOUS_KEYS.lock().unwrap();

    // Toggle keys: only trigger on newly pressed (not held)
    // space (32) - toggle pause
    if keys[32] && !previous_keys[32] {
        console::log_1(&"Toggle pause".into());
        cpu.toggle_pause();
    }
    // c (67) - toggle color mode
    if keys[67] && !previous_keys[67] {
        cpu.toggle_color_mode();
    }
    // l (76) - toggle console log
    if keys[76] && !previous_keys[76] {
        cpu.toggle_consolelog();
    }
    // n (78) - step next
    if keys[78] && !previous_keys[78] {
        cpu.set_next();
    }
    // v (86) - toggle VRAM viewer
    if keys[86] && !previous_keys[86] {
        cpu.toggle_showvram();
        toggleVramCanvas(cpu.show_vram);
    }
    // * numpad (106) - reset
    if keys[106] {
        cpu.reset();
    }
    // Tab (9) - cycle speed
    if keys[9] && !previous_keys[9] {
        cpu.cycle_speed();
    }

    // Joypad keys: 37=left, 38=up, 39=right, 40=down, 65=A, 66=B, 13=enter, 16=shift, 17=ctrl
    const JOYPAD_KEYS: [usize; 9] = [37, 38, 39, 40, 65, 66, 13, 16, 17];
    for &key in &JOYPAD_KEYS {
        cpu.set_keys(key as u32, keys[key]);
        if keys[key] {
            cpu.request_interrupt(4);
        }
    }

    *previous_keys = *keys;
}

fn setup_canvas_and_get_context(id: &str) -> web_sys::CanvasRenderingContext2d {
    let window = web_sys::window().unwrap();
    let document = window.document().unwrap();
    let canvas = document
        .get_element_by_id(id)
        .unwrap()
        .dyn_into::<web_sys::HtmlCanvasElement>()
        .unwrap();

    // VRAM canvas is 128×192 (16×24 tiles), game canvas is 160×144
    if id == "vram-canvas" {
        canvas.set_width(128);
        canvas.set_height(192);
    } else {
        canvas.set_width(160);
        canvas.set_height(144);
    }

    let context = canvas
        .get_context("2d")
        .unwrap()
        .unwrap()
        .dyn_into::<web_sys::CanvasRenderingContext2d>()
        .unwrap();

    context
}


use std::{cell::RefCell, rc::Rc};
use std::collections::HashMap;
use std::sync::{Mutex, atomic::Ordering};

use wasm_bindgen::prelude::*;
use web_sys::console;
use wasm_bindgen::JsCast;

mod cpu;
mod ppu;

lazy_static::lazy_static! {
    static ref KEYS: Mutex<HashMap<u32, bool>> = Mutex::new(HashMap::new());
    static ref PREVIOUS_KEYS: Mutex<HashMap<u32, bool>> = Mutex::new(HashMap::new());
}

// call JS function to toggle vram canvas
#[wasm_bindgen]
extern "C" {
    fn toggleVramCanvas(v: bool);
}

const TOGGLE_KEYS: [u32;5] = [32, 76, 78, 86, 106];

#[wasm_bindgen]
pub fn set_key_state(key_code: u32, is_pressed: bool) {
    let mut keys = KEYS.lock().unwrap();
    if !TOGGLE_KEYS.contains(&key_code) {
        console::log_1(&format!("Key: {} {}", key_code, is_pressed).into());
        keys.insert(key_code, is_pressed);
    }
    else if is_pressed {
        keys.insert(key_code, is_pressed);
    }
    else {
        keys.remove(&key_code);
    }

}


// This is like the `main` function, except for JavaScript.
#[wasm_bindgen(start)]
pub fn main_js() -> Result<(), JsValue> {
    let context = setup_canvas_and_get_context("rustboy-canvas");
    
    let vram_context = setup_canvas_and_get_context("vram-canvas");
    
    
    let bootrom = include_bytes!("../roms/DMG_ROM.bin");
    //let rom = include_bytes!("../roms/Dr. Mario (World).gb");
    //let rom = include_bytes!("../roms/02-interrupts.gb"); //1 ok, 2 fail, 3 ok, 4 ok, 5 ok, 6 ok, 7 ok, 8 ok, 9 ok, 10 ok, 11 ok
    let rom = include_bytes!("../roms/cpu_instrs.gb"); 
    let mut cpu = cpu::CPU::new();
    cpu.bootload(bootrom.to_vec());
    cpu.load_rom(rom.to_vec());
    
    let cpu = Rc::new(RefCell::new(cpu));
    let f = Rc::new(RefCell::new(None));
    let g = f.clone();
    let cpu_clone = cpu.clone();
    
    const CYCLES_PER_FRAME: u32 = 70224;
    const INTERRUPT_CHECK_INTERVAL: u32 = 4; // Check every 4 cycles
    
    let mut cycles_this_frame = 0;
    let mut cycles_since_interrupt_check = 0;
    
    *g.borrow_mut() = Some(Closure::wrap(Box::new(move || {
        let mut cpu = cpu_clone.borrow_mut();
        if cpu.is_paused.load(Ordering::Relaxed) && !cpu.go_next.load(Ordering::Relaxed) {
            check_keys(&mut cpu);
            request_animation_frame(f.borrow().as_ref().unwrap());
            return;
        }
        
        while cycles_this_frame < CYCLES_PER_FRAME {
            if cpu.go_next.load(Ordering::Relaxed) {
                cpu.go_next.store(false, Ordering::Relaxed);
            } else if cpu.is_paused.load(Ordering::Relaxed) {
                break;
            }
            
            let cycles = if !cpu.halt {
                cpu.execute();
                cpu.cycles
            } else {
                4 // HALT cycles
            };
            
            cycles_since_interrupt_check += cycles as u32;
            
            cpu.handle_timer(cycles);
            
            // Check for interrupts at regular intervals
            if cycles_since_interrupt_check >= INTERRUPT_CHECK_INTERVAL {
                cpu.handle_interrupts();
                cycles_since_interrupt_check -= INTERRUPT_CHECK_INTERVAL;
            }
            cycles_this_frame += cycles as u32;
            
            // PPU logic (consider implementing a more detailed PPU simulation)
            cpu.update_ppu(cycles as u32);
            
            if cpu.booting && cpu.program_counter == 0x100 {
                //cpu.bootload(rom[0..0x100].to_vec());
                cpu.booting = false;
            }
            
            check_keys(&mut cpu);
            cpu.total_cycles += cycles as u64;
            cpu.cycles=0;
        }
        
        ppu::draw_state(&context, &mut cpu);
        if cpu.show_vram {
            ppu::draw_vram(&vram_context, &mut cpu);
        }
        
        // Handle excess cycles, only if we're not paused
        if !cpu.is_paused.load(Ordering::Relaxed) {
            cycles_this_frame %= CYCLES_PER_FRAME;
            cycles_since_interrupt_check %= INTERRUPT_CHECK_INTERVAL;
        }
        
        request_animation_frame(f.borrow().as_ref().unwrap());
    }) as Box<dyn FnMut()>));

    request_animation_frame(g.borrow().as_ref().unwrap());

    Ok(())
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

    for key in keys.keys() {
        match key {
            // space
            32 => {
                if !previous_keys.contains_key(&32) {
                    console::log_1(&"Toggle pause".into());
                    cpu.toggle_pause();
                }
            }
            // l
            76 => {            
                if !previous_keys.contains_key(&76) {
                    cpu.toggle_consolelog();
                }
            }
            // n
            78 => {
                if !previous_keys.contains_key(&78) {
                    cpu.set_next();
                }
            }
            // v
            86 => {
                if !previous_keys.contains_key(&86) {
                    cpu.toggle_showvram();
                    toggleVramCanvas(cpu.show_vram);
                }
            }
            // *
            106 => {
                cpu.reset();
            }
            // joypad, request interrupt
            37 | 38 | 39 | 40 | 65 | 66 | 13 | 16 | 17 => {
                cpu.set_keys(*key, keys[key]);
                cpu.request_interrupt(4);
            }
            _ => {
            }
        }
    }
    *previous_keys = keys.clone();
}

fn setup_canvas_and_get_context(id: &str) -> web_sys::CanvasRenderingContext2d {
    let window = web_sys::window().unwrap();
    let document = window.document().unwrap();
    let canvas = document
        .get_element_by_id(id)
        .unwrap()
        .dyn_into::<web_sys::HtmlCanvasElement>()
        .unwrap();
    canvas.set_width(160);
    canvas.set_height(144);
    
    let context = canvas
    .get_context("2d")
        .unwrap()
        .unwrap()
        .dyn_into::<web_sys::CanvasRenderingContext2d>()
        .unwrap();

    context
}

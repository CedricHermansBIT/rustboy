//! Browser adapter. Console execution and output formats belong to the backend.
use std::{cell::{Cell, RefCell}, rc::Rc, sync::Mutex};
use wasm_bindgen::{prelude::*, Clamped, JsCast};
use web_sys::console;

use crate::emulator::{Button, Emulator, HostServices, PixelFormat, VideoFrame, VideoGeometry};
use crate::gameboy::{GameBoy, HardwareModel};
use crate::session::Session;

lazy_static::lazy_static! {
    static ref KEYS: Mutex<[bool; 256]> = Mutex::new([false; 256]);
    static ref PREVIOUS_KEYS: Mutex<[bool; 256]> = Mutex::new([false; 256]);
}

#[wasm_bindgen]
extern "C" {
    fn toggleVramCanvas(v: bool);
    fn queueAudioSamples(left: &[f32], right: &[f32], sample_rate: u32);
    fn storeSaveData(key: &str, data: &[u8]) -> bool;
    fn loadSaveData(key: &str) -> JsValue;
}

thread_local! {
    static SESSION: RefCell<Option<Rc<RefCell<Session>>>> = RefCell::new(None);
    static EMULATION_RUNNING: RefCell<bool> = RefCell::new(false);
    static DEBUG_ENABLED: Cell<bool> = const { Cell::new(false) };
    static BORDER_VISIBLE: Cell<bool> = const { Cell::new(true) };
    static VRAM_VIEW: Cell<u8> = const { Cell::new(0) };
}

#[wasm_bindgen]
pub fn set_debug_enabled(enabled: bool) {
    DEBUG_ENABLED.with(|value| value.set(enabled));
    if !enabled {
        with_gb_mut(|cpu| cpu.disable_debug_output());
        toggleVramCanvas(false);
        let mut keys = KEYS.lock().unwrap();
        for key in [76, 78, 86] { keys[key] = false; }
    }
}

#[wasm_bindgen]
pub fn get_debug_flags() -> u8 {
    let enabled = DEBUG_ENABLED.with(Cell::get) as u8;
    with_gb(|cpu| enabled | ((cpu.is_console_logging() as u8) << 1)
        | ((cpu.is_tracing() as u8) << 2) | ((cpu.show_vram as u8) << 3)).unwrap_or(enabled)
}

#[wasm_bindgen]
pub fn set_border_visible(visible: bool) {
    BORDER_VISIBLE.with(|value| value.set(visible));
    with_session_mut(|session| {
        if let Some(gb) = session.backend.debug_extension_mut().and_then(|v| v.downcast_mut::<GameBoy>()) {
            gb.set_border_visible(visible);
        }
    });
}

#[wasm_bindgen]
pub fn set_vram_view(view: &str) -> Result<(), JsValue> {
    let view = match view {
        "gb" => 0,
        "sgb-tiles" => 1,
        "sgb-border" => 2,
        _ => return Err(JsValue::from_str("Unknown VRAM view")),
    };
    VRAM_VIEW.with(|value| value.set(view));
    Ok(())
}

fn with_session<R>(f: impl FnOnce(&Session) -> R) -> Option<R> {
    SESSION.with(|slot| slot.borrow().as_ref().map(|session| f(&session.borrow())))
}
fn with_session_mut<R>(f: impl FnOnce(&mut Session) -> R) -> Option<R> {
    SESSION.with(|slot| {
        slot.borrow()
            .as_ref()
            .map(|session| f(&mut session.borrow_mut()))
    })
}

// Legacy console commands are an optional Game Boy debug extension. Other
// backends need not expose these registers, memory widths or PPU tools.
fn with_gb<R>(f: impl FnOnce(&crate::cpu::CPU) -> R) -> Option<R> {
    with_session(|session| {
        session
            .backend
            .debug_extension()
            .and_then(|debug| debug.downcast_ref::<GameBoy>())
            .map(|gb| f(gb.cpu()))
    })
    .flatten()
}
fn with_gb_mut<R>(f: impl FnOnce(&mut crate::cpu::CPU) -> R) -> Option<R> {
    with_session_mut(|session| {
        session
            .backend
            .debug_extension_mut()
            .and_then(|debug| debug.downcast_mut::<GameBoy>())
            .map(|gb| f(gb.cpu_mut()))
    })
    .flatten()
}

#[wasm_bindgen]
pub fn set_key_state(key_code: u32, pressed: bool) {
    if let Some(key) = KEYS.lock().unwrap().get_mut(key_code as usize) {
        *key = pressed;
    }
}

#[wasm_bindgen]
pub fn load_rom_data(rom: &[u8], boot_rom: &[u8]) -> Result<(), JsValue> {
    load_rom_data_with_model(rom, boot_rom, "auto")
}

#[wasm_bindgen]
pub fn load_rom_data_with_model(rom: &[u8], boot_rom: &[u8], model: &str) -> Result<(), JsValue> {
    let model = match model {
        "auto" => HardwareModel::Auto,
        "dmg" => HardwareModel::Dmg,
        "cgb" => HardwareModel::Cgb,
        "sgb" => HardwareModel::Sgb,
        _ => return Err(JsValue::from_str("Unknown Game Boy hardware model")),
    };
    // Construct and validate before replacing the running session or its saves.
    let mut backend = GameBoy::load_with_host(
        rom,
        boot_rom,
        model,
        HostServices {
            now_unix_seconds: || (js_sys::Date::now() / 1000.0) as u64,
            log: Some(|message| console::log_1(&message.into())),
        },
    )
    .map_err(|error| JsValue::from_str(&error))?;
    backend.set_border_visible(BORDER_VISIBLE.with(Cell::get));
    toggleVramCanvas(false);
    save_game();
    let previous_speed = get_speed();
    console::log_1(&format!("Loaded {}: {}", backend.system_name(), backend.title()).into());
    if let Some(info) = backend.save_info() {
        let mut saved = loadSaveData(&info.key);
        if saved.is_undefined() || saved.is_null() {
            // Keep the recoverable old title-only save during migration.
            saved = loadSaveData(&format!("rustboy_save_{}", backend.title()));
        }
        if let Ok(array) = saved.dyn_into::<js_sys::Uint8Array>() {
            backend
                .import_save(&array.to_vec())
                .map_err(|error| JsValue::from_str(&error))?;
            storeSaveData(&info.key, &backend.export_save());
        }
    }
    let mut session = Session::new(Box::new(backend));
    session.set_speed(previous_speed);
    *KEYS.lock().unwrap() = [false; 256];
    *PREVIOUS_KEYS.lock().unwrap() = [false; 256];
    SESSION.with(|slot| *slot.borrow_mut() = Some(Rc::new(RefCell::new(session))));
    EMULATION_RUNNING.with(|running| {
        if !*running.borrow() {
            *running.borrow_mut() = true;
            start_emulation_loop();
        }
    });
    Ok(())
}

#[wasm_bindgen]
pub fn save_game() {
    with_session_mut(|session| {
        if let Some(info) = session.backend.save_info() {
            if info.dirty && storeSaveData(&info.key, &session.backend.export_save()) {
                session.backend.mark_save_clean();
            }
        }
    });
}
#[wasm_bindgen]
pub fn export_state() -> Vec<u8> {
    with_session(|s| s.backend.export_state()).unwrap_or_default()
}
#[wasm_bindgen]
pub fn import_state(data: &[u8]) -> Result<(), JsValue> {
    with_session_mut(|s| s.import_state(data))
        .unwrap_or_else(|| Err("No ROM is loaded".into()))
        .map_err(|error| JsValue::from_str(&error))
}
#[wasm_bindgen]
pub fn get_state_id() -> String {
    with_session(|s| s.backend.state_id()).unwrap_or_default()
}
#[wasm_bindgen]
pub fn get_rom_title() -> String {
    with_session(|s| s.backend.title()).unwrap_or_default()
}
#[wasm_bindgen]
pub fn get_save_key() -> String {
    with_session(|s| s.backend.save_key()).unwrap_or_default()
}
#[wasm_bindgen]
pub fn export_save_data() -> Vec<u8> {
    with_session(|s| s.backend.export_save()).unwrap_or_default()
}
#[wasm_bindgen]
pub fn import_save_data(data: &[u8]) -> Result<(), JsValue> {
    with_session_mut(|s| s.backend.import_save(data))
        .unwrap_or_else(|| Err("No ROM is loaded".into()))
        .map_err(|error| JsValue::from_str(&error))
}
#[wasm_bindgen]
pub fn clear_save_data() {
    with_session_mut(|s| s.backend.clear_save());
}
#[wasm_bindgen]
pub fn set_paused(paused: bool) {
    with_session_mut(|s| s.backend.set_paused(paused));
}
#[wasm_bindgen]
pub fn is_paused() -> bool {
    with_session(|s| s.backend.paused()).unwrap_or(false)
}
#[wasm_bindgen]
pub fn reset_emulator() {
    with_session_mut(Session::reset);
}
#[wasm_bindgen]
pub fn set_speed(speed: u32) {
    with_session_mut(|s| s.set_speed(speed));
}
#[wasm_bindgen]
pub fn get_speed() -> u32 {
    with_session(|s| s.speed).unwrap_or(1)
}
#[wasm_bindgen]
pub fn get_debug_state() -> String {
    with_gb(|cpu| cpu.get_debug_state()).unwrap_or_else(|| "No Game Boy debugger loaded".into())
}
#[wasm_bindgen]
pub fn get_is_cgb() -> bool {
    with_gb(|cpu| cpu.is_cgb).unwrap_or(false)
}

#[wasm_bindgen]
pub fn get_sgb_status() -> String {
    with_gb(|cpu| {
        let Some(sgb) = &cpu.sgb else {
            return "SGB mode is off".into();
        };
        let unsupported = sgb
            .unsupported
            .iter()
            .enumerate()
            .filter(|(_, count)| **count != 0)
            .map(|(code, count)| format!("${code:02X}: {count}"))
            .collect::<Vec<_>>()
            .join(", ");
        format!(
            "SGB HLE: functions {}, players {}, commands {}; unsupported [{}]; border {}, pending transfers {}, dropped transfers {}; screen mask {}; rejected pulses {}",
            if sgb.enabled() {
                "enabled"
            } else {
                "disabled by cartridge header"
            },
            sgb.players(),
            sgb.commands_received,
            unsupported,
            if sgb.has_border() { "present" } else { "absent" },
            sgb.transfers_pending(),
            sgb.transfer_drops,
            sgb.screen_mask(),
            sgb.rejected_pulses,
        )
    })
    .unwrap_or_else(|| "No ROM is loaded".into())
}

/// Keep the replacement firmware's attribution available in binary deployments.
#[wasm_bindgen]
pub fn get_boot_rom_license() -> String {
    crate::boot_roms::LICENSE.to_owned()
}
#[wasm_bindgen]
pub fn add_breakpoint_pc(addr: u16) {
    with_gb_mut(|cpu| cpu.add_breakpoint_pc(addr));
}
#[wasm_bindgen]
pub fn add_breakpoint_reg(reg: &str, value: u16) {
    with_gb_mut(|cpu| cpu.add_breakpoint_reg(reg, value));
}
#[wasm_bindgen]
pub fn add_breakpoint_mem(addr: u16, value: u8) {
    with_gb_mut(|cpu| cpu.add_breakpoint_mem(addr, value));
}
#[wasm_bindgen]
pub fn add_breakpoint_opcode(opcode: u8) {
    with_gb_mut(|cpu| cpu.add_breakpoint_opcode(opcode));
}
#[wasm_bindgen]
pub fn add_breakpoint_cb_opcode(opcode: u8) {
    with_gb_mut(|cpu| cpu.add_breakpoint_cb_opcode(opcode));
}
#[wasm_bindgen]
pub fn remove_breakpoint(index: usize) {
    with_gb_mut(|cpu| cpu.remove_breakpoint(index));
}
#[wasm_bindgen]
pub fn clear_breakpoints() {
    with_gb_mut(|cpu| cpu.clear_breakpoints());
}
#[wasm_bindgen]
pub fn list_breakpoints() -> String {
    with_gb(|cpu| cpu.list_breakpoints()).unwrap_or_else(|| "No Game Boy debugger loaded".into())
}
#[wasm_bindgen]
pub fn peek(addr: u16) -> u8 {
    with_gb(|cpu| cpu.peek(addr)).unwrap_or(0)
}
#[wasm_bindgen]
pub fn peek_slice(start: u16, len: u16) -> String {
    with_gb(|cpu| cpu.peek_slice(start, len)).unwrap_or_default()
}
#[wasm_bindgen]
pub fn peek_regs() -> String {
    with_gb(|cpu| cpu.peek_regs()).unwrap_or_default()
}
#[wasm_bindgen]
pub fn toggle_trace() {
    if DEBUG_ENABLED.with(Cell::get) { with_gb_mut(|cpu| cpu.toggle_trace()); }
}
#[wasm_bindgen]
pub fn is_tracing() -> bool {
    with_gb(|cpu| cpu.is_tracing()).unwrap_or(false)
}
#[wasm_bindgen]
pub fn get_trace() -> String {
    with_gb(|cpu| cpu.get_trace()).unwrap_or_default()
}
#[wasm_bindgen]
pub fn clear_trace() {
    with_gb_mut(|cpu| cpu.clear_trace());
}
#[wasm_bindgen]
pub fn trace_len() -> usize {
    with_gb(|cpu| cpu.trace_buffer.len()).unwrap_or(0)
}

#[wasm_bindgen(start)]
pub fn main_js() -> Result<(), JsValue> {
    console_error_panic_hook::set_once();
    setup_canvas("rustboy-canvas", 160, 144);
    setup_canvas("vram-canvas", 128, 192);
    Ok(())
}

fn start_emulation_loop() {
    let context = setup_canvas("rustboy-canvas", 160, 144);
    let vram_context = setup_canvas("vram-canvas", 128, 192);
    let callback = Rc::new(RefCell::new(None));
    let next_callback = callback.clone();
    *next_callback.borrow_mut() = Some(Closure::wrap(Box::new(move |timestamp: f64| {
        let current = SESSION.with(|slot| slot.borrow().clone());
        if let Some(current) = current {
            let mut session = current.borrow_mut();
            session.tick(timestamp);
            check_keys(&mut session);
            draw_frame(&context, session.backend.video_frame());
            if let Some(debug) = session
                .backend
                .debug_extension()
                .and_then(|debug| debug.downcast_ref::<GameBoy>())
            {
                if DEBUG_ENABLED.with(Cell::get) && debug.cpu().show_vram {
                    let view = VRAM_VIEW.with(Cell::get);
                    let (pixels, width, height) = if let Some(sgb) = debug.cpu().sgb.as_ref().filter(|_| view != 0) {
                        if view == 1 { (sgb.debug_border_tiles(), 384, 128) }
                        else { (sgb.debug_border_map(), 256, 224) }
                    } else { (crate::ppu::debug_vram_rgba(debug.cpu()), 128, 192) };
                    draw_frame(
                        &vram_context,
                        VideoFrame {
                            geometry: VideoGeometry {
                                width,
                                height,
                                aspect_width: width,
                                aspect_height: height,
                            },
                            format: PixelFormat::Rgba8888,
                            pixels: &pixels,
                            enabled: true,
                        },
                    );
                }
            }
            let audio = session.backend.drain_audio();
            if !audio.samples.is_empty() && audio.channels > 0 {
                let channels = audio.channels as usize;
                let mut left = Vec::with_capacity(audio.samples.len() / channels);
                let mut right = Vec::with_capacity(left.capacity());
                for sample in audio.samples.chunks_exact(channels) {
                    left.push(sample[0]);
                    right.push(if channels == 1 { sample[0] } else { sample[1] });
                }
                queueAudioSamples(&left, &right, audio.sample_rate);
            }
        }
        request_animation_frame(callback.borrow().as_ref().unwrap());
    }) as Box<dyn FnMut(f64)>));
    request_animation_frame(next_callback.borrow().as_ref().unwrap());
}

fn check_keys(session: &mut Session) {
    let keys = KEYS.lock().unwrap();
    let mut previous = PREVIOUS_KEYS.lock().unwrap();
    if keys[32] && !previous[32] {
        session.backend.set_paused(!session.backend.paused());
    }
    let debug_enabled = DEBUG_ENABLED.with(Cell::get);
    if debug_enabled && keys[78] && !previous[78] {
        session.request_step();
    }
    if keys[106] && !previous[106] {
        session.reset();
    }
    if keys[9] && !previous[9] {
        session.cycle_speed();
    }
    if let Some(gb) = session
        .backend
        .debug_extension_mut()
        .and_then(|debug| debug.downcast_mut::<GameBoy>())
    {
        let cpu = gb.cpu_mut();
        if keys[67] && !previous[67] {
            cpu.toggle_color_mode();
        }
        if debug_enabled && keys[76] && !previous[76] {
            cpu.toggle_consolelog();
        }
        if debug_enabled && keys[86] && !previous[86] {
            cpu.toggle_showvram();
            toggleVramCanvas(cpu.show_vram);
        }
    }
    for (key, button) in [
        (37, Button::Left),
        (38, Button::Up),
        (39, Button::Right),
        (40, Button::Down),
        (65, Button::A),
        (66, Button::B),
        (13, Button::Start),
        (16, Button::Select),
    ] {
        let _ = session.backend.set_button(0, button, keys[key]);
    }
    *previous = *keys;
}

fn draw_frame(context: &web_sys::CanvasRenderingContext2d, frame: VideoFrame<'_>) {
    let canvas = context.canvas().unwrap();
    let geometry = frame.geometry;
    if canvas.width() != geometry.width {
        canvas.set_width(geometry.width);
    }
    if canvas.height() != geometry.height {
        canvas.set_height(geometry.height);
    }
    canvas
        .style()
        .set_property(
            "aspect-ratio",
            &format!("{} / {}", geometry.aspect_width, geometry.aspect_height),
        )
        .unwrap();
    if !frame.enabled {
        context.clear_rect(0.0, 0.0, geometry.width as f64, geometry.height as f64);
        return;
    }
    match frame.format {
        PixelFormat::Rgba8888 => {
            let image = web_sys::ImageData::new_with_u8_clamped_array_and_sh(
                Clamped(frame.pixels),
                geometry.width,
                geometry.height,
            )
            .unwrap();
            context.put_image_data(&image, 0.0, 0.0).unwrap();
        }
    }
}

fn request_animation_frame(callback: &Closure<dyn FnMut(f64)>) {
    web_sys::window()
        .unwrap()
        .request_animation_frame(callback.as_ref().unchecked_ref())
        .expect("could not schedule animation frame");
}
fn setup_canvas(id: &str, width: u32, height: u32) -> web_sys::CanvasRenderingContext2d {
    let canvas = web_sys::window()
        .unwrap()
        .document()
        .unwrap()
        .get_element_by_id(id)
        .unwrap()
        .dyn_into::<web_sys::HtmlCanvasElement>()
        .unwrap();
    canvas.set_width(width);
    canvas.set_height(height);
    canvas
        .get_context("2d")
        .unwrap()
        .unwrap()
        .dyn_into()
        .unwrap()
}

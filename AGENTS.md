# AGENTS.md — Contributor Guide for AI Agents

This guide helps AI coding agents become productive in the rustboy Game Boy emulator codebase.

## Architecture Overview

**rustboy** is a cycle-accurate Game Boy emulator written in Rust and compiled to WebAssembly for browser deployment. It also has a native SDL2 variant (mooneye-gb submodule).

### Component Breakdown

| Component | Role | Key Files | Timing |
|-----------|------|-----------|--------|
| **CPU** | Instruction execution, memory, interrupts, MBC banking | `src/cpu.rs` (~6000 lines) | 4.19 MHz (M-cycles) |
| **PPU** | Graphics rendering, palettes, VRAM access, LY timing | `src/ppu.rs` (~550 lines) | Scanline-driven, 144×160 display |
| **APU** | Audio synthesis, 4 channels (square, wave, noise) | `src/apu.rs` (~1000 lines) | 512 Hz frame sequencer |
| **Web Bridge** | WASM bindings, save persistence, debug APIs | `src/web.rs` (~500 lines) | Event-driven, localStorage integration |
| **UI/Frontend** | HTML canvas, touch controls, ROM picker | `index.html` (~1665 lines) | Vanilla JS, requestAnimationFrame loop |

**CPU ↔ PPU ↔ APU** communicate via shared state in `CPU` struct (CPU holds memory, frame buffer, gbc_palettes). CPU drives timing; PPU/APU react to clock advances.

## Building & Deployment

### Web Build (WASM)
```bash
wasm-pack build --target web
wasm-bindgen --target web --out-dir ./out ./target/wasm32-unknown-unknown/release/rustboy.wasm
# Serve index.html via http-server; built files in ./out/
```

### Native Build (mooneye-gb)
```bash
cd mooneye-gb
cargo build --release  # SDL2 GUI variant
```

## Critical Timing Concepts

**This emulator is cycle-precise.** Understand these deeply:

- **M-cycle (machine cycle)**: Base timing unit. Game Boy CPU runs at ~1.05 MHz M-cycles.
- **T-cycle (tick)**: 1 M-cycle = 4 T-cycles. Used for peripheral timing (8192 T-cycles = 1 frame sequencer tick).
- **Frame**: 70,224 T-cycles (~16.74 ms @ 60 Hz frame rate).
- **Scanline**: ~456 T-cycles per line; PPU modifies LY register during H-blank/V-blank phases.

In code: `handle_timer(t_cycles)` converts M-cycles to T-cycles; APU uses `CYCLES_PER_SAMPLE` (~95.1 T-cycles per audio sample).

## Memory & Cartridge Banking

### Address Space
- `0x0000–0x00FF`: Boot ROM (switchable via `booting` flag)
- `0x0100–0x3FFF`: Cartridge ROM Bank 0 (fixed)
- `0x4000–0x7FFF`: Cartridge ROM Bank N (switchable via MBC)
- `0x8000–0x9FFF`: VRAM (CGB has 2 banks; only 1 in DMG)
- `0xA000–0xBFFF`: External RAM (cartridge battery-backed)
- `0xC000–0xDFFF`: Internal WRAM
- `0xE000–0xFFFF`: Echo RAM (mirrors `0xC000–0xDFFF`) + I/O registers

### MBC Support
- **MBC1**: Supports mode (ROM/RAM), bank switching, RAM enable.
- **MBC2**: No RAM; MBC3 (RTC support), MBC5 (more ROM banks).
- Cartridge type detected from byte `0x147` in ROM header.

**Key fields** in CPU:
- `rom`: loaded cartridge ROM (`Vec<u8>`)
- `ram`: external RAM banks (`[[u8; 0x2000]; 16]` for MBC1/3)
- `rombank`, `rambank`: currently selected banks
- `mbc1_bank2`, `rom_bank_mask`, `ram_bank_mask`: MBC state & validation

## Debugging Infrastructure

Extensive debugging built into web interface (F12 console):

```javascript
bp(0x100)              // Breakpoint at PC
bpr("a", 0xFF)         // Break when register A = 0xFF
bpm(0xFF44, 0x90)      // Break when memory[0xFF44] = 0x90
bpd(0)                 // Delete breakpoint 0
bpl()                  // List active breakpoints

mem(0xFF44)            // Read one byte
dump(0xC000, 32)       // Hex dump 32 bytes
regs()                 // Print all registers
```

Check `cpu.rs` `check_breakpoints()`, `handle_interrupts()`, `peek_*` methods. Tracing via `toggle_trace()` records every instruction; `get_trace()` returns as newline-separated string (can be 100k+ lines).

### Instruction-Level Test Tracing (Headless)

For Mooneye/CI debugging, use the optional instruction tracer in `tests/headless.rs` + `src/debug_tracer.rs`.

- Default is **off** (no runtime overhead when not enabled)
- Enable with `TRACE_TEST=<substring>`
- Tracing now starts **after boot ROM handoff** (when `program_counter >= 0x0100`) to skip boot noise and focus on cartridge behavior

```bash
# Trace one specific test
TRACE_TEST=boot_div cargo test --test headless mooneye_boot_div_dmgabcmgb --no-default-features -- --nocapture

# Trace any test whose ROM path includes "div"
TRACE_TEST=div cargo test --test headless --no-default-features -- --nocapture
```

When enabled, the harness prints:
- Total traced instructions
- Instruction frequency stats
- Last 20 instructions with PC/opcode/registers/M-cycles

#### CSV Export

Set `TRACE_CSV=1` to write full traces to `trace_<rom-name>.csv`.

```bash
TRACE_TEST=boot_div TRACE_CSV=1 cargo test --test headless mooneye_boot_div_dmgabcmgb --no-default-features -- --nocapture
```

Useful quick checks:

```bash
# First instructions after tracing begins
head -20 trace_boot_div-dmgABCmgb.csv

# Most frequent PCs (loop hotspots)
awk -F, 'NR>1 {print $2}' trace_boot_div-dmgABCmgb.csv | sort | uniq -c | sort -rn | head
```

Key hooks:
- `tests/headless.rs`: `should_trace()`, `run_rom()`, `export_trace_if_needed()`
- `src/cpu.rs`: `set_instruction_tracing()`, `get_last_traces()`, `get_trace_statistics()`, `export_instruction_traces_csv()`, `trace_count()`
- `src/debug_tracer.rs`: trace capture format, CSV export, divergence helpers

## Key Developer Patterns

### 1. Feature-Gated Compilation
```rust
#[cfg(target_arch = "wasm32")]
use web_sys::console;

#[cfg(not(target_arch = "wasm32"))]
macro_rules! console_log { ($($t:tt)*) => { } }
```
Use this for platform-specific logging, bindings, and UI updates.

### 2. Cycle & Interrupt Handling
Every instruction:
1. `handle_interrupts()` — check IF/IE flags, trigger INT/IE pending logic
2. `execute()` — decode & run opcode (updates `cpu.cycles` in M-cycles)
3. `cpu.cycles *= 4` → T-cycles; pass to `handle_timer()`, PPU, APU
4. Reset `cpu.cycles = 0`

See `web.rs` main loop (~line 368–395) for the frame loop pattern.

### 3. Register Access Patterns
- 8-bit: `reg_a`, `reg_b`, ..., `reg_f` (flags); 16-bit pairs via methods.
- Flag register (F): bits 7–4 unused; bits 3–0 = Z, N, H, C.
- Stack pointer (`stackpointer: u16`) and program counter (`program_counter: u16`).

Use **read_reg(name: &str)** / **write_reg(name: &str, value: u16)** for symbolic access.

### 4. PPU Palette Handling
Game Boy Color uses 3 palette groups × 4 colors each:
- `gbc_palettes: [[[u8; 4]; 4]; 3]` — [BG, OBP0, OBP1] × 4 colors × RGBA
- `color_mode: u8` — 0 for GBC colors, 1 for classic DMG green
- GBC boot ROM loads 45 predefined combo palettes based on cart checksum (`ppu.rs:22–100`)

### 5. Audio Frame Sequencer
APU clocks at 512 Hz (every 8192 T-cycles). Channel 1 has sweep, all channels have envelope+length counters. Output: stereo f32 samples (~44.1 kHz) buffered in CPU's audio buffer; `queueAudioSamples(left, right)` sends to Web Audio API.

## Web Integration Points

### WASM Export Functions (in `src/web.rs`)
- `load_rom_data(rom: &[u8])` — Load ROM, init CPU, start emulation loop
- `save_game()` / ROM title persistence → localStorage
- `set_key_state(key_code: u32, pressed: bool)` — Input dispatch
- `add_breakpoint_pc(addr)`, `add_breakpoint_reg(reg, value)`, etc.
- `peek(addr)`, `peek_slice()`, `peek_regs()` — Memory inspection

### JavaScript Callbacks (defined in `index.html`)
```javascript
toggleVramCanvas(v)           // Show/hide VRAM debug canvas
queueAudioSamples(left, right) // Web Audio API integration
storeSaveData(key, data)       // Persist to localStorage
loadSaveData(key)              // Restore from localStorage
```

### ROM Picker & File Loading (`index.html:~1300–1400`)
ROMs are shown in three picker sections: Game Boy (`.gb` from `/roms/`), Game Boy Color (`.gbc` from `/roms/`), and Test ROMs (`/testroms/`, usually `.gb`). Lists are loaded from each folder's `romlist.json`. Upload or drag-and-drop supports `.gb` and `.gbc`; state persists in `localStorage['rustboy_keybindings']` and touch control layouts.

## Important Edge Cases & Pitfalls

1. **OAM DMA**: Byte-wise per-cycle transfer during HALT; blocks memory access (`oam_dma_active`, `oam_dma_remaining`, `oam_dma_index`).
2. **EI/DI Timing**: Interrupt Enable flag (IME) change is delayed one instruction (`ei_pending` flag).
3. **Falling-Edge Timer**: Timer increments on *falling edge* of selected bit in internal counter; not on write to `0xFF05`. See `handle_timer()` for falling-edge detection logic.
4. **LY Quirk**: Line 153 behavior differs across hardware revisions. Scanline vs. LY register discrepancy handled via `scanline` field.
5. **CGB vs DMG**: GBC color mode uses palette arrays; DMG uses fixed green palette. Both share core CPU logic.

## Testing & Validation

### Mooneye GB Test ROMs
Located in `testroms/artifacts/`. Run acceptance tests:
```bash
./mooneye-gb/run_acceptance_tests.sh
```
Key test categories: CPU instructions, timing, OAM DMA, PPU, MBC variants, interrupt sequencing. See `mooneye-gb/README.markdown` for full test matrix.

### Test Generation Scripts
- `generate_romlist.py` — Scans `/roms`, produces `/roms/romlist.json` (name, optional img path).
- `generate_tests.py` — Automation helper (use as reference for ROM handling).

## Code Organization Rules

1. **CPU state mutations** happen in `cpu.rs` (register updates, memory writes, cycle counts).
2. **PPU rendering** reads from CPU's VRAM/palettes; writes to `frame_buffer` (160×144 pixels as u8 RGBA tuple indices).
3. **APU output** buffered in CPU, flushed each frame to Web Audio.
4. **Web bridging** happens in `src/web.rs` only; keep platform logic isolated.
5. **Persist mutable state** (saves, settings) via localStorage or emulator struct fields.

## Performance Tips

- **Release mode mandatory**: `cargo build --release` for emulation speed (~100–200% on typical hardware).
- **Avoid console logging in hot paths**: Gated behind `consolelog` flag or conditional compilation.
- **Frame skip**: Not implemented; consider if full 60 Hz rendering is bottleneck.
- **WASM memory**: 64 KB of inline binary boot ROM + ~65 KB CPU struct + cartridge ROM (up to 16 MB) — manage carefully on low-memory devices.

## Quick Start for Code Changes

1. **Fix a CPU instruction bug?** → Modify instruction handler in `cpu.rs:execute()`, add tracing output, test against mooneye ROMs.
2. **Add a new PPU feature?** → Update palette handling or rendering logic in `ppu.rs`, ensure scanline timing alignment.
3. **Tweak APU channel?** → Modify `apu.rs` channel struct, verify frame-sequencer pacing against cycle count.
4. **Add a debug command?** → Expose via `#[wasm_bindgen] pub fn` in `web.rs`, wire in `index.html` console.
5. **UI/control changes?** → Edit `index.html`; key bindings map to Rust keyCodes via `ACTION_TO_RUST` in JS.

## Timing Bugs Fixed (April 2026)

### Critical Fixes Applied
1. **DIV Write Falling-Edge Detection** ✅
   - **Bug**: DIV (0xFF04) write was calling `check_timer_falling_edge()` AFTER resetting sys_counter, but timer falling-edge detection requires comparing old bit to new bit.
   - **Fix**: Capture the old timer output bit BEFORE resetting sys_counter, then check if there's a falling edge.
   - **Impact**: Fixed mooneye `div_timing` test and related timer tests.

2. **TAC Write Clock-Select Changes** ✅
   - **Bug**: TAC (0xFF07) write was checking falling edge AFTER updating the register, but should check based on OLD TAC configuration vs NEW configuration.
   - **Fix**: Save the old timer output bit before writing new TAC, then detect edge transition between old and new bit states.
   - **Impact**: Properly handles timer clock-select changes (bits 0-1 of TAC).

3. **TIMA Reload Write Quirks** ✅
   - **Clarification**: TIMA write behavior during reload delay cycle is properly documented and handled:
     - Writes during `tima_reloaded_this_cycle` are ignored (TMA value persists)
     - Writes during delay countdown (`tima_reload_delay > 0`) abort the reload
     - Normal writes are accepted when no reload is pending

### Test Results: 42/46 Mooneye Tests Passing (91%)

**Passing Tests (42):**
- ✅ All interrupt timing tests: `ei_timing`, `ei_sequence`, `rapid_di_ei`, `halt_ime0_*`, `halt_ime1_*`, `intr_timing`
- ✅ All timer tests: `div_timing` 
- ✅ All OAM DMA timing tests: `oam_dma_timing`, `oam_dma_restart`
- ✅ All instruction timing tests: `call_timing*`, `ret_timing*`, `rst_timing`, `jp_timing*`, `pop_timing`, `push_timing`, `ld_hl_sp_e_timing`, `add_sp_e_timing`
- ✅ All register/memory tests: `if_ie_registers`, `basic`, `bits_*`, `reg_read`
- ✅ All ROM cartridge tests: `rom_512kb`, `rom_1Mb`, `rom_2Mb`, `rom_4Mb`, `rom_8Mb`, `rom_16Mb`, `ram_64kb`, `ram_256kb`
- ✅ RETI and return timing: `reti_timing`, `reti_intr_timing`

**Failing Tests (4):**
- ❌ `mooneye_boot_div_dmgabcmgb` — DIV value after boot (boot sequence timing)
- ❌ `mooneye_boot_hwio_dmgabcmgb` — Hardware I/O after boot
- ❌ `mooneye_multicart_rom_8mb` — Multicart ROM handling (suspected non-timing issue)
- ❌ `mooneye_oam_dma_start` — OAM DMA start behavior (suspected boot/init issue)

**Not Tested (34 ignored):**
- PPU glitch tests: `m2_win_*`, `m3_*` tests (GBC color mode specific edge cases)
- DIV/serial boot tests: `boot_div_*`, `boot_hwio_*`, `boot_regs_*`
- Hardware variant tests: `di_timing-GS`, `halt_ime1_timing2-GS`, `sources-GS`

---

**Last Updated**: April 2026 | **Test Pass Rate**: 91% | **Architecture Stability**: ~85% (core CPU timing accurate) | **WASM Build**: `wasm-pack 1.x` + `wasm-bindgen 0.2.92+`




# Game Boy Advance development

SGB optimization work is on hold. The existing GB/CGB/SGB browser backend is
unchanged. `crates/gba` is a new, independently authored **headless development
backend**, not a claim of playable GBA support. No other emulator implementation,
commercial game, or Nintendo firmware is included.

## First milestone

- ARMv4T ARM/Thumb interpreter: ALU and shifter, conditions, loads/stores,
  block transfers, multiplies, branches/interworking, status registers,
  banked processor modes and IRQ/SWI/undefined exceptions.
- RAM/ROM/video memory address decoding and mirrors; video byte-write quirks;
  instruction-granular ROM WAITCNT and RAM bus costs; interrupt registers.
- Scanline/HBlank/VBlank event scheduling and interrupt requests, HALT event
  skipping, and initial bitmap modes 3/4/5 (identity coordinates only).
- Optional user-supplied 16 KiB BIOS; development direct entry into cartridge
  code without BIOS. Missing-firmware exceptions stop execution explicitly.
- Original unit/oracle tests and a synthetic cartridge that paints a mode-3
  frame, plus a bounded native diagnostic for local cartridges.

This is not yet pipeline/bus-cycle accurate. Branch refill accesses charge wait
states but do not cache a real pipeline. ROM prefetch, BIOS read protection,
open bus and bus arbitration remain unimplemented. Reserved/unpredictable
instructions are not a complete hardware oracle. Bitmap output does not yet
implement affine transforms, sprites, windows or blending. No audio is emitted.

## Run locally

```sh
cargo test -p rustboy-gba --offline
cargo run --release -p rustboy-gba --example inspect -- "path/to/game.gba" 60
# Optional third argument: a BIOS file supplied by the user.
```

The diagnostic reports instruction/clock counts, CPU/display state and the first
exception requiring missing firmware. Throughput measures this incomplete
workload, **not finished-game speed**. The local FireRed cartridge is not committed
and is not added to the public ROM picker. GBA is deliberately not advertised in
the browser until useful graphics, audio, input and compatibility exist.

The first local FireRed diagnostic reaches Thumb `SWI 01h` (`RegisterRamReset`)
at `081E3BF0h` after 20 instructions and stops there. This identifies the first
missing service; it does not establish game compatibility. The current test
suite has 20 unit tests and two original synthetic-program integration tests.
`python3 scripts/check.py --offline` includes those tests alongside the existing
GB/CGB/SGB checks; the first milestone passes all 193 native checks in that gate,
its JS/Python checks, GBA Clippy with warnings denied, and the root WASM check.
The local ROM acceptance suite is separate and has not been rerun for this
backend-only milestone.

## Remaining milestones

1. Expand ARM/Thumb instruction and exception validation; implement timers,
   four DMA channels, bus ownership, and an original BIOS-service replacement
   for boot-ROM-free deployments. Use local FireRed as a compatibility probe,
   never as a distributed fixture or the sole correctness oracle.
2. Complete PPU modes 0–5: tile and affine backgrounds, OBJ/OAM, priorities,
   windows, mosaic and color effects, affine bitmap sampling and timing quirks.
3. Implement the GBA PSG and Direct Sound FIFOs, timer-driven PCM and audio DMA;
   expose correctly clocked stereo samples through the shared frontend API.
4. Cartridge SRAM/Flash/EEPROM, save detection and persistence, validated state
   snapshots, keypad IRQ, serial/link devices and cartridge peripherals.
5. Integrate the shared emulator interface and WASM loader, GBA shoulder buttons,
   settings, save/state formats and safe speed controls. Keep the public homebrew
   selection separate from local commercial ROMs.
6. Profile native and WASM gameplay, batch events and audio, and optimize measured
   hot paths. No per-instruction allocation or dynamic bus dispatch is needed;
   the current scheduler jumps between video events rather than ticking every
   hardware clock. More accurate scheduling must precede performance shortcuts.

## Hardware references

- [ARM7TDMI Technical Reference Manual, ARM DDI 0210C](https://documentation-service.arm.com/static/5f4786a179ff4c392c0ff819)
- [GBATEK hardware documentation](https://problemkaputt.de/gbatek.htm)
- [GBA-only GBATEK documentation mirror](https://rust-console.github.io/gbatek-gbaonly/)

These are specifications, not implementation code. Tests and interpreter code
are written for this repository. Full compatibility will require substantially
more testing and implementation beyond this foundation.

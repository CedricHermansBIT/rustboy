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
  code without BIOS, using our own replacement services and IRQ shim. Unsupported
  services and exceptions stop execution explicitly.
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
unsupported exception/service. Throughput measures this incomplete
workload, **not finished-game speed**. The local FireRed cartridge is not committed
and is not added to the public ROM picker. Browser uploads now accept `.gba`
cartridges up to 32 MiB and route to this backend with a persistent development
warning, not a promise that games are playable. Drop/file preflight rejects
oversized cartridges before reading their contents. GB uploads retain an 8 MiB
limit. Header detection also recognizes GBA data uploaded with a `.bin` extension.

The initial foundation's local FireRed diagnostic reached Thumb `SWI 01h` (`RegisterRamReset`)
at `081E3BF0h` after 20 instructions and stops there. This identifies the first
missing service; it does not establish game compatibility. The current test
suite at that point had 20 unit tests and two original synthetic-program integration tests.
`python3 scripts/check.py --offline` includes those tests alongside the existing
GB/CGB/SGB checks; the first milestone passes all 193 native checks in that gate,
its JS/Python checks, GBA Clippy with warnings denied, and the root WASM check.
The local ROM acceptance suite is separate and has not been rerun for this
backend-only milestone.

## Remaining milestones

### Timer/DMA milestone

Four timers now implement reload/start semantics, all prescalers, overflow IRQs,
and cascades, using batched arithmetic. HALT skips to the next timer/video event.
All four DMA channels latch addresses/count on enable, arbitrate by priority,
stall CPU execution per transfer unit, and implement immediate/VBlank/visible
HBlank starts, transfer widths, address modes, repeat/reload and completion IRQs.
Video and timers continue during DMA. Ten new tests cover these interactions.
DMA startup phases, exact CPU register-write timing, cartridge DRQ/video capture,
and Direct Sound FIFO requests still need work; this is not cycle-exact DMA yet.

### Replacement BIOS and browser upload milestone

Original functional services now cover SoftReset, RegisterRamReset, Halt,
IntrWait/VBlankIntrWait, Div/DivArm, Sqrt, CpuSet/CpuFastSet, LZ77/RL decoding and
SoundBias. Our tiny original ARM IRQ dispatcher executes game handlers on the
emulated CPU and restores banked state. User-supplied BIOS execution remains
separate. Service timing, reset register defaults and compression edge-case
quirks are not claimed hardware-identical. Unimplemented services stop clearly.

FireRed now runs a bounded 600-frame diagnostic without an unsupported service,
but remains in a game-side interrupt polling loop around `080008BEh`; this needs
investigation. This is **not evidence of a playable game**, and tile/OBJ graphics,
audio and cartridge saves remain missing.

The shared frontend adapter includes pause/reset/input and checked version-1
snapshots: CPU/banks, timers/DMA, RAM/I/O, scheduler and rendered frame. Corrupt,
wrong-ROM/firmware and impossible peripheral states are rejected atomically;
host buttons are released on restore. Battery saves are not advertised yet.
Browser tests exercise an actual 32 MiB file upload into WASM, 240×160 bitmap
output, state restore and oversized-upload preservation. Native GBA coverage is
now 41 unit tests plus five original program tests.
The regression gate passes 217 native checks plus JS/Python checks; the existing
nine browser scenarios and the new 32 MiB GBA upload scenario pass. The GBA
scenario also switches back to the original GB backend to check coexistence.

1. Expand ARM/Thumb instruction and exception validation; finish bus arbitration
   and an original BIOS-service replacement
   for boot-ROM-free deployments. Use local FireRed as a compatibility probe,
   never as a distributed fixture or the sole correctness oracle.
2. Complete PPU modes 0–5: tile and affine backgrounds, OBJ/OAM, priorities,
   windows, mosaic and color effects, affine bitmap sampling and timing quirks.
3. Implement the GBA PSG and Direct Sound FIFOs, timer-driven PCM and audio DMA;
   expose correctly clocked stereo samples through the shared frontend API.
4. Cartridge SRAM/Flash/EEPROM, save detection and persistence, validated state
   snapshots, keypad IRQ, serial/link devices and cartridge peripherals.
5. Complete browser GBA shoulder buttons, settings, battery-save support and
   media/fault handling. Shared interface, upload routing, checked states and
   existing speed controls are connected. Keep the public homebrew
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

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
  skipping, and bitmap modes 3/4/5 (expanded by the graphics milestone below).
- Optional user-supplied 16 KiB BIOS; development direct entry into cartridge
  code without BIOS, using our own replacement services and IRQ shim. Unsupported
  services and exceptions stop execution explicitly.
- Original unit/oracle tests and a synthetic cartridge that paints a mode-3
  frame, plus a bounded native diagnostic for local cartridges.

This is not yet pipeline/bus-cycle accurate. Branch refill accesses charge wait
states but do not cache a real pipeline. ROM prefetch, BIOS read protection,
open bus and bus arbitration remain unimplemented. Reserved/unpredictable
instructions are not a complete hardware oracle. Scanline graphics and Direct
Sound are now functional baselines, not claims of raster/PWM accuracy or complete
game compatibility. PSG channels remain unimplemented.

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

### Fast-forward performance

Timer scheduling now caches derived deadlines, uses shifts for power-of-two
prescalers and avoids empty IRQ/audio work. Wide memory reads decode contiguous
RAM/ROM once, retaining byte-decoder fallback at mirrors and device boundaries.
Derived timer caches are reconstructed on BIOS reset and snapshot restore;
regression tests compare cached scheduling and wide reads against canonical state.

Browser execution runs in small clock-budget chunks with a 12-ms host allowance
at normal speed, increasing with fast-forward to a bounded 32-ms allowance.
This deliberately trades presentation refresh for throughput at higher speeds;
a fixed 12-ms allowance at every speed previously suppressed fast-forward on
busy hosts. If the host cannot sustain fast-forward, missed wall time
is dropped instead of accumulating an expanding catch-up job. Hardware events,
instruction timing and instruction overshoot remain intact. The selected speed
is a target, not a promise of 4x/8x on every device. Rendering still occurs each
scanline; this change does not skip emulated frames or disable hardware features.

To measure a local GBA cartridge in Chromium without copying it into the site:

```sh
RUSTBOY_GBA_PROFILE="path/to/game.gba" RUSTBOY_NO_BOOT=1 node tests/browser_smoke.mjs
```

The optional profile restores the same initial state for requested 1x, 4x and
8x, reporting achieved startup throughput and animation callback latencies.
It is not a benchmark of completed gameplay. The synthetic upload browser gate
also checks that 4x advances more emulated time per second than 1x, rather than
only checking the speed badge or callback liveness.

Initial optimization measurement (2026-10-03): the same 1,800-frame FireRed startup probe with
`perf record -g -e cpu-clock:u` took 14.261 s before and 11.626 s after these
changes. Both executed 505,612,801 clocks / 159,868,346 instructions and reached
the same final CPU/display state. In rebuilt Chromium WASM, the optional short
startup profile at requested 8x achieved 1.75x with 16.7-ms median / 18.4-ms p95
callbacks. These host/workload-specific measurements do not guarantee gameplay
throughput. All 272 self-contained native checks and eleven browser scenarios
pass, including responsive 8x synthetic GBA playback; GBA Clippy and WASM checks
also pass. The separate local GB ROM suite was not rerun in this milestone.

After correcting the fixed browser work ceiling, paired FireRed startup probes
from the same initial state measured 1.01x at normal speed, 1.83x at requested
4x and 1.75x at requested 8x on this host. The synthetic cartridge measured
1.09x / 4.02x at requested 1x / 4x. Fast-forward now increases the allowed host
work rather than sharing the normal-speed ceiling; actual FireRed throughput
remains host/workload limited. The self-contained native gate now has 273 checks.

### Timer/DMA milestone

Four timers now implement reload/start semantics, all prescalers, overflow IRQs,
and cascades, using batched arithmetic. HALT skips to the next timer/video event.
All four DMA channels latch addresses/count on enable, arbitrate by priority,
stall CPU execution per transfer unit, and implement immediate/VBlank/visible
HBlank starts, transfer widths, address modes, repeat/reload and completion IRQs.
Video and timers continue during DMA. Ten new tests cover these interactions.
DMA startup phases, exact CPU register-write timing, cartridge DRQ/video capture,
and precise Direct Sound request-edge timing still need work; this is not
cycle-exact DMA yet.

### Replacement BIOS and browser upload milestone

Original functional services now cover SoftReset, RegisterRamReset, Halt,
IntrWait/VBlankIntrWait, Div/DivArm, Sqrt, CpuSet/CpuFastSet, LZ77/RL decoding and
SoundBias. Our tiny original ARM IRQ dispatcher executes game handlers on the
emulated CPU and restores banked state. User-supplied BIOS execution remains
separate. Service timing, reset register defaults and compression edge-case
quirks are not claimed hardware-identical. Unimplemented services stop clearly.

FireRed now runs a bounded 600-frame diagnostic without an unsupported service,
and is sampled in a game-side interrupt polling loop around `080008BEh`.
Instruction tracing confirms IRQ delivery and execution of its VBlank callback;
sampling that loop alone does not prove the game is stuck. Its display control
still has no backgrounds enabled at the end of this probe. This is **not evidence
of a playable game**; full startup compatibility and cartridge saves need work.

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

### Scanline graphics and Direct Sound milestone

The original compositor now supports text/affine backgrounds, affine bitmap
sampling, regular/affine/double-size sprites, OBJ mapping and priorities,
palettes, transparency, regular/OBJ windows, mosaic, alpha blending and
brightness. It uses stack buffers, not per-line heap allocation. Twenty-one
original graphics tests cover these features. Internal affine reference latches,
mid-scanline effects, OBJ fetch budgets and window/mosaic hardware edge cases
remain accuracy work.

Direct Sound A/B now implement bounded signed-byte FIFOs, timer selection,
stereo routing/gain, clipping and bias/resolution quantization. DMA1/2 sound mode
uses four-word refills and fixed destinations; the scheduler samples at 32,768 Hz
and returns stereo audio through the shared backend API. PSG, precise FIFO
request/underflow behavior and variable-rate PWM filtering remain incomplete.
Version-2 snapshots preserve FIFO contents, held samples, output phase and
queued audio; version-1 snapshots remain readable. Native GBA coverage now has
68 unit tests and six original program tests, including timer/DMA PCM output
through the frontend API and replay across state restoration.

The native inspector supports bounded traces via `GBA_TRACE_IRQ=1` or
`GBA_TRACE_PC=080008b0..080008d0`; normal execution keeps tracing disabled.

### EEPROM milestone

Original serial EEPROM now supports 512-byte/8-KiB saves, DMA3 command-length
geometry detection, MSB-first commands, dummy read bits and deferred writes.
The 32-MiB cartridge mapping is restricted to the last 256 bytes of region 0Dh.
Battery imports and version-5 snapshots retain geometry and in-flight commands,
read streams and programming; older snapshots remain readable. Detection still
requires a versioned SDK tag. Programming uses the documented nominal delay,
not a claim of chip-specific or sub-access timing accuracy. Three synthetic
integration tests cover actual DMA transfers, persistence, replay and mapping.

### SRAM/Flash and local picker milestone

Local installs now have a GBA picker tab, generated from `.gba` files alongside
GB/CGB. The private manifest includes the local FireRed cartridge but remains
ignored by Git; published Pages builds remain homebrew-only.

Original backup hardware detects versioned SRAM/Flash SDK tags, supports 32 KiB
SRAM and 64/128 KiB Flash, chip-ID mode, unlock commands, byte programming,
4 KiB sector/chip erases and bank selection. Eight-bit save-bus accesses are
handled separately from wide RAM accesses. Programming/erase completion is
currently immediate; busy/toggle timing, alternate Flash-chip variants and
tag-free save detection remain work. Battery saves use ROM-hashed keys,
survive reset, and load/flush through the browser persistence API. Version-3
states preserve save bytes and in-flight Flash commands; version-1/2 states
retain current battery data when restored.

The local FireRed diagnostic now reaches its rendered GAME FREAK intro at
600 frames, with display control `5C40h`, after Flash hardware was added. It
stops at an unimplemented ObjAffineSet service at frame 897 in a longer run.
This demonstrates startup progress, not complete playability. Set
`GBA_FRAME_PNG=/tmp/new-capture.png` to capture a diagnostic frame (existing files
are not overwritten), or `GBA_TRACE_MEMORY=0300310c` for bounded halfword changes.
These local screenshots and ROM bytes are not distributed.

### Affine BIOS services milestone

Original BgAffineSet/ObjAffineSet services generate rotation/scaling matrices,
BG center offsets and strided OBJ entries for arrays. Sine values are generated
mathematically, not copied from Nintendo firmware or another emulator. Exact
firmware rounding and service timing remain unverified. Tests cover cardinal
rotations, negative scales, fractional angle handling, matrix norms, source
padding/strides, destination neighbors and actual Thumb SWI dispatch/return.

With these services, the local FireRed diagnostic reaches and renders its title
screen at 1,800 frames without a missing-service fault. The image still has
visible graphics inaccuracies; this is not proof of complete game compatibility.
Native GBA coverage now has 78 unit tests and seven original program tests.
The self-contained regression gate passes 256 native checks, its JS/Python
checks, and eleven browser scenarios, including local GBA selection and Flash
save programming/persistence/reload. GBA Clippy with warnings denied and the
root WASM check pass. This does not include the separate local GB ROM suite.

### Affine scanline accumulators milestone

BG2/BG3 now have independent signed 28-bit internal X/Y origins. Reference writes
replace the corresponding current origin; PB/PD changes affect subsequent
scanline increments rather than recalculating already elapsed lines. VBlank
and frame-start reloads preserve the public reference registers. Vertical mosaic
groups retain an origin rather than resampling all rows from changed parameters.
Scanline-edge timing, mid-dot writes and dynamic mosaic-size phases remain
unverified; this is not a dot-accurate PPU.

Version-4 states preserve both current and mosaic origins. Older version-1/2/3
states reconstruct their static affine convention before continuing with the new
latches. Tests exercise raster reference changes, per-axis halfword writes,
fractional signed wrapping, step changes, mosaic groups, reloads, state replay,
corruption rejection and migration. Native GBA coverage is 82 unit tests plus
seven program tests.

The native diagnostic accepts `GBA_START_AT=1800` for a five-frame Start press.
With this input the local FireRed probe reaches its controls introduction at
2,400 frames, without a missing-service fault. Full gameplay, audio quality,
cartridge/peripheral coverage and renderer accuracy still need validation.
The rebuilt browser bundle passes all eleven browser regression scenarios;
the self-contained gate now passes 260 native checks plus JS/Python checks.
GBA Clippy with warnings denied and the root WASM check also pass.

1. Expand ARM/Thumb instruction and exception validation; finish bus arbitration
   and an original BIOS-service replacement
   for boot-ROM-free deployments. Use local FireRed as a compatibility probe,
   never as a distributed fixture or the sole correctness oracle.
2. Validate and refine PPU modes 0–5, especially affine reference latches,
   raster effects, OBJ budgets and window/mosaic timing quirks.
3. Implement the GBA PSG and refine Direct Sound request/PWM timing; validate
   audio end to end in games and browser playback.
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

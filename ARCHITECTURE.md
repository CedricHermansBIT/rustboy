# Emulator backend architecture

The core implements Game Boy/Game Boy Color, plus an opt-in experimental
[high-level SGB adapter](SGB_SUPPORT.md). NES, SNES and full hardware-level
Super Game Boy remain future backends, not advertised as supported platforms.

## Workspace boundaries

```
rustboy                         browser/WASM exports, frontend session and pacing
  ├── rustboy-emulator-api       portable contract and injected host services
  └── rustboy-gameboy            Game Boy hardware, firmware and backend adapter
        └── rustboy-emulator-api
```

The root package keeps producing `out/rustboy.js` and `out/rustboy_bg.wasm`.
Existing `rustboy::cpu`, `ppu`, `apu`, `mbc`, `cartridge` and `debug_tracer` imports
are compatibility re-exports. Their implementations now live under
`crates/gameboy/src/`. Existing original ROM tests still exercise the hardware
directly; adapter and session tests additionally exercise the new boundary.

## Contract

`Emulator` is object-safe and can be used through `Box<dyn Emulator>`. It exposes
bounded execution, pause/debug stepping, logical controller buttons, video,
audio, battery storage and versioned save states. Constructors and cartridge
validation remain system-specific: no assumption that every console has a
Game Boy header, a boot ROM, or the same cartridge format.

Execution budgets use the backend's reported presentation-clock ticks. The
frontend converts elapsed wall time into those ticks and carries instruction
overshoot as debt. It does not operate the backend's CPU, timers or PPU. Game
Boy reports its base PPU clock even when the CGB CPU switches speed. Future
backends own their own clock domains, region settings and synchronization.

Video includes dimensions, pixel format and display aspect ratio. The first
format is RGBA byte order, independent of host endianness; Game Boy's internal
packed pixel metadata never reaches the canvas directly. The browser resizes
the canvas from frame metadata. Audio includes channel count and source sample
rate. The browser converts mono/stereo to its stereo queue and resamples to the
actual AudioContext rate, retaining phase across chunks. This is a linear
resampler, not a high-quality band-limited filter.

Input uses controller ports and logical buttons. Browser key codes, touch
bindings, hotkeys, speed multipliers and wall-clock catch-up policy belong to
the frontend. Game Boy currently translates logical buttons into its existing
internal key state; its CPU/PPU/APU layout is intentionally not rewritten.

Persistence callbacks and browser storage do not enter the backend. Game Boy
continues to produce the same battery-save/state bytes and identity keys.
Original state versions 1 and 2 remain supported. New backends must provide
their own version/ROM/hardware checks and distinct persistence namespaces.
The experimental SGB adapter wraps CPU state in a separate versioned envelope
and keeps its save-state identity distinct; battery saves stay cartridge-bound.

`HostServices` supplies a per-machine clock callback for RTCs and an optional
logger. Native defaults use SystemTime; a headless WASM default is frozen and
deterministic. The browser explicitly injects Date and console callbacks.
Callbacks are preserved through reset and are not serialized.

## Optional debugging

Game Boy's existing console commands remain available through an optional,
typed debug extension. Only these legacy debug tools and the VRAM visualization
downcast to Game Boy. Normal execution, game rendering, audio, input and storage
use the shared trait. Another backend can omit the debug hook entirely.

This avoids claiming that PC addresses, CPU registers, VRAM viewers or traces
are interchangeable across systems. A broader debugger protocol should be
designed when a second real backend supplies concrete requirements.

## Adding another backend

1. Add a console crate depending on `rustboy-emulator-api`, not on the browser.
2. Implement and test its hardware and the `Emulator` adapter headlessly.
3. Add explicit file/system detection and its loader to the frontend. Reject
   ambiguous/unsupported formats; do not guess a console only from an extension.
4. Describe video/audio/region/controller capabilities and isolate save keys.
5. Add its instruction/ROM/media tests and real-browser integration checks.

No placeholder NES/SNES crates, runtime plugin loader or universal CPU/bus/PPU
abstractions are added. Component reuse should follow real implementations.

For full SGB support, the intended lower-level composition is Game Boy hardware
plus SNES hardware/system software and an ICD2 bridge. It requires a shared
internal timeline and signal exchange, not merely passing one video frame
between two frontend-level `Emulator` objects. The interface leaves that work
inside the composed backend, where SNES CPU/audio components can later be reused.
The current SGB command interpreter deliberately implements only a subset of
the adapter behavior. It adds no placeholder SNES CPU/audio emulation.

## Verification

```
cargo test --locked --release --no-default-features --workspace --lib
cargo check --locked -p rustboy-gameboy --target wasm32-unknown-unknown
python3 scripts/build_web.py
python3 scripts/check.py --browser
```

`scripts/check.py` covers every workspace library, not only the root facade.
The Game Boy crate has no JS, DOM, SDL or audio-device dependency. Original ROM
tests keep their external firmware and fixture hashes; replacement firmware is
an independently tested, attributed deployment fallback.

# Rustboy

Rustboy is a browser-based Game Boy and Game Boy Color emulator, written in Rust
and compiled to WebAssembly. It includes keyboard/touch controls, a ROM picker,
battery saves, save states, save backups, debugging tools, and stereo audio.

Compatibility development is ongoing. See [the test results](ROM_TEST_RESULTS.md)
for the actual coverage and remaining failures; this is not a claim that every
game or hardware timing quirk is supported.

## Building

Requirements: Rust, the WASM target, Python 3.11+, and the matching
`wasm-bindgen` CLI. CI uses Rust 1.98.1 and Node.js 24 for JavaScript checks.

```sh
rustup target add wasm32-unknown-unknown
cargo install wasm-bindgen-cli --locked --version 0.2.118
python3 scripts/build_web.py
```

The build does not require game files or embed copyrighted boot ROMs.
Before playing, supply your own boot ROMs at these paths:

- `roms/dmg_boot.bin`: 256 bytes, for Game Boy games.
- `roms/cgb_boot.bin`: 2,304 bytes, for Game Boy Color games.

Serve the repository over HTTP; opening `index.html` directly is not supported:

```sh
python3 -m http.server 8000
```

Open `http://localhost:8000`, then upload or drag-and-drop your own `.gb` or
`.gbc` file. Optional library lists are read from `roms/romlist.json` and
`testroms/romlist.json`; uploading works without those lists. No ROMs or boot
ROMs are distributed by this repository.

## Saves and controls

Battery saves are stored in browser localStorage; save states use IndexedDB.
Use the save manager to export a backup before clearing browser data or moving
to another hostname/port. Saves are now keyed by ROM identity, not just title;
existing title-only battery saves are copied forward without deleting the old
copy. Small cartridge RAM saves use their actual size, with older padded saves
still accepted.

- Arrow keys: D-pad
- A: A button
- B: B button
- Enter: Start button
- Shift: Select button

Bindings and touch layouts can be customized in the UI. Pause, reset, speed,
and save-state controls are also available on screen. Audio starts after a
browser user gesture; the larger queue trades approximately 186 ms of startup
buffering at 44.1 kHz for more scheduling headroom.

## Verification

```sh
# No ROMs/BIOS required: native core, PPU, audio queue and upload validation.
python3 scripts/check.py

# Built out/ bundle, Node.js 24, and Chromium installed.
python3 scripts/check.py --browser

# Your local testroms/ fixtures and boot ROMs are required. Reports real failures.
python3 scripts/check.py --roms

# Unfiltered original ROM suite, including the known fixture failures.
cargo test --locked --release --no-default-features --test headless
```

`--roms` verifies recorded fixture/reference hashes and explicitly excludes only
the two original micro-ROM errors listed in `tests/fixture_errata.json`. It does
not suppress graphics failures or the unresolved Blargg timing ROM. CI covers
the self-contained checks, a clean WASM build, and browser initialization/error
handling; licensed local fixtures must be tested separately.

To reproduce a real-game browser smoke check with your local files:

```sh
RUSTBOY_ROM=path/to/game.gb node tests/browser_smoke.mjs
```

For instruction traces, use `TRACE_TEST=<ROM substring>` and optionally
`TRACE_CSV=1`. Graphics mismatches can be exported with
`GRAPHICS_ARTIFACT_DIR=/tmp/rustboy-graphics`. Edit the maintained helpers under
`tests/support/` and regenerate test functions with `python3 generate_tests.py`.

## Current scope

Standard ROM/RAM, MBC1 (including multicarts), MBC2, MBC3/RTC, and MBC5 cartridges
are supported. Unsupported controllers are rejected with an explanatory error.
Super Game Boy features, physical link/infrared connections, and unusual
cartridge peripherals are not part of the current validated baseline. Precise
mid-scanline tile/window behavior remains a known compatibility gap.

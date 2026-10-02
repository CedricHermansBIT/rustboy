# Rustboy

Rustboy is a browser-based Game Boy and Game Boy Color emulator, written in Rust
and compiled to WebAssembly. It includes keyboard/touch controls, a ROM picker,
battery saves, save states, save backups, debugging tools, and stereo audio.

Compatibility development is ongoing. See [the test results](ROM_TEST_RESULTS.md)
for the actual coverage and remaining failures; this is not a claim that every
game or hardware timing quirk is supported.

The project now separates its frontend, shared emulator interface and Game Boy
backend into a Cargo workspace. See [the architecture guide](ARCHITECTURE.md)
for the extension boundaries intended for future NES/SNES/SGB support.

Automatic hardware selection now recognizes **Super Game Boy-enhanced cartridges**,
including their palettes, attributes and game-provided borders. Explicit GB/CGB/SGB
overrides are available in the ROM picker. Appearance settings can hide SGB
decorations without changing emulation. Enhanced sound uses RustBoy's own
resident score player and synthesized instruments by default, without Nintendo
firmware. Uploaded sound programs run on our SPC700/DSP; original SGB sound
firmware is an optional browser-local override. Replacement timbres/effects and
exact SGB timing are not hardware-identical.
See [the SGB milestone and roadmap](SGB_SUPPORT.md).

Debug tools are off and hidden by default. Press the backtick key or **Show debug
tools** to reveal them. With tools enabled, L toggles instruction logging;
F5 stays browser refresh. H hides interface panels independently of debugging.
Unlicensed cartridges can require nonstandard banking despite ordinary headers;
see [the cartridge compatibility notes](CARTRIDGE_COMPATIBILITY.md).

## Building

Requirements: Rust, the WASM target, Python 3.11+, and the matching
`wasm-bindgen` CLI. CI uses Rust 1.98.1 and Node.js 24 for JavaScript checks.

```sh
rustup target add wasm32-unknown-unknown
cargo install wasm-bindgen-cli --locked --version 0.2.118
python3 scripts/build_web.py
```

The build includes RustBoy-branded replacement boot ROMs, with startup
animations and chimes. **Users do not need boot-ROM files to play.**
Both DMG and CGB display our own RustBoy pixel wordmark, rather than the exact Nintendo
animation. Attribution, source links and pinned hashes are in
[the replacement firmware notes](crates/gameboy/third_party/sameboy/README.md).

To use the exact original startup instead, optionally supply your own firmware:

- `roms/dmg_boot.bin`: 256 bytes, for Game Boy games.
- `roms/cgb_boot.bin`: 2,304 bytes, for Game Boy Color games.

Missing/unreachable optional firmware falls back to the bundled replacement.
An existing file of the wrong size is reported as an error. Original-firmware
boot/timing ROM tests still require local firmware; they do not substitute the
replacement or change their expected results.

Serve the repository over HTTP; opening `index.html` directly is not supported:

```sh
python3 -m http.server 8000
```

Open `http://localhost:8000`, then upload or drag-and-drop your own `.gb` or
`.gbc` file. Optional library lists are read from `roms/romlist.json` and
`testroms/romlist.json`; uploading works without those lists. No commercial game
ROMs or Nintendo boot ROMs are committed to this repository. The public deployment downloads
three licensed homebrew games into its deployment artifact (see below).
Pages uses a homebrew-only picker and automatic hardware detection; the local
GB/GBC/test categories and hardware overrides are not shown there. ROM uploads
remain available. Locally served checkouts retain the full picker.

On HTTPS/localhost, audio playback uses a dedicated AudioWorklet with a bounded
stereo buffer and short fades on underrun/recovery, so UI work does not run the
playback callback. Older/insecure contexts retain the buffered fallback.

## GitHub Pages and free homebrew

The public site includes **2048 — Game Boy Edition**, **Tobu Tobu Girl Deluxe**
and **µCity 1.3**, in a separate **Free homebrew** picker tab. Games remain
unmodified and credited to their creators. Full license notices and µCity's
matching source archive are downloadable from the credits page.

The [catalog](homebrew/catalog.json) pins download URLs, byte counts and SHA-256
hashes. ROMs/source archives are downloaded during deployment, not committed to
Git. Each homebrew ROM's size and hash are checked again before browser loading.
See [the provenance and license notes](homebrew/README.md).

To enable the included [Pages workflow](.github/workflows/pages.yml), set
**Settings → Pages → Build and deployment → Source → GitHub Actions** in the
GitHub repository. Push `main` or run the workflow manually. It builds WASM,
downloads the games, tests them, then publishes only the staged public site.
Local `roms/`, `testroms/` and external boot firmware are never copied into it.

To build and serve that same site locally:

```sh
python3 scripts/build_web.py
python3 scripts/prepare_pages.py --output dist
python3 -m http.server 8000 --directory dist
```

The output directory must be empty/new; existing contents are never silently
overwritten. Use another `--output` directory for a fresh staging run. Preparation
requires network access; once published, gameplay downloads use the site's own
origin, not a third-party host. Uploading your own games remains available.

To reproduce the native and browser homebrew smoke checks:

```sh
cargo run --locked --release --no-default-features --example homebrew_smoke -- dist/homebrew/2048.gb dist/homebrew/tobudx.gb dist/homebrew/ucity.gbc
RUSTBOY_SITE_DIR=dist RUSTBOY_BASE_PATH=/rustboy RUSTBOY_HOMEBREW=1 RUSTBOY_NO_BOOT=1 node tests/browser_smoke.mjs
```

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

# Deployed-site startup with no external firmware; synthetic game, no ROM needed.
RUSTBOY_NO_BOOT=1 node tests/browser_smoke.mjs
RUSTBOY_NO_BOOT=1 RUSTBOY_SYNTHETIC_CGB=1 node tests/browser_smoke.mjs

# Your local testroms/ fixtures and boot ROMs are required. Reports real failures.
python3 scripts/check.py --roms

# Unfiltered original ROM suite, including the known fixture failures.
cargo test --locked --release --no-default-features --test headless
```

`--roms` verifies recorded fixture/reference hashes and explicitly excludes only
the two original micro-ROM errors listed in `tests/fixture_errata.json`. It does
not suppress the Blargg standalone timing ROM's documented logger overflow.
All 31 Mealybug graphics captures currently pass; the unfiltered ROM run
reports 604 passed, 3 original fixture failures, and 24 ignored testbenches/SGB
cases. CI covers
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
mid-scanline tile/window behavior is checked against all 31 bundled Mealybug
hardware captures, which currently pass. Other games, hardware revisions, and
uncovered timing sequences may still reveal compatibility issues.

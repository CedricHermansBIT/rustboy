# SameBoy-derived replacement firmware

RustBoy's built-in boot ROMs are **modified replacements, not Nintendo firmware
dumps**. Both models display original RustBoy artwork. The source, generated hex
and visual test references are in [`../../bootroms/`](../../bootroms/).
DMG slides the wordmark into view; CGB uses a color-wave animation. Both play a
chime and execute on the emulated CPU before handing off to the cartridge.

Initialization and compatibility code were adapted from
[SameBoy v1.0.3](https://github.com/LIJI32/SameBoy/tree/v1.0.3/BootROMs).
Original helper files (`sameboot.inc`, `hardware.inc`, `pb12.c`) are retained
in [source/](source/) with normalized whitespace. `hardware.inc` retains its
own CC0-1.0 notice; its unused Nintendo-logo macro is omitted.
Upstream's Expat copyright and redistribution notice is retained in
[LICENSE](LICENSE), embedded in the WASM bundle, and available through
`get_boot_rom_license()`. RustBoy's modifications/artwork are covered by the
repository's GPL-3.0 license.

Changes from upstream:
- Original RustBoy pixel glyphs generated from [logo.py](../../bootroms/logo.py),
  replacing SameBoy artwork and the DMG cartridge-header logo during startup.
- DMG uses a compact slide-in animation so code plus artwork fit in 256 bytes.
  After the intro it restores the cartridge's header-logo tiles in VRAM for
  games that inspect/reuse them; this requires no embedded Nintendo artwork.
- CGB uses a sequential three-row tilemap without the upstream E/B tile reuse
  or cartridge-logo subtitle. Cartridge-header tiles and per-game palettes
  remain available for compatibility after handoff.
- No embedded Nintendo logo, firmware dump, or external font/image dependency.

## Rebuilding

Ordinary Cargo/WASM builds decode the checked-in hex at compile time and need
**no network, C compiler or RGBDS**. To modify/rebuild the firmware, install
RGBDS **1.0.4** and a C99 compiler, then run:

```sh
python3 scripts/build_bootroms.py
# Or point at a locally built RGBDS checkout:
python3 scripts/build_bootroms.py --rgbds-dir /path/to/rgbds
# Check byte-for-byte reproducibility without changing files:
python3 scripts/build_bootroms.py --rgbds-dir /path/to/rgbds --check
```

The script builds in a temporary directory; it regenerates hex and wordmark
silhouettes. Both the native boot test and the no-firmware browser tests verify
the actual rendered RustBoy silhouette, not just nonblank output.

| File | Bytes | SHA-256 |
| --- | ---: | --- |
| dmg_boot.hex | 256 | e79e658710af41422b89d1e513bd71a6bd2f9d8fe590a3dae070c25e7f71cf11 |
| cgb_boot.hex | 2304 | e356af876376ec1ba838b7689957360f1d4422c11bae2eed2ed33eb0ee7a1b2a |

Hashes are checked by `scripts/check.py`. Optional external firmware still takes
precedence. Original-firmware boot/timing tests keep using locally supplied
firmware and unchanged fixtures; RustBoy replacements do not promise the exact
Nintendo startup sequence or boot-cycle timing.

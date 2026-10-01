# Automated ROM test results

Verified on 2026-10-01 in the original checkout. ROM fixtures remain unchanged.

| Suite | Passed | Failed | Ignored |
| --- | ---: | ---: | ---: |
| Mooneye | 46 | 0 | 3 |
| Blargg | 43 | 1 | 0 |
| GBMicrotest (including raw-byte publishers) | 484 | 2 | 21 |
| Mealybug screenshot comparisons | 25 | 6 | 0 |
| Total registered ROMs | 598 | 9 | 24 |

The ignored count has fallen from 77 to 24. This does **not** mean all newly
enabled cases pass: 6 graphics failures and one unreliable standalone Blargg ROM are now
visible instead of excluded. Only the three Super Game Boy cases are outside
the target hardware; the 21 remaining older testbenches have no curated
automated oracle yet. They are not counted as validated.

Separate checks pass: 59 core unit tests, 2 PPU regressions, 5 boot/timer
diagnostics, and both DMG/CGB Acid2 reference images. Both optional Pinball
diagnostics pass with the locally supplied games. Browser smoke checks pass
for DMG and CGB loading, nonblank rendering, pause/resume, and malformed-upload
handling. Audio queue and boot-ROM loader JavaScript regressions pass.
This is not a broad manual gameplay or listening test.

## Newly enabled graphics checks

All 31 Mealybug ROMs now capture at their `LD B,B` software breakpoint and
compare displayed RGB against the bundled upstream references, using the
matching hardware model. References and ROMs have recorded SHA-256 hashes.
The historic `mooneye_m2_*` / `mooneye_m3_*` function names are retained, but
these cases use the graphics runner, not Mooneye's register signature.

Passing: `m2_win_en_toggle`, `m3_bgp_change`, `m3_bgp_change_sprites`,
`m3_obp0_change`, `m3_scx_low_3_bits`, `m3_window_timing`, and
`m3_window_timing_wx_0`, `m3_lcdc_win_en_change_multiple`,
`m3_lcdc_bg_map_change`, and `m3_scx_high_5_bits`.
Also passing: `m3_lcdc_tile_sel_change`, `m3_scy_change`, `m3_scy_change2`,
`m3_lcdc_bg_map_change2`, `m3_lcdc_win_map_change2`,
`m3_scx_high_5_bits_change2`, `m3_wx_4_change`, `m3_wx_4_change_sprites`,
`m3_wx_5_change`, `m3_wx_6_change`, `m3_lcdc_bg_en_change2`,
`m3_lcdc_win_map_change`, `m3_lcdc_tile_sel_win_change`,
and both `m3_lcdc_obj_size_change` variants.
Remaining failures involve mid-line tile/map/scroll
fetches, BG/OBJ enable changes, and window activation/restart behavior.
They are emulator compatibility gaps, not accepted fixture exceptions.

The renderer now accounts for visible-output startup, OBJ/window stalls,
one-dot DMG palette overlap, and separately latched map/bitplane reads.
Fine scroll includes writes on the first-fetch edge and then stays latched.
Window stalls follow live WX writes before the horizontal trigger, including
the extra activation dot at WX=0 with nonzero fine scroll.
Window disabling drains previously fetched tiles; changing WX does not
relocate active window pixels, and reactivation advances the window row.
The multiple-WX/window-enable capture still differs by five pixels.
Window tiles retain their fetched map index and bitplanes across register
writes, with SCY affecting only background fetches. Window map/tile-select
captures now pass on DMG; the two CGB tile-selection captures still expose
remaining fetch-stage/bus-latch differences.
Map reads precede CPU writes recorded on the same dot. Visible OBJ stalls
must not move an already-started background map read; left-clipped objects
instead delay startup. These orderings have dedicated unit regressions.
CGB SCY propagates to the fetcher two dots after writes. Its map-address latch
phase is inferred from three independent hardware captures, not presented as
an independently documented rule. Clipped WX comparisons occur during fetch
startup; repeated comparisons insert a raw-zero FIFO pixel without consuming
tile data. OBJ size is sampled for each plane, separately from OAM selection.
DMG object-fetch cancellation retains completed alignment/fetch steps; each
object retains its own fetch clock, including colocated and left-clipped
objects. Background-enable mixer reads precede same-dot CPU writes; the CGB
compatibility path has an additional inferred propagation dot. WX comparison
has an inferred two-dot propagation delay. Tile runs split at FIFO restart
events even when no new register write occurs on that output pixel. Window
map reads, like background reads, are not retimed by visible objects that
stall an already-started transaction; clipped objects instead delay startup.
CGB KEY0 compatibility mode is locked after boot; monochrome cartridges no
longer accidentally use native palette/VRAM banking.

## Unresolved Blargg timing ROM

`blargg_n7_timing_effect` is enabled and still fails to finish. Its original
single-ROM build prints every corruption dump to an 8 KiB cartridge-RAM
stream. Diagnostic execution observed the stream reaching WRAM: the
`LD (HL),A` at PC `C3FE` wrote a space to `C000`, after exceeding its
RAM buffer. The shell itself executes from WRAM, and the eventual status
remains `80` rather than a terminal result.

The other seven OAM singles and the aggregate OAM suite pass. The overflow
explains why this standalone result is unreliable, but it is not counted as
a pass, and no original ROM or emulator memory map was altered to hide it.
The aggregate validates the same timing CRC successfully. Detailed buffer and
checksum evidence is recorded in [the standalone-ROM erratum](tests/blargg_oam_timing_erratum.md).

## Remaining failures, intentionally retained

These are suspected fixture errors, not corrected or ignored, at the user's
request. The full test command therefore still exits unsuccessfully.

- `micro_halt_op_dupe_delay`: actual DIV `01`, expected `55`.
  [Upstream source](https://github.com/aappleby/GBMicrotest/blob/master/tests/halt_op_dupe_delay.s)
  resets DIV, executes HALT with a pending interrupt and IME clear, then a NOP
  and 58 NOPs before reading DIV. Including the read, this is approximately
  64 machine cycles: DIV increments once per 64 machine cycles, so `01` is
  consistent with the instruction sequence; `55` would require thousands more.
- `micro_stat_write_glitch_l154_d`: actual IF `E1`, expected `E0`.
  [Upstream source](https://github.com/aappleby/GBMicrotest/blob/master/tests/stat_write_glitch_l154_d.s)
  clears IF before enabling the LCD, waits a full frame plus 110 machine cycles,
  and never clears the VBlank request afterward. IF bit 0 should remain set.
  Adjacent l154 tests explicitly clear IF near the point being tested; this
  fixture does not. Changing STAT must not erase an unrelated VBlank request.

Unmodified fixture SHA-256 hashes:

```text
ab1656911841d9fdcbe34aad21dc44f554e84a6eef5a82558dc27c8e9a89250c  halt_op_dupe_delay.gb
4aa12886dac7c7d7dfe17476c4f473a47c8cc36ce43ff8c9b87ea7cb99d5be61  stat_write_glitch_l154_d.gb
```


## Other fixes and release infrastructure

- DMG OAM bus corruption for reads, writes, and increment/decrement accesses;
  CGB audio power-off/length behavior. Previously excluded OAM and CGB sound
  cases are enabled.
- ROM+RAM cartridges, absent-RAM access, 2 KiB mirroring, MBC2/small-RAM
  save sizes, writable RTC carry clearing, and compact/legacy RTC trailers.
- Ten legacy micro-ROMs now check their raw VRAM/RAM result and repeating
  publisher, with expected values taken from their original source.
- Blargg cartridges with no declared RAM use their ASCII LCD console as the
  result channel; the harness does not invent cartridge RAM.
- Browser pacing follows elapsed time rather than display refresh rate.
  Unit checks cover 30/60/120/144/240 Hz.
- Boot ROMs load at runtime rather than being embedded in WASM; invalid ROMs
  and unsupported controllers report errors before replacing a running game.
- Battery saves use ROM identity, with recoverable migration of old title-only
  saves. New ROM loads and focus loss clear held input.
- Reproducible WASM build script, self-contained CI, native/JavaScript checks,
  real Chromium smoke tests, and explicit local-fixture hash validation.
- The optional CGB Pinball regression hashes displayed RGB and excludes only
  eight animated sparkle pixels; its static reference was derived from the
  original known-good menu, not the newly generated output.

## Reproduction

```sh
# All original cases: currently exits nonzero (598 pass / 9 fail / 24 ignore).
cargo test --locked --offline --release --no-default-features --test headless

# Self-contained regressions; no ROMs/BIOS needed.
python3 scripts/check.py --offline

# Local ROMs, reference images and your own boot ROMs required.
# Excludes only the two explicitly retained micro-fixture errors.
# Graphics and Blargg timing failures still make this command fail.
python3 scripts/check.py --offline --roms

# Save mismatching screenshots for inspection.
GRAPHICS_ARTIFACT_DIR=/tmp/rustboy-graphics cargo test --locked --offline \
  --release --no-default-features --test headless mooneye_m

# Regenerate maintained ROM cases, without losing runner protocols.
python3 generate_tests.py

# Rebuild browser bundle with the CLI version matching Cargo.lock.
python3 scripts/build_web.py --offline

# Real browser, optionally with a locally supplied game.
RUSTBOY_ROM=testroms/artifacts/cgb-acid2/cgb-acid2.gbc node tests/browser_smoke.mjs
```

The stereo queue is 32,768 samples per channel (was 8,192); callbacks use
4,096 samples (was 2,048). Startup/underrun recovery prebuffers 8,192 samples,
approximately 186 ms at 44.1 kHz. This trades latency for scheduling headroom.
Audible crackling under the user's original workload still requires a listening
check; automated queue tests cannot establish subjective audio quality.

# Automated ROM test results

Verified on 2026-09-30 in this checkout, with the bundled ROMs unchanged.

| Suite | Passed | Failed | Ignored |
| --- | ---: | ---: | ---: |
| Mooneye | 46 | 0 | 34 |
| Blargg | 32 | 0 | 12 |
| GBMicrotest | 474 | 2 | 31 |
| Total ROMs | 552 | 2 | 77 |

The initial combined headless run had 133 failures: 12 Mooneye, 5 Blargg,
and 116 GBMicrotest. The fixes resolve 131 of them. Five existing boot/timer
diagnostics now live in `tests/boot_diagnostics.rs`, separate from ROM cases.

The 46 pre-existing exclusions remain unchanged in scope (other hardware
models, visual-only tests, and unimplemented OAM corruption). Another 31
GBMicrotest visual/testbench ROMs have no FF82 result publisher: they previously
returned early and were incorrectly reported as passing. They are now explicitly
ignored with reasons. Missing ROM files fail instead of silently passing.

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

## Fixes

- MBC1 multicart: decode raw zero before masking the disconnected bank bit.
- Restore power-on DIV phase, DMA/palette register defaults, and unused-I/O masks.
- Preserve in-flight DMA during restart, including source-bus contention.
- Sample interrupts during opcode fetch; correct HALT wakeup and IRQ phases.
- Correct LCD startup, line boundaries, LY/LYC coincidence, STAT IRQ edges,
  read/write access gates, and fine-scroll/window/sprite fetch penalties.
- Correct DMG wave-RAM access windows and retrigger corruption phase. Batched
  APU ticks retain precise wave-fetch ages without ticking the entire APU 4x.
- ROM harness: choose DMG/CGB boot ROM correctly and read the complete Blargg
  result buffer. Long wave-test output previously truncated before `Passed`.
- Extract a maintained shared harness and curated case registry so regeneration
  preserves runner protocols and exclusion reasons.

## Verification

```sh
# Full ROM run: 552 passed, 2 failed, 77 ignored.
cargo test --locked --offline --release --no-default-features --test headless

# Everything else: 552 ROMs, 14 unit tests, 5 boot/timer diagnostics,
# and 2 PPU regressions pass. Two optional Pinball diagnostics remain ignored.
cargo test --locked --offline --release --no-default-features -- \
  --skip micro_halt_op_dupe_delay --skip micro_stat_write_glitch_l154_d

# Audio regression: prebuffer, stereo playback, underrun recovery, reset,
# and ring overflow.
node tests/audio_queue.cjs

# Browser build (out/ is regenerated with matching wasm-bindgen 0.2.118).
cargo build --locked --offline --release --target wasm32-unknown-unknown
wasm-bindgen --target web --out-dir out \
  target/wasm32-unknown-unknown/release/rustboy.wasm

# Recreate the curated ROM cases; keep helper edits in tests/support/.
python3 generate_tests.py
```

For browser audio, the stereo queue is 32,768 samples per channel (was 8,192),
callbacks are 4,096 samples (was 2,048), and startup/underrun recovery prebuffers
8,192 samples, approximately 186 ms at 44.1 kHz. This trades latency for more
scheduling headroom. Actual audible crackling still needs a browser listening
check under the workload that previously caused it.

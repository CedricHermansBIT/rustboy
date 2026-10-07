# GBA performance architecture pass

Status: active; smooth browser 8x is not achieved. Prioritized before remaining
GBA feature implementation. This is a measured refactoring plan, not a claim
that the proposed architecture already exists or guarantees 8x on every device.

## Evidence and scale (2026-10-07)

The fixed FireRed workload executes 12,269,926 guest instructions in 33,707,520
emulated clocks: about 6.1 million guest instructions/second at 1x and 49 million
at 8x. Browser observations around 1.7–1.8x imply roughly a 4.5–4.7-fold further
throughput improvement is needed. Changing a few arithmetic operations cannot
be the overall strategy.

The current native `perf` sample and Chromium sample both identify
`GameBoyAdvance::step`, `advance_video`, instruction fetch and Game Pak prefetch
as major hot areas. In the native sample, step is about 31%, advance_video about
30%, fetch16 about 6%, Prefetch::fetch about 4%, charge about 3%, and
Prefetch::idle about 2%. These are *symbol self samples*, not an exact component
budget: release inlining attributes CPU execution to step and rendering/audio/
timers to advance_video. State construction/restore/export are also included in
whole-process profiles, though outside the native benchmark's timed run. Do not
label all advance_video samples as graphics or use these percentages as precise
predictions. The initial browser profile included setup/restore and all three
speeds; the runner now profiles only the 8x execution interval, excluding state
restore and clock-reading snapshot exports. `RUSTBOY_GBA_PROFILE_STATE` imports
a local checked snapshot and uses it as the baseline for all speed samples.

An optimized native build with debug information gives better source attribution
through inlining. Approximate whole-process source totals for the fixed workload
are: bus 21%, CPU/Thumb 20%, compositor 12%, machine 11%, timers/DMA 7%, Game Pak
prefetch 5%, audio 3%. Standard-library/inlining/initialization/restore/export
samples make up much of the remainder. These are not a pure timed-run component
budget, nor a browser budget. They nevertheless contradict treating the entire
30% advance_video symbol as rendering: graphics alone is not the primary target.
Prioritize coordinated CPU/bus execution and event synchronization; a scheduler
refactor or decoded-op cache alone also cannot plausibly provide the entire
required 4.5-fold gain. Measure their combined architecture, not isolated guards.

The fixed-state 8x Chromium sample contained 3,755 samples over 4.31 seconds:
advance_video 31%, step 29%, Prefetch::fetch 9%, fetch16 6%, Prefetch::idle 4%,
charge 3%. Another 4% was actual frontend status polling in state_id, not setup
noise: it repeatedly scanned the complete ROM. Reusing the already-calculated
immutable cartridge identity removes that scan. A separate unprofiled,
sequential fixed-state browser pair measured requested 8x at 1.79x before /
1.82x after; its p95 callback interval was 52.5 ms / 35.9 ms. These single samples
are preliminary, not proof of a general throughput gain. A profiled candidate
run measured only 1.37x, illustrating variability and profiling overhead; do not
mix instrumented and uninstrumented speed samples. The architectural goal
remains unfinished.

Reproduce source-level native attribution without changing production settings:

```bash
CARGO_PROFILE_RELEASE_DEBUG=1 CARGO_TARGET_DIR=target/gba-profile cargo build --release -p rustboy-gba --example benchmark --offline
GBA_BENCH_STATE=target/gba-firered-fixed-baseline.state perf record -o target/new-gba-profile.data --call-graph dwarf -F 997 -- target/gba-profile/release/examples/benchmark "path/to/game.gba" 0 120 7
perf report --stdio --no-children --inline --sort symbol,srcline -i target/new-gba-profile.data
```

Use a new output filename; `perf` output is a local diagnostic artifact. Linux
perf permissions and debug symbols are required. For browser fixed-state runs:

```bash
RUSTBOY_GBA_UPLOAD=1 RUSTBOY_NO_BOOT=1 RUSTBOY_GBA_PROFILE="path/to/game.gba" RUSTBOY_GBA_PROFILE_STATE=target/gba-firered-fixed-baseline.state RUSTBOY_GBA_CPU_PROFILE=target/new-gba-8x.cpuprofile node tests/browser_smoke.mjs
```

The CPU-profile output is exclusive-create. Omit `RUSTBOY_GBA_CPU_PROFILE` for
throughput measurements. Saved-state input must match the loaded ROM/firmware;
the normal checked importer rejects mismatches. No game/state/firmware artifacts
are distributed by these commands.

Current architecture explains the repeated work:

- `machine.rs::run` calls the full machine step for every instruction/transfer.
- `step` checks stop, DMA, BIOS wait, halt and interrupts, executes an instruction,
  collects its bus clocks, then invokes peripheral advancement.
- `advance_video` re-evaluates video/timer/audio deadlines and advances timers,
  audio and EEPROM for every instruction, even when no observable event occurs.
- Each fetch/data access performs region/timing logic and prefetch maintenance;
  CPU execution repeatedly decodes the same ARM/Thumb instructions in loops.
- Text backgrounds decode tile information per pixel; the new scanline-local
  tile-row cache removes repeated decoding without persisting stale VRAM data.
- The backend renders every emulated visible scanline. The browser presents only
  one resulting image per callback (`src/web.rs::start_emulation_loop`). At 8x,
  constructing every intermediate image may be substantial wasted presentation
  work, but display timing and game-visible effects must still execute.

## Refactoring sequence and decision gates

1. **Establish a representative, isolated budget.** Capture deterministic states
   for title/intro, walking gameplay, battle, bitmap/affine-heavy scenes and
   DMA/audio-heavy homebrew. Use the same states for A/B, interleave trials
   without simultaneous builds/profilers, record browser/device/build settings,
   and separate CPU, peripheral scheduling, compositor, upload and audio costs.
   Improve profile attribution before choosing the largest architectural change.
   Startup measurements alone are not enough to establish gameplay performance.

2. **Event-bounded execution, not a no-event `if` around the old loop.** Refactor
   CPU execution and peripheral timekeeping around one next observable event.
   Execute CPU work up to that horizon; synchronize peripherals at event and
   MMIO barriers rather than repeatedly traversing the complete scheduler. A
   meaningful implementation needs an explicit synchronization contract across
   machine/bus ownership, not merely another guard in advance_video. Timer reads,
   timer/APU/DMA writes, IRQ changes, STOP/HALT, BIOS calls and debug stepping must
   see correctly synchronized state. Preserve existing instruction-boundary
   overshoot semantics initially, without making eventual bus-phase accuracy
   harder. Compare full state and PCM with the reference step loop at each
   boundary and after randomized access sequences.

3. **Separate pixel presentation from display hardware.** PPU clocks, affine
   latches, HBlank/VBlank IRQs, capture and DMA triggers cannot be skipped. If
   measurements justify it, add a presentation policy that avoids composing
   intermediate high-speed frames while ensuring a complete current image for
   presentation, screenshots, debugging and checked state exports. This alone
   cannot reach 8x: even hypothetically removing a whole 30% cost yields only
   1.43x overall. Keep a full-render reference mode and test final-frame parity.

4. **Reduce fetch/decode work if it remains dominant.** Explore an immutable-ROM
   decoded-block cache with bounded event exits, not a cache of instruction
   results. Actual opcode fetch timing, sequential state, prefetch queues,
   protected BIOS/open-bus context, exceptions and interrupt opportunities must
   remain real. Start with immutable ROM; executable RAM requires invalidation
   for CPU/DMA writes, aliases and state restore. Never serialize derived caches.
   Cache size and browser memory costs need a budget. Benchmark first: adding
   large decoded enums or dispatch tables may worsen instruction-cache/branch
   behavior. A WASM-compatible dynamic recompiler is a later, much larger option
   only if interpreter/block improvements cannot meet the target.

5. **Optimize bus representation from access evidence.** Consider predecoded
   WAITCNT timings and specialized common ROM/IWRAM accesses, with separate
   side-effectful MMIO/backup fallbacks. Any broader page mapping must preserve
   mirrors, memory-control RAM remapping/locks, EEPROM windows, VRAM byte rules,
   BIOS protection and open-bus behavior. Do not trade away compatibility for an
   assumed fast path.

Each stage must demonstrate a material end-to-end gain on fixed browser
workloads, explain its cost model, and pass differential state/audio validation
plus existing native, external CPU/memory and browser checks. Reject changes
whose gain is noise or which merely move a bottleneck. Keep logical commits and
publish achieved speeds rather than the selected UI multiplier. The exact order
of stages 2–5 should follow the isolated component budget, not intuition.

## Current experiment disposition

### Implemented: event-bounded CPU slices

`execution.rs` replaces per-instruction machine dispatch during ordinary CPU
execution with slices capped at the remaining run budget, video boundary, timer
overflow and audio output deadline. The final instruction retains reference
whole-instruction overshoot. Every real opcode still executes and fetches through
the original CPU/bus; no decoded-code cache or presentation skipping is involved.

The bus accumulates preceding instructions' elapsed peripheral clocks. MMIO and
EEPROM accesses synchronize that prefix **before** observing/changing registers,
then end the slice. The current instruction's clocks are applied after its side
effects, exactly matching the existing instruction-granular reference model.
An explicitly credited prefix avoids advancing timers/audio twice when video
accounting catches up. BIOS/exception helpers synchronize prior instructions
before performing direct register changes or resets. DMA units, enabled pending
IRQs, HALT/STOP and BIOS waits use the original step path. All pending time is
flushed before returning; snapshot version 11 and byte layout are unchanged.
This does not yet implement sub-instruction bus arbitration or a real pipeline.

Eight differential tests compare full snapshots (including pixels and queued
PCM), clocks, timer deadlines and DMA request masks against repeated reference
steps at irregular budgets across multiple frames. Cases include ARM/Thumb,
RAM self-modification and open-bus history, 39 byte/halfword/word register-write
combinations, timers/cascade, PSG/FIFO sound, scheduled DMA, EEPROM busy reads,
IRQ arrival/unmasking, BIOS waits/returns/reset/faults, optional original test
firmware, HALT/STOP/key wake, RAM remapping/disable and memory locking. Starting
persistence dirty flags are normalized equally; mutations during execution are
still compared. Public snapshot exports assert synchronized execution scratch
in debug builds.

Measured sequential A/B from the persisted FireRed baseline:

- Seven native trials: prior-build median 0.773316 s vs sliced 0.500198 s
  (about 55% higher throughput). Both execute 33,707,520 clocks and 12,269,926
  instructions, ending at fingerprint `12f3190d01dcf2f2`. Initial same-binary
  step/slice comparison also improved (0.811834 s / 0.540447 s).
- Unprofiled browser requested 8x: previous committed bundle 1.79x vs sliced
  2.60x (about 45% higher throughput). Requested 4x: 1.81x / 2.61x. Median
  callbacks remain around 34 ms. An earlier sliced browser sample measured
  only 2.24x; these are local fixed-start samples, not guaranteed performance
  across devices or sustained walking/battle gameplay validation.

Validation: all 379 self-contained native checks (204 GBA), warnings-denied GBA
Clippy, independent ARM/Thumb/memory test ROMs and rebuilt targeted GBA/browser
control scenarios pass. Broad GB external tests/full browser matrix were not
rerun here. **Smooth 8x remains unachieved**, with roughly another threefold
browser throughput gain needed in the latest sample. Refresh the isolated
profile to choose the next CPU/bus/decode refactor rather than assuming that the
old source budget still describes the new runner.

The benchmark now accepts `GBA_BENCH_RUNNER=step` or `slice` (default) to compare
the reference and production runners from the same binary and saved state.

The refreshed isolated Chromium profile (`target/gba-firered-event-slice-8x.cpuprofile`)
contains 3,590 samples over 4.13 seconds: advance_video 24.6%, Cpu::step 24.5%,
Prefetch::fetch 10.3%, fetch16 7.9%, run/slice orchestration 6.7%, charge 5.0%,
Prefetch::idle 4.0%. Inlining still prevents treating these as exact isolated
component costs, but CPU/fetch/bus execution remains a substantial combined
target after batching. The profiled run achieved 2.46x at requested 8x; do not
compare that directly with the unprofiled 2.60x sample. Further work must reduce
fetch/decode/bus work together and remeasure, rather than assuming an additional
scheduler guard or graphics-only change can close the remaining gap.

### Implemented: write-derived cartridge timing

`GamePakTiming` decodes WAITCNT into three N/S halfword costs, backup cost and
prefetch enable once per register update, rather than re-reading and decoding
the register during every cartridge fetch, data access and FIFO producer step.
The 32-byte derived cache is initialized on power-on, updated on either WAITCNT
byte and internal halfword writes, and rebuilt after checked state import.
FIFO flush-on-change behavior, reserved-bit masks, physical access alignment,
128-KiB page restarts, EEPROM/MMIO barriers and prefetch progress are unchanged.
No opcode/data result is cached and no clocks/events/pixels/audio are skipped.
Snapshots retain version 11 and byte layout; the cache is not serialized.

Three additional tests exhaustively compare 65,536 wait settings against the
original timing formula across all three areas, sequential/nonsequential
accesses and page/end edges; check byte/halfword/word register writes; and check
current/legacy state restoration and reset rebuilding. The complete GBA test
set has 207 passing tests. All-feature warnings-denied GBA Clippy and two
candidate browser upload matrices pass, including the original prefetch and
memory-control cartridges. Existing full-state/PCM slice-reference tests pass.

Sequential native A/B then B/A pairs, seven trials each, measured medians:
0.507960 s reference / 0.448211 s candidate, and 0.499065 s reference /
0.450788 s candidate (about 11–13% higher throughput). All 28 trials reached
33,707,520 clocks, 12,269,926 instructions and the same complete ending-state
fingerprint `12f3190d01dcf2f2` from baseline `ee9b65ac2acf1d52`.

Sequential **unprofiled** fixed-state browser A/B then B/A pairs measured:
requested 8x 2.3223x reference / 2.6511x candidate, then 2.5096x reference /
2.6074x candidate (roughly 4–14% higher throughput, with clear run variability).
Requested 4x pairs were 2.2595x / 2.6418x and 2.5509x / 2.5937x. Median callback
intervals remained about 34 ms. Temporary sites contained the normal default
builds; the compositor profiling feature was disabled and no profiler/build ran
concurrently with measured trials. These are local startup-state observations,
not walking/battle/device-wide guarantees. **Smooth 8x is still not achieved.**
Continue reducing CPU fetch/decode/bus work together; this cache is one measured
part of that architecture pass, not a substitute for it.

The complete self-contained release gate passed (382 native checks plus JS/
Python checks), as did the pinned independent ARM/Thumb/memory ROMs. The browser
bundle was rebuilt with the normal default features; no profiling boundary is
enabled in the published artifact. Rebuilt GBA upload/control browser scenarios
passed on rerun; the first attempt ended in a CDP "Inspected target navigated or
closed" error before checks completed (not an emulator assertion failure).

### Rendering worker investigation

`advance_video` is a hardware scheduler, not just a compositor: it clocks timers,
audio and EEPROM, mutates DISPSTAT/VCOUNT, raises IRQs, and schedules DMA.
Keep those events under the CPU's timing authority. A rendering worker would
instead consume stable graphics inputs; asynchronously reading live VRAM and
registers is not correct. Copying the entire 96 KiB VRAM every visible scanline
would alone move about 900 MiB/s at 60 fps, or about 7 GiB/s at 8x, before other
state and synchronization. A useful design needs ordered write deltas or
versioned snapshots, not that naive copy. Moving the entire core to a worker is
a separate responsiveness improvement, not an automatic throughput multiplier.

Added an **opt-in** `gba-profile-rendering` root feature (forwarded to the GBA
crate's `profile-rendering`) that keeps the pixel compositor out of the scheduler
through an explicit non-inlined boundary. It does not skip clocks or pixels and
is disabled in normal builds. `scripts/gba_render_profile.mjs` counts inclusive
compositor samples, including its callees, and excludes scheduler audio/timer
work. It rejects profiles without that boundary rather than reporting zero.

A fresh native source-attributed profile contains 16,389 samples: PPU source
16.37%, bus 25.72%, CPU/Thumb 23.13%, prefetch 7.75%. This includes setup and
serialization and is **not** a pure execution or browser budget. All 25 fixed
workload trials reached 33,707,520 clocks, 12,269,926 instructions and the same
complete ending-state fingerprint `12f3190d01dcf2f2`.

The isolated browser diagnostic contains 3,583 samples: compositor 737 (20.57%),
other work under `advance_video` 115 (3.21%). Its illustrative zero-cost-rendering
ceiling is 1.259x, **not a worker speed prediction**. Crucially, this diagnostic
build reached only 1.85x at requested 8x, while a subsequent normal-build control
reached 2.54x. Inlining/code-layout changes and run variability mean this is not
a precise production component budget. The normal control still attributes
24.45% to CPU step, 9.71% to fetch16, 8.57% to prefetch fetch, 4.58% to prefetch
idle and 3.97% to bus charge; its combined advance_video self samples are 23.22%.
These observations do not support a rendering-only path to smooth 8x. Continue
the CPU/fetch/bus refactor before committing to a graphics-worker architecture.

Reproduction (use a temporary site so diagnostic output never replaces `out/`):

```bash
cargo build --locked --release --target wasm32-unknown-unknown --target-dir target/gba-render-profile --features gba-profile-rendering --offline
profile_site=$(mktemp -d /tmp/rustboy-gba-render-profile-XXXXXX)
git archive HEAD | tar -x -C "$profile_site"
wasm-bindgen --target web --out-dir "$profile_site/out" target/gba-render-profile/wasm32-unknown-unknown/release/rustboy.wasm
RUSTBOY_SITE_DIR="$profile_site" RUSTBOY_GBA_UPLOAD=1 RUSTBOY_NO_BOOT=1 RUSTBOY_GBA_PROFILE="path/to/game.gba" RUSTBOY_GBA_PROFILE_STATE=target/gba-firered-fixed-baseline.state RUSTBOY_GBA_CPU_PROFILE=target/new-render-boundary.cpuprofile node tests/browser_smoke.mjs
node scripts/gba_render_profile.mjs target/new-render-boundary.cpuprofile
node --test tests/gba_render_profile_test.mjs
```

The local state must belong to the selected ROM/firmware; profile artifacts are
exclusive-create. Run a normal-build control without `RUSTBOY_SITE_DIR` and with
a fresh output filename. All 204 GBA tests with the feature, GBA all-feature
clippy with warnings denied, and the diagnostic/control browser GBA upload
matrix passed. The complete self-contained gate also passed (379 native checks
plus JavaScript/Python checks, including the three new profile-analysis tests).
The shipped bundle is unchanged. Local artifacts:
`target/gba-event-slice-render-attribution.data`,
`target/gba-render-boundary-8x.cpuprofile`, and
`target/gba-render-boundary-control-8x.cpuprofile`.

### Earlier local experiments

Condition lookup and a scheduler guard were rejected after repeat measurements.
Register hoisting alone passed correctness checks but produced no supported
browser improvement and was removed. Text tile-row caching passed 122,880
uncached-pixel comparisons spanning format, scroll, mosaic, map sizes, flips,
palette banks and invalid tile fetches. A sequential seven-trial native pair
measured medians 0.792897 s before and 0.715828 s after, with the same complete
ending state. This is preliminary native evidence; the browser sample was
1.78x at requested 4x and 1.77x at 8x, not a demonstrated browser gain. This small
change is not a substitute for the architectural pass above.

Local diagnostic artifacts (not distributed):
`target/gba-firered-text-row.cpuprofile`, `target/gba-text-row-perf.data`, and
`target/gba-firered-fixed-baseline.state`. The benchmark and independent-test
commands are documented in [GBA_SUPPORT.md](GBA_SUPPORT.md).

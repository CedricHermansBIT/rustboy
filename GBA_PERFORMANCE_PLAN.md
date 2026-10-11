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

### Instruction-mix evidence and input-driven workload capture

Added a bounded native `instruction_profile` example. It uses reference stepping
and non-mutating pre-step opcode peeks, counting only steps that actually execute
an instruction (not DMA, HALT or IRQ dispatch). BIOS/open-bus/0Dh-region
encodings are conservatively left undecoded, since a plain peek cannot reproduce
every protected/serial fetch context. Counts include failed ARM conditions and
major encoding classes, not exact execution-time attribution. The hot-PC map is
capped at 65,536 `(PC, mode, encoding)` entries, with omitted samples reported.
It prints first-hit r0–r3 values, checked starting/ending-state fingerprints and
complete instruction accounting. Instrumented runtime is **not** a benchmark.

The existing fixed state is visually an intro star animation, not walking or
battle gameplay. Of its 12,269,926 instructions, five Thumb instructions at
080008BE–080008C6 each execute 1,229,866 times: 50.12% combined. Its first observed
load reads IWRAM 0300310Ch (r2=030030F0h plus 1Ch), then add/AND/compare/
conditional-branch. This is a **RAM-flag poll**, not repeated MMIO timer reads.
ARM ALU/PSR/multiply/halfword encodings account for 22.29% and ARM branch for
3.72%; hot ARM code is in IWRAM, not immutable cartridge ROM. The reference
profile reaches exactly 33,707,520 clocks and ending fingerprint
`12f3190d01dcf2f2`, matching the uninstrumented workload.

A captured, visually checked title-screen state still spends about 47.15% of
instructions in that polling loop. It reaches 33,707,521 clocks / 12,101,648
instructions and result `7843d9920f29ae4a` from starting fingerprint
`d23c8ad69105022e`. A ROM-only decode-cache strategy would miss substantial RAM
execution; optimizing only this polling loop would overfit the existing startup
sample. Do not introduce cartridge-specific idle skips. Obtain input-driven
gameplay/battle and graphics-heavy states before judging a decoded-block design.

Input-driven progression now reaches the playable bedroom, with visually checked
player/camera movement. A settled neutral-input room state at frame 9,540 has
starting fingerprint `0aea827a62bc56bf`; a reference 120-frame sample executes
11,874,950 instructions / 33,707,520 clocks and ends at `6e7b6b77514b6c71`.
That scene spends 57.94% of instructions in the same poll, with ARM ALU-family
18.40% and ARM branch 3.04%. A separate unprofiled browser sample achieved 2.18x
at requested 4x and 2.27x at 8x. This is a stationary room, **not** continuous
walking or a battle benchmark. Do not compare its speed directly with startup
A/B results. Local state/image files remain in ignored `target/`; this validates
one progression path, not full game compatibility. Battle/affine-heavy/device
coverage remains missing, and smooth 8x remains unachieved.

The `inspect` example now supports checked state import, exclusive-create state
output and up to 256 strictly increasing, bounded input events. Events are
`relative_frame:complete_hex_key_mask`; masks use GBA key order (A=0001, B=0002,
Select=0004, Start=0008, Right=0010, Left=0020, Up=0040, Down=0080, R=0100,
L=0200). Each event applies at the next instruction boundary at/after the desired
emulated time, preserving whole-instruction overshoot. End with mask zero for
neutral captured states. Frozen/faulted runs fail rather than exporting a
supposedly completed workload; existing trace loops now stop on zero progress.
This is not a host-wall-time STOP/wake simulator. New tests cover malformed and
oversized event sequences, full-state reference replay including keypad IRQs
and clock wrap, and encoding-family labels. They are included in the normal
self-contained gate; the shipped emulator/browser bundle is unchanged.

```bash
# No copyrighted data is distributed; state and image outputs stay local.
GBA_STATE=target/local-start.state GBA_INPUT_SEQUENCE="0:0008,5:0000" GBA_SAVE_STATE=target/local-next.state GBA_FRAME_PNG=target/local-next.png cargo run --release -p rustboy-gba --example inspect --offline -- "path/to/game.gba" 300
cargo run --release -p rustboy-gba --example instruction_profile --offline -- "path/to/game.gba" target/local-next.state 120
# Optional final argument to either diagnostic: matching user-supplied BIOS.
```

Omit GBA_STATE to start from direct boot. All output files use exclusive-create,
so use fresh paths; screenshots are diagnostic and never committed. State output
is refused if the requested workload did not finish. Throughput reported by an
input/tracing diagnostic must not be mixed with isolated benchmark timings.

Validation: 385 self-contained native checks (including the three new diagnostic
checks), JS/Python checks, warnings-denied all-feature GBA Clippy, and a targeted
browser GBA upload/profile run pass. The example also reports internal PPU
window/blend controls for screenshot investigation. No emulator hot-path or
shipping bundle change was made during this investigation.

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

### Implemented: mode-specialized slices and ordinary ROM fetches (2026-10-10)

The first implementation pass after studying mGBA's active-region fetches and
mode-specific run loop specializes each CPU slice for ARM or Thumb. A mode
change ends the slice before another opcode executes; BX, exception returns and
firmware handoffs retain the reference path's instruction-boundary semantics.
The normal single-step interface selects the same mode-specific instruction
implementation. This is not a decoded-block cache or dynamic recompiler.

Ordinary in-bounds ROM fetches in regions 08h–0Ch load directly from cartridge
storage and update the real wait-state/prefetch state without traversing the
general data-memory decoder. Region 0Dh, short-ROM holes and other memory retain
the existing fallback. No clocks, instructions, flags, graphics or audio are
skipped, and no new derived state is serialized. The decoded-block experiment
remains opt-in and was disabled in every throughput measurement below.

Sequential A/B then B/A comparisons use the same local FireRed cartridge and
the persisted intro and settled-room states. Each native batch has seven trials
of 120 emulated frames, with no concurrent builds, tests or profilers:

- Intro medians: 0.489708 / 0.442469 s, then 0.488293 / 0.449435 s
  (8.6–10.7% higher throughput). All 28 trials execute 33,707,520 clocks and
  12,269,926 instructions, ending at full-state fingerprint `19a0d4d4a7d6d61f`.
- Room medians: 0.611894 / 0.575299 s, then 0.600631 / 0.571890 s
  (5.0–6.4% higher throughput). All 28 trials execute 33,707,521 clocks and
  11,950,929 instructions, ending at full-state fingerprint `e9b02d1f54bd59a5`.

Unprofiled Chromium pairs at requested 8x, each using 120 browser callbacks:

- Intro: 2.7815x / 2.8471x, then 2.8335x / 2.9195x (2.4–3.0% gain).
- Room: 2.3066x / 2.4289x, then 2.2852x / 2.4215x (5.3–6.0% gain).

The smaller browser gain is the relevant result for the product. These are
local intro and stationary-room observations, not walking, battle or broad
device coverage. Matched browser builds use rustup Rust 1.98.1 and the same
wasm-bindgen 0.2.118; native pairs use system Rust 1.95.0. Do not attribute a
comparison with older published browser observations solely to this change.
**8x remains unachieved.** Keep this bounded improvement, then target coordinated
ARM/Thumb block execution and bus/prefetch work rather than expecting another
small fetch shortcut to close the gap.

Validation also repairs stale version-11 snapshot offsets in native and browser
fixtures: the existing version-12 format adds four trailing IntrWait resume-PC
bytes. Previously failing checks now target the intended field instead of an
adjacent one. New differential tests cover repeated ARM/Thumb interworking and
ROM fetch bytes, wait timings and FIFO state across widths, mirrors, page edges,
holes, prefetch settings and memory lock. Existing slice/reference comparisons
continue checking complete snapshots including pixels and PCM.

The self-contained release gate (including JS/Python checks), all-feature GBA
tests and warnings-denied all-target/all-feature GBA Clippy pass. The pinned
independent ARM, Thumb and memory cartridges pass. All eight matched browser
runs also pass the original GBA upload/control fixtures. The normal WASM bundle
is rebuilt from the measured candidate; the full GB external-ROM suite and full
eleven-scenario browser matrix were not rerun for this GBA-only change.

Local measurement logs are `target/gba-research-native-*.log` and
`target/gba-research-browser-*.log`; native batch results are also recorded in
`target/gba-research-native-results.json`. Game data and saved states stay local.

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

### Architecture experiments: exact idle loops and scanline dependencies

This round implements two larger strategies, independently switchable through
`gba-idle-loops` and `gba-render-cache` (crate features `idle-loops` and
`render-cache`). The decoded-block experiment was measured again and remains
disabled: decoding alone does not pay for its cache lookup/validation overhead.

The idle engine learns a fixed point from two actual loop iterations. It requires
identical CPU execution state and transient bus/prefetch state, permits only
ordinary storage reads, and rejects every write and peripheral observation.
It batches complete iterations strictly before the next video, timer, audio or
budget boundary, preserving logical instruction counts and the final real
instruction's overshoot. DMA, interrupts, BIOS exceptions, restore and public
host calls invalidate the proof. It has no game-specific addresses or signatures.
The measured FireRed intro, room and movement samples replace 5.50, 6.11 and
6.00 million interpreted instructions respectively over 120 emulated frames.

The rendering experiment retains current pixels for each scanline and records a
conservative set of graphics storage pages read by its compositor. Changed bytes
advance page versions; equal writes do not. Display registers and affine origins
also gate reuse. Direct host memory edits are reconciled against a 98 KiB shadow
at run entry, BIOS bulk mutations are reconciled after the helper, and public
single-stepping uses the uncached compositor. State restore discards the cache.
All hardware scanlines, affine stepping, interrupts, DMA, timers and audio still
run. No presentation frame is skipped. Palette/OAM dependencies are registered
in groups and text tile rows register their source page once to reduce WASM
bookkeeping. Rows which repeatedly change back off dependency recording and use
the uninstrumented compositor. Additional memory is derived cache storage, not
serialized hardware state.

Initial native A/B/B/A measurements showed rendering reuse on 12,800 of 19,200
room/movement scanlines and none in the animated intro. Across six trials per
variant, median room times were 0.676 s baseline, 0.544 s idle-only and 0.384 s
combined; movement was 0.633 / 0.545 / 0.384 s. Intro was 0.515 / 0.413 / 0.456 s,
showing why a native cache win alone cannot establish a production choice.
These are the first rendering implementation's numbers, before adaptive fallback
and cheaper dependency tracking. All variants have identical clocks, instruction
counts and complete ending-state fingerprints in each scenario, including pixels,
audio buffers, peripheral state and save storage. They are native measurements,
not an 8x browser claim.

Local reproducible artifacts are `target/gba-architecture-native-results.json`,
`target/gba-architecture-browser-results.json`, the matching browser logs and
saved baseline/idle/combined sites and benchmark executables. Performance runs
are sequential, without simultaneous compilation or profiling. Browser runs use
the documented `tests/browser_smoke.mjs` workflow, identical starting snapshots
and 120 callbacks at requested 1x, 4x and 8x. Saved game/ROM data stays local.

Production selection: `gba-idle-loops` is enabled in the root default features
and the shipped `out/` WASM is rebuilt from the final idle-only candidate.
`gba-render-cache` stays experimental and disabled by default, alongside
`gba-decoded-blocks`. Both rendering implementations were tested in the browser:
neither provides a supported gain over idle-only, despite the native benefit.
The later dependency-tracking version reached 2.92x in the room and 3.63x in the
intro, versus the corresponding idle-only runs at 3.07x and 3.78x. It therefore
must not be presented as another production throughput improvement.

Two initial sequential browser comparison rounds, before native sampling, at requested 8x:

| FireRed starting state | Baseline achieved | Selected idle-only achieved |
| --- | --- | --- |
| Intro | 2.91x / 2.43x | 4.02x / 3.78x |
| Settled room | 2.35x / 2.36x | 3.08x / 3.07x |

The gain is about 30–56% in these matched rounds, with substantial intro baseline
variation. Callback p95 remains roughly 34–36 ms at requested 8x, and requested
1x remains around 1x. This is a measured improvement, **not achievement of 8x**.
A real block translator/JIT and a cheaper CPU/bus execution interface remain
larger candidates; the tested decode-only cache is insufficient.

Validation: the self-contained offline gate passes 389 native checks plus the
JavaScript/Python checks, including the updated compositor-attribution test.
GBA all-feature validation passes 221 checks across unit, program, EEPROM and
example tests; warnings-denied all-target/all-feature GBA Clippy passes.
The pinned independent ARM, Thumb and memory conformance cartridges pass with
both architecture features. Twenty browser profile runs also pass their GBA
upload/control fixtures. New differential coverage includes ARM/Thumb loops in
ROM/EWRAM/IWRAM, multiple WAITCNT configurations, timer/audio/video boundaries,
DMA flag changes, unsafe writes/MMIO reads, public RAM edits, rendering modes
0–5, raster register changes, affine/mosaic origins, forced blank, debug stepping
and restore. Cache counters/proofs are derived and do not change snapshot bytes.
The full external GB ROM suite and eleven-scenario GB/SGB browser matrix were
not rerun for this GBA change.

Rebuild the selected web configuration normally. To reproduce experiments:

```bash
# Production core in a native fixed-state benchmark:
cargo run --release -p rustboy-gba --features idle-loops --example benchmark -- path/to/game.gba 300 120 5
# Combined architecture experiment:
cargo run --release -p rustboy-gba --features idle-loops,render-cache --example benchmark -- path/to/game.gba 300 120 5
# Web reference configuration without architecture features:
cargo build --release --target wasm32-unknown-unknown --no-default-features --features web
# Web cache experiment (defaults already include idle-loops):
cargo build --release --target wasm32-unknown-unknown --features gba-render-cache
```

The direct GBA crate keeps these features opt-in for reference comparisons;
root web defaults select idle-loop acceleration. As with earlier measurements,
use the same compiler/target and saved state for A/B comparisons. A sample's
snapshot fingerprint must match before its speed difference can be interpreted.


A further generality check found a native detection tax in Mario Kart's title
fixture (no eligible loops) and V-Rally's intro (few eligible loops). Native
builds now sample CPU-active work for one frame's clocks, back off for thirty
frames when less than one eighth can be batched, and subsequently probe again.
Unsafe backedges are rejected without cloning an anchor. Ordinary reads avoid
unnecessary native flag stores, and bookkeeping is gated during backoff.
This policy is derived, resets on restore and never changes guest execution.

The six-trial-per-variant native check after introducing backoff had median times
baseline / idle-only: FireRed intro 0.553 / 0.443 s, room 0.634 / 0.501 s,
Mario Kart title 0.323 / 0.320 s, V-Rally intro 0.686 / 0.672 s. All ending-state
fingerprints match their baselines. These low-yield cases support removing the
clear detection tax, not a meaningful speedup claim for those two cartridges.
Metroid Fusion's baseline already stops at an unimplemented BIOS service and
could not be benchmarked. Artifacts: `target/gba-architecture-other-games.json`,
`target/gba-architecture-policy-native-results.json` and the local intro states.

Browser sampling variants were also tried and rejected: the duplicated-loop
version reached 2.98x intro / 1.87x room, and the shared-loop version 3.86x /
2.48x. Neither improves on the earlier browser engine. Consequently the WASM
build keeps continuous loop monitoring and its measured flag-update form;
native builds retain adaptive sampling. This is an explicit platform-specific
performance choice, not a claim that every benchmark improves universally.
Those browser trials are recorded in `target/gba-architecture-policy-browser-results.json`
and `target/gba-architecture-shared-browser-results.json`.


Final shipped-bundle check (continuous monitoring in WASM): requested 8x reaches
3.86x in the FireRed intro and 2.79x in the room. The adjacent room baseline
reaches 2.14x, a 30.3% throughput improvement. Requested 1x remains approximately
1.02x; fast-forward callback p95 is 34.8–35.1 ms. This last room pair is slower
in absolute terms than the earlier pair on both builds, reinforcing the need to
compare controls rather than promise one hardware-independent multiplier.
The final `out/rustboy_bg.wasm` is byte-identical to the measured selected
candidate (1,120,812 bytes). Final run logs/results are
`target/gba-architecture-selected-browser-results.json` and
`target/gba-architecture-browser-*-selected.log`. The full self-contained gate,
all-feature GBA suite, independent conformance and Clippy were rerun after the
final execution-policy changes and pass. Work remains uncommitted.

### Thumb-to-WASM JIT experiment (October 2026)

Added opt-in root `gba-jit` / GBA crate `jit`; production defaults and the shipped
bundle retain the interpreter. This is an actual code generator: supported Thumb
instructions become WASM arithmetic, local-register operations and branches.
It does not replay a decoded-op enum. The prototype supports immediate shifts,
add/subtract, immediate arithmetic, selected logical/comparison ALU operations,
PC-relative loads, immediate word/byte/halfword and SP-relative accesses, and
conditional/unconditional branches. ARM, executable RAM, high-register/control
operations, stack transfers and other unsupported instructions fall back.

Generated modules share the root WASM memory and import raw WASM bus functions.
Functions are installed into Rust's funcref table and invoked via `call_indirect`,
with **no JavaScript transition per block or instruction**. The root build script
makes that table growable only for the opt-in WASM build. Eight low registers and
CPSR flags stay in generated-code locals; every exit writes them back. Cpu's
explicit layout is enabled only for `jit` and checked by a unit test. Pure ALU
runs batch real opcode fetches/timing in one imported helper; that helper stops
at the exact whole-instruction budget and returns the executed prefix length.
Generated code executes only that prefix before flushing registers.

Compilation begins after 32 visits, with a 4,096-entry cache cap. Only backed ROM
in regions 08h–0Ch is eligible. Compiled code revalidates opcode words once per
public `run` epoch to catch host ROM edits. Data accesses retain the existing
bus, prefetch, open-bus, unaligned-access, write-observation and MMIO machinery;
no hardware timing is approximated. Each memory/branch instruction can exit at
an observation barrier, memory lock or hardware horizon. ROM modules can survive
a successfully validated state restore because they contain no hardware state;
a new epoch revalidates them against the restored cartridge. Cartridge changes
and reset drop the cache. Dropped modules release/recycle their owned table slots.

Validation:

- Actual generated WASM matches Rust interpreter registers, CPSR and complete
  bus event traces for **41,024 supported Thumb encodings**, plus nine mixed-block
  budgets exercising early exits: **41,033 comparisons**.
- An original Thumb cartridge matches complete serialized hardware state across
  nine budgets (1–842,688 clocks), with timer reads, MMIO barriers, enabled ROM
  prefetch, unaligned RAM loads/stores, changing VRAM and conditional branches.
- FireRed intro, settled room and movement each match complete ending snapshots
  in all six alternating JIT-off/on fixed-work trials. All trials execute the
  same 33,707,520/33,707,521 clocks for their scene; all hot modules compile.
- 197 native GBA checks with `jit,idle-loops`, the standard repository gate,
  targeted WASM GBA clippy with warnings denied, and default-browser smoke pass.
  Workspace-wide clippy additionally encounters existing SNES APU warnings;
  those unrelated files were left alone.

Results, median of three trials per mode in the **same JIT-capable bundle**:

| FireRed workload | Interpreter ms | JIT ms | Throughput change |
| --- | ---: | ---: | ---: |
| Intro | 600.2 | 842.4 | -28.8% |
| Settled room | 716.6 | 952.1 | -24.7% |
| Movement state | 709.7 | 813.5 | -12.8% |

The environment has considerable run variability (one intro JIT trial takes
2.04 seconds), so these are diagnostic scene measurements, not universal speed
predictions. The earlier JS-entry prototype compiled too little of the room
workload to help. Broader block discovery plus direct table entry compiles about
2.17 million room instructions per 120-frame trial in roughly 443,000 calls.
Batching arithmetic fetch/timing does not establish a production speed benefit.
The opt-in JIT browser reaches approximately 2.63x in the room and 2.57x in the
movement state at requested 8x. It does **not** reach 8x, and is not enabled in
`out/` or root default features.

This experiment shows that removing decode alone, even with native WASM block
entry and resident registers, is insufficient with these short blocks and the
current hardware interface. The next hypothesis to test is generation of the
ordinary-memory/timing fast path itself, together with longer translated regions
and flag liveness optimization. Merely enabling this prototype would regress
performance; larger CPU coverage without reducing that interface cost is not a
measured solution yet.

Reproduce code-generator validation:

```bash
cargo run --release -p rustboy-gba --features jit --example jit_verify --offline -- target/jit-cases.jsonl
node tests/jit_verify.mjs target/jit-cases.jsonl
cargo test --release -p rustboy-gba --features jit,idle-loops --lib --offline
```

Build the experimental bundle into an isolated site, leaving production intact:

```bash
cargo build --locked --release --features gba-jit --target wasm32-unknown-unknown --target-dir target/gba-jit-wasm --offline
wasm-bindgen --target web --out-dir target/gba-jit-site/out target/gba-jit-wasm/wasm32-unknown-unknown/release/rustboy.wasm
# Supply index.html and the site's web/assets directories in this isolated site.
RUSTBOY_SITE_DIR=target/gba-jit-site RUSTBOY_GBA_JIT_VERIFY=1 node tests/browser_smoke.mjs
# For a private game: additionally set RUSTBOY_GBA_PROFILE and
# RUSTBOY_GBA_PROFILE_STATE; never copy game/firmware into public assets.
```

The fixed-work browser mode warms the code cache, restores the exact starting
hardware snapshot, alternates interpreter/JIT ordering, checks every byte of the
ending snapshot and records executed clocks, runtime and JIT counters. Timing
excludes snapshot allocation/export. Ignored local artifacts include
`target/gba-jit-room-batched.log`, `target/gba-jit-intro-final.log`,
`target/gba-jit-movement-final.log`, `target/gba-jit-original-fixture.log` and
`target/jit-cases-final.jsonl`.

### Connected ARM IWRAM JIT (follow-up)

The default browser build now enables `gba-jit`. The negative Thumb experiment
above is superseded by an ARM region compiler; its generator and differential
fixtures remain available, but browser execution leaves Thumb to the interpreter.
No ROM-specific addresses, game code or submodules are required.

A hot observed ARM entry triggers bounded control-flow discovery (128
instructions, within 512 bytes of the entry). Generated WASM keeps all fifteen
writable registers and CPSR in locals, links both branch paths inside the module,
and returns at a whole-instruction hardware deadline or an unsupported successor.
It supports immediate-shift arithmetic/logical instructions, MUL/MLA and branches
with IWRAM targets. Fetch timing, branch refill and operand-dependent multiply
cycles execute directly in generated code. This removes per-instruction bus calls
for this guarded arithmetic region. Data loads/stores, MMIO, exceptions, register
shifts and unsupported encodings execute through the existing interpreter.

The fixed-cost fetch path requires enabled, unlocked IWRAM. Derived 256-byte code
page versions track CPU/DMA writes, including RAM aliases and BIOS scratch clearing.
A changed page revalidates instruction words and discards changed modules. Public
run epochs also catch host edits and state restores. Compiled functions use the
shared core memory/function table, release their table references on invalidation,
and fall back if compilation fails. Code page versions are not serialized.

Same-build on/off runs, alternating modes three times each from the same checked
state, ran 33,707,520/521 clocks per measurement. Every complete ending snapshot
matched byte for byte; compilation failures were zero. Median browser runtimes:

| FireRed scene | Interpreter | ARM regions | Throughput improvement |
| --- | ---: | ---: | ---: |
| Room | 733.0 ms | 624.5 ms | 17.4% |
| Movement | 650.6 ms | 578.2 ms | 12.5% |
| Intro | 503.0 ms | 399.2 ms | 26.0% |

These are within-session comparisons, not comparisons against older runs on a
possibly different machine load. Requested 8x UI playback achieved approximately
3–5x across these scenes; this change does not achieve the 8x goal. Roughly 2.3–2.8
million instructions per 120-frame workload executed in ARM regions, averaging
about 10–11 instructions per entry. Memory operations still split active regions;
next work should extend guarded RAM data access at those exits, using the same
complete-state equivalence gate.

Verification and reproduction:

```sh
cargo test -p rustboy-gba --features jit,idle-loops --lib --offline
cargo run --release -p rustboy-gba --features jit --example jit_regions_verify -- target/jit-arm-cases.jsonl
node tests/jit_regions_verify.mjs target/jit-arm-cases.jsonl
RUSTBOY_GBA_JIT_VERIFY=1 node tests/browser_smoke.mjs
```

37,569 generated ARM cases match the independent interpreter’s registers, CPSR,
instruction counts and clocks, including all writable registers and PC operands.
The 199-test GBA JIT/idle suite, full repository gate and WASM Clippy checks pass.
Original browser cartridges cover Thumb fallback timing and connected ARM code
copied into RAM, guest code rewriting, multiplication, branch paths and timer
boundaries. Twelve ARM budget cutoffs compare complete hardware snapshots. Private
scene benchmark logs are ignored local artifacts in
`target/gba-jit-arm-{room,movement,intro}.log`. The build-time opt-out is
`--no-default-features --features web,gba-idle-loops`; runtime diagnostics include
`set_gba_jit_enabled(bool)` and `get_gba_jit_statistics()`.

### Complete ARM/Thumb compiler experiment

The general WASM compiler now covers ARM7TDMI ARM and Thumb instruction families:
arithmetic and all shift forms, multiply/long multiply, branches and PC writes,
loads/stores, swaps, block transfers, banked-register/status operations and traps.
Generated code performs instruction semantics directly; hardware bus, privilege
and exception helpers preserve the existing core's behavior. Registers and flags
stay in WASM locals within a block. Ordinary branch refill no longer spills the
whole CPU state. Immutable ROM blocks can continue through stores, while stores
in mutable executing code end the block before a potentially changed successor.
RAM code validation also catches DMA writes, BIOS scratch clearing, host edits
and state restores. SWI replacement services use the actual trap address.

Complete coverage does not yet make this a fast general execution engine. Blocks
are bounded to 32 instructions and usually end at control transfers; general
memory/fetch/timing paths still call helpers. Pure arithmetic fetch spans batch
bookkeeping, with a prefix cutoff for hardware barriers and deadlines. The faster
connected ARM IWRAM compiler remains available alongside this general compiler.

206,146 generated cases match independent interpreter execution in complete CPU
state and ordered bus events. These include every Thumb encoding, systematic
and randomized ARM cases, mixed blocks with varied budgets, banked/SPSR restores
and fetch-span early exits. The 200-test GBA JIT/idle suite, repository checks and
WASM Clippy pass. Original browser fixtures compare full hardware snapshots at
timer/MMIO, memory, self-modifying-code and execution-budget boundaries.

After warming compilation, three alternating trials per mode in the same browser
build produced these median times for identical 120-frame workloads. Every
complete ending snapshot matched byte for byte across modes; compilation failures
were zero. Each workload executed 33,707,520/521 clocks.

| FireRed scene | Interpreter | Selective ARM JIT | Selective + full JIT |
| --- | ---: | ---: | ---: |
| Room | 704.5 ms | 589.9 ms | 754.8 ms |
| Movement | 810.0 ms | 642.5 ms | 828.8 ms |
| Intro | 624.1 ms | 529.2 ms | 687.7 ms |

Full mode loses approximately 22–23% throughput against selective mode (28–30%
more runtime). Its additional general blocks average only 3.2–3.5 instructions
per call in these workloads. Warm-up is outside the timed interval, so compilation
latency alone cannot explain this regression. These counters suggest that short
blocks and helper costs need attention; they do not establish a precise component
profile. Full-mode requested-8x playback measured approximately 2.35–3.08x.

Production therefore enables the selective JIT and leaves general full mode off.
Use `set_gba_full_jit_enabled(true)` for the experiment, with the overall JIT also
enabled. Statistics expose `full_calls` and `full_instructions` separately. The
production bundle includes both paths; no submodules are required.

Recommended next experiment: extend connected hot regions through guarded RAM
loads/stores, inline the common access and timing paths, and retain registers
across connected branch paths. Preserve exact event deadlines, MMIO behavior,
code invalidation and complete ending-state equivalence. Measure this bounded
experiment before expanding it. Reprofile the resulting production build to
decide whether fetch/bus work, rendering or audio becomes the next bottleneck;
the older component percentages above are not a current full-JIT budget. Full
instruction coverage alone has not established a path to 8x.

Reproduction:

```sh
cargo run --release -p rustboy-gba --features jit --example jit_full_verify -- target/jit-full-cases.jsonl
node tests/jit_full_verify.mjs target/jit-full-cases.jsonl
RUSTBOY_GBA_UPLOAD=1 RUSTBOY_GBA_JIT_VERIFY=1 node tests/browser_smoke.mjs
```

The private fixed-scene browser benchmark accepts `RUSTBOY_GBA_FULL_JIT_VERIFY=1`
to compare interpreter, selective and full modes. Final local measurements are
in ignored `target/gba-full-jit-{room,movement,intro}-final.log` files. Generated
fixtures and verification output also remain ignored under `target/`.

### Guarded RAM regions and fast-forward presentation

Connected ARM regions now include byte/halfword/word loads and stores, signed
loads and ordinary multiple-register transfers. Generated WASM accesses RAM
directly and accounts for IWRAM and configurable EWRAM wait states without
calling per-access hardware helpers. A fixed-width descriptor supplies stable
allocation pointers and the current RAM mapping. Mirrors, unaligned load
rotation, odd signed-halfword loads, register offsets, pre/post indexing,
writeback and base/destination aliases follow the interpreter.

Every runtime guard executes before the current instruction fetch or any of
its side effects. Hardware accesses, locked EWRAM, allocation crossings and
stores overlapping a code page in the running region exit to the interpreter.
Multiple transfers guard their whole span before touching memory or registers.
Other RAM writes update derived code versions, including IWRAM aliases, so
other compiled entries revalidate before execution. Registers stay resident
across transfers and connected branches. Thumb slices with general full mode
disabled now omit repeated compiler probes; JIT policy is stable during a
synchronous slice. Runtime `set_gba_jit_ram_enabled(bool)` selects independent
arithmetic-only and RAM-capable caches for comparisons.

The new differential harness compares generated WASM to the independent CPU
and real GBA bus: **13,714 cases** cover conditions, addressing modes, register
aliases, data widths, RAM mappings/waits, protected-code/hardware exits and
whole-instruction budget cutoffs, including long multiple transfers. Complete
CPU state, clocks, final instruction addresses and written RAM match; periodic
whole-RAM fingerprints also match. Original browser cartridges now exercise
RAM transfers, self-modification and timer boundaries across twelve budgets
and four execution modes. The **202-test** GBA JIT/idle suite, repository release
gate and targeted WASM Clippy checks pass. An optional root-package WASM Clippy
run also exposes nine existing lints in the unchanged SNES APU dependency.

A fresh isolated movement profile attributes 54.31% of samples to scanline
composition and 3.54% to other work under video advancement. Production-profile
video self samples are similarly high, but the diagnostic boundary can change
code layout, so these percentages remain attribution rather than an exact
production budget. An experimental priority-ordered opaque text compositor
passes full-frame comparisons covering layers, objects, windows and mosaic,
but its throughput benefit is inconsistent. It remains **off by default**;
`set_gba_fast_render_enabled(bool)` is a diagnostic control. The general full
ARM/Thumb JIT also remains off; guarded connected RAM regions are enabled.

GBA presentation can now compose one out of every N emulated frames while the
scheduler still advances every scanline, timer, audio event, IRQ and DMA request.
This policy is derived state and is absent from snapshots. Raw/headless core
execution defaults to composing every frame. The generic frontend's optional
`set_video_frame_stride` hook enables skipping during fast-forward and restores
full rendering at 1x and debug steps. Reset and state restore reapply frontend
policy. Normal 60-Hz callbacks use the requested speed as their stride; longer
callbacks increase it according to their emulation budget, up to 16. This
avoids composing extra frames that cannot be presented. Display cadence is
bounded by browser callbacks and host throughput; it is not a guaranteed 60 fps.

Reference tests exercise repeating HBlank DMA, timer IRQs, changing colors and
stride transitions. Every selected frame matches full rendering, skipped
frames retain the previous pixels, and every serialized hardware/audio byte
matches after normalizing only the host pixel buffer and its checksum.
Frontend tests cover slower callbacks, restore, reset and debug stepping.

Reproduction:

```sh
cargo run --release -p rustboy-gba --features jit --example jit_ram_verify -- target/jit-ram-cases.jsonl
node tests/jit_ram_verify.mjs target/jit-ram-cases.jsonl
cargo test -p rustboy-gba --features jit,idle-loops --lib --offline
RUSTBOY_GBA_UPLOAD=1 RUSTBOY_GBA_JIT_VERIFY=1 node tests/browser_smoke.mjs
```

Private fixed-scene comparisons use `RUSTBOY_GBA_RAM_JIT_VERIFY=1` with the same
ROM/state inputs documented above. They alternate five execution/rendering
policies three times each, warm both code caches, and verify complete snapshots
when every frame is composed. For skipped presentation they compare every byte
except the host RGBA buffer and checksum; this explicitly includes VRAM, audio,
CPU, timers, DMA and save-device state. Native tests separately verify selected
pixels. Runtime diagnostics include `set_gba_video_stride(u32)` and
`get_gba_render_statistics()`. Final logs are ignored local files under
`target/gba-frame-skip-{room,movement,intro}.log`.

Final production-build measurements, with the experimental compositor and full
JIT disabled, warmed caches and three alternating trials per mode:

| FireRed scene | Arithmetic regions, every frame | RAM regions, every frame | RAM regions, stride 8 | Throughput gain vs arithmetic | UI requested 8x |
| --- | ---: | ---: | ---: | ---: | ---: |
| Room | 581.6 ms | 544.8 ms | 249.2 ms | 2.33x | 7.79x |
| Movement | 631.0 ms | 572.4 ms | 261.3 ms | 2.41x | 7.84x |
| Intro | 456.9 ms | 413.1 ms | 258.2 ms | 1.77x | 7.92x |

Each fixed workload advances 33,707,521 clocks. Guarded RAM execution improves
throughput approximately 7–11% with every frame composed. Fixed stride 8
composes 15–16 of the 120 emulated frames; the first partially restored frame
is conservatively rendered. UI measurements use the adaptive callback policy,
not a fixed stride; requested 4x keeps up in all three scenes. The 8x goal is
nearly met in these local browser workloads, not established as a universal or
consistent 8x guarantee. The production `out/` bundle contains these changes.

### Fixed-width cartridge opcode timing (follow-up)

The fresh production profile after RAM regions and adaptive presentation
attributes 18.07% of self samples to Thumb stepping, 8.30% to Thumb fetches and
7.61% to the general prefetch fetch routine. These are sampled attribution,
not isolated cost measurements. Instruction fetch overhead is now a useful
target after reducing scanline composition.

Cartridge opcode timing now specializes its halfword count at compile time:
one for Thumb and two for ARM. The routine inlines into the existing bus fetch
path, eliminating the runtime halfword loop for Thumb. Buffered opcodes use a
dedicated one-clock producer update: every cartridge wait is at least two
clocks, so at most one queued halfword can finish. Queue contents, partial
progress, sequential/nonsequential accesses, 128-KiB boundaries and cartridge
window transitions retain the original timing. The bus device/open-bus fallback
and all CPU instruction semantics remain unchanged. Broader inlining of the
bus into Thumb execution regressed the browser measurements and was reverted.

The original fetch algorithm remains a test-only differential oracle. The new
test covers all 512 combinations of the three banks' N/S wait settings, valid
queue counts and partial progress, page/window edges, both instruction widths
and discontinuities. Existing queue tests now exercise the specialized path;
bus tests compare it against the original model. All 203 GBA JIT/idle tests
pass, as do the repository release gate, production browser fixtures and
targeted GBA WASM Clippy checks. Browser comparisons additionally record SHA-256 fingerprints of complete
snapshots and hardware-only state for comparisons between separately built
binaries. Every corresponding complete snapshot matches the preserved previous
production bundle in all three scenes, including pixels and audio.

Three alternating trials at each frame stride, warmed JIT caches, same
33,707,521-clock workload and no concurrent builds or tests:

| FireRed scene | Previous, every frame | Specialized, every frame | Previous, stride 8 | Specialized, stride 8 | Stride-8 throughput gain | Specialized UI requested 8x |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Room | 563.8 ms | 538.5 ms | 269.4 ms | 239.5 ms | 12.5% | 8.08x |
| Movement | 585.4 ms | 552.2 ms | 262.8 ms | 242.2 ms | 8.5% | 7.96x |
| Intro | 423.6 ms | 408.3 ms | 263.0 ms | 240.5 ms | 9.4% | 7.93x |

Every-frame throughput improves 3.7–6.0%. UI results include callback pacing
and adaptive presentation and vary between runs; the fixed-work measurements
establish the throughput improvement. The general full JIT and experimental
compositor remain disabled. No new dependency or submodule is needed. The
production `out/` bundle includes the specialized fetch path.

For a before/after build comparison, preserve the previous site bundle, then
run each site with the same ROM and restored scene:

```sh
RUSTBOY_SITE_DIR=target/gba-jit-site \
RUSTBOY_GBA_JIT_VERIFY=1 RUSTBOY_GBA_FETCH_VERIFY=1 \
RUSTBOY_GBA_PROFILE='roms/Pokemon - FireRed Version (USA, Europe) (Rev 1).gba' \
RUSTBOY_GBA_PROFILE_STATE=target/gba-firered-room-movement.state \
node tests/browser_smoke.mjs
```

This mode alternates stride 1 and 8 three times each and compares hardware
state throughout. Compare `hardwareHash` across builds and `snapshotHash` at
the same stride. Local logs are ignored under
`target/gba-fetch-final-{before,after}-{room,movement,intro}.log`; the initial
profile is `target/gba-next-current-8x.cpuprofile`.

The browser build script now runs Binaryen after `wasm-bindgen`, sharing the
`Cargo.toml` release flags (`-O3`, `--enable-mutable-globals`). CI and Pages
install Binaryen before building; local builds require `wasm-opt` on PATH.
The exported growable function table remains available to the JIT. The local
Binaryen 117 build reduces the final WASM from 1,288,486 to 1,137,667 bytes
(11.7%). Production Game Boy and original GBA/JIT browser fixtures pass.
In the movement workload, complete snapshot hashes match the pre-optimization
build at both strides. Three-trial median time is 536.8 ms for every-frame
rendering and 231.2 ms at stride 8, compared with 552.2 and 242.2 ms above.
The UI achieves 8.07x in this run; these results remain scene-specific.
Logs are `target/gba-wasmopt-{build,browser,cgb-browser,movement}.log`.

### Sustained fast-forward headroom (October 10 follow-up)

Longer playback showed that short measurements near the requested 8x ceiling
left too little headroom. This pass retains seven core changes:

- Aligned RAM halfword/word stores decode the mapping once, retaining wait
  states, disabled/locked RAM behavior and JIT page invalidation.
- Enabled pending IRQs allow event-bounded CPU slices while CPSR.I masks
  dispatch. The slice exits immediately when an instruction unmasks the IRQ.
- ARM IWRAM JIT entries use physical-word slots with complete PC tags, retaining
  mirror-specific addresses and the separate arithmetic/RAM policies.
- Consecutive frontend run chunks share a validation epoch within one exclusive
  callback. Every new host callback revalidates code; guest page versions still
  invalidate compiled RAM immediately.
- Static register-use analysis loads only the registers a compiled ARM region
  can use, and stores only possible destinations. Conditional and guarded exits
  preserve the original values of destinations that did not execute.
- Text rows calculate their vertical map address once per scanline and their
  horizontal map address once per tile, reducing work inside the pixel loop.
- Interpreted slices accumulate the diagnostic instruction count locally,
  publishing it before JIT calls, idle proofs, exception helpers and return.

Paired optimized builds, warmed code caches, three alternating trials per
stride, and an identical 33,707,520-clock budget (with whole-instruction
overshoot) give these stride-8 results:

| FireRed scene | Previous median | Current median | Throughput gain | Current capacity |
| --- | ---: | ---: | ---: | ---: |
| Room with moving NPC | 246.8 ms | 223.6 ms | 10.4% | 8.99x |
| Settled room | 229.6 ms | 214.3 ms | 7.1% | 9.38x |
| Intro animation | 249.5 ms | 214.7 ms | 16.2% | 9.36x |

The complete same-stride snapshots and hardware hashes match the previous
production bundle in all 18 comparisons. Separate 600-callback playback runs
at requested 8x measured 8.031x, 7.998x and 7.919x, respectively. Callback
timing and the frontend work allowance affect sustained playback in addition
to fixed-work capacity. These are the available local scenes; the user's
Viridian Poké Mart save was not available for an exact-scene measurement.

Independent generated-WASM checks cover 37,569 ARM arithmetic/branch cases and
13,714 RAM-transfer cases against the Rust reference CPU and bus. The new
store/IRQ tests check mapping, alignment, waits and the precise unmasking
boundary. An additional replay test checks instruction-counter wraparound
through slice boundaries and idle-loop proofs.

Private paired logs are `target/gba-headroom-paired-{baseline,v7}-{movement,room,intro}.log`.
Use `RUSTBOY_GBA_PROFILE_FRAMES=600 RUSTBOY_GBA_PROFILE_SPEEDS=8` alongside the
fixed-work command above to repeat the sustained samples. The slower arithmetic
rewrite and forced fetch-inlining candidates were reverted.

The final production rebuild is 1,138,482 bytes after Binaryen. Its separate
600-callback movement replay measured 7.986x; the complete snapshots again
match the previous production bundle. The repository release gate, 206 GBA
JIT/idle tests, 202 decoded-block/idle tests, WASM GBA Clippy and production
Game Boy/GBA/JIT browser checks pass. Final logs are
`target/gba-headroom-{production-final-build,production-movement,production-final-browser,release-gate,final-native,decoded-native,wasm-clippy}.log`.

### Shared ARM entries and effective color operations (October 10 follow-up)

This pass retains two changes after the reported 770% throughput with drops:

- A connected ARM IWRAM region accepts every covered instruction as an entry
  point. Sequential instructions still fall through directly. Cache aliases
  share one compiled function and its validation across those PCs, avoiding
  overlapping compilations after scheduler exits. Full PC tags retain mirror
  behavior; owner generation tags prevent stale aliases after replacement or
  function-table recycling. Guest page versions still invalidate shared code.
- The general compositor specializes scanlines whose configured effects cannot
  change a color. It omits second-layer tracking and blending in those lines.
  This includes disabled effect targets, alpha coefficients 16/0 and zero
  brightness coefficients. FireRed's room fixture leaves blending enabled with
  no primary targets and identity alpha coefficients; checking only whether the
  effect mode is disabled missed this case. Semi-transparent objects retain
  the full path whenever their alpha operation can affect the output.

Independent generated-WASM verification passes 37,569 ARM and 13,714 RAM cases
both from the original entry and from varied interior entries (102,566 cases
in total). The randomized compositor comparison checks 96 scenes across all
six video modes, windows, object modes, mosaics, priorities, effect targets and
coefficients against the full color compositor.

Short fixed-work samples varied enough to obscure the smaller throughput gain.
The browser harness now accepts `RUSTBOY_GBA_FIXED_WORK_FRAMES=600` (120 remains
its default). Two sessions per variant, six alternating samples per rendering
stride, and the same 168,537,600-clock budget produce these movement-fixture
medians:

| Rendering | Previous median | Current median | Throughput gain | Current capacity |
| --- | ---: | ---: | ---: | ---: |
| Every frame | 2659.40 ms | 2255.60 ms | 17.9% | 4.45x |
| Stride 8 | 1185.95 ms | 1116.20 ms | 6.2% | 9.00x |

All complete snapshot hashes match across both builds, including pixels at the
same stride. Compiled-function totals after the long trials fall from
3829–3830 to 3742, including the unchanged full compiler used during cache
warmup. Sharing alone did not demonstrate a reliable warm-throughput gain.
A separate CPU RAM-read shortcut was tested and reverted because it did not
improve the measurements reliably.

These capacity measurements do not promise uninterrupted 8x browser playback.
The 600-callback playback samples still vary by scene and host scheduling, and
occasionally drop below 8x. The exact Viridian Poké Mart save is unavailable;
the private room/movement/intro fixtures remain the available comparisons.
Paired long logs are `target/gba-sharing-long-{baseline,baseline-repeat,candidate,candidate-repeat}.log`;
the shorter scene logs are `target/gba-sharing-final-{baseline,candidate}-{movement,room,intro}.log`.

The production build is byte-identical to the measured candidate: 1,133,617
bytes after Binaryen, SHA-256
`0222fe4521c06a8eb5edc0b6efc05d6e01ed6198dfc23692824c14b1e24d3d0a`.
The release gate, 207 GBA JIT/idle tests, 206 decoded/idle/render-cache tests,
WASM GBA Clippy, production Game Boy smoke and original GBA/JIT browser
fixtures all pass. Final logs are `target/gba-sharing-{production-build,clippy-final,release-gate,effective-native,decoded-render-native,production-browser,production-gb-browser}.log`.

### Current production bundle (October 11)

The compatibility follow-ups retain the measured performance settings above:
guarded ARM JIT and idle-loop batching are enabled by default, presentation is
adapted to fast-forward, and the general full JIT, decoded-block and render-cache
experiments remain opt-in. The optimized bundle also includes BIOS PCM/music
services, legacy autosave recovery, the shared DMA latch used by Phantasy Star
dialog scrolling, and Yoshi's Island's null decompression-source guard.

The current release bundle is 1,156,141 bytes after Binaryen, SHA-256
`35a2ab6f3109c0019535d837ed85d9e165dc3b3d295438273ce359c0875909ed`.
All 254 GBA JIT/idle-loop tests and the original GBA/JIT browser regression suite
pass. These compatibility changes do not establish a new throughput measurement
or guarantee 8x for every game or host. Latest build and browser logs are
`target/yoshi-bios-{web-build,browser}.log`.

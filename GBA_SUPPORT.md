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
states but do not cache a real pipeline. ROM prefetch now has an instruction-level
queue baseline (below); unused-memory CPU reads now have an open-bus baseline.
Write-only/unused I/O now has a register-readability baseline (below).
DMA open bus and general bus arbitration remain pending.
BIOS read protection has a functional baseline
(below), not an exact pipeline oracle. Reserved/unpredictable
instructions are not a complete hardware oracle. Scanline graphics and Direct
Sound are now functional baselines, not claims of raster/PWM accuracy or complete
game compatibility. PSG channels now have a functional synthesis baseline (below).

## Run locally

```sh
cargo test -p rustboy-gba --offline
cargo run --release -p rustboy-gba --example inspect -- "path/to/game.gba" 60
# Optional third argument: a BIOS file supplied by the user.
```

For deterministic local workload capture, `inspect` also accepts
`GBA_STATE=<checked-state>` and `GBA_SAVE_STATE=<new-output>`. Optional
`GBA_INPUT_SEQUENCE="0:0008,5:0000"` presses Start at relative frame zero and
releases all buttons at frame five. Each event is `frame:complete_hex_key_mask`
in GBA key order, up to 256 increasing events within the requested run. Inputs
are applied at whole-instruction boundaries; frozen/faulted runs do not export
a completed state. `GBA_FRAME_PNG=<new-output>` captures the resulting display.
Output files are exclusive-create: use fresh paths and keep private cartridge
states/screenshots local, not in git. Inspect counts are relative to the imported
state; tracing/input-script runtimes are not isolated performance benchmarks.

```sh
GBA_INPUT_SEQUENCE="0:0008,5:0000" GBA_SAVE_STATE=target/local-start.state GBA_FRAME_PNG=target/local-start.png cargo run --release -p rustboy-gba --example inspect --offline -- "path/to/game.gba" 300
cargo run --release -p rustboy-gba --example instruction_profile --offline -- "path/to/game.gba" target/local-start.state 120
```

The instruction-mix diagnostic counts guest encoding families and hot PCs under
reference stepping, not wall-time component costs. It includes first-hit r0–r3
and starting/ending state fingerprints, bounds its PC map, conservatively avoids
decoding protected/serial/open-bus regions, and accepts a matching user BIOS as
an optional final argument. The existing fixed FireRed benchmark is an intro
animation, with about half its instructions in a RAM-flag polling loop; do not
treat it as broad gameplay coverage or use cartridge-specific idle skips. See
[GBA_PERFORMANCE_PLAN.md](GBA_PERFORMANCE_PLAN.md) for measurements and next gates.

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

### Independent ARM/Thumb/memory conformance checks

The opt-in `conformance` example runs unmodified, locally supplied
[Julian Smolka's MIT-licensed GBA test ROMs](https://github.com/jsmolka/gba-tests)
at pinned revision `a7113b67e63f83a9b321696ddd7042ccfad6c881`. No third-party
emulator implementation is used. ROMs remain outside this repository; their
Nintendo logo/header data and any external firmware are not redistributed.
The runner checks fixture fingerprints, the source-verified final loop address,
ARM/display state, firmware faults and the correct result register (R7 for
Thumb, R12 for the other suites). It runs all four suites by default and exits
nonzero on failures; they are not silently skipped or marked ignored.

This revealed two independently reproduced CPU defects, fixed with original
regression tests: ARM TST/TEQ/CMP/CMN with encoded destination R15 restore CPSR
from SPSR without writing PC/refilling the pipeline; empty-list Thumb STM stores
instruction address +6 rather than +4, retaining its +64-byte base writeback.
ARM, Thumb and memory now reach their passing terminal loops. BIOS still reports
failure 1: its protected-read assertion expects the opcode left by Nintendo's
exact startup firmware, whereas RustBoy runs independently authored replacement
firmware. This failure remains reported and is not worked around by changing
the fixture or inventing a Nintendo opcode latch. It does not validate the
remaining BIOS tests or establish full CPU/cycle accuracy.

```sh
# Supply a local checkout at the pinned revision (ROMs are not bundled).
git clone https://github.com/jsmolka/gba-tests.git /tmp/rustboy-gba-tests
git -C /tmp/rustboy-gba-tests checkout a7113b67e63f83a9b321696ddd7042ccfad6c881
cargo run --release -p rustboy-gba --example conformance --offline -- /tmp/rustboy-gba-tests
# Selected suites can be run explicitly; default above retains the BIOS failure.
cargo run --release -p rustboy-gba --example conformance --offline -- /tmp/rustboy-gba-tests arm thumb memory
```

The self-contained gate passes 364 native checks (189 GBA: 170 unit, 16 original
program, 3 EEPROM), JS/Python checks, warnings-denied GBA Clippy and the root
WASM check. The rebuilt GBA browser scenario also passes. Independent fixtures
are a separate opt-in diagnostic, not included in that self-contained count.
The broader GB ROM suite/full browser matrix were not rerun in this milestone.

### Internal memory-control baseline

The original bus now models `04000800h`, including its 64 KiB I/O mirrors,
hardware reset value `0D000020h`, readable/writable masks and all access widths.
Clearing bit 5 maps the EWRAM address region to mirrored IWRAM without destroying
the hidden EWRAM contents. Bit 0 disables both RAM regions: writes are ignored,
debug peeks return zero and CPU data reads use the existing open-bus baseline.
The wait nibble programs EWRAM bus costs from 16 down to 2 clocks per halfword;
word transactions cost two halfwords. These costs also apply to DMA and opcode
fetches. Remapped IWRAM uses the fast 32-bit internal bus baseline.

Wait setting 15 latches a nonrecoverable bus lock on an enabled EWRAM access.
Subsequent machine execution stops; a stalled DMA neither advances counters nor
signals completion, and reset clears the latch. The frontend reports the lock
as paused. This is an instruction-boundary lockup model, not exact mid-instruction
bus suspension: the triggering CPU instruction may finish its register effects,
and precise clock/pipeline phases still need a cycle-level implementation.
No GBA Micro-specific overclock behavior is claimed.

Snapshot version 11 appends the memory register and lock latch. Versions 1–10
remain readable and migrate to normal reset mapping. Checksum-valid illegal
register bits, invalid booleans and inconsistent locked settings are rejected
atomically. The specifications are the
[GBATEK internal memory-control description](https://rust-console.github.io/gbatek-gbaonly/#4000800h---32bit---undocumented---internal-memory-control-rw).

Six new original native tests cover masks/mirrors, all finite wait settings,
RAM disable/mapping, hidden RAM preservation, DMA timing/stall, state migration
and lockup/reset. The native gate passes 362 self-contained checks (187 GBA:
168 unit, 16 original-program, 3 EEPROM). Clippy with warnings denied, the root
WASM check and rebuilt browser scenario pass; two original browser cartridges
exercise mirrored register writes and normal/remapped RAM through the real CPU,
canvas and state replay. The separate GB ROM suite/full browser matrix were not
rerun for this GBA-only change. DMA open-bus latches, real pipeline state and
accurate bus arbitration remain unfinished.

### Final-output Green Swap

GREEN_SWAP (`04000002h`, bit 0) now exchanges the green components of each
even/odd screen-pixel pair after layer composition and color effects. Red,
blue, alpha, palette contents and VRAM remain unchanged. The register accepts
byte/halfword/word writes and masks unused bits; forced blank remains white.
This follows the [GBATEK Green Swap specification](https://rust-console.github.io/gbatek-gbaonly/#4000002h---undocumented---green-swap-rw),
not another emulator's implementation. Existing snapshots already preserve this
I/O register, so the state format remains version 10.

Original tests cover all six modes, post-window/brightness ordering, an
OBJ/backdrop pair at the right screen edge, untouched neighboring rows, forced
blank and deterministic state replay. Two original browser cartridges verify
enabled/disabled output through CPU register writes and the actual WASM canvas,
with state round-trip checks. The self-contained native gate now has 356 checks
(181 GBA: 162 unit, 16 original-program and 3 EEPROM). Internal memory control,
RAM-disable/lockup behavior, DMA open bus and precise raster timing remain
unfinished; Green Swap does not imply full display-hardware accuracy.

### CPU I/O readability and open-bus baseline

CPU reads now distinguish readable I/O halfwords from write-only register
storage and unused slots. Wholly write-only/unused 32-bit words expose the
existing ARM/Thumb prefetch model; mixed words return readable register data
with zero in unreadable lanes. DMA count/control pairs therefore return zero
for the count and the control in the upper halfword. WAITCNT/IME return zero
in their unused partner halfwords; POSTFLG does not expose the write-only
HALTCNT byte. Normal I/O is not mirrored elsewhere in the 04h address region.
Debugger peeks return zero for unreadable storage, while internal `io16` reads
retain the PPU/APU/scheduler's actual written values. Sound masks/deferred reads
and timer counters remain handled by their existing peripherals. Mapped I/O
loads also retain the full I/O bus word for subsequent Thumb IWRAM open reads.

The map and mixed-word rule follow GBATEK's I/O map and unpredictable-read
sections:
https://rust-console.github.io/gbatek-gbaonly/#gba-io-map
https://rust-console.github.io/gbatek-gbaonly/#reading-from-unused-or-write-only-io-ports
GBATEK's MOSAIC example calls it readable, contradicting its own write-only
table and gbadoc's write-only description. This baseline follows the table for
MOSAIC/BLDY; effect-register edge cases and all hardware read masks still need
an independent hardware oracle. The undocumented internal-memory-control
register and mirrors are not implemented and are not covered by this map.

Three original tests cover unreadable words, byte/halfword lanes, preserved
internal storage, mixed DMA/control and WAITCNT/IME/POSTFLG words, non-mirroring,
Thumb retained I/O data and debugger isolation. The affine scanline test now
inspects the internal BG2Y latch and separately verifies that its write-only
debug read returns zero. Four additional original browser cartridges verify
BG-offset, DMA-address and unused I/O prefetch reads plus a zero WAITCNT partner
through CPU/WASM/Mode-3 pixels. No persistent field or snapshot version changed.

All 158 GBA unit / 16 original program / 3 EEPROM tests pass (177 GBA checks),
with 352 self-contained native checks, JS/Python checks, warnings-denied GBA
Clippy and the root WASM check. The rebuilt GBA browser scenario passes all
previous fixtures and all five open/zero-bus cartridges. The separate
GB ROM suite and full eleven-scenario browser gate were not rerun for this
GBA-only change. DMA open-bus history and exact pipeline/bus phases remain
pending; this baseline is not a full hardware-register accuracy claim.
FireRed's 1,800-frame startup probe remains fault-free with the previous
505,612,801 clocks, 175,983,876 instructions, 6,956 exceptions and CPU/display
state (PC 08079792h, display 9F40h). It took 13.989 s (2.15x native clock
throughput), not a controlled performance comparison or complete-game validation.

### Active major phase: browser 8x optimization

The architectural strategy and decision gates are in
[GBA_PERFORMANCE_PLAN.md](GBA_PERFORMANCE_PLAN.md). Further work should follow
isolated component measurements, not a succession of speculative micro-edits.

The user moved this phase ahead of the remaining GBA implementation work. Prioritize a substantial performance pass
targeting smooth 8x gameplay. The user's current real-game browser observation
is approximately 1.5x despite higher requested speeds. Profile real cartridges
on representative browser/device combinations, distinguish achieved emulated
clock throughput from the selected multiplier, and measure sustained gameplay
and frame pacing rather than only startup or synthetic loops. Preserve CPU,
peripheral and save-state correctness while optimizing; publish measured results
and remaining host limits instead of treating an 8x setting as proof of 8x speed.

First measured batch (2026-10-04): Chromium sampling on the local FireRed ROM
identified CPU/machine dispatch, per-instruction timer updates, rendering and
DMA polling as major costs. The profile runner now understands version-11
states and can write a DevTools CPU profile through
`RUSTBOY_GBA_CPU_PROFILE=<new-file.cpuprofile>` (exclusive creation, not overwrite).
The ROM and profiles are local diagnostic inputs/artifacts, not public assets.

Release builds now use thin cross-crate LTO and one code-generation unit.
The timer path advances only enabled independent counters before its cached
overflow deadline, avoiding generic overflow/cascade/IRQ work on each instruction.
It still updates readable counters and prescaler phases immediately; it does not
defer interrupts, change timer clock rates, skip hardware or drop audio. An
original differential test forces the old generic path and compares counters,
phases, overflows, IRQ flags and derived deadlines across event boundaries.

The same browser startup profiling procedure measured requested 8x at **1.29x
before and 1.75x after**; requested 4x measured **1.31x before and 1.82x after**.
Median fast-forward callbacks remain about **34–35 ms**, so smooth 8x is not
achieved. These are initial samples from this machine's headless Chromium,
not matched persistent-state A/B measurements, sustained gameplay validation
or a promise for other devices. CPU dispatch, graphics and DMA polling remain
optimization targets, as do better reproducible gameplay benchmarks. The major
pass remains in progress; full GBA accuracy work is also still outstanding.

Validation for this batch: 365 self-contained native checks (190 GBA), the
JS/Python gate, warnings-denied GBA Clippy, root WASM check, the independent
ARM/Thumb/memory ROMs and the rebuilt GBA browser scenario pass. The independent
BIOS firmware-latch mismatch remains unchanged and reported separately.

Second batch: DMA dispatch uses derived active-channel and FIFO eligibility
masks instead of scanning four descriptors on every CPU instruction. FIFO
request readiness is updated on DMA control changes, FIFO pushes/consumption,
FIFO reset, sound master changes and BIOS resets. Active bursts still execute
one transfer unit at a time with the existing priorities, startup costs,
counter behavior and interrupt timing. Caches are rebuilt after state restore;
no snapshot format change or hardware frame/audio skipping is involved.
Two original tests cover cache invariants, threshold requests, completion,
disable, master/FIFO resets and BIOS DMA resets. The self-contained gate passes
367 native checks (192 GBA), GBA Clippy, the root WASM check and independent
ARM/Thumb/memory ROMs. The isolated rebuilt GBA browser scenario also passes.

FireRed's latest browser sample is 1.74x at requested 4x and 1.80x at requested
8x, with median callbacks around 34 ms. These do **not** establish an additional
end-to-end speed gain over the first batch; saved-state-controlled A/B testing
is still needed. Remaining CPU dispatch/rendering costs dominate the next work.
This build was measured in an isolated worktree to preserve concurrent
UI/controller edits and their modified browser bundle in the shared directory.
The backend source changes are committed separately; shared `out/` artifacts
are not overwritten or committed as part of this batch.

Third batch (2026-10-07): text and affine background wrapping use exact
power-of-two masks rather than variable signed remainder operations. An original
affine test compares against a Euclidean-remainder oracle at negative and
positive coordinate extremes for all four map sizes. No timing, audio, state
format or hardware frame skipping changes are involved.

The new native fixed-workload benchmark restores one baseline before every
trial and requires byte-identical ending states. To create a local baseline:

```bash
GBA_BENCH_SAVE_STATE=target/my-gba-baseline.state cargo run --release -p rustboy-gba --example benchmark -- "path/to/game.gba" 300 120 5
```

For subsequent comparisons, use `GBA_BENCH_STATE=target/my-gba-baseline.state`
instead of `GBA_BENCH_SAVE_STATE`; imported states skip warmup. State output is
exclusive-create, never overwrite. Compare the printed ROM, baseline and result
fingerprints, clocks and instruction counts as well as elapsed time. The example
times native backend execution, not browser gameplay or frontend audio delivery.

Five sequential trials of the same saved FireRed workload measured median
0.824059 s before versus 0.776098 s after (roughly 6% faster), with overlapping
trial ranges. Both executed 33,707,520 clocks / 12,269,926 instructions and
produced result fingerprint `12f3190d01dcf2f2`. This is preliminary native
evidence, not an established browser improvement. The rebuilt browser measured
1.01x at requested 1x, 1.66x at 4x and 1.73x at 8x; fast-forward median callbacks
were about 34–35 ms. **Smooth 8x remains unachieved.** A condition-code table and
a scheduler fast-path experiment were discarded after repeat measurements
showed noise or regressions; the exhaustive condition-code oracle test remains.

Validation: 369 self-contained native checks (194 GBA), JS/Python checks,
warnings-denied GBA Clippy, independent ARM/Thumb/memory ROMs, and the rebuilt
GBA upload/control browser scenarios pass. This is not a rerun of the full GB
external ROM suite or the full browser matrix. The major optimization phase
remains active, ahead of the remaining GBA feature work.

Fourth batch: text backgrounds decode a complete eight-pixel tile row into a
scanline-local cache. Palette transparency, map/tile flips, mosaic, wrapping,
priority and invalid-character-address behavior match the uncached sampler in
122,880 original differential pixel checks. Registers, palette and VRAM are
immutable during one render call; no cache persists across subsequent raster
writes or state restores. Seven sequential native trials measured median
0.792897 s before / 0.715828 s after with byte-identical ending states. The
browser measured 1.78x at requested 4x / 1.77x at 8x, which does not demonstrate
a browser improvement over preceding samples. Register hoisting alone was
removed after failing to show a supported gain. Targeted GBA browser scenarios,
Clippy, the independent ARM/Thumb/memory ROMs and all 370 self-contained native
checks (195 GBA) pass. This is a small renderer improvement, not
completion of the architectural optimization pass or the 8x target.

Profiling refinement: `RUSTBOY_GBA_PROFILE_STATE=<local-state>` now gives each
browser speed sample the same checked saved-state start. CPU sampling is scoped
to the requested-8x execution interval, excluding startup, state restore and
clock-reading exports. Native source-level attribution separates the inlined
compositor from scheduler/audio/timer work; see the architecture plan for the
budget and its limitations. This makes CPU/bus execution the primary structural
target, not graphics-only micro-optimizations.

The isolated browser profile also identified full-ROM rehashing in frontend
status polling (about 4% of sampled time). GBA state IDs now reuse the immutable
ROM identity already computed during cartridge detection. Firmware identity and
checked snapshot hashes/validation remain unchanged. A new test checks identity
against snapshot headers for batteryless/SRAM cartridges and replacement/user
firmware, repeated polling, reset, clear-save, restore and distinct ROMs.
All 371 self-contained native checks (196 GBA), warnings-denied GBA Clippy,
independent ARM/Thumb/memory ROMs and targeted rebuilt GBA/browser-controls
scenarios pass. One unprofiled fixed-state browser pair measured requested 8x
at 1.79x before / 1.82x after, with p95 callback intervals 52.5 / 35.9 ms. The
removed hashing is proven by profiling, but the small throughput difference
is not a general speed claim; smooth 8x and broad gameplay validation remain
outstanding.

Major execution refactor: ordinary CPU execution now runs in event-bounded
slices instead of traversing the complete machine/peripheral scheduler after
every instruction. MMIO/EEPROM barriers synchronize preceding instruction time
before side effects; exception helpers also flush prior work. Video, timer and
audio deadlines cap slices, and DMA/IRQ/sleep/wait cases retain reference stepping.
Current-instruction side effects and whole-instruction overshoot preserve the
existing timing model. No opcode, hardware event, audio sample or rendered frame
is skipped; snapshots retain version 11 and identical byte layout.

Eight new differential tests compare complete state and PCM against reference
stepping across ARM/Thumb, self-modifying RAM/open bus, register-write widths,
timer/PSG/FIFO/DMA/EEPROM, IRQs, firmware/helpers, waits/sleep, RAM remapping and
locks. All 379 self-contained native checks (204 GBA), GBA Clippy, independent
ARM/Thumb/memory ROMs and rebuilt targeted GBA/browser controls pass.
The native benchmark supports `GBA_BENCH_RUNNER=step` versus `slice` (default).

Seven sequential fixed-state native trials measured medians 0.773316 s before /
0.500198 s after, with identical 33,707,520 clocks, 12,269,926 instructions and
ending fingerprint `12f3190d01dcf2f2`. A sequential unprofiled fixed-state browser
pair measured requested 8x at **1.79x before / 2.60x after**, and requested 4x
at 1.81x / 2.61x. These are promising local samples, not an 8x claim or broad
gameplay validation. The optimization pass remains active; its synchronization
contract and next decision gates are in the architecture plan.

Cartridge timing refactor: WAITCNT decoding now happens on register updates and
state restoration, not every fetch/access/prefetch step. A small derived timing
cache preserves all physical fetches, FIFO progress, page restarts and hardware
events, without changing saved-state layout. Three new exhaustive-write/timing/
restore tests bring the GBA set to 207 passing tests. Sequential native A/B and
B/A pairs measured about 11–13% higher throughput with matching ending-state
fingerprints. Unprofiled browser pairs measured requested 8x at 2.32x / 2.65x and
2.51x / 2.61x (reference / candidate). These variable local gains do not establish
smooth 8x or broad gameplay compatibility. Full measurement details and the
rendering-worker investigation are in [GBA_PERFORMANCE_PLAN.md](GBA_PERFORMANCE_PLAN.md).
All 382 self-contained native checks, JS/Python checks, GBA Clippy and the pinned
independent ARM/Thumb/memory ROMs pass; the browser bundle is rebuilt with normal
default features. Rebuilt GBA upload/control browser scenarios passed after one
DevTools navigation-error retry; the full external GB/browser matrix was not
rerun for this GBA-only change.

### CPU unused-memory open-bus baseline

CPU reads at 00004000h-01FFFFFFh and 10000000h-FFFFFFFFh now expose predicted
ARM7 prefetch data instead of zero. ARM uses the word at instruction PC+8.
Thumb ROM/EWRAM/palette/VRAM duplicate the halfword at PC+4; BIOS/OAM return
the corresponding aligned word. Thumb IWRAM combines the next prefetched
halfword with a retained data-bus halfword from a preceding load, or the usual
PC+2 halfword when no load history exists. Branch refills discard load history.
Byte/halfword lanes and CPU unaligned-word rotation retain their normal rules.
Opcode fetches and debugger peeks do not create data-load history; debugger
unused-memory peeks remain zero. Prefetch contents are computed lazily only
for unused reads, not by adding a second ROM lookup to every instruction.
Load-history tracking is restricted to Thumb IWRAM execution: entering that
region via a branch has already discarded prior history. Normal RAM/ROM opcode
fetches use their direct memory path rather than running data-read tracking.
The documented region/alignment behavior follows
https://rust-console.github.io/gbatek-gbaonly/#reading-from-unused-memory-00004000-01ffffff10000000-ffffffff

Snapshot V10 appends the preceding data word and its validity flag. Versions
1-9 migrate without invented read history; malformed flags reject atomically.
Five original tests cover ARM word/halfword/byte lanes, Thumb memory regions and
alignments, retained load data, refill reset, real ARM unaligned LDR execution,
real Thumb load-to-unused execution, checked replay and V9 migration. An original
browser cartridge stores an unused ARM read into a Mode-3 pixel and checks its
prefetched opcode bits through the actual CPU/WASM/canvas path.

All 155 GBA unit / 16 original program / 3 EEPROM tests pass (174 GBA checks),
with 349 self-contained native checks, JS/Python checks, warnings-denied GBA
Clippy and the root WASM check. The rebuilt GBA browser scenario passes all
previous fixtures and the new open-bus fixture. The separate GB ROM suite and
full eleven-scenario browser gate were not rerun for this GBA-only change.
Write-only/unused I/O, RAM-disable behavior, DMA bus history, exact narrow-load
lane retention and real pipeline/bus-phase behavior still need hardware checks
or implementation; this baseline is not a complete open-bus hardware oracle.
FireRed's 1,800-frame startup probe remains fault-free with the previous
505,612,801 clocks, 175,983,876 instructions, 6,956 exceptions and CPU/display
state (PC 08079792h, display 9F40h). The final optimized probe took 13.337 s
(2.26x native clock throughput). An earlier version tracking irrelevant ROM
load history took 15.868 s; that work was removed before committing. These
single runs are not a controlled benchmark or complete-game validation.

### Enabled cartridge-prefetch queue baseline

An original eight-halfword timing queue now overlaps Game Pak opcode reads with
CPU internal cycles and non-cartridge data-bus cycles. It retains partial
halfword progress, consumes buffered ARM/Thumb opcodes with zero waitstates,
uses each ROM area's WAITCNT settings and restarts physical cartridge reads at
128-KiB boundaries. Full-queue idle time is discarded, not accumulated as credit.
Non-sequential/discontinuous fetches, cartridge data accesses, WAITCNT changes,
RAM/BIOS instruction streams and DMA invalidate the queue. DMA RAM transfers
cannot fill it while the CPU is stopped. Immutable ROM contents are represented
by queued addresses rather than copied into another code cache.
The documented capacity/idle-bus behavior follows
https://rust-console.github.io/gbatek-gbaonly/#gba-gamepak-prefetch

Snapshot V9 appends six bytes for the queue address, count and partial-cycle
progress; malformed or disabled-but-active queues reject atomically. Versions
1-8 remain readable and migrate with an empty queue, since their missing
prefetch history cannot be inferred reliably. Eight new original tests cover
partial fills, capacity, ARM consumption, discontinuities, page/cartridge ends,
all wait settings (batched versus one-clock scheduling), data/DMA invalidation,
checked replay and V8 migration. Existing ARM/Thumb instruction-family tests now
also check the idle-cycle benefit rather than their old unbuffered timings.
A CPU-driven browser cartridge enables WAITCNT, runs a multiply loop and checks
queue-state replay through WASM; all previous GBA browser fixtures also pass.

All 150 GBA unit / 16 original program / 3 EEPROM tests pass (169 GBA checks),
with 344 self-contained native checks, JS/Python checks, warnings-denied GBA
Clippy and the root WASM check. The browser bundle was rebuilt. The separate
GB ROM suite and full eleven-scenario browser gate were not rerun for this
GBA-only change. Exact CPU pipeline refill overlap, in-flight cartridge abort
phases, HALT-time production and CPU/DMA/video arbitration remain unverified;
this is not a hardware-cycle oracle or proof of complete game compatibility.
FireRed's 1,800-frame startup probe remains fault-free: 505,612,801 clocks
(one clock of instruction overshoot), 175,983,876 instructions and 6,956
exceptions, ending in the startup loop at PC 08079792h with display 9F40h.
The larger instruction count is consistent with reduced opcode waits, not an
independently verified hardware trace. It took 13.349 s (2.26x native clock
throughput); the earlier unbuffered probe did less CPU work, so this is not a
controlled host-performance comparison or complete-game validation.

### Disabled cartridge-prefetch timing correction

ROM instructions with internal cycles now force the next opcode fetch to be
non-sequential when WAITCNT's prefetch-enable bit is clear, following the
documented prefetch-disable bug. This covers ARM/Thumb multiply, register shifts,
loads, block loads, swaps and stack loads through the shared internal-cycle hook.
Branch/exception refills clear the pending restart, and failed ARM conditions
do not execute internal cycles. RAM instructions are unaffected. The restart
is committed to the CPU's existing sequential-fetch state at the instruction
boundary, so snapshots need no format change or new persistent scratch state.
The specification is
https://rust-console.github.io/gbatek-gbaonly/#prefetch-disable-bug

Five original tests cover instruction families, enabled/disabled prefetch,
RAM execution, ARM/Thumb PC-load refills, failed conditions and exact state replay.
All 142 GBA unit / 16 original program / 3 EEPROM tests pass (161 GBA checks),
with 336 self-contained native checks, JS/Python checks, warnings-denied GBA
Clippy and the root WASM check. The rebuilt GBA browser scenario passes its
upload, audio, fast-forward, all-six-mode graphics, BIOS, DMA-capture and sprite
fixtures. The separate GB ROM suite and full eleven-scenario browser gate were
not rerun for this GBA-only change. At this milestone the enabled cartridge-prefetch
queue, exact pipeline and general bus arbitration remained unimplemented; this correction
does not claim full cartridge timing accuracy.
FireRed's 1,800-frame startup probe remains fault-free at 505,612,800 clocks,
159,995,145 instructions and 6,958 exceptions, ending at PC 08079754h in the
same startup loop with display 9F40h. The 55-instruction difference from the
previous milestone is consistent with charging additional disabled-prefetch
waits, not an independently verified hardware trace. The probe took 11.833 s
(2.55x native clock throughput), not a controlled performance comparison or
complete-game validation.

### Per-scanline OBJ budget baseline

The sprite compositor now limits each scanline to 1,210 rendering cycles,
or 954 when DISPCNT's HBlank-free bit is set. Normal objects cost one cycle
per bounding-box pixel; affine objects cost ten setup cycles plus two per
pixel, including double-size bounds. OAM order consumes the budget even for
transparent or horizontally offscreen objects, and the last object can be
partially rendered. Disabled objects and objects outside the current row are
excluded in this baseline; exact fetch phases and hardware edge cases remain
unverified. The documented costs follow
https://rust-console.github.io/gbatek-gbaonly/#maximum-number-of-sprites-per-line

Three original tests cover both budgets, partial-object clipping, transparency,
disabled/other-row objects, independent scanlines and affine double-size costs.
Two original CPU-driven browser cartridges verify visible pixel boundaries
through the real WASM upload/render path. All 137 GBA unit / 16 original program /
3 EEPROM tests pass (156 GBA checks), with 331 self-contained native checks,
JS/Python checks, warnings-denied GBA Clippy and the root WASM check. The rebuilt
GBA browser scenario passes all previous fixtures and both new sprite fixtures.
The separate GB ROM suite and full eleven-scenario browser gate were not rerun
for this GBA-only change. This does not establish raster accuracy or complete
game compatibility.
FireRed's 1,800-frame startup probe remains fault-free with the same
505,612,800 clocks, 159,995,200 instructions, 6,958 exceptions and CPU/display
state. It took 11.595 s (2.60x native clock throughput), not a controlled speed
comparison or complete-game validation.

### DMA3 video-capture baseline

DMA3's special start mode now queues capture bursts for scanlines 2-161,
including the two capture scanlines after visible display. It reloads the
latched count per request, retains source progression and supports destination
increment/reload, normal nonrepeat completion and per-burst IRQs. At scanline
162 further capture requests are disabled; an in-flight burst drains, so a row
longer than HBlank is not silently truncated. Software must re-enable capture
for another frame. DMA0 special mode and DMA1/2 FIFO requests remain separate.

This is a functional scheduler baseline using HBlank requests. Exact capture
request phase, shutdown latency, overruns, bus arbitration, cartridge DRQ and
external camera hardware remain unverified/unimplemented; this is not a claim
of hardware-cycle-exact capture. The documented scanline range and mode follow:
https://rust-console.github.io/gbatek-gbaonly/#video-capture-mode-dma3-only

Original tests cover the 160-request window, VBlank rows, enable shutdown,
nonrepeat, destination reload, channel isolation, IRQs and pending-burst replay
using the existing snapshot fields. An original CPU-driven cartridge captures
and verifies every pixel in a full bitmap, including the final row. The browser
cartridge checks distinct rendered coordinates and that last row through WASM.
All 134 GBA unit / 16 original program / 3 EEPROM tests pass (153 GBA checks),
with 328 self-contained native checks, JS/Python checks, warnings-denied GBA
Clippy and the root WASM check. The rebuilt GBA browser scenario passes its
existing regression checks and the capture fixture. The separate GB ROM suite
and full eleven-scenario browser gate were not rerun for this GBA-only feature.
FireRed's 1,800-frame startup probe remains fault-free at the same 505,612,800
clocks / 159,995,200 instructions / 6,958 exceptions and CPU/display state.
It took 11.212 s (2.69x native clock throughput), not a controlled speed
comparison, capture hardware oracle or complete-game validation.

### DMA register and address-width corrections

DMA control writes now clear unused low bits on every channel and the
DMA3-only DRQ bit on channels 0-2. Byte writes to either control byte update
the channel; changing the low byte while waiting for HBlank no longer leaves
the internal mode stale or reloads the already latched source/count.

Source and destination counters now wrap at their channel's physical 27/28-bit
address limit after increment/decrement. Every transfer masks addresses before
the bus access, so DMA0 cannot escape from internal memory into cartridge ROM.
Older saved unmasked counters resume using the physical width, without changing
snapshot layout. Source/destination initial register values remain separate from
the internal moving counters. This follows the DMA register descriptions:
https://rust-console.github.io/gbatek-gbaonly/#gba-dma-transfers

Original tests cover channel-specific control masks, bytewise changes to a
waiting channel, latched/programmed address separation, increment/decrement at
each address limit, DMA0's boundary result, replay across wrap and recovery of
older unmasked counters. External DRQ signaling, startup/arbitration phases,
SRAM-side restrictions and invalid-source open-bus details remain accuracy work.
Video-capture scheduling has the functional baseline described above.

Validation: 130 GBA unit / 15 original program / 3 EEPROM tests pass (148 GBA
checks), together with 323 self-contained native checks, JS/Python checks,
warnings-denied GBA Clippy and the root WASM check. The rebuilt GBA browser
scenario passes its upload, graphics, audio, saves, speed, BIOS and STOP checks.
FireRed's 1,800-frame startup probe remains fault-free at the same 505,612,800
clocks / 159,995,200 instructions / 6,958 exceptions and CPU/display state.
This run took 11.144 s (2.70x native clock throughput), not a controlled speed
comparison or full-game validation. The separate GB ROM suite and full
eleven-scenario browser gate were not rerun for this GBA-only correction.

### Replacement BIOS angle services

ArcTan (09h) and ArcTan2 (0Ah) now execute rather than stopping as unsupported.
Our original integer vectoring implementation uses shifts/additions and
mathematically generated rotation constants; no Nintendo table or other
emulator implementation was copied. ArcTan accepts a signed Q14 tangent and
returns a signed angle; ArcTan2 returns a wrapped counterclockwise 16-bit turn.
Axes, quadrants, diagonals and the zero vector are handled explicitly. Runtime
math is integer-only, including safe full-width signed magnitudes.

This is a functional mathematical replacement, **not Nintendo's polynomial
approximation**. Its particular rounding, out-of-range behavior, register
clobbers and exact instruction timing remain compatibility/accuracy work.
Tests compare against mathematics, not a physical-firmware oracle, and cannot
prove bit-exact firmware results. Service interface documentation:
https://rust-console.github.io/gbatek-gbaonly/#bios-arithmetic-functions

Validation: every documented 16-bit tangent is tested for mathematical accuracy,
monotonicity and odd symmetry; angle tests cover a signed grid, scale invariance,
axes/diagonals, i32 extremes and 8,192 deterministic full-width vectors.
Original ARM/Thumb cartridges verify SWI dispatch, signed/wrapped results,
return state and checked replay. All 125 GBA unit / 15 original program / 3
EEPROM tests pass (143 GBA checks), alongside 318 self-contained native checks,
JS/Python checks, warnings-denied GBA Clippy and the root WASM check.
The rebuilt GBA browser scenario passes its existing checks and four new
CPU-driven angle-service cartridges, verifying returned register values and
rendered output for signed ArcTan and ArcTan2 quadrants. The separate GB ROM
acceptance suite and full eleven-scenario browser gate were not rerun for these
GBA-only service additions.
FireRed's local 1,800-frame startup probe remains fault-free at the same clocks,
instruction/exception counts and CPU/display state as the preceding milestone.
It took 11.178 s (2.70x native clock throughput), not a controlled speed
comparison, angle-service oracle or complete-game validation.

### Protected BIOS reads

CPU instruction fetches are now distinct from data accesses. Only instructions
executing in the BIOS region can read its bytes as data; cartridge reads return
the protected BIOS opcode latch, with byte/halfword lane selection. Branch and
exception refills can fetch firmware regardless of the previous instruction's
region. Debugger peeks deliberately bypass protection without changing the
latch or permissions. DMA uses the restricted data-read path even if the CPU
was executing firmware. High-level BIOS services execute with BIOS permission;
CpuSet/CpuFastSet retain their explicit BIOS-source rejection.

Version-8 snapshots add the latch and access context after the existing PSG
state. Versions 1-7 still load, using zero for the previously unrecorded latch;
they cannot recover a last BIOS opcode from a cartridge PC. The direct-entry
replacement also starts with a zero latch, not Nintendo boot-firmware values.
The instruction-granular interpreter predicts visible ARM/Thumb BIOS prefetch
and observes refills; a real pipeline, boundary quirks and exact DMA open-bus
values remain accuracy work. This is not a claim of bit-exact Nintendo firmware
behavior. The hardware rule follows:
https://rust-console.github.io/gbatek-gbaonly/#reading-from-bios-memory-00000000-00003fff

Original synthetic firmware reads a private data word, then hands off to ARM
and Thumb cartridges; the cartridges see the opcode latch, not that data.
Tests cover read lanes, non-mutating debug access, refill/Thumb context, DMA
permissions, checked replay and legacy state migration. Validation: 122 GBA
unit / 14 original program / 3 EEPROM tests pass (139 GBA checks), and the
self-contained gate passes 314 native checks plus JS/Python. GBA Clippy with
warnings denied and the root WASM check pass.
The rebuilt GBA browser scenario passes its existing upload/audio/save/speed,
six-mode, BIOS-data and STOP tests, plus a CPU-driven cartridge rendering a
protected BIOS read instead of our nonzero IRQ firmware word. FireRed's local
1,800-frame startup probe remains fault-free at the same 505,612,800 clocks,
159,995,200 instructions, 6,958 exceptions and CPU/display state. This run took
11.269 s (2.67x native clock throughput), not a controlled speed comparison or
full-game validation. The separate GB ROM suite and full eleven-scenario browser
gate were not rerun for this GBA-only change.

### Cartridge page timing and OAM bus width

Game Pak accesses now restart non-sequential timing at every 128-KiB boundary,
even when the CPU/DMA marks the access sequential. The boundary check uses the
aligned physical access address; ARM words still consist of two halfword bus
transfers. All three ROM wait areas and all WAITCNT first/second selections are
covered by original tests. OAM's 32-bit bus now costs one baseline clock for a
word, rather than incorrectly sharing palette/VRAM's two-clock word cost.
WAITCNT's reserved bits and read-only Game Pak type flag cannot be overwritten.
These changes follow the hardware memory-map and system-control documentation:
https://rust-console.github.io/gbatek-gbaonly/#gba-memory-map
https://rust-console.github.io/gbatek-gbaonly/#gba-system-control

Original ARM block-load and DMA3 cartridges verify timing across a ROM page
boundary, loaded values, writeback, CPU stall and a 32-bit OAM destination.
This corrects baseline access costs, not ROM prefetch, video-memory contention,
bus arbitration or sub-instruction peripheral scheduling. Those remain accuracy
work; the backend is not cycle-exact.

Validation: 118 GBA unit / 13 original program / 3 EEPROM tests pass (134 GBA
checks), and the self-contained gate passes 309 native checks plus JS/Python.
GBA Clippy (warnings denied) and the root WASM check pass.
The rebuilt GBA browser scenario passes upload/audio/save/fast-forward, all six
display modes, BIOS-data cartridges and STOP/keypad-wake checks. The local
FireRed 1,800-frame startup probe remains fault-free: 505,612,800 clocks,
159,995,200 instructions, 6,958 exceptions, display 9F40h and PC 08079758h.
Corrected timing changes its sampled PC/instruction count; matching an earlier
sample is not the oracle. This run took 11.369 s (2.65x native clock throughput),
not a controlled speed comparison or full-game compatibility claim. The separate
GB ROM acceptance suite and all eleven browser scenarios were not rerun in this
GBA-only timing milestone.

### STOP mode and keypad wake

HALTCNT now distinguishes HALT from STOP. STOP gates CPU execution, video,
timers, audio and even queued DMA transfers; it does not simulate elapsed sleep
as emulated clocks. BIOS Stop (03h) and CustomHalt (27h) use this same path.
Keypad IRQs now evaluate KEYCNT's enable, selected keys and OR/AND condition
when input or control changes, including atomic halfword/word control writes.
Wake requires an enabled keypad, Game Pak or general-purpose SIO request,
independently of IME and CPSR's IRQ mask. This recognizes external wake requests;
it does not implement a link cable or cartridge peripheral generating them.

Version-7 snapshots preserve STOP without changing existing field offsets or
lengths, and versions 1-6 remain readable. Invalid low-power codes reject
atomically. Hardware sleep is separate from user pause; the frontend drops
unexecuted wall-clock budget so waking cannot trigger a sleep-time catch-up job.
Timing within the instruction entering STOP and physical wake latency remain
approximations, not a hardware-cycle oracle. Documentation used:
https://rust-console.github.io/gbatek-gbaonly/#swi-03h-gba---stop

Validation: 114 GBA unit tests, 11 original program tests and 3 EEPROM tests
(128 GBA checks) pass; the self-contained gate passes 303 native checks and
JS/Python checks. GBA Clippy with warnings denied and the root WASM check pass.
Original ARM/Thumb cartridges verify Stop/CustomHalt return state. Tests check
peripheral freeze, queued DMA resumption, enabled/masked wake sources, keypad
conditions, snapshot migration/replay and frontend pacing after sleep.
The rebuilt GBA browser scenario checks exact serialized-state freeze across
callbacks and restore, followed by keyboard-driven keypad wake, alongside its
existing upload/audio/save/fast-forward, six-mode and BIOS-data checks.
The full eleven-scenario browser gate passes, including GB/CGB, SGB
border/palette/audio/multiplayer and local GBA picker regressions. The separate
local GB ROM acceptance suite was not rerun for this milestone.

### BIOS data-service compatibility

Original implementations now add BitUnPack (10h), Huffman decoding (13h), and
the byte/halfword differential filters (16h-18h). Unpacking handles every
documented source/destination width, low-bit-first units and offset/zero flags.
Huffman handles 4/8-bit symbols, tree offsets and MSB-first bits within stream
words. Differential filters use wrapping cumulative sums. The implementations
use word/halfword bus writes where required, rather than incorrect VRAM byte
writes. They follow the documented formats, not another emulator's code:
https://rust-console.github.io/gbatek-gbaonly/#bios-decompression-functions

Malformed headers, widths, trees and documented alignment/whole-word violations
stop explicitly; decoded output is buffered before destination writes. Invalid
input behavior is defensive, not a reproduction of firmware hangs or overflow.
Odd-length Diff8 VRAM output is zero-padded, like our LZ/RL VRAM services. Exact
firmware instruction timing and register clobbers remain unsupported. Other
BIOS services such as the sound-driver helpers remain work. ArcTan/ArcTan2 now
have the functional mathematical replacement described above.

Validation (2026-10-04): 109 GBA unit tests, 10 original cartridge-program tests
and 3 EEPROM tests pass (122 GBA checks / 296 self-contained native checks),
plus JS/Python checks, warnings-denied GBA Clippy and the root WASM check.
Cartridge tests call all five services through both ARM and Thumb SWIs and
verify return state and output. The rebuilt GBA browser scenario passes its
upload/audio/persistence/fast-forward checks, all six patterned display modes
and five new cartridges rendering BIOS-decoded output through actual WASM.
The separate GB ROM acceptance suite was not rerun for this milestone.
The local FireRed 1,800-frame startup probe also remains fault-free at the same
505,612,801 clocks / 159,868,346 instructions and CPU/display state. This run
took 11.659 s (2.58x native clock throughput), not a controlled speed comparison
or proof of complete gameplay compatibility.

### Display-mode coverage and direct entry

All six display modes are implemented: text/tiled modes 0-2 and bitmap modes
3-5. New patterned tests exercise each mode through scheduled native rendering
and CPU-driven browser cartridges, checking distinct horizontal and vertical
coordinates. This is stronger coverage than the previous uniform-red fixture.

The investigation found a direct-entry initialization gap: zero affine matrices
made every bitmap pixel sample the same VRAM coordinate. BIOS-free cartridge
entry now initializes BG2/BG3 to identity, consistent with our reset service.
User-supplied BIOS execution still starts from raw reset state, and software can
explicitly select a zero transform. This fixes a demonstrated synthetic case,
not a claim to reproduce the user's earlier-version issue or all raster effects.

Validation (2026-10-04): 289 self-contained native checks pass, along with the
GBA browser upload/audio/persistence/fast-forward scenario and six patterned
CPU-driven display fixtures. Clippy and the WASM check pass. The local FireRed
1,800-frame startup probe still reaches the same CPU/display state without a
fault: 505,612,801 clocks / 159,868,346 instructions in 12.563 s (2.40x native
clock throughput with PSG enabled). This is not finished-game speed evidence.

### PSG synthesis milestone

Our original GBA PSG now provides both pulse channels (duty, length, envelopes
and channel-1 sweep), the two-bank rotating wave channel (32/64 digits, volume
and bank switching), and 7/15-bit noise. The frame sequencer follows the GBA
clock rate. Stereo routing, master volume and PSG gain share the DAC mixer with
Direct Sound; integrated digital levels reduce output-rate aliasing.

PSG work is deferred between output samples, then flushed before sound-register
reads/writes, so instruction fetches do not advance four oscillators separately.
Debugger peeks predict pending changes without mutating the machine. Version-6
snapshots retain oscillator/sequencer phases, both wave banks, pending clocks
and partial output integrals; versions 1-5 remain readable. Legacy states retain
their exposed wave bank but cannot supply previously nonexistent PSG phases.

This is not a claim of exact analog DAC/high-pass response, variable-rate PWM,
sub-instruction register timing or every live-envelope/hardware-revision quirk.
The implementation follows hardware documentation, not other emulator code:
https://rust-console.github.io/gbatek-gbaonly/#gba-sound-controller
Original tests cover periods, duty, sweep overflow, length/envelopes, wave banks,
noise cycles, batching equivalence and replay. An original ARM cartridge starts
all four channels through CPU stores; the browser fixture also drives PSG into
the actual Web Audio stereo queue.

Validation: 102 GBA unit tests plus 8 original program / 3 EEPROM integration
tests pass. The full self-contained gate passes 287 native checks and eleven
browser scenarios, including finite nonzero PSG audio through WASM, replay,
Flash persistence and measured fast-forward. GBA Clippy (warnings denied) and
the root WASM check pass. Analog/PWM and hardware-ROM audio validation remain.

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
DMA startup phases, exact CPU register-write timing, cartridge DRQ/capture phases,
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
mid-scanline effects, precise OBJ fetch phases and window/mosaic hardware edge cases
remain accuracy work.

Direct Sound A/B now implement bounded signed-byte FIFOs, timer selection,
stereo routing/gain, clipping and bias/resolution quantization. DMA1/2 sound mode
uses four-word refills and fixed destinations; the scheduler samples at 32,768 Hz
and returns stereo audio through the shared backend API. At this earlier
milestone PSG was missing; it is added above. Precise FIFO request/underflow
behavior and variable-rate PWM filtering remain incomplete.
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
   raster effects, precise OBJ fetch phases and window/mosaic timing quirks.
3. Refine GBA PSG quirks, analog response and Direct Sound request/PWM timing; validate
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

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
predictions. The browser profile includes setup/restore and all three speeds;
its next revision should isolate the actual high-speed execution interval.

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

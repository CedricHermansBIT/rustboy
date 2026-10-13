# GBA timing investigation

The independent release diagnostics run the original mGBA hardware-test ROM,
not an mGBA emulator. Current timing gaps are reported as failures, rather than
accepted as successful hardware behavior.

## Confirmed changes retained

DMA startup and its first data access now execute as separate scheduler events.
The original two/four startup clocks remain in the total, but timers, video,
audio and serial can advance before DMA reads or writes memory. A higher-priority
HBlank request may preempt the lower channel during that boundary. Startup state
is serialized, and EEPROM command setup happens only at the first data access.

Independent DMA tests exposed another concrete issue: the Game Pak ROM address
generator increments even when the readable source-control bits request fixed or
decrementing addresses. Modeling that behavior raised the mGBA DMA result from
1184/1244 to 1244/1244 without changing the test ROM or goldens.

## Timer-start experiment rejected

The original timer-IRQ fixture shows a consistent two-clock early counter read
before IRQ service: for example, its FFF9/no-NOP case reads FFFD instead of FFFB.
After IRQ service, the stopped counter is commonly 004E/004F instead of 0051/0052.
For FFFF/FFFE, the early interrupt can run before the following reload write and
leave the timer frozen at its initial reload value.

A two-clock pending-start model, with arithmetic batching split only at pending
starts, improved aggregate results:

- Timer count-up: 364/936 to 373/936.
- Timer IRQ: 0/90 to 28/90.
- General timing: 564/2020 to 570/2020.

However, comparing every timer assertion showed 27 fixes and **18 regressions**.
For example, 0b/0005, 1xv/1d/1i changed from the expected FFFF to FFFE; 0b/000C,
1xv/1d/2i changed from the expected FFFD to FFFB. The experiment was reverted,
including its unused state field. Aggregate improvement alone is insufficient
evidence for keeping a timing change.

## Required next architecture step

Timer enable, reload, counter reads and IRQ recognition need explicit access
phases. Currently an instruction performs its accesses before the machine advances
its accumulated clock budget, so a timer enabled near the end of an instruction
can receive clocks which physically preceded its enable write. Adding a fixed
delay to that model corrects some observations and moves others away from hardware.

Implement phase-aware MMIO advancement first: materialize pending timer time at
the actual access, preserve the remaining instruction clock budget, and ensure
those clocks are not advanced twice. Then introduce the measured enable and IRQ
latency, using the independent per-assertion matrix as the acceptance criterion.
Keep ordinary RAM operations and event-free instruction batches on their existing
fast path. Extend equivalent access-phase handling to DMA data reads/writes where
tests expose a difference; the retained DMA startup event is an initial boundary,
not a claim of full bus-cycle arbitration.

Sources: [original independent test source](https://github.com/mgba-emu/suite),
[author's hardware-test description](https://forums.mgba.io/showthread.php?pid=63),
and [documented GBA timer/DMA behavior](https://rust-console.github.io/gbatek-gbaonly/).
The pinned ROM/source provenance and current full results are recorded by the
compatibility gate; commercial gameplay and firmware cycle accuracy remain
separate validation requirements.

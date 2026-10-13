# GBA stability release gate

GBA remains a development backend. The compatibility gate deliberately reports
hardware failures instead of treating interpreter/JIT agreement as accuracy.
No commercial cartridge, Nintendo firmware or external emulator implementation
is included in the repository.

## Current independent results

The externally supplied, hash-pinned mGBA suite runs its original startup and
test functions. Current measured results:

| Suite | Passing assertions | Remaining issue |
| --- | ---: | --- |
| DMA | 1244/1244 | Broader arbitration still needs coverage |
| I/O reads | 130/130 | Raster latches and bus contention are separate |
| BIOS math | 615/615 | Other services' exact cycle costs remain approximate |
| SIO register reads/writes | 90/90 | UART/JoyBus transport is unsupported |
| Memory | 1542/1552 | Ten retail BIOS opcode-identity expectations |
| CPU timing | 564/2020 | Pipeline, prefetch and access-phase timing |
| Timers | 364/936 | Instruction-phase ordering and edge observations |
| Timer IRQ | 0/90 | Exact IRQ entry/observation timing |
| Normal serial timing | 0/4 | IRQ/BIOS Halt observation timing |

Disconnected multiplayer waits for the cartridge's timeout; the independent
author skips those four serial timing cases. They are not counted as passes.
Smolka ARM, Thumb, memory, SRAM, no-save and three graphics suites also pass.
The firmware-identity and immediate-Flash-operation fixtures remain diagnostics.

```bash
python3 scripts/check_gba_compatibility.py --fetch --baseline
python3 scripts/check_gba_compatibility.py --fetch
python3 scripts/check.py --offline --gba-jit
```

The baseline gate rejects regressions against explicit measured results. The
strict command fails while known hardware failures remain; a successful baseline
run does not make the emulator release-ready. Fixture provenance, hashes and
accepted failed groups are in `tests/gba_compatibility_cases.json`.

## Work added for the seven stability areas

1. Independent release checks now include timing, DMA, IRQ, graphics, save and
   serial assertions. Local gameplay checkpoints cover FireRed bedroom movement,
   Classic NES Metroid's Brinstar, Phantasy Star's story textbox and Yoshi's intro.
   The last two do not establish level gameplay. The runner checks deterministic
   state replay, finite audio, battery reload and reviewed frame hashes. Longer
   songs, levels and complete multiplayer matches remain release requirements.
2. Save detection accepts padded SDK tags and infers untagged chips from imported
   battery size. A persistent save-type override applies before importing bytes.
   Flash has delayed program/erase and busy polling. Browser storage failures
   warn visibly, retain dirty progress for retry and protect corrupt saved copies
   against replacement. Flash durations are nominal, not chip-specific measures.
3. Replacement BIOS music supports native PSG instruments and common extended
   commands. Mathematical register/rounding cases have independent coverage.
   HardReset preserves cartridge-backed state. MultiBoot executes original guest
   firmware through the ordinary CPU and serial scheduler; its built-in receiver
   supports two-player multiplayer-mode downloads. Obscure sound commands,
   undocumented helpers and exact service timing remain incomplete.
4. DMA pays a separate startup phase before its first bus operation, rechecks
   priority and preserves paid startup in snapshots. ROM DMA source wiring and
   unaligned save-bus lanes match independent fixtures. A general retained CPU
   pipeline and full bus arbitration remain unfinished. See `GBA_TIMING_ROADMAP.md`.
5. Relevant visible-line writes preserve the preceding pixels and render the
   remaining segment using the changed state. Unchanged and skipped scanlines
   retain their inexpensive paths. Pixel boundaries use an approximate fetch
   offset; precise register latches, contention and sprite fetches remain open.
6. Tagged RTC cartridges have GPIO/BCD clock support, snapshot validation and a
   persistent sidecar that advances for offline elapsed time. RTC advances with
   emulated clocks during play. Two-browser WebRTC cable, invite rooms, current
   cartridge sharing and a cartridge-free download receiver are experimental.
   Four-player, wireless, UART/JoyBus, solar/tilt/rumble and other-emulator network
   protocols are excluded. See `GBA_LINK.md` for tested behavior and setup.
7. Generated-WASM JIT checks run in CI instead of relying only on native tests.
   Browser regressions exercise live WebRTC, serial IRQs, state-import disconnects,
   save failures and actual cartridge upload. Exact PSG live-envelope/FIFO/PWM
   quirks and long-session audio still need independent validation. Full JIT
   remains disabled by default.

Snapshot version 15 retains migration from versions 1–14. Live connections never
resume from a snapshot; the hardware phase restores without external peers.
Firmware migration recognizes only known original replacement images. Existing
cartridge battery keys are unchanged.

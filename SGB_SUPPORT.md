# Super Game Boy: first implementation milestone

RustBoy has an **opt-in, experimental high-level SGB adapter**. It is not full
Super Game Boy hardware emulation and does not run Nintendo SGB/SNES firmware.
Automatic mode still selects ordinary GB/GBC hardware, as before.

In the ROM picker, select **Hardware for next load → Super Game Boy —
experimental**, then upload/select a game. A CGB-compatible cartridge runs on
the DMG path; a CGB-only cartridge is rejected without replacing the running
game. Non-SGB games can still run, with a neutral grayscale fallback palette.
The choice applies to the next load, not to an already-running cartridge.

## Implemented

- JOYP bit-4/bit-5 packet reception: reset, release pulses, LSB-first bytes,
  mandatory zero stop bit, bounded 1–7-packet commands and continuation data.
  Incomplete/malformed transfers cannot apply half a command.
- Cartridge header gating: SGB flag `0x03` and old licensee code `0x33`.
- `PAL01`, `PAL23`, `PAL03`, `PAL12`: little-endian RGB555 colors and a shared
  color-zero backdrop.
- `ATTR_BLK`, `ATTR_LIN`, `ATTR_DIV`, `ATTR_CHR`: 20×18 screen-tile attributes,
  multi-packet data, horizontal/vertical addressing and wrapping.
- Colorization of the final LCD shade **after BGP/OBP**, including sprites and
  mid-scanline palette changes. Raw tile color is not mistaken for LCD shade.
- `MASK_EN`: normal display, frozen image, black and backdrop-color masks.
  LCD-off retains the last complete frame rather than clearing the host canvas.
- `MLT_REQ`: controller IDs and multiplexing for 1, 2 or 4 players. The native
  `Emulator::set_button` API accepts ports 0–3; browser controls still drive only
  port 0. Full browser gamepad/multiplayer controls are future frontend work.
- `ICON_EN` register-file-disable bit; the other menu-related bits have no host
  menu implementation. Packet reception stops when requested, but controller
  multiplexing remains active.
- Bundled RustBoy startup followed by SGB identification (`A=0x01`, `C=0x14`).
  This is not a reproduction of the original SGB boot sequence/timing.
- Versioned, checksummed SGB snapshots including pending packets, attributes,
  palettes, masks, controller selection and the frozen display. Controller
  button presses remain host-owned and are released on import.

Battery saves retain the cartridge's existing identity and can be shared across
handheld/SGB modes. SGB save states use a separate `-sgb-hle-v1` identity and an
`RBSG` envelope; handheld snapshots keep their original `RBST` format. Loading a
snapshot into the wrong mode is rejected. For SGB snapshots use the backend's
`Emulator::export_state`, not the low-level CPU-only snapshot API.

Run `sgb()` in the browser console to see header gating, active player count,
received-command count and unsupported command codes/counts. Unsupported
commands are counted, not reported as implemented.

## Not implemented yet

- LCD/VRAM bulk transfers, `PAL_TRN`/`PAL_SET`, `ATTR_TRN`/`ATTR_SET` and
  transferred system palette/attribute tables. Games relying on these may have
  missing/incorrect colors even when direct palette commands work.
- Custom/built-in 256×224 SNES borders, `CHR_TRN`, `PCT_TRN`, SNES objects.
- SNES sounds, music, SPC700/DSP and sound-transfer commands. Normal Game Boy
  APU audio continues to work.
- SNES CPU/bus/PPU, `DATA_SND`/`DATA_TRN` patch execution and `JUMP` (including
  Space Invaders' SNES arcade mode).
- SNES system menus and user-selected palette/border overrides.
- Cycle-accurate ICD2 pulse timing, transfer scheduling and SGB1's faster clock.
  This adapter uses the GB/SGB2 base rate of 4,194,304 Hz; it does not claim
  hardware-identical SGB1/SGB2 behavior.

This milestone does **not** make existing hardware-variant ROM tests applicable:
original boot/timing fixtures and exclusions remain unchanged.

## Architecture and next stages

`crates/gameboy/src/sgb.rs` separates packet reception, completed command data,
command interpretation and display/controller state. CPU JOYP writes feed the
adapter; the PPU retains post-palette LCD shade metadata and supplies completed
frames at VBlank. Neither path depends on DOM, storage or an audio device.

The next stage is LCD-stream bulk transfers and their multi-frame scheduling,
then palette/attribute tables and border composition. Full SGB will require a
composed Game Boy + ICD2 + SNES backend with a shared internal timeline. Its
SNES CPU/audio/graphics components should then be reusable by a standalone SNES
backend; this high-level interpreter is not a substitute for those components.

## Verification

```sh
cargo test --locked --release --no-default-features -p rustboy-gameboy --lib sgb
python3 scripts/build_web.py
RUSTBOY_NO_BOOT=1 RUSTBOY_SYNTHETIC_SGB=1 node tests/browser_smoke.mjs
python3 scripts/check.py --browser
```

Fifteen initial unit/integration tests cover the protocol, command semantics,
controller bus, frame masks, startup, state isolation and malformed snapshots.
Original synthetic cartridge programs execute real LR35902 instructions to send
commands and verify BG/OBJ shade mapping. The browser smoke checks upload/model
selection, bundled boot without external firmware, command-driven canvas colors,
state restore and preservation of the running game after an incompatible upload.
These are not commercial-game playthroughs or a complete SGB compatibility suite.

The implementation follows primary [Pan Docs packet transport](https://gbdev.io/pandocs/SGB_Command_Packet.html),
[palette commands](https://gbdev.io/pandocs/SGB_Command_Palettes.html),
[attribute commands](https://gbdev.io/pandocs/SGB_Command_Attribute.html),
[multiplayer](https://gbdev.io/pandocs/SGB_Command_Multiplayer.html),
[system controls](https://gbdev.io/pandocs/SGB_Command_System.html) and
[header/detection](https://gbdev.io/pandocs/SGB_Unlocking.html) descriptions.

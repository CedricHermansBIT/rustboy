# Super Game Boy: experimental adapter and custom borders

RustBoy has an **opt-in, experimental high-level SGB adapter**. It is not full
Super Game Boy hardware emulation and does not run Nintendo SGB/SNES firmware.
Automatic mode still selects ordinary GB/GBC hardware, as before.

In the ROM picker, select **Hardware for next load → Super Game Boy —
experimental**, then upload/select a game. A CGB-compatible cartridge runs on
the DMG path; a CGB-only cartridge is rejected without replacing the running
game. Non-SGB games can still run, with a neutral grayscale fallback palette.
The choice applies to the next load, not to an already-running cartridge.

SGB colorization is separate from CGB rendering: SGB uses the four-shade Game
Boy image and applies its own palettes, not a cartridge's native CGB colors.
Direct palette commands work, but games using the still-unimplemented
`PAL_TRN`/`PAL_SET` tables can remain grayscale or have incorrect colors.

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
- `CHR_TRN`: both 128-tile halves of the game-provided 4-bitplane border.
  `PCT_TRN`: tilemap and three 16-color RGB555 border palettes (4–6), with
  horizontal/vertical flips and transparent color zero. Opaque border pixels
  can cover the game window, rather than being forcibly clipped outside it.
- LCD-stream bulk transfers for those two border commands. The first complete
  frame after a request supplies 4 KiB of post-BGP/OBP display data, decoded in
  visible 20-tile row order. The result is published after five complete frames.
  `MASK_EN` hides transfer garbage without blocking receipt. Up to four requests
  can overlap, retaining their own captured payloads; overflow is counted.
- The viewport expands from 160×144 to 256×224 when a custom `PCT_TRN` completes.
  The game occupies the central 160×144 region at (48,40). Reset, old snapshot
  import or loading another handheld game restores the appropriate geometry.
  Browser canvas and save previews preserve the entire border image.
- `MLT_REQ`: controller IDs and multiplexing for 1, 2 or 4 players. The native
  `Emulator::set_button` API accepts ports 0–3; browser controls still drive only
  port 0. Full browser gamepad/multiplayer controls are future frontend work.
- `ICON_EN` register-file-disable bit; the other menu-related bits have no host
  menu implementation. Packet reception stops when requested, but controller
  multiplexing remains active.
- Bundled RustBoy startup followed by SGB identification (`A=0x01`, `C=0x14`).
  This is not a reproduction of the original SGB boot sequence/timing.
- Versioned, checksummed SGB snapshots including pending packets, attributes,
  palettes, masks, controller selection, frozen display, borders and pending
  bulk transfers. Version 2 also stores the partially rendered LCD frame so a
  mid-transfer restore can replay its payload correctly. Controller button
  presses remain host-owned and are released on import. Version 1 snapshots
  remain readable, with no border/pending bulk transfer to restore.

Battery saves retain the cartridge's existing identity and can be shared across
handheld/SGB modes. SGB save states use a separate `-sgb-hle-v1` identity and an
`RBSG` envelope (now version 2); the storage identity remains stable so older
SGB autosaves can migrate. Handheld snapshots keep their original `RBST` format. Loading a
snapshot into the wrong mode is rejected. For SGB snapshots use the backend's
`Emulator::export_state`, not the low-level CPU-only snapshot API.

Run `sgb()` in the browser console to see header gating, active player count,
received-command count, border presence, pending/dropped bulk transfers and
unsupported command codes/counts. Unsupported
commands are counted, not reported as implemented.

## Not implemented yet

- `PAL_TRN`/`PAL_SET`, `ATTR_TRN`/`ATTR_SET` and
  transferred system palette/attribute tables. Games relying on these may have
  missing/incorrect colors even when direct palette commands work.
- Built-in Nintendo borders, SNES objects/`OBJ_TRN` and the firmware's border
  fade/menu animations. A game without a custom border keeps a 160×144 viewport.
- SNES sounds, music, SPC700/DSP and sound-transfer commands. Normal Game Boy
  APU audio continues to work.
- SNES CPU/bus/PPU, `DATA_SND`/`DATA_TRN` patch execution and `JUMP` (including
  Space Invaders' SNES arcade mode).
- SNES system menus and user-selected palette/border overrides.
- Cycle-accurate ICD2 pulse timing, transfer scheduling and SGB1's faster clock.
  This adapter uses the GB/SGB2 base rate of 4,194,304 Hz; it does not claim
  hardware-identical SGB1/SGB2 behavior.

The border transfer schedule above is an explicit HLE approximation: it samples
one stable complete LCD frame and defers publication, rather than emulating the
firmware's per-chunk reads. Games should keep transfer data stable as required by
the protocol. The nominal 28-row border viewport also omits the SNES layer's
one-scanline vertical-scroll/29th-row flicker quirk. `CHR_TRN`'s BG/OBJ flag uses
the same tile store; this does not implement SNES sprite commands. No built-in
Nintendo border graphics or commercial game data have been added to the repo.

This milestone does **not** make existing hardware-variant ROM tests applicable:
original boot/timing fixtures and exclusions remain unchanged.

## Architecture and next stages

`crates/gameboy/src/sgb.rs` separates packet reception, completed command data,
command interpretation and display/controller state. CPU JOYP writes feed the
adapter; the PPU retains post-palette LCD shade metadata and supplies completed
frames at VBlank. Neither path depends on DOM, storage or an audio device.

The next stage is transferred palette/attribute tables, followed by more precise
transfer scheduling and the remaining SGB features. Full SGB will require a
composed Game Boy + ICD2 + SNES backend with a shared internal timeline. Its
SNES CPU/audio/graphics components should then be reusable by a standalone SNES
backend; this high-level interpreter is not a substitute for those components.

## Verification

```sh
cargo test --locked --release --no-default-features -p rustboy-gameboy --lib sgb
python3 scripts/build_web.py
RUSTBOY_NO_BOOT=1 RUSTBOY_SYNTHETIC_SGB=1 node tests/browser_smoke.mjs
RUSTBOY_NO_BOOT=1 RUSTBOY_SGB_BORDER=1 node tests/browser_smoke.mjs
python3 scripts/check.py --browser
```

Twenty-six unit/integration tests cover the protocol, command semantics,
controller bus, frame masks, startup, border formats/transfers, state isolation,
legacy snapshot migration and malformed snapshots.
Original synthetic cartridge programs execute real LR35902 instructions to send
commands and verify BG/OBJ shade mapping. The browser smoke checks upload/model
selection, bundled boot without external firmware, command-driven canvas colors,
state restore and preservation of the running game after an incompatible upload.
Border tests exercise both tile halves and verify the expanded canvas, game
window transparency/overlay, desktop/portrait fit, reset/restore and switching
back to a handheld cartridge. To inspect the original synthetic border ROM:

```sh
cargo run --locked --release --no-default-features --example sgb_border_fixture -- /tmp/rustboy-border-demo.gb
```

Upload the resulting file with SGB hardware selected. The generator refuses to
overwrite an existing file. Its source is shared by native/browser regression
tests; the generated ROM is not added to the public game catalog.
These are not commercial-game playthroughs or a complete SGB compatibility suite.

The implementation follows primary [Pan Docs packet transport](https://gbdev.io/pandocs/SGB_Command_Packet.html),
[palette commands](https://gbdev.io/pandocs/SGB_Command_Palettes.html),
[attribute commands](https://gbdev.io/pandocs/SGB_Command_Attribute.html),
[multiplayer](https://gbdev.io/pandocs/SGB_Command_Multiplayer.html),
[system controls](https://gbdev.io/pandocs/SGB_Command_System.html) and
[header/detection](https://gbdev.io/pandocs/SGB_Unlocking.html) descriptions.
Borders and bulk-transfer requirements follow [border commands](https://gbdev.io/pandocs/SGB_Command_Border.html)
and [LCD/VRAM transfers](https://gbdev.io/pandocs/SGB_VRAM_Transfer.html).

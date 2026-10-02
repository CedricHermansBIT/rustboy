# Super Game Boy: automatic detection, palettes and custom borders

RustBoy has a **high-level SGB adapter**. It is not full
Super Game Boy hardware emulation and does not run Nintendo SGB/SNES firmware.
Automatic mode detects SGB enhancement from header flag `0x03` and old licensee
code `0x33`, and prefers SGB for enhanced cartridges, including dual-mode CGB
games. CGB-only cartridges always use CGB. Other cartridges select GB/CGB from
their CGB header flag. Filename extensions do not determine the hardware.

In the ROM picker, **Hardware for next load** can override detection with
Game Boy, Game Boy Color or Super Game Boy. In SGB, a CGB-compatible cartridge runs on
the DMG path; a CGB-only cartridge is rejected without replacing the running
game. Non-SGB games can still run, with a neutral grayscale fallback palette.
The choice applies to the next load, not to an already-running cartridge.

**Show SGB decorations**, under Appearance and in the touch-settings menu,
switches between the full 256×224 border and the 160×144 game image. This is a
persisted host preference: transfers, palettes, inputs and emulation continue
unchanged, and restoring a snapshot does not override it. H hides the interface
panels independently of debug mode.

Debug tools start hidden/off. Press the backtick key or **Show debug tools** to
reveal them; that does not start instruction logging. F5 remains browser refresh.
With tools enabled, F2 opens the memory viewer and its dropdown selects GB tiles,
SGB border tiles (all 256 tiles shown in each of palettes 4–6), or the SGB border
map. Border tiles belong to separate SNES-side memory, not a larger GB VRAM bank.
Turning debug tools off also disables logging, tracing, the HUD and the viewer.

SGB colorization is separate from CGB rendering: SGB uses the four-shade Game
Boy image and applies its own palettes, not a cartridge's native CGB colors.
Both direct palettes and transferred `PAL_TRN`/`PAL_SET` tables now work.
Non-SGB games retain the neutral grayscale fallback; built-in firmware palettes
and user-selected palette overrides are not included.

## Implemented

- JOYP bit-4/bit-5 packet reception: reset, release pulses, LSB-first bytes,
  mandatory zero stop bit, bounded 1–7-packet commands and continuation data.
  Incomplete/malformed transfers cannot apply half a command.
- Cartridge header gating: SGB flag `0x03` and old licensee code `0x33`.
- `PAL01`, `PAL23`, `PAL03`, `PAL12`: little-endian RGB555 colors and a shared
  color-zero backdrop.
- `PAL_TRN`: 512 four-color system palettes from a 4 KiB LCD-stream transfer.
  `PAL_SET`: nine-bit palette selection, shared color zero, optional attribute
  file application and independent `MASK_EN` cancellation.
- `ATTR_TRN`: 45 packed 20×18 attribute files; `ATTR_SET` and `PAL_SET` decode
  the selected file in MSB-first screen-row order. Invalid file IDs leave
  attributes unchanged without preventing a requested mask cancellation.
  Loading either table alone does not change the visible palettes/attributes.
- `ATTR_BLK`, `ATTR_LIN`, `ATTR_DIV`, `ATTR_CHR`: 20×18 screen-tile attributes,
  multi-packet data, horizontal/vertical addressing and wrapping.
- Colorization of the final LCD shade **after BGP/OBP**, including sprites and
  mid-scanline palette changes. Raw tile color is not mistaken for LCD shade.
- `MASK_EN`: normal display, frozen image, black and backdrop-color masks.
  LCD-off retains the last complete frame rather than clearing the host canvas.
  Freeze retains the LCD shades, so palette/attribute changes still recolor
  that image without exposing the intervening transfer graphics.
- `CHR_TRN`: both 128-tile halves of the game-provided 4-bitplane border.
  `PCT_TRN`: tilemap and three 16-color RGB555 border palettes (4–6), with
  horizontal/vertical flips and transparent color zero. Opaque border pixels
  can cover the game window, rather than being forcibly clipped outside it.
- LCD-stream bulk transfers for border, palette and attribute commands. The first
  complete frame after a request supplies 4 KiB of post-BGP/OBP display data, decoded in
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
  remain readable, with no border/pending bulk transfer to restore. Version 3
  adds transferred tables and packed frozen LCD shades. Both older versions
  retain their saved RGBA image until the next unmasked LCD frame, rather than
  guessing shades from possibly identical palette colors.

Battery saves retain the cartridge's existing identity and can be shared across
handheld/SGB modes. SGB save states use a separate `-sgb-hle-v1` identity and an
`RBSG` envelope (now version 3); the storage identity remains stable so older
SGB autosaves can migrate. Handheld snapshots keep their original `RBST` format. Loading a
snapshot into the wrong mode is rejected. For SGB snapshots use the backend's
`Emulator::export_state`, not the low-level CPU-only snapshot API.

Run `sgb()` in the browser console to see header gating, active player count,
received-command count, border presence, screen mask, pending/dropped bulk
transfers and unsupported command codes/counts. Unsupported commands are
counted, not reported as implemented.

## Not implemented yet

- Built-in Nintendo borders, SNES objects/`OBJ_TRN` and the firmware's border
  fade/menu animations. A game without a custom border keeps a 160×144 viewport.
- SNES sounds, music, SPC700/DSP and sound-transfer commands. Normal Game Boy
  APU audio continues to work.
- SNES CPU/bus/PPU, `DATA_SND`/`DATA_TRN` patch execution and `JUMP` (including
  Space Invaders' SNES arcade mode).
- SNES system menus, user-selected palette/border overrides and `PAL_PRI`.
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

The next stages are broader game compatibility checks, more precise transfer
scheduling and the remaining SGB features. Full SGB will require a
composed Game Boy + ICD2 + SNES backend with a shared internal timeline. Its
SNES CPU/audio/graphics components should then be reusable by a standalone SNES
backend; this high-level interpreter is not a substitute for those components.

## Verification

```sh
cargo test --locked --release --no-default-features -p rustboy-gameboy --lib sgb
python3 scripts/build_web.py
RUSTBOY_NO_BOOT=1 RUSTBOY_SYNTHETIC_SGB=1 node tests/browser_smoke.mjs
RUSTBOY_NO_BOOT=1 RUSTBOY_SGB_BORDER=1 node tests/browser_smoke.mjs
RUSTBOY_NO_BOOT=1 RUSTBOY_SGB_PALETTES=1 node tests/browser_smoke.mjs
python3 scripts/check.py --browser
```

Thirty-nine unit/integration tests cover the protocol, command semantics,
controller bus, frame masks, startup, border formats/transfers, state isolation,
legacy snapshot migration and malformed snapshots.
Table tests cover the 512-palette table, upper-half palette selection, shared backdrop, first/last
attribute files, mask cancellation, frozen-image recoloring and mid-transfer
snapshot replay. An original synthetic cartridge reproduces an all-white
palette followed by `PAL_SET`, the sequence that previously hid Harvest Moon's
game despite its running CPU/LCD and visible custom border.
Original synthetic cartridge programs execute real LR35902 instructions to send
commands and verify BG/OBJ shade mapping. The browser smoke checks upload/model
selection, bundled boot without external firmware, command-driven canvas colors,
state restore and preservation of the running game after an incompatible upload.
Border tests exercise both tile halves and verify the expanded canvas, game
window transparency/overlay, desktop/portrait fit, reset/restore and switching
back to a handheld cartridge. Presentation tests cover automatic hardware
selection, border hiding without state mutation and both SNES memory views.
Browser tests also verify opt-in debug tools, F5 refresh behavior and independent
appearance controls. To inspect the original synthetic border ROM:

```sh
cargo run --locked --release --no-default-features --example sgb_border_fixture -- /tmp/rustboy-border-demo.gb
```

Upload the resulting file with SGB hardware selected. The generator refuses to
overwrite an existing file. Its source is shared by native/browser regression
tests; the generated ROM is not added to the public game catalog.
Local startup/attract-screen checks of Harvest Moon GB (USA), Harvest Moon GB
(Europe, GB-compatible) and Harvest Moon GBC (USA, GB-compatible) show a
nonblank, colorized game window with custom border data loaded. No commercial ROM or
graphics are included in the automated fixtures. These checks are not complete
playthroughs or a complete SGB compatibility suite. For your own local ROM:

```sh
cargo run --locked --release --no-default-features --example sgb_inspect -- "path/to/game.gb" 1200
RUSTBOY_NO_BOOT=1 RUSTBOY_HARDWARE=sgb RUSTBOY_ROM="path/to/game.gb" RUSTBOY_GAME_WAIT_MS=22000 node tests/browser_smoke.mjs
```

The inspector reports masks, unsupported commands, LCD shades and final
game-window colors. The browser check examines the central game window rather
than accidentally treating a visible border around a blank game as success.

The implementation follows primary [Pan Docs packet transport](https://gbdev.io/pandocs/SGB_Command_Packet.html),
[palette commands](https://gbdev.io/pandocs/SGB_Command_Palettes.html),
[attribute commands](https://gbdev.io/pandocs/SGB_Command_Attribute.html),
[multiplayer](https://gbdev.io/pandocs/SGB_Command_Multiplayer.html),
[system controls](https://gbdev.io/pandocs/SGB_Command_System.html) and
[header/detection](https://gbdev.io/pandocs/SGB_Unlocking.html) descriptions.
Borders and bulk-transfer requirements follow [border commands](https://gbdev.io/pandocs/SGB_Command_Border.html)
and [LCD/VRAM transfers](https://gbdev.io/pandocs/SGB_VRAM_Transfer.html).

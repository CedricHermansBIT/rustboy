# Curated public homebrew

Only this catalog's explicitly licensed games are included in the public Pages
build. "Free to download", a public GitHub repository, or a mirror's license tag
is not sufficient permission by itself. Code and assets must both be covered.
No ROM binaries or downloaded source archives are committed here.

## Games and provenance

- **2048 — Game Boy Edition**, by Wyatt Ferguson. Unmodified 32 KiB ROM from
  [upstream](https://github.com/wyattferguson/2048-gb/tree/167788105db659f2049dd2d537ee12f63d87b1e7).
  MIT license; the full copyright/permission notice is retained in
  [licenses/2048-MIT.txt](licenses/2048-MIT.txt).
- **Tobu Tobu Girl Deluxe**, by Tangram Games, with original soundtrack by
  **potato-tan**. [Creator's page](https://tangramgames.itch.io/tobu-tobu-girl-deluxe).
  [Upstream source/license statement](https://github.com/SimonLarsen/tobutobugirl-dx/tree/4985d4d282aa43229f250ffa3b0d86998ebd55d3):
  MIT code and **CC BY 4.0 for all images, text, sound and music**. The full MIT
  notice and CC BY terms are included. The unmodified ROM is downloaded from
  [Homebrew Hub's pinned mirror](https://github.com/gbdev/database/tree/50293559a496a3e20382fbf6a2e84b70ec622f88/entries/tobutobugirldeluxe),
  not from expiring itch.io session URLs. Its MIT-only metadata does not override
  the author's separate asset license.
- **µCity 1.3**, by Antonio Niño Díaz (AntonioND / SkyLyrac). Unmodified standard
  `ucity.gbc` from the [official v1.3 release](https://codeberg.org/SkyLyrac/ucity/releases/tag/v1.3),
  not the reduced-save-capacity compatibility version. It uses 128 KiB of battery
  RAM. [Source at the v1.3 commit](https://codeberg.org/SkyLyrac/ucity/src/commit/d1880a2a112d7c26f16c0fc06a15b6c32fdc9137):
  GPL-3.0-or-later code, BSD-2-Clause GBT Player, and CC BY-SA 4.0 graphics/music.
  The full notices are included. The pinned **complete corresponding source ZIP**,
  including build scripts, assets and component notices, is shipped alongside
  the ROM at `homebrew/sources/ucity-v1.3.zip`. The credits page provides both
  that local download and the upstream source link; a moving upstream link alone
  is not our source distribution mechanism.

License texts were taken from the pinned upstream repositories and Creative
Commons' official legal-code text URLs:

- https://creativecommons.org/licenses/by/4.0/legalcode.txt
- https://creativecommons.org/licenses/by-sa/4.0/legalcode.txt

## Packaging and updates

Run `python3 scripts/prepare_pages.py --output <empty-directory>` after building
WASM. The script validates the catalog, downloads and verifies every payload,
then stages the site using an allowlist: `index.html`, `out/`, `web/`, the project
license, public games, matching source and notices. It does not copy `roms/` or
`testroms/`, even when those directories exist locally.

The generated `romlist.json` contains game titles, credits, platforms, expected
sizes and hashes. The picker offers a visible credits/licenses/source link;
all browser paths are relative so GitHub project Pages subdirectories work.
The browser verifies each curated ROM before passing it to WASM. Failed checks
must leave the previous game and save namespace intact.

Updates require deliberate URL/hash changes, rechecking upstream code **and
asset** license terms, updating matching source when applicable, and rerunning
the native/browser smoke checks documented in the root README. The smoke checks
cover startup, game rendering/input, state replay and save round trips—not a
claim of complete gameplay testing or completion of every game.

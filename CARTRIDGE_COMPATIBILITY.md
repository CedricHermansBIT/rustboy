# Unlicensed cartridge compatibility

`(Unl)` means unlicensed, not a different console. A `.gbc` filename and a
Pokémon title do not imply an official Nintendo game, nor reliably identify
the cartridge's banking hardware. RustBoy selects GB/CGB/SGB from header flags,
not filenames. Unlicensed hardware is not yet comprehensively supported.

## Two different Diamond games

The local files checked during this investigation are distinct:

| Local filename | Header title | Size | Declared mapper | Result |
| --- | --- | --- | --- | --- |
| `Pokemon Diamond (Taiwan) (En) (Unl).gbc` | `POKEMONDIAMOND` | 512 KiB | MBC1, no RAM (misleading) | NT-new mapper fix: nonblank, 24-color game screen |
| `Pocket Monsters Diamond (Taiwan) (En) (Unl).gbc` | `TELEFANG PWBTXJ` | 2 MiB | MBC3 + RAM + RTC | Rendered a nonblank, 12-color screen; not a full playthrough |

The first corresponds to Makon's Pikachu platformer family, not the Telefang
RPG translation. [The cartridge dumper's account](https://hhug.me/?page=7)
describes Makon's separate Diamond/Jade releases and their custom NT banking.
The second is the Telefang Power bootleg, as described by
[Wikifang](https://wiki.telefang.net/Bootleg). Neither is the Nintendo DS game.

File identities, to avoid conflating differently patched dumps:

```text
Pokemon Diamond:
7aefd42da1fdb1230cf8319b9e06eaaaabdffa34de3bc4c3b15e1feb2adb722c
Pocket Monsters Diamond:
36d434a24d3eb39b75e00c95bd9de9802226a22024a98acc17260924d32bd0b1
```

## White-screen findings

Before the mapper fix, after 1,200 nominal frames with Start/A input, the 512 KiB file displayed
one color. Its CPU continues executing, enters native CGB double speed and
services interrupts, but game graphics are not uploaded. It also remained white
with a locally supplied original CGB boot ROM, so this is not simply missing
firmware or an SGB decoration hiding the game. A diagnostic in-memory switch
to standard MBC5 did not fix it; the ROM file was not altered.

An independent SameBoy core check (source revision
`213a12ce93d66b105a113debd9396306066a7cfc`, original local CGB firmware,
1,200 frames) also produced white output and reported an illegal opcode halt.
That is corroboration of incompatibility, **not proof that the dump is corrupt**
or that RustBoy has no relevant bugs.

[hhugboy's NT-new implementation](https://github.com/tzlion/hhugboy/blob/b54549b70faf49ff73029f3cf7861d82e1a24543/src/memory/mbc/MbcUnlNtNew.cpp)
documents independent 8 KiB banking for later Makon hardware: writing `55` to
`1400–14FF` enables split mode; writes at `2000–20FF` and `2400–24FF` select
the two halves of ROMX. A standard MBC1/MBC5 substitution does not emulate
that behavior. Instruction-level tracing of this exact file subsequently showed
`PC=1770: [1400]=55`, followed by bank writes such as `[2000]=3E`: activation
and bank numbers matching NT-new hardware. Implementing those two independent
8 KiB windows restored graphics uploads and a 24-color game screen with Start/A
input. Synthetic tests cover activation, independent halves, wrapping/remapping,
ordinary MBC5-style banking and snapshot restoration.

Automatic detection is restricted to verified 512 KiB dumps (the SHA-256 above;
runtime FNV-1a-64 fingerprint `2cf5e0619327cc73`, and the three additional dumps
below). A filename or title match does
not enable it, and the header/ROM bytes are never modified. Other NT variants
and other differently patched dumps are not claimed supported. Existing states
saved during the earlier failed MBC1 execution may still restore that old state;
reset the cartridge rather than resuming such an autosave.

This is a startup/input compatibility check, not a complete game playthrough.
The dumper reports progression bugs in the original platformer; emulator support
does not fix the game itself. Do not treat every unlicensed cartridge as NT-new.

## Reproduce with your own files

```sh
cargo run --locked --release --no-default-features --example cartridge_inspect -- 'path/to/game.gbc'
```

The read-only diagnostic reports CPU/banking state, VRAM/palettes, distinct
frame colors and recent instructions. Optional `RUSTBOY_BOOT_ROM` supplies
local firmware; `RUSTBOY_MAPPER=19` changes only the diagnostic's in-memory
header to MBC5. Neither option is a compatibility fix or modifies the file.
`RUSTBOY_TRACE_CART=1` additionally prints the first 100 cartridge-register
writes observed over two million instruction steps.
`RUSTBOY_NT_NEW=1` forces the NT-new controller only for the diagnostic run.

## Pearl, Chinese Diamond/Jade and the broader library

All three additional files also execute `[1400]=55`. Standard MBC1/MBC5 produced
one-color screens; NT-new restored these nonblank screens after Start/A input:

| Exact local filename | Game colors | SHA-256 |
| --- | --- | --- |
| `Pokemon Pearl (Taiwan) (En) (Unl).gbc` | 5 | `ae7b0a92edec8544372bc31d88eacdfae525e023de9dbbc4633dc34e1b18596d` |
| `Pokemon Diamond (Taiwan) (Zh) (Unl).gbc` | 24 | `6cf9782a6f1cdc768213cc058e93a860b210314d289007a8811c8b1c09f37c9e` |
| `Pokemon Jade (Taiwan) (Zh) (Unl).gbc` | 18 | `393bcd55d8fae175fa309d2a5fff66e61aa6e8462e6376dc0adddab89520d974` |

They now select NT-new automatically, without a ROM patch. These counts are
startup/input diagnostics, not exhaustive gameplay or proof of correct audio.

The local inventory contains **808 `(Unl)` files**, of which **526 are tagged
Aftermarket**. That label includes modern homebrew; it does not mean all 808
are knock-offs, broken games, or NT cartridges. The remaining header groups
include MBC1/3/5 and nonstandard types (`97`, `99`, `C2`, `FA`, `FF`, etc.). Some
are multicarts, protection-equipped boards, copier BIOSes or tools rather than
ordinary single-game cartridges. These groups still need separate investigation.

Run `node scripts/cartridge_inventory.mjs roms` to generate a read-only JSON
inventory with header types, hashes, Aftermarket/bad-dump labels and candidate
NT activation instruction sequences. A sequence may be unused data, and actual
games may construct it non-contiguously; neither a match nor its absence proves
mapper identity. Do not use it alone for automatic overrides. The report reads
only local ROMs, downloads nothing and is not included in the public library.
No commercial cartridges or original firmware are included in Git.

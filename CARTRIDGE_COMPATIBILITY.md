# Unlicensed cartridge compatibility

`(Unl)` means unlicensed, not a different console. A `.gbc` filename and a
Pokémon title do not imply an official Nintendo game, nor reliably identify
the cartridge's banking hardware. RustBoy selects GB/CGB/SGB from header flags,
not filenames. Unlicensed hardware is not yet comprehensively supported.

## Two different Diamond games

The local files checked during this investigation are distinct:

| Local filename | Header title | Size | Declared mapper | Result |
| --- | --- | --- | --- | --- |
| `Pokemon Diamond (Taiwan) (En) (Unl).gbc` | `POKEMONDIAMOND` | 512 KiB | MBC1, no RAM | White game screen; unresolved |
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

After 1,200 nominal frames with Start/A input, the 512 KiB file still displayed
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
that behavior. Whether this particular older-looking dump needs that mapper,
a different NT variant, or dump-specific handling remains unverified.
There is deliberately no filename-only mapper override or ROM patch.

Next compatibility work should identify the actual bank-register accesses,
validate the hardware/dump variant, then implement that mapper with synthetic
banking and save-state tests. Do not treat every unlicensed cartridge as NT-new.

## Reproduce with your own files

```sh
cargo run --locked --release --no-default-features --example cartridge_inspect -- 'path/to/game.gbc'
```

The read-only diagnostic reports CPU/banking state, VRAM/palettes, distinct
frame colors and recent instructions. Optional `RUSTBOY_BOOT_ROM` supplies
local firmware; `RUSTBOY_MAPPER=19` changes only the diagnostic's in-memory
header to MBC5. Neither option is a compatibility fix or modifies the file.
No commercial cartridges or original firmware are included in Git.

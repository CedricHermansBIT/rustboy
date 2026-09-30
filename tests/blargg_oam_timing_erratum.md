# Blargg standalone OAM timing ROM: diagnostic-buffer overflow

The original `oam_bug/rom_singles/7-timing_effect.gb` remains enabled and
failing. No fixture bytes, expected values, harness result rules, or release
exclusions have been changed for this finding.

## Evidence

The upstream [test source](https://github.com/retrio/gb-test-roms/blob/master/oam_bug/source/7-timing_effect.s)
loops through 116 timings. When OAM differs from its original contents, it
prints the timing number and a complete OAM dump before checking CRC
`0x7D792E7C`. Printing a changed OAM row is normal here: this test deliberately
causes the DMG OAM corruption bug, rather than printing only failure details.

Each observed dump block occupies 525 bytes:

- Two hex timing digits, a trailing space, and newline: 4 bytes.
- Ten OAM rows, each 51 characters plus newline: 520 bytes.
- One final blank-line newline: 1 byte.

The 17-byte title and 19 corrupted timings already require 9,992 bytes. The
19 mutable OAM rows are consistent with the hardware's first-row immunity
described by [Pan Docs](https://gbdev.io/pandocs/OAM_Corruption_Bug.html).

The upstream [ROM build configuration](https://github.com/retrio/gb-test-roms/blob/master/oam_bug/source/common/build_rom.s)
declares 8 KiB of cartridge RAM. The signature/status occupies `A000..A003`,
leaving `A004..BFFF` (8,188 bytes) for the diagnostic string, including its
zero terminator.

The upstream [logger](https://github.com/retrio/gb-test-roms/blob/master/oam_bug/source/common/shell.s)
increments a 16-bit `text_out_addr` without any boundary check. In an
instruction-level run, its first character write beyond cartridge RAM was
`LD (HL),A` at PC `C3FE`, with `HL=C000` and `A=20` (space), replacing the
previous byte `00`. The log had reached timing `11` (hex). Code executes from
WRAM in this ROM, so continuing these writes destroys its running program;
status `A000` remains `80` (running), and the final CRC check is not reached.
The logger also writes the terminator one byte ahead of the character.

The original aggregate `oam_bug.gb` uses the shell's documented custom
multi-ROM printing path and completes with:

```text
01:ok  02:ok  03:ok  04:ok  05:ok  06:ok  07:ok  08:ok

Passed
```

Its final status is `00`, observed after the running status `80`. Both
original binaries contain the same expected CRC setup bytes:
`01 86 82 11 83 D1 CD` (`LD BC,8286; LD DE,D183; CALL ...`), at file offsets
16,534 in the standalone ROM and 41,110 in the aggregate. These are the
complemented halves of `7D792E7C`, matching the upstream CRC macro. Thus the
aggregate's seventh test validates the same OAM timing checksum while the
standalone logger cannot safely contain the normal diagnostic output.

## Reproduce

Run the original standalone failure and aggregate separately:

```bash
cargo test --locked --release --offline --no-default-features \
  --test headless blargg_n7_timing_effect -- --exact --nocapture

cargo test --locked --release --offline --no-default-features \
  --test headless blargg_oam_bug -- --exact --nocapture
```

The first prints its full RAM log and fails with running status `80`; the
second prints all eight `ok` results and passes. Verify the matching CRC
instructions without modifying either ROM:

```bash
rg --text --no-unicode --byte-offset --only-matching \
  '\x01\x86\x82\x11\x83\xD1\xCD' \
  testroms/artifacts/blargg/oam_bug/rom_singles/7-timing_effect.gb \
  testroms/artifacts/blargg/oam_bug/oam_bug.gb
```

This is evidence of a standalone fixture's unsafe diagnostic logger, not
permission to count its unfinished execution as a pass. The original
standalone failure remains visible; the aggregate supplies separate, valid
coverage of the timing checksum.

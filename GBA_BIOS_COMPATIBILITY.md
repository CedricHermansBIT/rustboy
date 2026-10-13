# Replacement GBA BIOS compatibility

The replacement is an original functional implementation. It contains no Nintendo
firmware bytes, instruction disassembly, or copied emulator implementation.
Passing its own tests does not establish firmware cycle or rounding equivalence.

## Sound and music paths

The native music sequencer supports PCM instruments and all four hardware PSG
types: two pulse channels, programmable wave, and noise. Notes retain MIDI key,
velocity, gate time, priority, track linkage, and instrument envelope state in RAM.
EOT, FINE, player stop, fading, and channel clear also release PSG notes.

If the cartridge supplies the public `CgbChannel[4]` area at SoundArea+0x1c, the
sequencer fills those dedicated records. A supplied `CgbSound` hook at +0x28
executes on the guest CPU and owns register updates. The replacement's default
hook uses an original MMIO driver. When no CgbChannel area exists, that driver
shares free PCM channel records to avoid inventing a host-only sequencer state or
overwriting unrelated guest memory. Those records are excluded from PCM mixing.
Thus a native-only song can have fewer simultaneous PCM voices when PSG is active.

The fallback uses mathematically generated equal-temperament pitch, software ADSR
at the player's frame cadence, binary left/right routing and the hardware wave
volume levels. Noise pitch, envelope cadence, stereo balance and mixer rounding
are functional approximations, not exact stock-driver output. Existing cartridge
sound drivers continue to run as guest code.

The extended sequence parser accepts waveform pointers (XCMD 1, four bytes),
type (2), ADSR (4–7), pseudo-echo volume/length (8–9), PSG length (10) and sweep
(11). Unknown commands fault explicitly instead of being silently skipped with
an invented operand width.

Jump-list helpers 30–33 implement sample-frequency setup, track stop, fade update
and volume/pitch refresh. Volume/pitch refresh does not advance the LFO clock.
Slots 34–35 still fault: the available independent BIOS reference explicitly lists
their precise mapping and ABI as an open question. Cartridge-specific extensions
must execute their supplied guest functions instead.

## Reset, bias and mathematical limits

HardReset (SWI 26h) requests a machine restart, preserving cartridge battery data.
The replacement cold launch omits the retail intro; it is not a SoftReset to the
RAM entry flag.

MultiBoot (SWI 25h) runs independently authored ARM firmware on the guest CPU.
The host routine handles the main upload, encryption and CRC exchange in normal
32-bit mode (256 kHz or 2 MHz) and multiplayer 16-bit mode (115.2 kbit/s). The
cartridge still supplies the documented discovery and header handshake before
calling the service. The cable currently supports one parent and one child.
Malformed parameters return failure before starting serial activity: the
supported source range must be aligned, contiguous within canonical EWRAM,
IWRAM or one cartridge ROM window, and contain 0x100–0x3ff40 bytes in multiples
of 16. Cartridge ROM mirrors remain valid upload sources.

The cartridge-free download receiver supports multiplayer 16-bit mode. It
decrypts into EWRAM, verifies the final CRC and executes the downloaded entry at
0x020000c0. Before handoff it supplies the extended-header boot mode (3) and
child ID (1) at offsets 0xc4/0xc5. A normal-mode or JoyBus download receiver is
not implemented. The receiver does not reproduce the retail BIOS download
animation or validate the retail logo/header checksum. Transfers use ordinary
serial registers and emulated clocks, so upload progress remains subject to the
same interrupts, network barriers and snapshot behavior as cartridge code.

Original ARM and Thumb native fixtures verify upload, CRC agreement, execution
of the RAM payload, preserved caller LR/NZCV and snapshot restoration during an
upload. Parameter boundary and rejection tests cover all three host modes and
assert that malformed requests leave serial hardware untouched. This is
protocol coverage, not proof that every commercial Single-Pak game works; real
gameplay requires separate compatibility regressions.

SoundBias preserves PWM resolution bits, leaves a bias already above the upward
target alone, and supplies the independently observed r1/r3 caller results.
It models elapsed ramp cost approximately; intermediate analog ramp samples and
firmware access timing are not reproduced.

Division models the independently measured small zero-denominator cases: 0/0
and ±1/0 return, while larger magnitudes retain an explicit hang fault. ArcTan
uses the documented wrapping fixed-point Horner polynomial, including asymmetric
negative rounding and overflow beyond its fitted range; ArcTan2 uses the measured
octant dispatch and register side effects. The independent mGBA BIOS math suite
passes all 615 assertions, without excluding registers or replacing its goldens.
Affine sine quantization, MIDI-frequency interpolation and BIOS service timing
still need further hardware comparisons before claiming exact firmware behavior.

CpuSet preserves logical source/destination lanes on the eight-bit save bus.
Odd halfword sources rotate as ARM LDRH before writing the low halfword; word
copies align on ordinary RAM without losing save-bus byte selection. Independent
mGBA memory now passes 1542/1552 assertions; the remaining ten are protected BIOS
reads that expect Nintendo's firmware opcode identity instead of our replacement.

## Independent references and regression coverage

- [GBATEK sound and reset register/service definitions](https://rust-console.github.io/gbatek-gbaonly/)
- [GBATEK MultiBoot protocol, parameter layout and extended header](https://rust-console.github.io/gbatek-gbaonly/#biosmultibootsinglegamepak)
- [VGMDocs MP2k sequence-format reference](https://loveemu.github.io/vgmdocs/Summary_of_GBA_Standard_Sound_Driver_MusicPlayer2000.html)
- [Public MP2k data structures and function declarations](https://github.com/pret/pokeemerald/blob/master/include/gba/m4a_internal.h)
- [SoundBias hardware-observed behavior](https://github.com/retrointernals/gba-bios-reference/blob/main/docs/swi_19_soundbias.md)
- [Jump-list reference and unresolved ABI questions](https://github.com/retrointernals/gba-bios-reference/blob/main/docs/swi_2a_soundgetjumplist.md)
- [HardReset hardware-observed behavior](https://github.com/retrointernals/gba-bios-reference/blob/main/docs/swi_26_hardreset.md)
- [Division hardware edge cases](https://github.com/retrointernals/gba-bios-reference/blob/main/docs/swi_06_div.md)
- [ArcTan polynomial and hardware rounding](https://github.com/retrointernals/gba-bios-reference/blob/main/docs/swi_09_arctan.md)
- [ArcTan2 hardware octant dispatch](https://github.com/retrointernals/gba-bios-reference/blob/main/docs/swi_0A_arctan2.md)

Original regression tests verify that all four PSG types produce finite audible
output and stop at their gates, lower-priority notes cannot steal dedicated PSG
records, supplied hooks keep register ownership, extended commands consume the
right number of stream bytes, helper calls preserve LFO timing, bias preserves
PWM settings and registers, and 120 frames of song/audio replay match after a
snapshot restore. These are behavioral regressions, not a hardware audio oracle.

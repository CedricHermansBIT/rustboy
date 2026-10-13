//! Original functional BIOS PCM driver, using the documented MP2k RAM ABI.
//! References: GBATEK BIOS Sound Functions; retrointernals BIOS behavioral
//! reference; VGMDocs MP2k format and m4a_internal.h's data layout. No firmware
//! bytes, disassembly, or another emulator's implementation are included.
//! Timing and mixer rounding are approximations, not firmware equivalence.
use super::Effect;
use crate::{
    bus::Bus,
    cpu::{Cpu, Memory},
    FRAME_CYCLES,
};
#[path = "bios_music.rs"]
mod music;

pub(crate) const MAIN: u32 = 0x600;
const MIX: u32 = MAIN + 0x28;
const RETURN: u32 = MAIN + 0x2c;
const STUB: u32 = 0x680;
const PLAYER: u32 = 0xb00;
const NOTE: u32 = 0x6a0;
const JUMPS: u32 = 0x800;
const TABLE: u32 = 0xa00;
const IDENT: u32 = 0x68736d53;
const GLOBAL: u32 = 0x03007ff0;
const BUFFER: u32 = 0x350;
const CAPACITY: u32 = 1584;
const AREA_BYTES: u32 = BUFFER + CAPACITY * 2;
// Counts are the sound format's frame-aligned rates, not copied BIOS bytes.
const SAMPLES: [u32; 12] = [96, 132, 176, 224, 264, 304, 352, 448, 528, 608, 672, 704];

pub(crate) fn firmware(bytes: &mut [u8]) {
    // Original ARM thunk: cartridge sequencer(r0=player) and PSG hook execute
    // normally, including nested SWIs/IRQs. The context lives on the guest stack.
    let code = [
        0xe5943020u32, // LDR r3,[r4,#20h] ; sequencer function
        0xe5940024,    // LDR r0,[r4,#24h] ; player
        0xe3530000,    // CMP r3,#0
        0x128fe000,    // ADDNE lr,pc,#0
        0x112fff13,    // BXNE r3
        0xe5943028,    // LDR r3,[r4,#28h] ; PSG update hook
        0xe3530000,
        0x128fe000,
        0x112fff13,
        0xe1a00004, // MOV r0,r4 ; area
        0xef800000, // private HLE mix entry
        0xef810000, // private context restore
    ];
    for (i, op) in code.into_iter().enumerate() {
        bytes[MAIN as usize + i * 4..MAIN as usize + i * 4 + 4].copy_from_slice(&op.to_le_bytes());
    }
    // Callable Thumb entry points retain LR even when a nested SWI changes it.
    for (address, number) in [(STUB, 0xa7), (NOTE, 0xa8)]
        .into_iter()
        .chain((0..36).map(|i| (JUMPS + i * 8, 0x82 + i as u8)))
    {
        for (i, op) in [0xb500u16, 0xdf00 | u16::from(number), 0xbc08, 0x4718]
            .into_iter()
            .enumerate()
        {
            let at = address as usize + i * 2;
            bytes[at..at + 2].copy_from_slice(&op.to_le_bytes());
        }
    }
    // Thumb player hook chains through the guest ABI, so a cartridge may
    // mix its own sequencer with BIOS players. Nested calls remain sliceable.
    for (i, op) in [
        0xb510u16, // PUSH {r4,lr}
        0x1c04,    // MOV r4,r0
        0xdfa6,    // update this player
        0x6ba3,    // LDR r3,[r4,#38h] ; next sequencer
        0x6be0,    // LDR r0,[r4,#3ch] ; next player
        0x2b00,    // CMP r3,#0
        0xd004,    // BEQ restore
        0xa202,    // ADR r2,restore
        0x3201,    // ADD r2,#1 ; Thumb return
        0x4696,    // MOV lr,r2
        0x4718,    // BX r3
        0x46c0,    // NOP/alignment
        0xbc10,    // POP {r4}
        0xbc08,    // POP {r3}
        0x4718,    // BX r3
    ]
    .into_iter()
    .enumerate()
    {
        let at = PLAYER as usize + i * 2;
        bytes[at..at + 2].copy_from_slice(&op.to_le_bytes());
    }
    for i in 0..36 {
        let at = TABLE as usize + i * 4;
        bytes[at..at + 4].copy_from_slice(&(JUMPS + i as u32 * 8 + 1).to_le_bytes());
    }
}

/// Private SWIs are accepted only at our own immutable replacement entry points.
pub(crate) fn private(
    number: u8,
    address: u32,
    cpu: &mut Cpu,
    bus: &mut Bus,
) -> Option<Result<Effect, &'static str>> {
    match (number, address) {
        (0x80, MIX) => Some(mix(bus, cpu.r[0]).map(|()| Effect::Return)),
        (0x81, RETURN) => Some(Ok(Effect::SoundReturn)),
        (0xa6, a) if a == PLAYER + 4 => {
            Some(music::players(bus, cpu.r[0]).map(|()| Effect::Return))
        }
        (0xa7, a) if a == STUB + 2 => Some(Ok(Effect::Return)),
        (0xa8, a) if a == NOTE + 2 => {
            Some(music::note(bus, cpu.r[0] as u8, cpu.r[1], cpu.r[2]).map(|()| Effect::Return))
        }
        (0x82..=0xa5, a) if a == JUMPS + u32::from(number - 0x82) * 8 + 2 => {
            Some(music::jump(bus, number - 0x82, cpu).map(|()| Effect::Return))
        }
        _ => None,
    }
}
fn ram(bus: &Bus, address: u32, size: u32) -> bool {
    let (offset, length) = match address >> 24 {
        2 => (address & 0x3ffff, bus.ewram.len() as u32),
        3 => (address & 0x7fff, bus.iwram.len() as u32),
        _ => return false,
    };
    address & 3 == 0 && offset.checked_add(size).is_some_and(|end| end <= length)
}
pub(crate) fn stack_valid(bus: &Bus, address: u32) -> bool {
    ram(bus, address, 68)
}
fn area(bus: &Bus) -> Option<u32> {
    let a = bus.peek32(GLOBAL);
    (ram(bus, a, AREA_BYTES) && bus.peek32(a) == IDENT).then_some(a)
}
fn zero(bus: &mut Bus, address: u32, count: u32) {
    for i in (0..count).step_by(4) {
        bus.write32(address + i, 0, true);
    }
}
fn dma_off(bus: &mut Bus) {
    for a in [0x040000c6, 0x040000d2] {
        bus.write16(a, 0, false);
    }
}
fn dma_on(bus: &mut Bus) {
    for a in [0x040000c6, 0x040000d2] {
        bus.write16(a, 0xb600, false);
    }
}
fn vsync_off(bus: &mut Bus, a: u32) {
    dma_off(bus);
    bus.write8(a + 4, 0);
    zero(bus, a + BUFFER, CAPACITY * 2);
}
fn frequency(bus: &mut Bus, a: u32, index: u8) -> Result<(), &'static str> {
    let samples = *SAMPLES
        .get(usize::from(index.wrapping_sub(1)))
        .ok_or("BIOS sound sample-rate index out of range")?;
    vsync_off(bus, a);
    let rate = ((u64::from(samples) * 16777216 + FRAME_CYCLES / 2) / FRAME_CYCLES) as u32;
    let period = CAPACITY / samples;
    bus.write8(a + 8, index);
    bus.write8(a + 0xb, period as u8);
    bus.write32(a + 0x10, samples, false);
    bus.write32(a + 0x14, rate, false);
    bus.write32(a + 0x18, ((1u64 << 32) / u64::from(rate)) as u32, false);
    bus.write16(0x04000102, 0, false);
    let clocks = (FRAME_CYCLES / u64::from(samples)) as u16;
    bus.write16(0x04000100, 0u16.wrapping_sub(clocks), false);
    bus.write16(0x04000102, 0x80, false);
    // Re-arm from the buffer bases, rather than an earlier mode's read pointer.
    bus.write32(0x040000bc, a + BUFFER, false);
    bus.write32(0x040000c8, a + BUFFER + CAPACITY, false);
    dma_on(bus);
    Ok(())
}

fn mode(bus: &mut Bus, a: u32, value: u32) -> Result<(), &'static str> {
    if value & 0xff != 0 {
        bus.write8(a + 5, (value & 0x7f) as u8);
    }
    let channels = ((value >> 8) & 15) as u8;
    if channels != 0 {
        bus.write8(a + 6, channels.min(12));
        for i in 0..12 {
            let c = a + 0x50 + i * 0x40;
            if bus.peek8(c) != 0 && bus.peek8(c + 1) & 7 != 0 {
                music::stop_psg(bus, bus.peek8(c + 1) & 7);
            }
            bus.write8(c, 0);
        }
    }
    let volume = ((value >> 12) & 15) as u8;
    if volume != 0 {
        bus.write8(a + 7, volume);
    }
    if value & 0xb00000 != 0 {
        let bias = (bus.io16(0x88) & 0x3fff) | ((((value >> 20) & 3) as u16) << 14);
        bus.write16(0x04000088, bias, false);
    }
    let rate = ((value >> 16) & 15) as u8;
    if rate != 0 {
        frequency(bus, a, rate)?;
    }
    Ok(())
}
pub(crate) fn service(number: u8, cpu: &mut Cpu, bus: &mut Bus) -> Result<Effect, &'static str> {
    match number {
        0x1a => {
            let a = cpu.r[0] & !3;
            if !ram(bus, a, AREA_BYTES) {
                return Err("BIOS sound work area must fit in RAM");
            }
            dma_off(bus);
            zero(bus, a, AREA_BYTES);
            bus.write32(GLOBAL, a, false);
            bus.write16(0x04000084, 0x80, false);
            bus.write16(0x04000082, 0xa90e, false);
            bus.write16(0x04000088, (bus.io16(0x88) & 0x3fff) | 0x4000, false);
            bus.write32(0x040000c0, 0x040000a0, false);
            bus.write32(0x040000cc, 0x040000a4, false);
            bus.write8(a + 6, 8);
            bus.write8(a + 7, 15);
            for offset in [0x28, 0x2c, 0x30, 0x3c] {
                bus.write32(a + offset, STUB | 1, false);
            }
            bus.write32(a + 0x34, TABLE, false);
            bus.write32(a + 0x38, NOTE | 1, false);
            frequency(bus, a, 4)?;
            bus.write32(a, IDENT, false);
            cpu.r[0] = IDENT;
        }
        0x1b => {
            if let Some(a) = area(bus) {
                mode(bus, a, cpu.r[0])?;
            }
        }
        0x1c => {
            if let Some(a) = area(bus) {
                bus.write32(a, IDENT + 1, false);
                return Ok(Effect::SoundMain(a));
            }
        }
        0x1d => {
            if let Some(a) = sound_area_for_vsync(bus) {
                let counter = bus.peek8(a + 4);
                if counter <= 1 {
                    bus.write8(a + 4, bus.peek8(a + 0xb));
                    dma_off(bus);
                    dma_on(bus);
                } else {
                    bus.write8(a + 4, counter - 1);
                }
            }
        }
        0x1e => {
            if let Some(a) = area(bus) {
                clear(bus, a);
            }
        }
        0x28 => {
            if let Some(a) = area(bus) {
                vsync_off(bus, a);
            }
        }
        0x29 => dma_on(bus),
        0x2a => {
            for i in 0..36 {
                bus.write32(cpu.r[0].wrapping_add(i * 4), JUMPS + i * 8 + 1, true);
            }
            cpu.r[0] = cpu.r[0].wrapping_add(144);
        }
        0x20..=0x24 => music::service(number, cpu, bus)?,
        _ => return Err("BIOS sound service not implemented"),
    }
    Ok(Effect::Return)
}
fn sound_area_for_vsync(bus: &Bus) -> Option<u32> {
    let a = bus.peek32(GLOBAL);
    // VSync may interrupt the mixer while its identity holds the busy lock.
    (ram(bus, a, AREA_BYTES) && bus.peek32(a).wrapping_sub(IDENT) <= 1).then_some(a)
}
fn clear(bus: &mut Bus, a: u32) {
    for i in 0..12 {
        let c = a + 0x50 + i * 0x40;
        if bus.peek8(c) != 0 && bus.peek8(c + 1) & 7 != 0 {
            music::stop_psg(bus, bus.peek8(c + 1) & 7);
        }
        bus.write8(a + 0x50 + i * 0x40, 0);
    }
    let psg = bus.peek32(a + 0x1c);
    if psg != 0 && ram(bus, psg, 0x100) {
        for i in 0..4 {
            bus.write8(psg + i * 0x40, 0);
        }
        for (envelope, trigger) in [
            (0x04000063, 0x04000065),
            (0x04000069, 0x0400006d),
            (0x04000073, 0x04000075),
            (0x04000079, 0x0400007d),
        ] {
            bus.write8(envelope, 0);
            bus.write8(trigger, 0x80);
        }
        bus.write8(0x04000070, 0);
    }
}

fn envelope(bus: &mut Bus, c: u32, master: u8) -> Option<(i32, i32)> {
    let mut status = bus.peek8(c);
    if status == 0 {
        return None;
    }
    let mut level = bus.peek8(c + 9);
    if status & 0x80 != 0 {
        if status & 0x40 != 0 {
            bus.write8(c, 0);
            return None;
        }
        let wave = bus.peek32(c + 0x24);
        let size = bus.peek32(wave.wrapping_add(12));
        if size == 0 {
            bus.write8(c, 0);
            return None;
        }
        bus.write32(c + 0x28, wave.wrapping_add(16), false);
        bus.write32(c + 0x18, size, false);
        bus.write32(c + 0x1c, 0, false);
        status = 3 | if bus.peek16(wave.wrapping_add(2)) & 0x4000 != 0 {
            0x10
        } else {
            0
        };
        level = 0;
    }
    if status & 4 != 0 {
        let length = bus.peek8(c + 13);
        if length <= 1 {
            bus.write8(c, 0);
            return None;
        }
        bus.write8(c + 13, length - 1);
    } else if status & 0x40 != 0 {
        level = ((u16::from(level) * u16::from(bus.peek8(c + 7))) >> 8) as u8;
        if level <= bus.peek8(c + 12) {
            level = bus.peek8(c + 12);
            if level == 0 || bus.peek8(c + 13) == 0 {
                bus.write8(c, 0);
                return None;
            }
            status = (status & !3) | 4;
        }
    } else {
        match status & 3 {
            3 => {
                let attack = u16::from(level) + u16::from(bus.peek8(c + 4));
                level = attack.min(255) as u8;
                if attack >= 255 {
                    status = (status & !3) | 2;
                }
            }
            2 => {
                level = ((u16::from(level) * u16::from(bus.peek8(c + 5))) >> 8) as u8;
                let sustain = bus.peek8(c + 6);
                if level <= sustain {
                    level = sustain;
                    status = (status & !3) | 1;
                }
            }
            _ => {}
        }
    }
    if level == 0 && status & 3 != 3 {
        bus.write8(c, 0);
        return None;
    }
    bus.write8(c, status);
    bus.write8(c + 9, level);
    let gain = (u32::from(level) * (u32::from(master) + 1)) >> 4;
    let right = (gain * u32::from(bus.peek8(c + 2))) >> 8;
    let left = (gain * u32::from(bus.peek8(c + 3))) >> 8;
    bus.write8(c + 10, right as u8);
    bus.write8(c + 11, left as u8);
    Some((right as i32, left as i32))
}
fn mix(bus: &mut Bus, a: u32) -> Result<(), &'static str> {
    if !ram(bus, a, AREA_BYTES) || bus.peek32(a) != IDENT + 1 {
        return Ok(());
    }
    let n = bus.peek32(a + 0x10);
    let period = u32::from(bus.peek8(a + 0xb));
    let counter = u32::from(bus.peek8(a + 4));
    if n == 0 || n > 704 || period == 0 || period * n > CAPACITY || counter > period {
        bus.write32(a, IDENT, false);
        return Err("BIOS sound buffer geometry is invalid");
    }
    let slot = if counter <= 1 {
        0
    } else {
        period - counter + 1
    };
    let offset = slot * n;
    let mut output = vec![[0i32; 2]; n as usize];
    let reverb = i32::from(bus.peek8(a + 5));
    if reverb != 0 {
        let span = n * period;
        for (i, out) in output.iter_mut().enumerate() {
            let at = offset + i as u32;
            let next = (at + n) % span;
            let sum = [at, next]
                .into_iter()
                .map(|p| {
                    i32::from(bus.peek8(a + BUFFER + p) as i8)
                        + i32::from(bus.peek8(a + BUFFER + CAPACITY + p) as i8)
                })
                .sum::<i32>();
            *out = [(sum * reverb) >> 9; 2];
        }
    }
    let rate = bus.peek32(a + 0x14).max(1);
    let master = bus.peek8(a + 7);
    let channels = bus.peek8(a + 6).min(12);
    music::native_psg(bus, a);
    let mut active = 0u32;
    for i in 0..channels {
        let c = a + 0x50 + u32::from(i) * 0x40;
        if bus.peek8(c + 1) & 7 != 0 {
            continue;
        }
        let Some((right, left)) = envelope(bus, c, master) else {
            continue;
        };
        active += 1;
        let wave = bus.peek32(c + 0x24);
        let size = bus.peek32(wave.wrapping_add(12));
        let loop_at = bus.peek32(wave.wrapping_add(8));
        let looped = bus.peek8(c) & 0x10 != 0 && loop_at < size;
        let mut pos = bus.peek32(c + 0x28);
        let mut remaining = bus.peek32(c + 0x18);
        let mut fraction = u64::from(bus.peek32(c + 0x1c) & 0x7fffff);
        // MidiKey2Freq produces a frequency in Hz; phase uses 23 fraction bits.
        let step = if bus.peek8(c + 1) & 8 != 0 {
            1u64 << 23
        } else {
            (u64::from(bus.peek32(c + 0x20)) << 23) / u64::from(rate)
        };
        for out in &mut output {
            if remaining == 0 {
                bus.write8(c, 0);
                break;
            }
            let sample = i32::from(bus.peek8(pos) as i8);
            let next = if remaining > 1 {
                i32::from(bus.peek8(pos.wrapping_add(1)) as i8)
            } else if looped {
                i32::from(bus.peek8(wave.wrapping_add(16 + loop_at)) as i8)
            } else {
                0
            };
            let value = sample + (((i64::from(next - sample) * fraction as i64) >> 23) as i32);
            out[0] += (value * right) >> 8;
            out[1] += (value * left) >> 8;
            fraction += step;
            let advance = (fraction >> 23) as u32;
            fraction &= 0x7fffff;
            if advance < remaining {
                pos = pos.wrapping_add(advance);
                remaining -= advance;
            } else if looped {
                let loop_size = size - loop_at;
                let extra = (advance - remaining) % loop_size;
                pos = wave.wrapping_add(16 + loop_at + extra);
                remaining = loop_size - extra;
            } else {
                remaining = 0;
                bus.write8(c, 0);
            }
        }
        bus.write32(c + 0x28, pos, false);
        bus.write32(c + 0x18, remaining, false);
        bus.write32(c + 0x1c, fraction as u32, false);
    }
    for (i, out) in output.into_iter().enumerate() {
        // The firmware's signed-byte mix buffer wraps; APU PWM clips later.
        bus.write8(a + BUFFER + offset + i as u32, out[0] as u8);
        bus.write8(a + BUFFER + CAPACITY + offset + i as u32, out[1] as u8);
    }
    bus.write32(a, IDENT, false);
    bus.internal(1000 + n * active * 16);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    const A: u32 = 0x02001000;
    const W: u32 = 0x02010000;
    fn setup() -> (Cpu, Bus) {
        let mut b = Bus::new(vec![0; 192], None).unwrap();
        let mut c = Cpu::default();
        c.r[0] = A;
        service(0x1a, &mut c, &mut b).unwrap();
        b.take_cycles();
        (c, b)
    }
    fn voice(b: &mut Bus, samples: &[i8], loop_at: Option<u32>, pitched: bool) -> u32 {
        b.write16(W + 2, if loop_at.is_some() { 0x4000 } else { 0 }, false);
        b.write32(W + 8, loop_at.unwrap_or(0), false);
        b.write32(W + 12, samples.len() as u32, false);
        for (i, s) in samples.iter().enumerate() {
            b.write8(W + 16 + i as u32, *s as u8);
        }
        let c = A + 0x50;
        b.write8(c, 0x80);
        b.write8(c + 1, if pitched { 0 } else { 8 });
        for (offset, value) in [(2, 255), (3, 128), (4, 255), (5, 128), (6, 64), (7, 128)] {
            b.write8(c + offset, value);
        }
        b.write32(c + 0x24, W, false);
        b.write32(c + 0x20, b.peek32(A + 0x14) / 2, false);
        c
    }
    fn render(b: &mut Bus) {
        b.write32(A, IDENT + 1, false);
        mix(b, A).unwrap();
    }
    #[test]
    fn initialization_rates_partial_mode_and_ram_bounds() {
        let (mut cpu, mut b) = setup();
        assert_eq!(b.peek32(GLOBAL), A);
        assert_eq!(b.peek32(A), IDENT);
        assert_eq!(b.peek8(A + 6), 8);
        assert_eq!(b.peek8(A + 7), 15);
        assert_eq!(b.peek32(A + 0x10), 224);
        assert_eq!(b.peek8(A + 0xb), 7);
        assert_eq!(b.io16(0x82), 0x210e);
        assert_eq!(b.dma[1].source, A + BUFFER);
        assert_eq!(b.dma[2].source, A + BUFFER + CAPACITY);
        assert_eq!(b.io16(0xc6), 0xb600);
        assert_eq!(b.io16(0xd2), 0xb600);
        for (i, n) in SAMPLES.into_iter().enumerate() {
            cpu.r[0] = ((i + 1) as u32) << 16;
            service(0x1b, &mut cpu, &mut b).unwrap();
            assert_eq!(b.peek32(A + 0x10), n);
            assert!(u32::from(b.peek8(A + 0xb)) * n <= CAPACITY);
            assert_eq!(
                u16::MAX - b.io16(0x100) + 1,
                (FRAME_CYCLES / u64::from(n)) as u16
            );
            assert_eq!(b.io16(0x102), 0x80);
        }
        cpu.r[0] = 0x80 | 0xf00 | 0x7000 | 0x800000;
        service(0x1b, &mut cpu, &mut b).unwrap();
        assert_eq!(b.peek8(A + 5), 0);
        assert_eq!(b.peek8(A + 6), 12);
        assert_eq!(b.peek8(A + 7), 7);
        assert_eq!(b.io16(0x88) & 0xc000, 0);
        cpu.r[0] = 0;
        service(0x1b, &mut cpu, &mut b).unwrap();
        assert_eq!(b.peek8(A + 7), 7);
        cpu.r[0] = 0x03007ffc;
        assert!(service(0x1a, &mut cpu, &mut b).is_err());
    }
    #[test]
    fn fixed_pcm_stereo_loop_one_shot_envelope_and_clear() {
        let (mut cpu, mut b) = setup();
        let c = voice(&mut b, &[64, -64, 32, -32], Some(1), false);
        render(&mut b);
        assert_eq!(
            (0..7)
                .map(|i| b.peek8(A + BUFFER + i) as i8)
                .collect::<Vec<_>>(),
            [63, -64, 31, -32, -64, 31, -32]
        );
        assert_eq!(b.peek8(A + BUFFER + CAPACITY) as i8, 31);
        assert_ne!(b.peek8(c), 0);
        assert_eq!(b.peek32(A), IDENT);
        b.write8(c, b.peek8(c) | 0x40);
        render(&mut b);
        assert!(b.peek8(c + 9) < 255, "release envelope decays");
        service(0x1e, &mut cpu, &mut b).unwrap();
        render(&mut b);
        assert_eq!(b.peek8(c), 0);
        assert_eq!(b.peek8(A + BUFFER), 0);
        voice(&mut b, &[64, -64], None, false);
        render(&mut b);
        assert_eq!(b.peek8(c), 0);
        assert_eq!(b.peek8(A + BUFFER + 2), 0);
    }
    #[test]
    fn pitched_interpolation_ring_countdown_and_vsync_pause() {
        let (mut cpu, mut b) = setup();
        let c = voice(&mut b, &[0, 64], Some(0), true);
        // Exact half-rate source stepping, independent of the nominal rate's oddness.
        b.write32(A + 0x14, 1000, false);
        b.write32(c + 0x20, 500, false);
        render(&mut b);
        assert_eq!(
            (0..5)
                .map(|i| b.peek8(A + BUFFER + i) as i8)
                .collect::<Vec<_>>(),
            [0, 31, 63, 31, 0]
        );
        b.write8(A + 4, 7);
        render(&mut b);
        assert_ne!(b.peek8(A + BUFFER + 224 + 1), 0);
        b.write32(A, IDENT + 1, false); // VBlank can interrupt a busy mixer.
        for expected in [6, 5, 4, 3, 2, 1, 7] {
            service(0x1d, &mut cpu, &mut b).unwrap();
            assert_eq!(b.peek8(A + 4), expected);
        }
        b.write32(A, IDENT, false);
        service(0x28, &mut cpu, &mut b).unwrap();
        assert_eq!(b.io16(0xc6), 0);
        assert_eq!(b.io16(0xd2), 0);
        assert!((0..CAPACITY * 2).all(|i| b.peek8(A + BUFFER + i) == 0));
        service(0x29, &mut cpu, &mut b).unwrap();
        assert_eq!(b.io16(0xc6), 0xb600);
    }
    #[test]
    fn malformed_sound_geometry_is_bounded_and_restores_busy_lock() {
        let (_, mut b) = setup();
        b.write32(A + 0x10, u32::MAX, false);
        b.write32(A, IDENT + 1, false);
        assert!(mix(&mut b, A).is_err());
        assert_eq!(b.peek32(A), IDENT);
        assert!(private(0x80, 0x08000000, &mut Cpu::default(), &mut b).is_none());
    }
}

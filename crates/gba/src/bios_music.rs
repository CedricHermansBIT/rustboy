//! Original MP2k sequence parser for BIOS music-player services. State uses the
//! public player/track/channel RAM ABI; commands follow VGMDocs' format reference.
//! PCM instruments are supported. Cartridge extensions and PSG synthesis hooks
//! execute on the guest CPU; undocumented jump-list helper slots fail explicitly.
use super::{area, mode, ram, Bus, Cpu, Memory, IDENT, PLAYER};
const PAUSED: u32 = 0x80000000;

fn valid(bus: &Bus, p: u32) -> bool {
    ram(bus, p, 0x40) && bus.peek32(p + 0x34) == IDENT
}
fn tracks(bus: &Bus, p: u32) -> Result<(u32, u8), &'static str> {
    let t = bus.peek32(p + 0x2c);
    let count = bus.peek8(p + 8).min(16);
    if !ram(bus, t, u32::from(count) * 0x50) {
        return Err("BIOS music tracks must fit in RAM");
    }
    Ok((t, count))
}
fn release(bus: &mut Bus, t: u32, key: Option<u8>) {
    let a = bus.peek32(super::GLOBAL);
    if !ram(bus, a, super::AREA_BYTES) {
        return;
    }
    for i in 0..12 {
        let c = a + 0x50 + i * 0x40;
        if bus.peek32(c + 0x2c) == t && key.is_none_or(|k| bus.peek8(c + 0x11) == k) {
            let s = bus.peek8(c);
            if s != 0 {
                bus.write8(c, s | 0x40);
            }
        }
    }
}
fn init_track(bus: &mut Bus, t: u32) {
    release(bus, t, None);
    super::zero(bus, t, 0x50);
    bus.write8(t + 0xf, 2); // default bend range
    bus.write8(t + 0x13, 64); // external/fade volume
    bus.write8(t + 0x19, 22); // default LFO speed
}
pub(super) fn service(n: u8, cpu: &mut Cpu, bus: &mut Bus) -> Result<(), &'static str> {
    let p = cpu.r[0];
    if n == 0x20 {
        let Some(a) = area(bus) else {
            return Ok(());
        };
        let t = cpu.r[1];
        let count = cpu.r[2].min(16) as u8;
        if !ram(bus, p, 0x40) || !ram(bus, t, u32::from(count) * 0x50) {
            return Err("BIOS music player and tracks must fit in RAM");
        }
        // Preserve an existing player's list links on reopen; don't form a cycle.
        let reopening = valid(bus, p);
        let old_next = [bus.peek32(p + 0x38), bus.peek32(p + 0x3c)];
        super::zero(bus, p, 0x40);
        bus.write8(p + 8, count);
        bus.write32(p + 0x2c, t, false);
        bus.write32(p + 4, PAUSED, false);
        for i in 0..count {
            init_track(bus, t + u32::from(i) * 0x50);
        }
        if reopening {
            bus.write32(p + 0x38, old_next[0], false);
            bus.write32(p + 0x3c, old_next[1], false);
        } else {
            bus.write32(p + 0x38, bus.peek32(a + 0x20), false);
            bus.write32(p + 0x3c, bus.peek32(a + 0x24), false);
            bus.write32(a + 0x20, PLAYER | 1, false);
            bus.write32(a + 0x24, p, false);
        }
        bus.write32(p + 0x34, IDENT, false);
        return Ok(());
    }
    if !valid(bus, p) {
        return Ok(());
    }
    let (t, count) = tracks(bus, p)?;
    match n {
        0x21 => {
            let song = cpu.r[1];
            let used = bus.peek8(song).min(count);
            bus.write32(p, song, false);
            bus.write32(p + 4, 0, false);
            bus.write8(p + 9, bus.peek8(song.wrapping_add(2)));
            bus.write32(p + 0xc, 0, false);
            bus.write32(p + 0x30, bus.peek32(song.wrapping_add(4)), false);
            bus.write16(p + 0x1c, 150, false);
            bus.write16(p + 0x1e, 256, false);
            bus.write16(p + 0x20, 150, false);
            bus.write16(p + 0x22, 0, false);
            bus.write16(p + 0x24, 0, false);
            bus.write16(p + 0x26, 0, false);
            bus.write16(p + 0x28, 256, false);
            for i in 0..count {
                let at = t + u32::from(i) * 0x50;
                init_track(bus, at);
                if i < used {
                    bus.write8(at, 0x80);
                    bus.write32(
                        at + 0x40,
                        bus.peek32(song.wrapping_add(8 + u32::from(i) * 4)),
                        false,
                    );
                }
            }
            let reverb = bus.peek8(song.wrapping_add(3));
            if reverb & 0x80 != 0 {
                if let Some(a) = area(bus) {
                    mode(bus, a, u32::from(reverb))?;
                }
            }
        }
        0x22 => {
            bus.write32(p + 4, bus.peek32(p + 4) | PAUSED, false);
            for i in 0..count {
                release(bus, t + u32::from(i) * 0x50, None);
            }
        }
        0x23 => bus.write32(p + 4, bus.peek32(p + 4) & !PAUSED, false),
        0x24 => {
            bus.write16(p + 0x24, cpu.r[1] as u16, false);
            bus.write16(p + 0x26, cpu.r[1] as u16, false);
            bus.write16(p + 0x28, 256, false);
        }
        _ => unreachable!(),
    }
    Ok(())
}
// Define the format's note lengths arithmetically rather than embedding a
// firmware table. The higher range alternates whole/dotted subdivisions.
fn length(index: u8) -> u8 {
    match index {
        0..=24 => index,
        25..=48 => {
            let i = index - 25;
            let group = i / 4;
            let base = 28 + group * 12;
            base + [0, 2, 4, 8][usize::from(i % 4)]
        }
        _ => 0,
    }
}
fn byte(bus: &Bus, t: u32) -> u8 {
    bus.peek8(bus.peek32(t + 0x40))
}
fn take(bus: &mut Bus, t: u32) -> u8 {
    let at = bus.peek32(t + 0x40);
    let b = bus.peek8(at);
    bus.write32(t + 0x40, at.wrapping_add(1), false);
    b
}
fn word(bus: &mut Bus, t: u32) -> u32 {
    // Stream pointers need not be word-aligned; decode bytes without ARM LDR rotation.
    u32::from_le_bytes(std::array::from_fn(|_| take(bus, t)))
}
fn argument(bus: &mut Bus, t: u32, offset: u32) {
    let v = take(bus, t);
    bus.write8(t + offset, v);
}
pub(super) fn players(bus: &mut Bus, p: u32) -> Result<(), &'static str> {
    if valid(bus, p) {
        player(bus, p)
    } else {
        Ok(())
    }
}
fn player(bus: &mut Bus, p: u32) -> Result<(), &'static str> {
    if bus.peek32(p + 4) & PAUSED != 0 {
        return Ok(());
    }
    let (tracks, count) = tracks(bus, p)?;
    let interval = bus.peek16(p + 0x24);
    if interval != 0 {
        let countdown = bus.peek16(p + 0x26).saturating_sub(1);
        bus.write16(p + 0x26, countdown, false);
        if countdown == 0 {
            bus.write16(p + 0x26, interval, false);
            let fade = bus.peek16(p + 0x28).saturating_sub(16);
            bus.write16(p + 0x28, fade, false);
            for i in 0..count {
                let t = tracks + u32::from(i) * 0x50;
                bus.write8(t + 0x13, (fade >> 2) as u8);
                if fade == 0 {
                    release(bus, t, None);
                    bus.write8(t, 0);
                }
            }
            if fade == 0 {
                bus.write32(p + 4, PAUSED, false);
                return Ok(());
            }
        }
    }
    let accumulated = u32::from(bus.peek16(p + 0x22)) + u32::from(bus.peek16(p + 0x20));
    let ticks = accumulated / 150;
    bus.write16(p + 0x22, (accumulated % 150) as u16, false);
    if ticks > 32 {
        return Err("BIOS music tempo exceeds bounded sequence rate");
    }
    for _ in 0..ticks {
        for i in 0..count {
            let t = tracks + u32::from(i) * 0x50;
            if bus.peek8(t) & 0x80 == 0 {
                continue;
            }
            gate(bus, t);
            let wait = bus.peek8(t + 1);
            if wait != 0 {
                bus.write8(t + 1, wait - 1);
            }
            if bus.peek8(t + 1) == 0 {
                sequence(bus, p, t)?;
            }
            update_voices(bus, p, t);
        }
        bus.write32(p + 0xc, bus.peek32(p + 0xc).wrapping_add(1), false);
    }
    let mut status = 0;
    for i in 0..count {
        if bus.peek8(tracks + u32::from(i) * 0x50) & 0x80 != 0 {
            status |= 1 << i;
        }
    }
    bus.write32(p + 4, if status == 0 { PAUSED } else { status }, false);
    Ok(())
}
fn gate(bus: &mut Bus, t: u32) {
    let a = bus.peek32(super::GLOBAL);
    if !ram(bus, a, super::AREA_BYTES) {
        return;
    }
    for i in 0..12 {
        let c = a + 0x50 + i * 0x40;
        if bus.peek8(c) != 0 && bus.peek32(c + 0x2c) == t {
            let time = bus.peek8(c + 0x10);
            if time != 0 {
                bus.write8(c + 0x10, time - 1);
                if time == 1 {
                    bus.write8(c, bus.peek8(c) | 0x40);
                }
            }
        }
    }
}
fn sequence(bus: &mut Bus, p: u32, t: u32) -> Result<(), &'static str> {
    for _ in 0..1024 {
        let v = byte(bus, t);
        let cmd = if v >= 0x80 {
            let c = take(bus, t);
            if c >= 0xbd {
                bus.write8(t + 7, c);
            }
            c
        } else {
            bus.peek8(t + 7)
        };
        if (0x80..=0xb0).contains(&cmd) {
            let delay = length(cmd - 0x80);
            bus.write8(t + 1, delay);
            if delay != 0 {
                return Ok(());
            }
        } else if cmd >= 0xcf {
            note(bus, cmd - 0xcf, p, t)?;
        } else {
            command(bus, cmd, p, t)?;
            if bus.peek8(t) & 0x80 == 0 {
                return Ok(());
            }
        }
    }
    Err("BIOS music command stream has no wait or end")
}
fn command(bus: &mut Bus, cmd: u8, p: u32, t: u32) -> Result<(), &'static str> {
    match cmd {
        0xb1 => {
            release(bus, t, None);
            bus.write8(t, 0);
        }
        0xb2 => {
            let dest = word(bus, t);
            bus.write32(t + 0x40, dest, false);
        }
        0xb3 => {
            let dest = word(bus, t);
            let depth = bus.peek8(t + 2);
            if depth >= 3 {
                return Err("BIOS music pattern nesting exceeds three levels");
            }
            bus.write32(t + 0x44 + u32::from(depth) * 4, bus.peek32(t + 0x40), false);
            bus.write8(t + 2, depth + 1);
            bus.write32(t + 0x40, dest, false);
        }
        0xb4 => {
            let depth = bus.peek8(t + 2);
            if depth != 0 && depth <= 3 {
                bus.write8(t + 2, depth - 1);
                bus.write32(
                    t + 0x40,
                    bus.peek32(t + 0x44 + u32::from(depth - 1) * 4),
                    false,
                );
            }
        }
        0xb5 => {
            let times = take(bus, t);
            let dest = word(bus, t);
            let repeats = bus.peek8(t + 3).wrapping_add(1);
            if times == 0 || repeats < times {
                bus.write8(t + 3, repeats);
                bus.write32(t + 0x40, dest, false);
            } else {
                bus.write8(t + 3, 0);
            }
        }
        0xb9 => memacc(bus, p, t)?,
        0xba => argument(bus, t, 0x1d),
        0xbb => {
            let tempo = u16::from(take(bus, t)) * 2;
            bus.write16(p + 0x1c, tempo, false);
            bus.write16(
                p + 0x20,
                ((u32::from(tempo) * u32::from(bus.peek16(p + 0x1e))) >> 8) as u16,
                false,
            );
        }
        0xbc => argument(bus, t, 0xa),
        0xbd => {
            let instrument = u32::from(take(bus, t));
            let at = bus.peek32(p + 0x30).wrapping_add(instrument * 12);
            for i in 0..12 {
                bus.write8(t + 0x24 + i, bus.peek8(at + i));
            }
        }
        0xbe => argument(bus, t, 0x12),
        0xbf => {
            let v = take(bus, t);
            bus.write8(t + 0x14, v.wrapping_sub(64));
        }
        0xc0 => {
            let v = take(bus, t);
            bus.write8(t + 0xe, v.wrapping_sub(64));
        }
        0xc1 => argument(bus, t, 0xf),
        0xc2 => argument(bus, t, 0x19),
        0xc3 => {
            argument(bus, t, 0x1b);
            bus.write8(t + 0x1c, bus.peek8(t + 0x1b));
        }
        0xc4 => argument(bus, t, 0x17),
        0xc5 => argument(bus, t, 0x18),
        0xc8 => {
            let v = take(bus, t);
            bus.write8(t + 0xc, v.wrapping_sub(64));
        }
        0xcd => {
            let sub = take(bus, t);
            let value = take(bus, t);
            match sub {
                8 => bus.write8(t + 0x1e, value),
                9 => bus.write8(t + 0x1f, value),
                _ => return Err("BIOS music extended command not implemented"),
            }
        }
        0xce => {
            let key = if byte(bus, t) < 0x80 {
                Some(take(bus, t))
            } else {
                None
            };
            release(bus, t, key);
        }
        // Reserved commands in the stock sequence format have no operands.
        0xb6..=0xb8 | 0xc6..=0xc7 | 0xc9..=0xcc => {}
        _ => return Err("BIOS music command is invalid"),
    }
    Ok(())
}
fn memacc(bus: &mut Bus, p: u32, t: u32) -> Result<(), &'static str> {
    let operation = take(bus, t);
    let x = take(bus, t);
    let y = take(bus, t);
    let mem = bus.peek32(p + 0x18);
    if !ram(bus, mem, 256) {
        return Err("BIOS music memory-access area is invalid");
    }
    let a = mem + u32::from(x);
    let lhs = bus.peek8(a);
    let rhs = if matches!(operation,3..=5|12..=17) {
        bus.peek8(mem + u32::from(y))
    } else {
        y
    };
    match operation {
        0 | 3 => bus.write8(a, rhs),
        1 | 4 => bus.write8(a, lhs.wrapping_add(rhs)),
        2 | 5 => bus.write8(a, lhs.wrapping_sub(rhs)),
        6..=17 => {
            let kind = (operation - 6) % 6;
            let yes = match kind {
                0 => lhs == rhs,
                1 => lhs != rhs,
                2 => lhs > rhs,
                3 => lhs >= rhs,
                4 => lhs <= rhs,
                _ => lhs < rhs,
            };
            let dest = word(bus, t);
            if yes {
                bus.write32(t + 0x40, dest, false);
            }
        }
        _ => return Err("BIOS music memory-access operation is invalid"),
    }
    Ok(())
}
pub(super) fn note(bus: &mut Bus, index: u8, p: u32, t: u32) -> Result<(), &'static str> {
    if !valid(bus, p) || !ram(bus, t, 0x50) {
        return Ok(());
    }
    if index != 0 {
        bus.write8(t + 4, length(index));
    } else {
        bus.write8(t + 4, 0);
    }
    if byte(bus, t) < 0x80 {
        argument(bus, t, 5);
    }
    if byte(bus, t) < 0x80 {
        argument(bus, t, 6);
    }
    let extra = if byte(bus, t) < 0x80 { take(bus, t) } else { 0 };
    let Some(a) = sound_area_locked(bus) else {
        return Ok(());
    };
    let key = bus.peek8(t + 5);
    let mut tone = t + 0x24;
    let kind = bus.peek8(tone);
    if kind & 0xc0 != 0 {
        let mapping = if kind & 0x40 != 0 {
            bus.peek8(
                bus.peek32(tone.wrapping_add(8))
                    .wrapping_add(u32::from(key)),
            )
        } else {
            key
        };
        tone = bus
            .peek32(tone.wrapping_add(4))
            .wrapping_add(u32::from(mapping) * 12);
    }
    if bus.peek8(tone) & 7 != 0 {
        return Err("BIOS sequencer PSG instruments require a cartridge PSG driver");
    }
    let count = bus.peek8(a + 6).min(12);
    let priority = bus.peek8(p + 9).saturating_add(bus.peek8(t + 0x1d));
    let mut selected = None;
    for i in 0..count {
        let c = a + 0x50 + u32::from(i) * 0x40;
        if bus.peek8(c) == 0 {
            selected = Some(c);
            break;
        }
        if bus.peek8(c + 0x13) <= priority
            && selected.is_none_or(|old| bus.peek8(c + 0x13) < bus.peek8(old + 0x13))
        {
            selected = Some(c);
        }
    }
    let Some(c) = selected else {
        return Ok(());
    };
    unlink(bus, c);
    super::zero(bus, c, 0x40);
    bus.write8(c + 1, bus.peek8(tone));
    for i in 0..4 {
        bus.write8(c + 4 + i, bus.peek8(tone.wrapping_add(8 + i)));
    }
    bus.write8(
        c + 0x10,
        if index == 0 {
            0
        } else {
            bus.peek8(t + 4).saturating_add(extra)
        },
    );
    bus.write8(c + 0x11, key);
    bus.write8(c + 0x12, bus.peek8(t + 6));
    bus.write8(c + 0x13, priority);
    bus.write8(c + 12, bus.peek8(t + 0x1e));
    bus.write8(c + 13, bus.peek8(t + 0x1f));
    bus.write32(c + 0x24, bus.peek32(tone.wrapping_add(4)), false);
    bus.write32(c + 0x2c, t, false);
    // Keep the ABI's doubly linked channel list usable by cartridge extensions.
    let old = bus.peek32(t + 0x20);
    bus.write32(c + 0x34, old, false);
    if ram(bus, old, 0x40) {
        bus.write32(old + 0x30, c, false);
    }
    bus.write32(t + 0x20, c, false);
    bus.write8(c, 0x80);
    update_voice(bus, p, t, c);
    Ok(())
}
fn unlink(bus: &mut Bus, c: u32) {
    let track = bus.peek32(c + 0x2c);
    let previous = bus.peek32(c + 0x30);
    let next = bus.peek32(c + 0x34);
    if ram(bus, previous, 0x40) {
        bus.write32(previous + 0x34, next, false);
    } else if ram(bus, track, 0x50) && bus.peek32(track + 0x20) == c {
        bus.write32(track + 0x20, next, false);
    }
    if ram(bus, next, 0x40) {
        bus.write32(next + 0x30, previous, false);
    }
}
fn sound_area_locked(bus: &Bus) -> Option<u32> {
    let a = bus.peek32(super::GLOBAL);
    (ram(bus, a, super::AREA_BYTES) && bus.peek32(a).wrapping_sub(IDENT) <= 1).then_some(a)
}
fn update_voices(bus: &mut Bus, p: u32, t: u32) {
    if bus.peek8(t + 0x1c) != 0 {
        bus.write8(t + 0x1c, bus.peek8(t + 0x1c) - 1);
    } else {
        let phase = bus.peek8(t + 0x1a).wrapping_add(bus.peek8(t + 0x19));
        bus.write8(t + 0x1a, phase);
        let triangle = if phase < 64 {
            i16::from(phase)
        } else if phase < 192 {
            128 - i16::from(phase)
        } else {
            i16::from(phase) - 256
        };
        bus.write8(
            t + 0x16,
            ((triangle * i16::from(bus.peek8(t + 0x17))) >> 6) as u8,
        );
    }
    let Some(a) = sound_area_locked(bus) else {
        return;
    };
    for i in 0..12 {
        let c = a + 0x50 + i * 0x40;
        if bus.peek8(c) != 0 && bus.peek32(c + 0x2c) == t {
            update_voice(bus, p, t, c);
        }
    }
}
fn update_voice(bus: &mut Bus, _p: u32, t: u32, c: u32) {
    let modulation = i32::from(bus.peek8(t + 0x16) as i8);
    let mod_type = bus.peek8(t + 0x18);
    let volume = (i32::from(bus.peek8(t + 0x12)) * (i32::from(bus.peek8(t + 0x13)))).max(0) / 64;
    let volume = (volume + if mod_type == 1 { modulation } else { 0 }).clamp(0, 127);
    let volume = volume * i32::from(bus.peek8(c + 0x12)) / 128;
    let pan = (i32::from(bus.peek8(t + 0x14) as i8)
        + i32::from(bus.peek8(t + 0x15) as i8)
        + if mod_type == 2 { modulation } else { 0 })
    .clamp(-64, 63);
    bus.write8(c + 2, ((volume * (64 + pan)) >> 6).clamp(0, 255) as u8);
    bus.write8(c + 3, ((volume * (64 - pan)) >> 6).clamp(0, 255) as u8);
    let key = i32::from(bus.peek8(c + 0x11))
        + i32::from(bus.peek8(t + 0xa) as i8)
        + i32::from(bus.peek8(t + 0xb) as i8);
    let pitch = i32::from(bus.peek8(t + 0xc) as i8) * 4
        + i32::from(bus.peek8(t + 0xe) as i8) * i32::from(bus.peek8(t + 0xf)) * 4
        + if mod_type == 0 { modulation * 16 } else { 0 };
    let pitch = (key * 256 + pitch).clamp(0, 178 * 256);
    bus.write8(c + 8, (pitch >> 8) as u8);
    let wave = bus.peek32(c + 0x24);
    let freq = super::super::math::midi_frequency(
        bus.peek32(wave.wrapping_add(4)),
        (pitch >> 8) as u32,
        (pitch & 255) as u32,
    );
    bus.write32(c + 0x20, freq, false);
}
pub(super) fn jump(bus: &mut Bus, index: u8, cpu: &mut Cpu) -> Result<(), &'static str> {
    if index < 30 {
        command(bus, 0xb1 + index, cpu.r[0], cpu.r[1])
    } else {
        Err("BIOS sound jump-list extension helper not implemented")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    const A: u32 = 0x02001000;
    const P: u32 = 0x02004000;
    const T: u32 = 0x02005000;
    const SONG: u32 = 0x02008000;
    const TONES: u32 = 0x02008100;
    const STREAM: u32 = 0x02008200;
    const WAVE: u32 = 0x02009000;
    fn setup(stream: &[u8]) -> (Cpu, Bus) {
        let mut b = Bus::new(vec![0; 192], None).unwrap();
        let mut c = Cpu::default();
        c.r[0] = A;
        super::super::service(0x1a, &mut c, &mut b).unwrap();
        c.r[0] = P;
        c.r[1] = T;
        c.r[2] = 1;
        service(0x20, &mut c, &mut b).unwrap();
        b.write8(SONG, 1);
        b.write32(SONG + 4, TONES, false);
        b.write32(SONG + 8, STREAM, false);
        b.write8(TONES, 0);
        b.write32(TONES + 4, WAVE, false);
        for (i, v) in [255u8, 128, 64, 128].into_iter().enumerate() {
            b.write8(TONES + 8 + i as u32, v);
        }
        b.write16(WAVE + 2, 0x4000, false);
        b.write32(WAVE + 4, 16000 * 1024, false);
        b.write32(WAVE + 8, 0, false);
        b.write32(WAVE + 12, 4, false);
        for (i, v) in [64i8, -64, 32, -32].into_iter().enumerate() {
            b.write8(WAVE + 16 + i as u32, v as u8);
        }
        for (i, v) in stream.iter().enumerate() {
            b.write8(STREAM + i as u32, *v);
        }
        c.r[0] = P;
        c.r[1] = SONG;
        service(0x21, &mut c, &mut b).unwrap();
        (c, b)
    }
    fn pass(b: &mut Bus) {
        b.write32(A, IDENT + 1, false);
        players(b, P).unwrap();
        super::super::mix(b, A).unwrap();
    }
    #[test]
    fn stock_note_lengths_cover_every_wait_and_gate() {
        let expected = [
            0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23,
            24, 28, 30, 32, 36, 40, 42, 44, 48, 52, 54, 56, 60, 64, 66, 68, 72, 76, 78, 80, 84, 88,
            90, 92, 96,
        ];
        for (i, value) in expected.into_iter().enumerate() {
            assert_eq!(length(i as u8), value);
        }
    }
    #[test]
    fn song_controls_generate_pcm_pause_resume_fade_and_keep_linkage_on_reopen() {
        let (mut cpu, mut b) = setup(&[
            0xbd,
            0,
            0xbe,
            127,
            0xbf,
            64,
            0xcf,
            60,
            127,
            0xb0,
            0xb2,
            (STREAM & 255) as u8,
            ((STREAM >> 8) & 255) as u8,
            ((STREAM >> 16) & 255) as u8,
            (STREAM >> 24) as u8,
        ]);
        assert_eq!(b.peek32(A + 0x20), PLAYER | 1);
        assert_eq!(b.peek32(A + 0x24), P);
        pass(&mut b);
        let c = A + 0x50;
        assert_ne!(b.peek8(c), 0);
        assert_eq!(b.peek32(c + 0x20), 16000);
        assert_eq!(b.peek8(c + 2), b.peek8(c + 3));
        assert!((0..224).any(|i| b.peek8(A + super::super::BUFFER + i) != 0));
        let clock = b.peek32(P + 0xc);
        cpu.r[0] = P;
        service(0x22, &mut cpu, &mut b).unwrap();
        pass(&mut b);
        assert_eq!(b.peek32(P + 0xc), clock);
        assert_ne!(b.peek8(c) & 0x40, 0);
        service(0x23, &mut cpu, &mut b).unwrap();
        pass(&mut b);
        assert_eq!(b.peek32(P + 0xc), clock + 1);
        cpu.r[1] = 1;
        service(0x24, &mut cpu, &mut b).unwrap();
        for _ in 0..16 {
            pass(&mut b);
        }
        assert_eq!(b.peek16(P + 0x28), 0);
        assert_eq!(b.peek8(T), 0);
        assert_eq!(b.peek32(P + 4), PAUSED);
        cpu.r[0] = P;
        cpu.r[1] = T;
        cpu.r[2] = 1;
        service(0x20, &mut cpu, &mut b).unwrap();
        assert_eq!(b.peek32(P + 0x3c), 0);
        assert_eq!(b.peek32(A + 0x24), P);
    }
    #[test]
    fn patterns_running_status_stereo_and_malformed_streams() {
        // Instrument/volume, left pan; two notes with running status; bounded waits.
        let (_, mut b) = setup(&[
            0xbd, 0, 0xbe, 127, 0xbf, 0, 0xd3, 60, 127, 0x84, 62, 64, 0x84, 0xb1,
        ]);
        pass(&mut b);
        let first = A + 0x50;
        assert_eq!(b.peek8(first + 2), 0);
        assert!(b.peek8(first + 3) > 0);
        assert_eq!(b.peek8(first + 0x11), 60);
        assert_eq!(b.peek8(first + 0x10), 4);
        for _ in 0..4 {
            pass(&mut b);
        }
        assert!((0..12).any(|i| b.peek8(A + 0x50 + i * 0x40 + 0x11) == 62));
        let at = STREAM + 0x40;
        for (i, v) in at.to_le_bytes().into_iter().enumerate() {
            b.write8(STREAM + 1 + i as u32, v);
        }
        b.write32(T + 0x40, STREAM + 1, false);
        command(&mut b, 0xb3, P, T).unwrap();
        assert_eq!(b.peek32(T + 0x40), at);
        assert_eq!(b.peek8(T + 2), 1);
        command(&mut b, 0xb4, P, T).unwrap();
        assert_eq!(b.peek32(T + 0x40), STREAM + 5);
        b.write8(T + 2, 3);
        assert!(command(&mut b, 0xb3, P, T).is_err());
    }
    #[test]
    fn command_budget_rejects_self_jumps_and_memacc_branches_use_unaligned_words() {
        let (_, mut b) = setup(&[
            0xb2,
            (STREAM & 255) as u8,
            ((STREAM >> 8) & 255) as u8,
            ((STREAM >> 16) & 255) as u8,
            (STREAM >> 24) as u8,
        ]);
        assert!(players(&mut b, P).is_err());
        let target = STREAM + 0x70;
        b.write32(T + 0x40, STREAM, false);
        b.write32(P + 0x18, 0x0200a000, false);
        b.write8(0x0200a005, 7);
        for (i, v) in [6u8, 5, 7]
            .into_iter()
            .chain(target.to_le_bytes())
            .enumerate()
        {
            b.write8(STREAM + i as u32, v);
        }
        memacc(&mut b, P, T).unwrap();
        assert_eq!(b.peek32(T + 0x40), target);
    }
}

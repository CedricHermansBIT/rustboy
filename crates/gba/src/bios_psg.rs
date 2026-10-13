//! Original fallback MusicPlayer2000 PSG driver. It programs documented sound
//! registers and keeps its envelope in the public CgbChannel RAM ABI. Oscillator
//! synthesis still runs through the ordinary APU; no host-only continuation state.
//! Software envelope cadence and noise pitch mapping are functional approximations,
//! not a claim of bit-identical Nintendo/cartridge driver output.
use super::{Bus, Memory};
const LENGTH: [u32; 4] = [0x04000062, 0x04000068, 0x04000072, 0x04000078];
const CONTROL: [u32; 4] = [0x04000064, 0x0400006c, 0x04000074, 0x0400007c];

pub(super) fn frequency(kind: u8, pitch: i32) -> u32 {
    // Original equal-temperament values, derived from A4=440 Hz. These are not
    // firmware's quantized pitch table. Compute once, then interpolate in integers.
    static HZ: std::sync::OnceLock<[u32; 180]> = std::sync::OnceLock::new();
    let hz = HZ.get_or_init(|| {
        std::array::from_fn(|key| {
            (440. * 4096. * 2f64.powf((key as f64 - 69.) / 12.)).round() as u32
        })
    });
    let pitch = pitch.clamp(0, 178 * 256);
    let key = (pitch >> 8) as usize;
    let fine = (pitch & 255) as u64;
    let rate = u64::from(hz[key]) + ((u64::from(hz[key + 1] - hz[key]) * fine) >> 8);
    if kind == 4 {
        let mut best = (u64::MAX, 0);
        for shift in 0..16 {
            for ratio in 0..8 {
                let period = (if ratio == 0 { 32 } else { ratio * 64 }) << shift;
                let error = (16777216u64 * 4096 / period).abs_diff(rate);
                if error < best.0 {
                    best = (error, ((shift << 4) | ratio) as u32);
                }
            }
        }
        best.1
    } else {
        let numerator = if kind == 3 { 65536u64 } else { 131072u64 } * 4096;
        let period = ((numerator + rate / 2) / rate.max(1)).clamp(1, 2048);
        2048 - period as u32
    }
}
pub(super) fn stop(bus: &mut Bus, kind: u8) {
    let Some(n) = kind.checked_sub(1).filter(|n| *n < 4) else {
        return;
    };
    let mask = 0x11u16 << n;
    bus.write16(0x04000080, bus.io16(0x80) & !(mask << 8), false);
    if n == 2 {
        bus.write8(0x04000070, 0);
    } else {
        bus.write8(LENGTH[usize::from(n)] + 1, 0);
    }
}
pub(super) fn update(bus: &mut Bus, c: u32) {
    let kind = bus.peek8(c + 1) & 7;
    let Some(n) = kind.checked_sub(1).filter(|n| *n < 4) else {
        return;
    };
    let n = usize::from(n);
    let mut state = bus.peek8(c);
    let start = state & 0x80 != 0;
    let old_level = bus.peek8(c + 9);
    let mut level = old_level;
    let mut countdown = bus.peek8(c + 11);
    let mut changed = start;
    if start {
        if state & 0x40 != 0 {
            stop(bus, kind);
            bus.write8(c, 0);
            return;
        }
        level = if bus.peek8(c + 4) == 0 { 15 } else { 0 };
        state = if level == 15 { 2 } else { 3 };
        countdown = bus.peek8(c + 4);
        let length = bus.peek8(c + 0x1e);
        let length_mask = if n == 2 { 255 } else { 63 };
        let control = if length == 0 { 0 } else { 0x40 };
        bus.write8(c + 0x1a, control);
        if n == 0 {
            bus.write8(0x04000060, bus.peek8(c + 0x1f) & 0x7f);
        }
        if n == 2 {
            // CPU wave access sees the bank opposite the playback bank. Load
            // while stopped, then select that bank for 32-sample playback.
            bus.write8(0x04000070, 0);
            let wave = bus.peek32(c + 0x24);
            for i in 0..16 {
                bus.write8(0x04000090 + i, bus.peek8(wave.wrapping_add(i)));
            }
            bus.write8(0x04000070, 0xc0);
            bus.write8(LENGTH[n], length);
        } else {
            let duty = (bus.peek32(c + 0x24) as u8 & 3) << 6;
            bus.write8(
                LENGTH[n],
                (length & length_mask) | if n < 2 { duty } else { 0 },
            );
        }
    } else if state & 4 != 0 {
        let remaining = bus.peek8(c + 13).saturating_sub(1);
        bus.write8(c + 13, remaining);
        if remaining == 0 {
            level = 0;
        }
    } else {
        countdown = countdown.saturating_sub(1);
        if countdown == 0 || state & 0x40 != 0 && state & 3 != 0 {
            if state & 0x40 != 0 {
                state &= !3;
                if bus.peek8(c + 7) == 0 {
                    level = 0;
                } else {
                    level = level.saturating_sub(1);
                    countdown = bus.peek8(c + 7);
                }
            } else {
                match state & 3 {
                    3 => {
                        level = level.saturating_add(1).min(15);
                        countdown = bus.peek8(c + 4);
                        if level == 15 {
                            state = 2;
                            countdown = bus.peek8(c + 5);
                        }
                    }
                    2 => {
                        level = if bus.peek8(c + 5) == 0 {
                            bus.peek8(c + 6).min(15)
                        } else {
                            level.saturating_sub(1)
                        };
                        countdown = bus.peek8(c + 5);
                        if level <= bus.peek8(c + 6).min(15) {
                            level = bus.peek8(c + 6).min(15);
                            state = 1;
                        }
                    }
                    0 if state & 0x40 != 0 => {
                        level = level.saturating_sub(1);
                        countdown = bus.peek8(c + 7);
                    }
                    _ => {}
                }
            }
        }
    }
    // Pseudo-echo is a finite held tail after release reaches silence.
    if level == 0 && state & 3 != 3 {
        let echo = ((u16::from(old_level) * u16::from(bus.peek8(c + 12))) >> 7) as u8;
        if state & 4 == 0 && echo != 0 && bus.peek8(c + 13) != 0 {
            level = echo.min(15);
            state = 4;
        } else {
            stop(bus, kind);
            bus.write8(c, 0);
            return;
        }
    }
    changed |= old_level != level;
    bus.write8(c, state);
    bus.write8(c + 9, level);
    bus.write8(c + 11, countdown);
    let right = bus.peek8(c + 2);
    let left = bus.peek8(c + 3);
    let volume = ((u16::from(right.max(left)) * u16::from(level) + 63) / 127).min(15) as u8;
    changed |= bus.peek8(c + 10) != volume;
    bus.write8(c + 10, volume);
    let mask = 0x11u16 << n;
    let pan = if right != 0 { 1u16 << n } else { 0 } | if left != 0 { 0x10u16 << n } else { 0 };
    let routing = (bus.io16(0x80) & !(mask << 8)) | (pan << 8);
    bus.write16(0x04000080, routing | 0x77, false);
    if bus.peek8(c + 0x1d) & 1 != 0 || changed {
        if n == 2 {
            let scale = match volume {
                0 => 0,
                1..=3 => 0x60,
                4..=7 => 0x40,
                8..=11 => 0x80,
                _ => 0x20,
            };
            bus.write8(LENGTH[n] + 1, scale);
        } else {
            bus.write8(LENGTH[n] + 1, (volume << 4) | 8);
        }
    }
    let mut freq = bus.peek32(c + 0x20) as u16;
    if n == 3 {
        freq |= (bus.peek32(c + 0x24) as u16 & 1) << 3;
    }
    if start || changed || bus.peek8(c + 0x1d) & 2 != 0 {
        let control = u16::from(bus.peek8(c + 0x1a)) << 8;
        let restart = if start || changed && n != 2 {
            0x8000
        } else {
            0
        };
        bus.write16(CONTROL[n], freq | control | restart, false);
    }
    bus.write8(c + 0x1d, 0);
}

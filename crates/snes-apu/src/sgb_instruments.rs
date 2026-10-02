//! Original synthesized SGB substitute instruments: harmonic tone loops and
//! independently generated percussion/noise. No recordings or BIOS assets.
use crate::{brr, SpcRam};

fn family(id: u8) -> usize {
    match id {
        0..=7 => 0,
        8..=11 => 1,
        12..=15 => 23,
        16..=19 => 2,
        20..=26 => 3,
        27 => 4,
        28..=30 => 5,
        31..=32 => 6,
        33..=34 => 24,
        35..=38 => 7,
        39..=40 => 8,
        41 => 25,
        42..=45 => 26,
        46..=47 => 9,
        48 => 10,
        49 => 11,
        50 => 12,
        51 => 13,
        52..=53 => 14,
        54 => 15,
        55 => 16,
        56..=57 => 17,
        58 => 18,
        59 => 19,
        60 => 20,
        61 => 21,
        _ => 22,
    }
}
fn noise(state: &mut u32) -> f64 {
    *state ^= *state << 13;
    *state ^= *state >> 17;
    *state ^= *state << 5;
    f64::from(*state & 65535) / 32767.5 - 1.0
}
fn tone(kind: usize, phase: f64, cycle: f64) -> f64 {
    let angle = phase * std::f64::consts::TAU;
    match kind {
        0 => angle.sin(),
        1 => angle.sin() * 0.8 + (angle * 2.0).sin() * 0.14 + (angle * 3.0).sin() * 0.06,
        2 => (1..=9)
            .map(|h| {
                ((angle * h as f64).sin() / ((h * h) as f64)) * if h & 1 == 0 { 0.45 } else { 0.7 }
            })
            .sum(),
        3 => {
            angle.sin() * 0.7
                + (angle * 2.0 + 0.5 * (angle * 3.0).sin()).sin() * 0.2
                + (angle * 4.0).sin() * 0.1
        }
        4 => {
            angle.sin() * 0.5
                + (angle * 2.0).sin() * 0.24
                + (angle * 3.0).sin() * 0.14
                + (angle * 4.0).sin() * 0.08
        }
        5 => (1..=10)
            .map(|h| {
                ((angle * h as f64 + 0.025 * (cycle * std::f64::consts::TAU).sin()).sin()
                    / (h as f64).powf(1.6))
                    * 0.6
            })
            .sum(),
        6 => {
            angle.sin() * 0.5
                + (angle * 2.0).sin() * 0.15
                + (angle * 3.0).sin() * 0.22
                + (angle * 5.0).sin() * 0.08
        }
        7 => angle.sin() * 0.65 + (angle * 4.0).sin() * 0.24 + (angle * 7.0).sin() * 0.06,
        8 => (1..=8)
            .map(|h| (angle * h as f64).sin() * (-h as f64 * 0.34).exp() * 0.65)
            .sum(),
        9 => angle.sin() * 0.66 + (angle * 3.0).sin() * 0.22 + (angle * 5.0).sin() * 0.06,
        _ => angle.sin() * 0.93 + (angle * 2.0).sin() * 0.045 + (angle * 3.0).sin() * 0.02,
    }
}
pub(super) fn initialize(ram: &mut SpcRam) {
    static BANK: std::sync::OnceLock<SpcRam> = std::sync::OnceLock::new();
    let bank = BANK.get_or_init(|| {
        let mut bank = SpcRam::default();
        build_bank(&mut bank);
        bank
    });
    ram.write_wrapping(0x4b00, &bank.bytes()[0x4b00..0xef00]);
}
fn build_bank(ram: &mut SpcRam) {
    let mut address = 0x4db0u16;
    let mut sources = [(0u16, 0u16); 27];
    for (kind, source) in sources.iter_mut().enumerate() {
        let tonal = kind <= 10 || kind >= 23;
        let looped = tonal || matches!(kind, 18 | 21 | 22);
        let count = match kind {
            0..=10 | 23..=26 => 528,
            11 => 2560,
            12 => 1536,
            13 => 6144,
            14 | 15 | 19 => 4096,
            16 => 3072,
            17 => 8192,
            20 => 2048,
            _ => 528,
        };
        let mut seed = 0x52425347u32 + kind as u32;
        let mut low = 0.;
        let mut oscillator = 0.;
        let mut pcm = Vec::with_capacity(count);
        for i in 0..count {
            let t = i as f64 / 32000.0;
            let n = noise(&mut seed);
            low = low * 0.88 + n * 0.12;
            let value = if tonal {
                // Warm-up contains the preceding 16 samples; loop predictors
                // enter the same phase at start and subsequent wraps.
                let period = match kind {
                    5 | 10 | 25 | 26 => 32.0,
                    6 | 24 => 16.0,
                    7 => 128.0,
                    _ => 64.0,
                };
                let phase = (i as f64 - 16.0) / period;
                let cycle = (i as f64 - 16.0) / 512.0;
                tone(
                    match kind {
                        23 => 1,
                        24 => 6,
                        25 | 26 => 8,
                        _ => kind,
                    },
                    phase,
                    cycle,
                )
            } else {
                match kind {
                    11 => {
                        oscillator +=
                            (52.0 + 110.0 * (-t * 55.0).exp()) * std::f64::consts::TAU / 32000.0;
                        (oscillator.sin() + n * 0.1 * (-t * 150.0).exp()) * (-t * 33.0).exp()
                    }
                    12 => n * (-t * 95.0).exp() * 0.7,
                    13 => (n - low) * (-t * 15.0).exp() * 0.6,
                    14 => {
                        (n * 0.72 + (t * 185.0 * std::f64::consts::TAU).sin() * 0.28)
                            * (-t * 28.0).exp()
                    }
                    15 => {
                        oscillator +=
                            (105.0 + 85.0 * (-t * 40.0).exp()) * std::f64::consts::TAU / 32000.0;
                        (oscillator.sin() * 0.9 + n * 0.1) * (-t * 25.0).exp()
                    }
                    16 => {
                        let burst = if t < 0.04 {
                            (t * 160.0).fract()
                        } else {
                            (-t * 36.0).exp()
                        };
                        n * burst
                    }
                    17 => low * 0.95 * (1.0 - (-t * 25.0).exp()) * (-t * 8.0).exp(),
                    18 => n * 0.65,
                    19 => {
                        (n * 0.6
                            + (t * 3300.0 * std::f64::consts::TAU).sin() * 0.25
                            + (t * 5400.0 * std::f64::consts::TAU).sin() * 0.15)
                            * (-t * 35.0).exp()
                    }
                    20 => (low * 0.7 + n * 0.3) * (-t * 65.0).exp(),
                    21 => low * 0.85 + (n - low) * 0.15,
                    _ => low,
                }
            };
            let attack = if tonal {
                (i as f64 / 16.0).min(1.0)
            } else {
                (i as f64 / 8.0).min(1.0)
            };
            // Calibrated using independently authored one-note tests, not
            // extracted waveforms. Cartridge samples are never rescaled.
            let amplitude = match kind {
                0 => 1500.,
                1 => 2100.,
                2 => 1560.,
                3 => 1800.,
                4 => 2380.,
                5 => 1560.,
                6 => 1500.,
                7 => 1280.,
                8 => 3600.,
                9 => 1970.,
                10 => 2070.,
                23 => 1370.,
                24 => 2790.,
                25 => 1430.,
                26 => 1100.,
                _ => 16000.,
            };
            pcm.push(
                (value * attack * amplitude)
                    .round()
                    .clamp(-22000.0, 22000.0) as i16,
            );
        }
        // Weather loops crossfade their last 32 samples into the first loop
        // samples to avoid periodic clicks. Tone loops are phase-continuous.
        if matches!(kind, 18 | 21 | 22) {
            for i in 0..32 {
                let ratio = i as f64 / 32.;
                let tail = count - 32 + i;
                pcm[tail] =
                    (f64::from(pcm[tail]) * (1. - ratio) + f64::from(pcm[16 + i]) * ratio) as i16;
            }
        }
        let data = brr::encode(&pcm, looped);
        assert!(
            usize::from(address) + data.len() <= 0xef00,
            "original bank exceeds SGB sample RAM"
        );
        *source = (
            address,
            if matches!(kind, 18 | 21 | 22) {
                address + 27
            } else if looped {
                address + 9
            } else {
                address
            },
        );
        ram.write_wrapping(address, &data);
        address += data.len() as u16;
    }
    for id in 0..64u8 {
        let kind = family(id);
        let (start, loop_address) = sources[kind];
        ram.write_wrapping(
            0x4b00 + u16::from(id) * 4,
            &[
                start as u8,
                (start >> 8) as u8,
                loop_address as u8,
                (loop_address >> 8) as u8,
            ],
        );
        // Tuned 64-frame tone periods; one-shots play near native 32 kHz at C3.
        // Resident source families have different natural octaves. These tune
        // our own waveforms to measured behavior; no BIOS tables are embedded.
        let multiplier: u16 = match kind {
            0 | 3 | 4 | 8 | 9 => 1024,
            1 => 1023,
            23 => 1027,
            2 => 1016,
            5 => 1005,
            6 => 1048,
            24 => 524,
            7 => 1012,
            10 => 1015,
            25 => 1023,
            26 => 1017,
            _ => 1960,
        };
        let (adsr1, adsr2) = match id {
            1 | 18 | 35 | 42 => (0x8f, 0x25),
            5 | 8 | 12 | 17 | 43 => (0xcf, 0x46),
            2 | 20 | 24 | 36 => (0xbf, 0x69),
            6 | 14 | 25 | 31 | 44 => (0xa9, 0xa0),
            9 | 13 => (0xbf, 0x88),
            28 => (0xaa, 0xa0),
            29 | 30 => (0xab, 0xc0),
            3 | 39 | 40 | 41 => (0xad, 0xc0),
            _ if kind >= 11 => (0x8f, 0xe0),
            _ => (0xaf, 0xe0),
        };
        ram.write_wrapping(
            0x4c30 + u16::from(id) * 6,
            &[
                id,
                adsr1,
                adsr2,
                0,
                (multiplier >> 8) as u8,
                multiplier as u8,
            ],
        );
    }
}

//! Independently authored integer math for replacement BIOS services.
//! Angle services compute mathematical angles, not firmware polynomial rounding.

// round(atan(2^-n) * 2^32 / TAU), generated from the mathematical definition.
// These are not firmware tables. Full-turn Q32 keeps rounding below the final
// 16-bit angular unit; runtime uses only integer shifts/additions.
const ROTATIONS: [i64; 31] = [
    536870912, 316933406, 167458907, 85004756, 42667331, 21354465, 10679838, 5340245, 2670163,
    1335087, 667544, 333772, 166886, 83443, 41722, 20861, 10430, 5215, 2608, 1304, 652, 326, 163,
    81, 41, 20, 10, 5, 3, 1, 1,
];

fn first_quadrant(x: u32, y: u32) -> u16 {
    // Headroom is enough even for both unsigned magnitudes of i32::MIN. The
    // fractional bits retain small coordinates through vectoring iterations.
    let (mut x, mut y) = (i64::from(x) << 28, i64::from(y) << 28);
    let mut angle = 0i64;
    for (shift, rotation) in ROTATIONS.into_iter().enumerate() {
        if y == 0 {
            break;
        }
        let old_x = x;
        if y > 0 {
            x += y >> shift;
            y -= old_x >> shift;
            angle += rotation;
        } else {
            x -= y >> shift;
            y += old_x >> shift;
            angle -= rotation;
        }
    }
    ((angle + (1 << 15)) >> 16).clamp(0, 0x4000) as u16
}

/// Signed tangent in Q14; signed angle where one turn is 65536 units.
pub(super) fn arctan(tangent: i32) -> i32 {
    let angle = i32::from(first_quadrant(1 << 14, tangent.unsigned_abs()));
    if tangent < 0 {
        -angle
    } else {
        angle
    }
}

/// Counterclockwise angle from +X, wrapped to a 16-bit turn. Zero vector is 0.
pub(super) fn arctan2(x: i32, y: i32) -> u16 {
    if y == 0 {
        return if x < 0 { 0x8000 } else { 0 };
    }
    if x == 0 {
        return if y < 0 { 0xc000 } else { 0x4000 };
    }
    let angle = first_quadrant(x.unsigned_abs(), y.unsigned_abs());
    match (x < 0, y < 0) {
        (false, false) => angle,
        (true, false) => 0x8000 - angle,
        (true, true) => 0x8000 + angle,
        (false, true) => 0u16.wrapping_sub(angle),
    }
}

// round(2^(31 + semitone/12)), independently generated from equal temperament.
// The octave shift and integer interpolation avoid floating-point variation
// in the playback path. These mathematical constants are not firmware bytes.
const SEMITONES: [u32; 12] = [
    0x80000000, 0x879c7c97, 0x8facd61e, 0x9837f052, 0xa14517cc, 0xaadc0848, 0xb504f334, 0xbfc886bb,
    0xcb2ff52a, 0xd744fccb, 0xe411f03a, 0xf1a1bf39,
];

/// WaveData frequency is referenced to MIDI key 180. Hardware interpolates
/// adjacent semitone ratios linearly in Q32, then takes the high product word.
pub(super) fn midi_frequency(base: u32, key: u32, fine: u32) -> u32 {
    let (key, fine) = if key > 178 {
        (178, 0)
    } else {
        (key, fine & 255)
    };
    let ratio = |note: u32| SEMITONES[(note % 12) as usize] >> (14 - note / 12);
    let low = ratio(key);
    let high = ratio(key + 1);
    let interpolated = low + ((u64::from(high - low) * u64::from(fine)) >> 8) as u32;
    ((u64::from(base) * u64::from(interpolated)) >> 32) as u32
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn midi_octaves_clamping_and_full_width_interpolation() {
        for base in [0, 1, 0x00400000, 0x12345678, u32::MAX] {
            for key in (0..=168).step_by(12) {
                assert_eq!(midi_frequency(base, key, 0), base >> (15 - key / 12));
            }
            for key in [179, 180, 255, u32::MAX] {
                assert_eq!(midi_frequency(base, key, 255), midi_frequency(base, 178, 0));
            }
            for key in 0..=178 {
                let low = 2f64.powf((f64::from(key) - 180.) / 12.);
                let high = 2f64.powf((f64::from(key) + 1. - 180.) / 12.);
                let mut previous = 0;
                for fine in 0..=255 {
                    let actual = midi_frequency(base, key, fine);
                    let ideal = (f64::from(base) * (low + (high - low) * f64::from(fine) / 256.))
                        .floor() as u32;
                    assert!(
                        actual.abs_diff(ideal) <= 2,
                        "base {base:x}, key {key}, fine {fine}"
                    );
                    assert!(actual >= previous);
                    assert_eq!(actual, midi_frequency(base, key, fine + 256));
                    previous = actual;
                }
            }
        }
    }
    #[test]
    fn axes_diagonals_zero_and_signed_extremes() {
        for (x, y, angle) in [
            (0, 0, 0),
            (1, 0, 0),
            (0, 1, 0x4000),
            (-1, 0, 0x8000),
            (0, -1, 0xc000),
            (1, 1, 0x2000),
            (-1, 1, 0x6000),
            (-1, -1, 0xa000),
            (1, -1, 0xe000),
            (i32::MIN, i32::MIN, 0xa000),
            (i32::MIN, 0, 0x8000),
            (0, i32::MIN, 0xc000),
            (i32::MAX, i32::MAX, 0x2000),
        ] {
            assert_eq!(arctan2(x, y), angle);
        }
        assert_eq!(arctan(0), 0);
        assert_eq!(arctan(0x4000), 0x2000);
        assert_eq!(arctan(-0x4000), -0x2000);
    }
    #[test]
    fn every_documented_tangent_is_monotonic_odd_and_matches_math() {
        let mut previous = i32::MIN;
        for tangent in i16::MIN..=i16::MAX {
            let tangent = i32::from(tangent);
            let actual = arctan(tangent);
            let expected = ((f64::from(tangent) / 16384.).atan() * 65536. / std::f64::consts::TAU)
                .round() as i32;
            assert!((actual - expected).abs() <= 1, "tangent {tangent}");
            assert!(actual >= previous);
            assert_eq!(actual, -arctan(-tangent));
            previous = actual;
        }
    }
    #[test]
    fn vectoring_quadrants_scale_invariance_and_full_width_math_oracle() {
        for x in -64..=64i32 {
            for y in -64..=64i32 {
                let actual = arctan2(x, y);
                let expected = (f64::from(y).atan2(f64::from(x)) * 65536. / std::f64::consts::TAU)
                    .round() as i32 as u16;
                assert!(
                    actual
                        .wrapping_sub(expected)
                        .min(expected.wrapping_sub(actual))
                        <= 1
                );
                assert_eq!(arctan2(x * 32768, y * 32768), actual);
            }
        }
        let mut bits = 0x12345678u32;
        for _ in 0..8192 {
            bits = bits.wrapping_mul(1664525).wrapping_add(1013904223);
            let x = bits as i32;
            bits = bits.wrapping_mul(1664525).wrapping_add(1013904223);
            let y = bits as i32;
            let expected = (f64::from(y).atan2(f64::from(x)) * 65536. / std::f64::consts::TAU)
                .round() as i32 as u16;
            let actual = arctan2(x, y);
            assert!(
                actual
                    .wrapping_sub(expected)
                    .min(expected.wrapping_sub(actual))
                    <= 1
            );
        }
    }
}

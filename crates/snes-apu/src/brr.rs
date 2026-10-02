//! RustBoy's original BRR encoder for procedurally authored instruments.
//! Searches scale/filter pairs against the actual decoder, carrying predictor
//! history. Direct 4-bit PCM waveforms are far too coarse for musical samples.
use crate::dsp::{brr_prediction, reconstruct_brr};

pub fn encode(pcm: &[i16], looped: bool) -> Vec<u8> {
    assert!(!pcm.is_empty() && pcm.len() % 16 == 0);
    let mut out = Vec::with_capacity(pcm.len() / 16 * 9);
    let mut history = [0; 2];
    for (block_index, block) in pcm.chunks_exact(16).enumerate() {
        let mut best_cost = u64::MAX;
        let mut best = [0; 9];
        let mut best_history = history;
        for filter in 0..=if block_index == 0 { 0 } else { 3 } {
            for shift in 0..=12 {
                let mut trial = [0; 9];
                trial[0] = (shift << 4) | (filter << 2);
                let mut state = history;
                let mut cost = 0u64;
                for (i, &target) in block.iter().enumerate() {
                    let residual = i32::from(target) - 2 * brr_prediction(filter, state);
                    let estimate = (residual as f64 / f64::from(1u32 << shift)).round() as i32;
                    let mut chosen = 0;
                    let mut chosen_cost = u64::MAX;
                    let mut chosen_state = state;
                    for candidate in estimate.saturating_sub(1)..=estimate.saturating_add(1) {
                        let nibble = candidate.clamp(-8, 7) as i8;
                        let mut next = state;
                        let reconstructed = reconstruct_brr(nibble, shift, filter, &mut next);
                        let error = i64::from(target) - i64::from(reconstructed);
                        let error = (error * error) as u64;
                        if error < chosen_cost {
                            chosen = nibble;
                            chosen_cost = error;
                            chosen_state = next;
                        }
                    }
                    state = chosen_state;
                    cost += chosen_cost;
                    trial[1 + i / 2] |= (chosen as u8 & 15) << if i & 1 == 0 { 4 } else { 0 };
                }
                if cost < best_cost {
                    best_cost = cost;
                    best = trial;
                    best_history = state;
                }
            }
        }
        history = best_history;
        if block_index == pcm.len() / 16 - 1 {
            best[0] |= if looped { 3 } else { 1 };
        }
        out.extend_from_slice(&best);
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{dsp::decode_brr, SpcRam};
    #[test]
    fn predictive_encoding_preserves_music_with_much_less_than_one_percent_error() {
        let pcm: Vec<i16> = (0..1024)
            .map(|i| (16000.0 * (i as f64 * std::f64::consts::TAU / 64.0).sin()) as i16)
            .collect();
        let data = encode(&pcm, true);
        let mut ram = SpcRam::default();
        ram.write_wrapping(0x4000, &data);
        let mut history = [0; 2];
        let mut decoded = Vec::new();
        for i in 0..64 {
            decoded.extend_from_slice(&decode_brr(&ram, 0x4000 + i * 9, &mut history));
        }
        let energy: f64 = pcm[16..].iter().map(|&v| f64::from(v).powi(2)).sum();
        let error: f64 = pcm[16..]
            .iter()
            .zip(&decoded[16..])
            .map(|(&a, &b)| f64::from(a - b).powi(2))
            .sum();
        assert!(
            error / energy < 0.0001,
            "encoding SNR must exceed 40 dB: {}",
            10.0 * (energy / error).log10()
        );
        assert!(data.chunks_exact(9).skip(1).any(|b| b[0] & 12 != 0));
        assert_eq!(data[data.len() - 9] & 3, 3);
        assert_eq!(encode(&pcm, false)[data.len() - 9] & 3, 1);
    }
}

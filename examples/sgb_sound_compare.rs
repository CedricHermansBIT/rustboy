//! Compare original one-note test scores on user-supplied SGB sound firmware
//! and RustBoy's own bank. Measures output, not just DSP pitch-register values.
//! No firmware/sample assets are copied into source or the web bundle.
use rustboy_snes_apu::{apu::Apu, firmware::load_sgb_firmware, sgb_player::Player};

fn score(apu: &mut Apu, instrument: u8, fine: u8) {
    apu.bus.ram.write_wrapping(0x2b00, &[0x20, 0x2b]);
    apu.bus.ram.write_wrapping(0x2b20, &[0x30, 0x2b, 0, 0]);
    apu.bus.ram.write_wrapping(
        0x2b30,
        &[0x50, 0x2b, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    );
    apu.bus.ram.write_wrapping(
        0x2b50,
        &[
            0xe7, 20, 0xe5, 192, 0xed, 192, 0xe0, instrument, 0xf4, fine, 96, 0x7f, 0xa4, 0,
        ],
    );
}
fn frequency(samples: &[[i16; 2]]) -> f64 {
    let pcm: Vec<f64> = samples
        .iter()
        .skip(1500)
        .take(4096)
        .map(|s| (f64::from(s[0]) + f64::from(s[1])) / 2.0)
        .collect();
    let mean = pcm.iter().sum::<f64>() / pcm.len() as f64;
    let pcm: Vec<f64> = pcm.iter().map(|v| v - mean).collect();
    let mut correlation = vec![0.; 801];
    for (lag, c) in correlation.iter_mut().enumerate().skip(20) {
        let mut product = 0.;
        let mut a = 0.;
        let mut b = 0.;
        for i in 0..pcm.len() - lag {
            product += pcm[i] * pcm[i + lag];
            a += pcm[i] * pcm[i];
            b += pcm[i + lag] * pcm[i + lag];
        }
        *c = product / (a * b).sqrt().max(1.);
    }
    let maximum = correlation.iter().copied().fold(0., f64::max);
    if maximum < 0.5 {
        return 0.;
    }
    for lag in 21..800 {
        let c = correlation[lag];
        if c >= maximum * 0.97 && c >= correlation[lag - 1] && c > correlation[lag + 1] {
            let offset = 0.5 * (correlation[lag - 1] - correlation[lag + 1])
                / (correlation[lag - 1] - 2. * c + correlation[lag + 1]);
            return 32000. / (lag as f64 + offset);
        }
    }
    0.
}
fn rms(samples: &[[i16; 2]]) -> f64 {
    (samples
        .iter()
        .skip(1500)
        .flat_map(|v| v.iter())
        .map(|&v| f64::from(v).powi(2))
        .sum::<f64>()
        / (samples.len().saturating_sub(1500) * 2).max(1) as f64)
        .sqrt()
        / 32768.
}
fn wav(path: &std::path::Path, samples: &[[i16; 2]]) -> std::io::Result<()> {
    let size = (samples.len() * 4) as u32;
    let mut bytes = b"RIFF".to_vec();
    bytes.extend_from_slice(&(36 + size).to_le_bytes());
    bytes.extend_from_slice(b"WAVEfmt ");
    bytes.extend_from_slice(&16u32.to_le_bytes());
    bytes.extend_from_slice(&1u16.to_le_bytes());
    bytes.extend_from_slice(&2u16.to_le_bytes());
    bytes.extend_from_slice(&32000u32.to_le_bytes());
    bytes.extend_from_slice(&128000u32.to_le_bytes());
    bytes.extend_from_slice(&4u16.to_le_bytes());
    bytes.extend_from_slice(&16u16.to_le_bytes());
    bytes.extend_from_slice(b"data");
    bytes.extend_from_slice(&size.to_le_bytes());
    for sample in samples {
        for channel in sample {
            bytes.extend_from_slice(&channel.to_le_bytes());
        }
    }
    std::fs::write(path, bytes)
}
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut args = std::env::args().skip(1);
    let firmware = std::fs::read(
        args.next()
            .ok_or("Usage: sgb_sound_compare LOCAL_SGB_FIRMWARE [EXISTING_OUTPUT_DIRECTORY]")?,
    )?;
    let directory = args.next().map(std::path::PathBuf::from);
    let check = std::env::var("RUSTBOY_COMPARE_ASSERT").as_deref() == Ok("1");
    println!("id,original_hz,replacement_hz,difference_semitones,original_rms,replacement_rms");
    for (id, fine) in [
        (0, 0),
        (2, 0),
        (7, 0),
        (11, 13),
        (15, 20),
        (19, 40),
        (22, 0),
        (27, 0),
        (30, 80),
        (32, 170),
        (34, 165),
        (38, 55),
        (40, 0),
        (41, 20),
        (45, 40),
        (47, 0),
        (48, 53),
        (49, 0),
        (50, 0),
        (51, 0),
        (52, 0),
        (53, 0),
        (54, 10),
        (55, 0),
    ] {
        let mut original = load_sgb_firmware(&firmware)?;
        // Let the resident driver's global unmute fade settle before comparing
        // source levels; otherwise transient master gain biases calibration.
        original.run(1_490_944);
        original.drain_samples();
        score(&mut original, id, fine);
        original.bus.input = [1, 0, 0, 0];
        if id < 49 {
            original.run(1_228_800);
            original.drain_samples();
        }
        original.run(250_000);
        let reference = original.drain_samples();
        let mut own = Apu::default();
        let mut player = Player::initialize(&mut own);
        score(&mut own, id, fine);
        player.command(&mut own, [0, 0, 0, 1]);
        if id < 49 {
            for _ in 0..1_228_800 {
                player.clock(&mut own);
                own.run(1);
            }
            own.drain_samples();
        }
        for _ in 0..250_000 {
            player.clock(&mut own);
            own.run(1);
        }
        let replacement = own.drain_samples();
        let a = frequency(&reference);
        let b = frequency(&replacement);
        if check && id < 49 {
            assert!(
                a > 0. && b > 0. && (12. * (b / a).log2()).abs() < 0.1,
                "instrument {id}: octave/tuning mismatch ({a} vs {b} Hz)"
            );
            let ratio = rms(&replacement) / rms(&reference);
            assert!(
                (0.5..2.0).contains(&ratio),
                "instrument {id}: level mismatch {ratio}"
            );
        }
        println!(
            "{id},{a:.2},{b:.2},{:.2},{:.5},{:.5}",
            if a > 0. && b > 0. {
                12. * (b / a).log2()
            } else {
                0.
            },
            rms(&reference),
            rms(&replacement)
        );
        if let Some(directory) = &directory {
            wav(
                &directory.join(format!("instrument-{id:02}-original.wav")),
                &reference,
            )?;
            wav(
                &directory.join(format!("instrument-{id:02}-rustboy.wav")),
                &replacement,
            )?;
        }
    }
    Ok(())
}

//! RustBoy's eight-voice BRR sample renderer. No instrument data is embedded.
use crate::SpcRam;

const PERIODS: [u32; 32] = [
    0, 2048, 1536, 1280, 1024, 768, 640, 512, 384, 320, 256, 192, 160, 128, 96, 80, 64, 48, 40, 32,
    24, 20, 16, 12, 10, 8, 6, 5, 4, 3, 2, 1,
];

#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct Voice {
    address: u16,
    loop_address: u16,
    block: [i16; 16],
    previous: [i32; 2],
    index: u8,
    phase: u32,
    header: u8,
    envelope: u16,
    stage: u8, // attack, decay, sustain, release
    active: bool,
    delay: u8,
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn brr_signed_nibbles_invalid_ranges_and_address_wrap() {
        let mut ram = SpcRam::default();
        ram.write_wrapping(0xfffc, &[0xc0, 0x7f, 0x18, 0, 0, 0, 0, 0, 0]);
        let block = decode_brr(&ram, 0xfffc, &mut [0; 2]);
        assert_eq!(&block[..4], &[28672, -4096, 4096, -32768]);
        ram.write(0xfffc, 0xf0);
        let block = decode_brr(&ram, 0xfffc, &mut [0; 2]);
        assert_eq!(&block[..4], &[0, -4096, 0, -4096]);
    }
    #[test]
    fn looping_voice_is_audible_and_keyoff_releases_without_touching_samples() {
        let mut ram = SpcRam::default();
        ram.write_wrapping(0x100, &[0, 2, 0, 2]);
        ram.write_wrapping(
            0x200,
            &[0xa3, 0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0],
        );
        let mut dsp = Dsp::default();
        for (address, value) in [
            (0, 127),
            (1, 127),
            (2, 0),
            (3, 16),
            (4, 0),
            (5, 0),
            (7, 127),
            (0x0c, 127),
            (0x1c, 127),
            (0x5d, 1),
            (0x6c, 32),
            (0x4c, 1),
        ] {
            dsp.write(address, value);
        }
        let output: Vec<_> = (0..64).map(|_| dsp.sample(&mut ram)).collect();
        assert!(output.iter().any(|s| s[0] != 0));
        assert!(output.iter().all(|s| s[0] == s[1]));
        assert_eq!(dsp.read(0x7c) & 1, 1);
        assert_eq!(ram.read(0x200), 0xa3);
        dsp.write(0x5c, 1);
        for _ in 0..300 {
            dsp.sample(&mut ram);
        }
        assert_eq!(dsp.sample(&mut ram), [0, 0]);
        assert_eq!(dsp.read(8), 0);
    }
}

/// Decode one nine-byte BRR block, retaining the two-sample filter history.
pub fn decode_brr(ram: &SpcRam, address: u16, history: &mut [i32; 2]) -> [i16; 16] {
    let header = ram.read(address);
    let shift = header >> 4;
    let mut output = [0; 16];
    for (index, sample) in output.iter_mut().enumerate() {
        let byte = ram.read(address.wrapping_add(1 + index as u16 / 2));
        let nibble = if index & 1 == 0 { byte >> 4 } else { byte & 15 };
        *sample = reconstruct_brr(
            ((nibble as i8) << 4) >> 4,
            shift,
            (header >> 2) & 3,
            history,
        );
    }
    output
}

pub(crate) fn brr_prediction(filter: u8, history: [i32; 2]) -> i32 {
    let [p1, p2] = history;
    match filter {
        0 => 0,
        1 => p1 + ((-p1) >> 4),
        2 => 2 * p1 + ((-3 * p1) >> 5) - p2 + (p2 >> 4),
        _ => 2 * p1 + ((-13 * p1) >> 6) - p2 + ((3 * p2) >> 4),
    }
}
pub(crate) fn reconstruct_brr(nibble: i8, shift: u8, filter: u8, history: &mut [i32; 2]) -> i16 {
    let value = if shift <= 12 {
        (i32::from(nibble) << shift) >> 1
    } else if nibble < 0 {
        -2048
    } else {
        0
    };
    let decoded =
        ((value + brr_prediction(filter, *history)).clamp(-32768, 32767) as i16).wrapping_mul(2);
    *history = [i32::from(decoded) >> 1, history[0]];
    decoded
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Dsp {
    registers: [u8; 128],
    voices: [Voice; 8],
    counter: u32,
    noise: u16,
    echo_position: u16,
    echo_history: [[i16; 2]; 8],
    echo_index: u8,
}

impl Default for Dsp {
    fn default() -> Self {
        let mut registers = [0; 128];
        registers[0x6c] = 0xe0;
        Self {
            registers,
            voices: std::array::from_fn(|_| Voice::default()),
            counter: 0,
            noise: 0x4000,
            echo_position: 0,
            echo_history: [[0; 2]; 8],
            echo_index: 0,
        }
    }
}
crate::state::snapshot!(
    Voice,
    address,
    loop_address,
    block,
    previous,
    index,
    phase,
    header,
    envelope,
    stage,
    active,
    delay
);
crate::state::snapshot!(
    Dsp,
    registers,
    voices,
    counter,
    noise,
    echo_position,
    echo_history,
    echo_index
);

impl Dsp {
    pub(crate) fn validate_state(&self) -> Result<(), &'static str> {
        if self.echo_index >= 8
            || self.echo_position >= 30720
            || self.noise >= 32768
            || self.voices.iter().any(|v| {
                v.index >= 16
                    || v.phase >= 4096
                    || v.envelope > 2047
                    || v.stage > 3
                    || v.delay > 5
                    || v.previous.iter().any(|p| !(-16384..=16383).contains(p))
            })
        {
            return Err("Invalid DSP state");
        }
        Ok(())
    }
    pub fn read(&self, register: u8) -> u8 {
        self.registers[usize::from(register & 127)]
    }
    pub fn write(&mut self, register: u8, value: u8) {
        if register >= 128 {
            return;
        }
        let index = usize::from(register);
        if register & 15 == 8 || register & 15 == 9 {
            return;
        }
        self.registers[index] = if register == 0x7c { 0 } else { value };
        if register == 0x4c {
            for i in 0..8 {
                if value & (1 << i) != 0 {
                    self.voices[i].delay = 5;
                }
            }
        }
        if register == 0x5c {
            for i in 0..8 {
                if value & (1 << i) != 0 {
                    self.voices[i].stage = 3;
                }
            }
        }
    }
    fn due(counter: u32, rate: u8) -> bool {
        let period = PERIODS[usize::from(rate & 31)];
        period != 0 && counter % period == 0
    }
    fn envelope(voice: &mut Voice, adsr: u8, sustain: u8, gain: u8, counter: u32) {
        if voice.stage == 3 {
            voice.envelope = voice.envelope.saturating_sub(8);
            return;
        }
        if adsr & 128 != 0 {
            let rate = match voice.stage {
                0 => (adsr & 15) * 2 + 1,
                1 => ((adsr >> 4) & 7) * 2 + 16,
                _ => sustain & 31,
            };
            if !Self::due(counter, rate) {
                return;
            }
            if voice.stage == 0 {
                voice.envelope = voice
                    .envelope
                    .saturating_add(if rate == 31 { 1024 } else { 32 });
                if voice.envelope >= 2047 {
                    voice.envelope = 2047;
                    voice.stage = 1;
                }
            } else {
                voice.envelope = voice
                    .envelope
                    .saturating_sub(((voice.envelope.saturating_sub(1)) >> 8) + 1);
                if voice.stage == 1 && voice.envelope <= (u16::from(sustain >> 5) + 1) * 256 {
                    voice.stage = 2;
                }
            }
        } else if gain & 128 == 0 {
            voice.envelope = u16::from(gain) * 16;
        } else if Self::due(counter, gain & 31) {
            voice.envelope = match (gain >> 5) & 3 {
                0 => voice.envelope.saturating_sub(32),
                1 => voice
                    .envelope
                    .saturating_sub(((voice.envelope.saturating_sub(1)) >> 8) + 1),
                2 => voice.envelope.saturating_add(32).min(2047),
                _ => voice
                    .envelope
                    .saturating_add(if voice.envelope < 1536 { 32 } else { 8 })
                    .min(2047),
            };
        }
    }
    fn word(ram: &SpcRam, address: u16) -> u16 {
        u16::from_le_bytes([ram.read(address), ram.read(address.wrapping_add(1))])
    }
    fn next(voice: &mut Voice, ram: &SpcRam, ended: &mut bool) {
        voice.index += 1;
        if voice.index < 16 {
            return;
        }
        voice.index = 0;
        if voice.header & 1 != 0 {
            *ended = true;
            if voice.header & 2 == 0 {
                voice.active = false;
                voice.envelope = 0;
                return;
            }
            voice.address = voice.loop_address;
        } else {
            voice.address = voice.address.wrapping_add(9);
        }
        voice.header = ram.read(voice.address);
        voice.block = decode_brr(ram, voice.address, &mut voice.previous);
    }
    /// One stereo frame at 32 kHz. SPC ports/register accesses are clocked by
    /// the owner; this renderer never consults wall-clock or browser state.
    pub fn sample(&mut self, ram: &mut SpcRam) -> [i16; 2] {
        self.counter = self.counter.wrapping_add(1);
        if Self::due(self.counter, self.registers[0x6c] & 31) {
            self.noise = (self.noise >> 1) | (((self.noise ^ (self.noise >> 1)) & 1) << 14);
        }
        let mut mixed = [0i32; 2];
        let mut echo_send = [0i32; 2];
        let mut previous_output = 0;
        for i in 0..8 {
            let base = i * 16;
            let voice = &mut self.voices[i];
            if self.registers[0x6c] & 128 != 0 {
                voice.stage = 3;
                voice.envelope = 0;
            }
            if voice.delay != 0 {
                voice.delay -= 1;
                if voice.delay == 0 {
                    let entry = (u16::from(self.registers[0x5d]) << 8)
                        .wrapping_add(u16::from(self.registers[base + 4]) * 4);
                    *voice = Voice {
                        address: Self::word(ram, entry),
                        loop_address: Self::word(ram, entry.wrapping_add(2)),
                        active: true,
                        ..Voice::default()
                    };
                    voice.header = ram.read(voice.address);
                    voice.block = decode_brr(ram, voice.address, &mut voice.previous);
                    self.registers[0x7c] &= !(1 << i);
                }
                self.registers[base + 8] = 0;
                self.registers[base + 9] = 0;
                previous_output = 0;
                continue;
            }
            if !voice.active {
                previous_output = 0;
                continue;
            }
            Self::envelope(
                voice,
                self.registers[base + 5],
                self.registers[base + 6],
                self.registers[base + 7],
                self.counter,
            );
            let index = usize::from(voice.index);
            // Linear interpolation is the initial renderer; hardware Gaussian
            // interpolation and intra-sample DSP register timing remain separate.
            let current = i32::from(voice.block[index]);
            let next = i32::from(voice.block[(index + 1).min(15)]);
            let sample = if self.registers[0x3d] & (1 << i) != 0 {
                i32::from((self.noise << 1) as i16)
            } else {
                current + ((next - current) * voice.phase as i32 >> 12)
            };
            let output = (sample * i32::from(voice.envelope) >> 11) & !1;
            self.registers[base + 8] = (voice.envelope >> 4) as u8;
            self.registers[base + 9] = (output >> 8) as u8;
            let mut pitch = u32::from(self.registers[base + 2])
                | (u32::from(self.registers[base + 3] & 63) << 8);
            if i != 0 && self.registers[0x2d] & (1 << i) != 0 {
                pitch = ((pitch as i32 * (previous_output + 32768)) >> 15).clamp(0, 16383) as u32;
            }
            previous_output = output;
            for channel in 0..2 {
                let value = (output * i32::from(self.registers[base + channel] as i8)) >> 7;
                mixed[channel] = (mixed[channel] + value).clamp(-32768, 32767);
                if self.registers[0x4d] & (1 << i) != 0 {
                    echo_send[channel] = (echo_send[channel] + value).clamp(-32768, 32767);
                }
            }
            voice.phase += pitch;
            while voice.phase >= 4096 {
                voice.phase -= 4096;
                let mut ended = false;
                Self::next(voice, ram, &mut ended);
                if ended {
                    self.registers[0x7c] |= 1 << i;
                }
            }
        }
        let address = (u16::from(self.registers[0x6d]) << 8).wrapping_add(self.echo_position);
        let history_index = usize::from(self.echo_index);
        self.echo_history[history_index] = [
            Self::word(ram, address) as i16,
            Self::word(ram, address.wrapping_add(2)) as i16,
        ];
        self.echo_index = (self.echo_index + 1) & 7;
        let mut filtered = [0i32; 2];
        for tap in 0..8 {
            let entry = self.echo_history[(history_index + 8 - tap) & 7];
            let coefficient = i32::from(self.registers[tap * 16 + 15] as i8);
            for channel in 0..2 {
                filtered[channel] += (i32::from(entry[channel]) * coefficient) >> 7;
            }
        }
        let mut output = [0; 2];
        for channel in 0..2 {
            let echo = filtered[channel].clamp(-32768, 32767);
            let value = (mixed[channel] * i32::from(self.registers[0x0c + channel * 16] as i8)
                >> 7)
                + (echo * i32::from(self.registers[0x2c + channel * 16] as i8) >> 7);
            output[channel] = if self.registers[0x6c] & 64 != 0 {
                0
            } else {
                value.clamp(-32768, 32767) as i16
            };
            if self.registers[0x6c] & 32 == 0 {
                let feedback = (echo_send[channel]
                    + (echo * i32::from(self.registers[0x0d] as i8) >> 7))
                    .clamp(-32768, 32767) as i16
                    & !1;
                let dest = address.wrapping_add(channel as u16 * 2);
                let bytes = feedback.to_le_bytes();
                ram.write(dest, bytes[0]);
                ram.write(dest.wrapping_add(1), bytes[1]);
            }
        }
        let length = u16::from(self.registers[0x7d] & 15) * 2048;
        self.echo_position = if self.echo_position + 4 >= length.max(4) {
            0
        } else {
            self.echo_position + 4
        };
        output
    }
}

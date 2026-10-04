//! Original GBA PSG from documented registers and oscillator rates. No external
//! emulator code. Functional digital synthesis; analog DAC/high-pass response,
//! obscure live-envelope quirks and sub-instruction register timing are pending.
pub(crate) const STATE_BYTES: usize = 115;
const LENGTH: [usize; 4] = [0x62, 0x68, 0x72, 0x78];
const CONTROL: [usize; 4] = [0x64, 0x6c, 0x74, 0x7c];
const DUTY: [u8; 4] = [0x80, 0x81, 0xe1, 0x7e];
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
struct Voice {
    length: u16,
    remaining: u32,
    frequency: u16,
    position: u8,
    volume: u8,
    envelope_timer: u8,
    envelope_running: bool,
    lfsr: u16,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{bus::Bus, cpu::Memory};
    fn bus() -> Bus {
        let mut b = Bus::new(vec![0; 192], None).unwrap();
        b.write16(0x04000084, 0x80, false);
        b
    }
    fn write(b: &mut Bus, at: usize, value: u16) {
        b.write16(0x04000000 + at as u32, value, false);
    }
    fn pulse(b: &mut Bus, n: usize, frequency: u16, envelope: u8) {
        write(b, LENGTH[n], (u16::from(envelope) << 8) | 0x80);
        write(b, CONTROL[n], 0x8000 | frequency);
    }
    fn clock(b: &mut Bus, mut clocks: u64, chunk: u64) {
        while clocks != 0 {
            let advance = clocks.min(chunk).min(b.apu.deadline());
            b.clock_audio(advance);
            clocks -= advance;
        }
    }
    #[test]
    fn both_pulses_use_gba_clock_period_and_all_duty_ratios() {
        let mut b = bus();
        for (n, length) in LENGTH.iter().copied().enumerate().take(2) {
            pulse(&mut b, n, 1024, 0xf0);
            b.apu.psg.clock(16383, &mut b.io);
            assert_eq!(b.apu.psg.voices[n].position, 0);
            assert_eq!(b.apu.psg.voices[n].remaining, 1);
            b.apu.psg.clock(1, &mut b.io);
            assert_eq!(b.apu.psg.voices[n].position, 1);
            for (duty, highs) in [1, 2, 4, 6].into_iter().enumerate() {
                b.io[length] = (duty as u8) << 6;
                let mut count = 0;
                for step in 0..8 {
                    b.apu.psg.voices[n].position = step;
                    count += usize::from(b.apu.psg.level(n, &b.io) > 0);
                }
                assert_eq!(count, highs);
            }
            b.apu.psg.reset();
        }
    }
    #[test]
    fn length_stops_all_four_channels_and_status_is_read_only() {
        for n in 0..4 {
            let mut b = bus();
            if n == 2 {
                write(&mut b, 0x70, 0x80);
            }
            write(&mut b, LENGTH[n], if n == 2 { 0x20ff } else { 0xf03f });
            write(&mut b, CONTROL[n], 0xc000);
            assert_eq!(b.peek8(0x04000084), 0x80 | (1 << n));
            clock(&mut b, 32767, 512);
            assert_eq!(b.apu.psg.active, 1 << n);
            clock(&mut b, 1, 512);
            assert_eq!(b.peek8(0x04000084), 0x80);
            write(&mut b, 0x84, 0x8f);
            assert_eq!(b.peek8(0x04000084), 0x80);
        }
    }
    #[test]
    fn envelope_runs_at_64hz_and_saturation_does_not_stop_voice() {
        let mut b = bus();
        pulse(&mut b, 1, 1000, 0xe9);
        clock(&mut b, 32768 * 8 - 1, 512);
        assert_eq!(b.apu.psg.voices[1].volume, 14);
        clock(&mut b, 1, 512);
        assert_eq!(b.apu.psg.voices[1].volume, 15);
        clock(&mut b, 32768 * 16, 512);
        assert_eq!(b.apu.psg.voices[1].volume, 15);
        assert_eq!(b.apu.psg.active, 2);
        pulse(&mut b, 1, 1000, 0x10);
        clock(&mut b, 32768 * 16, 512);
        assert_eq!(b.apu.psg.voices[1].volume, 1);
        write(&mut b, 0x68, 0);
        assert_eq!(b.apu.psg.active, 0, "DAC off stops output immediately");
    }
    #[test]
    fn sweep_checks_initial_and_second_overflow_and_negate_clear() {
        let mut b = bus();
        write(&mut b, 0x60, 0x11);
        pulse(&mut b, 0, 1500, 0xf0);
        assert_eq!(b.apu.psg.active, 0, "initial overflow");
        pulse(&mut b, 0, 1000, 0xf0);
        clock(&mut b, 32768 * 3, 512);
        assert_eq!(b.apu.psg.voices[0].frequency, 1500);
        assert_eq!(b.apu.psg.active, 0, "second overflow after valid update");
        // Restarting a voice does not reset the shared frame sequencer.
        let mut b = bus();
        write(&mut b, 0x60, 0x19);
        pulse(&mut b, 0, 1000, 0xf0);
        clock(&mut b, 32768 * 3, 512);
        assert_eq!(b.apu.psg.voices[0].frequency, 500);
        assert_eq!(word(&b.io, 0x64) & 0x7ff, 500);
        assert_eq!(b.peek16(0x04000064), 0, "frequency is write-only");
        write(&mut b, 0x60, 0x11);
        assert_eq!(b.apu.psg.active, 0, "negate-clear quirk");
    }
    #[test]
    fn wave_accesses_opposite_bank_rotates_and_switches_after_32_digits() {
        let mut b = bus();
        b.write8(0x04000090, 0xab); // CPU writes bank 1.
        write(&mut b, 0x70, 0xe0); // Play bank 1, two-bank mode.
        b.write8(0x04000090, 0x12); // CPU now writes bank 0.
        write(&mut b, 0x72, 0x2000);
        write(&mut b, 0x74, 0x87ff);
        b.apu.psg.clock(8, &mut b.io);
        assert_eq!(b.apu.psg.voices[2].volume, 10);
        assert_eq!(b.apu.psg.wave[16], 0xb0);
        assert_eq!(b.peek8(0x04000090), 0x12);
        b.apu.psg.clock(8 * 31, &mut b.io);
        assert_eq!(b.io[0x70] & 0x40, 0);
        assert_eq!(b.peek8(0x04000090), 0xab);
        b.apu.psg.clock(8, &mut b.io);
        assert_eq!(b.apu.psg.voices[2].volume, 1);
        b.apu.psg.voices[2].volume = 15;
        for (volume, output) in [
            (0, 0),
            (0x20, 15),
            (0x40, 7),
            (0x60, 3),
            (0x80, 11),
            (0xe0, 11),
        ] {
            b.io[0x73] = volume;
            assert_eq!(b.apu.psg.level(2, &b.io), output);
        }
    }
    #[test]
    fn noise_periods_and_polynomial_cycles_cover_both_widths() {
        let mut b = bus();
        for ratio in 0..8 {
            for shift in [0, 1, 7, 15] {
                b.io[0x7c] = ratio | (shift << 4);
                let base = if ratio == 0 {
                    32
                } else {
                    u32::from(ratio) * 64
                };
                assert_eq!(b.apu.psg.period(3, &b.io), base << shift);
            }
        }
        for (width, period, mask) in [(0, 32767, 0x7fff), (8, 127, 0x7f)] {
            b.io[0x7c] = width;
            b.apu.psg.voices[3].lfsr = 0x7fff;
            for step in 1..=period {
                b.apu.psg.oscillator_edge(3, &mut b.io);
                assert_eq!(b.apu.psg.voices[3].lfsr & mask == mask, step == period);
            }
        }
    }
    #[test]
    fn master_off_resets_psg_but_preserves_wave_and_direct_sound_controls() {
        let mut b = bus();
        pulse(&mut b, 0, 1000, 0xf0);
        b.write8(0x04000090, 0xab);
        write(&mut b, 0x82, 0x330e);
        write(&mut b, 0x84, 0);
        assert_eq!(b.apu.psg.active, 0);
        assert_eq!(b.peek8(0x04000090), 0xab);
        assert_eq!(b.peek16(0x04000082), 0x330e);
        pulse(&mut b, 0, 1000, 0xf0);
        assert_eq!(b.peek16(0x04000062), 0);
        assert_eq!(b.apu.psg.active, 0);
    }
    #[test]
    fn enabling_length_between_sequencer_ticks_clocks_and_retrigger_reloads() {
        let mut b = bus();
        write(&mut b, 0x68, 0xf0bf); // Length 1, duty 50%.
        write(&mut b, 0x6c, 0x8000);
        clock(&mut b, 32768, 512); // Next sequencer step is not a length tick.
        write(&mut b, 0x6c, 0x4000);
        assert_eq!(b.apu.psg.voices[1].length, 0);
        assert_eq!(b.apu.psg.active, 0);
        write(&mut b, 0x6c, 0xc000);
        assert_eq!(b.apu.psg.voices[1].length, 63);
        assert_eq!(b.apu.psg.active, 2);
    }
    #[test]
    fn integrated_psg_routing_and_direct_sound_share_dac() {
        let mut b = bus();
        pulse(&mut b, 1, 0, 0xf0);
        write(&mut b, 0x80, 0x2077); // Pulse 2 left only, full master volume.
        write(&mut b, 0x82, 0x0306); // Full PSG and FIFO A, both FIFO sides.
        b.apu.fifo[0].held = 64;
        b.clock_audio(256);
        write(&mut b, 0x80, 0x0277); // Switch pulse 2 to right halfway through.
        b.clock_audio(256);
        assert_eq!(b.apu.samples.pop_front(), Some([316. / 512.; 2]));
        assert_eq!(b.apu.psg.integral, [0; 2]);
    }
    #[test]
    fn fine_and_batched_advancement_have_identical_pcm_and_state() {
        let configure = || {
            let mut b = bus();
            pulse(&mut b, 0, 1900, 0xa1);
            pulse(&mut b, 1, 2000, 0xf0);
            write(&mut b, 0x78, 0x7800);
            write(&mut b, 0x7c, 0x8019);
            write(&mut b, 0x80, 0xff77);
            write(&mut b, 0x82, 2);
            b
        };
        let mut a = configure();
        let mut b = configure();
        clock(&mut a, 32768 * 10 + 257, 512);
        clock(&mut b, 32768 * 10 + 257, 7);
        assert_eq!(a.apu.psg, b.apu.psg);
        assert_eq!(a.apu.samples, b.apu.samples);
        assert!(a.apu.samples.iter().any(|sample| sample[0] != 0.));
        assert_eq!(Psg::decode(&a.apu.psg.encode()).unwrap(), a.apu.psg);
    }
    #[test]
    fn malformed_psg_states_are_rejected_without_panics() {
        let good = Psg::default().encode();
        for (at, value) in [(0, 255), (11, 2), (56, 1), (58, 128), (59, 8), (63, 2)] {
            let mut invalid = good.clone();
            invalid[at] = value;
            assert!(Psg::decode(&invalid).is_err(), "byte {at}");
        }
        assert!(Psg::decode(&good[..100]).is_err());
    }
    #[test]
    fn deferred_wave_changes_are_visible_to_peeks_and_cpu_reads_before_output_tick() {
        let mut b = bus();
        b.write8(0x04000090, 0xab);
        write(&mut b, 0x70, 0xe0);
        write(&mut b, 0x72, 0x2000);
        write(&mut b, 0x74, 0x87ff);
        b.clock_audio(256); // 32 digits: switch banks, without an output sample.
        assert_eq!(b.apu.psg.pending, 256);
        assert_eq!(b.io[0x70], 0xe0, "canonical work is deferred");
        let state = b.apu.psg.encode();
        assert_eq!(b.peek8(0x04000070), 0xa0);
        assert_eq!(b.peek8(0x04000090), 0xab);
        assert_eq!(b.apu.psg.encode(), state, "debugger did not mutate state");
        assert_eq!(b.read16(0x04000070, false), 0xa0);
        assert_eq!(b.apu.psg.pending, 0);
        assert_eq!(b.io[0x70], 0xa0);
        b.write8(0x04000090, 0x56);
        assert_eq!(b.apu.psg.wave[16], 0x56);
    }
}
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub(crate) struct Psg {
    voices: [Voice; 4],
    pub active: u8,
    frame_phase: u16,
    frame_step: u8,
    sweep_shadow: u16,
    sweep_timer: u8,
    sweep_enabled: bool,
    sweep_negated: bool,
    pub wave: [u8; 32],
    // Box-filter the digital PSG between 32-kHz output samples. Routing/volume
    // changes within a sample interval affect only the following segment.
    integral: [i64; 2],
    pub pending: u16,
}
fn word(io: &[u8], at: usize) -> u16 {
    u16::from_le_bytes([io[at], io[at + 1]])
}
impl Psg {
    pub fn wave_index(io: &[u8], offset: usize) -> usize {
        (((io[0x70] >> 6) ^ 1) as usize & 1) * 16 + offset - 0x90
    }
    pub fn reset(&mut self) {
        let wave = self.wave;
        *self = Self {
            wave,
            ..Self::default()
        };
    }
    fn period(&self, n: usize, io: &[u8]) -> u32 {
        if n == 3 {
            let ratio = io[0x7c] & 7;
            let divisor = if ratio == 0 {
                32
            } else {
                u32::from(ratio) * 64
            };
            divisor << (io[0x7c] >> 4)
        } else {
            u32::from(2048 - self.voices[n].frequency) * if n == 2 { 8 } else { 16 }
        }
    }
    fn sweep_next(&mut self, io: &[u8]) -> Option<u16> {
        let delta = self.sweep_shadow >> (io[0x60] & 7);
        let next = if io[0x60] & 8 != 0 {
            self.sweep_negated = true;
            self.sweep_shadow - delta
        } else {
            self.sweep_shadow + delta
        };
        if next > 2047 {
            self.active &= !1;
            None
        } else {
            Some(next)
        }
    }
    pub fn write(&mut self, offset: usize, old: u8, io: &mut [u8]) {
        if (0x90..0xa0).contains(&offset) {
            self.wave[Self::wave_index(io, offset)] = io[offset];
            return;
        }
        if !(0x60..0x82).contains(&offset) {
            return;
        }
        if io[0x84] & 0x80 == 0 {
            io[offset] = 0;
            return;
        }
        if offset == 0x60 && old & 8 != 0 && io[offset] & 8 == 0 && self.sweep_negated {
            self.active &= !1;
        }
        if offset == 0x70 && io[offset] & 0x80 == 0 {
            self.active &= !4;
        }
        for n in 0..4 {
            if offset == LENGTH[n] {
                let maximum = if n == 2 { 256 } else { 64 };
                let mask = if n == 2 { 255 } else { 63 };
                self.voices[n].length = maximum - u16::from(io[offset] & mask);
            }
            if n != 2 && offset == LENGTH[n] + 1 && io[offset] & 0xf8 == 0 {
                self.active &= !(1 << n);
            }
            if n != 3 && (offset == CONTROL[n] || offset == CONTROL[n] + 1) {
                self.voices[n].frequency = word(io, CONTROL[n]) & 0x7ff;
            }
            if offset != CONTROL[n] + 1 {
                continue;
            }
            let length_enabled = io[offset] & 0x40 != 0;
            // Extra length clock when enabling between length-sequencer ticks.
            if old & 0x40 == 0
                && length_enabled
                && self.frame_step & 1 != 0
                && self.voices[n].length != 0
            {
                self.voices[n].length -= 1;
                if self.voices[n].length == 0 {
                    self.active &= !(1 << n);
                }
            }
            if io[offset] & 0x80 == 0 {
                continue;
            }
            io[offset] &= !0x80; // Write-only restart strobe.
            let v = &mut self.voices[n];
            if v.length == 0 {
                v.length = if n == 2 { 256 } else { 64 };
                if length_enabled && self.frame_step & 1 != 0 {
                    v.length -= 1;
                }
            }
            let envelope = io[LENGTH[n] + 1];
            v.volume = envelope >> 4;
            v.envelope_timer = (envelope & 7).max(1);
            v.envelope_running = envelope & 7 != 0;
            // Pulse duty phase survives retrigger; wave and noise restart.
            if n == 2 {
                v.position = 0;
                v.volume = 0;
            }
            if n == 3 {
                v.lfsr = 0x7fff;
                v.position = 0;
            }
            self.voices[n].remaining = self.period(n, io);
            let dac = if n == 2 {
                io[0x70] & 0x80 != 0
            } else {
                envelope & 0xf8 != 0
            };
            if dac {
                self.active |= 1 << n;
            } else {
                self.active &= !(1 << n);
            }
            if n == 0 {
                self.sweep_shadow = self.voices[0].frequency;
                let time = (io[0x60] >> 4) & 7;
                self.sweep_timer = if time == 0 { 8 } else { time };
                self.sweep_enabled = io[0x60] & 0x77 != 0;
                self.sweep_negated = false;
                if io[0x60] & 7 != 0 {
                    self.sweep_next(io);
                }
            }
        }
    }
    fn level(&self, n: usize, io: &[u8]) -> i32 {
        let v = &self.voices[n];
        match n {
            0 | 1 => {
                let high = DUTY[(io[LENGTH[n]] >> 6) as usize] >> v.position & 1 != 0;
                if high {
                    i32::from(v.volume)
                } else {
                    -i32::from(v.volume)
                }
            }
            2 => {
                let level = i32::from(v.volume) * 2 - 15;
                let volume = io[0x73];
                if volume & 0x80 != 0 {
                    level * 3 / 4
                } else {
                    match volume >> 5 & 3 {
                        0 => 0,
                        1 => level,
                        2 => level / 2,
                        _ => level / 4,
                    }
                }
            }
            _ => {
                if v.lfsr & 1 == 0 {
                    i32::from(v.volume)
                } else {
                    -i32::from(v.volume)
                }
            }
        }
    }
    fn oscillator_edge(&mut self, n: usize, io: &mut [u8]) {
        let v = &mut self.voices[n];
        if n < 2 {
            v.position = (v.position + 1) & 7;
        } else if n == 2 {
            let start = ((io[0x70] >> 6) as usize & 1) * 16;
            v.volume = self.wave[start] >> 4;
            // GBA wave RAM is a circulating 128-bit shift register, not a
            // read-only pattern addressed by a cursor.
            for i in start..start + 15 {
                self.wave[i] = (self.wave[i] << 4) | (self.wave[i + 1] >> 4);
            }
            self.wave[start + 15] = (self.wave[start + 15] << 4) | v.volume;
            v.position = (v.position + 1) & 31;
            if v.position == 0 && io[0x70] & 0x20 != 0 {
                io[0x70] ^= 0x40;
            }
        } else {
            let feedback = (v.lfsr ^ (v.lfsr >> 1)) & 1;
            v.lfsr = (v.lfsr >> 1) | (feedback << 14);
            if io[0x7c] & 8 != 0 {
                v.lfsr = (v.lfsr & !(1 << 6)) | (feedback << 6);
            }
        }
    }
    fn frame_tick(&mut self, io: &mut [u8]) {
        if self.frame_step & 1 == 0 {
            for (n, v) in self.voices.iter_mut().enumerate() {
                if io[CONTROL[n] + 1] & 0x40 != 0 && v.length != 0 {
                    v.length -= 1;
                    if v.length == 0 {
                        self.active &= !(1 << n);
                    }
                }
            }
        }
        if self.frame_step == 2 || self.frame_step == 6 {
            self.sweep_timer = self.sweep_timer.saturating_sub(1);
            if self.sweep_timer == 0 {
                let time = io[0x60] >> 4 & 7;
                self.sweep_timer = if time == 0 { 8 } else { time };
                if self.sweep_enabled && time != 0 && io[0x60] & 7 != 0 {
                    if let Some(next) = self.sweep_next(io) {
                        self.sweep_shadow = next;
                        self.voices[0].frequency = next;
                        io[0x64] = next as u8;
                        io[0x65] = (io[0x65] & !7) | (next >> 8) as u8;
                        self.sweep_next(io);
                    }
                }
            }
        }
        if self.frame_step == 7 {
            for n in [0, 1, 3] {
                let v = &mut self.voices[n];
                if !v.envelope_running {
                    continue;
                }
                v.envelope_timer = v.envelope_timer.saturating_sub(1);
                if v.envelope_timer != 0 {
                    continue;
                }
                let envelope = io[LENGTH[n] + 1];
                v.envelope_timer = envelope & 7;
                let next = i16::from(v.volume) + if envelope & 8 != 0 { 1 } else { -1 };
                if (0..=15).contains(&next) {
                    v.volume = next as u8;
                } else {
                    v.envelope_running = false;
                }
            }
        }
        self.frame_step = (self.frame_step + 1) & 7;
    }
    pub fn clock(&mut self, mut clocks: u32, io: &mut [u8]) {
        if io[0x84] & 0x80 == 0 {
            return;
        }
        while clocks != 0 {
            let advance = clocks.min(32768 - u32::from(self.frame_phase));
            if self.active != 0 {
                let route = word(io, 0x80);
                let gain_shift = match io[0x82] & 3 {
                    0 => 2,
                    1 => 1,
                    _ => 0,
                };
                for n in 0..4 {
                    if self.active & (1 << n) == 0 {
                        continue;
                    }
                    let mut left = advance;
                    let mut area = 0i64;
                    while left != 0 {
                        let segment = left.min(self.voices[n].remaining);
                        area += i64::from(self.level(n, io)) * i64::from(segment);
                        self.voices[n].remaining -= segment;
                        left -= segment;
                        if self.voices[n].remaining == 0 {
                            self.oscillator_edge(n, io);
                            self.voices[n].remaining = self.period(n, io);
                        }
                    }
                    for side in 0..2 {
                        let bit = if side == 0 { 12 + n } else { 8 + n };
                        let volume = if side == 0 {
                            (route >> 4) & 7
                        } else {
                            route & 7
                        };
                        if route & (1 << bit) != 0 {
                            self.integral[side] += area * i64::from(volume + 1) * (4 >> gain_shift);
                        }
                    }
                }
            }
            self.frame_phase += advance as u16;
            clocks -= advance;
            if self.frame_phase == 32768 {
                self.frame_phase = 0;
                self.frame_tick(io);
            }
        }
    }
    pub fn output(&mut self) -> [i32; 2] {
        std::mem::take(&mut self.integral).map(|sum| (sum / (512 * 4)) as i32)
    }
    pub fn flush(&mut self, io: &mut [u8]) {
        let clocks = std::mem::take(&mut self.pending);
        if clocks != 0 {
            self.clock(u32::from(clocks), io);
        }
    }
    pub fn valid_io(&self, io: &[u8], sample_phase: u16) -> bool {
        (io[0x84] & 0x80 != 0 || (self.active == 0 && self.pending == 0))
            && self.pending <= sample_phase
            && self
                .integral
                .iter()
                .all(|sum| sum.unsigned_abs() <= u64::from(sample_phase) * 480 * 4)
            && self.voices.iter().enumerate().all(|(n, v)| {
                self.active & (1 << n) == 0
                    || ((if n == 2 {
                        io[0x70] & 0x80 != 0
                    } else {
                        io[LENGTH[n] + 1] & 0xf8 != 0
                    }) && (io[CONTROL[n] + 1] & 0x40 == 0 || v.length != 0))
            })
    }
    pub fn encode(&self) -> Vec<u8> {
        let mut data = Vec::with_capacity(STATE_BYTES);
        for v in self.voices {
            data.extend_from_slice(&v.length.to_le_bytes());
            data.extend_from_slice(&v.remaining.to_le_bytes());
            data.extend_from_slice(&v.frequency.to_le_bytes());
            data.extend_from_slice(&[
                v.position,
                v.volume,
                v.envelope_timer,
                u8::from(v.envelope_running),
            ]);
            data.extend_from_slice(&v.lfsr.to_le_bytes());
        }
        data.push(self.active);
        data.extend_from_slice(&self.frame_phase.to_le_bytes());
        data.push(self.frame_step);
        data.extend_from_slice(&self.sweep_shadow.to_le_bytes());
        data.extend_from_slice(&[
            self.sweep_timer,
            u8::from(self.sweep_enabled),
            u8::from(self.sweep_negated),
        ]);
        data.extend_from_slice(&self.wave);
        for integral in self.integral {
            data.extend_from_slice(&integral.to_le_bytes());
        }
        data.extend_from_slice(&self.pending.to_le_bytes());
        debug_assert_eq!(data.len(), STATE_BYTES);
        data
    }
    pub fn decode(data: &[u8]) -> Result<Self, String> {
        if data.len() != STATE_BYTES {
            return Err("Invalid GBA PSG state size".into());
        }
        let mut at = 0;
        let mut integer = |bytes: usize| {
            let mut value = [0; 8];
            value[..bytes].copy_from_slice(&data[at..at + bytes]);
            at += bytes;
            u64::from_le_bytes(value)
        };
        let mut psg = Self::default();
        for (n, v) in psg.voices.iter_mut().enumerate() {
            v.length = integer(2) as u16;
            v.remaining = integer(4) as u32;
            v.frequency = integer(2) as u16;
            v.position = integer(1) as u8;
            v.volume = integer(1) as u8;
            v.envelope_timer = integer(1) as u8;
            let running = integer(1);
            v.envelope_running = running != 0;
            v.lfsr = integer(2) as u16;
            if v.length > if n == 2 { 256 } else { 64 }
                || v.remaining > if n == 3 { 448 << 15 } else { 32768 }
                || v.frequency > 2047
                || v.position > if n == 2 { 31 } else { 7 }
                || v.volume > 15
                || v.envelope_timer > 8
                || running > 1
                || v.lfsr > 0x7fff
            {
                return Err("Invalid GBA PSG voice state".into());
            }
        }
        psg.active = integer(1) as u8;
        psg.frame_phase = integer(2) as u16;
        psg.frame_step = integer(1) as u8;
        psg.sweep_shadow = integer(2) as u16;
        psg.sweep_timer = integer(1) as u8;
        let enabled = integer(1);
        let negated = integer(1);
        psg.sweep_enabled = enabled != 0;
        psg.sweep_negated = negated != 0;
        for byte in &mut psg.wave {
            *byte = integer(1) as u8;
        }
        for sum in &mut psg.integral {
            *sum = integer(8) as i64;
        }
        psg.pending = integer(2) as u16;
        if psg.active > 15
            || psg.frame_phase >= 32768
            || psg.frame_step >= 8
            || psg.pending >= 512
            || psg.sweep_shadow > 2047
            || psg.sweep_timer > 8
            || enabled > 1
            || negated > 1
            || psg
                .integral
                .iter()
                .any(|v| v.unsigned_abs() > 512 * 480 * 4)
            || psg
                .voices
                .iter()
                .enumerate()
                .any(|(n, v)| psg.active & (1 << n) != 0 && v.remaining == 0)
        {
            return Err("Invalid GBA PSG sequencer state".into());
        }
        Ok(psg)
    }
}

//! Original timer-clocked Direct Sound and PSG stereo DAC mixing. Precise
//! analog response and variable-rate PWM filtering remain subsequent work.
use crate::bus::Bus;
use std::collections::VecDeque;

pub(crate) const MAX_SAMPLES: usize = 65536;
#[derive(Clone, Copy, Debug, Default)]
pub(crate) struct Fifo {
    pub data: [i8; 32],
    pub head: u8,
    pub len: u8,
    pub held: i8,
}
impl Fifo {
    fn push(&mut self, value: i8) {
        if self.len < 32 {
            self.data[(self.head as usize + self.len as usize) & 31] = value;
            self.len += 1;
        }
    }
    fn clock(&mut self, ticks: u64) {
        let take = ticks.min(u64::from(self.len));
        for _ in 0..take {
            self.held = self.data[self.head as usize];
            self.head = (self.head + 1) & 31;
            self.len -= 1;
        }
        if ticks > take {
            self.held = 0;
        }
    }
}
pub(crate) struct Apu {
    pub psg: crate::psg::Psg,
    pub fifo: [Fifo; 2],
    pub phase: u16,
    pub samples: VecDeque<[f32; 2]>,
}
impl Default for Apu {
    fn default() -> Self {
        Self {
            psg: crate::psg::Psg::default(),
            fifo: [Fifo::default(); 2],
            phase: 0,
            samples: VecDeque::with_capacity(1024),
        }
    }
}
impl Apu {
    pub fn deadline(&self) -> u64 {
        512 - u64::from(self.phase)
    }
    pub fn drain(&mut self) -> Vec<f32> {
        let mut output = Vec::with_capacity(self.samples.len() * 2);
        for sample in self.samples.drain(..) {
            output.extend_from_slice(&sample);
        }
        output
    }
}
impl Bus {
    #[inline]
    pub(crate) fn flush_sound_read(&mut self, address: u32) {
        if address >> 24 == 4 && (0x60..0xa0).contains(&(address & 0xffffff)) {
            self.apu.psg.flush(&mut self.io);
        }
    }
    pub(crate) fn sound_read(&self, offset: usize) -> u8 {
        if self.apu.psg.pending != 0
            && (offset == 0x70 || offset == 0x84 || (0x90..0xa0).contains(&offset))
        {
            // Debugger peeks remain non-mutating, including deferred wave shifts
            // and length expiry. Normal CPU reads flush without copying first.
            let mut psg = self.apu.psg.clone();
            let mut io: [u8; 0x400] = self.io.as_slice().try_into().unwrap();
            psg.flush(&mut io);
            return match offset {
                0x70 => io[0x70] & 0xe0,
                0x84 => (io[0x84] & 0x80) | psg.active,
                _ => psg.wave[crate::psg::Psg::wave_index(&io, offset)],
            };
        }
        match offset {
            0x90..=0x9f => self.apu.psg.wave[crate::psg::Psg::wave_index(&self.io, offset)],
            0x60 => self.io[offset] & 0x7f,
            0x62 | 0x68 => self.io[offset] & 0xc0,
            0x63 | 0x69 | 0x79 | 0x7c | 0x81 => self.io[offset],
            0x65 | 0x6d | 0x75 | 0x7d => self.io[offset] & 0x40,
            0x70 | 0x73 => self.io[offset] & 0xe0,
            0x80 => self.io[offset] & 0x77,
            0x82 => self.io[offset] & 0x0f,
            0x83 => self.io[offset] & 0x77,
            0x84 => (self.io[offset] & 0x80) | self.apu.psg.active,
            0x88 => self.io[offset] & 0xfe,
            0x89 => self.io[offset] & 0xc3,
            _ => 0,
        }
    }
    pub(crate) fn sound_write(&mut self, offset: usize, old: u8) {
        self.apu.psg.write(offset, old, &mut self.io);
        match offset {
            0xa0..=0xa7 => self.apu.fifo[(offset - 0xa0) / 4].push(self.io[offset] as i8),
            0x83 => {
                if self.io[0x83] & 8 != 0 {
                    self.apu.fifo[0] = Fifo::default();
                }
                if self.io[0x83] & 128 != 0 {
                    self.apu.fifo[1] = Fifo::default();
                }
                self.io[0x83] &= !0x88;
            }
            0x84 => {
                self.io[0x84] &= 0x80;
                if self.io[0x84] == 0 {
                    self.apu.psg.reset();
                    self.io[0x60..0x82].fill(0);
                }
            }
            _ => {}
        }
        if matches!(offset, 0x83 | 0x84 | 0xa0..=0xa7) {
            self.refresh_fifo_requests();
        }
    }
    pub(crate) fn clock_direct_sound(&mut self, overflows: [u64; 4]) {
        if self.io16(0x84) & 0x80 == 0 {
            return;
        }
        let control = self.io16(0x82);
        for n in 0..2 {
            let timer = (control >> (10 + n * 4) & 1) as usize;
            self.apu.fifo[n].clock(overflows[timer]);
        }
        self.refresh_fifo_requests();
    }
    pub(crate) fn clock_audio(&mut self, clocks: u64) {
        // Scheduler bounds this at the next output deadline, preserving each
        // timer overflow's order relative to resampling rather than batch-ending.
        if self.io[0x84] & 0x80 != 0 {
            self.apu.psg.pending += clocks as u16;
        }
        self.apu.phase += clocks as u16;
        if self.apu.phase < 512 {
            return;
        }
        self.apu.phase -= 512;
        self.apu.psg.flush(&mut self.io);
        let psg = self.apu.psg.output();
        let control = self.io16(0x82);
        let bias = self.io16(0x88);
        let enabled = self.io16(0x84) & 128 != 0;
        let mut stereo = [0.; 2];
        if enabled {
            for (side, value) in stereo.iter_mut().enumerate() {
                let mut sum = psg[side];
                for n in 0..2 {
                    let route = if side == 0 { 9 + n * 4 } else { 8 + n * 4 };
                    if control >> route & 1 != 0 {
                        let gain = if control >> (2 + n) & 1 != 0 { 4 } else { 2 };
                        sum += i32::from(self.apu.fifo[n].held) * gain;
                    }
                }
                let output = (sum + i32::from(bias & 0x3fe)).clamp(0, 1023);
                let discarded = 1 + (bias >> 14);
                let output = output & !((1 << discarded) - 1);
                *value = (output - 512) as f32 / 512.;
            }
        }
        if self.apu.samples.len() == MAX_SAMPLES {
            self.apu.samples.pop_front();
        }
        self.apu.samples.push_back(stereo);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cpu::Memory;
    fn bus() -> Bus {
        Bus::new(vec![0; 192], None).unwrap()
    }
    #[test]
    fn fifo_orders_bytes_bounds_and_underflow() {
        let mut b = bus();
        b.write32(0x040000a0, 0x807f02ff, false);
        assert_eq!(b.apu.fifo[0].len, 4);
        for expected in [-1, 2, 127, -128] {
            b.apu.fifo[0].clock(1);
            assert_eq!(b.apu.fifo[0].held, expected);
        }
        b.apu.fifo[0].clock(1);
        assert_eq!(b.apu.fifo[0].held, 0);
        for n in 0..40 {
            b.write8(0x040000a0, n);
        }
        assert_eq!(b.apu.fifo[0].len, 32);
        assert_eq!(b.peek32(0x040000a0), 0);
        b.write16(0x04000082, 0x0800, false);
        assert_eq!(b.apu.fifo[0].len, 0);
        assert_eq!(b.io16(0x82) & 0x800, 0);
    }
    #[test]
    fn timer_selection_and_signed_stereo_mix() {
        let mut b = bus();
        b.write16(0x04000084, 0x80, false);
        b.write16(0x04000082, 0x610c, false);
        b.write8(0x040000a0, 64);
        b.write8(0x040000a4, (-64i8) as u8);
        b.clock_direct_sound([1, 0, 0, 0]);
        assert_eq!(b.apu.fifo[0].held, 64);
        assert_eq!(b.apu.fifo[1].held, 0);
        b.clock_direct_sound([0, 1, 0, 0]);
        b.clock_audio(512);
        assert_eq!(b.apu.samples.pop_front(), Some([-0.5, 0.5]));
    }
    #[test]
    fn master_off_is_silent_and_keeps_fifo_control() {
        let mut b = bus();
        b.write16(0x04000082, 0x0304, false);
        b.write16(0x04000084, 0, false);
        b.write8(0x040000a0, 120);
        b.clock_direct_sound([1, 0, 0, 0]);
        b.clock_audio(512);
        assert_eq!(b.apu.samples.pop_front(), Some([0., 0.]));
        assert_eq!(b.apu.fifo[0].len, 1);
        assert_eq!(b.io16(0x82), 0x304);
    }
    #[test]
    fn fifo_sum_clips_and_resolution_quantizes() {
        let mut b = bus();
        b.write16(0x04000084, 0x80, false);
        b.write16(0x04000082, 0x330c, false);
        b.apu.fifo[0].held = 127;
        b.apu.fifo[1].held = 127;
        b.clock_audio(512);
        assert_eq!(b.apu.samples.pop_front(), Some([510. / 512.; 2]));
        b.apu.fifo[0].held = -128;
        b.apu.fifo[1].held = -128;
        b.clock_audio(512);
        assert_eq!(b.apu.samples.pop_front(), Some([-1.; 2]));
    }
}

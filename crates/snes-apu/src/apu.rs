//! Independent SPC clock domain, CPU bus, directional ports and sound timers.
use crate::{
    dsp::Dsp,
    spc700::{Bus, Spc700},
    SpcRam,
};
use std::collections::VecDeque;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Registers {
    pub ram: SpcRam,
    pub dsp: Dsp,
    pub input: [u8; 4],
    pub output: [u8; 4],
    dsp_address: u8,
    control: u8,
    timer_target: [u8; 3],
    timer_stage: [u8; 3],
    timer_counter: [u8; 3],
    clock: u8,
}

impl Bus for Registers {
    fn read(&mut self, address: u16) -> u8 {
        match address {
            0xf0 | 0xf1 | 0xfa..=0xfc => 0,
            0xf2 => self.dsp_address,
            0xf3 => self.dsp.read(self.dsp_address),
            0xf4..=0xf7 => self.input[usize::from(address - 0xf4)],
            0xfd..=0xff => {
                let i = usize::from(address - 0xfd);
                let value = self.timer_counter[i];
                self.timer_counter[i] = 0;
                value
            }
            _ => self.ram.read(address),
        }
    }
    fn write(&mut self, address: u16, value: u8) {
        match address {
            // Normal sound drivers do not use nonstandard TEST clock modes.
            0xf0 => self.ram.write(address, value),
            0xf1 => {
                for i in 0..3 {
                    if value & (1 << i) != 0 && self.control & (1 << i) == 0 {
                        self.timer_stage[i] = 0;
                        self.timer_counter[i] = 0;
                    }
                }
                if value & 16 != 0 {
                    self.input[..2].fill(0);
                }
                if value & 32 != 0 {
                    self.input[2..].fill(0);
                }
                self.control = value;
            }
            0xf2 => self.dsp_address = value,
            0xf3 => self.dsp.write(self.dsp_address, value),
            0xf4..=0xf7 => self.output[usize::from(address - 0xf4)] = value,
            0xfa..=0xfc => self.timer_target[usize::from(address - 0xfa)] = value,
            0xfd..=0xff => {}
            _ => self.ram.write(address, value),
        }
    }
}

impl Registers {
    pub fn tick(&mut self) -> Option<[i16; 2]> {
        self.clock = self.clock.wrapping_add(1);
        for i in 0..3 {
            let divider = if i == 2 { 16 } else { 128 };
            if u16::from(self.clock) % divider == 0 && self.control & (1 << i) != 0 {
                self.timer_stage[i] = self.timer_stage[i].wrapping_add(1);
                if self.timer_stage[i] == self.timer_target[i] {
                    self.timer_stage[i] = 0;
                    self.timer_counter[i] = (self.timer_counter[i] + 1) & 15;
                }
            }
        }
        if self.clock & 31 == 0 {
            Some(self.dsp.sample(&mut self.ram))
        } else {
            None
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Apu {
    pub cpu: Spc700,
    pub bus: Registers,
    instruction_clocks: u8,
    samples: VecDeque<[i16; 2]>,
}
crate::state::snapshot!(
    Registers,
    ram,
    dsp,
    input,
    output,
    dsp_address,
    control,
    timer_target,
    timer_stage,
    timer_counter,
    clock
);
crate::state::snapshot!(Apu, cpu, bus, instruction_clocks, samples);
impl Apu {
    pub fn export_state(&self) -> Vec<u8> {
        use crate::state::State;
        let mut output = Vec::new();
        output.extend_from_slice(b"RBAP\x01\x00");
        self.encode(&mut output);
        output
    }
    pub fn import_state(bytes: &[u8]) -> Result<Self, &'static str> {
        use crate::state::{Reader, State};
        let mut input = Reader(bytes);
        if input.take(6)? != b"RBAP\x01\x00" {
            return Err("Unsupported SPC state format");
        }
        let apu = Self::decode(&mut input)?;
        if !input.0.is_empty()
            || apu.instruction_clocks > 12
            || apu.bus.timer_counter.iter().any(|&v| v > 15)
        {
            return Err("Invalid SPC state");
        }
        apu.bus.dsp.validate_state()?;
        Ok(apu)
    }
    pub fn pop_sample(&mut self) -> Option<[i16; 2]> {
        self.samples.pop_front()
    }
    /// Fraction of the next 32 kHz DSP frame already clocked (0..31).
    pub fn sample_phase(&self) -> u8 {
        self.bus.clock & 31
    }
    /// Start an already uploaded program; the original IPL is not needed here.
    pub fn start(&mut self, address: u16) {
        self.cpu.pc = address;
        self.cpu.halted = false;
        self.instruction_clocks = 0;
    }
    pub fn run(&mut self, clocks: u32) {
        for _ in 0..clocks {
            if self.instruction_clocks == 0 {
                self.instruction_clocks = self.cpu.step(&mut self.bus) as u8;
            }
            self.instruction_clocks -= 1;
            if let Some(sample) = self.bus.tick() {
                // Host queues are bounded even when a diagnostic does not drain.
                if self.samples.len() == 8192 {
                    self.samples.pop_front();
                }
                self.samples.push_back(sample);
            }
        }
    }
    pub fn drain_samples(&mut self) -> Vec<[i16; 2]> {
        self.samples.drain(..).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn snapshots_replay_cpu_timers_dsp_and_queued_samples_and_reject_corruption() {
        let mut apu = Apu::default();
        apu.bus.ram.write_wrapping(0x400, &[0xab, 0x20, 0x2f, 0xfc]);
        apu.start(0x400);
        apu.bus.write(0xfa, 2);
        apu.bus.write(0xf1, 1);
        apu.run(1397);
        let bytes = apu.export_state();
        let mut restored = Apu::import_state(&bytes).unwrap();
        assert_eq!(restored, apu);
        apu.run(6701);
        restored.run(6701);
        assert_eq!(restored, apu);
        assert_eq!(restored.drain_samples(), apu.drain_samples());
        for length in [0, 5, 100, bytes.len() - 1] {
            assert!(Apu::import_state(&bytes[..length]).is_err());
        }
        let mut trailing = bytes.clone();
        trailing.push(0);
        assert!(Apu::import_state(&trailing).is_err());
        let mut flag = bytes;
        flag[13] = 2; // CPU halted is a checked Boolean.
        assert!(Apu::import_state(&flag).is_err());
    }
    #[test]
    fn ports_are_directional_and_timer_reads_clear_only_the_counter() {
        let mut bus = Registers::default();
        bus.input = [11, 22, 33, 44];
        bus.write(0xf4, 99);
        assert_eq!(bus.read(0xf4), 11);
        assert_eq!(bus.output[0], 99);
        bus.write(0xfa, 2);
        bus.write(0xf1, 1);
        for _ in 0..256 {
            bus.tick();
        }
        assert_eq!(bus.read(0xfd), 1);
        assert_eq!(bus.read(0xfd), 0);
        bus.write(0xf1, 1); // An already enabled timer is not retriggered.
        for _ in 0..128 {
            bus.tick();
        }
        assert_eq!(bus.read(0xfd), 0);
        for _ in 0..128 {
            bus.tick();
        }
        assert_eq!(bus.read(0xfd), 1);
        bus.write(0xf1, 0x30);
        assert_eq!(bus.input, [0; 4]);
        assert_eq!(bus.output[0], 99);
    }
    #[test]
    fn zero_target_means_256_ticks_and_sample_clock_is_32_khz() {
        let mut apu = Apu::default();
        apu.cpu.halted = true;
        apu.bus.write(0xf1, 4);
        apu.run(4096);
        assert_eq!(apu.bus.read(0xff), 1);
        assert_eq!(apu.drain_samples(), vec![[0, 0]; 128]);
        assert!(apu.drain_samples().is_empty());
    }
}

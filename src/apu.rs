/// Game Boy DMG APU (Audio Processing Unit)
/// 4 channels: CH1 square+sweep, CH2 square, CH3 wave, CH4 noise
/// Frame sequencer clocked at 512 Hz (every 8192 T-cycles)
/// Output: stereo f32 samples at ~44100 Hz

const CPU_FREQ: f64 = 4_194_304.0;
const SAMPLE_RATE: f64 = 44100.0;
const CYCLES_PER_SAMPLE: f64 = CPU_FREQ / SAMPLE_RATE; // ~95.1

const DUTY_TABLE: [[u8; 8]; 4] = [
    [0, 0, 0, 0, 0, 0, 0, 1], // 12.5%
    [1, 0, 0, 0, 0, 0, 0, 1], // 25%
    [1, 0, 0, 0, 0, 1, 1, 1], // 50%
    [0, 1, 1, 1, 1, 1, 1, 0], // 75%
];

// ─── Channel 1: Square + Sweep ───────────────────────────────────────────────

struct Channel1 {
    enabled: bool,
    dac_enabled: bool,

    // Sweep
    sweep_period: u8,
    sweep_negate: bool,
    sweep_shift: u8,
    sweep_timer: u8,
    sweep_enabled: bool,
    sweep_shadow_freq: u16,
    sweep_negate_used: bool,

    // Duty & length
    duty: u8,
    length_counter: u8,
    length_enabled: bool,

    // Volume envelope
    volume_initial: u8,
    volume: u8,
    envelope_add: bool,
    envelope_period: u8,
    envelope_timer: u8,
    envelope_running: bool,

    // Frequency & timer
    frequency: u16,
    timer: i32,
    duty_pos: u8,
}

impl Channel1 {
    fn new() -> Self {
        Channel1 {
            enabled: false,
            dac_enabled: false,
            sweep_period: 0,
            sweep_negate: false,
            sweep_shift: 0,
            sweep_timer: 0,
            sweep_enabled: false,
            sweep_shadow_freq: 0,
            sweep_negate_used: false,
            duty: 0,
            length_counter: 0,
            length_enabled: false,
            volume_initial: 0,
            volume: 0,
            envelope_add: false,
            envelope_period: 0,
            envelope_timer: 0,
            envelope_running: true,
            frequency: 0,
            timer: 0,
            duty_pos: 0,
        }
    }

    fn tick(&mut self, cycles: u32) {
        self.timer -= cycles as i32;
        while self.timer <= 0 {
            self.timer += ((2048 - self.frequency as i32) * 4) as i32;
            self.duty_pos = (self.duty_pos + 1) & 7;
        }
    }

    fn output(&self) -> f32 {
        if !self.enabled || !self.dac_enabled {
            return 0.0;
        }
        let sample = DUTY_TABLE[self.duty as usize][self.duty_pos as usize];
        // DAC converts 0-15 digital to -1..+1 analog
        let dac_input = if sample != 0 { self.volume } else { 0 };
        (dac_input as f32 / 7.5) - 1.0
    }

    fn sweep_calc(&mut self) -> u16 {
        let mut new_freq = self.sweep_shadow_freq >> self.sweep_shift;
        if self.sweep_negate {
            new_freq = self.sweep_shadow_freq.wrapping_sub(new_freq);
            self.sweep_negate_used = true;
        } else {
            new_freq = self.sweep_shadow_freq.wrapping_add(new_freq);
        }
        // Overflow check
        if new_freq > 2047 {
            self.enabled = false;
        }
        new_freq
    }

    fn clock_sweep(&mut self) {
        self.sweep_timer = self.sweep_timer.saturating_sub(1);
        if self.sweep_timer == 0 {
            self.sweep_timer = if self.sweep_period == 0 { 8 } else { self.sweep_period };
            if self.sweep_enabled && self.sweep_period > 0 {
                let new_freq = self.sweep_calc();
                if new_freq <= 2047 && self.sweep_shift > 0 {
                    self.frequency = new_freq;
                    self.sweep_shadow_freq = new_freq;
                    // Do overflow check again
                    self.sweep_calc();
                }
            }
        }
    }

    fn clock_length(&mut self) {
        if self.length_enabled && self.length_counter > 0 {
            self.length_counter -= 1;
            if self.length_counter == 0 {
                self.enabled = false;
            }
        }
    }

    fn clock_envelope(&mut self) {
        if self.envelope_period == 0 { return; }
        if !self.envelope_running { return; }
        self.envelope_timer = self.envelope_timer.saturating_sub(1);
        if self.envelope_timer == 0 {
            self.envelope_timer = if self.envelope_period == 0 { 8 } else { self.envelope_period };
            if self.envelope_add && self.volume < 15 {
                self.volume += 1;
            } else if !self.envelope_add && self.volume > 0 {
                self.volume -= 1;
            } else {
                self.envelope_running = false;
            }
        }
    }

    fn trigger(&mut self, frame_seq_step: u8) {
        self.enabled = true;
        if self.length_counter == 0 {
            self.length_counter = 64;
            // If length enable is set and we're on a length-clocking step,
            // the counter is immediately decremented
            if self.length_enabled && (frame_seq_step & 1) == 0 {
                self.length_counter -= 1;
            }
        }
        self.timer = ((2048 - self.frequency as i32) * 4) as i32;
        self.volume = self.volume_initial;
        self.envelope_timer = if self.envelope_period == 0 { 8 } else { self.envelope_period };
        self.envelope_running = true;

        // Sweep
        self.sweep_shadow_freq = self.frequency;
        self.sweep_timer = if self.sweep_period == 0 { 8 } else { self.sweep_period };
        self.sweep_negate_used = false;
        self.sweep_enabled = self.sweep_period > 0 || self.sweep_shift > 0;
        if self.sweep_shift > 0 {
            self.sweep_calc();
        }

        if !self.dac_enabled {
            self.enabled = false;
        }
    }
}

// ─── Channel 2: Square (no sweep) ───────────────────────────────────────────

struct Channel2 {
    enabled: bool,
    dac_enabled: bool,

    duty: u8,
    length_counter: u8,
    length_enabled: bool,

    volume_initial: u8,
    volume: u8,
    envelope_add: bool,
    envelope_period: u8,
    envelope_timer: u8,
    envelope_running: bool,

    frequency: u16,
    timer: i32,
    duty_pos: u8,
}

impl Channel2 {
    fn new() -> Self {
        Channel2 {
            enabled: false,
            dac_enabled: false,
            duty: 0,
            length_counter: 0,
            length_enabled: false,
            volume_initial: 0,
            volume: 0,
            envelope_add: false,
            envelope_period: 0,
            envelope_timer: 0,
            envelope_running: true,
            frequency: 0,
            timer: 0,
            duty_pos: 0,
        }
    }

    fn tick(&mut self, cycles: u32) {
        self.timer -= cycles as i32;
        while self.timer <= 0 {
            self.timer += ((2048 - self.frequency as i32) * 4) as i32;
            self.duty_pos = (self.duty_pos + 1) & 7;
        }
    }

    fn output(&self) -> f32 {
        if !self.enabled || !self.dac_enabled {
            return 0.0;
        }
        let sample = DUTY_TABLE[self.duty as usize][self.duty_pos as usize];
        let dac_input = if sample != 0 { self.volume } else { 0 };
        (dac_input as f32 / 7.5) - 1.0
    }

    fn clock_length(&mut self) {
        if self.length_enabled && self.length_counter > 0 {
            self.length_counter -= 1;
            if self.length_counter == 0 {
                self.enabled = false;
            }
        }
    }

    fn clock_envelope(&mut self) {
        if self.envelope_period == 0 { return; }
        if !self.envelope_running { return; }
        self.envelope_timer = self.envelope_timer.saturating_sub(1);
        if self.envelope_timer == 0 {
            self.envelope_timer = if self.envelope_period == 0 { 8 } else { self.envelope_period };
            if self.envelope_add && self.volume < 15 {
                self.volume += 1;
            } else if !self.envelope_add && self.volume > 0 {
                self.volume -= 1;
            } else {
                self.envelope_running = false;
            }
        }
    }

    fn trigger(&mut self, frame_seq_step: u8) {
        self.enabled = true;
        if self.length_counter == 0 {
            self.length_counter = 64;
            if self.length_enabled && (frame_seq_step & 1) == 0 {
                self.length_counter -= 1;
            }
        }
        self.timer = ((2048 - self.frequency as i32) * 4) as i32;
        self.volume = self.volume_initial;
        self.envelope_timer = if self.envelope_period == 0 { 8 } else { self.envelope_period };
        self.envelope_running = true;

        if !self.dac_enabled {
            self.enabled = false;
        }
    }
}

// ─── Channel 3: Wave ─────────────────────────────────────────────────────────

struct Channel3 {
    enabled: bool,
    dac_enabled: bool,

    length_counter: u16,
    length_enabled: bool,

    volume_code: u8, // 0=mute, 1=100%, 2=50%, 3=25%

    frequency: u16,
    timer: i32,
    wave_pos: u8,

    wave_ram: [u8; 16], // 32 x 4-bit samples stored in 16 bytes
}

impl Channel3 {
    fn new() -> Self {
        Channel3 {
            enabled: false,
            dac_enabled: false,
            length_counter: 0,
            length_enabled: false,
            volume_code: 0,
            frequency: 0,
            timer: 0,
            wave_pos: 0,
            wave_ram: [0; 16],
        }
    }

    fn tick(&mut self, cycles: u32) {
        self.timer -= cycles as i32;
        while self.timer <= 0 {
            self.timer += ((2048 - self.frequency as i32) * 2) as i32;
            self.wave_pos = (self.wave_pos + 1) & 31;
        }
    }

    fn output(&self) -> f32 {
        if !self.enabled || !self.dac_enabled {
            return 0.0;
        }
        let byte = self.wave_ram[(self.wave_pos / 2) as usize];
        let sample = if self.wave_pos & 1 == 0 {
            (byte >> 4) & 0x0F
        } else {
            byte & 0x0F
        };
        let shifted = match self.volume_code {
            0 => 0,
            1 => sample,
            2 => sample >> 1,
            3 => sample >> 2,
            _ => 0,
        };
        (shifted as f32 / 7.5) - 1.0
    }

    fn clock_length(&mut self) {
        if self.length_enabled && self.length_counter > 0 {
            self.length_counter -= 1;
            if self.length_counter == 0 {
                self.enabled = false;
            }
        }
    }

    fn trigger(&mut self, frame_seq_step: u8) {
        self.enabled = true;
        if self.length_counter == 0 {
            self.length_counter = 256;
            if self.length_enabled && (frame_seq_step & 1) == 0 {
                self.length_counter -= 1;
            }
        }
        self.timer = ((2048 - self.frequency as i32) * 2) as i32;
        self.wave_pos = 0;

        if !self.dac_enabled {
            self.enabled = false;
        }
    }
}

// ─── Channel 4: Noise ────────────────────────────────────────────────────────

struct Channel4 {
    enabled: bool,
    dac_enabled: bool,

    length_counter: u8,
    length_enabled: bool,

    volume_initial: u8,
    volume: u8,
    envelope_add: bool,
    envelope_period: u8,
    envelope_timer: u8,
    envelope_running: bool,

    clock_shift: u8,
    width_mode: bool, // true = 7-bit, false = 15-bit
    divisor_code: u8,

    timer: i32,
    lfsr: u16,
}

impl Channel4 {
    fn new() -> Self {
        Channel4 {
            enabled: false,
            dac_enabled: false,
            length_counter: 0,
            length_enabled: false,
            volume_initial: 0,
            volume: 0,
            envelope_add: false,
            envelope_period: 0,
            envelope_timer: 0,
            envelope_running: true,
            clock_shift: 0,
            width_mode: false,
            divisor_code: 0,
            timer: 0,
            lfsr: 0x7FFF,
        }
    }

    fn get_divisor(&self) -> i32 {
        match self.divisor_code {
            0 => 8,
            1 => 16,
            2 => 32,
            3 => 48,
            4 => 64,
            5 => 80,
            6 => 96,
            7 => 112,
            _ => 8,
        }
    }

    fn tick(&mut self, cycles: u32) {
        self.timer -= cycles as i32;
        while self.timer <= 0 {
            self.timer += self.get_divisor() << self.clock_shift;
            // Clock the LFSR
            let xor = (self.lfsr & 1) ^ ((self.lfsr >> 1) & 1);
            self.lfsr = (self.lfsr >> 1) | (xor << 14);
            if self.width_mode {
                // 7-bit mode: also set bit 6
                self.lfsr &= !(1 << 6);
                self.lfsr |= xor << 6;
            }
        }
    }

    fn output(&self) -> f32 {
        if !self.enabled || !self.dac_enabled {
            return 0.0;
        }
        // Output is inverted bit 0 of LFSR
        let dac_input = if self.lfsr & 1 == 0 { self.volume } else { 0 };
        (dac_input as f32 / 7.5) - 1.0
    }

    fn clock_length(&mut self) {
        if self.length_enabled && self.length_counter > 0 {
            self.length_counter -= 1;
            if self.length_counter == 0 {
                self.enabled = false;
            }
        }
    }

    fn clock_envelope(&mut self) {
        if self.envelope_period == 0 { return; }
        if !self.envelope_running { return; }
        self.envelope_timer = self.envelope_timer.saturating_sub(1);
        if self.envelope_timer == 0 {
            self.envelope_timer = if self.envelope_period == 0 { 8 } else { self.envelope_period };
            if self.envelope_add && self.volume < 15 {
                self.volume += 1;
            } else if !self.envelope_add && self.volume > 0 {
                self.volume -= 1;
            } else {
                self.envelope_running = false;
            }
        }
    }

    fn trigger(&mut self, frame_seq_step: u8) {
        self.enabled = true;
        if self.length_counter == 0 {
            self.length_counter = 64;
            if self.length_enabled && (frame_seq_step & 1) == 0 {
                self.length_counter -= 1;
            }
        }
        self.timer = self.get_divisor() << self.clock_shift;
        self.lfsr = 0x7FFF;
        self.volume = self.volume_initial;
        self.envelope_timer = if self.envelope_period == 0 { 8 } else { self.envelope_period };
        self.envelope_running = true;

        if !self.dac_enabled {
            self.enabled = false;
        }
    }
}

// ─── Main APU ────────────────────────────────────────────────────────────────

pub struct APU {
    powered: bool,

    ch1: Channel1,
    ch2: Channel2,
    ch3: Channel3,
    ch4: Channel4,

    // NR50: master volume
    left_volume: u8,
    right_volume: u8,
    vin_left: bool,
    vin_right: bool,

    // NR51: panning
    panning: u8, // raw register value

    // Frame sequencer: clocked every 8192 T-cycles (512 Hz)
    frame_seq_cycles: u32,
    frame_seq_step: u8,

    // Sample generation
    sample_timer: f64,
    sample_buffer: Vec<f32>, // interleaved stereo [L, R, L, R, ...]
}

impl APU {
    pub fn new() -> Self {
        APU {
            powered: true,
            ch1: Channel1::new(),
            ch2: Channel2::new(),
            ch3: Channel3::new(),
            ch4: Channel4::new(),
            left_volume: 7,
            right_volume: 7,
            vin_left: false,
            vin_right: false,
            panning: 0xFF,
            frame_seq_cycles: 0,
            frame_seq_step: 0,
            sample_timer: 0.0,
            sample_buffer: Vec::with_capacity(2048),
        }
    }

    /// Advance the APU by `t_cycles` T-cycles
    pub fn tick(&mut self, t_cycles: u32) {
        if !self.powered {
            // Still need to generate silence samples to keep audio stream alive
            self.sample_timer += t_cycles as f64;
            while self.sample_timer >= CYCLES_PER_SAMPLE {
                self.sample_timer -= CYCLES_PER_SAMPLE;
                self.sample_buffer.push(0.0);
                self.sample_buffer.push(0.0);
            }
            return;
        }

        // Tick channels
        self.ch1.tick(t_cycles);
        self.ch2.tick(t_cycles);
        self.ch3.tick(t_cycles);
        self.ch4.tick(t_cycles);

        // Frame sequencer
        self.frame_seq_cycles += t_cycles;
        while self.frame_seq_cycles >= 8192 {
            self.frame_seq_cycles -= 8192;
            self.clock_frame_sequencer();
        }

        // Generate samples
        self.sample_timer += t_cycles as f64;
        while self.sample_timer >= CYCLES_PER_SAMPLE {
            self.sample_timer -= CYCLES_PER_SAMPLE;
            self.generate_sample();
        }
    }

    fn clock_frame_sequencer(&mut self) {
        match self.frame_seq_step {
            0 => {
                self.ch1.clock_length();
                self.ch2.clock_length();
                self.ch3.clock_length();
                self.ch4.clock_length();
            }
            1 => {}
            2 => {
                self.ch1.clock_length();
                self.ch2.clock_length();
                self.ch3.clock_length();
                self.ch4.clock_length();
                self.ch1.clock_sweep();
            }
            3 => {}
            4 => {
                self.ch1.clock_length();
                self.ch2.clock_length();
                self.ch3.clock_length();
                self.ch4.clock_length();
            }
            5 => {}
            6 => {
                self.ch1.clock_length();
                self.ch2.clock_length();
                self.ch3.clock_length();
                self.ch4.clock_length();
                self.ch1.clock_sweep();
            }
            7 => {
                self.ch1.clock_envelope();
                self.ch2.clock_envelope();
                self.ch4.clock_envelope();
            }
            _ => {}
        }
        self.frame_seq_step = (self.frame_seq_step + 1) & 7;
    }

    fn generate_sample(&mut self) {
        let ch1 = self.ch1.output();
        let ch2 = self.ch2.output();
        let ch3 = self.ch3.output();
        let ch4 = self.ch4.output();

        let mut left: f32 = 0.0;
        let mut right: f32 = 0.0;

        // NR51 panning
        if self.panning & 0x10 != 0 { left += ch1; }
        if self.panning & 0x20 != 0 { left += ch2; }
        if self.panning & 0x40 != 0 { left += ch3; }
        if self.panning & 0x80 != 0 { left += ch4; }

        if self.panning & 0x01 != 0 { right += ch1; }
        if self.panning & 0x02 != 0 { right += ch2; }
        if self.panning & 0x04 != 0 { right += ch3; }
        if self.panning & 0x08 != 0 { right += ch4; }

        // NR50 master volume (0-7 → scale factor)
        left *= (self.left_volume as f32 + 1.0) / 8.0;
        right *= (self.right_volume as f32 + 1.0) / 8.0;

        // Scale down (4 channels mixed, each -1..+1, so max amplitude ~4.0)
        left /= 4.0;
        right /= 4.0;

        self.sample_buffer.push(left);
        self.sample_buffer.push(right);
    }

    /// Drain accumulated samples for JS audio output
    pub fn drain_samples(&mut self) -> Vec<f32> {
        std::mem::take(&mut self.sample_buffer)
    }

    /// Write to an APU register (0xFF10-0xFF3F)
    pub fn write_register(&mut self, addr: u16, val: u8) {
        // NR52 (0xFF26) can always be written
        if addr == 0xFF26 {
            let was_on = self.powered;
            self.powered = val & 0x80 != 0;
            if was_on && !self.powered {
                self.power_off();
            }
            return;
        }

        // Wave RAM (0xFF30-0xFF3F) can always be written
        if (0xFF30..=0xFF3F).contains(&addr) {
            self.ch3.wave_ram[(addr - 0xFF30) as usize] = val;
            return;
        }

        // On DMG, length counters can be written even when powered off
        if !self.powered {
            match addr {
                0xFF11 => { self.ch1.length_counter = 64 - (val & 0x3F); }
                0xFF16 => { self.ch2.length_counter = 64 - (val & 0x3F); }
                0xFF1B => { self.ch3.length_counter = 256 - val as u16; }
                0xFF20 => { self.ch4.length_counter = 64 - (val & 0x3F); }
                _ => {}
            }
            return;
        }

        match addr {
            // ── Channel 1: Square + Sweep ──
            0xFF10 => {
                // NR10: Sweep
                self.ch1.sweep_period = (val >> 4) & 0x07;
                let new_negate = val & 0x08 != 0;
                // Switching from negate to non-negate after using negate disables channel
                if self.ch1.sweep_negate && !new_negate && self.ch1.sweep_negate_used {
                    self.ch1.enabled = false;
                }
                self.ch1.sweep_negate = new_negate;
                self.ch1.sweep_shift = val & 0x07;
            }
            0xFF11 => {
                // NR11: Duty + Length
                self.ch1.duty = (val >> 6) & 0x03;
                self.ch1.length_counter = 64 - (val & 0x3F);
            }
            0xFF12 => {
                // NR12: Volume Envelope
                self.ch1.volume_initial = (val >> 4) & 0x0F;
                self.ch1.envelope_add = val & 0x08 != 0;
                self.ch1.envelope_period = val & 0x07;
                self.ch1.dac_enabled = val & 0xF8 != 0;
                if !self.ch1.dac_enabled {
                    self.ch1.enabled = false;
                }
            }
            0xFF13 => {
                // NR13: Frequency low
                self.ch1.frequency = (self.ch1.frequency & 0x700) | val as u16;
            }
            0xFF14 => {
                // NR14: Trigger + Length enable + Frequency high
                self.ch1.frequency = (self.ch1.frequency & 0xFF) | ((val as u16 & 0x07) << 8);
                let old_length_enabled = self.ch1.length_enabled;
                self.ch1.length_enabled = val & 0x40 != 0;
                // Extra length clock: enabling length on a step that doesn't clock length
                // when length counter is non-zero causes an immediate clock
                if !old_length_enabled && self.ch1.length_enabled
                    && self.ch1.length_counter > 0
                    && (self.frame_seq_step & 1) == 0
                {
                    self.ch1.length_counter -= 1;
                    if self.ch1.length_counter == 0 && (val & 0x80 == 0) {
                        self.ch1.enabled = false;
                    }
                }
                if val & 0x80 != 0 {
                    self.ch1.trigger(self.frame_seq_step);
                }
            }

            // ── Channel 2: Square ──
            0xFF16 => {
                // NR21: Duty + Length
                self.ch2.duty = (val >> 6) & 0x03;
                self.ch2.length_counter = 64 - (val & 0x3F);
            }
            0xFF17 => {
                // NR22: Volume Envelope
                self.ch2.volume_initial = (val >> 4) & 0x0F;
                self.ch2.envelope_add = val & 0x08 != 0;
                self.ch2.envelope_period = val & 0x07;
                self.ch2.dac_enabled = val & 0xF8 != 0;
                if !self.ch2.dac_enabled {
                    self.ch2.enabled = false;
                }
            }
            0xFF18 => {
                // NR23: Frequency low
                self.ch2.frequency = (self.ch2.frequency & 0x700) | val as u16;
            }
            0xFF19 => {
                // NR24: Trigger + Length enable + Frequency high
                self.ch2.frequency = (self.ch2.frequency & 0xFF) | ((val as u16 & 0x07) << 8);
                let old_length_enabled = self.ch2.length_enabled;
                self.ch2.length_enabled = val & 0x40 != 0;
                if !old_length_enabled && self.ch2.length_enabled
                    && self.ch2.length_counter > 0
                    && (self.frame_seq_step & 1) == 0
                {
                    self.ch2.length_counter -= 1;
                    if self.ch2.length_counter == 0 && (val & 0x80 == 0) {
                        self.ch2.enabled = false;
                    }
                }
                if val & 0x80 != 0 {
                    self.ch2.trigger(self.frame_seq_step);
                }
            }

            // ── Channel 3: Wave ──
            0xFF1A => {
                // NR30: DAC enable
                self.ch3.dac_enabled = val & 0x80 != 0;
                if !self.ch3.dac_enabled {
                    self.ch3.enabled = false;
                }
            }
            0xFF1B => {
                // NR31: Length
                self.ch3.length_counter = 256 - val as u16;
            }
            0xFF1C => {
                // NR32: Volume code
                self.ch3.volume_code = (val >> 5) & 0x03;
            }
            0xFF1D => {
                // NR33: Frequency low
                self.ch3.frequency = (self.ch3.frequency & 0x700) | val as u16;
            }
            0xFF1E => {
                // NR34: Trigger + Length enable + Frequency high
                self.ch3.frequency = (self.ch3.frequency & 0xFF) | ((val as u16 & 0x07) << 8);
                let old_length_enabled = self.ch3.length_enabled;
                self.ch3.length_enabled = val & 0x40 != 0;
                if !old_length_enabled && self.ch3.length_enabled
                    && self.ch3.length_counter > 0
                    && (self.frame_seq_step & 1) == 0
                {
                    self.ch3.length_counter -= 1;
                    if self.ch3.length_counter == 0 && (val & 0x80 == 0) {
                        self.ch3.enabled = false;
                    }
                }
                if val & 0x80 != 0 {
                    self.ch3.trigger(self.frame_seq_step);
                }
            }

            // ── Channel 4: Noise ──
            0xFF20 => {
                // NR41: Length
                self.ch4.length_counter = 64 - (val & 0x3F);
            }
            0xFF21 => {
                // NR42: Volume Envelope
                self.ch4.volume_initial = (val >> 4) & 0x0F;
                self.ch4.envelope_add = val & 0x08 != 0;
                self.ch4.envelope_period = val & 0x07;
                self.ch4.dac_enabled = val & 0xF8 != 0;
                if !self.ch4.dac_enabled {
                    self.ch4.enabled = false;
                }
            }
            0xFF22 => {
                // NR43: Polynomial counter
                self.ch4.clock_shift = (val >> 4) & 0x0F;
                self.ch4.width_mode = val & 0x08 != 0;
                self.ch4.divisor_code = val & 0x07;
            }
            0xFF23 => {
                // NR44: Trigger + Length enable
                let old_length_enabled = self.ch4.length_enabled;
                self.ch4.length_enabled = val & 0x40 != 0;
                if !old_length_enabled && self.ch4.length_enabled
                    && self.ch4.length_counter > 0
                    && (self.frame_seq_step & 1) == 0
                {
                    self.ch4.length_counter -= 1;
                    if self.ch4.length_counter == 0 && (val & 0x80 == 0) {
                        self.ch4.enabled = false;
                    }
                }
                if val & 0x80 != 0 {
                    self.ch4.trigger(self.frame_seq_step);
                }
            }

            // ── Master control ──
            0xFF24 => {
                // NR50: Master volume / VIN panning
                self.vin_left = val & 0x80 != 0;
                self.left_volume = (val >> 4) & 0x07;
                self.vin_right = val & 0x08 != 0;
                self.right_volume = val & 0x07;
            }
            0xFF25 => {
                // NR51: Sound panning
                self.panning = val;
            }

            // 0xFF15, 0xFF1F unused
            _ => {}
        }
    }

    /// Read from an APU register (0xFF10-0xFF3F)
    pub fn read_register(&self, addr: u16) -> u8 {
        // Wave RAM — on DMG, only readable while ch3 is disabled
        if (0xFF30..=0xFF3F).contains(&addr) {
            // For simplicity, always allow reads (accurate enough for most games)
            return self.ch3.wave_ram[(addr - 0xFF30) as usize];
        }

        match addr {
            0xFF10 => {
                0x80 | (self.ch1.sweep_period << 4)
                    | if self.ch1.sweep_negate { 0x08 } else { 0 }
                    | self.ch1.sweep_shift
            }
            0xFF11 => (self.ch1.duty << 6) | 0x3F, // low 6 bits write-only
            0xFF12 => {
                (self.ch1.volume_initial << 4)
                    | if self.ch1.envelope_add { 0x08 } else { 0 }
                    | self.ch1.envelope_period
            }
            0xFF13 => 0xFF, // write-only
            0xFF14 => 0xBF | if self.ch1.length_enabled { 0x40 } else { 0 },

            0xFF15 => 0xFF, // unused
            0xFF16 => (self.ch2.duty << 6) | 0x3F,
            0xFF17 => {
                (self.ch2.volume_initial << 4)
                    | if self.ch2.envelope_add { 0x08 } else { 0 }
                    | self.ch2.envelope_period
            }
            0xFF18 => 0xFF, // write-only
            0xFF19 => 0xBF | if self.ch2.length_enabled { 0x40 } else { 0 },

            0xFF1A => 0x7F | if self.ch3.dac_enabled { 0x80 } else { 0 },
            0xFF1B => 0xFF, // write-only
            0xFF1C => 0x9F | (self.ch3.volume_code << 5),
            0xFF1D => 0xFF, // write-only
            0xFF1E => 0xBF | if self.ch3.length_enabled { 0x40 } else { 0 },

            0xFF1F => 0xFF, // unused
            0xFF20 => 0xFF, // write-only
            0xFF21 => {
                (self.ch4.volume_initial << 4)
                    | if self.ch4.envelope_add { 0x08 } else { 0 }
                    | self.ch4.envelope_period
            }
            0xFF22 => {
                (self.ch4.clock_shift << 4)
                    | if self.ch4.width_mode { 0x08 } else { 0 }
                    | self.ch4.divisor_code
            }
            0xFF23 => 0xBF | if self.ch4.length_enabled { 0x40 } else { 0 },

            0xFF24 => {
                (if self.vin_left { 0x80 } else { 0 })
                    | (self.left_volume << 4)
                    | (if self.vin_right { 0x08 } else { 0 })
                    | self.right_volume
            }
            0xFF25 => self.panning,

            0xFF26 => {
                let mut val = 0x70; // bits 4-6 always read as 1
                if self.powered { val |= 0x80; }
                if self.ch1.enabled { val |= 0x01; }
                if self.ch2.enabled { val |= 0x02; }
                if self.ch3.enabled { val |= 0x04; }
                if self.ch4.enabled { val |= 0x08; }
                val
            }

            0xFF27..=0xFF2F => 0xFF, // unused

            _ => 0xFF,
        }
    }

    fn power_off(&mut self) {
        // On DMG, powering off clears all registers except length counters and wave RAM
        let ch1_len = self.ch1.length_counter;
        let ch2_len = self.ch2.length_counter;
        let ch3_len = self.ch3.length_counter;
        let ch4_len = self.ch4.length_counter;
        let wave_ram = self.ch3.wave_ram;

        self.ch1 = Channel1::new();
        self.ch2 = Channel2::new();
        self.ch3 = Channel3::new();
        self.ch4 = Channel4::new();

        // Restore length counters (DMG preserves them on power off)
        self.ch1.length_counter = ch1_len;
        self.ch2.length_counter = ch2_len;
        self.ch3.length_counter = ch3_len;
        self.ch4.length_counter = ch4_len;
        self.ch3.wave_ram = wave_ram;

        self.left_volume = 0;
        self.right_volume = 0;
        self.vin_left = false;
        self.vin_right = false;
        self.panning = 0;
    }
}


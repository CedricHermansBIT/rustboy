//! Independently authored high-level replacement for the resident SGB sound
//! engine. Implements the score interface documented in Nintendo's Game Boy
//! Programming Manual, chapter 7. No firmware, sample recordings or sequences
//! from Nintendo (or another emulator) are embedded here.
//!
//! This is a host-side driver, not a binary firmware image. It feeds our DSP;
//! cartridge-uploaded SPC programs still execute on our SPC700 instead.
use crate::{
    apu::Apu,
    state::{Reader, State},
    SpcRam,
};

#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct Fade {
    value: i32,
    target: i32,
    left: u16,
}
impl Fade {
    fn set(&mut self, value: i32) {
        self.value = value;
        self.target = value;
        self.left = 0;
    }
    fn to(&mut self, target: i32, steps: u8) {
        self.target = target;
        self.left = u16::from(steps);
        if steps == 0 {
            self.value = target;
        }
    }
    fn tick(&mut self) {
        if self.left != 0 {
            self.value += (self.target - self.value) / i32::from(self.left);
            self.left -= 1;
        }
    }
}
crate::state::snapshot!(Fade, value, target, left);

#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct Modulation {
    delay: u8,
    rate: u8,
    depth: u8,
    phase: u8,
    age: u16,
    fade: u8,
}
impl Modulation {
    fn tick(&mut self) -> i32 {
        self.age = self.age.saturating_add(1);
        if self.age <= u16::from(self.delay) {
            return 0;
        }
        self.phase = self.phase.wrapping_add(self.rate);
        let phase = i32::from(self.phase);
        let triangle = if phase < 64 {
            phase
        } else if phase < 192 {
            128 - phase
        } else {
            phase - 256
        };
        let depth = if self.fade == 0 {
            i32::from(self.depth)
        } else {
            i32::from(self.depth)
                * i32::from((self.age - u16::from(self.delay)).min(u16::from(self.fade)))
                / i32::from(self.fade)
        };
        triangle * depth / 64
    }
}
crate::state::snapshot!(Modulation, delay, rate, depth, phase, age, fade);

#[derive(Clone, Debug, PartialEq, Eq)]
struct Track {
    pc: u16,
    duration: u8,
    wait: u8,
    gate: u8,
    articulation: u8,
    instrument: u8,
    note: u8,
    transpose: i16,
    fine: u8,
    pan: Fade,
    volume: Fade,
    subroutine: u16,
    return_pc: u16,
    repeats: u8,
    vibrato: Modulation,
    tremolo: Modulation,
    pitch: Fade,
    pitch_delay: u8,
    envelope_delay: u8,
    envelope_steps: u8,
    envelope_offset: i16,
    envelope_from: bool,
    muted: bool,
}
impl Default for Track {
    fn default() -> Self {
        Self {
            pc: 0,
            duration: 24,
            wait: 0,
            gate: 0,
            articulation: 0x7f,
            instrument: 0,
            note: 0xA4,
            transpose: 0,
            fine: 0,
            pan: Fade {
                value: 10,
                target: 10,
                left: 0,
            },
            volume: Fade {
                value: 192,
                target: 192,
                left: 0,
            },
            subroutine: 0,
            return_pc: 0,
            repeats: 0,
            vibrato: Modulation::default(),
            tremolo: Modulation::default(),
            pitch: Fade::default(),
            pitch_delay: 0,
            envelope_delay: 0,
            envelope_steps: 0,
            envelope_offset: 0,
            envelope_from: false,
            muted: false,
        }
    }
}
crate::state::snapshot!(
    Track,
    pc,
    duration,
    wait,
    gate,
    articulation,
    instrument,
    note,
    transpose,
    fine,
    pan,
    volume,
    subroutine,
    return_pc,
    repeats,
    vibrato,
    tremolo,
    pitch,
    pitch_delay,
    envelope_delay,
    envelope_steps,
    envelope_offset,
    envelope_from,
    muted
);

#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct Effect {
    code: u8,
    age: u16,
    pitch: u8,
    volume: u8,
}
crate::state::snapshot!(Effect, code, age, pitch, volume);

/// A deterministic, bounded resident player. Instrument identities follow the
/// documented families, but their waveforms/envelopes are original substitutes.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Player {
    tracks: [Track; 8],
    effects: [Effect; 2],
    clock: u16,
    tempo_fraction: u16,
    tempo: Fade,
    volume: Fade,
    mute: Fade,
    phrase: u16,
    phrase_repeat: u8,
    song: u8,
    transpose: i16,
    percussion: u8,
    paused: bool,
    fast_forward: bool,
    echo_left: Fade,
    echo_right: Fade,
    program_uploaded: bool,
    notes: u32,
    errors: u32,
    startup_ticks: u8,
}
impl Default for Player {
    fn default() -> Self {
        Self {
            tracks: std::array::from_fn(|_| Track::default()),
            effects: std::array::from_fn(|_| Effect::default()),
            clock: 0,
            tempo_fraction: 0,
            tempo: Fade {
                value: 20,
                target: 20,
                left: 0,
            },
            volume: Fade {
                value: 192,
                target: 192,
                left: 0,
            },
            mute: Fade::default(),
            phrase: 0,
            phrase_repeat: 0,
            song: 0,
            transpose: 0,
            percussion: 0,
            paused: false,
            fast_forward: false,
            echo_left: Fade::default(),
            echo_right: Fade::default(),
            program_uploaded: false,
            notes: 0,
            errors: 0,
            startup_ticks: 0,
        }
    }
}
crate::state::snapshot!(
    Player,
    tracks,
    effects,
    clock,
    tempo_fraction,
    tempo,
    volume,
    mute,
    phrase,
    phrase_repeat,
    song,
    transpose,
    percussion,
    paused,
    fast_forward,
    echo_left,
    echo_right,
    program_uploaded,
    notes,
    errors,
    startup_ticks
);

fn word(ram: &SpcRam, address: u16) -> u16 {
    u16::from_le_bytes([ram.read(address), ram.read(address.wrapping_add(1))])
}
fn byte(ram: &SpcRam, pc: &mut u16) -> u8 {
    let value = ram.read(*pc);
    *pc = pc.wrapping_add(1);
    value
}
fn pitch(note: i32) -> u16 {
    // Resident-driver units before instrument multiplication. A one-note
    // reference score on user-supplied firmware gives C3, multiplier $0400 ->
    // DSP pitch $085C. Generate intervals mathematically, without embedding a
    // firmware table. $80 is C0 and $A4 is C3.
    (535.0 * 2.0f64.powf((f64::from(note) / 256.0 - 36.0) / 12.0))
        .round()
        .clamp(1.0, 16383.0) as u16
}
impl Player {
    /// Initialize our own small BRR bank in the documented sample RAM region.
    pub fn initialize(apu: &mut Apu) -> Self {
        apu.cpu.halted = true;
        crate::sgb_instruments::initialize(&mut apu.bus.ram);
        apu.bus.ram.write_wrapping(
            0x4c10,
            &[
                50, 101, 127, 152, 178, 203, 229, 252, 25, 50, 76, 101, 114, 127, 140, 152, 165,
                178, 191, 203, 216, 229, 242, 252,
            ],
        );
        for (reg, value) in [
            (0x5d, 0x4b),
            (0x6c, 0x20),
            (0x0c, 0),
            (0x1c, 0),
            (0x6d, 0xef),
            (0x7d, 2),
            (0x0f, 127),
        ] {
            apu.bus.dsp.write(reg, value);
        }
        Self::default()
    }
    pub fn notes(&self) -> u32 {
        self.notes
    }
    pub fn errors(&self) -> u32 {
        self.errors
    }
    pub fn mark_upload(&mut self, address: u16, size: usize) {
        for offset in 0..size {
            if (0x400..0x2b00).contains(&address.wrapping_add(offset as u16)) {
                self.program_uploaded = true;
                break;
            }
        }
    }
    pub fn use_uploaded_program(&self, jump: u16) -> bool {
        self.program_uploaded || jump != 0x400
    }
    pub fn restart(&mut self, apu: &mut Apu) {
        self.stop(apu);
        apu.bus.dsp.write(0x5c, 255);
        self.song = 0;
        self.effects = std::array::from_fn(|_| Effect::default());
    }
    pub fn command(&mut self, apu: &mut Apu, request: [u8; 4]) {
        let [a, b, attributes, music] = request;
        self.mute.to(if attributes & 12 == 12 { 0 } else { 127 }, 8);
        for (index, code) in [a, b].into_iter().enumerate() {
            let max = if index == 0 { 0x30 } else { 0x19 };
            if code == 0x80 {
                self.effects[index] = Effect::default();
                apu.bus.dsp.write(0x5c, 1 << if index == 0 { 7 } else { 5 });
            } else if code != 0 && code <= max {
                self.effects[index] = Effect {
                    code,
                    age: 0,
                    pitch: if index == 0 {
                        attributes & 3
                    } else {
                        attributes >> 4 & 3
                    },
                    volume: if index == 0 {
                        attributes >> 2 & 3
                    } else {
                        attributes >> 6 & 3
                    },
                };
            } else if code != 0 {
                self.errors = self.errors.saturating_add(1);
            }
        }
        if music == 0 {
            return;
        } // Dummy/retrigger flag: do not stop current music.
        if music == 0x80 {
            self.stop(apu);
            self.song = 0;
            return;
        }
        if music == 0xf0 {
            self.paused = true;
            apu.bus.dsp.write(0x5c, self.music_mask());
            return;
        }
        if music == 0xf1 {
            self.paused = false;
            return;
        }
        if music > 15 {
            self.errors = self.errors.saturating_add(1);
            return;
        }
        self.stop(apu);
        self.song = music;
        self.paused = false;
        self.fast_forward = false;
        self.transpose = 0;
        self.percussion = 0;
        self.phrase_repeat = 0;
        self.tempo_fraction = 0;
        // A two-note interface probe measures about 60 ms of resident song
        // initialization beyond the initial tempo tick.
        self.startup_ticks = 30;
        self.tracks = std::array::from_fn(|_| Track::default());
        self.volume.set(192);
        self.tempo.set(20);
        self.phrase = word(&apu.bus.ram, 0x2b00 + u16::from(music - 1) * 2);
        self.next_phrase(apu);
    }
    fn stop(&mut self, apu: &mut Apu) {
        self.phrase = 0;
        for track in &mut self.tracks {
            track.pc = 0;
            track.wait = 0;
        }
        apu.bus.dsp.write(0x5c, self.music_mask());
    }
    fn music_mask(&self) -> u8 {
        let mut mask = 255;
        if self.effects[0].code != 0 {
            mask &= !(1 << 7);
        }
        if self.effects[1].code != 0 {
            mask &= !(1 << 5);
        }
        mask
    }
    fn next_phrase(&mut self, apu: &mut Apu) {
        // Malformed loops are bounded; a cartridge cannot hang the host player.
        for _ in 0..64 {
            if self.phrase == 0 {
                return;
            }
            let item = word(&apu.bus.ram, self.phrase);
            self.phrase = self.phrase.wrapping_add(2);
            if item == 0 {
                self.stop(apu);
                return;
            }
            if item < 256 {
                if item == 0x80 {
                    self.fast_forward = true;
                    continue;
                }
                if item == 0x81 {
                    self.fast_forward = false;
                    continue;
                }
                let destination = word(&apu.bus.ram, self.phrase);
                self.phrase = self.phrase.wrapping_add(2);
                if item >= 0x82 {
                    self.phrase = destination;
                    continue;
                }
                if self.phrase_repeat == 0 {
                    self.phrase_repeat = item as u8;
                }
                self.phrase_repeat -= 1;
                if self.phrase_repeat != 0 {
                    self.phrase = destination;
                }
                continue;
            }
            for (index, track) in self.tracks.iter_mut().enumerate() {
                track.pc = word(&apu.bus.ram, item.wrapping_add(index as u16 * 2));
                track.wait = 0;
                track.repeats = 0;
            }
            return;
        }
        self.errors = self.errors.saturating_add(1);
        self.stop(apu);
    }
    /// One 1.024 MHz APU clock. Sequencing uses the documented 2 ms timer basis.
    pub fn clock(&mut self, apu: &mut Apu) {
        self.clock += 1;
        if self.clock != 2048 {
            return;
        }
        self.clock = 0;
        self.mute.tick();
        apu.bus.dsp.write(0x0c, self.mute.value as u8);
        apu.bus.dsp.write(0x1c, self.mute.value as u8);
        self.effects_tick(apu);
        if self.paused || self.phrase == 0 {
            return;
        }
        if self.startup_ticks != 0 {
            self.startup_ticks -= 1;
            return;
        }
        self.tempo_fraction += self.tempo.value.clamp(0, 255) as u16;
        if self.tempo_fraction < 256 {
            return;
        }
        self.tempo_fraction -= 256;
        self.music_tick(apu);
        if self.fast_forward {
            for _ in 0..32 {
                if !self.fast_forward || self.phrase == 0 {
                    break;
                }
                self.music_tick(apu);
            }
        }
    }
    fn music_tick(&mut self, apu: &mut Apu) {
        self.tempo.tick();
        self.volume.tick();
        self.echo_left.tick();
        self.echo_right.tick();
        apu.bus.dsp.write(0x2c, self.echo_left.value as u8);
        apu.bus.dsp.write(0x3c, self.echo_right.value as u8);
        for index in 0..8 {
            let mut track = std::mem::take(&mut self.tracks[index]);
            track.pan.tick();
            track.volume.tick();
            if track.pitch_delay != 0 {
                track.pitch_delay -= 1;
            } else {
                track.pitch.tick();
            }
            if track.gate > 0 {
                track.gate -= 1;
                if track.gate == 0 && apu.bus.ram.read(track.pc) != 0xc8 && !self.effect_owns(index)
                {
                    apu.bus.dsp.write(0x5c, 1 << index);
                }
            }
            if track.wait > 0 {
                track.wait -= 1;
            }
            let ended = if track.pc != 0 && track.wait == 0 {
                self.read_track(apu, index, &mut track)
            } else {
                false
            };
            self.update_voice(apu, index, &mut track);
            self.tracks[index] = track;
            if ended {
                self.next_phrase(apu);
                self.start_phrase_tracks(apu);
                break;
            }
        }
    }
    fn start_phrase_tracks(&mut self, apu: &mut Apu) {
        // Do not insert an extra tempo tick at every phrase boundary.
        for _ in 0..64 {
            if self.phrase == 0 {
                return;
            }
            let mut advance = false;
            for index in 0..8 {
                let mut track = std::mem::take(&mut self.tracks[index]);
                let ended = track.pc != 0 && self.read_track(apu, index, &mut track);
                self.update_voice(apu, index, &mut track);
                self.tracks[index] = track;
                if ended {
                    self.next_phrase(apu);
                    advance = true;
                    break;
                }
            }
            if !advance {
                return;
            }
        }
        self.errors = self.errors.saturating_add(1);
        self.stop(apu);
    }
    fn read_track(&mut self, apu: &mut Apu, index: usize, t: &mut Track) -> bool {
        for _ in 0..128 {
            let op = byte(&apu.bus.ram, &mut t.pc);
            match op {
                0 => {
                    if t.repeats != 0 {
                        t.repeats -= 1;
                        t.pc = if t.repeats == 0 {
                            t.return_pc
                        } else {
                            t.subroutine
                        };
                    } else {
                        t.pc = 0;
                        return true;
                    }
                }
                1..=0x7f => {
                    t.duration = op;
                    if apu.bus.ram.read(t.pc) < 128 {
                        t.articulation = byte(&apu.bus.ram, &mut t.pc);
                    }
                }
                0x80..=0xdf => {
                    t.wait = t.duration.max(1);
                    let gate = apu
                        .bus
                        .ram
                        .read(0x4c10 + u16::from(t.articulation >> 4 & 7));
                    t.gate = ((u16::from(t.wait) * u16::from(gate)) >> 8).max(1) as u8;
                    if op == 0xc9 {
                        if !self.effect_owns(index) {
                            apu.bus.dsp.write(0x5c, 1 << index);
                        }
                    } else if op != 0xc8 {
                        if op >= 0xca {
                            t.instrument = self.percussion.wrapping_add(op - 0xca);
                            t.note = 0xa4;
                        } else {
                            t.note = op;
                        }
                        let note = (i32::from(t.note) - 128
                            + i32::from(self.transpose)
                            + i32::from(t.transpose))
                            * 256
                            + i32::from(t.fine);
                        t.pitch.set(note);
                        t.pitch_delay = t.envelope_delay;
                        if t.envelope_from {
                            t.pitch.value += i32::from(t.envelope_offset) * 256;
                        } else {
                            t.pitch.target += i32::from(t.envelope_offset) * 256;
                        }
                        t.pitch.left = u16::from(t.envelope_steps);
                        t.vibrato.age = 0;
                        t.tremolo.age = 0;
                        if !t.muted && !self.effect_owns(index) && !self.fast_forward {
                            self.instrument(apu, index, t.instrument);
                            self.update_voice(apu, index, t);
                            apu.bus.dsp.write(0x5c, 0);
                            apu.bus.dsp.write(0x4c, 1 << index);
                            self.notes = self.notes.saturating_add(1);
                        }
                    }
                    return false;
                }
                0xe0 => t.instrument = byte(&apu.bus.ram, &mut t.pc),
                0xe1 => t.pan.set(i32::from(byte(&apu.bus.ram, &mut t.pc))),
                0xe2 => {
                    let steps = byte(&apu.bus.ram, &mut t.pc);
                    t.pan.to(i32::from(byte(&apu.bus.ram, &mut t.pc)), steps);
                }
                0xe3 | 0xeb => {
                    let m = Modulation {
                        delay: byte(&apu.bus.ram, &mut t.pc),
                        rate: byte(&apu.bus.ram, &mut t.pc),
                        depth: byte(&apu.bus.ram, &mut t.pc),
                        ..Modulation::default()
                    };
                    if op == 0xe3 {
                        t.vibrato = m;
                    } else {
                        t.tremolo = m;
                    }
                }
                0xe4 => t.vibrato = Modulation::default(),
                0xec => t.tremolo = Modulation::default(),
                0xe5 => self.volume.set(i32::from(byte(&apu.bus.ram, &mut t.pc))),
                0xe6 | 0xe8 | 0xee => {
                    let steps = byte(&apu.bus.ram, &mut t.pc);
                    let value = i32::from(byte(&apu.bus.ram, &mut t.pc));
                    match op {
                        0xe6 => self.volume.to(value, steps),
                        0xe8 => self.tempo.to(value, steps),
                        _ => t.volume.to(value, steps),
                    }
                }
                0xe7 => self.tempo.set(i32::from(byte(&apu.bus.ram, &mut t.pc))),
                0xe9 => self.transpose = i16::from(byte(&apu.bus.ram, &mut t.pc) as i8),
                0xea => t.transpose = i16::from(byte(&apu.bus.ram, &mut t.pc) as i8),
                0xed => t.volume.set(i32::from(byte(&apu.bus.ram, &mut t.pc))),
                0xef => {
                    let address = word(&apu.bus.ram, t.pc);
                    t.pc = t.pc.wrapping_add(2);
                    let count = byte(&apu.bus.ram, &mut t.pc);
                    if t.repeats != 0 || count == 0 {
                        self.errors = self.errors.saturating_add(1);
                        t.pc = 0;
                        return true;
                    }
                    t.subroutine = address;
                    t.return_pc = t.pc;
                    t.repeats = count;
                    t.pc = address;
                }
                0xf0 => t.vibrato.fade = byte(&apu.bus.ram, &mut t.pc),
                0xf1 | 0xf2 => {
                    t.envelope_delay = byte(&apu.bus.ram, &mut t.pc);
                    t.envelope_steps = byte(&apu.bus.ram, &mut t.pc);
                    t.envelope_offset = i16::from(byte(&apu.bus.ram, &mut t.pc) as i8);
                    t.envelope_from = op == 0xf2;
                }
                0xf3 => {
                    t.envelope_steps = 0;
                    t.envelope_offset = 0;
                }
                0xf4 => t.fine = byte(&apu.bus.ram, &mut t.pc),
                0xf5 => {
                    apu.bus.dsp.write(0x4d, byte(&apu.bus.ram, &mut t.pc));
                    self.echo_left
                        .set(i32::from(byte(&apu.bus.ram, &mut t.pc) as i8));
                    self.echo_right
                        .set(i32::from(byte(&apu.bus.ram, &mut t.pc) as i8));
                    apu.bus.dsp.write(0x6c, apu.bus.dsp.read(0x6c) & !32);
                }
                0xf6 => {
                    apu.bus.dsp.write(0x4d, 0);
                    self.echo_left.set(0);
                    self.echo_right.set(0);
                    apu.bus.dsp.write(0x6c, apu.bus.dsp.read(0x6c) | 32);
                }
                0xf7 => {
                    apu.bus
                        .dsp
                        .write(0x7d, byte(&apu.bus.ram, &mut t.pc).min(2));
                    apu.bus.dsp.write(0x0d, byte(&apu.bus.ram, &mut t.pc));
                    let filter = byte(&apu.bus.ram, &mut t.pc);
                    for tap in 0..8 {
                        let coefficient = match (filter & 3, tap) {
                            (0, 0) => 127,
                            (1, 0) => 96,
                            (1, 1) => -48,
                            (2, 0) => 64,
                            (2, 1) => 32,
                            (2, 2) => 16,
                            (3, 0) => 64,
                            (3, 2) => -64,
                            _ => 0,
                        };
                        apu.bus.dsp.write(tap * 16 + 15, coefficient as u8);
                    }
                }
                0xf8 => {
                    let steps = byte(&apu.bus.ram, &mut t.pc);
                    self.echo_left
                        .to(i32::from(byte(&apu.bus.ram, &mut t.pc) as i8), steps);
                    self.echo_right
                        .to(i32::from(byte(&apu.bus.ram, &mut t.pc) as i8), steps);
                }
                0xf9 => {
                    t.pitch_delay = byte(&apu.bus.ram, &mut t.pc);
                    let steps = byte(&apu.bus.ram, &mut t.pc);
                    let note = byte(&apu.bus.ram, &mut t.pc);
                    t.pitch.to(
                        (i32::from(note) - 128
                            + i32::from(self.transpose)
                            + i32::from(t.transpose))
                            * 256
                            + i32::from(t.fine),
                        steps,
                    );
                }
                0xfa => self.percussion = byte(&apu.bus.ram, &mut t.pc),
                0xfb => {
                    byte(&apu.bus.ram, &mut t.pc);
                    byte(&apu.bus.ram, &mut t.pc);
                }
                0xfc => t.muted = true,
                0xfd => self.fast_forward = true,
                0xfe => self.fast_forward = false,
                _ => {
                    self.errors = self.errors.saturating_add(1);
                    t.pc = 0;
                    return true;
                }
            }
        }
        self.errors = self.errors.saturating_add(1);
        t.pc = 0;
        true
    }
    fn instrument(&self, apu: &mut Apu, index: usize, id: u8) {
        let descriptor = 0x4c30 + u16::from(id & 127) * 6;
        let sample = apu.bus.ram.read(descriptor);
        let bit = 1 << index;
        let non = apu.bus.dsp.read(0x3d);
        apu.bus.dsp.write(
            0x3d,
            if sample & 128 != 0 {
                non | bit
            } else {
                non & !bit
            },
        );
        if sample & 128 != 0 {
            apu.bus
                .dsp
                .write(0x6c, (apu.bus.dsp.read(0x6c) & !31) | (sample & 31));
        }
        for offset in 0..4 {
            apu.bus.dsp.write(
                index as u8 * 16 + 4 + offset,
                apu.bus.ram.read(descriptor + u16::from(offset)),
            );
        }
    }
    fn effect_owns(&self, index: usize) -> bool {
        (index == 7 && self.effects[0].code != 0) || (index == 5 && self.effects[1].code != 0)
    }
    fn update_voice(&self, apu: &mut Apu, index: usize, t: &mut Track) {
        if self.effect_owns(index) {
            return;
        }
        let modulation = t.vibrato.tick();
        let tremolo = t.tremolo.tick().abs();
        let velocity = i32::from(apu.bus.ram.read(0x4c18 + u16::from(t.articulation & 15)));
        let level =
            (t.volume.value.clamp(0, 255) * self.volume.value.clamp(0, 255) / 255 * velocity / 255)
                .clamp(0, 255);
        let level = (level * level / 256 * (255 - tremolo.min(255)) / 255).min(127);
        let pan = t.pan.value as u8;
        let position = i32::from(pan & 31).min(20);
        // Equal-power stereo avoids making centered instruments much quieter
        // than hard-panned instruments. Keep signed DSP volume within ±127.
        let left = (f64::from(level) * (f64::from(20 - position) / 20.0).sqrt()).round() as i32
            * if pan & 128 != 0 { -1 } else { 1 };
        let right = (f64::from(level) * (f64::from(position) / 20.0).sqrt()).round() as i32
            * if pan & 64 != 0 { -1 } else { 1 };
        apu.bus.dsp.write(index as u8 * 16, left as i8 as u8);
        apu.bus.dsp.write(index as u8 * 16 + 1, right as i8 as u8);
        let tuning = 0x4c30 + u16::from(t.instrument & 127) * 6 + 4;
        let multiplier =
            u16::from_be_bytes([apu.bus.ram.read(tuning), apu.bus.ram.read(tuning + 1)]);
        let frequency = (u32::from(pitch(t.pitch.value + modulation)) * u32::from(multiplier) / 256)
            .min(16383) as u16;
        apu.bus.dsp.write(index as u8 * 16 + 2, frequency as u8);
        apu.bus
            .dsp
            .write(index as u8 * 16 + 3, (frequency >> 8) as u8);
    }
    fn effects_tick(&mut self, apu: &mut Apu) {
        for index in 0..2 {
            let mut effect = std::mem::take(&mut self.effects[index]);
            if effect.code == 0 {
                continue;
            }
            let voice = if index == 0 { 7 } else { 5 };
            let reg = voice as u8 * 16;
            let instrument = if index == 0 {
                match effect.code {
                    9 => 47,
                    11 | 29 | 30 => 19,
                    12 | 13 | 44 => 60,
                    14..=17 | 37..=39 => 54,
                    18 => 62,
                    19 | 47 => 56,
                    20 | 21 | 46 | 48 => 7,
                    22 | 25 => 59,
                    26 | 41 => 47,
                    28 => 58,
                    40 => 40,
                    _ => 35,
                }
            } else {
                match effect.code {
                    1..=3 => 55,
                    4 => 62,
                    5 | 7 | 11 | 12 => 61,
                    6 | 8 => 60,
                    9 | 10 | 13 | 18 | 19 => 56,
                    14 | 15 => 59,
                    16 | 17 => 40,
                    20 | 25 => 35,
                    21 | 24 => 58,
                    22 | 23 => 19,
                    _ => 35,
                }
            };
            // Applause and engines use successive original transients, rather
            // than looping a tiny, pitched noise waveform indefinitely.
            let retrigger = index == 1
                && matches!(effect.code,1..=3|6|8..=10|13..=15|18|19)
                && effect.age
                    % (if effect.code <= 3 {
                        45 + u16::from(effect.code) * 15
                    } else {
                        90
                    })
                    == 0;
            if effect.age == 0 || retrigger {
                self.instrument(apu, voice, instrument);
                apu.bus.dsp.write(reg + 5, 0);
                apu.bus.dsp.write(reg + 7, 127);
                apu.bus.dsp.write(0x5c, 0);
                apu.bus.dsp.write(0x4c, 1 << voice);
            }
            // Original procedural substitutes: transient A, sustained/periodic B.
            let life = if index == 0 {
                120 + u16::from(effect.code % 7) * 35
            } else {
                u16::MAX
            };
            let level = match effect.volume {
                0 => 50,
                1 => 34,
                2 => 20,
                _ if index == 1 => 50,
                _ => 0,
            };
            let attack = i32::from(effect.age.min(4));
            let amplitude = if index == 0 {
                i32::from(life.saturating_sub(effect.age)) * level / i32::from(life)
            } else {
                level * (75 + i32::from(effect.age % 125)) / 200
            };
            apu.bus.dsp.write(reg, (amplitude * attack / 4) as u8);
            apu.bus.dsp.write(reg + 1, (amplitude * attack / 4) as u8);
            let slide = if index == 0 {
                i32::from(effect.age.min(life)) * 3
            } else {
                i32::from(effect.age % 200)
            };
            let melody = if index == 0 && matches!(effect.code, 1 | 2 | 4..=8 | 35 | 42 | 43) {
                // Our own short UI motifs, not the built-in Nintendo jingles.
                [0, 4, 7, 12][usize::from(effect.age / 50) % 4] * 256
            } else {
                0
            };
            let base = if instrument >= 49 {
                36
            } else {
                36 + i32::from(effect.code % 12)
            };
            let multiplier = if instrument >= 49 { 1960 } else { 528 };
            let frequency = (u32::from(pitch(
                (base + i32::from(effect.pitch) * 4) * 256 - slide + melody,
            )) * multiplier
                / 256)
                .min(16383) as u16;
            apu.bus.dsp.write(reg + 2, frequency as u8);
            apu.bus.dsp.write(reg + 3, (frequency >> 8) as u8);
            effect.age = effect.age.saturating_add(1);
            if index == 0 && effect.age >= life {
                apu.bus.dsp.write(0x5c, 1 << voice);
                effect = Effect::default();
            }
            self.effects[index] = effect;
        }
    }
    pub fn export_state(&self) -> Vec<u8> {
        let mut out = b"RBRE\x02\0".to_vec();
        self.encode(&mut out);
        out
    }
    pub fn import_state(data: &[u8]) -> Result<Self, &'static str> {
        if data.len() > 4096
            || !(data.starts_with(b"RBRE\x01\0") || data.starts_with(b"RBRE\x02\0"))
        {
            return Err("Invalid replacement player state");
        }
        let legacy = if data[4] == 1 {
            let mut bytes = data[6..].to_vec();
            bytes.push(0);
            Some(bytes)
        } else {
            None
        };
        let mut input = Reader(legacy.as_deref().unwrap_or(&data[6..]));
        let state = Self::decode(&mut input)?;
        let valid_fade = |f: &Fade| {
            f.left <= 255
                && (-65536..=65536).contains(&f.value)
                && (-65536..=65536).contains(&f.target)
        };
        if !input.0.is_empty()
            || state.clock >= 2048
            || state.tempo_fraction >= 256
            || state.song > 15
            || state.startup_ticks > 30
            || !(-128..=127).contains(&state.transpose)
            || [
                &state.tempo,
                &state.volume,
                &state.mute,
                &state.echo_left,
                &state.echo_right,
            ]
            .iter()
            .any(|f| !valid_fade(f))
            || state.tracks.iter().any(|t| {
                !valid_fade(&t.pan)
                    || !valid_fade(&t.volume)
                    || !valid_fade(&t.pitch)
                    || !(-128..=127).contains(&t.transpose)
            })
            || state
                .effects
                .iter()
                .enumerate()
                .any(|(i, e)| e.code > if i == 0 { 48 } else { 25 } || e.pitch > 3 || e.volume > 3)
        {
            return Err("Invalid replacement player values");
        }
        Ok(state)
    }
}

//! SGB sound command transport and composition with RustBoy's portable SNES APU.
use super::word;
use rustboy_snes_apu::{SpcRam, RAM_BYTES};
use rustboy_snes_apu::{apu::Apu, firmware::load_sgb_firmware};

pub(super) const STATE_BYTES: usize = RAM_BYTES + 4 + 8 + 8 + 1 + 2;
pub(super) const AUDIO_STATE_BASE_BYTES: usize = 16;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub(super) struct Sound {
    pub ram: SpcRam,
    pub request: [u8; 4], // Effect A, effect B, pitch/volume, music score.
    pub uploads: u64,
    pub rejected: u64,
    pub jump: Option<u16>,
    pub apu: Option<Box<Apu>>,
    clock_remainder: u32,
    previous: [i16;2],
    current: [i16;2],
}

impl Sound {
    pub fn load_firmware(&mut self, data:&[u8]) -> Result<(),String> {
        let mut apu = load_sgb_firmware(data)?;
        // SNES firmware initializes its sound processor before the handheld
        // cartridge starts. Bound this HLE initialization, not the host clock.
        let mut ready = false;
        for _ in 0..256 {
            apu.run(1024);
            if apu.bus.dsp.read(0x5d) == 0x4b && apu.bus.dsp.read(0x6c)&128 == 0 { ready = true; break; }
        }
        if !ready { return Err("SGB sound firmware did not initialize".into()); }
        // Real SGB starts muted. Retaining that port value also makes the first
        // SOUND(volume=0) an observable unmute request instead of a no-op.
        apu.bus.input[3] = 0x0c;
        apu.run(8192); apu.drain_samples();
        self.apu = Some(Box::new(apu));
        self.clock_remainder = 0; self.previous=[0;2]; self.current=[0;2];
        Ok(())
    }
    pub fn command(&mut self, request:[u8;4]) {
        self.request = request;
        if let Some(apu) = &mut self.apu {
            // SOUND layout: effect A/B, attributes, music. The SPC ports are
            // music, effect A, effect B, attributes (not packet order).
            apu.bus.input = [request[3],request[0],request[1],request[2]];
        }
    }
    pub fn tick(&mut self, ticks:u32) {
        let Some(apu) = &mut self.apu else { return; };
        self.clock_remainder += ticks*1_024_000;
        while self.clock_remainder >= 4_194_304 {
            self.clock_remainder -= 4_194_304;
            apu.run(1);
            if let Some(sample) = apu.pop_sample() { self.previous=self.current; self.current=sample; }
        }
    }
    pub fn sample(&self) -> [f32;2] {
        let Some(apu) = &self.apu else { return [0.0;2]; };
        let fraction = (f64::from(apu.sample_phase()) + f64::from(self.clock_remainder)/4_194_304.0)/32.0;
        std::array::from_fn(|i|((f64::from(self.previous[i])+(f64::from(self.current[i])-f64::from(self.previous[i]))*fraction)/32768.0) as f32)
    }
    pub fn upload(&mut self, data: &[u8; 4096]) {
        // Validate the entire list before applying any writes. Malformed input
        // cannot leave a partly modified sound program or change its entry point.
        let mut offset = 0;
        let mut writes = Vec::new();
        let mut jump = None;
        while offset < data.len() {
            if offset + 4 > data.len() {
                self.rejected = self.rejected.saturating_add(1);
                return;
            }
            let size = word(data, offset) as usize;
            let address = word(data, offset + 2);
            offset += 4;
            if size == 0 {
                jump = Some(address);
                break;
            }
            if size > data.len() - offset {
                self.rejected = self.rejected.saturating_add(1);
                return;
            }
            writes.push((address, offset, size));
            offset += size;
        }
        for (address, start, size) in writes {
            self.ram.write_wrapping(address, &data[start..start + size]);
            if let Some(apu) = &mut self.apu { apu.bus.ram.write_wrapping(address,&data[start..start+size]); }
        }
        if jump.is_some() {
            self.jump = jump;
            if let Some(apu) = &mut self.apu { apu.start(jump.unwrap()); }
        }
        self.uploads = self.uploads.saturating_add(1);
    }

    pub fn export_state(&self, out: &mut Vec<u8>) {
        out.extend_from_slice(self.ram.bytes());
        out.extend_from_slice(&self.request);
        out.extend_from_slice(&self.uploads.to_le_bytes());
        out.extend_from_slice(&self.rejected.to_le_bytes());
        out.push(self.jump.is_some() as u8);
        out.extend_from_slice(&self.jump.unwrap_or(0).to_le_bytes());
    }

    pub fn import_state(data: &[u8]) -> Result<Self, String> {
        if data.len() != STATE_BYTES {
            return Err("Invalid SGB sound snapshot length".into());
        }
        let jump = match data[STATE_BYTES - 3] {
            0 if word(data, STATE_BYTES - 2) == 0 => None,
            1 => Some(word(data, STATE_BYTES - 2)),
            _ => return Err("Invalid SGB sound jump flag".into()),
        };
        Ok(Self {
            ram: SpcRam::from_bytes(&data[..RAM_BYTES])?,
            request: data[RAM_BYTES..RAM_BYTES + 4].try_into().unwrap(),
            uploads: u64::from_le_bytes(data[RAM_BYTES + 4..RAM_BYTES + 12].try_into().unwrap()),
            rejected: u64::from_le_bytes(data[RAM_BYTES + 12..RAM_BYTES + 20].try_into().unwrap()),
            jump,
            ..Self::default()
        })
    }
    pub fn export_audio_state(&self,out:&mut Vec<u8>) {
        out.extend_from_slice(&self.clock_remainder.to_le_bytes());
        for sample in self.previous.iter().chain(&self.current) { out.extend_from_slice(&sample.to_le_bytes()); }
        let bytes = self.apu.as_ref().map(|apu|apu.export_state()).unwrap_or_default();
        out.extend_from_slice(&(bytes.len() as u32).to_le_bytes()); out.extend_from_slice(&bytes);
    }
    pub fn import_audio_state(&mut self,data:&[u8]) -> Result<(),String> {
        if data.len()<AUDIO_STATE_BASE_BYTES { return Err("Truncated SGB audio state".into()); }
        self.clock_remainder = u32::from_le_bytes(data[..4].try_into().unwrap());
        self.previous = [word(data,4) as i16,word(data,6) as i16];
        self.current = [word(data,8) as i16,word(data,10) as i16];
        let size = u32::from_le_bytes(data[12..16].try_into().unwrap()) as usize;
        if self.clock_remainder>=4_194_304 || size != data.len()-16 { return Err("Invalid SGB audio state".into()); }
        self.apu = if size==0 { None } else { Some(Box::new(Apu::import_state(&data[16..])?)) };
        if self.apu.is_none() && (self.clock_remainder!=0 || self.previous!=[0;2] || self.current!=[0;2]) { return Err("Unexpected SGB audio resampler state".into()); }
        Ok(())
    }
}

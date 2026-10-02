//! SGB command/upload transport only. SPC700/DSP playback is not implemented.
use super::word;
use rustboy_snes_apu::{SpcRam, RAM_BYTES};

pub(super) const STATE_BYTES: usize = RAM_BYTES + 4 + 8 + 8 + 1 + 2;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub(super) struct Sound {
    pub ram: SpcRam,
    pub request: [u8; 4], // Effect A, effect B, pitch/volume, music score.
    pub uploads: u64,
    pub rejected: u64,
    pub jump: Option<u16>,
}

impl Sound {
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
        }
        if jump.is_some() {
            self.jump = jump;
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
        })
    }
}

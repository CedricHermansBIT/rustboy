//! Game-transferred palettes and attribute files, not Nintendo system assets.
use super::word;

pub(super) const STATE_BYTES: usize = 4096 + 45 * 90;

#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) struct Tables {
    palettes: Box<[[u16; 4]; 512]>,
    attributes: Box<[u8; 45 * 90]>,
}

impl Default for Tables {
    fn default() -> Self {
        Self {
            // PAL_SET before PAL_TRN has no defined game-provided colors.
            // Keep our neutral fallback rather than copying system firmware.
            palettes: Box::new([[0x7FFF, 0x56B5, 0x294A, 0]; 512]),
            attributes: Box::new([0; 45 * 90]),
        }
    }
}

impl Tables {
    pub fn transfer(&mut self, destination: u8, data: &[u8; 4096]) {
        match destination {
            4 => {
                for (index, color) in self.palettes.iter_mut().flatten().enumerate() {
                    *color = word(data, index * 2) & 0x7FFF;
                }
            }
            5 => self.attributes.copy_from_slice(&data[..45 * 90]),
            _ => unreachable!("validated table transfer destination"),
        }
    }

    pub fn palette(&self, index: u16) -> [u16; 4] {
        // The SGB palette address has nine bits; upper ID bits are unused.
        self.palettes[(index & 0x1FF) as usize]
    }

    pub fn attributes(&self, index: u8) -> Option<[u8; 360]> {
        if index >= 45 {
            return None;
        }
        let start = index as usize * 90;
        Some(std::array::from_fn(|i| {
            (self.attributes[start + i / 4] >> (6 - 2 * (i % 4))) & 3
        }))
    }

    pub fn export_state(&self, out: &mut Vec<u8>) {
        for color in self.palettes.iter().flatten() {
            out.extend_from_slice(&color.to_le_bytes());
        }
        out.extend_from_slice(self.attributes.as_ref());
    }

    pub fn import_state(data: &[u8]) -> Result<Self, String> {
        if data.len() != STATE_BYTES {
            return Err("Invalid SGB table snapshot length".into());
        }
        let mut tables = Self::default();
        for (index, color) in tables.palettes.iter_mut().flatten().enumerate() {
            *color = word(data, index * 2);
            if *color > 0x7FFF {
                return Err("Invalid SGB system palette in snapshot".into());
            }
        }
        tables.attributes.copy_from_slice(&data[4096..]);
        Ok(tables)
    }
}

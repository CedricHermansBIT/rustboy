//! Game-provided SGB border data. No built-in Nintendo artwork or SNES firmware.
//! Format: https://gbdev.io/pandocs/SGB_Command_Border.html
use super::{rgb, word};

pub(super) const WIDTH: usize = 256;
pub(super) const HEIGHT: usize = 224;
pub(super) const STATE_BYTES: usize = 1 + 8192 + 32 * 29 * 2 + 3 * 16 * 2;

#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) struct Border {
    pub active: bool,
    tiles: Vec<u8>,
    map: [u16; 32 * 29],
    palettes: [[u16; 16]; 3],
}

impl Default for Border {
    fn default() -> Self {
        Self {
            active: false,
            tiles: vec![0; 8192],
            map: [0; 32 * 29],
            palettes: [[0; 16]; 3],
        }
    }
}

impl Border {
    pub fn transfer(&mut self, destination: u8, data: &[u8; 4096]) {
        match destination {
            1 | 2 => {
                let offset = (destination as usize - 1) * 4096;
                self.tiles[offset..offset + 4096].copy_from_slice(data);
            }
            3 => {
                for (i, entry) in self.map.iter_mut().enumerate() {
                    *entry = word(data, i * 2);
                }
                for (i, color) in self.palettes.iter_mut().flatten().enumerate() {
                    *color = word(data, 0x800 + i * 2) & 0x7FFF;
                }
                self.active = true;
            }
            _ => unreachable!("validated border transfer destination"),
        }
    }

    /// Draw only nonzero border pixels, over both the backdrop and game window.
    /// Priority bit 13 is not used by the SGB border layer.
    pub fn overlay(&self, out: &mut [u8]) {
        for y in 0..HEIGHT {
            for x in 0..WIDTH {
                let entry = self.map[(y / 8) * 32 + x / 8];
                let tile = (entry & 0x3FF) as usize;
                let palette = ((entry >> 10) & 7) as usize;
                if tile >= 256 || !(4..=6).contains(&palette) {
                    continue;
                }
                let row = if entry & 0x8000 != 0 {
                    7 - y % 8
                } else {
                    y % 8
                };
                let bit = if entry & 0x4000 != 0 {
                    x % 8
                } else {
                    7 - x % 8
                };
                let offset = tile * 32 + row * 2;
                let color = ((self.tiles[offset] >> bit) & 1)
                    | (((self.tiles[offset + 1] >> bit) & 1) << 1)
                    | (((self.tiles[offset + 16] >> bit) & 1) << 2)
                    | (((self.tiles[offset + 17] >> bit) & 1) << 3);
                if color != 0 {
                    let offset = (y * WIDTH + x) * 4;
                    out[offset..offset + 4]
                        .copy_from_slice(&rgb(self.palettes[palette - 4][color as usize]));
                }
            }
        }
    }

    pub fn export_state(&self, out: &mut Vec<u8>) {
        out.push(self.active as u8);
        out.extend_from_slice(&self.tiles);
        for value in &self.map {
            out.extend_from_slice(&value.to_le_bytes());
        }
        for value in self.palettes.iter().flatten() {
            out.extend_from_slice(&value.to_le_bytes());
        }
    }

    pub fn import_state(data: &[u8]) -> Result<Self, String> {
        if data.len() != STATE_BYTES || data[0] > 1 {
            return Err("Invalid SGB border snapshot".into());
        }
        let mut border = Self::default();
        border.active = data[0] != 0;
        border.tiles.copy_from_slice(&data[1..8193]);
        for (i, value) in border.map.iter_mut().enumerate() {
            *value = word(data, 8193 + i * 2);
        }
        let offset = 8193 + 32 * 29 * 2;
        for (i, value) in border.palettes.iter_mut().flatten().enumerate() {
            *value = word(data, offset + i * 2);
            if *value > 0x7FFF {
                return Err("Invalid SGB border palette in snapshot".into());
            }
        }
        Ok(border)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn four_planes_both_tile_halves_three_palettes_and_flips() {
        let mut border = Border::default();
        let mut low = [0; 4096];
        // Tile 1 has colors 1, 2, 4 and 8 at its four corners.
        low[32] = 0x80;
        low[33] = 1;
        low[32 + 30] = 0x80;
        low[32 + 31] = 1;
        border.transfer(1, &low);
        let mut high = [0; 4096];
        high[0] = 0x80;
        high[1] = 0x80;
        high[16] = 0x80;
        high[17] = 0x80;
        border.transfer(2, &high); // tile 128, color 15
        let mut map = [0; 4096];
        for (index, entry) in [0x1001u16, 0x5401, 0x9801, 0xD801, 0x1880]
            .iter()
            .enumerate()
        {
            map[index * 2..index * 2 + 2].copy_from_slice(&entry.to_le_bytes());
        }
        for (palette, color, rgb) in [
            (0, 1, 31u16),
            (1, 2, 0x3E0),
            (2, 4, 0x7C00),
            (2, 8, 0x3FF),
            (2, 15, 0x7FFF),
        ] {
            let offset = 0x800 + palette * 32 + color * 2;
            map[offset..offset + 2].copy_from_slice(&rgb.to_le_bytes());
        }
        border.transfer(3, &map);
        let mut output = vec![0; WIDTH * HEIGHT * 4];
        border.overlay(&mut output);
        let pixel = |x, y| &output[(y * WIDTH + x) * 4..(y * WIDTH + x + 1) * 4];
        assert_eq!(pixel(0, 0), [255, 0, 0, 255]);
        assert_eq!(pixel(8, 0), [0, 255, 0, 255]);
        assert_eq!(pixel(16, 0), [0, 0, 255, 255]);
        assert_eq!(pixel(24, 0), [255, 255, 0, 255]);
        assert_eq!(pixel(32, 0), [255; 4]);
        assert_eq!(pixel(1, 0), [0; 4]); // color zero never overwrites the game
        assert_eq!(
            Border::import_state(&{
                let mut data = Vec::new();
                border.export_state(&mut data);
                data
            })
            .unwrap(),
            border
        );
    }

    #[test]
    fn invalid_tile_and_palette_entries_are_transparent_not_out_of_bounds() {
        let mut border = Border::default();
        border.tiles.fill(255);
        for entry in [0x13FF, 0, 0x1C00, 0x0C00] {
            border.map.fill(entry);
            let mut out = vec![77; WIDTH * HEIGHT * 4];
            border.overlay(&mut out);
            assert!(out.iter().all(|&value| value == 77));
        }
    }
}

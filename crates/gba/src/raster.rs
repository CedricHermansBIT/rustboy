//! Preserve displayed pixels before visible graphics mutations. Unchanged
//! scanlines retain the fast compositor; changed lines retain a committed prefix.
pub(crate) const STATE_BYTES: usize = 965;
pub(crate) struct Raster {
    pub enabled: bool,
    pub clock: u16,
    pub drawn: u16,
    pub pixels: [u8; 960],
}
impl Default for Raster {
    fn default() -> Self {
        Self {
            enabled: false,
            clock: 0,
            drawn: 0,
            pixels: [0; 960],
        }
    }
}
impl crate::bus::Bus {
    pub(crate) fn raster_before_write(&mut self, address: u32, value: u8) {
        let region = address >> 24;
        if !self.raster.enabled
            || self.io16(6) >= 160
            || self.raster.clock >= 1006
            || !(matches!(region, 5..=7) || region == 4 && address & 0xffffff <= 0x55)
        {
            return;
        }
        let old = if region == 4 {
            self.io[(address & 0x3ff) as usize]
        } else {
            self.peek8(address)
        };
        if old == value {
            return;
        }
        let clock = u64::from(self.raster.clock)
            + self.execution.credited
            + self.execution.pending
            + self.clocks;
        let x = ((clock.saturating_sub(46) / 4).min(240)) as u16;
        if x <= self.raster.drawn {
            return;
        }
        let mut row = [0; 960];
        crate::ppu::render_row(self, self.io16(6), &mut row);
        let range = usize::from(self.raster.drawn) * 4..usize::from(x) * 4;
        self.raster.pixels[range.clone()].copy_from_slice(&row[range]);
        self.raster.drawn = x;
    }
}

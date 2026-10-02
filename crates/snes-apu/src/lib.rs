//! Reusable SNES audio components for SGB and future SNES composition.
//! The memory and SPC700 interpreter contain no system firmware, sound engine,
//! samples or platform-specific audio APIs. DSP composition is built separately.
pub const RAM_BYTES: usize = 65536;
pub mod apu;
pub mod dsp;
pub mod firmware;
pub mod spc700;
#[cfg(test)]
mod spc700_tests;
mod state;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SpcRam(Box<[u8; RAM_BYTES]>);

impl Default for SpcRam {
    fn default() -> Self {
        Self(Box::new([0; RAM_BYTES]))
    }
}

impl SpcRam {
    pub fn read(&self, address: u16) -> u8 {
        self.0[address as usize]
    }
    pub fn write(&mut self, address: u16, value: u8) {
        self.0[address as usize] = value;
    }
    pub fn write_wrapping(&mut self, address: u16, data: &[u8]) {
        for (offset, &value) in data.iter().enumerate() {
            self.write(address.wrapping_add(offset as u16), value);
        }
    }
    pub fn bytes(&self) -> &[u8; RAM_BYTES] {
        &self.0
    }
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, &'static str> {
        if bytes.len() != RAM_BYTES {
            return Err("Invalid SPC RAM snapshot length");
        }
        let mut ram = Self::default();
        ram.0.copy_from_slice(bytes);
        Ok(ram)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn address_wrap_and_snapshot_cover_all_spc_memory() {
        let mut ram = SpcRam::default();
        ram.write_wrapping(0xFFFE, &[1, 2, 3, 4]);
        assert_eq!(
            [ram.read(0xFFFE), ram.read(0xFFFF), ram.read(0), ram.read(1)],
            [1, 2, 3, 4]
        );
        assert_eq!(SpcRam::from_bytes(ram.bytes()).unwrap(), ram);
        assert!(SpcRam::from_bytes(&ram.bytes()[..65535]).is_err());
    }
}

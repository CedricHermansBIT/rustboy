//! Locate the sound boot upload in user-supplied SGB SNES firmware. No firmware
//! bytes, instrument bank or fixed offsets from a particular revision are stored.
use crate::apu::Apu;

fn word(bytes: &[u8], offset: usize) -> u16 {
    u16::from_le_bytes([bytes[offset], bytes[offset + 1]])
}

fn packets(bytes: &[u8], offset: usize) -> Option<Vec<(u16, &[u8])>> {
    let mut position = offset;
    let mut blocks = Vec::new();
    let mut total = 0usize;
    while position + 4 <= bytes.len() && blocks.len() < 16 {
        let size = usize::from(word(bytes, position));
        let address = word(bytes, position + 2);
        position += 4;
        if size == 0 {
            return if address == 0x400
                && total >= 32768
                && [0x400, 0x4b00, 0x4c30, 0x4db0]
                    .iter()
                    .all(|wanted| blocks.iter().any(|(address, _)| address == wanted))
            {
                Some(blocks)
            } else {
                None
            };
        }
        if usize::from(address) + size > 65536 || position + size > bytes.len() {
            return None;
        }
        blocks.push((address, &bytes[position..position + size]));
        position += size;
        total += size;
    }
    None
}

pub fn load_sgb_firmware(bytes: &[u8]) -> Result<Apu, String> {
    // Accept normal headerless or copier-header SGB1/SGB2 LoROM images only.
    let bytes = if matches!(bytes.len(), 262656 | 524800) {
        &bytes[512..]
    } else {
        bytes
    };
    if !matches!(bytes.len(), 262144 | 524288)
        || !bytes[0x7fc0..0x7fd5].starts_with(b"Super GAMEBOY")
    {
        return Err(
            "Expected an SGB1/SGB2 SNES firmware image, not a 256-byte Game Boy boot ROM".into(),
        );
    }
    let mut found = None;
    for offset in 0..bytes.len() - 4 {
        let size = word(bytes, offset);
        // The instrument descriptors precede the program/directory/samples.
        // Starting at the program packet alone loses the instrument bank.
        if word(bytes, offset + 2) != 0x4c30 || !(256..=16384).contains(&size) {
            continue;
        }
        if let Some(blocks) = packets(bytes, offset) {
            if found.is_some() {
                return Err("Ambiguous SGB sound firmware upload".into());
            }
            found = Some(blocks);
        }
    }
    let blocks = found.ok_or("SGB sound boot upload not found in firmware")?;
    let mut apu = Apu::default();
    for (address, data) in blocks {
        apu.bus.ram.write_wrapping(address, data);
    }
    apu.start(0x400);
    Ok(apu)
}

#[cfg(test)]
mod tests {
    use super::*;
    fn image() -> Vec<u8> {
        let mut image = vec![0; 262144];
        image[0x7fc0..0x7fc0 + 13].copy_from_slice(b"Super GAMEBOY");
        let mut offset = 0x10000;
        for (address, size) in [
            (0x4c30u16, 378u16),
            (0x4c10, 24),
            (0x400, 512),
            (0x4b00, 256),
            (0x4db0, 32768),
        ] {
            image[offset..offset + 2].copy_from_slice(&size.to_le_bytes());
            image[offset + 2..offset + 4].copy_from_slice(&address.to_le_bytes());
            image[offset + 4..offset + 4 + usize::from(size)].fill(0x55);
            offset += 4 + usize::from(size);
        }
        image[offset + 2..offset + 4].copy_from_slice(&0x400u16.to_le_bytes());
        image
    }
    #[test]
    fn firmware_extraction_validates_before_loading_and_accepts_copier_header() {
        let image = image();
        let apu = load_sgb_firmware(&image).unwrap();
        assert_eq!(apu.cpu.pc, 0x400);
        assert_eq!(apu.bus.ram.read(0x400), 0x55);
        assert_eq!(apu.bus.ram.read(0x4b00), 0x55);
        let mut header = vec![0; 512];
        header.extend_from_slice(&image);
        assert_eq!(load_sgb_firmware(&header).unwrap(), apu);
        assert!(load_sgb_firmware(&[0; 256]).is_err());
        let mut corrupt = image;
        corrupt[0x10000..0x10002].fill(0xff);
        assert!(load_sgb_firmware(&corrupt).is_err());
    }
}

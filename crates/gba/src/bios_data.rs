//! Original functional implementations of the GBATEK-documented data SWIs.
//! Accesses use the bus, including word/halfword writes required by video RAM.
//! Firmware register clobbers and exact instruction timing are not reproduced.
use crate::{bus::Bus, cpu::Memory};

pub(super) fn unpack(
    bus: &mut Bus,
    source: u32,
    destination: u32,
    info: u32,
) -> Result<(), &'static str> {
    let length = usize::from(bus.read16(info, false));
    let input_bits = bus.read8(info.wrapping_add(2));
    let output_bits = bus.read8(info.wrapping_add(3));
    let offset_flags = bus.read32(info.wrapping_add(4), false);
    if !matches!(input_bits, 1 | 2 | 4 | 8)
        || !matches!(output_bits, 1 | 2 | 4 | 8 | 16 | 32)
        || output_bits < input_bits
        || destination & 3 != 0
    {
        return Err("Invalid BIOS BitUnPack widths or destination alignment");
    }
    let output_length = length * usize::from(output_bits) / usize::from(input_bits);
    if output_length & 3 != 0 {
        return Err("BIOS BitUnPack output must contain whole words");
    }
    let mask = (1u32 << input_bits) - 1;
    let offset = offset_flags & 0x7fff_ffff;
    let mut words = Vec::with_capacity(output_length / 4);
    let mut word = 0u32;
    let mut position = 0;
    for n in 0..length {
        let byte = u32::from(bus.read8(source.wrapping_add(n as u32)));
        for shift in (0..8).step_by(usize::from(input_bits)) {
            let raw = (byte >> shift) & mask;
            let value = if raw != 0 || offset_flags >> 31 != 0 {
                raw.wrapping_add(offset)
            } else {
                0
            };
            if output_bits != 32 && value >> output_bits != 0 {
                return Err("BIOS BitUnPack offset exceeds destination width");
            }
            word |= value << position;
            position += output_bits;
            if position == 32 {
                words.push(word);
                word = 0;
                position = 0;
            }
        }
    }
    for (n, word) in words.into_iter().enumerate() {
        bus.write32(destination.wrapping_add(n as u32 * 4), word, n != 0);
    }
    Ok(())
}

pub(super) fn differential(
    bus: &mut Bus,
    source: u32,
    destination: u32,
    service: u8,
) -> Result<(), &'static str> {
    let header = bus.read32(source, false);
    let length = (header >> 8) as usize;
    let wide = service == 0x18;
    if source & 3 != 0
        || header as u8 != if wide { 0x82 } else { 0x81 }
        || (wide && length & 1 != 0)
        || (service != 0x16 && destination & 1 != 0)
    {
        return Err("Invalid BIOS differential header, size or alignment");
    }
    let mut output = Vec::with_capacity(length);
    let mut value = 0u16;
    for n in (0..length).step_by(if wide { 2 } else { 1 }) {
        let address = source.wrapping_add(4).wrapping_add(n as u32);
        if wide {
            value = value.wrapping_add(bus.read16(address, n != 0));
            output.extend_from_slice(&value.to_le_bytes());
        } else {
            value = u16::from((value as u8).wrapping_add(bus.read8(address)));
            output.push(value as u8);
        }
    }
    if service == 0x16 {
        for (n, byte) in output.into_iter().enumerate() {
            bus.write8(destination.wrapping_add(n as u32), byte);
        }
    } else {
        // As with our LZ/RL VRAM services, an odd final byte is padded with zero.
        for (n, pair) in output.chunks(2).enumerate() {
            bus.write16(
                destination.wrapping_add(n as u32 * 2),
                u16::from_le_bytes([pair[0], pair.get(1).copied().unwrap_or(0)]),
                n != 0,
            );
        }
    }
    Ok(())
}

pub(super) fn huffman(bus: &mut Bus, source: u32, destination: u32) -> Result<(), &'static str> {
    let header = bus.read32(source, false);
    let bits = header as u8 & 15;
    let length = (header >> 8) as usize;
    if source & 3 != 0
        || destination & 3 != 0
        || header as u8 >> 4 != 2
        || !matches!(bits, 4 | 8)
        || length & 3 != 0
    {
        return Err("Invalid BIOS Huffman header, size or alignment");
    }
    // The size byte is part of the tree span; index 1 is the root. Relative
    // indices preserve absolute address parity because source + 4 is aligned.
    let tree_start = source.wrapping_add(4);
    let tree_length = (usize::from(bus.read8(tree_start)) + 1) * 2;
    let mut tree = [0u8; 512];
    for (n, byte) in tree.iter_mut().enumerate().take(tree_length).skip(1) {
        *byte = bus.read8(tree_start.wrapping_add(n as u32));
    }
    let mut cursor = tree_start.wrapping_add(tree_length as u32);
    if cursor & 3 != 0 {
        return Err("BIOS Huffman bitstream must be word aligned");
    }
    let mut output = Vec::with_capacity(length / 4);
    let mut packed = 0u32;
    let mut packed_bits = 0;
    let mut stream = 0u32;
    let mut remaining = 0;
    let mut node = 1;
    while output.len() < length / 4 {
        if remaining == 0 {
            stream = bus.read32(cursor, false);
            cursor = cursor.wrapping_add(4);
            remaining = 32;
        }
        let branch = (stream >> 31) as usize;
        stream <<= 1;
        remaining -= 1;
        let descriptor = tree[node];
        let child = (node & !1) + usize::from(descriptor & 63) * 2 + 2 + branch;
        if child >= tree_length {
            return Err("BIOS Huffman child lies outside tree");
        }
        if descriptor & (0x80 >> branch) != 0 {
            let value = u32::from(tree[child]);
            if bits == 4 && value > 15 {
                return Err("BIOS Huffman nibble leaf exceeds data width");
            }
            packed |= value << packed_bits;
            packed_bits += bits;
            if packed_bits == 32 {
                output.push(packed);
                packed = 0;
                packed_bits = 0;
            }
            node = 1;
        } else {
            // Every edge moves forward, so malformed trees cannot form cycles.
            node = child;
        }
    }
    for (n, word) in output.into_iter().enumerate() {
        bus.write32(destination.wrapping_add(n as u32 * 4), word, n != 0);
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    fn bus() -> Bus {
        Bus::new(vec![0; 192], None).unwrap()
    }
    #[test]
    fn unpack_low_bits_first_offset_and_zero_flag_in_video_ram() {
        let mut b = bus();
        b.ewram[0] = 0xa5;
        b.ewram[16..24].copy_from_slice(&[1, 0, 1, 4, 2, 0, 0, 0]);
        unpack(&mut b, 0x02000000, 0x06000000, 0x02000010).unwrap();
        assert_eq!(b.peek32(0x06000000), 0x30300303);
        b.ewram[23] = 0x80;
        unpack(&mut b, 0x02000000, 0x06000004, 0x02000010).unwrap();
        assert_eq!(b.peek32(0x06000004), 0x32322323);
    }
    #[test]
    fn unpack_all_documented_width_pairs_and_multiple_words() {
        for input in [1, 2, 4, 8] {
            for output in [1, 2, 4, 8, 16, 32] {
                if output < input {
                    continue;
                }
                let mut b = bus();
                b.ewram[..4].fill(0xff);
                b.ewram[16..24].copy_from_slice(&[4, 0, input, output, 0, 0, 0, 0]);
                unpack(&mut b, 0x02000000, 0x03000000, 0x02000010).unwrap();
                let value = ((1u16 << input) - 1) as u32;
                let expected = (0..32)
                    .step_by(output as usize)
                    .fold(0, |w, s| w | value << s);
                for n in 0..(output / input) {
                    assert_eq!(b.peek32(0x03000000 + u32::from(n) * 4), expected);
                }
            }
        }
    }
    #[test]
    fn differential_wrapping_and_halfword_video_writes() {
        let mut b = bus();
        b.ewram[..8].copy_from_slice(&[0x81, 4, 0, 0, 250, 10, 255, 2]);
        differential(&mut b, 0x02000000, 0x03000000, 0x16).unwrap();
        differential(&mut b, 0x02000000, 0x06000000, 0x17).unwrap();
        assert_eq!(&b.iwram[..4], &[250, 4, 3, 5]);
        assert_eq!(&b.vram[..4], &[250, 4, 3, 5]);
        b.ewram[..12].copy_from_slice(&[0x82, 8, 0, 0, 255, 255, 2, 0, 255, 255, 0, 128]);
        differential(&mut b, 0x02000000, 0x06000000, 0x18).unwrap();
        assert_eq!(&b.vram[..8], &[255, 255, 1, 0, 0, 0, 0, 128]);
    }
    #[test]
    fn huffman_two_level_byte_and_low_first_nibble_streams() {
        let mut b = bus();
        b.ewram[..16].copy_from_slice(&[
            0x28, 4, 0, 0, 3, 0x80, b'f', 0xc0, b'H', b'u', 0, 0, 0, 0, 0, 0xb0,
        ]);
        huffman(&mut b, 0x02000000, 0x06000000).unwrap();
        assert_eq!(&b.vram[..4], b"Huff");
        b.ewram[..12].copy_from_slice(&[0x24, 4, 0, 0, 1, 0xc0, 1, 2, 0, 0, 0, 0x5a]);
        huffman(&mut b, 0x02000000, 0x06000004).unwrap();
        assert_eq!(&b.vram[4..8], &[0x21, 0x21, 0x12, 0x12]);
    }
    #[test]
    fn huffman_crosses_stream_words_and_uses_node_offset() {
        let mut b = bus();
        b.ewram[..20].copy_from_slice(&[
            0x28, 36, 0, 0, 3, 0xc1, 0, 0, b'A', b'B', 0, 0, 0xaa, 0xaa, 0xaa, 0xaa, 0, 0, 0, 0x50,
        ]);
        huffman(&mut b, 0x02000000, 0x03000000).unwrap();
        assert_eq!(&b.iwram[..32], b"BABABABABABABABABABABABABABABABA");
        assert_eq!(&b.iwram[32..36], b"ABAB");
    }
    #[test]
    fn invalid_parameters_leave_destination_untouched() {
        let mut b = bus();
        b.iwram.fill(0xee);
        b.ewram[16..24].copy_from_slice(&[1, 0, 0, 32, 0, 0, 0, 0]);
        assert!(unpack(&mut b, 0x02000000, 0x03000000, 0x02000010).is_err());
        b.ewram[18] = 8;
        b.ewram[19] = 4;
        assert!(unpack(&mut b, 0x02000000, 0x03000000, 0x02000010).is_err());
        b.ewram[..12].copy_from_slice(&[0x28, 4, 0, 0, 1, 0xff, 1, 2, 0, 0, 0, 0]);
        assert!(huffman(&mut b, 0x02000000, 0x03000000).is_err());
        b.ewram[0] = 0x82;
        b.ewram[1] = 3;
        assert!(differential(&mut b, 0x02000000, 0x03000000, 0x18).is_err());
        assert!(b.iwram.iter().all(|v| *v == 0xee));
    }
}

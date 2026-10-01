//! Original palette/ATF bulk-transfer cartridge; no game or firmware assets.
// Reuse the existing synthetic ROM's instruction-level JOYP transmitter.
#[allow(dead_code)]
#[path = "border_rom.rs"]
mod transport;

pub fn make_rom() -> Vec<u8> {
    let mut rom = vec![0; 32768];
    rom[0x100..0x103].copy_from_slice(&[0xC3, 0x50, 0x01]);
    rom[0x104..0x134].fill(0xAA);
    rom[0x134..0x13D].copy_from_slice(b"SGBPALETT");
    rom[0x143] = 0x80;
    rom[0x146] = 3;
    rom[0x14B] = 0x33;
    for (id, colors) in [
        (0, [0x7FFFu16, 31, 0x3E0, 0x7C00]),
        (1, [0, 0x3E0, 0x7C00, 31]),
        (256, [0x1234, 0x7C00, 31, 0x3E0]),
        (511, [0xFFFF, 0x83FF, 0x7C1F, 0x7FE0]),
    ] {
        for (i, color) in colors.iter().enumerate() {
            let offset = 0x5000 + id * 8 + i * 2;
            rom[offset..offset + 2].copy_from_slice(&color.to_le_bytes());
        }
    }
    rom[0x6000 + 44 * 90..0x6000 + 45 * 90].fill(0x1B);

    let mut code = vec![0xF3, 0xAF, 0xE0, 0x40, 0xE0, 0x42, 0xE0, 0x43];
    // Reproduce the failure: an all-white physical palette is replaced later
    // by PAL_SET. Ignoring that command leaves a running game entirely white.
    let mut white = [0xFF; 16];
    white[0] = 1;
    white[15] = 0;
    transport::packet_bytes(&mut code, &white);
    send(&mut code, 0x17, &[1]);
    code.extend_from_slice(&[0x3E, 0xE4, 0xE0, 0x47]);
    for tile in 0..256 {
        let address = 0x9800 + (tile / 20) * 32 + tile % 20;
        code.extend_from_slice(&[
            0x21,
            address as u8,
            (address >> 8) as u8,
            0x3E,
            tile as u8,
            0x77,
        ]);
    }
    for (source, command) in [(0x5000, 0x0B), (0x6000, 0x15)] {
        code.extend_from_slice(&[
            0xAF,
            0xE0,
            0x40,
            0x11,
            source as u8,
            (source >> 8) as u8,
            0x21,
            0,
            0x80,
            0x01,
            0,
            0x10,
            0x1A,
            0x22,
            0x13,
            0x0B,
            0x78,
            0xB1,
            0x20,
            0xF8,
            0x3E,
            0x91,
            0xE0,
            0x40,
        ]);
        send(&mut code, command, &[]);
        transport::wait_frames(&mut code);
    }
    // Normal game image: BG raw 1 maps to shade 2; sprite raw 1 to shade 3.
    code.extend_from_slice(&[
        0xAF, 0xE0, 0x40, 0x21, 0, 0x98, 0x01, 0, 4, 0xAF, 0x22, 0x0B, 0x78, 0xB1, 0x20, 0xF9,
        0x21, 0, 0x80,
    ]);
    for _ in 0..8 {
        code.extend_from_slice(&[0x3E, 0xFF, 0x22, 0xAF, 0x22]);
    }
    code.extend_from_slice(&[0x21, 0, 0xFE]);
    for value in [16, 16, 0, 0] {
        code.extend_from_slice(&[0x3E, value, 0x22]);
    }
    code.extend_from_slice(&[0x3E, 0xE8, 0xE0, 0x47, 0x3E, 0xFC, 0xE0, 0x48]);
    // Select palettes 0,1,256,511, apply ATF44 and cancel freeze. No MASK_EN
    // cancel packet follows, so both flag semantics are required to pass.
    send(&mut code, 0x0A, &[0, 0, 1, 0, 0, 1, 0xFF, 1, 0xC0 | 44]);
    code.extend_from_slice(&[
        0x3E, 0x93, 0xE0, 0x40, 0x3E, 0x66, 0xEA, 0, 0xC0, 0x18, 0xFE,
    ]);
    assert!(0x150 + code.len() < 0x5000, "code overlaps transfer data");
    rom[0x150..0x150 + code.len()].copy_from_slice(&code);
    rom
}

fn send(code: &mut Vec<u8>, command: u8, payload: &[u8]) {
    let mut bytes = [0; 16];
    bytes[0] = (command << 3) | 1;
    bytes[1..1 + payload.len()].copy_from_slice(payload);
    transport::packet_bytes(code, &bytes);
}

//! Original looping score, transferred through actual cartridge instructions.
#[allow(dead_code)]
#[path = "border_rom.rs"]
mod transport;

pub fn make_rom() -> Vec<u8> {
    let mut rom = vec![0; 32768];
    rom[0x100..0x103].copy_from_slice(&[0xc3, 0x50, 1]);
    rom[0x104..0x134].fill(0xaa);
    rom[0x134..0x13c].copy_from_slice(b"SGBAUDIO");
    rom[0x146] = 3;
    rom[0x14b] = 0x33;
    let mut score = [0; 64];
    // Pattern at $2B08; channel 0 begins at $2B18.
    score[8..10].copy_from_slice(&[0x18, 0x2b]);
    // Phrase list follows the track data and channel-pointer table.
    score[..2].copy_from_slice(&0x2b30u16.to_le_bytes());
    score[0x30..0x36].copy_from_slice(&[8, 0x2b, 0xff, 0, 0x30, 0x2b]);
    score[0x18..0x29].copy_from_slice(&[
        0xe7, 40, 0xe0, 48, 0xe1, 4, 12, 0x7f, 0xa4, 0xa8, 0xab, 0xc8, 0xc9, 0xa8, 0xa4, 0xc9, 0,
    ]);
    rom[0x5000..0x5004].copy_from_slice(&[64, 0, 0, 0x2b]);
    rom[0x5004..0x5044].copy_from_slice(&score);
    rom[0x5044..0x5048].copy_from_slice(&[0, 0, 0, 4]);
    let mut code = vec![
        0xf3, 0xaf, 0xe0, 0x40, 0xe0, 0x42, 0xe0, 0x43, 0xe0, 0x26, 0x3e, 0xe4, 0xe0, 0x47,
    ];
    send(&mut code, 0x17, &[1]);
    for tile in 0..256 {
        let address = 0x9800 + (tile / 20) * 32 + tile % 20;
        code.extend_from_slice(&[
            0x21,
            address as u8,
            (address >> 8) as u8,
            0x3e,
            tile as u8,
            0x77,
        ]);
    }
    code.extend_from_slice(&[
        0x11, 0, 0x50, 0x21, 0, 0x80, 0x01, 0, 0x10, 0x1a, 0x22, 0x13, 0x0b, 0x78, 0xb1, 0x20,
        0xf8, 0x3e, 0x91, 0xe0, 0x40,
    ]);
    send(&mut code, 9, &[]);
    transport::wait_frames(&mut code);
    send(&mut code, 8, &[0, 0, 0, 1]);
    send(&mut code, 0x17, &[0]);
    code.extend_from_slice(&[0x3e, 0x66, 0xea, 0, 0xc0, 0x18, 0xfe]);
    assert!(0x150 + code.len() < 0x5000);
    rom[0x150..0x150 + code.len()].copy_from_slice(&code);
    rom
}
fn send(code: &mut Vec<u8>, command: u8, payload: &[u8]) {
    let mut bytes = [0; 16];
    bytes[0] = (command << 3) | 1;
    bytes[1..1 + payload.len()].copy_from_slice(payload);
    transport::packet_bytes(code, &bytes);
}

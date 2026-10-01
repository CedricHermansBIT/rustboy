//! Original synthetic SGB cartridge, shared by native and browser tests.
//! Emits CHR_TRN for both halves and PCT_TRN using actual LCD transfer layouts.
pub fn make_rom() -> Vec<u8> {
    let mut rom = vec![0; 32768];
    rom[0x100..0x103].copy_from_slice(&[0xC3, 0x50, 0x01]);
    rom[0x104..0x134].fill(0xAA);
    rom[0x134..0x13D].copy_from_slice(b"SGBBORDER");
    rom[0x143] = 0x80;
    rom[0x146] = 3;
    rom[0x14B] = 0x33;
    let mut low = [0; 4096];
    for y in 0..8 {
        low[32 + y * 2] = 255;
    } // tile 1, color 1
    let mut high = [0; 4096];
    for y in 0..8 {
        high[y * 2 + 1] = 255;
    } // tile 128, color 2
    let mut map = [0; 4096];
    for y in 0..29 {
        for x in 0..32 {
            let entry: u16 = if (6..26).contains(&x) && (5..23).contains(&y) {
                0x1000
            } else {
                0x1001
            };
            let offset = (y * 32 + x) * 2;
            map[offset..offset + 2].copy_from_slice(&entry.to_le_bytes());
        }
    }
    // Top-left uses the high tile half/palette 5; one foreground border tile
    // at (56,48) overlays the game. Palette 4 color 1 is red; palette 5 #2 green.
    map[..2].copy_from_slice(&0x1480u16.to_le_bytes());
    let offset = (6 * 32 + 7) * 2;
    map[offset..offset + 2].copy_from_slice(&0x1001u16.to_le_bytes());
    map[0x802..0x804].copy_from_slice(&31u16.to_le_bytes());
    map[0x824..0x826].copy_from_slice(&0x3E0u16.to_le_bytes());
    rom[0x5000..0x6000].copy_from_slice(&low);
    rom[0x6000..0x7000].copy_from_slice(&high);
    rom[0x7000..].copy_from_slice(&map);

    let mut code = vec![
        0xF3, 0xAF, 0xE0, 0x40, 0xE0, 0x42, 0xE0, 0x43, 0x3E, 0xE4, 0xE0, 0x47,
    ];
    packet(&mut code, 0x17, 1); // hide bulk transfer garbage
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
    for (source, command, argument) in [(0x5000, 0x13, 0), (0x6000, 0x13, 1), (0x7000, 0x14, 0)] {
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
        packet(&mut code, command, argument);
        wait_frames(&mut code);
    }
    code.extend_from_slice(&[
        0xAF, 0xE0, 0x40, 0x21, 0, 0x98, 0x01, 0, 4, 0xAF, 0x22, 0x0B, 0x78, 0xB1, 0x20, 0xF9,
        0x21, 0, 0x80,
    ]);
    for _ in 0..8 {
        code.extend_from_slice(&[0x3E, 0xAA, 0x22, 0xAF, 0x22]);
    }
    packet(&mut code, 0x17, 0);
    code.extend_from_slice(&[
        0x3E, 0x91, 0xE0, 0x40, 0x3E, 0x66, 0xEA, 0, 0xC0, 0x18, 0xFE,
    ]);
    assert!(
        0x150 + code.len() < 0x5000,
        "fixture code overlaps its transfer payloads"
    );
    rom[0x150..0x150 + code.len()].copy_from_slice(&code);
    rom
}

fn packet(code: &mut Vec<u8>, command: u8, argument: u8) {
    let mut bytes = [0; 16];
    bytes[0] = (command << 3) | 1;
    bytes[1] = argument;
    packet_bytes(code, &bytes);
}

pub fn packet_bytes(code: &mut Vec<u8>, bytes: &[u8; 16]) {
    let pulse = |code: &mut Vec<u8>, value| {
        code.extend_from_slice(&[0x3E, value, 0xE0, 0]);
        code.extend_from_slice(&[0; 3]);
        code.extend_from_slice(&[0x3E, 0x30, 0xE0, 0]);
        code.extend_from_slice(&[0; 13]);
    };
    pulse(code, 0);
    for &byte in bytes {
        for bit in 0..8 {
            pulse(code, if byte & (1 << bit) == 0 { 0x20 } else { 0x10 });
        }
    }
    pulse(code, 0x20);
}

pub fn wait_frames(code: &mut Vec<u8>) {
    code.extend_from_slice(&[0x06, 6]);
    let start = code.len();
    code.extend_from_slice(&[
        0xF0, 0x44, 0xFE, 144, 0x20, 0xFA, 0xF0, 0x44, 0xFE, 0, 0x20, 0xFA, 0x05, 0x20,
    ]);
    code.push((start as isize - code.len() as isize - 1) as i8 as u8);
}

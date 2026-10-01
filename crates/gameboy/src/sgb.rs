//! Experimental high-level SGB adapter, not a SNES/ICD2 implementation.
//! JOYP packet transport is independent of the command interpreter, so a future
//! composed SNES backend can consume complete packets at this boundary.
//! See https://gbdev.io/pandocs/SGB_Command_Packet.html and its command chapters.

const PIXELS: usize = 160 * 144;
mod border;
#[cfg(test)]
mod border_tests;
const LEGACY_STATE_BYTES: usize = 11 + 16 + 112 + 6 + 32 + 360 + 8 + 256 + PIXELS * 4;
const MAX_TRANSFERS: usize = 4;
const TRANSFER_STATE_BYTES: usize = 3 + 4096;

#[derive(Clone, Debug, PartialEq, Eq)]
struct Transfer {
    // 1: low tiles, 2: high tiles, 3: map + palettes.
    destination: u8,
    remaining: u8,
    frame_started: bool,
    data: Box<[u8; 4096]>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Command {
    pub code: u8,
    /// Complete transmission including the first header; continuation packets
    /// have no additional header. At most seven 16-byte packets.
    pub bytes: Vec<u8>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Receiver {
    packet: [u8; 16],
    bytes: [u8; 112],
    bits: u16,
    packets: u8,
    expected: u8,
    receiving: bool,
    released: bool,
}

impl Default for Receiver {
    fn default() -> Self {
        Self {
            packet: [0; 16],
            bytes: [0; 112],
            bits: 0,
            packets: 0,
            expected: 0,
            receiving: false,
            released: false,
        }
    }
}

impl Receiver {
    fn abort(&mut self) {
        *self = Self::default();
    }

    fn write(&mut self, lines: u8) -> Option<Command> {
        match lines {
            0 => {
                // A reset during an incomplete packet abandons that command,
                // but the start of a continuation preserves completed packets.
                if self.receiving {
                    self.abort();
                }
                self.packet.fill(0);
                self.bits = 0;
                self.receiving = true;
                self.released = false;
            }
            0x30 => self.released = true,
            0x10 | 0x20 if self.receiving && self.released => {
                self.released = false;
                let bit = u8::from(lines == 0x10);
                if self.bits < 128 {
                    self.packet[self.bits as usize / 8] |= bit << (self.bits % 8);
                    self.bits += 1;
                } else {
                    // The mandatory stop bit is zero. Nothing is dispatched
                    // until every packet, including its stop bit, is complete.
                    if bit != 0 {
                        self.abort();
                        return None;
                    }
                    self.receiving = false;
                    if self.packets == 0 {
                        self.expected = self.packet[0] & 7;
                        if self.expected == 0 {
                            self.abort();
                            return None;
                        }
                    }
                    let offset = self.packets as usize * 16;
                    self.bytes[offset..offset + 16].copy_from_slice(&self.packet);
                    self.packets += 1;
                    if self.packets == self.expected {
                        let command = Command {
                            code: self.bytes[0] >> 3,
                            bytes: self.bytes[..self.packets as usize * 16].to_vec(),
                        };
                        self.abort();
                        return Some(command);
                    }
                }
            }
            _ => {}
        }
        None
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Sgb {
    enabled: bool,
    receiver: Receiver,
    palettes: [[u16; 4]; 4],
    attributes: [u8; 20 * 18],
    mask: u8,
    players: u8,
    player: u8,
    /// Pressed bits: Right, Left, Up, Down, A, B, Select, Start.
    buttons: [u8; 4],
    last_lines: u8,
    suppress_release: bool,
    commands_disabled: bool,
    /// The last complete, unmasked, colorized LCD frame. Freeze and LCD-off
    /// retain it; it is not dependent on the frontend asking for a frame.
    frame: Vec<u8>,
    border: border::Border,
    transfers: Vec<Transfer>,
    pub transfer_drops: u64,
    pub commands_received: u64,
    /// Unsupported commands are visible rather than reported as emulated.
    pub unsupported: [u64; 32],
}

impl Sgb {
    pub fn new(rom: &[u8]) -> Self {
        Self {
            enabled: rom.get(0x146) == Some(&3) && rom.get(0x14B) == Some(&0x33),
            receiver: Receiver::default(),
            // Neutral fallback, not a copy of Nintendo's built-in palettes.
            palettes: [[0x7FFF, 0x56B5, 0x294A, 0]; 4],
            attributes: [0; 360],
            mask: 0,
            players: 1,
            player: 0,
            buttons: [0; 4],
            last_lines: 0x30,
            suppress_release: false,
            commands_disabled: false,
            frame: vec![255; PIXELS * 4],
            border: border::Border::default(),
            transfers: Vec::new(),
            transfer_drops: 0,
            commands_received: 0,
            unsupported: [0; 32],
        }
    }

    pub fn enabled(&self) -> bool {
        self.enabled
    }

    pub fn players(&self) -> u8 {
        self.players
    }

    pub fn has_border(&self) -> bool {
        self.border.active
    }
    pub fn transfers_pending(&self) -> usize {
        self.transfers.len()
    }

    /// Called at LCD line-zero start. A command issued in the middle of a
    /// frame cannot consume that incomplete frame as its transfer payload.
    pub(crate) fn start_frame(&mut self) {
        for transfer in &mut self.transfers {
            transfer.frame_started = true;
        }
    }

    pub(crate) fn lcd_off(&mut self) {
        for transfer in &mut self.transfers {
            transfer.frame_started = false;
        }
    }

    fn request_transfer(&mut self, destination: u8) {
        if self.transfers.len() == MAX_TRANSFERS {
            self.transfer_drops = self.transfer_drops.saturating_add(1);
            return;
        }
        self.transfers.push(Transfer {
            destination,
            remaining: 5,
            frame_started: false,
            data: Box::new([0; 4096]),
        });
    }

    pub fn write_joyp(&mut self, data: u8) {
        let lines = data & 0x30;
        if self.enabled {
            let transferring = !self.commands_disabled
                && (self.receiver.receiving || self.receiver.packets != 0 || lines == 0);
            let command = if self.commands_disabled {
                None
            } else {
                self.receiver.write(lines)
            };
            if let Some(command) = command {
                self.execute(&command);
                self.suppress_release = true;
            } else if !transferring
                && !self.suppress_release
                && self.last_lines & 0x20 == 0
                && lines & 0x20 != 0
            {
                self.player = (self.player + 1) & (self.players - 1);
            }
            if lines == 0x30 {
                self.suppress_release = false;
            }
        }
        self.last_lines = lines;
    }

    pub fn read_joyp(&self, select: u8) -> u8 {
        let lines = select & 0x30;
        let mut result = 0x0F;
        if lines == 0x30 {
            result -= self.player;
        } else {
            let pressed = self.buttons[self.player as usize];
            if lines & 0x10 == 0 {
                result &= !(pressed & 0x0F);
            }
            if lines & 0x20 == 0 {
                result &= !(pressed >> 4);
            }
        }
        0xC0 | lines | result
    }

    pub fn set_button(&mut self, port: usize, bit: u8, pressed: bool) -> Result<(), String> {
        if bit >= 8 {
            return Err("Invalid SGB controller button bit".into());
        }
        let buttons = self
            .buttons
            .get_mut(port)
            .ok_or("SGB supports controller ports 0–3")?;
        if pressed {
            *buttons |= 1 << bit;
        } else {
            *buttons &= !(1 << bit);
        }
        Ok(())
    }

    pub fn capture_frame(&mut self, pixels: &[u32]) {
        if pixels.len() < PIXELS {
            return;
        }
        // Transfers consume the unmasked LCD stream even while MASK_EN freezes
        // the visible game window. Never peek at the cartridge's VRAM layout.
        for transfer in &mut self.transfers {
            if !transfer.frame_started || transfer.remaining == 0 {
                continue;
            }
            if transfer.remaining == 5 {
                decode_transfer(pixels, &mut transfer.data);
            }
            transfer.remaining -= 1;
        }
        while self
            .transfers
            .first()
            .is_some_and(|transfer| transfer.remaining == 0)
        {
            let transfer = self.transfers.remove(0);
            self.border.transfer(transfer.destination, &transfer.data);
        }
        if self.mask != 0 {
            return;
        }
        for (index, (&pixel, out)) in pixels
            .iter()
            .take(PIXELS)
            .zip(self.frame.chunks_exact_mut(4))
            .enumerate()
        {
            let palette = self.attributes[(index / 160 / 8) * 20 + index % 160 / 8] as usize;
            // This is the LCD shade *after* BGP/OBP, not raw tile color.
            out.copy_from_slice(&rgb(self.palettes[palette][((pixel >> 27) & 3) as usize]));
        }
    }

    pub fn copy_frame(&self, out: &mut [u8]) {
        if self.border.active {
            for pixel in out.chunks_exact_mut(4) {
                pixel.copy_from_slice(&rgb(self.palettes[0][0]));
            }
            for y in 0..144 {
                let offset = ((y + 40) * border::WIDTH + 48) * 4;
                self.copy_game_row(y, &mut out[offset..offset + 160 * 4]);
            }
            self.border.overlay(out);
            return;
        }
        for y in 0..144 {
            self.copy_game_row(y, &mut out[y * 160 * 4..(y + 1) * 160 * 4]);
        }
    }

    fn copy_game_row(&self, y: usize, out: &mut [u8]) {
        match self.mask {
            2 | 3 => {
                let color = rgb(if self.mask == 2 {
                    0
                } else {
                    self.palettes[0][0]
                });
                for pixel in out.chunks_exact_mut(4) {
                    pixel.copy_from_slice(&color);
                }
            }
            _ => out.copy_from_slice(&self.frame[y * 160 * 4..(y + 1) * 160 * 4]),
        }
    }

    fn execute(&mut self, command: &Command) {
        let data = &command.bytes;
        self.commands_received = self.commands_received.saturating_add(1);
        match command.code {
            0..=3 => {
                let (first, second) = [(0, 1), (2, 3), (0, 3), (1, 2)][command.code as usize];
                let backdrop = word(data, 1) & 0x7FFF;
                for palette in &mut self.palettes {
                    palette[0] = backdrop;
                }
                for color in 1..4 {
                    self.palettes[first][color] = word(data, 1 + color * 2) & 0x7FFF;
                    self.palettes[second][color] = word(data, 7 + color * 2) & 0x7FFF;
                }
            }
            4 => {
                // Validate the whole command before changing attributes.
                let count = (data[1] as usize).min(18);
                if 2 + count * 6 > data.len() {
                    return;
                }
                for block in data[2..2 + count * 6].chunks_exact(6) {
                    let [control, colors, x1, y1, x2, y2] = <[u8; 6]>::try_from(block).unwrap();
                    if x1 > x2 || y1 > y2 {
                        continue;
                    }
                    for y in 0..18 {
                        for x in 0..20 {
                            let inside = x > x1 && x < x2 && y > y1 && y < y2;
                            let outside = x < x1 || x > x2 || y < y1 || y > y2;
                            let palette = if inside && control & 1 != 0 {
                                Some(colors & 3)
                            } else if outside && control & 4 != 0 {
                                Some((colors >> 4) & 3)
                            } else if !inside && !outside {
                                if control & 2 != 0 {
                                    Some((colors >> 2) & 3)
                                } else if control & 7 == 1 {
                                    Some(colors & 3)
                                } else if control & 7 == 4 {
                                    Some((colors >> 4) & 3)
                                } else {
                                    None
                                }
                            } else {
                                None
                            };
                            if let Some(palette) = palette {
                                self.attributes[y as usize * 20 + x as usize] = palette;
                            }
                        }
                    }
                }
            }
            5 => {
                let count = (data[1] as usize).min(110);
                if 2 + count > data.len() {
                    return;
                }
                for &line in &data[2..2 + count] {
                    let coordinate = (line & 31) as usize;
                    let palette = (line >> 5) & 3;
                    if line & 0x80 != 0 && coordinate < 18 {
                        self.attributes[coordinate * 20..coordinate * 20 + 20].fill(palette);
                    } else if line & 0x80 == 0 && coordinate < 20 {
                        for y in 0..18 {
                            self.attributes[y * 20 + coordinate] = palette;
                        }
                    }
                }
            }
            6 => {
                for y in 0..18 {
                    for x in 0..20 {
                        let coordinate = if data[1] & 0x40 != 0 { y } else { x };
                        let shift = if coordinate < data[2] as usize {
                            2
                        } else if coordinate == data[2] as usize {
                            4
                        } else {
                            0
                        };
                        self.attributes[y * 20 + x] = (data[1] >> shift) & 3;
                    }
                }
            }
            7 => {
                let count = (word(data, 3) as usize).min(360);
                if 6 + count.div_ceil(4) > data.len() {
                    return;
                }
                let mut x = (data[1] as usize).min(19);
                let mut y = (data[2] as usize).min(17);
                for i in 0..count {
                    self.attributes[y * 20 + x] = (data[6 + i / 4] >> (6 - 2 * (i % 4))) & 3;
                    if data[5] & 1 == 0 {
                        x += 1;
                        if x == 20 {
                            x = 0;
                            y = (y + 1) % 18;
                        }
                    } else {
                        y += 1;
                        if y == 18 {
                            y = 0;
                            x = (x + 1) % 20;
                        }
                    }
                }
            }
            0x0E => self.commands_disabled = data[1] & 4 != 0,
            0x11 => {
                self.players = if data[1] & 1 == 0 {
                    1
                } else if data[1] & 2 == 0 {
                    2
                } else {
                    4
                };
                self.player &= self.players - 1;
            }
            0x17 => self.mask = data[1] & 3,
            0x13 => self.request_transfer(1 + (data[1] & 1)),
            0x14 => self.request_transfer(3),
            code => {
                self.unsupported[code as usize] = self.unsupported[code as usize].saturating_add(1)
            }
        }
    }

    /// Fixed-layout, bounded adapter snapshot. The backend envelope supplies
    /// version, ROM identity, length and checksum; no host state is serialized.
    pub(crate) fn export_state(&self) -> Vec<u8> {
        let mut out = vec![
            self.enabled as u8,
            self.mask,
            self.players,
            self.player,
            self.last_lines,
            self.suppress_release as u8,
            self.commands_disabled as u8,
        ];
        out.extend_from_slice(&self.buttons);
        out.extend_from_slice(&self.receiver.packet);
        out.extend_from_slice(&self.receiver.bytes);
        out.extend_from_slice(&self.receiver.bits.to_le_bytes());
        out.extend_from_slice(&[
            self.receiver.packets,
            self.receiver.expected,
            self.receiver.receiving as u8,
            self.receiver.released as u8,
        ]);
        for &color in self.palettes.iter().flatten() {
            out.extend_from_slice(&color.to_le_bytes());
        }
        out.extend_from_slice(&self.attributes);
        out.extend_from_slice(&self.commands_received.to_le_bytes());
        for &count in &self.unsupported {
            out.extend_from_slice(&count.to_le_bytes());
        }
        out.extend_from_slice(&self.frame);
        self.border.export_state(&mut out);
        out.extend_from_slice(&self.transfer_drops.to_le_bytes());
        out.push(self.transfers.len() as u8);
        for index in 0..MAX_TRANSFERS {
            if let Some(transfer) = self.transfers.get(index) {
                out.extend_from_slice(&[
                    transfer.destination,
                    transfer.remaining,
                    transfer.frame_started as u8,
                ]);
                out.extend_from_slice(transfer.data.as_ref());
            } else {
                out.resize(out.len() + TRANSFER_STATE_BYTES, 0);
            }
        }
        out
    }

    pub(crate) fn import_state(rom: &[u8], data: &[u8], version: u16) -> Result<Self, String> {
        let mut sgb = Self::new(rom);
        let expected = if version == 1 {
            LEGACY_STATE_BYTES
        } else {
            sgb.export_state().len()
        };
        if !(1..=2).contains(&version) || data.len() != expected {
            return Err("Invalid SGB snapshot length".into());
        }
        let mut input = data;
        fn take<'a>(input: &mut &'a [u8], count: usize) -> &'a [u8] {
            let (head, tail) = input.split_at(count);
            *input = tail;
            head
        }
        fn byte(input: &mut &[u8]) -> u8 {
            take(input, 1)[0]
        }
        fn boolean(input: &mut &[u8]) -> Result<bool, String> {
            match byte(input) {
                0 => Ok(false),
                1 => Ok(true),
                _ => Err("Invalid SGB snapshot flag".into()),
            }
        }
        if boolean(&mut input)? != sgb.enabled {
            return Err("SGB snapshot cartridge mode mismatch".into());
        }
        sgb.mask = byte(&mut input);
        sgb.players = byte(&mut input);
        sgb.player = byte(&mut input);
        sgb.last_lines = byte(&mut input);
        sgb.suppress_release = boolean(&mut input)?;
        sgb.commands_disabled = boolean(&mut input)?;
        sgb.buttons.copy_from_slice(take(&mut input, 4));
        sgb.receiver.packet.copy_from_slice(take(&mut input, 16));
        sgb.receiver.bytes.copy_from_slice(take(&mut input, 112));
        sgb.receiver.bits = u16::from_le_bytes(take(&mut input, 2).try_into().unwrap());
        sgb.receiver.packets = byte(&mut input);
        sgb.receiver.expected = byte(&mut input);
        sgb.receiver.receiving = boolean(&mut input)?;
        sgb.receiver.released = boolean(&mut input)?;
        for color in sgb.palettes.iter_mut().flatten() {
            *color = u16::from_le_bytes(take(&mut input, 2).try_into().unwrap());
        }
        sgb.attributes.copy_from_slice(take(&mut input, 360));
        sgb.commands_received = u64::from_le_bytes(take(&mut input, 8).try_into().unwrap());
        for count in &mut sgb.unsupported {
            *count = u64::from_le_bytes(take(&mut input, 8).try_into().unwrap());
        }
        sgb.frame.copy_from_slice(take(&mut input, PIXELS * 4));
        if version >= 2 {
            sgb.border = border::Border::import_state(take(&mut input, border::STATE_BYTES))?;
            sgb.transfer_drops = u64::from_le_bytes(take(&mut input, 8).try_into().unwrap());
            let count = byte(&mut input) as usize;
            if count > MAX_TRANSFERS {
                return Err("Invalid SGB pending transfer count".into());
            }
            for index in 0..MAX_TRANSFERS {
                let bytes = take(&mut input, TRANSFER_STATE_BYTES);
                if index < count {
                    if !(1..=3).contains(&bytes[0]) || !(1..=5).contains(&bytes[1]) || bytes[2] > 1
                    {
                        return Err("Invalid SGB pending transfer state".into());
                    }
                    let mut data = Box::new([0; 4096]);
                    data.copy_from_slice(&bytes[3..]);
                    sgb.transfers.push(Transfer {
                        destination: bytes[0],
                        remaining: bytes[1],
                        frame_started: bytes[2] != 0,
                        data,
                    });
                } else if bytes.iter().any(|&value| value != 0) {
                    return Err("Unexpected SGB pending transfer data".into());
                }
            }
        }
        if sgb.mask > 3
            || ![1, 2, 4].contains(&sgb.players)
            || sgb.player >= sgb.players
            || sgb.last_lines & !0x30 != 0
            || sgb.receiver.bits > 128
            || sgb.receiver.expected > 7
            || sgb.receiver.packets >= 7
            || (sgb.receiver.packets != 0 && sgb.receiver.packets >= sgb.receiver.expected)
            || sgb.attributes.iter().any(|&value| value > 3)
            || sgb.palettes.iter().flatten().any(|&color| color > 0x7FFF)
            || sgb
                .transfers
                .windows(2)
                .any(|pair| pair[0].remaining > pair[1].remaining)
        {
            return Err("Invalid SGB snapshot values".into());
        }
        // Controller input is frontend-owned, just like CPU::keys.
        sgb.buttons.fill(0);
        Ok(sgb)
    }
}

fn word(data: &[u8], offset: usize) -> u16 {
    u16::from_le_bytes([data[offset], data[offset + 1]])
}

fn decode_transfer(pixels: &[u32], out: &mut [u8; 4096]) {
    // The first 256 visible 8x8 LCD tiles become 256 interleaved two-plane
    // GB tiles. Scrolling, BGP remapping and sprites are already in the signal.
    for tile in 0..256 {
        for row in 0..8 {
            let mut low = 0;
            let mut high = 0;
            for x in 0..8 {
                let offset = (tile / 20 * 8 + row) * 160 + tile % 20 * 8 + x;
                let shade = ((pixels[offset] >> 27) & 3) as u8;
                low |= (shade & 1) << (7 - x);
                high |= ((shade >> 1) & 1) << (7 - x);
            }
            out[tile * 16 + row * 2] = low;
            out[tile * 16 + row * 2 + 1] = high;
        }
    }
}

fn rgb(color: u16) -> [u8; 4] {
    let expand = |value: u16| ((value << 3) | (value >> 2)) as u8;
    [
        expand(color & 31),
        expand((color >> 5) & 31),
        expand((color >> 10) & 31),
        255,
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    fn adapter() -> Sgb {
        let mut rom = vec![0; 32768];
        rom[0x146] = 3;
        rom[0x14B] = 0x33;
        Sgb::new(&rom)
    }

    pub(crate) fn send(sgb: &mut Sgb, bytes: &[u8]) {
        assert_eq!(bytes.len() % 16, 0);
        for packet in bytes.chunks_exact(16) {
            sgb.write_joyp(0);
            sgb.write_joyp(0x30);
            for &byte in packet {
                for bit in 0..8 {
                    sgb.write_joyp(if byte & (1 << bit) == 0 { 0x20 } else { 0x10 });
                    sgb.write_joyp(0x30);
                }
            }
            sgb.write_joyp(0x20);
            sgb.write_joyp(0x30);
        }
    }

    fn command(sgb: &mut Sgb, code: u8, payload: &[u8]) {
        let mut bytes = vec![0; (payload.len() + 1).div_ceil(16) * 16];
        bytes[0] = (code << 3) | (bytes.len() / 16) as u8;
        bytes[1..1 + payload.len()].copy_from_slice(payload);
        send(sgb, &bytes);
    }

    #[test]
    fn header_gates_functions_and_normal_polling_does_not_dispatch_commands() {
        for (flag, license) in [(0, 0x33), (3, 0), (0, 0)] {
            let mut rom = vec![0; 32768];
            rom[0x146] = flag;
            rom[0x14B] = license;
            let mut sgb = Sgb::new(&rom);
            command(&mut sgb, 0x11, &[3]);
            assert_eq!(sgb.players, 1);
            assert_eq!(sgb.commands_received, 0);
        }
        let mut sgb = adapter();
        for _ in 0..500 {
            for lines in [0x20, 0x30, 0x10, 0x30] {
                sgb.write_joyp(lines);
            }
        }
        assert_eq!(sgb.commands_received, 0);
    }

    #[test]
    fn palettes_are_lsb_first_little_endian_and_share_color_zero() {
        let mut sgb = adapter();
        command(
            &mut sgb,
            0,
            &[
                0xFF, 0x7F, 0x1F, 0, 0xE0, 3, 0, 0x7C, 0, 0, 0xFF, 0x7F, 0, 0,
            ],
        );
        assert_eq!(sgb.palettes[0], [0x7FFF, 31, 0x3E0, 0x7C00]);
        assert!(sgb.palettes.iter().all(|p| p[0] == 0x7FFF));
        command(&mut sgb, 1, &[0; 14]);
        assert!(sgb.palettes.iter().all(|p| p[0] == 0));
        assert_eq!(sgb.palettes[0][1], 31);
    }

    #[test]
    fn multi_packet_attributes_have_no_repeated_header_and_apply_atomically() {
        let mut sgb = adapter();
        let mut bytes = [0; 32];
        bytes[0] = (4 << 3) | 2;
        bytes[1] = 3;
        bytes[2..8].copy_from_slice(&[1, 1, 0, 0, 3, 3]);
        bytes[8..14].copy_from_slice(&[1, 2, 4, 4, 7, 7]);
        bytes[14..20].copy_from_slice(&[1, 3, 8, 8, 11, 11]);
        send(&mut sgb, &bytes[..16]);
        assert_eq!(sgb.attributes, [0; 360]);
        send(&mut sgb, &bytes[16..]);
        assert_eq!(sgb.commands_received, 1);
        assert_eq!(sgb.attributes[0], 1); // inside-only also colors rectangle edge
        assert_eq!(sgb.attributes[4 * 20 + 4], 2);
        assert_eq!(sgb.attributes[8 * 20 + 8], 3);
    }

    #[test]
    fn bad_stop_reset_and_zero_packet_count_cannot_change_state() {
        let mut sgb = adapter();
        sgb.write_joyp(0);
        sgb.write_joyp(0x30);
        for _ in 0..128 {
            sgb.write_joyp(0x10);
            sgb.write_joyp(0x30);
        }
        sgb.write_joyp(0x10); // invalid stop bit
        assert_eq!(sgb.commands_received, 0);
        sgb.write_joyp(0);
        sgb.write_joyp(0x30);
        sgb.write_joyp(0x10);
        sgb.write_joyp(0x30);
        command(&mut sgb, 0x11, &[1]); // reset abandons the incomplete packet
        assert_eq!(sgb.players, 2);
        send(&mut sgb, &[0; 16]);
        assert_eq!(sgb.commands_received, 1);
    }

    #[test]
    fn missing_release_and_missing_stop_do_not_latch_extra_bits() {
        let mut receiver = Receiver::default();
        receiver.write(0);
        receiver.write(0x30);
        receiver.write(0x10);
        receiver.write(0x20);
        receiver.write(0x20);
        assert_eq!(receiver.bits, 1);
        for _ in 1..128 {
            receiver.write(0x30);
            assert!(receiver.write(0x20).is_none());
        }
        assert_eq!(receiver.bits, 128);
        assert_eq!(receiver.packets, 0);
    }

    #[test]
    fn lines_divisions_character_order_and_wrapping_are_bounded() {
        let mut sgb = adapter();
        command(&mut sgb, 5, &[2, 0x80 | (2 << 5) | 3, (1 << 5) | 4]);
        assert_eq!(sgb.attributes[3 * 20], 2);
        assert_eq!(sgb.attributes[4], 1);
        command(&mut sgb, 6, &[(1 << 2) | (2 << 4) | 3, 10]);
        assert_eq!(&sgb.attributes[..12], &[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 3]);
        command(&mut sgb, 7, &[19, 17, 4, 0, 0, 0x1B]);
        assert_eq!(sgb.attributes[359], 0);
        assert_eq!(&sgb.attributes[..3], &[1, 2, 3]);
        command(&mut sgb, 7, &[19, 17, 4, 0, 1, 0xE4]);
        assert_eq!(sgb.attributes[359], 3);
        assert_eq!(sgb.attributes[0], 2);
        assert_eq!(sgb.attributes[20], 1);
        let old = sgb.attributes;
        command(&mut sgb, 7, &[0, 0, 0xFF, 0xFF, 0]); // too few bytes for the count
        assert_eq!(sgb.attributes, old);
        command(&mut sgb, 5, &[1, 31]); // out-of-range column is ignored
        assert_eq!(sgb.attributes, old);
    }

    #[test]
    fn controller_detection_cycles_on_p15_release_and_routes_four_ports() {
        let mut sgb = adapter();
        command(&mut sgb, 0x11, &[3]);
        assert_eq!(sgb.read_joyp(0x30), 0xFF);
        sgb.set_button(0, 4, true).unwrap();
        sgb.set_button(1, 5, true).unwrap();
        sgb.write_joyp(0x10);
        assert_eq!(sgb.read_joyp(0x10) & 15, 14);
        sgb.write_joyp(0x30);
        assert_eq!(sgb.read_joyp(0x30), 0xFE);
        sgb.write_joyp(0x20);
        sgb.write_joyp(0x30); // direction selection doesn't advance
        assert_eq!(sgb.read_joyp(0x30), 0xFE);
        sgb.write_joyp(0x10);
        assert_eq!(sgb.read_joyp(0x10) & 15, 13);
        sgb.write_joyp(0x30);
        assert_eq!(sgb.read_joyp(0x30), 0xFD);
        command(&mut sgb, 0x11, &[1]);
        assert_eq!(sgb.player, 0); // selected player AND (new count - 1)
        assert!(sgb.set_button(4, 4, true).is_err());
    }

    #[test]
    fn lcd_shades_tile_attributes_freeze_black_and_backdrop_masks() {
        let mut sgb = adapter();
        sgb.palettes[0] = [0x7FFF, 31, 0x3E0, 0x7C00];
        sgb.palettes[1] = [0x7FFF, 0x3E0, 31, 0];
        sgb.attributes[1] = 1;
        let frame = vec![1 << 27; PIXELS];
        sgb.capture_frame(&frame);
        let mut output = vec![0; PIXELS * 4];
        sgb.copy_frame(&mut output);
        assert_eq!(&output[..4], &[255, 0, 0, 255]);
        assert_eq!(&output[8 * 4..9 * 4], &[0, 255, 0, 255]);
        command(&mut sgb, 0x17, &[1]);
        sgb.capture_frame(&vec![2 << 27; PIXELS]);
        sgb.copy_frame(&mut output);
        assert_eq!(&output[..4], &[255, 0, 0, 255]);
        command(&mut sgb, 0x17, &[2]);
        sgb.copy_frame(&mut output);
        assert!(output.chunks_exact(4).all(|p| p == [0, 0, 0, 255]));
        command(&mut sgb, 0x17, &[3]);
        sgb.copy_frame(&mut output);
        assert!(output.chunks_exact(4).all(|p| p == [255; 4]));
        command(&mut sgb, 0x17, &[0]);
        sgb.capture_frame(&vec![2 << 27; PIXELS]);
        sgb.copy_frame(&mut output);
        assert_eq!(&output[..4], &[0, 255, 0, 255]);
    }

    #[test]
    fn unknown_commands_are_counted_and_icon_disable_blocks_later_packets() {
        let mut sgb = adapter();
        command(&mut sgb, 0x12, &[0; 7]);
        assert_eq!(sgb.unsupported[0x12], 1);
        command(&mut sgb, 0x0E, &[4]);
        command(&mut sgb, 0x11, &[3]);
        assert_eq!(sgb.commands_received, 2);
        assert_eq!(sgb.players, 1);
    }

    #[test]
    fn snapshots_preserve_partial_packets_and_frozen_frames_and_reject_bad_values() {
        let sgb = adapter();
        let mut rom = vec![0; 32768];
        rom[0x146] = 3;
        rom[0x14B] = 0x33;
        let mut sgb = sgb;
        command(&mut sgb, 0x17, &[1]);
        sgb.write_joyp(0);
        sgb.write_joyp(0x30);
        sgb.write_joyp(0x10);
        let bytes = sgb.export_state();
        assert_eq!(Sgb::import_state(&rom, &bytes, 2).unwrap(), sgb);
        assert!(Sgb::import_state(&rom, &bytes[..bytes.len() - 1], 2).is_err());
        let mut bad = bytes;
        bad[2] = 3;
        assert!(Sgb::import_state(&rom, &bad, 2).is_err());
    }
}

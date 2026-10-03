//! Experimental high-level SGB adapter, not a SNES/ICD2 implementation.
//! JOYP packet transport is independent of the command interpreter, so a future
//! composed SNES backend can consume complete packets at this boundary.
//! See https://gbdev.io/pandocs/SGB_Command_Packet.html and its command chapters.

const PIXELS: usize = 160 * 144;
mod border;
#[cfg(test)]
mod border_tests;
#[cfg(test)]
mod palette_tests;
mod tables;
mod sound;
#[cfg(test)]
mod sound_tests;
#[cfg(test)]
mod timing_tests;
const LEGACY_STATE_BYTES: usize = 11 + 16 + 112 + 6 + 32 + 360 + 8 + 256 + PIXELS * 4;
const MAX_TRANSFERS: usize = 4;
const TRANSFER_STATE_BYTES: usize = 3 + 4096;
const BORDER_STATE_BYTES: usize =
    LEGACY_STATE_BYTES + border::STATE_BYTES + 9 + MAX_TRANSFERS * TRANSFER_STATE_BYTES;
const SHADE_BYTES: usize = PIXELS / 4;
const TIMING_STATE_BYTES: usize = 11;

#[derive(Clone, Debug, PartialEq, Eq)]
struct Transfer {
    // 1: low tiles, 2: high tiles, 3: map + palettes, 4: PAL_TRN, 5: ATTR_TRN, 6: SOU_TRN.
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
    pulse_lines: u8,
    pulse_ticks: u8,
    pulse_armed: bool,
    pub rejected_pulses: u64,
    sound: sound::Sound,
    palettes: [[u16; 4]; 4],
    user_palette: Option<[[u16; 4]; 4]>,
    palette_priority: bool,
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
    // Freeze holds the LCD image, not its colorization. Palette/attribute
    // changes must still recolor it, including while the LCD is disabled.
    shades: Box<[u8; SHADE_BYTES]>,
    shades_valid: bool,
    border: border::Border,
    tables: tables::Tables,
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
            pulse_lines: 0x30,
            pulse_ticks: 8,
            pulse_armed: true,
            rejected_pulses: 0,
            sound: sound::Sound::default(),
            // Neutral fallback, not a copy of Nintendo's built-in palettes.
            palettes: [[0x7FFF, 0x56B5, 0x294A, 0]; 4],
            user_palette: None,
            palette_priority: false,
            attributes: [0; 360],
            mask: 0,
            players: 1,
            player: 0,
            buttons: [0; 4],
            last_lines: 0x30,
            suppress_release: false,
            commands_disabled: false,
            frame: vec![255; PIXELS * 4],
            shades: Box::new([0; SHADE_BYTES]),
            shades_valid: true,
            border: border::Border::default(),
            tables: tables::Tables::default(),
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

    /// 0: visible, 1: frozen, 2: black, 3: backdrop color.
    pub fn screen_mask(&self) -> u8 {
        self.mask
    }

    /// Override LCD colors with original host-provided RGB555 palettes.
    /// Color zero is shared by all four palettes, as on the SGB.
    pub fn set_user_palette(&mut self, mut palettes: [[u16; 4]; 4]) -> Result<(), String> {
        if palettes.iter().flatten().any(|&color| color > 0x7FFF) {
            return Err("SGB user palette colors must be RGB555".into());
        }
        let backdrop = palettes[0][0];
        for palette in &mut palettes {
            palette[0] = backdrop;
        }
        self.user_palette = Some(palettes);
        self.recolor_frame();
        Ok(())
    }

    pub fn clear_user_palette(&mut self) {
        self.user_palette = None;
        self.recolor_frame();
    }

    pub fn user_palette_active(&self) -> bool { self.user_palette.is_some() }
    pub fn palette_priority(&self) -> bool { self.palette_priority }
    pub fn visible_palettes(&self) -> [[u16; 4]; 4] {
        self.user_palette.unwrap_or(self.palettes)
    }

    pub fn has_border(&self) -> bool {
        self.border.active
    }
    pub fn transfers_pending(&self) -> usize {
        self.transfers.len()
    }
    pub fn sound_uploads(&self) -> u64 { self.sound.uploads }
    pub fn sound_upload_rejections(&self) -> u64 { self.sound.rejected }
    pub fn sound_request(&self) -> [u8; 4] { self.sound.request }
    pub fn sound_ram(&self) -> &[u8; 65536] { self.sound.ram.bytes() }
    pub fn sound_playback_available(&self) -> bool { true }
    pub fn sound_uses_replacement(&self) -> bool { self.sound.uses_replacement() }
    pub fn sound_replacement_statistics(&self) -> (u32,u32) { self.sound.replacement_statistics() }
    pub fn debug_sound_registers(&self) -> Option<[u8;128]> {
        self.sound.apu.as_ref().map(|apu|std::array::from_fn(|r|apu.bus.dsp.read(r as u8)))
    }
    pub fn load_sound_firmware(&mut self,data:&[u8]) -> Result<(),String> { self.sound.load_firmware(data) }
    pub(crate) fn tick_sound(&mut self,ticks:u32) { self.sound.tick(ticks); }
    pub(crate) fn sound_sample(&self) -> [f32;2] { self.sound.sample() }

    /// Separate SNES tile memory, not an extension of the Game Boy's VRAM.
    pub fn debug_border_tiles(&self) -> Vec<u8> {
        self.border.debug_tiles(self.visible_palettes()[0][0])
    }

    pub fn debug_border_map(&self) -> Vec<u8> {
        let mut out = rgb(self.visible_palettes()[0][0]).repeat(border::WIDTH * border::HEIGHT);
        if self.has_border() {
            self.border.overlay(&mut out);
        }
        out
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
        let transferring = self.receiver.receiving || self.receiver.packets != 0 || lines == 0;
        let command = if self.enabled && !self.commands_disabled { self.receiver.write(lines) } else { None };
        self.update_joyp(lines, command, transferring);
    }

    /// CPU-clocked transport. Hardware can receive pulses/spaces of two
    /// M-cycles; a pulse is accepted on release, never before its width is known.
    pub(crate) fn tick_joyp(&mut self, ticks: u8) {
        self.pulse_ticks = self.pulse_ticks.saturating_add(ticks).min(8);
    }

    pub(crate) fn write_joyp_timed(&mut self, data: u8) {
        let lines = data & 0x30;
        let transferring = self.receiver.receiving || self.receiver.packets != 0
            || self.pulse_lines == 0 || lines == 0;
        let mut command = None;
        if self.enabled && !self.commands_disabled && lines != self.pulse_lines {
            if self.pulse_ticks < 8 || (self.pulse_lines != 0x30 && lines != 0x30) {
                let packet_active = self.receiver.receiving || self.receiver.packets != 0 || self.pulse_lines == 0;
                self.receiver.abort();
                self.pulse_armed = false;
                // Ordinary rapid button-group polling is not a bad SGB packet.
                if packet_active { self.rejected_pulses = self.rejected_pulses.saturating_add(1); }
            } else if lines == 0x30 && self.pulse_armed {
                command = self.receiver.write(self.pulse_lines);
                self.receiver.write(0x30);
            }
            if lines == 0 { self.pulse_armed = true; }
            self.pulse_lines = lines;
            self.pulse_ticks = 0;
        }
        self.update_joyp(lines, command, transferring);
    }

    fn update_joyp(&mut self, lines: u8, command: Option<Command>, transferring: bool) {
        if self.enabled {
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
            match transfer.destination {
                1..=3 => self.border.transfer(transfer.destination, &transfer.data),
                4..=5 => self.tables.transfer(transfer.destination, &transfer.data),
                6 => self.sound.upload(&transfer.data),
                _ => unreachable!("validated LCD transfer destination"),
            }
        }
        if self.mask != 0 {
            return;
        }
        for (index, &pixel) in pixels.iter().take(PIXELS).enumerate() {
            let shift = (index % 4) * 2;
            self.shades[index / 4] =
                (self.shades[index / 4] & !(3 << shift)) | ((((pixel >> 27) & 3) as u8) << shift);
        }
        self.shades_valid = true;
        self.recolor_frame();
    }

    fn recolor_frame(&mut self) {
        // Old v1/v2 snapshots contain only RGBA, with potentially ambiguous
        // equal palette colors. Preserve that image until a fresh LCD frame.
        if !self.shades_valid {
            return;
        }
        let palettes = self.visible_palettes();
        for (index, out) in self.frame.chunks_exact_mut(4).enumerate() {
            let palette = self.attributes[(index / 160 / 8) * 20 + index % 160 / 8] as usize;
            // This is the LCD shade *after* BGP/OBP, not raw tile color.
            let shade = (self.shades[index / 4] >> ((index % 4) * 2)) & 3;
            out.copy_from_slice(&rgb(palettes[palette][shade as usize]));
        }
    }

    pub fn copy_frame(&self, out: &mut [u8]) {
        if self.border.active {
            for pixel in out.chunks_exact_mut(4) {
                pixel.copy_from_slice(&rgb(self.visible_palettes()[0][0]));
            }
            for y in 0..144 {
                let offset = ((y + 40) * border::WIDTH + 48) * 4;
                self.copy_game_row(y, &mut out[offset..offset + 160 * 4]);
            }
            self.border.overlay(out);
            return;
        }
        self.copy_game_frame(out);
    }

    pub fn copy_game_frame(&self, out: &mut [u8]) {
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
                    self.visible_palettes()[0][0]
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
        if self.palette_priority && matches!(command.code, 0..=3 | 0x0A) {
            self.user_palette = None;
        }
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
            0x08 => {
                self.sound.command(data[1..5].try_into().unwrap());
                if !self.sound_playback_available() { self.unsupported[0x08] = self.unsupported[0x08].saturating_add(1); }
            }
            0x09 => {
                self.request_transfer(6);
                if !self.sound_playback_available() { self.unsupported[0x09] = self.unsupported[0x09].saturating_add(1); }
            }
            0x0A => {
                for palette in 0..4 {
                    self.palettes[palette] = self.tables.palette(word(data, 1 + palette * 2));
                }
                let backdrop = self.palettes[0][0];
                for palette in &mut self.palettes {
                    palette[0] = backdrop;
                }
                if data[9] & 0x80 != 0 {
                    self.apply_attribute_file(data[9] & 0x3F);
                }
                if data[9] & 0x40 != 0 {
                    self.mask = 0;
                }
            }
            0x0B => self.request_transfer(4),
            0x19 => self.palette_priority = data[1] & 1 != 0,
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
            0x15 => self.request_transfer(5),
            0x16 => {
                self.apply_attribute_file(data[1] & 0x3F);
                if data[1] & 0x40 != 0 {
                    self.mask = 0;
                }
            }
            code => {
                self.unsupported[code as usize] = self.unsupported[code as usize].saturating_add(1)
            }
        }
        if matches!(command.code, 0..=7 | 0x0A | 0x16) {
            self.recolor_frame();
        }
    }

    fn apply_attribute_file(&mut self, index: u8) {
        if let Some(attributes) = self.tables.attributes(index) {
            self.attributes = attributes;
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
        self.tables.export_state(&mut out);
        out.push(self.shades_valid as u8);
        out.extend_from_slice(self.shades.as_ref());
        out.extend_from_slice(&[self.pulse_lines, self.pulse_ticks, self.pulse_armed as u8]);
        out.extend_from_slice(&self.rejected_pulses.to_le_bytes());
        self.sound.export_state(&mut out);
        self.sound.export_audio_state(&mut out);
        out.extend_from_slice(&[self.palette_priority as u8, self.user_palette.is_some() as u8]);
        for color in self.user_palette.unwrap_or([[0; 4]; 4]).iter().flatten() {
            out.extend_from_slice(&color.to_le_bytes());
        }
        out
    }

    pub(crate) fn import_state(rom: &[u8], data: &[u8], version: u16) -> Result<Self, String> {
        let mut sgb = Self::new(rom);
        const PALETTE_STATE_BYTES: usize = 34;
        let v6_length = sgb.export_state().len() - PALETTE_STATE_BYTES;
        let expected = match version {
            1 => LEGACY_STATE_BYTES,
            2 => BORDER_STATE_BYTES,
            3 => v6_length - TIMING_STATE_BYTES - sound::STATE_BYTES - sound::AUDIO_STATE_BASE_BYTES,
            4 => v6_length - sound::STATE_BYTES - sound::AUDIO_STATE_BASE_BYTES,
            5 => v6_length - sound::AUDIO_STATE_BASE_BYTES,
            6 => v6_length,
            7 => v6_length + PALETTE_STATE_BYTES,
            _ => return Err("Unsupported SGB snapshot version".into()),
        };
        if (version<6 && data.len()!=expected) || (version>=6 && data.len()<expected) {
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
                    let max_destination = if version >= 5 { 6 } else if version >= 3 { 5 } else { 3 };
                    if !(1..=max_destination).contains(&bytes[0])
                        || !(1..=5).contains(&bytes[1])
                        || bytes[2] > 1
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
        if version >= 3 {
            sgb.tables = tables::Tables::import_state(take(&mut input, tables::STATE_BYTES))?;
            sgb.shades_valid = boolean(&mut input)?;
            sgb.shades.copy_from_slice(take(&mut input, SHADE_BYTES));
        } else {
            sgb.shades_valid = false;
        }
        if version >= 4 {
            sgb.pulse_lines = byte(&mut input);
            sgb.pulse_ticks = byte(&mut input);
            sgb.pulse_armed = boolean(&mut input)?;
            sgb.rejected_pulses = u64::from_le_bytes(take(&mut input, 8).try_into().unwrap());
            if sgb.pulse_lines & !0x30 != 0 || sgb.pulse_ticks > 8 {
                return Err("Invalid SGB pulse timing state".into());
            }
        }
        if version >= 5 { sgb.sound = sound::Sound::import_state(take(&mut input, sound::STATE_BYTES))?; }
        if version >= 7 {
            let (audio, palette_state) = input.split_at(input.len() - PALETTE_STATE_BYTES);
            sgb.sound.import_audio_state(audio)?;
            input = palette_state;
            sgb.palette_priority = boolean(&mut input)?;
            let active = boolean(&mut input)?;
            let mut palettes = [[0; 4]; 4];
            for color in palettes.iter_mut().flatten() {
                *color = u16::from_le_bytes(take(&mut input, 2).try_into().unwrap());
            }
            if palettes.iter().flatten().any(|&color| color > 0x7FFF)
                || (active && palettes.iter().any(|palette| palette[0] != palettes[0][0]))
                || (!active && palettes != [[0; 4]; 4])
            {
                return Err("Invalid SGB user palette state".into());
            }
            sgb.user_palette = active.then_some(palettes);
        } else if version >= 6 { sgb.sound.import_audio_state(input)?; }
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
        assert_eq!(Sgb::import_state(&rom, &bytes, 7).unwrap(), sgb);
        assert!(Sgb::import_state(&rom, &bytes[..bytes.len() - 1], 7).is_err());
        let mut bad = bytes;
        bad[2] = 3;
        assert!(Sgb::import_state(&rom, &bad, 7).is_err());
    }
}

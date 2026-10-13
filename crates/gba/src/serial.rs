//! Independently authored GBA cable registers and clocked transfers.
//! Network latency is handled outside the hardware: an attached transfer waits
//! for its peer's captured word, then advances exclusively in emulated clocks.
//! Two endpoints are supported. UART, JoyBus, wireless and GPIO cables are not.
//! Reference: GBATEK, "GBA Communication Ports". Multiplayer inter-word delay
//! and final cable timeout are approximated by one additional 18-bit slot.

use crate::CLOCK_HZ;
pub const STATE_BYTES: usize = 64;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct LinkRequest {
    pub id: u32,
    /// 0: normal 8-bit, 1: normal 32-bit, 2: multiplayer 16-bit.
    pub mode: u8,
    pub baud: u8,
    pub value: u32,
}

#[derive(Clone, Copy, Debug)]
struct Transfer {
    request: LinkRequest,
    initiated: bool,
    remaining: Option<u64>,
    words: [u32; 2],
}

#[derive(Clone, Debug)]
pub struct Serial {
    control: u16,
    rcnt: u16,
    data: u32,
    send: u16,
    multi: [u16; 4],
    connected: bool,
    player: u8,
    peer_mode: u8,
    peer_ready: bool,
    peer_so: bool,
    error: bool,
    next_id: u32,
    transfer: Option<Transfer>,
}

impl Default for Serial {
    fn default() -> Self {
        Self {
            control: 0,
            rcnt: 0,
            data: 0,
            send: 0,
            multi: [0; 4],
            connected: false,
            player: 0,
            peer_mode: 3,
            peer_ready: false,
            peer_so: true,
            error: false,
            next_id: 0,
            transfer: None,
        }
    }
}

impl Serial {
    pub fn connected(&self) -> bool {
        self.connected
    }
    pub fn player(&self) -> u8 {
        self.player
    }
    pub fn mode(&self) -> u8 {
        if self.rcnt & 0x8000 != 0 {
            3
        } else {
            (self.control >> 12 & 3) as u8
        }
    }
    pub fn so(&self) -> bool {
        if self.rcnt & 0x8000 != 0 {
            self.rcnt & 0x80 == 0 || self.rcnt & 8 != 0
        } else {
            self.control & 8 != 0
        }
    }
    /// BIOS register reset affects the controller, never the attached cable.
    pub fn reset_registers(&mut self) {
        self.control = 0;
        self.rcnt = 0x8000;
        self.data = 0;
        self.send = 0;
        self.multi = [0; 4];
        self.error = false;
        self.transfer = None;
    }
    pub fn reset_data_low(&mut self) {
        self.data &= 0xffff0000;
        self.multi[0] = 0;
    }
    pub fn ready(&self) -> bool {
        match self.mode() {
            0 | 1 => self.control & 0x81 == 0x80,
            // Multiplayer SD readiness indicates that the controller selected
            // this cable mode. Busy is a separate status bit; publishing busy
            // as "not ready" left the parent's peer signal stale at completion.
            2 => true,
            _ => false,
        }
    }
    pub fn attach(&mut self, player: u8) -> Result<(), &'static str> {
        if player > 1 {
            return Err("Cable supports players 0 and 1");
        }
        if self.transfer.is_some() {
            return Err("Cannot attach during a serial transfer");
        }
        self.connected = true;
        self.player = player;
        self.peer_mode = 3;
        self.peer_ready = false;
        self.peer_so = true;
        Ok(())
    }
    /// Release transport barriers; an already started transfer finishes with an
    /// absent peer. No network identifier survives a disconnected cable.
    pub fn disconnect(&mut self) {
        self.connected = false;
        self.peer_ready = false;
        self.peer_mode = 3;
        self.peer_so = true;
        if let Some(mut transfer) = self.transfer {
            if transfer.remaining.is_none() {
                transfer.words = [u32::MAX; 2];
                transfer.words[self.player as usize] = transfer.request.value;
                transfer.remaining = Some(duration(
                    transfer.request.mode,
                    transfer.request.baud,
                    false,
                ));
                self.transfer = Some(transfer);
                self.error = transfer.request.mode == 2;
            }
        }
    }
    pub fn set_peer(&mut self, mode: u8, ready: bool, so: bool) {
        self.peer_mode = mode;
        self.peer_ready = ready;
        self.peer_so = so;
    }
    pub fn waiting(&self) -> bool {
        self.transfer
            .is_some_and(|transfer| transfer.remaining.is_none())
    }
    pub fn request(&self) -> Option<LinkRequest> {
        self.transfer
            .filter(|transfer| transfer.initiated && transfer.remaining.is_none())
            .map(|transfer| transfer.request)
    }
    pub fn deadline(&self) -> u64 {
        self.transfer
            .and_then(|transfer| transfer.remaining)
            .unwrap_or(u64::MAX)
    }
    fn outgoing(&self, mode: u8) -> u32 {
        match mode {
            0 => u32::from(self.send & 0xff),
            1 => self.data,
            _ => u32::from(self.send),
        }
    }
    fn start(&mut self) {
        let mode = self.mode();
        if mode > 2 || self.transfer.is_some() || self.control & 0x80 == 0 {
            return;
        }
        // Normal external-clock slaves are armed, not started. Multiplayer
        // slaves cannot initiate a transfer by writing their read-only busy bit.
        if mode < 2 && self.control & 1 == 0 || mode == 2 && self.connected && self.player != 0 {
            return;
        }
        self.next_id = self.next_id.wrapping_add(1).max(1);
        let request = LinkRequest {
            id: self.next_id,
            mode,
            baud: self.control as u8 & 3,
            value: self.outgoing(mode),
        };
        let mut words = [u32::MAX; 2];
        words[self.player as usize] = request.value;
        self.multi = [0xffff; 4];
        self.error = false;
        if mode == 2 && !self.connected {
            // Without a child there is no multiplayer completion clock. Guest
            // timer/IRQ code can time out; never fabricate a completed packet.
            return;
        }
        self.transfer = Some(Transfer {
            request,
            initiated: true,
            words,
            remaining: if self.connected {
                None
            } else {
                Some(duration(mode, request.baud, false))
            },
        });
    }
    /// Capture the child data only when its actual guest serial hardware is
    /// ready. A caller must retry a not-ready peer rather than manufacture data.
    pub fn begin_remote(&mut self, id: u32, mode: u8, baud: u8) -> Result<u32, &'static str> {
        if !self.connected {
            return Err("Cable is disconnected");
        }
        if id == 0 || mode > 2 || baud > 3 {
            return Err("Invalid cable transfer");
        }
        if self.mode() != mode {
            return Err("Peer has not selected the same serial mode");
        }
        if self.transfer.is_some() {
            return Err("Serial transfer already active");
        }
        if mode == 2 && self.player == 0 {
            return Err("Only the multiplayer parent starts transfers");
        }
        if mode < 2 && !self.ready() {
            return Err("Normal-mode slave is not armed for external clock");
        }
        let value = self.outgoing(mode);
        self.control |= 0x80;
        self.multi = [0xffff; 4];
        self.error = false;
        self.transfer = Some(Transfer {
            request: LinkRequest {
                id,
                mode,
                baud,
                value,
            },
            initiated: false,
            remaining: None,
            words: [u32::MAX; 2],
        });
        Ok(value)
    }
    pub fn supply(&mut self, id: u32, words: [u32; 2]) -> Result<(), &'static str> {
        let transfer = self.transfer.as_mut().ok_or("No active serial transfer")?;
        if transfer.request.id != id || transfer.remaining.is_some() {
            return Err("Stale or duplicate cable response");
        }
        let mask = match transfer.request.mode {
            0 => 0xff,
            2 => 0xffff,
            _ => u32::MAX,
        };
        if words.iter().any(|word| word & !mask != 0)
            || words[self.player as usize] != transfer.request.value
        {
            return Err("Cable response does not match captured outgoing data");
        }
        transfer.words = words;
        transfer.remaining = Some(duration(transfer.request.mode, transfer.request.baud, true));
        self.error = false;
        Ok(())
    }
    pub fn supply_absent(&mut self, id: u32) -> Result<(), &'static str> {
        let transfer = self.transfer.as_mut().ok_or("No active serial transfer")?;
        if transfer.request.id != id || transfer.remaining.is_some() {
            return Err("Stale or duplicate cable response");
        }
        transfer.words = [u32::MAX; 2];
        transfer.words[self.player as usize] = transfer.request.value;
        transfer.remaining = Some(duration(
            transfer.request.mode,
            transfer.request.baud,
            false,
        ));
        self.error = transfer.request.mode == 2;
        Ok(())
    }
    /// Return true exactly once when an enabled serial IRQ is due.
    pub fn advance(&mut self, clocks: u64) -> bool {
        let Some(mut transfer) = self.transfer else {
            return false;
        };
        let Some(remaining) = transfer.remaining else {
            return false;
        };
        if clocks < remaining {
            transfer.remaining = Some(remaining - clocks);
            self.transfer = Some(transfer);
            return false;
        }
        match transfer.request.mode {
            0 => {
                self.send =
                    (self.send & 0xff00) | transfer.words[1 - self.player as usize] as u8 as u16
            }
            1 => self.data = transfer.words[1 - self.player as usize],
            2 => {
                self.multi[0] = transfer.words[0] as u16;
                self.multi[1] = transfer.words[1] as u16;
            }
            _ => unreachable!(),
        }
        self.transfer = None;
        self.control &= !0x80;
        self.control & 0x4000 != 0
    }
    fn control_value(&self) -> u16 {
        if self.mode() == 2 {
            let ready = !self.connected || self.peer_mode == 2 && self.peer_ready;
            (self.control & 0x7f83)
                | if !self.connected || self.player != 0 {
                    4 | u16::from(self.player) << 4
                } else {
                    0
                }
                | if ready { 8 } else { 0 }
                | if self.error { 0x40 } else { 0 }
        } else {
            (self.control & 0x7f8b)
                | if self.mode() == 3 && self.rcnt & 0x8000 == 0 {
                    0x20
                } else {
                    0
                }
                | if !self.connected || self.peer_so {
                    4
                } else {
                    0
                }
        }
    }
    pub fn read8(&self, offset: usize) -> Option<u8> {
        let value = match offset {
            0x120..=0x123 if self.mode() == 1 => (self.data >> ((offset - 0x120) * 8)) as u8,
            0x120..=0x127 if self.mode() != 2 => 0,
            0x120..=0x127 => (self.multi[(offset - 0x120) / 2] >> ((offset & 1) * 8)) as u8,
            0x128..=0x129 => (self.control_value() >> ((offset & 1) * 8)) as u8,
            0x12a..=0x12b if self.mode() == 3 && self.rcnt & 0x8000 == 0 => 0,
            0x12a..=0x12b => (self.send >> ((offset & 1) * 8)) as u8,
            0x134..=0x135 => {
                let pins = if self.mode() < 3 {
                    // SC rests high. SD reports multiplayer readiness, and
                    // parent SI is grounded on the multiplayer cable.
                    let idle = self.transfer.is_none();
                    let ready = !self.connected || self.peer_mode == self.mode() && self.peer_ready;
                    (self.rcnt & !0xf)
                        | u16::from(idle)
                        | u16::from(self.mode() >= 2 && ready) << 1
                        | u16::from(
                            !self.connected
                                || self.peer_so && (self.mode() != 2 || self.player != 0),
                        ) << 2
                        | u16::from(if self.mode() >= 2 { idle } else { self.so() }) << 3
                } else if self.connected && self.rcnt & 0x4000 == 0 && self.rcnt & 0x40 == 0 {
                    // SI is an input unless its GPIO direction bit is set.
                    // The parent's input is grounded by a multiplayer cable;
                    // the child's input follows the preceding endpoint's SO.
                    (self.rcnt & !4) | u16::from(self.player != 0 && self.peer_so) << 2
                } else {
                    if self.rcnt & 0xc000 == 0xc000 {
                        self.rcnt & !3
                    } else {
                        self.rcnt
                    }
                };
                (pins >> ((offset & 1) * 8)) as u8
            }
            _ => return None,
        };
        Some(value)
    }
    pub fn write8(&mut self, offset: usize, value: u8) -> bool {
        let shift = (offset & 1) * 8;
        let merge = |old: u16| (old & !(0xff << shift)) | u16::from(value) << shift;
        match offset {
            0x120..=0x123 if self.mode() == 1 => {
                let shift = (offset - 0x120) * 8;
                self.data = (self.data & !(0xff << shift)) | u32::from(value) << shift;
            }
            0x120..=0x127 => {} // Multiplayer receive and unused registers are read-only.
            0x128..=0x129 => {
                let old_mode = self.mode();
                let busy = self.control & 0x80;
                self.control = merge(self.control) & if old_mode == 2 { 0x7f83 } else { 0x7f8b };
                if old_mode == 2 && self.connected && self.player != 0 {
                    self.control = self.control & !0x80 | busy;
                }
                if self.control & 0x80 == 0 || self.mode() != old_mode {
                    self.transfer = None;
                }
                self.start();
            }
            0x12a..=0x12b => self.send = merge(self.send),
            0x134..=0x135 => {
                self.rcnt = merge(self.rcnt) & 0xc1ff;
                if self.rcnt & 0x8000 != 0 {
                    self.transfer = None;
                    self.control &= !0x80;
                }
            }
            _ => return false,
        }
        true
    }
    pub fn write16(&mut self, offset: usize, value: u16) -> bool {
        // Configure the control high byte before applying a start in the low
        // byte, so a halfword mode+start write captures the intended word.
        if offset == 0x128 {
            self.write8(offset + 1, (value >> 8) as u8);
            self.write8(offset, value as u8)
        } else {
            let first = self.write8(offset, value as u8);
            let second = self.write8(offset + 1, (value >> 8) as u8);
            first && second
        }
    }
    /// Populate from an existing register image after a legacy state restore.
    /// External connections and pending network responses are never restored.
    pub fn from_registers(io: &[u8]) -> Self {
        let mut serial = Self::default();
        let word = |offset| u16::from_le_bytes([io[offset], io[offset + 1]]);
        let control = word(0x128);
        serial.control = control
            & if control >> 12 & 3 == 2 {
                0x7f03
            } else {
                0x7f0b
            };
        serial.rcnt = word(0x134) & 0xc1ff;
        serial.send = word(0x12a);
        serial.data = u32::from_le_bytes(io[0x120..0x124].try_into().unwrap());
        if serial.mode() == 2 {
            serial.multi = std::array::from_fn(|i| word(0x120 + i * 2));
        }
        serial
    }
    pub fn store_registers(&self, io: &mut [u8]) {
        for offset in (0x120..=0x12b).chain(0x134..=0x135) {
            io[offset] = self.read8(offset).unwrap();
        }
    }
    /// Snapshot only hardware. Restoring does not reconnect external peers or
    /// reuse their transaction IDs. A pending network barrier becomes a clocked
    /// absent-peer transfer, while an already clocking transfer keeps its phase.
    pub fn encode(&self) -> [u8; STATE_BYTES] {
        let mut serial = self.clone();
        if serial.connected {
            serial.disconnect();
        }
        let mut bytes = [0; STATE_BYTES];
        bytes[0..2].copy_from_slice(&serial.control.to_le_bytes());
        bytes[2..4].copy_from_slice(&serial.rcnt.to_le_bytes());
        bytes[4..8].copy_from_slice(&serial.data.to_le_bytes());
        bytes[8..10].copy_from_slice(&serial.send.to_le_bytes());
        for (i, value) in serial.multi.into_iter().enumerate() {
            bytes[10 + i * 2..12 + i * 2].copy_from_slice(&value.to_le_bytes());
        }
        bytes[18] = serial.player;
        bytes[19] = u8::from(serial.error);
        bytes[20..24].copy_from_slice(&serial.next_id.to_le_bytes());
        if let Some(transfer) = serial.transfer {
            bytes[24] = 1;
            bytes[25] = transfer.request.mode;
            bytes[26] = transfer.request.baud;
            bytes[28..32].copy_from_slice(&transfer.request.id.to_le_bytes());
            bytes[32..36].copy_from_slice(&transfer.request.value.to_le_bytes());
            bytes[36..44].copy_from_slice(&transfer.remaining.unwrap().to_le_bytes());
            bytes[44..48].copy_from_slice(&transfer.words[0].to_le_bytes());
            bytes[48..52].copy_from_slice(&transfer.words[1].to_le_bytes());
        }
        bytes
    }
    pub fn decode(bytes: &[u8]) -> Result<Self, &'static str> {
        if bytes.len() != STATE_BYTES
            || bytes[18] > 1
            || bytes[19] > 1
            || bytes[24] > 1
            || bytes[27] != 0
            || bytes[52..].iter().any(|byte| *byte != 0)
        {
            return Err("Invalid serial snapshot");
        }
        let word = |offset| u16::from_le_bytes(bytes[offset..offset + 2].try_into().unwrap());
        let dword = |offset| u32::from_le_bytes(bytes[offset..offset + 4].try_into().unwrap());
        let mut serial = Self {
            control: word(0),
            rcnt: word(2),
            data: dword(4),
            send: word(8),
            multi: std::array::from_fn(|i| word(10 + i * 2)),
            player: bytes[18],
            error: bytes[19] != 0,
            next_id: dword(20),
            ..Self::default()
        };
        if serial.control & 0x8000 != 0 || serial.rcnt & !0xc1ff != 0 {
            return Err("Invalid serial snapshot registers");
        }
        if bytes[24] != 0 {
            let mode = bytes[25];
            let baud = bytes[26];
            let value = dword(32);
            let id = dword(28);
            let remaining = u64::from_le_bytes(bytes[36..44].try_into().unwrap());
            let words = [dword(44), dword(48)];
            let mask = match mode {
                0 => 0xff,
                2 => 0xffff,
                _ => u32::MAX,
            };
            if mode > 2
                || mode != serial.mode()
                || baud > 3
                || id == 0
                || serial.control & 0x80 == 0
                || value & !mask != 0
                || remaining == 0
                || remaining > duration(mode, baud, true)
                || words[serial.player as usize] != value
                || words
                    .iter()
                    .any(|word| *word != u32::MAX && word & !mask != 0)
            {
                return Err("Invalid serial snapshot transfer");
            }
            serial.transfer = Some(Transfer {
                request: LinkRequest {
                    id,
                    mode,
                    baud,
                    value,
                },
                remaining: Some(remaining),
                words,
                initiated: false,
            });
        } else if bytes[25..52].iter().any(|byte| *byte != 0) {
            return Err("Invalid serial snapshot inactive transfer");
        }
        Ok(serial)
    }
}

fn duration(mode: u8, baud: u8, connected: bool) -> u64 {
    if mode < 2 {
        let bits = if mode == 0 { 8 } else { 32 };
        bits * if baud & 2 != 0 { 8 } else { 64 }
    } else {
        let baud = [9_600, 38_400, 57_600, 115_200][baud as usize & 3];
        let bits = if connected { 54 } else { 36 };
        (CLOCK_HZ * bits).div_ceil(baud)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn word(serial: &Serial, offset: usize) -> u16 {
        u16::from_le_bytes([
            serial.read8(offset).unwrap(),
            serial.read8(offset + 1).unwrap(),
        ])
    }
    fn pair() -> [Serial; 2] {
        std::array::from_fn(|player| {
            let mut serial = Serial::default();
            serial.attach(player as u8).unwrap();
            serial
        })
    }
    #[test]
    fn receive_registers_are_read_only_and_unused_data_reads_zero() {
        let mut serial = Serial::default();
        for mode in [0, 2, 3] {
            serial.write16(0x128, mode << 12);
            for offset in [0x120, 0x122, 0x124, 0x126] {
                serial.write16(offset, 0xffff);
                assert_eq!(word(&serial, offset), 0);
            }
        }
        serial.write16(0x128, 0x1000);
        serial.write16(0x120, 0x1234);
        assert_eq!(word(&serial, 0x120), 0x1234);
    }
    #[test]
    fn bios_reset_keeps_cable_roles_and_gpio_reads_physical_si() {
        let [mut parent, mut child] = pair();
        for serial in [&mut parent, &mut child] {
            serial.write16(0x128, 0x2003);
            serial.reset_registers();
            assert!(serial.connected());
            assert_eq!(serial.mode(), 3);
        }
        assert_eq!(child.player(), 1);
        parent.set_peer(3, false, true);
        child.set_peer(3, false, true);
        assert_eq!(word(&parent, 0x134) & 4, 0);
        assert_eq!(word(&child, 0x134) & 4, 4);
        parent.write16(0x134, 0x8044); // Explicitly drive SI high as a GPIO output.
        assert_eq!(word(&parent, 0x134) & 4, 4);
    }
    #[test]
    fn normal_internal_without_cable_clocks_ones_and_irq_once() {
        let mut serial = Serial::default();
        serial.write16(0x12a, 0x125a);
        serial.write16(0x128, 0x4081);
        assert_eq!(serial.deadline(), 512);
        assert!(!serial.advance(511));
        assert_ne!(word(&serial, 0x128) & 0x80, 0);
        assert!(serial.advance(1));
        assert_eq!(word(&serial, 0x12a), 0x12ff);
        assert_eq!(word(&serial, 0x128) & 0x80, 0);
        assert!(!serial.advance(512));
    }
    #[test]
    fn external_clock_stays_armed_without_master() {
        let mut serial = Serial::default();
        serial.write16(0x128, 0x4080);
        assert!(serial.ready());
        assert_eq!(serial.deadline(), u64::MAX);
        assert!(!serial.advance(u64::MAX));
        assert_ne!(word(&serial, 0x128) & 0x80, 0);
    }
    #[test]
    fn normal32_barrier_preserves_clocks_and_exchanges_captured_words() {
        let [mut parent, mut child] = pair();
        parent.write16(0x128, 0x1001);
        child.write16(0x128, 0x5080);
        for (serial, value) in [(&mut parent, 0x12345678u32), (&mut child, 0x90abcdef)] {
            for (i, byte) in value.to_le_bytes().into_iter().enumerate() {
                serial.write8(0x120 + i, byte);
            }
        }
        parent.write16(0x128, 0x5083);
        let request = parent.request().unwrap();
        assert!(parent.waiting());
        assert!(!parent.advance(1_000_000));
        let slave = child
            .begin_remote(request.id, request.mode, request.baud)
            .unwrap();
        let words = [request.value, slave];
        parent.supply(request.id, words).unwrap();
        child.supply(request.id, words).unwrap();
        assert_eq!(parent.deadline(), 256);
        assert!(parent.advance(256));
        assert!(child.advance(256));
        assert_eq!(parent.data, 0x90abcdef);
        assert_eq!(child.data, 0x12345678);
        assert!(parent.supply(request.id, words).is_err());
    }
    #[test]
    fn multiplayer_child_busy_readonly_ids_and_all_receive_slots() {
        let [mut parent, mut child] = pair();
        parent.write16(0x128, 0x6003);
        child.write16(0x128, 0x6003);
        parent.set_peer(2, true, true);
        child.set_peer(2, true, true);
        assert_eq!(word(&parent, 0x128) & 0x3c, 8);
        assert_eq!(word(&child, 0x128) & 0x3c, 0x1c);
        child.write16(0x128, 0x6083);
        assert!(child.request().is_none());
        assert_eq!(word(&child, 0x128) & 0x80, 0);
        parent.write16(0x12a, 0x1234);
        child.write16(0x12a, 0xabcd);
        parent.write16(0x128, 0x6083);
        assert!(
            parent.ready(),
            "multiplayer busy does not withdraw cable readiness"
        );
        assert_ne!(word(&parent, 0x128) & 8, 0);
        let request = parent.request().unwrap();
        let outgoing = child
            .begin_remote(request.id, request.mode, request.baud)
            .unwrap();
        for serial in [&mut parent, &mut child] {
            assert_eq!(word(serial, 0x120), 0xffff);
            serial
                .supply(request.id, [request.value, outgoing])
                .unwrap();
            assert!(serial.advance(serial.deadline()));
            assert_eq!(word(serial, 0x120), 0x1234);
            assert_eq!(word(serial, 0x122), 0xabcd);
            assert_eq!(word(serial, 0x124), 0xffff);
            assert_eq!(word(serial, 0x126), 0xffff);
            assert_eq!(word(serial, 0x128) & 0xc0, 0);
        }
    }
    #[test]
    fn disconnect_releases_pending_transfer_and_unmatched_packets_fail() {
        let [mut parent, mut child] = pair();
        parent.write16(0x128, 0x2083);
        assert!(parent.waiting());
        assert!(child.begin_remote(1, 2, 3).is_err());
        assert!(parent.supply(99, [0, 0]).is_err());
        parent.disconnect();
        assert!(!parent.waiting());
        assert_ne!(parent.deadline(), u64::MAX);
        parent.advance(parent.deadline());
        assert_ne!(word(&parent, 0x128) & 0x40, 0);
    }
    #[test]
    fn register_state_roundtrip_detaches_transport() {
        let [mut parent, _] = pair();
        parent.write16(0x128, 0x2003);
        parent.write16(0x12a, 0x1357);
        let mut io = vec![0; 0x400];
        parent.store_registers(&mut io);
        let restored = Serial::from_registers(&io);
        assert!(!restored.connected());
        assert_eq!(word(&restored, 0x12a), 0x1357);
        assert_eq!(restored.mode(), 2);
        assert!(!restored.waiting());
    }
    #[test]
    fn snapshot_replays_standalone_remaining_phase_and_detaches_live_barrier() {
        let mut serial = Serial::default();
        serial.write16(0x128, 0x4081);
        serial.advance(123);
        let mut restored = Serial::decode(&serial.encode()).unwrap();
        assert_eq!(restored.deadline(), 389);
        assert!(!restored.advance(388));
        assert!(restored.advance(1));
        let [mut parent, _] = pair();
        parent.write16(0x128, 0x6083);
        assert!(parent.waiting());
        let restored = Serial::decode(&parent.encode()).unwrap();
        assert!(!restored.connected());
        assert!(!restored.waiting());
        assert_ne!(restored.deadline(), u64::MAX);
        for index in [18, 19, 24, 27, 52, 63] {
            let mut bytes = parent.encode();
            bytes[index] = 255;
            assert!(Serial::decode(&bytes).is_err());
        }
    }
    fn machine(words: &[u32]) -> crate::machine::GameBoyAdvance {
        let mut rom = vec![0; 0x200];
        for (i, word) in words.iter().enumerate() {
            rom[i * 4..i * 4 + 4].copy_from_slice(&word.to_le_bytes());
        }
        crate::machine::GameBoyAdvance::new(rom, None).unwrap()
    }
    #[test]
    fn machine_network_wait_freezes_timer_audio_and_video_and_resumes_both_words() {
        use crate::cpu::Memory;
        let mut parent = machine(&[0xe1c010b0, 0xeafffffe]); // STRH r1,[r0]; B .
        let mut child = machine(&[0xeafffffe]);
        for (player, machine) in [&mut parent, &mut child].into_iter().enumerate() {
            machine.bus.serial.attach(player as u8).unwrap();
            machine.bus.write16(0x04000128, 0x6003, false);
            machine
                .bus
                .write16(0x0400012a, if player == 0 { 0x1234 } else { 0xabcd }, false);
            machine.bus.write16(0x04000100, 0xff00, false);
            machine.bus.write16(0x04000102, 0x80, false);
            machine.bus.take_cycles();
        }
        parent.cpu.r[0] = 0x04000128;
        parent.cpu.r[1] = 0x6083;
        assert!(parent.run(crate::FRAME_CYCLES) > 0);
        assert_eq!(
            parent.cpu.r[15], 0x08000004,
            "CPU slice exits immediately after the start write"
        );
        let request = parent.bus.serial.request().unwrap();
        let timer = parent.bus.peek16(0x04000100);
        let clocks = parent.clocks;
        let audio = parent.bus.apu.samples.len();
        let pixels = parent.pixels.clone();
        assert_eq!(parent.run(crate::FRAME_CYCLES), 0);
        assert_eq!(parent.clocks, clocks);
        assert_eq!(parent.bus.peek16(0x04000100), timer);
        assert_eq!(parent.bus.apu.samples.len(), audio);
        assert_eq!(parent.pixels, pixels);
        let peer = child
            .bus
            .serial
            .begin_remote(request.id, request.mode, request.baud)
            .unwrap();
        parent
            .bus
            .serial
            .supply(request.id, [request.value, peer])
            .unwrap();
        child
            .bus
            .serial
            .supply(request.id, [request.value, peer])
            .unwrap();
        let duration = parent.bus.serial.deadline();
        parent.run(duration);
        child.run(duration);
        for machine in [&parent, &child] {
            assert_eq!(machine.bus.peek16(0x04000120), 0x1234);
            assert_eq!(machine.bus.peek16(0x04000122), 0xabcd);
            assert_ne!(machine.bus.peek16(0x04000202) & 0x80, 0);
            assert_eq!(machine.bus.peek16(0x04000128) & 0x80, 0);
            assert!(machine.fault.is_none());
        }
    }
    #[test]
    fn machine_halt_wakes_on_serial_irq_and_snapshot_replays_remaining_phase() {
        use crate::cpu::Memory;
        use rustboy_emulator_api::Emulator;
        let mut machine = machine(&[0xeafffffe]);
        machine.bus.write16(0x04000128, 0x4081, false);
        machine.bus.write16(0x04000200, 0x80, false);
        machine.bus.take_cycles();
        machine.bus.halted = true;
        machine.run(123);
        let snapshot = machine.export_state();
        let remaining = machine.bus.serial.deadline();
        machine.run(remaining);
        let expected = machine.export_state();
        assert_ne!(machine.bus.peek16(0x04000202) & 0x80, 0);
        assert_eq!(machine.bus.peek16(0x0400012a), 0xff);
        assert!(machine.bus.halted);
        assert!(machine.step() > 0);
        assert!(
            !machine.bus.halted,
            "enabled serial IRQ wakes HALT even with IME disabled"
        );
        machine.import_state(&snapshot).unwrap();
        assert_eq!(machine.bus.serial.deadline(), remaining);
        machine.run(remaining);
        assert_eq!(machine.export_state(), expected);
    }
}

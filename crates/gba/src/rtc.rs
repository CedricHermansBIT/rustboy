//! S-3511 compatible cartridge GPIO/RTC, authored from GBATEK's wire protocol.
//! Commands accept either command bit order; payload bytes are LSB first.
//! The clock follows emulated time. Hosts may seed it and add elapsed offline
//! time when loading the separately persisted RTC record.
use crate::CLOCK_HZ;

pub(crate) const STATE_BYTES: usize = 40;
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct Rtc {
    pub enabled: bool,
    data: u8,
    direction: u8,
    readable: bool,
    control: u8,
    command: u8,
    bits: u8,
    count: u8,
    length: u8,
    reading: bool,
    output: bool,
    payload: [u8; 7],
    pub unix_seconds: u64,
    fraction: u64,
    irq: bool,
}
impl Default for Rtc {
    fn default() -> Self {
        Self {
            enabled: false,
            data: 0,
            direction: 0,
            readable: false,
            control: 0x40,
            command: 0,
            bits: 0,
            count: 0,
            length: 0,
            reading: false,
            output: false,
            payload: [0; 7],
            unix_seconds: 946684800,
            fraction: 0,
            irq: false,
        }
    }
}
impl Rtc {
    pub fn detect(rom: &[u8]) -> Self {
        Self {
            enabled: rom.windows(8).any(|w| w == b"SIIRTC_V"),
            ..Self::default()
        }
    }
    pub fn read(&self, address: u32) -> Option<u8> {
        if !self.enabled || !self.readable {
            return None;
        }
        match address {
            0x080000c4 => Some(
                (self.data & self.direction)
                    | if self.direction & 2 == 0 && self.output {
                        2
                    } else {
                        0
                    },
            ),
            0x080000c6 => Some(self.direction),
            0x080000c8 => Some(1),
            0x080000c5 | 0x080000c7 | 0x080000c9 => Some(0),
            _ => None,
        }
    }
    pub fn write16(&mut self, address: u32, value: u16) -> bool {
        if !self.enabled {
            return false;
        }
        match address {
            0x080000c6 => self.direction = value as u8 & 15,
            0x080000c8 => self.readable = value & 1 != 0,
            0x080000c4 => {
                let old = self.data;
                self.data = value as u8 & 15;
                if self.data & 4 == 0 || old & 4 == 0 {
                    self.command = 0;
                    self.bits = 0;
                    self.count = 0;
                    self.length = 0;
                    self.reading = false;
                    self.output = false;
                } else if old & 1 != 0 && self.data & 1 == 0 && self.reading {
                    self.output = self.count < self.length
                        && self.payload[self.count as usize] >> self.bits & 1 != 0;
                    self.bits += 1;
                    if self.bits == 8 {
                        self.bits = 0;
                        self.count = self.count.saturating_add(1);
                    }
                } else if old & 1 == 0
                    && self.data & 1 != 0
                    && !self.reading
                    && self.direction & 2 != 0
                {
                    let bit = (self.data >> 1) & 1;
                    if self.length == 0 {
                        self.command |= bit << self.bits;
                        self.bits += 1;
                        if self.bits == 8 {
                            self.start_command();
                        }
                    } else if self.count < self.length {
                        self.payload[self.count as usize] |= bit << self.bits;
                        self.bits += 1;
                        if self.bits == 8 {
                            self.bits = 0;
                            self.count += 1;
                            if self.count == self.length {
                                self.commit();
                            }
                        }
                    }
                }
            }
            _ => return false,
        }
        true
    }
    fn start_command(&mut self) {
        if self.command & 0xf0 != 0x60 {
            self.command = self.command.reverse_bits();
        }
        self.bits = 0;
        self.count = 0;
        self.payload = [0; 7];
        if self.command & 0xf0 != 0x60 {
            self.length = 1;
            return;
        }
        let register = self.command >> 1 & 7;
        self.length = match register {
            0 | 4 => 0,
            2 => 7,
            3 => 3,
            _ => 1,
        };
        self.reading = self.command & 1 != 0;
        if register == 0 {
            self.control = 0;
        }
        if register == 4 {
            self.irq = true;
        }
        if !self.reading {
            return;
        }
        let [year, month, day, weekday, hour, minute, second] = date(self.unix_seconds);
        let hour = if self.control & 0x40 != 0 {
            bcd(hour)
        } else {
            bcd(if hour % 12 == 0 { 12 } else { hour % 12 }) | if hour >= 12 { 0x80 } else { 0 }
        };
        match register {
            1 => {
                self.payload[0] = self.control;
                self.control &= 0x7f;
            }
            2 => {
                self.payload = [
                    bcd(year),
                    bcd(month),
                    bcd(day),
                    weekday,
                    hour,
                    bcd(minute),
                    bcd(second),
                ]
            }
            3 => self.payload[..3].copy_from_slice(&[hour, bcd(minute), bcd(second)]),
            _ => self.payload.fill(0xff),
        }
    }
    fn commit(&mut self) {
        if self.command & 0xf0 != 0x60 {
            return;
        }
        let register = self.command >> 1 & 7;
        if register == 1 {
            self.control = self.payload[0] & 0x6a;
            return;
        }
        if !matches!(register, 2 | 3) {
            return;
        }
        let mut fields = date(self.unix_seconds);
        let offset = if register == 2 { 4 } else { 0 };
        let Some(mut hour) = unbcd(self.payload[offset] & 0x7f) else {
            return;
        };
        if self.control & 0x40 == 0 {
            if !(1..=12).contains(&hour) {
                return;
            }
            hour = hour % 12
                + if self.payload[offset] & 0x80 != 0 {
                    12
                } else {
                    0
                };
        }
        let (Some(minute), Some(second)) = (
            unbcd(self.payload[offset + 1]),
            unbcd(self.payload[offset + 2]),
        ) else {
            return;
        };
        if hour > 23 || minute > 59 || second > 59 {
            return;
        }
        fields[4..].copy_from_slice(&[hour, minute, second]);
        if register == 2 {
            for (i, field) in fields[..3].iter_mut().enumerate() {
                let Some(value) = unbcd(self.payload[i]) else {
                    return;
                };
                *field = value;
            }
            if fields[0] > 99
                || !(1..=12).contains(&fields[1])
                || fields[2] == 0
                || fields[2] > days_in_month(2000 + u32::from(fields[0]), fields[1])
            {
                return;
            }
        }
        self.unix_seconds = timestamp(fields);
        self.fraction = 0;
    }
    pub fn advance(&mut self, clocks: u64) -> bool {
        if !self.enabled {
            return false;
        }
        let old_minute = self.unix_seconds / 60;
        let sum = self.fraction + clocks;
        self.unix_seconds = self.unix_seconds.saturating_add(sum / CLOCK_HZ);
        self.fraction = sum % CLOCK_HZ;
        std::mem::take(&mut self.irq)
            || self.control & 8 != 0 && old_minute != self.unix_seconds / 60
    }
    pub fn encode(&self) -> [u8; STATE_BYTES] {
        let mut bytes = [0; STATE_BYTES];
        bytes[..12].copy_from_slice(&[
            self.enabled as u8,
            self.data,
            self.direction,
            self.readable as u8,
            self.control,
            self.command,
            self.bits,
            self.count,
            self.length,
            self.reading as u8,
            self.output as u8,
            self.irq as u8,
        ]);
        bytes[12..19].copy_from_slice(&self.payload);
        bytes[19..27].copy_from_slice(&self.unix_seconds.to_le_bytes());
        bytes[27..35].copy_from_slice(&self.fraction.to_le_bytes());
        bytes
    }
    pub fn decode(bytes: &[u8]) -> Result<Self, String> {
        if bytes.len() != STATE_BYTES
            || [0, 3, 9, 10, 11].iter().any(|&i| bytes[i] > 1)
            || bytes[1] > 15
            || bytes[2] > 15
            || bytes[6] > 7
            || bytes[7] > 8
            || bytes[8] > 7
            || bytes[35..].iter().any(|&b| b != 0)
        {
            return Err("Invalid GBA RTC state".into());
        }
        let unix_seconds = u64::from_le_bytes(bytes[19..27].try_into().unwrap());
        let fraction = u64::from_le_bytes(bytes[27..35].try_into().unwrap());
        if fraction >= CLOCK_HZ || unix_seconds > 4102444799 {
            return Err("Invalid GBA RTC clock".into());
        }
        Ok(Self {
            enabled: bytes[0] != 0,
            data: bytes[1],
            direction: bytes[2],
            readable: bytes[3] != 0,
            control: bytes[4],
            command: bytes[5],
            bits: bytes[6],
            count: bytes[7],
            length: bytes[8],
            reading: bytes[9] != 0,
            output: bytes[10] != 0,
            irq: bytes[11] != 0,
            payload: bytes[12..19].try_into().unwrap(),
            unix_seconds,
            fraction,
        })
    }
}
fn bcd(v: u8) -> u8 {
    (v / 10) * 16 + v % 10
}
fn unbcd(v: u8) -> Option<u8> {
    (v & 15 < 10 && v >> 4 < 10).then_some((v >> 4) * 10 + (v & 15))
}
fn days_in_month(year: u32, month: u8) -> u8 {
    match month {
        2 => {
            if year.is_multiple_of(4) && (!year.is_multiple_of(100) || year.is_multiple_of(400)) {
                29
            } else {
                28
            }
        }
        4 | 6 | 9 | 11 => 30,
        _ => 31,
    }
}
fn date(unix: u64) -> [u8; 7] {
    let mut days = unix / 86400;
    let weekday = ((days + 4) % 7) as u8;
    let mut year = 1970;
    loop {
        let n = if days_in_month(year, 2) == 29 {
            366
        } else {
            365
        };
        if days < n {
            break;
        }
        days -= n;
        year += 1;
    }
    let mut month = 1;
    while days >= u64::from(days_in_month(year, month)) {
        days -= u64::from(days_in_month(year, month));
        month += 1;
    }
    [
        (year % 100) as u8,
        month,
        days as u8 + 1,
        weekday,
        (unix / 3600 % 24) as u8,
        (unix / 60 % 60) as u8,
        (unix % 60) as u8,
    ]
}
fn timestamp(fields: [u8; 7]) -> u64 {
    let year = 2000 + u32::from(fields[0]);
    let mut days = 0u64;
    for y in 1970..year {
        days += if days_in_month(y, 2) == 29 { 366 } else { 365 };
    }
    for m in 1..fields[1] {
        days += u64::from(days_in_month(year, m));
    }
    days += u64::from(fields[2] - 1);
    days * 86400 + u64::from(fields[4]) * 3600 + u64::from(fields[5]) * 60 + u64::from(fields[6])
}
#[cfg(test)]
mod tests {
    use super::*;
    fn command(rtc: &mut Rtc, byte: u8, msb: bool) {
        rtc.write16(0x080000c6, 7);
        rtc.write16(0x080000c8, 1);
        rtc.write16(0x080000c4, 1);
        rtc.write16(0x080000c4, 5);
        for n in 0..8 {
            let bit = byte >> if msb { 7 - n } else { n } & 1;
            rtc.write16(0x080000c4, 4 | (u16::from(bit) << 1));
            rtc.write16(0x080000c4, 5 | (u16::from(bit) << 1));
        }
    }
    fn read(rtc: &mut Rtc) -> u8 {
        rtc.write16(0x080000c6, 5);
        let mut byte = 0;
        for n in 0..8 {
            rtc.write16(0x080000c4, 4);
            rtc.write16(0x080000c4, 5);
            byte |= ((rtc.read(0x080000c4).unwrap() >> 1) & 1) << n;
        }
        byte
    }
    #[test]
    fn gpio_wire_commands_dates_and_snapshot_continue_mid_byte() {
        let mut r = Rtc {
            enabled: true,
            unix_seconds: 1709210096,
            ..Rtc::default()
        };
        command(&mut r, 0x65, true);
        assert_eq!(
            (read(&mut r), read(&mut r), read(&mut r)),
            (0x24, 0x02, 0x29)
        );
        let mut copy = Rtc::decode(&r.encode()).unwrap();
        for _ in 0..4 {
            assert_eq!(read(&mut r), read(&mut copy));
        }
        command(&mut r, 0x63, false);
        assert_eq!(read(&mut r), 0x40);
    }
    #[test]
    fn emulated_time_leap_day_irq_and_bad_state() {
        let mut r = Rtc {
            enabled: true,
            unix_seconds: timestamp([24, 2, 28, 3, 23, 59, 59]),
            control: 0x48,
            ..Rtc::default()
        };
        assert!(r.advance(CLOCK_HZ));
        assert_eq!(&date(r.unix_seconds)[..3], &[24, 2, 29]);
        let mut bad = r.encode();
        bad[6] = 8;
        assert!(Rtc::decode(&bad).is_err());
        assert_eq!(
            date(timestamp([0, 1, 1, 6, 0, 0, 0])),
            [0, 1, 1, 6, 0, 0, 0]
        );
    }
}

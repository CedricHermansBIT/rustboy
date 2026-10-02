//! RustBoy's SPC700 interpreter. Instruction semantics are implemented here;
//! no emulator implementation or Nintendo IPL bytes are embedded.
use crate::SpcRam;

const C: u8 = 1;
const Z: u8 = 2;
const I: u8 = 4;
const H: u8 = 8;
const B: u8 = 16;
const P: u8 = 32;
const V: u8 = 64;
const N: u8 = 128;

/// The host owns the bus: SPC execution, timers, ports and DSP remain separable.
pub trait Bus {
    fn read(&mut self, address: u16) -> u8;
    fn write(&mut self, address: u16, value: u8);
}

impl Bus for SpcRam {
    fn read(&mut self, address: u16) -> u8 {
        SpcRam::read(self, address)
    }
    fn write(&mut self, address: u16, value: u8) {
        SpcRam::write(self, address, value)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Spc700 {
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub sp: u8,
    pub psw: u8,
    pub pc: u16,
    pub halted: bool,
}

impl Default for Spc700 {
    fn default() -> Self {
        Self {
            a: 0,
            x: 0,
            y: 0,
            sp: 0xef,
            psw: Z,
            pc: 0xffc0,
            halted: false,
        }
    }
}
crate::state::snapshot!(Spc700, a, x, y, sp, psw, pc, halted);

impl Spc700 {
    fn fetch(&mut self, bus: &mut impl Bus) -> u8 {
        let value = bus.read(self.pc);
        self.pc = self.pc.wrapping_add(1);
        value
    }
    fn word(&mut self, bus: &mut impl Bus) -> u16 {
        let lo = self.fetch(bus);
        u16::from_le_bytes([lo, self.fetch(bus)])
    }
    fn dp(&self, address: u8) -> u16 {
        u16::from(address) | if self.psw & P != 0 { 0x100 } else { 0 }
    }
    fn pointer(&self, bus: &mut impl Bus, address: u8) -> u16 {
        u16::from_le_bytes([
            bus.read(self.dp(address)),
            bus.read(self.dp(address.wrapping_add(1))),
        ])
    }
    fn flag(&mut self, flag: u8, set: bool) {
        self.psw = (self.psw & !flag) | if set { flag } else { 0 };
    }
    fn nz(&mut self, value: u8) {
        self.flag(Z, value == 0);
        self.flag(N, value & N != 0);
    }
    fn nz16(&mut self, value: u16) {
        self.flag(Z, value == 0);
        self.flag(N, value & 0x8000 != 0);
    }
    fn push(&mut self, bus: &mut impl Bus, value: u8) {
        bus.write(0x100 | u16::from(self.sp), value);
        self.sp = self.sp.wrapping_sub(1);
    }
    fn pop(&mut self, bus: &mut impl Bus) -> u8 {
        self.sp = self.sp.wrapping_add(1);
        bus.read(0x100 | u16::from(self.sp))
    }
    fn call(&mut self, bus: &mut impl Bus, address: u16) {
        self.push(bus, (self.pc >> 8) as u8);
        self.push(bus, self.pc as u8);
        self.pc = address;
    }
    fn ret(&mut self, bus: &mut impl Bus) {
        let lo = self.pop(bus);
        self.pc = u16::from_le_bytes([lo, self.pop(bus)]);
    }
    fn branch(&mut self, bus: &mut impl Bus, take: bool) -> u32 {
        let offset = self.fetch(bus) as i8;
        if take {
            self.pc = self.pc.wrapping_add_signed(i16::from(offset));
            2
        } else {
            0
        }
    }
    fn compare(&mut self, a: u8, b: u8) {
        self.nz(a.wrapping_sub(b));
        self.flag(C, a >= b);
    }
    fn alu(&mut self, operation: u8, a: u8, b: u8) -> u8 {
        let value = match operation {
            0 => a | b,
            1 => a & b,
            2 => a ^ b,
            3 => {
                self.compare(a, b);
                return a;
            }
            4 | 5 => {
                let rhs = if operation == 5 { !b } else { b };
                let carry = u16::from(self.psw & C);
                let sum = u16::from(a) + u16::from(rhs) + carry;
                self.flag(C, sum > 255);
                self.flag(H, u16::from(a & 15) + u16::from(rhs & 15) + carry > 15);
                self.flag(V, (!(a ^ rhs) & (a ^ sum as u8) & 0x80) != 0);
                sum as u8
            }
            _ => unreachable!(),
        };
        self.nz(value);
        value
    }
    fn shift(&mut self, operation: u8, value: u8) -> u8 {
        let carry = self.psw & C;
        let result = match operation {
            0 => value << 1,
            1 => (value << 1) | carry,
            2 => value >> 1,
            3 => (value >> 1) | (carry << 7),
            _ => unreachable!(),
        };
        self.flag(C, value & if operation < 2 { 0x80 } else { 1 } != 0);
        self.nz(result);
        result
    }
    /// Execute one instruction, returning SPC clocks (1.024 MHz).
    pub fn step(&mut self, bus: &mut impl Bus) -> u32 {
        if self.halted {
            return 2;
        }
        let op = self.fetch(bus);
        let row = op >> 4;
        let col = op & 15;
        // Six ALU families share precisely the same addressing matrix.
        if row < 12 && (4..=9).contains(&col) {
            let operation = row / 2;
            if row & 1 == 0 {
                if col == 9 {
                    let src = self.fetch(bus);
                    let dst = self.fetch(bus);
                    let a = bus.read(self.dp(dst));
                    let b = bus.read(self.dp(src));
                    let value = self.alu(operation, a, b);
                    if operation != 3 {
                        bus.write(self.dp(dst), value);
                    }
                    return 6;
                }
                let (value, cycles) = match col {
                    4 => {
                        let dp = self.fetch(bus);
                        (bus.read(self.dp(dp)), 3)
                    }
                    5 => {
                        let address = self.word(bus);
                        (bus.read(address), 4)
                    }
                    6 => (bus.read(self.dp(self.x)), 3),
                    7 => {
                        let dp = self.fetch(bus).wrapping_add(self.x);
                        let address = self.pointer(bus, dp);
                        (bus.read(address), 6)
                    }
                    8 => (self.fetch(bus), 2),
                    _ => unreachable!(),
                };
                self.a = self.alu(operation, self.a, value);
                return cycles;
            } else if col >= 8 {
                let (address, value) = if col == 8 {
                    let value = self.fetch(bus);
                    let dp = self.fetch(bus);
                    (self.dp(dp), value)
                } else {
                    (self.dp(self.x), bus.read(self.dp(self.y)))
                };
                let a = bus.read(address);
                let result = self.alu(operation, a, value);
                if operation != 3 {
                    bus.write(address, result);
                }
                return 5;
            } else {
                let (address, cycles) = match col {
                    4 => {
                        let dp = self.fetch(bus).wrapping_add(self.x);
                        (self.dp(dp), 4)
                    }
                    5 => {
                        let base = self.word(bus);
                        (base.wrapping_add(u16::from(self.x)), 5)
                    }
                    6 => {
                        let base = self.word(bus);
                        (base.wrapping_add(u16::from(self.y)), 5)
                    }
                    7 => {
                        let dp = self.fetch(bus);
                        (self.pointer(bus, dp).wrapping_add(u16::from(self.y)), 6)
                    }
                    _ => unreachable!(),
                };
                let value = bus.read(address);
                self.a = self.alu(operation, self.a, value);
                return cycles;
            }
        }
        if col == 1 {
            let vector = 0xffdeu16.wrapping_sub(u16::from(row) * 2);
            let address = u16::from_le_bytes([bus.read(vector), bus.read(vector.wrapping_add(1))]);
            self.call(bus, address);
            return 8;
        }
        if col == 2 || col == 3 {
            let dp = self.fetch(bus);
            let address = self.dp(dp);
            let bit = 1 << (row / 2);
            let value = bus.read(address);
            if col == 2 {
                bus.write(
                    address,
                    if row & 1 == 0 {
                        value | bit
                    } else {
                        value & !bit
                    },
                );
                return 4;
            }
            let extra = self.branch(bus, (value & bit != 0) == (row & 1 == 0));
            return 5 + extra;
        }
        if row < 8 && (col == 0xb || col == 0xc) {
            let (address, cycles) = if col == 0xc && row & 1 != 0 {
                self.a = self.shift(row / 2, self.a);
                return 2;
            } else if col == 0xc {
                (self.word(bus), 5)
            } else {
                let dp = self
                    .fetch(bus)
                    .wrapping_add(if row & 1 != 0 { self.x } else { 0 });
                (self.dp(dp), if row & 1 != 0 { 5 } else { 4 })
            };
            let value = bus.read(address);
            let result = self.shift(row / 2, value);
            bus.write(address, result);
            return cycles;
        }
        if col == 0xa && row & 1 == 0 {
            let bits = self.word(bus);
            let address = bits & 0x1fff;
            let mask = 1 << (bits >> 13);
            let value = bus.read(address);
            let bit = value & mask != 0;
            let carry = self.psw & C != 0;
            match row {
                0 => self.flag(C, carry | bit),
                2 => self.flag(C, carry | !bit),
                4 => self.flag(C, carry & bit),
                6 => self.flag(C, carry & !bit),
                8 => self.flag(C, carry ^ bit),
                10 => self.flag(C, bit),
                12 => bus.write(address, (value & !mask) | if carry { mask } else { 0 }),
                14 => bus.write(address, value ^ mask),
                _ => unreachable!(),
            }
            return if matches!(row, 0 | 2 | 8 | 14) {
                5
            } else if row == 12 {
                6
            } else {
                4
            };
        }
        match op {
            0x00 => 2,
            0x10 | 0x30 | 0x50 | 0x70 | 0x90 | 0xb0 | 0xd0 | 0xf0 => {
                let take = match op {
                    0x10 => self.psw & N == 0,
                    0x30 => self.psw & N != 0,
                    0x50 => self.psw & V == 0,
                    0x70 => self.psw & V != 0,
                    0x90 => self.psw & C == 0,
                    0xb0 => self.psw & C != 0,
                    0xd0 => self.psw & Z == 0,
                    _ => self.psw & Z != 0,
                };
                2 + self.branch(bus, take)
            }
            0x20 | 0x40 | 0x60 | 0x80 | 0xa0 | 0xc0 | 0xe0 | 0xed => {
                match op {
                    0x20 => self.psw &= !P,
                    0x40 => self.psw |= P,
                    0x60 => self.psw &= !C,
                    0x80 => self.psw |= C,
                    0xa0 => self.psw |= I,
                    0xc0 => self.psw &= !I,
                    0xe0 => self.psw &= !(V | H),
                    _ => self.psw ^= C,
                }
                if matches!(op, 0xa0 | 0xc0 | 0xed) {
                    3
                } else {
                    2
                }
            }
            0x0d | 0x2d | 0x4d | 0x6d => {
                let value = match op {
                    0x0d => self.psw,
                    0x2d => self.a,
                    0x4d => self.x,
                    _ => self.y,
                };
                self.push(bus, value);
                4
            }
            0x8e | 0xae | 0xce | 0xee => {
                let value = self.pop(bus);
                match op {
                    0x8e => self.psw = value,
                    0xae => self.a = value,
                    0xce => self.x = value,
                    _ => self.y = value,
                };
                4
            }
            0x0e | 0x4e => {
                let address = self.word(bus);
                let value = bus.read(address);
                self.nz(self.a.wrapping_sub(value));
                bus.write(
                    address,
                    if op == 0x0e {
                        value | self.a
                    } else {
                        value & !self.a
                    },
                );
                6
            }
            0x0f => {
                let address = u16::from_le_bytes([bus.read(0xffde), bus.read(0xffdf)]);
                self.push(bus, (self.pc >> 8) as u8);
                self.push(bus, self.pc as u8);
                self.push(bus, self.psw);
                self.psw = (self.psw | B) & !I;
                self.pc = address;
                8
            }
            0x1a | 0x3a => {
                let dp = self.fetch(bus);
                let value =
                    self.pointer(bus, dp)
                        .wrapping_add(if op == 0x1a { u16::MAX } else { 1 });
                bus.write(self.dp(dp), value as u8);
                bus.write(self.dp(dp.wrapping_add(1)), (value >> 8) as u8);
                self.nz16(value);
                6
            }
            0x1d | 0x3d | 0xdc | 0xfc | 0x9c | 0xbc => {
                let value = match op {
                    0x1d | 0x3d => &mut self.x,
                    0xdc | 0xfc => &mut self.y,
                    _ => &mut self.a,
                };
                *value = value.wrapping_add(if matches!(op, 0x1d | 0xdc | 0x9c) {
                    255
                } else {
                    1
                });
                let result = *value;
                self.nz(result);
                2
            }
            0x1e | 0x3e | 0x5e | 0x7e | 0xad | 0xc8 => {
                let value = if matches!(op, 0xad | 0xc8) {
                    self.fetch(bus)
                } else {
                    let address = if matches!(op, 0x1e | 0x5e) {
                        self.word(bus)
                    } else {
                        let dp = self.fetch(bus);
                        self.dp(dp)
                    };
                    bus.read(address)
                };
                self.compare(
                    if matches!(op, 0x1e | 0x3e | 0xc8) {
                        self.x
                    } else {
                        self.y
                    },
                    value,
                );
                if matches!(op, 0xad | 0xc8) {
                    2
                } else if matches!(op, 0x1e | 0x5e) {
                    4
                } else {
                    3
                }
            }
            0x1f => {
                let base = self.word(bus).wrapping_add(u16::from(self.x));
                self.pc = u16::from_le_bytes([bus.read(base), bus.read(base.wrapping_add(1))]);
                6
            }
            0x2e | 0xde => {
                let dp = self
                    .fetch(bus)
                    .wrapping_add(if op == 0xde { self.x } else { 0 });
                let value = bus.read(self.dp(dp));
                let extra = self.branch(bus, self.a != value);
                if op == 0xde {
                    6 + extra
                } else {
                    5 + extra
                }
            }
            0x2f => 2 + self.branch(bus, true),
            0x3f => {
                let address = self.word(bus);
                self.call(bus, address);
                8
            }
            0x4f => {
                let address = 0xff00 | u16::from(self.fetch(bus));
                self.call(bus, address);
                6
            }
            0x5f => {
                self.pc = self.word(bus);
                3
            }
            0x6e => {
                let dp = self.fetch(bus);
                let value = bus.read(self.dp(dp)).wrapping_sub(1);
                bus.write(self.dp(dp), value);
                5 + self.branch(bus, value != 0)
            }
            0xfe => {
                self.y = self.y.wrapping_sub(1);
                4 + self.branch(bus, self.y != 0)
            }
            0x6f => {
                self.ret(bus);
                5
            }
            0x7f => {
                self.psw = self.pop(bus);
                self.ret(bus);
                6
            }
            0x5a | 0x7a | 0x9a | 0xba | 0xda => {
                let dp = self.fetch(bus);
                let ya = u16::from_le_bytes([self.a, self.y]);
                if op == 0xda {
                    bus.write(self.dp(dp), self.a);
                    bus.write(self.dp(dp.wrapping_add(1)), self.y);
                    return 5;
                }
                let value = self.pointer(bus, dp);
                let result = match op {
                    0xba => value,
                    0x5a => {
                        let result = ya.wrapping_sub(value);
                        self.flag(C, ya >= value);
                        self.nz16(result);
                        return 4;
                    }
                    0x7a => {
                        let sum = u32::from(ya) + u32::from(value);
                        self.flag(C, sum > 65535);
                        self.flag(H, (ya & 4095) + (value & 4095) > 4095);
                        self.flag(V, (!(ya ^ value) & (ya ^ sum as u16) & 0x8000) != 0);
                        sum as u16
                    }
                    _ => {
                        let result = ya.wrapping_sub(value);
                        self.flag(C, ya >= value);
                        self.flag(H, (ya & 4095) >= (value & 4095));
                        self.flag(V, ((ya ^ value) & (ya ^ result) & 0x8000) != 0);
                        result
                    }
                };
                self.a = result as u8;
                self.y = (result >> 8) as u8;
                self.nz16(result);
                5
            }
            0x8b | 0x8c | 0x9b | 0xab | 0xac | 0xbb => {
                let (address, cycles) = if op & 15 == 12 {
                    (self.word(bus), 5)
                } else {
                    let dp = self
                        .fetch(bus)
                        .wrapping_add(if op & 0x10 != 0 { self.x } else { 0 });
                    (self.dp(dp), if op & 0x10 != 0 { 5 } else { 4 })
                };
                let value = bus
                    .read(address)
                    .wrapping_add(if op < 0xa0 { 255 } else { 1 });
                bus.write(address, value);
                self.nz(value);
                cycles
            }
            0x8d | 0xcd | 0xe8 => {
                let value = self.fetch(bus);
                match op {
                    0x8d => self.y = value,
                    0xcd => self.x = value,
                    _ => self.a = value,
                };
                self.nz(value);
                2
            }
            0x8f => {
                let value = self.fetch(bus);
                let dp = self.fetch(bus);
                bus.write(self.dp(dp), value);
                5
            }
            0x9d => {
                self.x = self.sp;
                self.nz(self.x);
                2
            }
            0xbd => {
                self.sp = self.x;
                2
            }
            0x9e => {
                let ya = u16::from_le_bytes([self.a, self.y]);
                let x = u16::from(self.x);
                self.flag(H, self.y & 15 >= self.x & 15);
                self.flag(V, self.y >= self.x);
                if u16::from(self.y) < x * 2 {
                    self.a = (ya / x) as u8;
                    self.y = (ya % x) as u8;
                } else {
                    let remainder = ya.wrapping_sub(x * 512);
                    self.a = (255 - remainder / (256 - x)) as u8;
                    self.y = (x + remainder % (256 - x)) as u8;
                }
                self.nz(self.a);
                12
            }
            0x9f => {
                self.a = self.a.rotate_left(4);
                self.nz(self.a);
                5
            }
            0xcf => {
                let result = u16::from(self.a) * u16::from(self.y);
                self.a = result as u8;
                self.y = (result >> 8) as u8;
                self.nz(self.y);
                9
            }
            0xbe => {
                if self.psw & C == 0 || self.a > 0x99 {
                    self.a = self.a.wrapping_sub(0x60);
                    self.psw &= !C;
                }
                if self.psw & H == 0 || self.a & 15 > 9 {
                    self.a = self.a.wrapping_sub(6);
                }
                self.nz(self.a);
                3
            }
            0xdf => {
                if self.psw & C != 0 || self.a > 0x99 {
                    self.a = self.a.wrapping_add(0x60);
                    self.psw |= C;
                }
                if self.psw & H != 0 || self.a & 15 > 9 {
                    self.a = self.a.wrapping_add(6);
                }
                self.nz(self.a);
                3
            }
            0x5d | 0x7d | 0xdd | 0xfd => {
                match op {
                    0x5d => {
                        self.x = self.a;
                        self.nz(self.x);
                    }
                    0x7d => {
                        self.a = self.x;
                        self.nz(self.a);
                    }
                    0xdd => {
                        self.a = self.y;
                        self.nz(self.a);
                    }
                    _ => {
                        self.y = self.a;
                        self.nz(self.y);
                    }
                };
                2
            }
            0xaf => {
                bus.write(self.dp(self.x), self.a);
                self.x = self.x.wrapping_add(1);
                4
            }
            0xbf => {
                self.a = bus.read(self.dp(self.x));
                self.x = self.x.wrapping_add(1);
                self.nz(self.a);
                4
            }
            0xef | 0xff => {
                self.halted = true;
                3
            }
            0xfa => {
                let src = self.fetch(bus);
                let dst = self.fetch(bus);
                let value = bus.read(self.dp(src));
                bus.write(self.dp(dst), value);
                5
            }
            // A/X/Y load/store addressing forms occupy the final four rows.
            _ => {
                let store = row < 14;
                let indexed = row & 1 != 0;
                let (address, reg, cycles) = match col {
                    4 => {
                        let dp = self
                            .fetch(bus)
                            .wrapping_add(if indexed { self.x } else { 0 });
                        (self.dp(dp), 0, if indexed { 4 } else { 3 })
                    }
                    5 => {
                        let base = self.word(bus);
                        (
                            base.wrapping_add(if indexed { u16::from(self.x) } else { 0 }),
                            0,
                            if indexed { 5 } else { 4 },
                        )
                    }
                    6 => {
                        if indexed {
                            let base = self.word(bus);
                            (base.wrapping_add(u16::from(self.y)), 0, 5)
                        } else {
                            (self.dp(self.x), 0, 3)
                        }
                    }
                    7 => {
                        let dp = self
                            .fetch(bus)
                            .wrapping_add(if indexed { 0 } else { self.x });
                        (
                            self.pointer(bus, dp).wrapping_add(if indexed {
                                u16::from(self.y)
                            } else {
                                0
                            }),
                            0,
                            6,
                        )
                    }
                    8 if op == 0xd8 => {
                        let dp = self.fetch(bus);
                        (self.dp(dp), 1, 3)
                    }
                    9 => {
                        let address = if indexed {
                            let dp = self.fetch(bus).wrapping_add(self.y);
                            self.dp(dp)
                        } else {
                            self.word(bus)
                        };
                        (address, 1, 4)
                    }
                    0xb => {
                        let dp = self
                            .fetch(bus)
                            .wrapping_add(if indexed { self.x } else { 0 });
                        (self.dp(dp), 2, if indexed { 4 } else { 3 })
                    }
                    0xc => {
                        let address = self.word(bus);
                        (address, 2, 4)
                    }
                    0xe if op == 0xf8 => unreachable!(),
                    _ if op == 0xf8 => {
                        let dp = self.fetch(bus);
                        (self.dp(dp), 1, 3)
                    }
                    _ => panic!(
                        "SPC700 opcode {op:02X} not decoded at {:04X}",
                        self.pc.wrapping_sub(1)
                    ),
                };
                if store {
                    let value = match reg {
                        1 => self.x,
                        2 => self.y,
                        _ => self.a,
                    };
                    bus.write(address, value);
                    cycles + 1
                } else {
                    let value = bus.read(address);
                    match reg {
                        1 => self.x = value,
                        2 => self.y = value,
                        _ => self.a = value,
                    };
                    self.nz(value);
                    cycles
                }
            }
        }
    }
}

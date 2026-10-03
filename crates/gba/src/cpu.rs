//! ARMv4T interpreter authored for RustBoy. Register 15 stores the next fetch
//! address; operand reads add the architecturally visible pipeline offset.
//! Timing is instruction-granular, not yet a cycle-level instruction pipeline.
mod thumb;
pub const N: u32 = 1 << 31;
pub const Z: u32 = 1 << 30;
pub const C: u32 = 1 << 29;
pub const V: u32 = 1 << 28;
pub const I: u32 = 1 << 7;
pub const T: u32 = 1 << 5;

/// Statically dispatched bus: no virtual call or allocation per CPU access.
pub trait Memory {
    fn read8(&mut self, address: u32) -> u8;
    fn read16(&mut self, address: u32, sequential: bool) -> u16;
    fn read32(&mut self, address: u32, sequential: bool) -> u32;
    fn write8(&mut self, address: u32, value: u8);
    fn write16(&mut self, address: u32, value: u16, sequential: bool);
    fn write32(&mut self, address: u32, value: u32, sequential: bool);
    fn internal(&mut self, clocks: u32);
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Cpu {
    pub r: [u32; 16],
    pub(crate) cpsr: u32,
    pub(crate) banks: [[u32; 7]; 6],
    pub(crate) spsr: [u32; 6],
    pub(crate) sequential: bool,
    pub instructions: u64,
    pub exceptions: u64,
}
impl Default for Cpu {
    fn default() -> Self {
        Self {
            r: [0; 16],
            cpsr: 0xd3,
            banks: [[0; 7]; 6],
            spsr: [0; 6],
            sequential: false,
            instructions: 0,
            exceptions: 0,
        }
    }
}
pub(crate) fn bank(mode: u32) -> Option<usize> {
    Some(match mode & 31 {
        0x10 | 0x1f => 0,
        0x11 => 1,
        0x12 => 2,
        0x13 => 3,
        0x17 => 4,
        0x1b => 5,
        _ => return None,
    })
}
pub fn add_with_carry(a: u32, b: u32, carry: bool) -> (u32, bool, bool) {
    let sum = u64::from(a) + u64::from(b) + u64::from(carry);
    let value = sum as u32;
    (value, sum >> 32 != 0, (!(a ^ b) & (a ^ value)) & N != 0)
}
pub fn shift(value: u32, kind: u32, amount: u32, immediate: bool, carry: bool) -> (u32, bool) {
    if amount == 0 {
        if !immediate || kind == 0 {
            return (value, carry);
        }
        if kind == 3 {
            return ((u32::from(carry) << 31) | (value >> 1), value & 1 != 0);
        }
    }
    let amount = if amount == 0 { 32 } else { amount };
    match kind {
        0 => match amount {
            1..=31 => (value << amount, value >> (32 - amount) & 1 != 0),
            32 => (0, value & 1 != 0),
            _ => (0, false),
        },
        1 => match amount {
            1..=31 => (value >> amount, value >> (amount - 1) & 1 != 0),
            32 => (0, value & N != 0),
            _ => (0, false),
        },
        2 => {
            if amount >= 32 {
                (((value as i32) >> 31) as u32, value & N != 0)
            } else {
                (
                    ((value as i32) >> amount) as u32,
                    value >> (amount - 1) & 1 != 0,
                )
            }
        }
        _ => {
            let result = value.rotate_right(amount & 31);
            (result, result & N != 0)
        }
    }
}
impl Cpu {
    pub(crate) fn return_swi<B: Memory>(&mut self, bus: &mut B) {
        let pc = self.r[14];
        self.write_result(bus, 15, pc, true);
    }
    pub fn cpsr(&self) -> u32 {
        self.cpsr
    }
    pub fn thumb(&self) -> bool {
        self.cpsr & T != 0
    }
    pub fn set_cpsr(&mut self, value: u32) -> Result<(), &'static str> {
        let next = bank(value).ok_or("Invalid ARM processor mode")?;
        let old = bank(self.cpsr).unwrap();
        if old != next {
            let shared = if old == 1 { 1 } else { 0 };
            self.banks[shared][..5].copy_from_slice(&self.r[8..13]);
            self.banks[old][5..].copy_from_slice(&self.r[13..15]);
            let shared = if next == 1 { 1 } else { 0 };
            self.r[8..13].copy_from_slice(&self.banks[shared][..5]);
            self.r[13..15].copy_from_slice(&self.banks[next][5..]);
        }
        self.cpsr = value & 0xf00000ff;
        Ok(())
    }
    pub fn condition(&self, condition: u32) -> bool {
        let [n, z, c, v] = [N, Z, C, V].map(|bit| self.cpsr & bit != 0);
        match condition {
            0 => z,
            1 => !z,
            2 => c,
            3 => !c,
            4 => n,
            5 => !n,
            6 => v,
            7 => !v,
            8 => c && !z,
            9 => !c || z,
            10 => n == v,
            11 => n != v,
            12 => !z && n == v,
            13 => z || n != v,
            14 => true,
            _ => false,
        }
    }
    fn operand(&self, reg: usize, extra: u32) -> u32 {
        if reg == 15 {
            self.r[15]
                .wrapping_add(if self.thumb() { 2 } else { 4 })
                .wrapping_add(extra)
        } else {
            self.r[reg]
        }
    }
    fn flag(&mut self, bit: u32, value: bool) {
        self.cpsr = (self.cpsr & !bit) | if value { bit } else { 0 };
    }
    fn nz(&mut self, value: u32) {
        self.flag(N, value & N != 0);
        self.flag(Z, value == 0);
    }
    fn arithmetic(&mut self, value: u32, carry: bool, overflow: bool) {
        self.nz(value);
        self.flag(C, carry);
        self.flag(V, overflow);
    }
    fn branch<B: Memory>(&mut self, bus: &mut B, target: u32, exchange: bool) {
        if exchange {
            self.flag(T, target & 1 != 0);
        }
        self.r[15] = target & if self.thumb() { !1 } else { !3 };
        // Incremental refill N+S, in addition to this instruction's fetch.
        if self.thumb() {
            bus.read16(self.r[15], false);
            bus.read16(self.r[15].wrapping_add(2), true);
        } else {
            bus.read32(self.r[15], false);
            bus.read32(self.r[15].wrapping_add(4), true);
        }
        self.sequential = true;
    }
    fn exception<B: Memory>(&mut self, bus: &mut B, mode: u32, vector: u32, lr: u32) {
        let old = self.cpsr;
        self.set_cpsr((old & !(31 | T)) | mode | I).unwrap();
        self.spsr[bank(mode).unwrap()] = old;
        self.r[14] = lr;
        self.exceptions = self.exceptions.wrapping_add(1);
        self.branch(bus, vector, false);
    }
    pub fn irq<B: Memory>(&mut self, bus: &mut B) -> bool {
        if self.cpsr & I != 0 {
            return false;
        }
        self.exception(bus, 0x12, 0x18, self.r[15].wrapping_add(4));
        true
    }
    fn undefined<B: Memory>(&mut self, bus: &mut B) {
        self.exception(bus, 0x1b, 4, self.r[15]);
    }
    pub fn step<B: Memory>(&mut self, bus: &mut B) {
        self.instructions = self.instructions.wrapping_add(1);
        if self.thumb() {
            let op = bus.read16(self.r[15], self.sequential);
            self.r[15] = self.r[15].wrapping_add(2);
            self.sequential = true;
            self.execute_thumb(bus, op);
        } else {
            let op = bus.read32(self.r[15], self.sequential);
            self.r[15] = self.r[15].wrapping_add(4);
            self.sequential = true;
            if self.condition(op >> 28) {
                self.execute_arm(bus, op);
            }
        }
    }
    fn shifter<B: Memory>(&self, bus: &mut B, op: u32) -> (u32, bool) {
        let carry = self.cpsr & C != 0;
        if op & 0x02000000 != 0 {
            let rotation = (op >> 8 & 15) * 2;
            let value = (op & 255).rotate_right(rotation);
            (value, if rotation == 0 { carry } else { value & N != 0 })
        } else {
            let register = op & 16 != 0;
            let amount = if register {
                bus.internal(1);
                self.operand((op >> 8 & 15) as usize, 0) & 255
            } else {
                op >> 7 & 31
            };
            shift(
                self.operand((op & 15) as usize, if register { 4 } else { 0 }),
                op >> 5 & 3,
                amount,
                !register,
                carry,
            )
        }
    }
    fn write_result<B: Memory>(&mut self, bus: &mut B, rd: usize, value: u32, restore: bool) {
        if rd == 15 {
            let mode = bank(self.cpsr).unwrap();
            if restore && mode != 0 && self.set_cpsr(self.spsr[mode]).is_err() {
                self.undefined(bus);
                return;
            }
            self.branch(bus, value, false);
        } else {
            self.r[rd] = value;
        }
    }
    fn user_read(&self, reg: usize) -> u32 {
        let mode = bank(self.cpsr).unwrap();
        if ((13..15).contains(&reg) && mode != 0) || ((8..13).contains(&reg) && mode == 1) {
            self.banks[0][reg - 8]
        } else {
            self.operand(reg, if reg == 15 { 4 } else { 0 })
        }
    }
    fn user_write(&mut self, reg: usize, value: u32) {
        let mode = bank(self.cpsr).unwrap();
        if ((13..15).contains(&reg) && mode != 0) || ((8..13).contains(&reg) && mode == 1) {
            self.banks[0][reg - 8] = value;
        } else {
            self.r[reg] = value;
        }
    }
    fn multiply_cycles(value: u32, signed: bool) -> u32 {
        for bytes in 1..=3 {
            let high = value >> (bytes * 8);
            if high == 0 || (signed && high == u32::MAX >> (bytes * 8)) {
                return bytes;
            }
        }
        4
    }
    fn execute_arm<B: Memory>(&mut self, bus: &mut B, op: u32) {
        let rn = (op >> 16 & 15) as usize;
        let rd = (op >> 12 & 15) as usize;
        let rm = (op & 15) as usize;
        if op & 0x0ffffff0 == 0x012fff10 {
            self.branch(bus, self.operand(rm, 0), true);
            return;
        }
        if op & 0x0fbf0fff == 0x010f0000 {
            self.r[rd] = if op & 0x00400000 != 0 {
                self.spsr[bank(self.cpsr).unwrap()]
            } else {
                self.cpsr
            };
            return;
        }
        if op & 0x0fb0fff0 == 0x0120f000 || op & 0x0fb0f000 == 0x0320f000 {
            let value = if op & 0x02000000 != 0 {
                (op & 255).rotate_right((op >> 8 & 15) * 2)
            } else {
                self.operand(rm, 0)
            };
            let mut mask = if op & 0x00080000 != 0 { 0xf0000000 } else { 0 };
            if op & 0x00010000 != 0 && self.cpsr & 31 != 0x10 {
                mask |= if op & 0x00400000 != 0 { 0xff } else { 0xdf };
            }
            let mode = bank(self.cpsr).unwrap();
            if op & 0x00400000 != 0 {
                if mode != 0 {
                    self.spsr[mode] = (self.spsr[mode] & !mask) | (value & mask);
                }
            } else {
                let next = (self.cpsr & !mask) | (value & mask);
                let _ = self.set_cpsr(next);
            }
            return;
        }
        if op & 0x0f8000f0 == 0x00800090 {
            let signed = op & 0x00400000 != 0;
            let rs = (op >> 8 & 15) as usize;
            let mut value = if signed {
                (self.r[rm] as i32 as i64).wrapping_mul(self.r[rs] as i32 as i64) as u64
            } else {
                u64::from(self.r[rm]) * u64::from(self.r[rs])
            };
            if op & 0x00200000 != 0 {
                value = value.wrapping_add((u64::from(self.r[rn]) << 32) | u64::from(self.r[rd]));
            }
            bus.internal(
                Self::multiply_cycles(self.r[rs], signed) + 1 + u32::from(op & 0x00200000 != 0),
            );
            self.r[rd] = value as u32;
            self.r[rn] = (value >> 32) as u32;
            if op & 0x00100000 != 0 {
                self.flag(N, value >> 63 != 0);
                self.flag(Z, value == 0);
            }
            return;
        }
        if op & 0x0fc000f0 == 0x00000090 {
            let rs = (op >> 8 & 15) as usize;
            let mut value = self.r[rm].wrapping_mul(self.r[rs]);
            bus.internal(Self::multiply_cycles(self.r[rs], true) + u32::from(op & 0x00200000 != 0));
            if op & 0x00200000 != 0 {
                value = value.wrapping_add(self.r[rd]);
            }
            self.r[rn] = value;
            if op & 0x00100000 != 0 {
                self.nz(value);
            }
            return;
        }
        if op & 0x0fb00ff0 == 0x01000090 {
            let address = self.operand(rn, 0);
            let value = self.operand(rm, 0);
            let old = if op & 0x00400000 != 0 {
                let old = u32::from(bus.read8(address));
                bus.write8(address, value as u8);
                old
            } else {
                let old = bus
                    .read32(address & !3, false)
                    .rotate_right((address & 3) * 8);
                bus.write32(address & !3, value, false);
                old
            };
            self.r[rd] = old;
            bus.internal(1);
            return;
        }
        if op & 0x0e000090 == 0x00000090 && op & 0x60 != 0 {
            let offset = if op & 0x00400000 != 0 {
                (op >> 4 & 0xf0) | (op & 15)
            } else {
                self.operand(rm, 0)
            };
            let base = self.operand(rn, 0);
            let adjusted = if op & 0x00800000 != 0 {
                base.wrapping_add(offset)
            } else {
                base.wrapping_sub(offset)
            };
            let address = if op & 0x01000000 != 0 { adjusted } else { base };
            let load = op & 0x00100000 != 0;
            if load {
                let value = match op >> 5 & 3 {
                    1 => u32::from(bus.read16(address & !1, false)).rotate_right((address & 1) * 8),
                    2 => bus.read8(address) as i8 as i32 as u32,
                    _ => {
                        if address & 1 != 0 {
                            bus.read8(address) as i8 as i32 as u32
                        } else {
                            bus.read16(address, false) as i16 as i32 as u32
                        }
                    }
                };
                bus.internal(1);
                self.write_result(bus, rd, value, false);
            } else if op >> 5 & 3 == 1 {
                bus.write16(address & !1, self.operand(rd, 4) as u16, false);
            } else {
                self.undefined(bus);
                return;
            }
            if (op & 0x01000000 == 0 || op & 0x00200000 != 0) && !(load && rd == rn) {
                self.r[rn] = adjusted;
            }
            return;
        }
        match op >> 25 & 7 {
            0 | 1 => {
                let opcode = op >> 21 & 15;
                let flags = op & 0x00100000 != 0 || (8..12).contains(&opcode);
                if ((8..12).contains(&opcode) && op & 0x00100000 == 0) || op & 0x02000090 == 0x90 {
                    self.undefined(bus);
                    return;
                }
                let (b, carry) = self.shifter(bus, op);
                let a = self.operand(rn, if op & 0x02000010 == 16 { 4 } else { 0 });
                let old_c = self.cpsr & C != 0;
                let (value, c, v) = match opcode {
                    0 | 8 => (a & b, carry, false),
                    1 | 9 => (a ^ b, carry, false),
                    2 | 10 => add_with_carry(a, !b, true),
                    3 => add_with_carry(b, !a, true),
                    4 | 11 => add_with_carry(a, b, false),
                    5 => add_with_carry(a, b, old_c),
                    6 => add_with_carry(a, !b, old_c),
                    7 => add_with_carry(b, !a, old_c),
                    12 => (a | b, carry, false),
                    13 => (b, carry, false),
                    14 => (a & !b, carry, false),
                    _ => (!b, carry, false),
                };
                if flags && !(!(8..12).contains(&opcode) && rd == 15) {
                    self.nz(value);
                    self.flag(C, c);
                    if (2..=7).contains(&opcode) || opcode == 10 || opcode == 11 {
                        self.flag(V, v);
                    }
                }
                if !(8..12).contains(&opcode) {
                    self.write_result(bus, rd, value, flags);
                }
            }
            2 | 3 => {
                if op & 0x02000010 == 0x02000010 {
                    self.undefined(bus);
                    return;
                }
                let offset = if op & 0x02000000 != 0 {
                    shift(
                        self.operand(rm, 0),
                        op >> 5 & 3,
                        op >> 7 & 31,
                        true,
                        self.cpsr & C != 0,
                    )
                    .0
                } else {
                    op & 4095
                };
                let base = self.operand(rn, 0);
                let adjusted = if op & 0x00800000 != 0 {
                    base.wrapping_add(offset)
                } else {
                    base.wrapping_sub(offset)
                };
                let address = if op & 0x01000000 != 0 { adjusted } else { base };
                let load = op & 0x00100000 != 0;
                if load {
                    let value = if op & 0x00400000 != 0 {
                        u32::from(bus.read8(address))
                    } else {
                        bus.read32(address & !3, false)
                            .rotate_right((address & 3) * 8)
                    };
                    bus.internal(1);
                    self.write_result(bus, rd, value, false);
                } else {
                    let value = self.operand(rd, 4);
                    if op & 0x00400000 != 0 {
                        bus.write8(address, value as u8);
                    } else {
                        bus.write32(address & !3, value, false);
                    }
                }
                if (op & 0x01000000 == 0 || op & 0x00200000 != 0) && !(load && rn == rd) {
                    self.r[rn] = adjusted;
                }
            }
            4 => {
                let load = op & 0x00100000 != 0;
                let user = op & 0x00400000 != 0;
                let writeback = op & 0x00200000 != 0;
                let empty = op & 65535 == 0;
                let list = if empty { 0x8000 } else { op & 65535 };
                let count = if empty { 16 } else { list.count_ones() };
                let base = self.operand(rn, 0);
                let up = op & 0x00800000 != 0;
                let pre = op & 0x01000000 != 0;
                let final_base = if up {
                    base.wrapping_add(count * 4)
                } else {
                    base.wrapping_sub(count * 4)
                };
                let mut address = if up {
                    base.wrapping_add(if pre { 4 } else { 0 })
                } else {
                    final_base.wrapping_add(if pre { 0 } else { 4 })
                };
                let restore = user && list & 0x8000 != 0;
                let mut pc = None;
                let mut first = true;
                for reg in 0..16 {
                    if list >> reg & 1 == 0 {
                        continue;
                    }
                    if load {
                        let value = bus.read32(address & !3, !first);
                        if reg == 15 {
                            pc = Some(value);
                        } else if user && !restore {
                            self.user_write(reg, value);
                        } else {
                            self.r[reg] = value;
                        }
                    } else {
                        let value = if reg == rn && writeback && !first {
                            final_base
                        } else if user {
                            self.user_read(reg)
                        } else {
                            self.operand(reg, 4)
                        };
                        bus.write32(address & !3, value, !first);
                    }
                    first = false;
                    address = address.wrapping_add(4);
                }
                if writeback && !(load && list >> rn & 1 != 0) {
                    self.r[rn] = final_base;
                }
                if load {
                    bus.internal(1);
                }
                if let Some(pc) = pc {
                    self.write_result(bus, 15, pc, restore);
                }
            }
            5 => {
                let target = self
                    .operand(15, 0)
                    .wrapping_add((((op & 0xffffff) << 8) as i32 >> 6) as u32);
                if op & 0x01000000 != 0 {
                    self.r[14] = self.r[15];
                }
                self.branch(bus, target, false);
            }
            7 if op & 0x01000000 != 0 => self.exception(bus, 0x13, 8, self.r[15]),
            _ => self.undefined(bus),
        }
    }
}

#[cfg(test)]
mod tests;

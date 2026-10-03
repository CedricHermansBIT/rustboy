use super::*;
impl Cpu {
    pub(super) fn execute_thumb<B: Memory>(&mut self, bus: &mut B, instruction: u16) {
        let op = u32::from(instruction);
        let rd = (op & 7) as usize;
        let rs = (op >> 3 & 7) as usize;
        match op >> 13 {
            0 => {
                if op & 0x1800 == 0x1800 {
                    let b = if op & 0x400 != 0 {
                        op >> 6 & 7
                    } else {
                        self.r[(op >> 6 & 7) as usize]
                    };
                    let (value, c, v) = if op & 0x200 != 0 {
                        add_with_carry(self.r[rs], !b, true)
                    } else {
                        add_with_carry(self.r[rs], b, false)
                    };
                    self.r[rd] = value;
                    self.arithmetic(value, c, v);
                } else {
                    let (value, c) = shift(
                        self.r[rs],
                        op >> 11 & 3,
                        op >> 6 & 31,
                        true,
                        self.cpsr & C != 0,
                    );
                    self.r[rd] = value;
                    self.nz(value);
                    self.flag(C, c);
                }
            }
            1 => {
                let reg = (op >> 8 & 7) as usize;
                let value = op & 255;
                match op >> 11 & 3 {
                    0 => {
                        self.r[reg] = value;
                        self.nz(value);
                    }
                    kind => {
                        let (result, c, v) = if kind == 2 {
                            add_with_carry(self.r[reg], value, false)
                        } else {
                            add_with_carry(self.r[reg], !value, true)
                        };
                        self.arithmetic(result, c, v);
                        if kind != 1 {
                            self.r[reg] = result;
                        }
                    }
                }
            }
            2 => {
                if op & 0xfc00 == 0x4000 {
                    let a = self.r[rd];
                    let b = self.r[rs];
                    let carry = self.cpsr & C != 0;
                    let kind = op >> 6 & 15;
                    let (value, c, v) = match kind {
                        0 => (a & b, carry, None),
                        1 => (a ^ b, carry, None),
                        2 | 3 | 4 | 7 => {
                            bus.internal(1);
                            let (value, c) = shift(
                                a,
                                if kind == 7 { 3 } else { kind - 2 },
                                b & 255,
                                false,
                                carry,
                            );
                            (value, c, None)
                        }
                        5 | 6 | 9 | 10 | 11 => {
                            let (value, c, v) = match kind {
                                5 => add_with_carry(a, b, carry),
                                6 => add_with_carry(a, !b, carry),
                                9 => add_with_carry(0, !b, true),
                                10 => add_with_carry(a, !b, true),
                                _ => add_with_carry(a, b, false),
                            };
                            (value, c, Some(v))
                        }
                        8 => (a & b, carry, None),
                        12 => (a | b, carry, None),
                        13 => {
                            bus.internal(Self::multiply_cycles(b, true));
                            (a.wrapping_mul(b), carry, None)
                        }
                        14 => (a & !b, carry, None),
                        _ => (!b, carry, None),
                    };
                    self.nz(value);
                    self.flag(C, c);
                    if let Some(v) = v {
                        self.flag(V, v);
                    }
                    if ![8, 10, 11].contains(&kind) {
                        self.r[rd] = value;
                    }
                } else if op & 0xfc00 == 0x4400 {
                    let dst = (op & 7 | op >> 4 & 8) as usize;
                    let src = (op >> 3 & 15) as usize;
                    let a = self.operand(dst, 0);
                    let b = self.operand(src, 0);
                    match op >> 8 & 3 {
                        0 => self.write_result(bus, dst, a.wrapping_add(b), false),
                        1 => {
                            let (value, c, v) = add_with_carry(a, !b, true);
                            self.arithmetic(value, c, v);
                        }
                        2 => self.write_result(bus, dst, b, false),
                        _ => {
                            if op & 0x80 != 0 {
                                self.undefined(bus);
                            } else {
                                self.branch(bus, b, true);
                            }
                        }
                    }
                } else if op & 0xf800 == 0x4800 {
                    let reg = (op >> 8 & 7) as usize;
                    let address = (self.operand(15, 0) & !3).wrapping_add((op & 255) * 4);
                    self.r[reg] = bus.read32(address, false);
                    bus.internal(1);
                } else {
                    let address = self.r[rs].wrapping_add(self.r[(op >> 6 & 7) as usize]);
                    let kind = op >> 9 & 7;
                    match kind {
                        0 => bus.write32(address & !3, self.r[rd], false),
                        1 => bus.write16(address & !1, self.r[rd] as u16, false),
                        2 => bus.write8(address, self.r[rd] as u8),
                        kind => {
                            self.r[rd] = match kind {
                                3 => bus.read8(address) as i8 as i32 as u32,
                                4 => bus
                                    .read32(address & !3, false)
                                    .rotate_right((address & 3) * 8),
                                5 => u32::from(bus.read16(address & !1, false))
                                    .rotate_right((address & 1) * 8),
                                6 => u32::from(bus.read8(address)),
                                _ => {
                                    if address & 1 != 0 {
                                        bus.read8(address) as i8 as i32 as u32
                                    } else {
                                        bus.read16(address, false) as i16 as i32 as u32
                                    }
                                }
                            };
                            bus.internal(1);
                        }
                    }
                }
            }
            3 => {
                let byte = op & 0x1000 != 0;
                let address = self.r[rs].wrapping_add((op >> 6 & 31) * if byte { 1 } else { 4 });
                if op & 0x800 != 0 {
                    self.r[rd] = if byte {
                        u32::from(bus.read8(address))
                    } else {
                        bus.read32(address & !3, false)
                            .rotate_right((address & 3) * 8)
                    };
                    bus.internal(1);
                } else if byte {
                    bus.write8(address, self.r[rd] as u8);
                } else {
                    bus.write32(address & !3, self.r[rd], false);
                }
            }
            4 => {
                if op & 0x1000 == 0 {
                    let address = self.r[rs].wrapping_add((op >> 6 & 31) * 2);
                    if op & 0x800 != 0 {
                        self.r[rd] = u32::from(bus.read16(address & !1, false))
                            .rotate_right((address & 1) * 8);
                        bus.internal(1);
                    } else {
                        bus.write16(address & !1, self.r[rd] as u16, false);
                    }
                } else {
                    let reg = (op >> 8 & 7) as usize;
                    let address = self.r[13].wrapping_add((op & 255) * 4);
                    if op & 0x800 != 0 {
                        self.r[reg] = bus
                            .read32(address & !3, false)
                            .rotate_right((address & 3) * 8);
                        bus.internal(1);
                    } else {
                        bus.write32(address & !3, self.r[reg], false);
                    }
                }
            }
            5 => {
                if op & 0x1000 == 0 {
                    let base = if op & 0x800 != 0 {
                        self.r[13]
                    } else {
                        self.operand(15, 0) & !3
                    };
                    self.r[(op >> 8 & 7) as usize] = base.wrapping_add((op & 255) * 4);
                } else if op & 0xff00 == 0xb000 {
                    let offset = (op & 127) * 4;
                    self.r[13] = if op & 128 != 0 {
                        self.r[13].wrapping_sub(offset)
                    } else {
                        self.r[13].wrapping_add(offset)
                    };
                } else if op & 0xf600 == 0xb400 {
                    let pop = op & 0x800 != 0;
                    let list = op & 255;
                    let extra = op & 256 != 0;
                    let count = list.count_ones() + u32::from(extra);
                    if count == 0 {
                        self.undefined(bus);
                        return;
                    }
                    let mut address = if pop {
                        self.r[13]
                    } else {
                        self.r[13].wrapping_sub(count * 4)
                    };
                    let mut sequential = false;
                    for reg in 0..8 {
                        if list >> reg & 1 == 0 {
                            continue;
                        }
                        if pop {
                            self.r[reg] = bus.read32(address & !3, sequential);
                        } else {
                            bus.write32(address & !3, self.r[reg], sequential);
                        }
                        sequential = true;
                        address = address.wrapping_add(4);
                    }
                    let pc = if extra && pop {
                        Some(bus.read32(address & !3, sequential))
                    } else {
                        if extra {
                            bus.write32(address & !3, self.r[14], sequential);
                        }
                        None
                    };
                    self.r[13] = if pop {
                        self.r[13].wrapping_add(count * 4)
                    } else {
                        self.r[13].wrapping_sub(count * 4)
                    };
                    if pop {
                        bus.internal(1);
                    }
                    if let Some(pc) = pc {
                        self.branch(bus, pc, false);
                    }
                } else {
                    self.undefined(bus);
                }
            }
            6 => {
                if op & 0x1000 == 0 {
                    let base_reg = (op >> 8 & 7) as usize;
                    let base = self.r[base_reg];
                    let list = op & 255;
                    let load = op & 0x800 != 0;
                    let count = if list == 0 { 16 } else { list.count_ones() };
                    let final_base = base.wrapping_add(count * 4);
                    if list == 0 {
                        if load {
                            let pc = bus.read32(base & !3, false);
                            self.branch(bus, pc, false);
                            bus.internal(1);
                        } else {
                            bus.write32(base & !3, self.operand(15, 0), false);
                        }
                        self.r[base_reg] = final_base;
                        return;
                    }
                    let mut address = base;
                    let mut sequential = false;
                    for reg in 0..8 {
                        if list >> reg & 1 == 0 {
                            continue;
                        }
                        if load {
                            self.r[reg] = bus.read32(address & !3, sequential);
                        } else {
                            bus.write32(
                                address & !3,
                                if reg == base_reg && sequential {
                                    final_base
                                } else {
                                    self.r[reg]
                                },
                                sequential,
                            );
                        }
                        sequential = true;
                        address = address.wrapping_add(4);
                    }
                    if !load || list >> base_reg & 1 == 0 {
                        self.r[base_reg] = final_base;
                    }
                    if load {
                        bus.internal(1);
                    }
                } else {
                    let cond = op >> 8 & 15;
                    if cond == 15 {
                        self.exception(bus, 0x13, 8, self.r[15]);
                    } else if cond == 14 {
                        self.undefined(bus);
                    } else if self.condition(cond) {
                        let address = self
                            .operand(15, 0)
                            .wrapping_add(((op as u8 as i8 as i32) * 2) as u32);
                        self.branch(bus, address, false);
                    }
                }
            }
            _ => match op >> 11 & 3 {
                0 => {
                    let target = self
                        .operand(15, 0)
                        .wrapping_add(((op << 21) as i32 >> 20) as u32);
                    self.branch(bus, target, false);
                }
                2 => {
                    self.r[14] = self
                        .operand(15, 0)
                        .wrapping_add(((op << 21) as i32 >> 9) as u32)
                }
                3 => {
                    let address = self.r[14].wrapping_add((op & 2047) * 2);
                    self.r[14] = self.r[15] | 1;
                    self.branch(bus, address, false);
                }
                _ => self.undefined(bus), // BLX is ARMv5, absent on ARM7TDMI.
            },
        }
    }
}

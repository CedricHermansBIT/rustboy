//! General ARM7TDMI-to-WASM compiler. Instruction decoding happens at compile
//! time; generated arithmetic and transfers call only hardware/privilege helpers.
use super::{
    arm::{condition, reg},
    name, section, signed, unsigned, Code, A, B, COUNT, ELAPSED, FLAGS, RESULT, TEMP,
};
const PC: u32 = 27;
const CARRY: u32 = 30;
const COST: u32 = 31;
const ADDRESS: u32 = 32;
const BASE: u32 = 33;
const FINAL: u32 = 34;
const SAVED: u32 = 35;
const AMOUNT: u32 = 36;
const STOP: u32 = 38;
const EXCEPTIONS: u32 = 39;
const LONG: u32 = 40;
fn reload(c: &mut Code) {
    for r in 0..15 {
        c.load(0, r * 4);
        c.set(reg(r));
    }
    c.load(0, 64);
    c.set(FLAGS);
    c.load(0, 60);
    c.set(PC);
}
fn flush(c: &mut Code) {
    for r in 0..15 {
        c.store(0, r * 4, reg(r));
    }
    c.store(0, 64, FLAGS);
    c.store(0, 60, PC);
}
fn operand(c: &mut Code, r: u32, pc: u32, thumb: bool, extra: u32) {
    if r == 15 {
        c.constant(
            pc.wrapping_add(if thumb { 4 } else { 8 })
                .wrapping_add(extra),
        );
    } else {
        c.get(reg(r));
    }
}
fn internal(c: &mut Code, clocks: u32) {
    c.get(1);
    c.constant(clocks);
    c.call(4);
}
fn bit_c(c: &mut Code) {
    c.get(FLAGS);
    c.constant(29);
    c.op(0x76);
    c.constant(1);
    c.op(0x71);
    c.set(CARRY);
}
fn logical_flags(c: &mut Code) {
    c.nz(0xe0000000);
    c.get(FLAGS);
    c.get(CARRY);
    c.constant(29);
    c.op(0x74);
    c.op(0x72);
    c.set(FLAGS);
}
fn result(c: &mut Code, rd: u32, restore: bool) {
    if rd != 15 {
        c.get(RESULT);
        c.set(reg(rd));
    } else if !restore {
        branch(c, false);
    } else {
        flush(c);
        c.get(0);
        c.get(1);
        c.get(RESULT);
        c.constant(2);
        c.call(5);
        reload(c);
    }
}
fn branch(c: &mut Code, exchange: bool) {
    if exchange {
        c.get(FLAGS);
        c.constant(!32);
        c.op(0x71);
        c.get(RESULT);
        c.constant(1);
        c.op(0x71);
        c.constant(5);
        c.op(0x74);
        c.op(0x72);
        c.set(FLAGS);
    }
    c.get(FLAGS);
    c.constant(32);
    c.op(0x71);
    c.op(0x04);
    c.op(0x7f);
    c.get(RESULT);
    c.constant(!1);
    c.op(0x71);
    c.op(0x05);
    c.get(RESULT);
    c.constant(!3);
    c.op(0x71);
    c.op(0x0b);
    c.set(PC);
    c.get(1);
    c.get(PC);
    c.get(FLAGS);
    c.constant(32);
    c.op(0x71);
    c.call(11);
}
fn exception(c: &mut Code, swi: bool) {
    flush(c);
    c.get(0);
    c.get(1);
    c.constant(u32::from(swi));
    c.call(8);
    reload(c);
}
fn read(c: &mut Code, kind: u32, sequential: bool) {
    c.get(1);
    c.get(ADDRESS);
    c.constant(kind);
    c.constant(u32::from(sequential));
    c.call(2);
    c.set(RESULT);
}
fn write(c: &mut Code, kind: u32, sequential: bool) {
    c.get(1);
    c.get(ADDRESS);
    c.get(RESULT);
    c.constant(kind);
    c.constant(u32::from(sequential));
    c.call(3);
}
// B is the shifted operand, AMOUNT its runtime byte count, CARRY incoming C.
fn variable_shift(c: &mut Code, kind: u32) {
    c.get(AMOUNT);
    c.op(0x45);
    c.op(0x04);
    c.op(0x40); // zero preserves value/carry
    c.op(0x05);
    if kind == 3 {
        c.get(B);
        c.get(AMOUNT);
        c.op(0x78);
        c.set(B);
        c.get(B);
        c.constant(31);
        c.op(0x76);
        c.set(CARRY);
    } else {
        c.get(AMOUNT);
        c.constant(32);
        c.op(0x49);
        c.op(0x04);
        c.op(0x40);
        c.get(B);
        if kind == 0 {
            c.constant(32);
            c.get(AMOUNT);
            c.op(0x6b);
        } else {
            c.get(AMOUNT);
            c.constant(1);
            c.op(0x6b);
        }
        c.op(0x76);
        c.constant(1);
        c.op(0x71);
        c.set(CARRY);
        c.get(B);
        c.get(AMOUNT);
        c.op(match kind {
            0 => 0x74,
            1 => 0x76,
            _ => 0x75,
        });
        c.set(B);
        c.op(0x05);
        if kind == 2 {
            c.get(B);
            c.constant(31);
            c.op(0x75);
            c.set(B);
            c.get(B);
            c.constant(1);
            c.op(0x71);
            c.set(CARRY);
        } else {
            c.get(AMOUNT);
            c.constant(32);
            c.op(0x46);
            c.op(0x04);
            c.op(0x7f);
            c.get(B);
            c.constant(if kind == 0 { 0 } else { 31 });
            c.op(0x76);
            c.constant(1);
            c.op(0x71);
            c.op(0x05);
            c.constant(0);
            c.op(0x0b);
            c.set(CARRY);
            c.constant(0);
            c.set(B);
        }
        c.op(0x0b);
    }
    c.op(0x0b);
}
fn immediate_shift(c: &mut Code, kind: u32, amount: u32) {
    if kind == 0 && amount == 0 {
        return;
    }
    if kind == 3 && amount == 0 {
        c.get(B);
        c.constant(1);
        c.op(0x76);
        c.get(CARRY);
        c.constant(31);
        c.op(0x74);
        c.op(0x72);
        c.set(TEMP);
        c.get(B);
        c.constant(1);
        c.op(0x71);
        c.set(CARRY);
        c.get(TEMP);
        c.set(B);
        return;
    }
    c.constant(if amount == 0 { 32 } else { amount });
    c.set(AMOUNT);
    variable_shift(c, kind);
}
fn arithmetic(c: &mut Code, kind: u32, flags: bool) {
    // ADC/SBC/RSC use a 64-bit unsigned sum, retaining original A/B for V.
    if kind == 3 || kind == 7 {
        c.get(A);
        c.get(B);
        c.set(A);
        c.set(B);
    }
    let subtract = matches!(kind, 2 | 3 | 6 | 7 | 10);
    if !matches!(kind, 5..=7) {
        if flags {
            c.arithmetic(subtract);
        } else {
            c.get(A);
            c.get(B);
            c.op(if subtract { 0x6b } else { 0x6a });
            c.set(RESULT);
        }
        return;
    }
    c.get(A);
    c.op(0xad);
    c.get(B);
    if subtract {
        c.constant(u32::MAX);
        c.op(0x73);
    }
    c.op(0xad);
    c.op(0x7c);
    c.get(FLAGS);
    c.constant(29);
    c.op(0x76);
    c.constant(1);
    c.op(0x71);
    c.op(0xad);
    c.op(0x7c);
    c.set(LONG);
    c.get(LONG);
    c.op(0xa7);
    c.set(RESULT);
    if flags {
        c.nz(0xf0000000);
        c.get(FLAGS);
        c.get(LONG);
        c.op(0x42);
        signed(32, &mut c.0);
        c.op(0x88);
        c.op(0xa7);
        c.constant(29);
        c.op(0x74);
        c.op(0x72);
        c.get(A);
        c.get(B);
        c.op(0x73);
        if !subtract {
            c.constant(u32::MAX);
            c.op(0x73);
        }
        c.get(A);
        c.get(RESULT);
        c.op(0x73);
        c.op(0x71);
        c.constant(0x80000000);
        c.op(0x71);
        c.constant(3);
        c.op(0x76);
        c.op(0x72);
        c.set(FLAGS);
    }
}
fn mul_cycles(c: &mut Code, source: u32, signed_mul: bool, extra: u32) {
    c.constant(4);
    c.set(COST);
    for bytes in (1..=3).rev() {
        c.get(source);
        c.constant(bytes * 8);
        c.op(0x76);
        c.set(TEMP);
        c.get(TEMP);
        c.op(0x45);
        if signed_mul {
            c.get(TEMP);
            c.constant(u32::MAX >> (bytes * 8));
            c.op(0x46);
            c.op(0x72);
        }
        c.op(0x04);
        c.op(0x40);
        c.constant(bytes);
        c.set(COST);
        c.op(0x0b);
    }
    c.get(1);
    c.get(COST);
    if extra != 0 {
        c.constant(extra);
        c.op(0x6a);
    }
    c.call(4);
}
fn arm_shifter(c: &mut Code, pc: u32, op: u32) {
    bit_c(c);
    if op & 0x02000000 != 0 {
        let rotation = (op >> 8 & 15) * 2;
        let value = (op & 255).rotate_right(rotation);
        c.constant(value);
        c.set(B);
        if rotation != 0 {
            c.constant(value >> 31);
            c.set(CARRY);
        }
    } else {
        let register = op & 16 != 0;
        operand(c, op & 15, pc, false, if register { 4 } else { 0 });
        c.set(B);
        if register {
            internal(c, 1);
            operand(c, op >> 8 & 15, pc, false, 0);
            c.constant(255);
            c.op(0x71);
            c.set(AMOUNT);
            variable_shift(c, op >> 5 & 3);
        } else {
            immediate_shift(c, op >> 5 & 3, op >> 7 & 31);
        }
    }
}
fn arm(c: &mut Code, pc: u32, op: u32) {
    let rn = op >> 16 & 15;
    let rd = op >> 12 & 15;
    let rm = op & 15;
    if op & 0x0ffffff0 == 0x012fff10 {
        operand(c, rm, pc, false, 0);
        c.set(RESULT);
        branch(c, true);
        return;
    }
    if op & 0x0fbf0fff == 0x010f0000 {
        flush(c);
        c.get(0);
        c.get(1);
        c.constant(0);
        c.constant(0);
        c.constant(u32::from(op & 0x00400000 != 0));
        c.call(6);
        c.set(RESULT);
        reload(c);
        c.get(RESULT);
        c.set(reg(rd));
        return;
    }
    if op & 0x0fb0fff0 == 0x0120f000 || op & 0x0fb0f000 == 0x0320f000 {
        if op & 0x02000000 != 0 {
            c.constant((op & 255).rotate_right((op >> 8 & 15) * 2));
        } else {
            operand(c, rm, pc, false, 0);
        }
        c.set(RESULT);
        flush(c);
        c.get(0);
        c.get(1);
        c.get(RESULT);
        c.constant(op >> 16 & 15);
        c.constant(2 + u32::from(op & 0x00400000 != 0));
        c.call(6);
        c.op(0x1a);
        reload(c);
        return;
    }
    if op & 0x0f8000f0 == 0x00800090 {
        let signed_mul = op & 0x00400000 != 0;
        let rs = op >> 8 & 15;
        mul_cycles(c, reg(rs), signed_mul, 1 + u32::from(op & 0x00200000 != 0));
        c.get(reg(rm));
        c.op(if signed_mul { 0xac } else { 0xad });
        c.get(reg(rs));
        c.op(if signed_mul { 0xac } else { 0xad });
        c.op(0x7e);
        if op & 0x00200000 != 0 {
            c.get(reg(rn));
            c.op(0xad);
            c.op(0x42);
            signed(32, &mut c.0);
            c.op(0x86);
            c.get(reg(rd));
            c.op(0xad);
            c.op(0x84);
            c.op(0x7c);
        }
        c.set(LONG);
        c.get(LONG);
        c.op(0xa7);
        c.set(reg(rd));
        c.get(LONG);
        c.op(0x42);
        signed(32, &mut c.0);
        c.op(0x88);
        c.op(0xa7);
        c.set(reg(rn));
        if op & 0x00100000 != 0 {
            c.get(FLAGS);
            c.constant(!0xc0000000);
            c.op(0x71);
            c.get(LONG);
            c.op(0x42);
            signed(32, &mut c.0);
            c.op(0x88);
            c.op(0xa7);
            c.constant(0x80000000);
            c.op(0x71);
            c.op(0x72);
            c.get(LONG);
            c.op(0x50);
            c.constant(30);
            c.op(0x74);
            c.op(0x72);
            c.set(FLAGS);
        }
        return;
    }
    if op & 0x0fc000f0 == 0x00000090 {
        let rs = op >> 8 & 15;
        mul_cycles(c, reg(rs), true, u32::from(op & 0x00200000 != 0));
        c.get(reg(rm));
        c.get(reg(rs));
        c.op(0x6c);
        if op & 0x00200000 != 0 {
            c.get(reg(rd));
            c.op(0x6a);
        }
        c.set(RESULT);
        if op & 0x00100000 != 0 {
            c.nz(0xc0000000);
        }
        c.get(RESULT);
        c.set(reg(rn));
        return;
    }
    if op & 0x0fb00ff0 == 0x01000090 {
        operand(c, rn, pc, false, 0);
        c.set(ADDRESS);
        operand(c, rm, pc, false, 0);
        c.set(SAVED);
        let kind = if op & 0x00400000 != 0 { 1 } else { 0 };
        read(c, kind, false);
        c.get(RESULT);
        c.set(FINAL);
        c.get(SAVED);
        c.set(RESULT);
        write(c, kind, false);
        c.get(FINAL);
        c.set(reg(rd));
        internal(c, 1);
        return;
    }
    if op & 0x0e000090 == 0x00000090 && op & 0x60 != 0 {
        if op & 0x00400000 != 0 {
            c.constant((op >> 4 & 0xf0) | (op & 15));
        } else {
            operand(c, rm, pc, false, 0);
        }
        c.set(B);
        transfer(
            c,
            pc,
            op,
            match op >> 5 & 3 {
                1 => 2,
                2 => 3,
                _ => 4,
            },
            true,
        );
        return;
    }
    match op >> 25 & 7 {
        0 | 1 => {
            let kind = op >> 21 & 15;
            let flags = op & 0x00100000 != 0;
            if ((8..12).contains(&kind) && !flags) || op & 0x02000090 == 0x90 {
                exception(c, false);
                return;
            }
            arm_shifter(c, pc, op);
            operand(c, rn, pc, false, if op & 0x02000010 == 16 { 4 } else { 0 });
            c.set(A);
            if matches!(kind, 2..=7 | 10 | 11) {
                arithmetic(c, kind, flags && rd != 15);
            } else {
                match kind {
                    13 => c.get(B),
                    15 => {
                        c.get(B);
                        c.constant(u32::MAX);
                        c.op(0x73);
                    }
                    _ => {
                        c.get(A);
                        c.get(B);
                        if kind == 14 {
                            c.constant(u32::MAX);
                            c.op(0x73);
                        }
                        c.op(match kind {
                            0 | 8 | 14 => 0x71,
                            1 | 9 => 0x73,
                            _ => 0x72,
                        });
                    }
                }
                c.set(RESULT);
                if flags && rd != 15 {
                    logical_flags(c);
                }
            }
            if !(8..12).contains(&kind) {
                result(c, rd, flags);
            } else if rd == 15 {
                flush(c);
                c.get(0);
                c.get(1);
                c.constant(0);
                c.constant(0);
                c.constant(4);
                c.call(6);
                c.op(0x1a);
                reload(c);
            }
        }
        2 | 3 => {
            if op & 0x02000010 == 0x02000010 {
                exception(c, false);
                return;
            }
            if op & 0x02000000 != 0 {
                operand(c, rm, pc, false, 0);
                c.set(B);
                bit_c(c);
                immediate_shift(c, op >> 5 & 3, op >> 7 & 31);
            } else {
                c.constant(op & 4095);
                c.set(B);
            }
            transfer(c, pc, op, if op & 0x00400000 != 0 { 1 } else { 0 }, false);
        }
        4 => multiple(c, pc, op, false),
        5 => {
            if op & 0x01000000 != 0 {
                c.constant(pc.wrapping_add(4));
                c.set(reg(14));
            }
            c.constant(
                pc.wrapping_add(8)
                    .wrapping_add(((op << 8) as i32 >> 6) as u32),
            );
            c.set(RESULT);
            branch(c, false);
        }
        7 if op & 0x01000000 != 0 => exception(c, true),
        _ => exception(c, false),
    }
}
fn transfer(c: &mut Code, pc: u32, op: u32, kind: u32, half: bool) {
    let rn = op >> 16 & 15;
    let rd = op >> 12 & 15;
    let load = op & 0x00100000 != 0;
    operand(c, rn, pc, false, 0);
    c.set(BASE);
    c.get(BASE);
    c.get(B);
    c.op(if op & 0x00800000 != 0 { 0x6a } else { 0x6b });
    c.set(FINAL);
    c.get(if op & 0x01000000 != 0 { FINAL } else { BASE });
    c.set(ADDRESS);
    if load {
        read(c, kind, false);
        internal(c, 1);
        result(c, rd, false);
    } else if half && kind != 2 {
        exception(c, false);
        return;
    } else {
        operand(c, rd, pc, false, 4);
        c.set(RESULT);
        write(c, kind, false);
    }
    if (op & 0x01000000 == 0 || op & 0x00200000 != 0) && !(load && rd == rn) {
        c.get(FINAL);
        c.set(reg(rn));
    }
}
fn multiple(c: &mut Code, pc: u32, op: u32, thumb: bool) {
    let rn = if thumb { op >> 8 & 7 } else { op >> 16 & 15 };
    let load = op & if thumb { 0x800 } else { 0x00100000 } != 0;
    let user = !thumb && op & 0x00400000 != 0;
    let wb = thumb || op & 0x00200000 != 0;
    let raw = op & if thumb { 255 } else { 65535 };
    let empty = raw == 0;
    let list = if empty { 0x8000 } else { raw };
    let count = if empty { 16 } else { list.count_ones() };
    let up = thumb || op & 0x00800000 != 0;
    let pre = !thumb && op & 0x01000000 != 0;
    let restore = user && list & 0x8000 != 0;
    operand(c, rn, pc, thumb, 0);
    c.set(BASE);
    c.get(BASE);
    c.constant(count * 4);
    c.op(if up { 0x6a } else { 0x6b });
    c.set(FINAL);
    c.get(if up { BASE } else { FINAL });
    c.constant(if up && pre || !up && !pre { 4 } else { 0 });
    c.op(0x6a);
    c.set(ADDRESS);
    let mut first = true;
    for r in 0..16 {
        if list >> r & 1 == 0 {
            continue;
        }
        if load {
            read(c, 5, !first);
            if r == 15 {
                c.get(RESULT);
                c.set(SAVED);
            } else if user && !restore {
                flush(c);
                c.get(0);
                c.constant(r);
                c.get(RESULT);
                c.call(7);
                reload(c);
            } else {
                c.get(RESULT);
                c.set(reg(r));
            }
        } else {
            if r == rn && wb && !first {
                c.get(FINAL);
            } else if user {
                flush(c);
                c.get(0);
                c.constant(r);
                c.call(10);
            } else {
                operand(c, r, pc, thumb, if thumb { 2 } else { 4 });
            }
            c.set(RESULT);
            write(c, 5, !first);
        }
        first = false;
        c.get(ADDRESS);
        c.constant(4);
        c.op(0x6a);
        c.set(ADDRESS);
    }
    if wb && !(load && !empty && list >> rn & 1 != 0) {
        c.get(FINAL);
        c.set(reg(rn));
    }
    // Thumb empty-list LDM refills before the load's internal cycle.
    if thumb && empty && load {
        c.get(SAVED);
        c.set(RESULT);
        branch(c, false);
        internal(c, 1);
    } else {
        if load {
            internal(c, 1);
        }
        if load && list & 0x8000 != 0 {
            c.get(SAVED);
            c.set(RESULT);
            result(c, 15, restore);
        }
    }
}
fn thumb(c: &mut Code, pc: u32, op: u32) {
    let rd = op & 7;
    let rs = op >> 3 & 7;
    match op >> 13 {
        0 => {
            if op & 0x1800 == 0x1800 {
                c.get(reg(rs));
                c.set(A);
                if op & 0x400 != 0 {
                    c.constant(op >> 6 & 7);
                } else {
                    c.get(reg(op >> 6 & 7));
                }
                c.set(B);
                arithmetic(c, if op & 0x200 != 0 { 2 } else { 4 }, true);
            } else {
                c.get(reg(rs));
                c.set(B);
                bit_c(c);
                immediate_shift(c, op >> 11 & 3, op >> 6 & 31);
                c.get(B);
                c.set(RESULT);
                logical_flags(c);
            }
            result(c, rd, false);
        }
        1 => {
            let dst = op >> 8 & 7;
            let kind = op >> 11 & 3;
            if kind == 0 {
                c.constant(op & 255);
                c.set(RESULT);
                c.nz(0xc0000000);
            } else {
                c.get(reg(dst));
                c.set(A);
                c.constant(op & 255);
                c.set(B);
                arithmetic(c, if kind == 2 { 4 } else { 2 }, true);
            }
            if kind != 1 {
                result(c, dst, false);
            }
        }
        2 if op & 0xfc00 == 0x4000 => {
            let kind = op >> 6 & 15;
            c.get(reg(rd));
            c.set(A);
            c.get(reg(rs));
            c.set(B);
            bit_c(c);
            match kind {
                2 | 3 | 4 | 7 => {
                    internal(c, 1);
                    c.get(B);
                    c.constant(255);
                    c.op(0x71);
                    c.set(AMOUNT);
                    c.get(A);
                    c.set(B);
                    variable_shift(c, if kind == 7 { 3 } else { kind - 2 });
                    c.get(B);
                    c.set(RESULT);
                    logical_flags(c);
                }
                5 | 6 | 9 | 10 | 11 => {
                    if kind == 9 {
                        c.constant(0);
                        c.set(A);
                    }
                    arithmetic(
                        c,
                        match kind {
                            5 => 5,
                            6 => 6,
                            9 | 10 => 2,
                            _ => 4,
                        },
                        true,
                    );
                }
                13 => {
                    mul_cycles(c, reg(rs), true, 0);
                    c.get(A);
                    c.get(B);
                    c.op(0x6c);
                    c.set(RESULT);
                    logical_flags(c);
                }
                _ => {
                    if kind == 15 {
                        c.get(B);
                        c.constant(u32::MAX);
                        c.op(0x73);
                    } else {
                        c.get(A);
                        c.get(B);
                        if kind == 14 {
                            c.constant(u32::MAX);
                            c.op(0x73);
                        }
                        c.op(match kind {
                            0 | 8 | 14 => 0x71,
                            1 => 0x73,
                            _ => 0x72,
                        });
                    }
                    c.set(RESULT);
                    logical_flags(c);
                }
            }
            if ![8, 10, 11].contains(&kind) {
                result(c, rd, false);
            }
        }
        2 if op & 0xfc00 == 0x4400 => {
            let dst = op & 7 | op >> 4 & 8;
            let src = op >> 3 & 15;
            operand(c, dst, pc, true, 0);
            c.set(A);
            operand(c, src, pc, true, 0);
            c.set(B);
            match op >> 8 & 3 {
                0 => {
                    c.get(A);
                    c.get(B);
                    c.op(0x6a);
                    c.set(RESULT);
                    result(c, dst, false);
                }
                1 => arithmetic(c, 2, true),
                2 => {
                    c.get(B);
                    c.set(RESULT);
                    result(c, dst, false);
                }
                _ => {
                    if op & 0x80 != 0 {
                        exception(c, false);
                    } else {
                        c.get(B);
                        c.set(RESULT);
                        branch(c, true);
                    }
                }
            }
        }
        2 if op & 0xf800 == 0x4800 => {
            c.constant((pc.wrapping_add(4) & !3).wrapping_add((op & 255) * 4));
            c.set(ADDRESS);
            read(c, 0, false);
            internal(c, 1);
            result(c, op >> 8 & 7, false);
        }
        2 => {
            c.get(reg(rs));
            c.get(reg(op >> 6 & 7));
            c.op(0x6a);
            c.set(ADDRESS);
            let kind = op >> 9 & 7;
            if kind < 3 {
                c.get(reg(rd));
                c.set(RESULT);
                write(c, [0, 2, 1][kind as usize], false);
            } else {
                read(c, [3, 0, 2, 1, 4][kind as usize - 3], false);
                internal(c, 1);
                result(c, rd, false);
            }
        }
        3 | 4 => {
            let (dst, base, offset, kind) = if op >> 13 == 3 {
                (
                    rd,
                    rs,
                    (op >> 6 & 31) * if op & 0x1000 != 0 { 1 } else { 4 },
                    if op & 0x1000 != 0 { 1 } else { 0 },
                )
            } else if op & 0x1000 == 0 {
                (rd, rs, (op >> 6 & 31) * 2, 2)
            } else {
                (op >> 8 & 7, 13, (op & 255) * 4, 0)
            };
            c.get(reg(base));
            c.constant(offset);
            c.op(0x6a);
            c.set(ADDRESS);
            if op & 0x800 != 0 {
                read(c, kind, false);
                internal(c, 1);
                result(c, dst, false);
            } else {
                c.get(reg(dst));
                c.set(RESULT);
                write(c, kind, false);
            }
        }
        5 => {
            if op & 0x1000 == 0 {
                if op & 0x800 != 0 {
                    c.get(reg(13));
                } else {
                    c.constant(pc.wrapping_add(4) & !3);
                }
                c.constant((op & 255) * 4);
                c.op(0x6a);
                c.set(reg(op >> 8 & 7));
            } else if op & 0xff00 == 0xb000 {
                c.get(reg(13));
                c.constant((op & 127) * 4);
                c.op(if op & 128 != 0 { 0x6b } else { 0x6a });
                c.set(reg(13));
            } else if op & 0xf600 == 0xb400 {
                stack(c, op);
            } else {
                exception(c, false);
            }
        }
        6 if op & 0x1000 == 0 => multiple(c, pc, op, true),
        6 => {
            let cond = op >> 8 & 15;
            if cond >= 14 {
                exception(c, cond == 15);
            } else {
                condition(c, cond);
                c.op(0x04);
                c.op(0x40);
                c.constant(
                    pc.wrapping_add(4)
                        .wrapping_add((op as u8 as i8 as i32 * 2) as u32),
                );
                c.set(RESULT);
                branch(c, false);
                c.op(0x0b);
            }
        }
        _ => match op >> 11 & 3 {
            0 => {
                c.constant(
                    pc.wrapping_add(4)
                        .wrapping_add(((op << 21) as i32 >> 20) as u32),
                );
                c.set(RESULT);
                branch(c, false);
            }
            2 => {
                c.constant(
                    pc.wrapping_add(4)
                        .wrapping_add(((op << 21) as i32 >> 9) as u32),
                );
                c.set(reg(14));
            }
            3 => {
                c.get(reg(14));
                c.constant((op & 2047) * 2);
                c.op(0x6a);
                c.set(RESULT);
                c.constant(pc.wrapping_add(2) | 1);
                c.set(reg(14));
                branch(c, false);
            }
            _ => exception(c, false),
        },
    }
}
fn stack(c: &mut Code, op: u32) {
    let pop = op & 0x800 != 0;
    let extra = op & 256 != 0;
    let list = op & 255;
    let count = list.count_ones() + u32::from(extra);
    if count == 0 {
        exception(c, false);
        return;
    }
    c.get(reg(13));
    c.constant(count * 4);
    c.op(if pop { 0x6a } else { 0x6b });
    c.set(FINAL);
    c.get(if pop { reg(13) } else { FINAL });
    c.set(ADDRESS);
    let mut first = true;
    for r in 0..8 {
        if list >> r & 1 == 0 {
            continue;
        }
        if pop {
            read(c, 5, !first);
            result(c, r, false);
        } else {
            c.get(reg(r));
            c.set(RESULT);
            write(c, 5, !first);
        }
        first = false;
        c.get(ADDRESS);
        c.constant(4);
        c.op(0x6a);
        c.set(ADDRESS);
    }
    if extra {
        if pop {
            read(c, 5, !first);
            c.get(RESULT);
            c.set(SAVED);
        } else {
            c.get(reg(14));
            c.set(RESULT);
            write(c, 5, !first);
        }
    }
    c.get(FINAL);
    c.set(reg(13));
    if pop {
        internal(c, 1);
        if extra {
            c.get(SAVED);
            c.set(RESULT);
            branch(c, false);
        }
    }
}
/// Terminate on writes, indirect/control transfers, privilege changes or traps.
/// This also prevents executing any cached successor after self-modifying code.
pub fn terminal(op: u32, thumb: bool) -> bool {
    if thumb {
        match op >> 13 {
            0 | 1 => false,
            2 if op & 0xfc00 == 0x4000 || op & 0xf800 == 0x4800 => false,
            2 if op & 0xfc00 == 0x4400 => {
                op >> 8 & 3 == 3 || ((op & 7 | op >> 4 & 8) == 15 && op >> 8 & 3 != 1)
            }
            2 => op >> 9 & 7 < 3,
            3 | 4 => op & 0x800 == 0,
            5 => op & 0x1000 != 0 && op & 0xff00 != 0xb000,
            6 => true,
            _ => op >> 11 & 3 != 2,
        }
    } else {
        let transfer =
            matches!(op >> 25 & 7, 2 | 3) || op & 0x0e000090 == 0x00000090 && op & 0x60 != 0;
        if transfer && op >> 16 & 15 == 15 && (op & 0x01000000 == 0 || op & 0x00200000 != 0) {
            return true;
        }
        if op & 0x0fbf0fff == 0x010f0000 {
            return op >> 12 & 15 == 15;
        }
        if op & 0x0fb0fff0 == 0x0120f000 || op & 0x0fb0f000 == 0x0320f000 {
            return true;
        }
        if op & 0x0fc000f0 == 0x00000090 || op & 0x0f8000f0 == 0x00800090 {
            return op >> 16 & 15 == 15 || op >> 12 & 15 == 15;
        }
        if op & 0x0ffffff0 == 0x012fff10 || op & 0x0fb00ff0 == 0x01000090 {
            return true;
        }
        if op & 0x0e000090 == 0x00000090 && op & 0x60 != 0 {
            return op & 0x00100000 == 0 || op >> 12 & 15 == 15;
        }
        match op >> 25 & 7 {
            0 | 1 => {
                op >> 12 & 15 == 15
                    || ((8..12).contains(&(op >> 21 & 15)) && op & 0x00100000 == 0)
                    || op & 0x02000090 == 0x90
            }
            2 | 3 => op & 0x00100000 == 0 || op >> 12 & 15 == 15 || op & 0x02000010 == 0x02000010,
            _ => true,
        }
    }
}
/// Immutable executable storage permits continuing after ordinary stores.
/// Privilege, traps and writes to PC remain control-flow exits in every region.
pub fn ends_block(address: u32, op: u32, thumb: bool) -> bool {
    if !(matches!(address >> 24, 8..=12) || address < 0x4000) {
        return terminal(op, thumb);
    }
    if thumb {
        return match op >> 13 {
            0 | 1 | 3 | 4 => false,
            2 if op & 0xfc00 == 0x4000 || op & 0xf800 == 0x4800 => false,
            2 if op & 0xfc00 == 0x4400 => terminal(op, true),
            2 => false,
            5 if op & 0xf600 == 0xb400 => op & 0x1ff == 0 || op & 0x900 == 0x900,
            5 => terminal(op, true),
            6 if op & 0x1000 == 0 => op & 255 == 0 && op & 0x800 != 0,
            _ => terminal(op, true),
        };
    }
    if op & 0x0fb00ff0 == 0x01000090 {
        return op >> 12 & 15 == 15;
    }
    if op & 0x0e000090 == 0x00000090 && op & 0x60 != 0 {
        return op >> 12 & 15 == 15
            || op & 0x00100000 == 0 && op >> 5 & 3 != 1
            || op >> 16 & 15 == 15 && (op & 0x01000000 == 0 || op & 0x00200000 != 0);
    }
    match op >> 25 & 7 {
        2 | 3 => {
            op >> 12 & 15 == 15
                || op & 0x02000010 == 0x02000010
                || op >> 16 & 15 == 15 && (op & 0x01000000 == 0 || op & 0x00200000 != 0)
        }
        4 => {
            op & 0x00100000 != 0 && (op & 65535 == 0 || op & 0x8000 != 0)
                || op >> 16 & 15 == 15 && op & 0x00200000 != 0
        }
        _ => terminal(op, false),
    }
}
fn pure(op: u32, thumb: bool) -> bool {
    if terminal(op, thumb) {
        return false;
    }
    if thumb {
        op >> 13 <= 1
            || op & 0xfc00 == 0x4000 && !matches!(op >> 6 & 15, 2 | 3 | 4 | 7 | 13)
            || op & 0xfc00 == 0x4400
            || op & 0xf000 == 0xa000
            || op & 0xff00 == 0xb000
            || op & 0xf800 == 0xf000
    } else {
        op & 0x0c000000 == 0
            && (op & 0x02000000 != 0 || op & 16 == 0)
            && op & 0x02000090 != 0x90
            && !(8..12).contains(&(op >> 21 & 15))
            || op & 0x0c000000 == 0
                && (op & 0x02000000 != 0 || op & 16 == 0)
                && op & 0x02000090 != 0x90
                && op & 0x00100000 != 0
    }
}
pub fn compile(address: u32, words: &[u32], thumb_mode: bool) -> Option<Vec<u8>> {
    if words.is_empty() || words.len() > 32 {
        return None;
    }
    let mut c = Code(Vec::new());
    c.op(2);
    c.op(37);
    c.op(0x7f);
    c.op(1);
    c.op(0x7e);
    reload(&mut c);
    c.load(0, std::mem::offset_of!(crate::cpu::Cpu, exceptions) as u32);
    c.set(EXCEPTIONS);
    c.op(0x02);
    c.op(0x40);
    for (index, &op) in words.iter().enumerate() {
        if ends_block(address, op, thumb_mode) && index + 1 != words.len() {
            return None;
        }
        let pc = address.wrapping_add(index as u32 * if thumb_mode { 2 } else { 4 });
        let batched = pure(op, thumb_mode);
        if batched {
            if index == 0 || !pure(words[index - 1], thumb_mode) {
                let count = words[index..]
                    .iter()
                    .take_while(|&&op| pure(op, thumb_mode))
                    .count();
                c.get(0);
                c.get(1);
                c.constant(pc);
                c.constant(count as u32);
                c.get(2);
                c.get(ELAPSED);
                c.op(0x6b);
                c.constant(u32::from(thumb_mode));
                c.call(9);
                c.set(LONG);
                c.get(LONG);
                c.op(0xa7);
                c.constant(0);
                c.op(0x48);
                c.set(STOP);
                c.get(LONG);
                c.op(0xa7);
                c.constant(0x7fffffff);
                c.op(0x71);
                c.get(ELAPSED);
                c.op(0x6a);
                c.set(ELAPSED);
                c.get(LONG);
                c.op(0x42);
                signed(32, &mut c.0);
                c.op(0x88);
                c.op(0xa7);
                c.set(COUNT);
            }
        } else {
            c.get(0);
            c.get(1);
            c.constant(pc);
            c.constant(u32::from(thumb_mode));
            c.call(0);
        }
        c.constant(pc.wrapping_add(if thumb_mode { 2 } else { 4 }));
        c.set(PC);
        if !thumb_mode && op >> 28 != 14 {
            condition(&mut c, op >> 28);
            if op >> 28 == 15 {
                c.op(0x1a);
                c.constant(0);
            }
            c.op(0x04);
            c.op(0x40);
        }
        if thumb_mode {
            thumb(&mut c, pc, op);
        } else {
            arm(&mut c, pc, op);
        }
        if !thumb_mode && op >> 28 != 14 {
            c.op(0x0b);
        }
        c.load(0, std::mem::offset_of!(crate::cpu::Cpu, exceptions) as u32);
        c.get(EXCEPTIONS);
        c.op(0x47);
        c.op(0x0d);
        c.op(0);
        if batched {
            c.get(COUNT);
            c.constant(1);
            c.op(0x6b);
            c.set(COUNT);
            c.get(COUNT);
            c.op(0x45);
            c.get(ELAPSED);
            c.get(2);
            c.op(0x4f);
            c.get(STOP);
            c.op(0x72);
            c.op(0x71);
            c.op(0x0d);
            c.op(0);
        } else {
            c.finish();
        }
    }
    c.op(0x0b);
    flush(&mut c);
    c.get(ELAPSED);
    c.op(0x0b);
    let mut module = b"\0asm\x01\0\0\0".to_vec();
    let mut types = vec![13];

    for (args, result) in [
        (4, 0),
        (2, 1),
        (4, 1),
        (5, 0),
        (2, 0),
        (4, 0),
        (5, 1),
        (3, 0),
        (3, 0),
        (6, 2),
        (2, 1),
        (3, 0),
        (3, 1),
    ] {
        types.extend([0x60, args]);
        types.extend(std::iter::repeat_n(0x7f, args as usize));
        types.push(u8::from(result != 0));
        if result != 0 {
            types.push(if result == 2 { 0x7e } else { 0x7f });
        }
    }
    section(1, types, &mut module);
    let mut imports = vec![13];
    name("env", &mut imports);
    name("memory", &mut imports);
    imports.extend([2, 0, 0]);
    for (index, symbol) in [
        "fetch_mode",
        "finish",
        "read_full",
        "write_full",
        "internal",
        "control",
        "status",
        "bank_write",
        "exception",
        "fetch_span",
        "bank_read",
        "refill",
    ]
    .iter()
    .enumerate()
    {
        name("env", &mut imports);
        name(symbol, &mut imports);
        imports.extend([0, index as u8]);
    }
    section(2, imports, &mut module);
    section(3, vec![1, 12], &mut module);
    let mut exports = vec![1];
    name("run", &mut exports);
    exports.extend([0, 12]);
    section(7, exports, &mut module);
    let mut body = vec![1];
    unsigned(c.0.len() as u32, &mut body);
    body.extend(c.0);
    section(10, body, &mut module);
    Some(module)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn stores_continue_only_in_immutable_code_and_pc_writes_always_exit() {
        assert!(compile(0x08000100, &[0x6001, 0x3001], true).is_some());
        assert!(compile(0x03000100, &[0x6001, 0x3001], true).is_none());
        for address in [0x03000100, 0x08000100] {
            for op in [0xe49f0004, 0xe1bf00b2, 0xe1a0f000, 0xe129f000, 0xef060000] {
                assert!(ends_block(address, op, false), "{op:08x}");
                assert!(compile(address, &[op, 0xe1a00000], false).is_none());
            }
        }
    }
}

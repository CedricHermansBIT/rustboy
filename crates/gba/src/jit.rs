//! ARM7TDMI WASM code generation: complete ARM/Thumb instruction families plus
//! specialized connected IWRAM arithmetic regions. The original Thumb block
//! compiler remains available to its independent differential harness.
fn unsigned(mut value: u32, out: &mut Vec<u8>) {
    loop {
        let byte = (value & 127) as u8;
        value >>= 7;
        out.push(byte | if value != 0 { 128 } else { 0 });
        if value == 0 {
            break;
        }
    }
}
fn signed(mut value: i32, out: &mut Vec<u8>) {
    loop {
        let byte = (value & 127) as u8;
        value >>= 7;
        let done = (value == 0 && byte & 64 == 0) || (value == -1 && byte & 64 != 0);
        out.push(byte | if done { 0 } else { 128 });
        if done {
            break;
        }
    }
}
fn name(value: &str, out: &mut Vec<u8>) {
    unsigned(value.len() as u32, out);
    out.extend_from_slice(value.as_bytes());
}
fn section(id: u8, payload: Vec<u8>, out: &mut Vec<u8>) {
    out.push(id);
    unsigned(payload.len() as u32, out);
    out.extend(payload);
}

const FLAGS: u32 = 11;
const A: u32 = 12;
const B: u32 = 13;
const RESULT: u32 = 14;
const ELAPSED: u32 = 15;
const CLOCKS: u32 = 16;
const TEMP: u32 = 17;
const COUNT: u32 = 18;
const PACKED: u32 = 19;
struct Code(Vec<u8>);
impl Code {
    fn op(&mut self, op: u8) {
        self.0.push(op);
    }
    fn get(&mut self, local: u32) {
        self.op(0x20);
        unsigned(local, &mut self.0);
    }
    fn set(&mut self, local: u32) {
        self.op(0x21);
        unsigned(local, &mut self.0);
    }
    fn constant(&mut self, value: u32) {
        self.op(0x41);
        signed(value as i32, &mut self.0);
    }
    fn call(&mut self, index: u32) {
        self.op(0x10);
        unsigned(index, &mut self.0);
    }
    fn load(&mut self, ptr: u32, offset: u32) {
        self.get(ptr);
        self.op(0x28);
        unsigned(2, &mut self.0);
        unsigned(offset, &mut self.0);
    }
    fn store(&mut self, ptr: u32, offset: u32, local: u32) {
        self.get(ptr);
        self.get(local);
        self.op(0x36);
        unsigned(2, &mut self.0);
        unsigned(offset, &mut self.0);
    }
    fn flush(&mut self) {
        for r in 0..8 {
            self.store(0, r * 4, 3 + r);
        }
        self.store(0, 64, FLAGS);
    }
    fn nz(&mut self, mask: u32) {
        self.get(FLAGS);
        self.constant(!mask);
        self.op(0x71);
        self.get(RESULT);
        self.constant(0x80000000);
        self.op(0x71);
        self.op(0x72);
        self.get(RESULT);
        self.op(0x45);
        self.constant(30);
        self.op(0x74);
        self.op(0x72);
        self.set(FLAGS);
    }
    fn arithmetic(&mut self, subtract: bool) {
        self.get(A);
        self.get(B);
        self.op(if subtract { 0x6b } else { 0x6a });
        self.set(RESULT);
        self.nz(0xf0000000);
        self.get(FLAGS);
        if subtract {
            self.get(A);
            self.get(B);
            self.op(0x4f);
        } else {
            self.get(RESULT);
            self.get(A);
            self.op(0x49);
        }
        self.constant(29);
        self.op(0x74);
        self.op(0x72);
        self.get(A);
        self.get(B);
        self.op(0x73);
        if !subtract {
            self.constant(u32::MAX);
            self.op(0x73);
        }
        self.get(A);
        self.get(RESULT);
        self.op(0x73);
        self.op(0x71);
        self.constant(0x80000000);
        self.op(0x71);
        self.constant(3);
        self.op(0x76);
        self.op(0x72);
        self.set(FLAGS);
    }
    fn finish(&mut self) {
        self.get(0);
        self.get(1);
        self.call(1);
        self.set(CLOCKS);
        self.get(ELAPSED);
        self.get(CLOCKS);
        self.constant(0);
        self.op(0x48);
        self.op(0x04);
        self.op(0x7f); // if (result i32)
        self.constant(0);
        self.get(CLOCKS);
        self.op(0x6b);
        self.op(0x05);
        self.get(CLOCKS);
        self.op(0x0b);
        self.op(0x6a);
        self.set(ELAPSED);
        self.get(CLOCKS);
        self.constant(0);
        self.op(0x48);
        self.get(ELAPSED);
        self.get(2);
        self.op(0x4f);
        self.op(0x72);
        self.op(0x0d);
        self.op(0); // exit at the first whole-instruction boundary
    }
}

/// Supported instructions, and whether they terminate a block.
pub fn supported(op: u16) -> Option<bool> {
    match op >> 13 {
        0 | 1 => Some(false),
        2 if op & 0xfc00 == 0x4000 => match (op >> 6) & 15 {
            0 | 1 | 8..=12 | 14 | 15 => Some(false),
            _ => None,
        },
        2 if op & 0xf800 == 0x4800 => Some(false), // PC-relative LDR
        3 | 4 => Some(false),                      // immediate word/byte/halfword and SP-relative
        6 if op & 0xf000 == 0xd000 && op & 0xff00 < 0xde00 => Some(true),
        7 if op & 0xf800 == 0xe000 => Some(true),
        _ => None,
    }
}

fn pure(op: u16) -> bool {
    supported(op).is_some() && (op >> 13 <= 1 || op & 0xfc00 == 0x4000)
}

/// Compile at most 32 instructions. Branch targets stay under the scheduler.
pub fn compile(address: u32, instructions: &[u16]) -> Option<Vec<u8>> {
    if instructions.is_empty() || instructions.len() > 32 {
        return None;
    }
    let mut c = Code(Vec::new());
    // Three i32 parameters; fifteen i32 locals, including eight resident registers.
    c.op(2);
    c.op(16);
    c.op(0x7f);
    c.op(1);
    c.op(0x7e);
    for r in 0..8 {
        c.load(0, r * 4);
        c.set(3 + r);
    }
    c.load(0, 64);
    c.set(FLAGS);
    c.op(0x02);
    c.op(0x40); // block: all early exits flush architectural state
    for (index, &op) in instructions.iter().enumerate() {
        let terminal = supported(op)?;
        if terminal && index + 1 != instructions.len() {
            return None;
        }
        let pc = address.wrapping_add(index as u32 * 2);
        let arithmetic_run = pure(op);
        if arithmetic_run {
            if index == 0 || !pure(instructions[index - 1]) {
                let count = instructions[index..]
                    .iter()
                    .take_while(|&&word| pure(word))
                    .count();
                c.get(0);
                c.get(1);
                c.constant(pc);
                c.constant(count as u32);
                c.get(2);
                c.get(ELAPSED);
                c.op(0x6b);
                c.call(6);
                c.set(PACKED);
                c.get(PACKED);
                c.op(0xa7);
                c.set(CLOCKS);
                c.get(ELAPSED);
                c.get(CLOCKS);
                c.op(0x6a);
                c.set(ELAPSED);
                c.get(PACKED);
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
            c.call(0);
        }
        let rd = 3 + u32::from(op & 7);
        let rs = 3 + u32::from((op >> 3) & 7);
        match op >> 13 {
            0 if op & 0x1800 == 0x1800 => {
                c.get(rs);
                c.set(A);
                if op & 0x400 != 0 {
                    c.constant(u32::from((op >> 6) & 7));
                } else {
                    c.get(3 + u32::from((op >> 6) & 7));
                }
                c.set(B);
                c.arithmetic(op & 0x200 != 0);
                c.get(RESULT);
                c.set(rd);
            }
            0 => {
                let kind = (op >> 11) & 3;
                let amount = u32::from((op >> 6) & 31);
                c.get(rs);
                c.set(A);
                if kind == 0 && amount == 0 {
                    c.get(A);
                    c.set(RESULT);
                    c.nz(0xc0000000);
                } else {
                    let shift = if amount == 0 { 32 } else { amount };
                    c.get(A);
                    c.constant(if kind == 0 { 32 - shift } else { shift - 1 });
                    c.op(0x76);
                    c.constant(1);
                    c.op(0x71);
                    c.constant(29);
                    c.op(0x74);
                    c.set(TEMP);
                    if shift == 32 && kind == 1 {
                        c.constant(0);
                    } else {
                        c.get(A);
                        c.constant(shift.min(31));
                        c.op(match kind {
                            0 => 0x74,
                            1 => 0x76,
                            _ => 0x75,
                        });
                    }
                    c.set(RESULT);
                    c.nz(0xe0000000);
                    c.get(FLAGS);
                    c.get(TEMP);
                    c.op(0x72);
                    c.set(FLAGS);
                }
                c.get(RESULT);
                c.set(rd);
            }
            1 => {
                let dest = 3 + u32::from((op >> 8) & 7);
                let kind = (op >> 11) & 3;
                if kind == 0 {
                    c.constant(u32::from(op & 255));
                    c.set(RESULT);
                    c.nz(0xc0000000);
                } else {
                    c.get(dest);
                    c.set(A);
                    c.constant(u32::from(op & 255));
                    c.set(B);
                    c.arithmetic(kind != 2);
                }
                if kind != 1 {
                    c.get(RESULT);
                    c.set(dest);
                }
            }
            2 if op & 0xfc00 == 0x4000 => {
                let kind = (op >> 6) & 15;
                c.get(rd);
                c.set(A);
                c.get(rs);
                c.set(B);
                match kind {
                    9 => {
                        c.constant(0);
                        c.set(A);
                        c.arithmetic(true);
                    }
                    10 => c.arithmetic(true),
                    11 => c.arithmetic(false),
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
                        c.nz(0xc0000000);
                    }
                }
                if ![8, 10, 11].contains(&kind) {
                    c.get(RESULT);
                    c.set(rd);
                }
            }
            2 => {
                c.get(1);
                c.constant((pc.wrapping_add(4) & !3).wrapping_add(u32::from(op & 255) * 4));
                c.constant(0);
                c.call(2);
                c.set(3 + u32::from((op >> 8) & 7));
            }
            3 | 4 => {
                let (dest, base, offset, kind, load) = if op >> 13 == 3 {
                    let byte = op & 0x1000 != 0;
                    (
                        rd,
                        rs,
                        u32::from((op >> 6) & 31) * if byte { 1 } else { 4 },
                        if byte { 1 } else { 0 },
                        op & 0x800 != 0,
                    )
                } else if op & 0x1000 == 0 {
                    (rd, rs, u32::from((op >> 6) & 31) * 2, 2, op & 0x800 != 0)
                } else {
                    (
                        3 + u32::from((op >> 8) & 7),
                        u32::MAX,
                        u32::from(op & 255) * 4,
                        0,
                        op & 0x800 != 0,
                    )
                };
                c.get(1);
                if base == u32::MAX {
                    c.load(0, 13 * 4);
                } else {
                    c.get(base);
                }
                c.constant(offset);
                c.op(0x6a);
                if load {
                    c.constant(kind);
                    c.call(2);
                    c.set(dest);
                } else {
                    c.get(dest);
                    c.constant(kind);
                    c.call(3);
                }
            }
            6 | 7 => {
                c.flush();
                c.get(0);
                c.get(1);
                let offset = if op >> 13 == 6 {
                    (op as u8 as i8 as i32) * 2
                } else {
                    (i32::from(op & 0x7ff) << 21) >> 20
                };
                c.constant(pc.wrapping_add(4).wrapping_add(offset as u32));
                c.constant(if op >> 13 == 6 {
                    u32::from((op >> 8) & 15)
                } else {
                    14
                });
                c.call(5);
            }
            _ => return None,
        }
        if arithmetic_run {
            c.get(COUNT);
            c.constant(1);
            c.op(0x6b);
            c.set(COUNT);
            c.get(COUNT);
            c.op(0x45);
            c.get(ELAPSED);
            c.get(2);
            c.op(0x4f);
            c.op(0x71);
            c.op(0x0d);
            c.op(0);
        } else {
            c.finish();
        }
    }
    c.op(0x0b);
    c.flush();
    c.get(ELAPSED);
    c.op(0x0b);
    let mut module = b"\0asm\x01\0\0\0".to_vec();
    let mut types = vec![7];
    for (args, result) in [
        (3, false),
        (2, true),
        (3, true),
        (4, false),
        (2, false),
        (3, true),
    ] {
        types.extend([0x60, args]);
        types.extend(std::iter::repeat_n(0x7f, args as usize));
        types.push(u8::from(result));
        if result {
            types.push(0x7f);
        }
    }
    types.extend([0x60, 5, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 1, 0x7e]);
    section(1, types, &mut module);
    let mut imports = vec![8];
    name("env", &mut imports);
    name("memory", &mut imports);
    imports.extend([2, 0, 0]);
    for (symbol, ty) in [
        ("fetch", 0),
        ("finish", 1),
        ("read", 2),
        ("write", 3),
        ("internal", 4),
        ("branch", 3),
        ("fetch_run", 6),
    ] {
        name("env", &mut imports);
        name(symbol, &mut imports);
        imports.extend([0, ty]);
    }
    section(2, imports, &mut module);
    section(3, vec![1, 5], &mut module);
    let mut exports = vec![1];
    name("run", &mut exports);
    exports.extend([0, 7]);
    section(7, exports, &mut module);
    let mut body = vec![1];
    unsigned(c.0.len() as u32, &mut body);
    body.extend(c.0);
    section(10, body, &mut module);
    Some(module)
}

#[cfg(target_arch = "wasm32")]
mod browser;
#[cfg(target_arch = "wasm32")]
pub use browser::*;

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn generated_cpu_layout_is_explicit_and_stable() {
        assert_eq!(std::mem::offset_of!(crate::cpu::Cpu, r), 0);
        assert_eq!(std::mem::offset_of!(crate::cpu::Cpu, cpsr), 64);
    }
    #[test]
    fn unsupported_control_uses_the_interpreter() {
        for op in [
            0xc000, 0xc800, 0xde00, 0xdf00, 0xf000, 0xf800, 0x4700, 0x4140, 0x4340,
        ] {
            assert_eq!(supported(op), None, "{op:04x}");
            assert_eq!(compile(0x08000000, &[op]), None);
        }
        assert!(compile(0x08000000, &[]).is_none());
        assert!(compile(0x08000000, &[0x2000; 33]).is_none());
        assert!(compile(0x08000000, &[0xe000, 0x2000]).is_none());
    }
}

pub mod arm;
pub mod full;

/// Architectural fields used by the independent native/WASM differential harness.
pub fn verification_state(cpu: &crate::cpu::Cpu) -> Vec<u32> {
    let mut words = cpu.r.to_vec();
    words.push(cpu.cpsr);
    for bank in cpu.banks {
        words.extend(bank);
    }
    words.extend(cpu.spsr);
    words.push(u32::from(cpu.sequential));
    words.extend([
        cpu.instructions as u32,
        (cpu.instructions >> 32) as u32,
        cpu.exceptions as u32,
        (cpu.exceptions >> 32) as u32,
    ]);
    words
}
pub fn verification_layout() -> [usize; 5] {
    use crate::cpu::Cpu;
    [
        std::mem::offset_of!(Cpu, banks),
        std::mem::offset_of!(Cpu, spsr),
        std::mem::offset_of!(Cpu, sequential),
        std::mem::offset_of!(Cpu, instructions),
        std::mem::offset_of!(Cpu, exceptions),
    ]
}

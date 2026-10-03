use super::*;
#[derive(Default)]
struct Ram {
    data: Vec<u8>,
    clocks: u64,
}
impl Ram {
    fn arm(words: &[u32]) -> Self {
        let mut ram = Self {
            data: vec![0; 0x10000],
            clocks: 0,
        };
        for (i, word) in words.iter().enumerate() {
            ram.data[i * 4..i * 4 + 4].copy_from_slice(&word.to_le_bytes());
        }
        ram
    }
    fn thumb(words: &[u16]) -> Self {
        let mut ram = Self {
            data: vec![0; 0x10000],
            clocks: 0,
        };
        for (i, word) in words.iter().enumerate() {
            ram.data[i * 2..i * 2 + 2].copy_from_slice(&word.to_le_bytes());
        }
        ram
    }
}
impl Memory for Ram {
    fn read8(&mut self, a: u32) -> u8 {
        self.clocks += 1;
        self.data[a as usize & 0xffff]
    }
    fn read16(&mut self, a: u32, _: bool) -> u16 {
        u16::from_le_bytes([self.read8(a), self.read8(a + 1)])
    }
    fn read32(&mut self, a: u32, _: bool) -> u32 {
        u32::from_le_bytes(std::array::from_fn(|i| self.read8(a + i as u32)))
    }
    fn write8(&mut self, a: u32, v: u8) {
        self.clocks += 1;
        self.data[a as usize & 0xffff] = v;
    }
    fn write16(&mut self, a: u32, v: u16, _: bool) {
        for (i, b) in v.to_le_bytes().into_iter().enumerate() {
            self.write8(a + i as u32, b);
        }
    }
    fn write32(&mut self, a: u32, v: u32, _: bool) {
        for (i, b) in v.to_le_bytes().into_iter().enumerate() {
            self.write8(a + i as u32, b);
        }
    }
    fn internal(&mut self, c: u32) {
        self.clocks += u64::from(c);
    }
}
fn cpu() -> Cpu {
    let mut c = Cpu::default();
    c.set_cpsr(0x1f).unwrap();
    c
}
#[test]
fn arithmetic_against_wide_oracle() {
    let cases = [0, 1, 0x7fffffff, 0x80000000, 0xfffffffe, 0xffffffff];
    for a in cases {
        for b in cases {
            for carry in [false, true] {
                let (r, c, v) = add_with_carry(a, b, carry);
                let unsigned = u64::from(a) + u64::from(b) + u64::from(carry);
                let signed = i64::from(a as i32) + i64::from(b as i32) + i64::from(carry);
                assert_eq!(r, unsigned as u32);
                assert_eq!(c, unsigned > u64::from(u32::MAX));
                assert_eq!(
                    v,
                    signed < i64::from(i32::MIN) || signed > i64::from(i32::MAX)
                );
            }
        }
    }
    let mut seed = 1u32;
    for _ in 0..10000 {
        seed = seed.wrapping_mul(1664525).wrapping_add(1013904223);
        let a = seed;
        seed = seed.rotate_left(13).wrapping_add(12345);
        let b = seed;
        let (r, c, v) = add_with_carry(a, b, seed & 1 != 0);
        let sum = u64::from(a) + u64::from(b) + u64::from(seed & 1);
        let signed = i64::from(a as i32) + i64::from(b as i32) + i64::from(seed & 1);
        assert_eq!(r, sum as u32);
        assert_eq!(c, sum >> 32 != 0);
        assert_eq!(
            v,
            signed < i64::from(i32::MIN) || signed > i64::from(i32::MAX)
        );
    }
}
#[test]
fn register_shifts_against_single_bit_oracle() {
    for value in [0, 1, 0x80000000, 0xdeadbeef, u32::MAX] {
        for kind in 0..4 {
            for amount in 0..256 {
                let (mut result, mut carry) = (value, true);
                for _ in 0..amount {
                    let old = result;
                    match kind {
                        0 => {
                            result <<= 1;
                            carry = old & N != 0;
                        }
                        1 => {
                            result >>= 1;
                            carry = old & 1 != 0;
                        }
                        2 => {
                            result = ((old as i32) >> 1) as u32;
                            carry = old & 1 != 0;
                        }
                        _ => {
                            result = old.rotate_right(1);
                            carry = old & 1 != 0;
                        }
                    }
                }
                assert_eq!(shift(value, kind, amount, false, true), (result, carry));
            }
        }
    }
    assert_eq!(shift(2, 3, 0, true, true), (0x80000001, false));
    assert_eq!(shift(u32::MAX, 1, 0, true, false), (0, true));
}
#[test]
fn arm_alu_flags_and_conditional_execution() {
    let mut c = cpu();
    let mut m = Ram::arm(&[
        0xe3a00001, 0xe2901002, 0x03a020ff, 0xe3510003, 0x03a02055, 0xe2d13004,
    ]);
    for _ in 0..6 {
        c.step(&mut m);
    }
    assert_eq!(c.r[..4], [1, 3, 0x55, u32::MAX]);
    assert_eq!(c.cpsr & (N | Z | C | V), N);
}
#[test]
fn visible_pc_and_register_shift_pc() {
    let mut c = cpu();
    let mut m = Ram::arm(&[0xe1a0000f, 0xe1a0111f]);
    c.r[1] = 0;
    c.step(&mut m);
    assert_eq!(c.r[0], 8);
    c.step(&mut m);
    assert_eq!(c.r[1], 16);
}
#[test]
fn bx_switches_to_thumb_and_back() {
    let mut c = cpu();
    let mut m = Ram::arm(&[0xe12fff10, 0, 0]);
    m.data[0x20..0x24].copy_from_slice(&[7, 0x21, 0x10, 0x47]);
    c.r[0] = 0x21;
    c.r[2] = 8;
    c.step(&mut m);
    assert!(c.thumb());
    c.step(&mut m);
    assert_eq!(c.r[1], 7);
    c.step(&mut m);
    assert!(!c.thumb());
    assert_eq!(c.r[15], 8);
}
#[test]
fn thumb_arithmetic_stack_and_long_branch() {
    let mut c = cpu();
    c.set_cpsr(0x3f).unwrap();
    c.r[13] = 0x800;
    c.r[14] = 0x123;
    let mut m = Ram::thumb(&[0x2007, 0x3003, 0xb501, 0x2000, 0xbc03, 0xf000, 0xf802]);
    for _ in 0..7 {
        c.step(&mut m);
    }
    assert_eq!(c.r[0], 10);
    assert_eq!(c.r[1], 0x123);
    assert_eq!(c.r[13], 0x800);
    assert_eq!(c.r[15], 18);
    assert_eq!(c.r[14], 15);
}
#[test]
fn signed_and_rotated_unaligned_loads() {
    let mut c = cpu();
    let mut m = Ram::arm(&[0xe5910000, 0xe1d120f0, 0xe1d130b0]);
    c.r[1] = 0x101;
    m.write32(0x100, 0x123480fe, false);
    for _ in 0..3 {
        c.step(&mut m);
    }
    assert_eq!(c.r[0], 0xfe123480);
    assert_eq!(c.r[2], 0xffffff80);
    assert_eq!(c.r[3], 0xfe000080);
}
#[test]
fn banked_fiq_and_exception_return() {
    let mut c = cpu();
    c.r[8] = 8;
    c.r[13] = 0x800;
    c.set_cpsr(0x11).unwrap();
    assert_eq!(c.r[8], 0);
    c.r[8] = 99;
    c.r[13] = 0x900;
    c.set_cpsr(0x1f).unwrap();
    assert_eq!(c.r[8], 8);
    assert_eq!(c.r[13], 0x800);
    let mut m = Ram::arm(&[0xe1b0f00e]);
    c.r[15] = 0x100;
    assert!(c.irq(&mut m));
    assert_eq!(c.r[14], 0x104);
    assert_eq!(c.cpsr & 31, 0x12);
    c.r[15] = 0;
    c.r[14] = 0x100;
    c.step(&mut m);
    assert_eq!(c.cpsr & 31, 0x1f);
    assert_eq!(c.r[15], 0x100);
    assert_eq!(c.r[13], 0x800);
}
#[test]
fn swi_and_undefined_vectors() {
    for (op, mode, pc) in [
        (0xef000001, 0x13, 8),
        (0xe12fff30, 0x1b, 4),
        (0xee000000, 0x1b, 4),
    ] {
        let mut c = cpu();
        let mut m = Ram::arm(&[op]);
        c.step(&mut m);
        assert_eq!(c.cpsr & 31, mode);
        assert_eq!(c.r[15], pc);
        assert_eq!(c.r[14], 4);
        assert_eq!(c.exceptions, 1);
    }
}
#[test]
fn block_transfer_base_quirks() {
    let mut c = cpu();
    let mut m = Ram::arm(&[0xe8a10003, 0xe8b10006]);
    c.r[0] = 0x55;
    c.r[1] = 0x100;
    c.step(&mut m);
    assert_eq!(m.read32(0x100, false), 0x55);
    assert_eq!(m.read32(0x104, false), 0x108);
    assert_eq!(c.r[1], 0x108);
    m.write32(0x108, 0x123, false);
    m.write32(0x10c, 0x456, false);
    c.step(&mut m);
    assert_eq!(c.r[1], 0x123);
    assert_eq!(c.r[2], 0x456);
}
#[test]
fn multiply_long_and_psr_restore_validation() {
    let mut c = cpu();
    let mut m = Ram::arm(&[0xe0810392]);
    c.r[2] = u32::MAX;
    c.r[3] = 2;
    c.step(&mut m);
    assert_eq!(c.r[0], 0xfffffffe);
    assert_eq!(c.r[1], 1);
    c.set_cpsr(0x13).unwrap();
    c.r[15] = 4;
    m.write32(4, 0xe1b0f00e, false);
    c.step(&mut m);
    assert_eq!(c.cpsr & 31, 0x1b);
}

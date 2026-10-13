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
fn halfword_word_stores_keep_source_byte_lanes_on_the_eight_bit_save_bus() {
    // GBATEK Game Pak SRAM: STRH/STR place the addressed source byte on its
    // eight-bit bus. Independent jsmolka SRAM tests exercise the same contract.
    for (thumb, opcode, width) in [
        (false, 0xe1c100b0u32, 2u32), // STRH r0,[r1]
        (false, 0xe5810000, 4),       // STR r0,[r1]
        (true, 0x8008, 2),            // STRH r0,[r1,#0]
        (true, 0x6008, 4),            // STR r0,[r1,#0]
        (true, 0x5288, 2),            // STRH r0,[r1,r2]
        (true, 0x5088, 4),            // STR r0,[r1,r2]
    ] {
        for lane in 0..width {
            let mut rom = vec![0; 192];
            rom[..4].copy_from_slice(&opcode.to_le_bytes());
            rom[0x80..0x89].copy_from_slice(b"SRAM_V000");
            let mut bus = crate::bus::Bus::new(rom, None).unwrap();
            bus.backup.data.fill(0x5a);
            let mut c = cpu();
            c.set_cpsr(0x1f | if thumb { T } else { 0 }).unwrap();
            c.r[0] = 0xaabbccdd;
            c.r[1] = 0x0e000010 + lane;
            c.r[2] = 0;
            c.r[15] = 0x08000000;
            c.step(&mut bus);
            assert_eq!(
                bus.backup.data[0x10 + lane as usize],
                (0xaabbccddu32 >> (lane * 8)) as u8,
                "{opcode:08x}, lane {lane}"
            );
            assert!(bus
                .backup
                .data
                .iter()
                .enumerate()
                .all(|(i, v)| i == 0x10 + lane as usize || *v == 0x5a));
            // SRAM reads repeat its addressed byte on every physical data lane.
            assert_eq!(
                bus.read32(0x0e000010 + lane, false),
                u32::from((0xaabbccddu32 >> (lane * 8)) as u8) * 0x01010101
            );
        }
    }
}
#[test]
fn unaligned_store_helpers_preserve_normal_ram_alignment() {
    let mut ram = Ram::arm(&[0xe1c100b0, 0xe5820000]);
    let mut c = cpu();
    c.r[0] = 0xaabbccdd;
    c.r[1] = 0x101;
    c.r[2] = 0x107;
    c.step(&mut ram);
    assert_eq!(&ram.data[0x100..0x102], &[0xdd, 0xcc]);
    c.step(&mut ram);
    assert_eq!(&ram.data[0x104..0x108], &[0xdd, 0xcc, 0xbb, 0xaa]);
    assert_eq!(ram.data[0x102], 0);
    assert_eq!(ram.data[0x108], 0);
}
#[test]
fn save_bus_loads_select_the_logical_byte_before_arm_rotation_and_sign_extension() {
    for (thumb, opcode, kind) in [
        (false, 0xe1d100b0u32, 0u8), // LDRH r0,[r1]
        (false, 0xe5910000, 1),      // LDR r0,[r1]
        (false, 0xe1d100f0, 2),      // LDRSH r0,[r1]
        (true, 0x8808, 0),           // LDRH r0,[r1,#0]
        (true, 0x5a88, 0),           // LDRH r0,[r1,r2]
        (true, 0x6808, 1),           // LDR r0,[r1,#0]
        (true, 0x5888, 1),           // LDR r0,[r1,r2]
        (true, 0x5e88, 2),           // LDRSH r0,[r1,r2]
    ] {
        for (lane, byte) in [0x80u8, 0x7f, 0xef, 0x21].into_iter().enumerate() {
            let mut rom = vec![0; 192];
            rom[..4].copy_from_slice(&opcode.to_le_bytes());
            rom[0x80..0x89].copy_from_slice(b"SRAM_V000");
            let mut bus = crate::bus::Bus::new(rom, None).unwrap();
            bus.backup.data[0x10..0x14].copy_from_slice(&[0x80, 0x7f, 0xef, 0x21]);
            let mut c = cpu();
            c.set_cpsr(0x1f | if thumb { T } else { 0 }).unwrap();
            c.r[1] = 0x0e000010 + lane as u32;
            c.r[2] = 0;
            c.r[15] = 0x08000000;
            c.step(&mut bus);
            let expected = match kind {
                0 => (u32::from(byte) * 0x101).rotate_right((lane as u32 & 1) * 8),
                1 => u32::from(byte) * 0x01010101,
                _ if lane & 1 != 0 => byte as i8 as i32 as u32,
                _ => (u16::from(byte) * 0x101) as i16 as i32 as u32,
            };
            assert_eq!(c.r[0], expected, "{opcode:08x}, lane {lane}");
        }
    }
}
#[test]
fn conditions_match_boolean_oracle_for_every_condition_and_flag_tuple() {
    let mut c = cpu();
    for flags in 0..16 {
        c.set_cpsr(0x1f | flags << 28).unwrap();
        let n = flags & 8 != 0;
        let z = flags & 4 != 0;
        let carry = flags & 2 != 0;
        let v = flags & 1 != 0;
        let expected = [
            z,
            !z,
            carry,
            !carry,
            n,
            !n,
            v,
            !v,
            carry && !z,
            !carry || z,
            n == v,
            n != v,
            !z && n == v,
            z || n != v,
            true,
            false,
        ];
        for (condition, expected) in expected.into_iter().enumerate() {
            assert_eq!(c.condition(condition as u32), expected);
        }
        assert!(!c.condition(16));
        assert!(!c.condition(u32::MAX));
    }
}
#[test]
fn arm_psr_compares_restore_saved_flags_and_banks_without_pipeline_refill() {
    for opcode in 8..12 {
        for mode in [0x11, 0x12, 0x13, 0x17, 0x1b] {
            let mut c = cpu();
            c.r[8] = 32;
            c.r[13] = 0x1000;
            c.set_cpsr(mode | N).unwrap();
            c.r[8] = 64;
            c.r[13] = 0x2000;
            c.spsr[bank(mode).unwrap()] = 0x1f | C | V;
            let op = 0xe010f000 | (opcode << 21); // Test r0,r0 with Rd=15.
            let mut ram = Ram::arm(&[op, 0xe3a01055]);
            c.step(&mut ram);
            assert_eq!(c.cpsr(), 0x1f | C | V);
            assert_eq!(c.r[15], 4);
            assert_eq!(ram.clocks, 4, "must not refill the pipeline");
            assert_eq!(c.r[13], 0x1000);
            assert_eq!(c.r[8], if mode == 0x11 { 32 } else { 64 });
            assert_eq!(c.exceptions, 0);
            c.step(&mut ram);
            assert_eq!(c.r[1], 0x55);
        }
    }
}

#[test]
fn thumb_empty_stm_stores_pc_plus_six_and_writes_back_sixty_four_bytes() {
    for base_register in 0..8 {
        let mut c = cpu();
        c.set_cpsr(0x3f).unwrap();
        c.r[base_register] = 0x101;
        let mut ram = Ram::thumb(&[0xc000 | ((base_register as u16) << 8), 0x46c0]);
        c.step(&mut ram);
        assert_eq!(c.r[base_register], 0x141);
        assert_eq!(
            u32::from_le_bytes(ram.data[0x100..0x104].try_into().unwrap()),
            6
        );
        assert_eq!(c.r[15], 2);
        assert_eq!(c.cpsr(), 0x3f);
        assert_eq!(c.exceptions, 0);
    }
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

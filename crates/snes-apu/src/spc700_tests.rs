use crate::{spc700::Spc700, SpcRam};

#[test]
fn all_opcodes_have_a_decoder_including_zero_divisor() {
    for opcode in 0..=255 {
        let mut ram = SpcRam::default();
        ram.write(0x200, opcode);
        let mut cpu = Spc700 {
            pc: 0x200,
            ..Default::default()
        };
        assert!((2..=12).contains(&cpu.step(&mut ram)), "{opcode:02X}");
    }
}

#[test]
fn adc_and_sbc_flags_cover_every_operand_and_carry() {
    let mut ram = SpcRam::default();
    for subtract in [false, true] {
        ram.write(0x200, if subtract { 0xa8 } else { 0x88 });
        for a in 0..=255u8 {
            for b in 0..=255u8 {
                ram.write(0x201, b);
                for carry in 0..=1u8 {
                    let mut cpu = Spc700 {
                        pc: 0x200,
                        a,
                        psw: carry,
                        ..Default::default()
                    };
                    assert_eq!(cpu.step(&mut ram), 2);
                    let result = if subtract {
                        i16::from(a) - i16::from(b) - i16::from(1 - carry)
                    } else {
                        i16::from(a) + i16::from(b) + i16::from(carry)
                    };
                    let byte = result as u8;
                    assert_eq!(cpu.a, byte);
                    assert_eq!(
                        cpu.psw & 1 != 0,
                        if subtract { result >= 0 } else { result > 255 }
                    );
                    assert_eq!(cpu.psw & 2 != 0, byte == 0);
                    assert_eq!(cpu.psw & 128 != 0, byte & 128 != 0);
                    let signed = if subtract {
                        i16::from(a as i8) - i16::from(b as i8) - i16::from(1 - carry)
                    } else {
                        i16::from(a as i8) + i16::from(b as i8) + i16::from(carry)
                    };
                    assert_eq!(cpu.psw & 64 != 0, !(-128..=127).contains(&signed));
                    let half = if subtract {
                        (a & 15) as i16 - (b & 15) as i16 - i16::from(1 - carry)
                    } else {
                        (a & 15) as i16 + (b & 15) as i16 + i16::from(carry)
                    };
                    assert_eq!(
                        cpu.psw & 8 != 0,
                        if subtract { half >= 0 } else { half > 15 }
                    );
                }
            }
        }
    }
}

#[test]
fn direct_page_pointer_wrap_and_stack_return() {
    let mut ram = SpcRam::default();
    ram.write_wrapping(0x200, &[0xe7, 0xff, 0x3f, 0x00, 0x03]);
    ram.write(0x1ff, 0x00);
    ram.write(0x100, 0x04);
    ram.write(0x400, 0x91);
    ram.write(0x300, 0x6f);
    let mut cpu = Spc700 {
        pc: 0x200,
        psw: 0x20,
        ..Default::default()
    };
    assert_eq!(cpu.step(&mut ram), 6);
    assert_eq!(cpu.a, 0x91);
    assert_eq!(cpu.step(&mut ram), 8);
    assert_eq!(cpu.pc, 0x300);
    assert_eq!(cpu.step(&mut ram), 5);
    assert_eq!(cpu.pc, 0x205);
    assert_eq!(cpu.sp, 0xef);
}

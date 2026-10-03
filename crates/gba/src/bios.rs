//! Original high-level implementations of documented BIOS services. No Nintendo
//! bytes/tables or other emulator code. Functional, not firmware-cycle accurate.
use crate::{
    bus::Bus,
    cpu::{Cpu, Memory},
};

/// Our tiny ARM IRQ dispatcher executes on the emulated CPU, so handlers may be
/// ARM/Thumb, run for arbitrary time, use peripherals and return normally.
pub(crate) fn irq_firmware() -> Vec<u8> {
    let mut bytes = vec![0; 0x4000];
    bytes[0x18..0x1c].copy_from_slice(&0xea000038u32.to_le_bytes()); // B 100h
    let words: [u32; 9] = [
        0xe92d500f, // PUSH {r0-r3,r12,lr}
        0xe3a00403, // MOV r0,#03000000h
        0xe2800c7f, // ADD r0,r0,#7f00h
        0xe28000fc, // ADD r0,r0,#fch
        0xe5901000, // LDR r1,[r0] ; user IRQ at 03007ffch
        0xe1a0e00f, // MOV lr,pc ; handler return at 11ch
        0xe12fff11, // BX r1
        0xe8bd500f, // POP {r0-r3,r12,lr}
        0xe25ef004, // SUBS pc,lr,#4 ; restore caller CPSR
    ];
    for (n, op) in words.into_iter().enumerate() {
        bytes[0x100 + n * 4..0x104 + n * 4].copy_from_slice(&op.to_le_bytes());
    }
    bytes
}

pub(crate) enum Effect {
    Return,
    Halt,
    Wait { mask: u16, discard: bool },
    Reset,
}

pub(crate) fn service(number: u8, cpu: &mut Cpu, bus: &mut Bus) -> Result<Effect, &'static str> {
    bus.internal(8); // Explicit approximation; not Nintendo routine timing.
    match number {
        0 => {
            let target = if bus.peek8(0x03007ffa) == 0 {
                0x08000000
            } else {
                0x02000000
            };
            bus.iwram[0x7e00..].fill(0);
            let instructions = cpu.instructions;
            let exceptions = cpu.exceptions;
            *cpu = Cpu::default();
            cpu.set_cpsr(0xd2).unwrap();
            cpu.r[13] = 0x03007fa0;
            cpu.set_cpsr(0xd3).unwrap();
            cpu.r[13] = 0x03007fe0;
            cpu.set_cpsr(0x1f).unwrap();
            cpu.r[13] = 0x03007f00;
            cpu.r[14] = target;
            cpu.r[15] = target;
            cpu.instructions = instructions;
            cpu.exceptions = exceptions;
            return Ok(Effect::Reset);
        }
        1 => reset_memory(bus, cpu.r[0] as u8),
        2 => return Ok(Effect::Halt),
        4 => {
            return Ok(Effect::Wait {
                mask: cpu.r[1] as u16,
                discard: cpu.r[0] != 0,
            })
        }
        5 => {
            cpu.r[0] = 1;
            cpu.r[1] = 1;
            return Ok(Effect::Wait {
                mask: 1,
                discard: true,
            });
        }
        6 | 7 => {
            let (a, b) = if number == 6 {
                (cpu.r[0] as i32, cpu.r[1] as i32)
            } else {
                (cpu.r[1] as i32, cpu.r[0] as i32)
            };
            if b == 0 {
                return Err("BIOS division by zero would hang");
            }
            let quotient = a.wrapping_div(b);
            cpu.r[0] = quotient as u32;
            cpu.r[1] = a.wrapping_rem(b) as u32;
            cpu.r[3] = quotient.unsigned_abs();
            bus.internal(64);
        }
        8 => {
            cpu.r[0] = integer_sqrt(cpu.r[0]);
            bus.internal(32);
        }
        0xb | 0xc => copy(bus, cpu.r[0], cpu.r[1], cpu.r[2], number == 0xc),
        0x11 | 0x12 | 0x14 | 0x15 => decompress(bus, cpu.r[0], cpu.r[1], number)?,
        0x19 => bus.write16(0x04000088, if cpu.r[0] == 0 { 0 } else { 0x200 }, false),
        _ => return Err("BIOS service not implemented yet"),
    }
    Ok(Effect::Return)
}
fn reset_memory(bus: &mut Bus, flags: u8) {
    let mut clocks = 0;
    for (bit, data, cost) in [
        (0, &mut bus.ewram, 6),
        (1, &mut bus.iwram, 1),
        (2, &mut bus.palette, 2),
        (3, &mut bus.vram, 2),
        (4, &mut bus.oam, 1),
    ] {
        if flags >> bit & 1 != 0 {
            let length = if bit == 1 { 0x7e00 } else { data.len() };
            data[..length].fill(0);
            clocks += (length / 4 * cost) as u32;
        }
    }
    bus.internal(clocks);
    // Reset separate I/O groups without changing read-only keypad/video state.
    bus.set_io(0x120, 0); // Documented SIO low-halfword reset regardless of flags.
    if flags & 0x20 != 0 {
        bus.io[0x120..0x130].fill(0);
        bus.io[0x134..0x15c].fill(0);
        bus.set_io(0x134, 0x8000);
    }
    if flags & 0x40 != 0 {
        bus.apu.fifo = [Default::default(); 2];
        bus.io[0x60..0x88].fill(0);
        bus.io[0x90..0xb0].fill(0);
    }
    if flags & 0x80 != 0 {
        let video_flags = bus.io16(4) & 7;
        let line = bus.io16(6);
        bus.io[0..0x60].fill(0);
        bus.set_io(4, video_flags);
        bus.set_io(6, line);
        bus.io[0xb0..0x120].fill(0);
        bus.set_io(0x200, 0);
        bus.set_io(0x202, 0);
        bus.set_io(0x208, 0);
        bus.timers = [Default::default(); 4];
        bus.dma = [Default::default(); 4];
        for address in [0x20, 0x26, 0x30, 0x36] {
            bus.set_io(address, 0x100);
        }
    }
    bus.set_io(0, 0x80);
}
fn integer_sqrt(value: u32) -> u32 {
    let mut remainder = u64::from(value);
    let mut root = 0;
    let mut bit = 1u64 << 30;
    while bit != 0 {
        if remainder >= root + bit {
            remainder -= root + bit;
            root = (root >> 1) + bit;
        } else {
            root >>= 1;
        }
        bit >>= 2;
    }
    root as u32
}
fn copy(bus: &mut Bus, source: u32, destination: u32, control: u32, fast: bool) {
    let width = if fast || control & 0x4000000 != 0 {
        4
    } else {
        2
    };
    let mut count = control & 0x1fffff;
    if fast {
        count = (count + 7) & !7;
    }
    let (mut source, mut destination) = (source & !(width - 1), destination & !(width - 1));
    if source < 0x02000000 || source.wrapping_add(count * width) < 0x02000000 || count == 0 {
        return;
    }
    let fill = control & 0x1000000 != 0;
    let mut value = 0;
    for n in 0..count {
        if !fill || n == 0 {
            value = if width == 4 {
                bus.read32(source, n != 0)
            } else {
                u32::from(bus.read16(source, n != 0))
            };
            if !fill {
                source = source.wrapping_add(width);
            }
        }
        if width == 4 {
            bus.write32(destination, value, n != 0);
        } else {
            bus.write16(destination, value as u16, n != 0);
        }
        destination = destination.wrapping_add(width);
    }
}
fn decompress(
    bus: &mut Bus,
    source: u32,
    destination: u32,
    number: u8,
) -> Result<(), &'static str> {
    let header = bus.read32(source, false);
    let length = (header >> 8) as usize;
    let lz = number <= 0x12;
    if header as u8 != if lz { 0x10 } else { 0x30 } {
        return Err("Invalid BIOS decompression header");
    }
    let mut output = Vec::with_capacity(length);
    let mut cursor = source.wrapping_add(4);
    let next = |bus: &mut Bus, cursor: &mut u32| {
        let value = bus.read8(*cursor);
        *cursor = cursor.wrapping_add(1);
        value
    };
    while output.len() < length {
        let flags = next(bus, &mut cursor);
        if lz {
            for bit in (0..8).rev() {
                if output.len() == length {
                    break;
                }
                if flags >> bit & 1 == 0 {
                    output.push(next(bus, &mut cursor));
                } else {
                    let hi = next(bus, &mut cursor);
                    let lo = next(bus, &mut cursor);
                    let distance = ((usize::from(hi) & 15) << 8) + usize::from(lo) + 1;
                    if distance > output.len() {
                        return Err("Invalid BIOS LZ back-reference");
                    }
                    for _ in 0..(usize::from(hi >> 4) + 3).min(length - output.len()) {
                        output.push(output[output.len() - distance]);
                    }
                }
            }
        } else {
            let repeat = flags & 128 != 0;
            let count =
                (usize::from(flags & 127) + if repeat { 3 } else { 1 }).min(length - output.len());
            if repeat {
                let byte = next(bus, &mut cursor);
                output.resize(output.len() + count, byte);
            } else {
                for _ in 0..count {
                    output.push(next(bus, &mut cursor));
                }
            }
        }
    }
    if number == 0x12 || number == 0x15 {
        for (n, bytes) in output.chunks(2).enumerate() {
            bus.write16(
                (destination & !1).wrapping_add(n as u32 * 2),
                u16::from_le_bytes([bytes[0], bytes.get(1).copied().unwrap_or(0)]),
                n != 0,
            );
        }
    } else {
        for (n, byte) in output.into_iter().enumerate() {
            bus.write8(destination.wrapping_add(n as u32), byte);
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    fn bus() -> Bus {
        Bus::new(vec![0; 192], None).unwrap()
    }
    #[test]
    fn sqrt_uses_integer_floor_across_boundaries() {
        for value in [0, 1, 2, 3, 4, 15, 16, 17, 0x7fffffff, u32::MAX] {
            let root = u64::from(integer_sqrt(value));
            assert!(root * root <= u64::from(value));
            assert!((root + 1) * (root + 1) > u64::from(value));
        }
        for root in 1..65536u32 {
            assert_eq!(integer_sqrt(root * root), root);
            assert_eq!(integer_sqrt(root * root - 1), root - 1);
        }
    }
    #[test]
    fn division_signs_overflow_and_zero() {
        let mut b = bus();
        let mut c = Cpu::default();
        for (a, d, q, r) in [
            (-1234, 10, -123, -4),
            (1234, -10, -123, 4),
            (i32::MIN, -1, i32::MIN, 0),
        ] {
            c.r[0] = a as u32;
            c.r[1] = d as u32;
            service(6, &mut c, &mut b).unwrap();
            assert_eq!(c.r[0], q as u32);
            assert_eq!(c.r[1], r as u32);
            assert_eq!(c.r[3], q.unsigned_abs());
            c.r[0] = d as u32;
            c.r[1] = a as u32;
            service(7, &mut c, &mut b).unwrap();
            assert_eq!(c.r[0], q as u32);
        }
        c.r[1] = 0;
        assert!(service(6, &mut c, &mut b).is_err());
    }
    #[test]
    fn reset_preserves_bios_stacks_and_unselected_memory() {
        let mut b = bus();
        b.ewram.fill(0xaa);
        b.iwram.fill(0xbb);
        b.vram.fill(0xcc);
        reset_memory(&mut b, 2);
        assert!(b.iwram[..0x7e00].iter().all(|b| *b == 0));
        assert!(b.iwram[0x7e00..].iter().all(|b| *b == 0xbb));
        assert_eq!(b.ewram[0], 0xaa);
        assert_eq!(b.vram[0], 0xcc);
        assert_eq!(b.io16(0), 0x80);
        b.set_io(6, 100);
        b.set_io(4, 2);
        reset_memory(&mut b, 0x80);
        assert_eq!(b.io16(6), 100);
        assert_eq!(b.io16(4) & 7, 2);
    }
    #[test]
    fn copies_fill_rounding_and_bios_protection() {
        let mut b = bus();
        b.write32(0x03000000, 0x1234abcd, false);
        copy(&mut b, 0x03000000, 0x02000000, 0x01000003, false);
        assert_eq!(b.peek32(0x02000000), 0xabcdabcd);
        assert_eq!(b.peek16(0x02000004), 0xabcd);
        assert_eq!(b.peek16(0x02000006), 0);
        copy(&mut b, 0x03000000, 0x02000020, 0x01000001, true);
        for i in 0..8 {
            assert_eq!(b.peek32(0x02000020 + i * 4), 0x1234abcd);
        }
        assert_eq!(b.peek32(0x02000040), 0);
        copy(&mut b, 0, 0x02000000, 0x04000001, false);
        assert_eq!(b.peek16(0x02000000), 0xabcd);
    }
    #[test]
    fn lz_and_rl_decode_original_streams() {
        let mut b = bus();
        // Literal ABC followed by a six-byte overlapping back-reference.
        let lz = [0x10, 9, 0, 0, 0x10, b'A', b'B', b'C', 0x30, 2];
        b.ewram[..lz.len()].copy_from_slice(&lz);
        for number in [0x11, 0x12] {
            decompress(&mut b, 0x02000000, 0x03000000, number).unwrap();
            assert_eq!(&b.iwram[..9], b"ABCABCABC");
        }
        let rl = [0x30, 7, 0, 0, 0x81, b'A', 2, b'B', b'C', b'D'];
        b.ewram[..rl.len()].copy_from_slice(&rl);
        for number in [0x14, 0x15] {
            decompress(&mut b, 0x02000000, 0x03000000, number).unwrap();
            assert_eq!(&b.iwram[..7], b"AAAABCD");
        }
    }
    #[test]
    fn malformed_decompression_does_not_write_destination() {
        let mut b = bus();
        b.ewram[..7].copy_from_slice(&[0x10, 3, 0, 0, 0x80, 0, 0]);
        b.iwram[0] = 0xee;
        assert!(decompress(&mut b, 0x02000000, 0x03000000, 0x11).is_err());
        assert_eq!(b.iwram[0], 0xee);
    }
    #[test]
    fn soft_reset_selects_ram_and_reinitializes_stacks() {
        let mut b = bus();
        let mut c = Cpu::default();
        b.iwram[0x7ffa] = 1;
        c.r.fill(0xdeadbeef);
        assert!(matches!(service(0, &mut c, &mut b), Ok(Effect::Reset)));
        assert_eq!(c.r[15], 0x02000000);
        assert_eq!(c.r[13], 0x03007f00);
        assert_eq!(c.cpsr(), 0x1f);
        assert!(b.iwram[0x7e00..].iter().all(|b| *b == 0));
        assert!(c.r[..13].iter().all(|r| *r == 0));
    }
}

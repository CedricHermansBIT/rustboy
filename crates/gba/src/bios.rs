//! Original high-level implementations of documented BIOS services. No Nintendo
//! bytes/tables or other emulator code. Functional, not firmware-cycle accurate.
use crate::{
    bus::Bus,
    cpu::{Cpu, Memory},
};
#[path = "bios_data.rs"]
mod data;
#[path = "bios_math.rs"]
mod math;
#[path = "bios_sound.rs"]
pub(crate) mod sound;

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
    sound::firmware(&mut bytes);
    let multiboot = include_bytes!("../firmware/multiboot.bin");
    bytes[0x1800..0x1800 + multiboot.len()].copy_from_slice(multiboot);
    for (index, opcode) in [
        0xe59dc008u32,
        0xe128f00c,
        0xe49de004,
        0xe49dc00c,
        0xe12fff1c,
    ]
    .into_iter()
    .enumerate()
    {
        bytes[0x1700 + index * 4..0x1704 + index * 4].copy_from_slice(&opcode.to_le_bytes());
    }
    bytes
}

pub(crate) enum Effect {
    Return,
    Halt,
    Stop,
    Wait { mask: u16, discard: bool },
    Reset,
    HardReset,
    SoundMain(u32),
    SoundReturn,
    FirmwareCall(u32),
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
            #[cfg(all(feature = "jit", target_arch = "wasm32"))]
            {
                bus.jit_memory_version = bus.jit_memory_version.wrapping_add(1);
            }
            #[cfg(all(feature = "jit", target_arch = "wasm32"))]
            for version in &mut bus.jit_iwram_versions[126..] {
                *version = version.wrapping_add(1);
            }
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
        3 => return Ok(Effect::Stop),
        0x27 => {
            return Ok(if cpu.r[2] & 0x80 != 0 {
                Effect::Stop
            } else {
                Effect::Halt
            })
        }
        // This restarts the entire machine, rather than SoftReset's chosen RAM
        // or cartridge entry point. The HLE BIOS omits the retail boot animation.
        0x26 => return Ok(Effect::HardReset),
        0x25 => {
            // The original routine below executes on the guest CPU and uses
            // ordinary serial registers, so network latency cannot collapse a
            // BIOS upload into one atomic host-side operation.
            let p = cpu.r[0];
            if !multiboot_parameters_valid(bus, p, cpu.r[1]) {
                cpu.r[0] = 1;
            } else {
                return Ok(Effect::FirmwareCall(0x1800));
            }
        }
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
                // Real GBA's restoring divider returns for the three degenerate
                // small numerators: hardware measurements and mGBA's independent
                // fixture agree. Larger magnitudes get stuck aligning a zero
                // divisor; retain the explicit fault instead of hanging the host.
                if a.unsigned_abs() > 1 {
                    return Err("BIOS division by zero would hang");
                }
                cpu.r[0] = if a < 0 { u32::MAX } else { 1 };
                cpu.r[1] = a as u32;
                cpu.r[3] = 1;
                bus.internal(64);
                return Ok(Effect::Return);
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
        9 => {
            let (angle, square, polynomial) = math::bios_arctan(cpu.r[0] as i32);
            cpu.r[0] = angle as u32;
            cpu.r[1] = square as u32;
            cpu.r[3] = polynomial as u32;
            bus.internal(64); // Functional approximation, not firmware timing.
        }
        0xa => {
            let (angle, scratch) = math::bios_arctan2(cpu.r[0] as i32, cpu.r[1] as i32);
            cpu.r[0] = u32::from(angle);
            cpu.r[1] = scratch as u32;
            cpu.r[3] = 0x170; // independently measured caller-visible leftover
            bus.internal(64);
        }
        0xb | 0xc => copy(bus, cpu.r[0], cpu.r[1], cpu.r[2], number == 0xc),
        0xd => {
            // Documented retail GBA checksum: a service result, not firmware
            // bytes. Summing our replacement IRQ stub would identify the HLE
            // implementation rather than the emulated GBA hardware.
            cpu.r[0] = 0xbaae187f;
            cpu.r[1] = 1;
            cpu.r[3] = 0x4000;
            bus.internal(40960); // Functional approximation, not exact timing.
        }
        0xe | 0xf => affine_set(bus, cpu.r[0], cpu.r[1], cpu.r[2], cpu.r[3], number == 0xe)?,
        0x10 => data::unpack(bus, cpu.r[0], cpu.r[1], cpu.r[2])?,
        0x13 => data::huffman(bus, cpu.r[0], cpu.r[1])?,
        0x11 | 0x12 | 0x14 | 0x15 => decompress(bus, cpu.r[0], cpu.r[1], number)?,
        0x16..=0x18 => data::differential(bus, cpu.r[0], cpu.r[1], number)?,
        0x19 => {
            // SoundBias adjusts the DAC bias field and preserves PWM resolution.
            // Exact intermediate analog ramp samples are not generated by HLE.
            let value = bus.io16(0x88);
            let target = if cpu.r[0] == 0 { 0 } else { 0x200 };
            let current = value & 0x3ff;
            let level = if cpu.r[0] == 0 {
                0
            } else {
                current.max(target)
            };
            bus.write16(0x04000088, (value & !0x3ff) | level, false);
            cpu.r[1] = u32::from(level);
            cpu.r[3] = 0x04000088;
            // Measured full ramps: 15688 down / 15942 up; no ramp: 70.
            // Linear cost between endpoints remains an explicit approximation.
            let steps = current.abs_diff(level) / 2;
            bus.internal(62 + u32::from(steps) * if target == 0 { 61 } else { 62 });
        }
        0x1a..=0x1e | 0x20..=0x24 | 0x28..=0x2a => {
            return sound::service(number, cpu, bus);
        }
        0x1f => {
            let address = cpu.r[0].wrapping_add(4);
            let base = bus
                .read32(address & !3, false)
                .rotate_right((address & 3) * 8);
            cpu.r[0] = math::midi_frequency(base, cpu.r[1], cpu.r[2]);
            bus.internal(128); // HLE sound math, not firmware instruction timing.
        }
        _ => return Err("BIOS service not implemented yet"),
    }
    Ok(Effect::Return)
}
/// Validate the supported upload ABI before entering executable firmware. Bad
/// addresses must not turn into an open-bus upload or wrap across a ROM mirror.
fn multiboot_parameters_valid(bus: &Bus, p: u32, mode: u32) -> bool {
    if mode > 2 || p & 3 != 0 {
        return false;
    }
    let parameter_end = match p >> 24 {
        2 => 0x02040000,
        3 => 0x03008000,
        _ => return false,
    };
    if p > parameter_end - 0x4c || bus.peek8(p + 0x1e) != 2 {
        return false;
    }
    let source = bus.peek32(p + 0x20);
    let end = bus.peek32(p + 0x24);
    let Some(length) = end.checked_sub(source) else {
        return false;
    };
    if source & 3 != 0 || !(0x100..=0x3ff40).contains(&length) || length & 15 != 0 {
        return false;
    }
    match source {
        0x02000000..=0x0203ffff => end <= 0x02040000,
        0x03000000..=0x03007fff => end <= 0x03008000,
        0x08000000..=0x0dffffff => end <= 0x0e000000 && source >> 25 == (end - 1) >> 25,
        _ => false,
    }
}

/// Independently generated mathematical sine values, not firmware table bytes.
/// Quantized 8-bit angles / 14-bit sine; exact firmware rounding is not claimed.
fn affine_matrix(x: i16, y: i16, angle: u16) -> [i16; 4] {
    static SINE: std::sync::OnceLock<[i16; 256]> = std::sync::OnceLock::new();
    let sine = SINE.get_or_init(|| {
        std::array::from_fn(|i| {
            ((i as f64 * std::f64::consts::TAU / 256.).sin() * 16384.).round() as i16
        })
    });
    let index = usize::from(angle >> 8);
    let (sin, cos) = (i32::from(sine[index]), i32::from(sine[(index + 64) & 255]));
    [
        ((i32::from(x) * cos) >> 14) as i16,
        (-((i32::from(x) * sin) >> 14)) as i16,
        ((i32::from(y) * sin) >> 14) as i16,
        ((i32::from(y) * cos) >> 14) as i16,
    ]
}
fn affine_set(
    bus: &mut Bus,
    mut source: u32,
    mut dest: u32,
    count: u32,
    stride: u32,
    bg: bool,
) -> Result<(), &'static str> {
    if count > 65536 {
        return Err("BIOS affine calculation count exceeds bounded hardware storage");
    }
    for _ in 0..count {
        let offset = if bg { 12 } else { 0 };
        let x = bus.read16(source.wrapping_add(offset), false) as i16;
        let y = bus.read16(source.wrapping_add(offset + 2), false) as i16;
        let angle = bus.read16(source.wrapping_add(offset + 4), false);
        let matrix = affine_matrix(x, y, angle);
        let spacing = if bg { 2 } else { stride };
        for value in matrix {
            bus.write16(dest, value as u16, false);
            dest = dest.wrapping_add(spacing);
        }
        if bg {
            let origin_x = bus.read32(source, false);
            let origin_y = bus.read32(source.wrapping_add(4), false);
            let screen_x = bus.read16(source.wrapping_add(8), false) as i16 as i32;
            let screen_y = bus.read16(source.wrapping_add(10), false) as i16 as i32;
            for (origin, a, b) in [
                (origin_x, matrix[0], matrix[1]),
                (origin_y, matrix[2], matrix[3]),
            ] {
                let start = origin
                    .wrapping_sub(i32::from(a).wrapping_mul(screen_x) as u32)
                    .wrapping_sub(i32::from(b).wrapping_mul(screen_y) as u32);
                bus.write32(dest, start, false);
                dest = dest.wrapping_add(4);
            }
        }
        source = source.wrapping_add(if bg { 20 } else { 8 });
        bus.internal(32);
    }
    Ok(())
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
    bus.serial.reset_data_low(); // Documented reset regardless of flags.
    if flags & 0x20 != 0 {
        bus.io[0x120..0x130].fill(0);
        bus.io[0x134..0x15c].fill(0);
        bus.serial.reset_registers();
    }
    bus.serial.store_registers(&mut bus.io);
    if flags & 0x40 != 0 {
        bus.apu.psg = Default::default();
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
        bus.refresh_timer_schedule();
        bus.dma = [Default::default(); 4];
        bus.refresh_dma_schedule();
        for address in [0x20, 0x26, 0x30, 0x36] {
            bus.set_io(address, 0x100);
        }
        bus.reload_affine();
    }
    bus.set_io(0, 0x80);
    bus.refresh_fifo_requests();
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
    // Retain logical byte lanes for the 8-bit save bus. Normal RAM is aligned
    // by Bus, while CpuSet's halfword source uses ARM LDRH's odd-address rotation.
    // The word paths use aligned word transfers, not single-LDR rotation.
    let (mut source, mut destination) = (source, destination);
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
                u32::from(bus.read16(source, n != 0)).rotate_right((source & 1) * 8)
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
    // The BIOS rejects protected/unused source regions before examining the
    // stream. Games can pass a null asset pointer and expect a harmless return.
    // Reading our replacement firmware as a compression header instead faults.
    if source < 0x02000000 {
        return Ok(());
    }
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
    fn multiboot_params(b: &mut Bus, source: u32, end: u32) {
        b.iwram[0x11e] = 2;
        b.iwram[0x120..0x124].copy_from_slice(&source.to_le_bytes());
        b.iwram[0x124..0x128].copy_from_slice(&end.to_le_bytes());
    }
    #[test]
    fn multiboot_accepts_supported_modes_and_contiguous_ram_rom_bounds() {
        let mut b = bus();
        let mut cpu = Cpu::default();
        for mode in 0..=2 {
            for (source, end) in [
                (0x020000c0, 0x020001c0),
                (0x020000c0, 0x02040000),
                (0x03000000, 0x03008000),
                (0x080000c0, 0x080001c0),
                (0x09ffff00, 0x0a000000),
                (0x0a0000c0, 0x0a0001c0),
                (0x0dffff00, 0x0e000000),
            ] {
                multiboot_params(&mut b, source, end);
                cpu.r[0] = 0x03000100;
                cpu.r[1] = mode;
                assert!(
                    matches!(
                        service(0x25, &mut cpu, &mut b).unwrap(),
                        Effect::FirmwareCall(0x1800)
                    ),
                    "mode {mode}, source {source:08x}, end {end:08x}"
                );
                assert_eq!(cpu.r[0], 0x03000100);
            }
        }
    }
    #[test]
    fn malformed_multiboot_parameters_fail_before_starting_serial_or_firmware() {
        let mut b = bus();
        b.serial.attach(0).unwrap();
        b.write16(0x04000128, 0x2003, false);
        b.take_cycles();
        let serial = b.serial.encode();
        let mut cpu = Cpu::default();
        for (source, end) in [
            (0x080000c0, 0x080001b0), // Too short.
            (0x080000c0, 0x08040010), // Too long.
            (0x080000c0, 0x080001c4), // Not a multiple of 16.
            (0x080000c2, 0x080001c2), // Unaligned word source.
            (0x080001c0, 0x080000c0), // Reversed range.
            (0x080000c0, 0x080000c0), // Empty range.
            (0xffffff00, 0),          // Wrapped range.
            (0x0203ff10, 0x02040010), // EWRAM mirror boundary.
            (0x02040000, 0x02040100), // Noncanonical EWRAM.
            (0x03007f10, 0x03008010), // IWRAM mirror boundary.
            (0x09ffff10, 0x0a000010), // Physical ROM window boundary.
            (0x0dffff10, 0x0e000010), // ROM to backup bus.
            (0, 0x100),               // BIOS source.
            (0x04000000, 0x04000100), // Peripheral source.
            (0x06000000, 0x06000100), // Video source outside supported ABI.
        ] {
            multiboot_params(&mut b, source, end);
            cpu.r[0] = 0x03000100;
            cpu.r[1] = 1;
            assert!(matches!(
                service(0x25, &mut cpu, &mut b).unwrap(),
                Effect::Return
            ));
            assert_eq!(cpu.r[0], 1, "source {source:08x}, end {end:08x}");
            assert_eq!(b.serial.encode(), serial);
            assert!(b.serial.request().is_none());
        }
        multiboot_params(&mut b, 0x080000c0, 0x080001c0);
        for p in [
            0, 0x02000002, 0x0203ffb8, 0x02040000, 0x03007fb8, 0x03008000,
        ] {
            cpu.r[0] = p;
            cpu.r[1] = 1;
            assert!(matches!(
                service(0x25, &mut cpu, &mut b).unwrap(),
                Effect::Return
            ));
            assert_eq!(cpu.r[0], 1);
            assert_eq!(b.serial.encode(), serial);
        }
        for (mode, child) in [(3, 2), (u32::MAX, 2), (1, 0), (1, 4), (1, 6)] {
            cpu.r[0] = 0x03000100;
            cpu.r[1] = mode;
            b.iwram[0x11e] = child;
            assert!(matches!(
                service(0x25, &mut cpu, &mut b).unwrap(),
                Effect::Return
            ));
            assert_eq!(cpu.r[0], 1);
            assert_eq!(b.serial.encode(), serial);
        }
    }
    #[test]
    fn sound_bias_preserves_pwm_and_documented_caller_registers() {
        let mut b = bus();
        let mut cpu = Cpu::default();
        cpu.r[0] = 1;
        cpu.r[2] = 0x12345678;
        b.write16(0x04000088, 0xc040, false);
        service(0x19, &mut cpu, &mut b).unwrap();
        assert_eq!(b.io16(0x88), 0xc200);
        assert_eq!(cpu.r[0], 1);
        assert_eq!(cpu.r[1], 0x200);
        assert_eq!(cpu.r[2], 0x12345678);
        assert_eq!(cpu.r[3], 0x04000088);
        cpu.r[0] = 0;
        service(0x19, &mut cpu, &mut b).unwrap();
        assert_eq!(b.io16(0x88), 0xc000);
        assert_eq!(cpu.r[1], 0);
        b.write16(0x04000088, 0xc300, false);
        cpu.r[0] = 1;
        service(0x19, &mut cpu, &mut b).unwrap();
        assert_eq!(
            b.io16(0x88),
            0xc300,
            "already above target remains unchanged"
        );
    }
    #[test]
    fn cpu_set_halfword_odd_source_rotates_and_preserves_save_bus_byte_lanes() {
        let mut b = bus();
        b.write32(0x02000000, 0x1234abcd, false);
        copy(&mut b, 0x02000001, 0x03000000, 2, false);
        assert_eq!(b.peek32(0x03000000), 0x001200ab);
        // Unlike CpuSet's halfwords, word copies use aligned RAM words.
        copy(&mut b, 0x02000001, 0x03000005, 0x04000001, false);
        assert_eq!(b.peek32(0x03000004), 0x1234abcd);

        b.backup = crate::backup::Backup::detect(b"SRAM_V000");
        b.backup.data[..4].copy_from_slice(&[0x47, 0x61, 0x6d, 0x65]);
        copy(&mut b, 0x0e000001, 0x03000010, 2, false);
        assert_eq!(b.peek32(0x03000010), 0x00650061);
        copy(&mut b, 0x0e000002, 0x03000020, 0x04000001, false);
        assert_eq!(b.peek32(0x03000020), 0x6d6d6d6d);
        copy(&mut b, 0x0e000003, 0x03000030, 1, true);
        assert_eq!(b.peek32(0x03000030), 0x65656565);
        b.backup.data.fill(0x66);
        copy(&mut b, 0x02000000, 0x0e000001, 0x04000001, false);
        assert_eq!(&b.backup.data[..4], &[0x66, 0xab, 0x66, 0x66]);
        b.backup.data.fill(0x66);
        copy(&mut b, 0x02000000, 0x0e000001, 1, false);
        assert_eq!(&b.backup.data[..4], &[0x66, 0xab, 0x66, 0x66]);
    }
    #[test]
    fn affine_cardinal_rotations_signed_scaling_and_fractional_angle() {
        for (angle, expected) in [
            (0, [256, 0, 0, 512]),
            (0x4000, [0, -256, 512, 0]),
            (0x8000, [-256, 0, 0, -512]),
            (0xc000, [0, 256, -512, 0]),
        ] {
            assert_eq!(affine_matrix(256, 512, angle), expected);
            assert_eq!(affine_matrix(256, 512, angle | 255), expected);
        }
        assert_eq!(affine_matrix(-256, 256, 0), [-256, 0, 0, 256]);
        for angle in 0..256u16 {
            let matrix = affine_matrix(256, 256, angle << 8);
            assert_eq!(matrix[0], matrix[3]);
            assert!((i32::from(matrix[1]) + i32::from(matrix[2])).abs() <= 1);
            let norm = i32::from(matrix[0]).pow(2) + i32::from(matrix[2]).pow(2);
            assert!((norm - 65536).abs() < 750);
        }
    }
    #[test]
    fn object_affine_array_stride_and_padding_preserve_neighbors() {
        let mut b = bus();
        b.oam.fill(0x5a);
        for (n, angle) in [0, 0x4000].into_iter().enumerate() {
            let source = 0x02000000 + n as u32 * 8;
            b.write16(source, 256, false);
            b.write16(source + 2, 512, false);
            b.write16(source + 4, angle, false);
            b.write16(source + 6, 0xffff, false);
        }
        affine_set(&mut b, 0x02000000, 0x07000006, 2, 8, false).unwrap();
        for (n, value) in [256, 0, 0, 512, 0, -256, 512, 0].into_iter().enumerate() {
            assert_eq!(b.peek16(0x07000006 + n as u32 * 8), value as u16);
            assert_eq!(b.peek16(0x07000000 + n as u32 * 8), 0x5a5a);
        }
        let saved = b.oam.clone();
        assert!(affine_set(&mut b, 0, 0x07000000, 65537, 8, false).is_err());
        assert_eq!(b.oam, saved);
    }
    #[test]
    fn background_affine_array_centers_and_twenty_byte_source_stride() {
        let mut b = bus();
        for (n, angle) in [0, 0x4000].into_iter().enumerate() {
            let source = 0x02000000 + n as u32 * 20;
            b.write32(source, 100 << 8, false);
            b.write32(source + 4, 200 << 8, false);
            b.write16(source + 8, 10, false);
            b.write16(source + 10, 20, false);
            b.write16(source + 12, 256, false);
            b.write16(source + 14, 512, false);
            b.write16(source + 16, angle, false);
            b.write16(source + 18, 0xffff, false);
        }
        affine_set(&mut b, 0x02000000, 0x03000000, 2, 0, true).unwrap();
        assert_eq!(b.peek32(0x03000008), 90 << 8);
        assert_eq!(b.peek32(0x0300000c), 160 << 8);
        assert_eq!(b.peek32(0x03000018), 120 << 8);
        assert_eq!(b.peek32(0x0300001c), 180 << 8);
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
    fn division_zero_hardware_boundary_and_math_scratch_registers() {
        let mut b = bus();
        let mut c = Cpu::default();
        for number in [6, 7] {
            for numerator in [-1i32, 0, 1] {
                c.r[0] = if number == 6 { numerator as u32 } else { 0 };
                c.r[1] = if number == 6 { 0 } else { numerator as u32 };
                c.r[2] = 0x11223344;
                service(number, &mut c, &mut b).unwrap();
                assert_eq!(c.r[0], if numerator < 0 { u32::MAX } else { 1 });
                assert_eq!(c.r[1], numerator as u32);
                assert_eq!(c.r[2], 0x11223344);
                assert_eq!(c.r[3], 1);
            }
            for numerator in [-2i32, 2, i32::MIN, i32::MAX] {
                c.r[0] = if number == 6 { numerator as u32 } else { 0 };
                c.r[1] = if number == 6 { 0 } else { numerator as u32 };
                assert!(service(number, &mut c, &mut b).is_err());
            }
        }
        c.r[0] = 0x1000;
        c.r[2] = 0x11223344;
        service(9, &mut c, &mut b).unwrap();
        assert_eq!(&c.r[..4], &[0x9fb, 0xfffffc00, 0x11223344, 0x9fb3]);
        c.r[0] = 1;
        c.r[1] = 0;
        service(0xa, &mut c, &mut b).unwrap();
        assert_eq!(&c.r[..4], &[0, 0, 0x11223344, 0x170]);
        c.r[0] = 1;
        c.r[1] = 1;
        service(0xa, &mut c, &mut b).unwrap();
        assert_eq!(&c.r[..4], &[0x2000, 0xffffc000, 0x11223344, 0x170]);
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
    fn protected_decompression_sources_return_without_reading_or_writing() {
        let mut b = bus();
        b.iwram[..32].fill(0xee);
        b.vram[..32].fill(0xdd);
        b.bios_latch = 0x12345678;
        for number in [0x11, 0x12, 0x14, 0x15] {
            for source in [0, 0x100, 0x3ffc, 0x01000000, 0x01fffffc] {
                let mut c = Cpu::default();
                c.r[0] = source;
                c.r[1] = if number & 1 == 0 {
                    0x06000000
                } else {
                    0x03000000
                };
                assert!(matches!(
                    service(number, &mut c, &mut b),
                    Ok(Effect::Return)
                ));
                assert_eq!(&b.iwram[..32], &[0xee; 32]);
                assert_eq!(&b.vram[..32], &[0xdd; 32]);
                assert_eq!(b.bios_latch, 0x12345678);
            }
        }
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
    #[test]
    fn midi_reads_wave_frequency_through_bus_with_arm_word_rotation() {
        let mut b = bus();
        for wave in [0x02000000, 0x03000000] {
            b.write32(wave, 0xdeadbeef, false);
            b.write32(wave + 4, 0x12345678, false);
            for offset in 0..4 {
                let mut c = Cpu::default();
                c.r[0] = wave + offset;
                c.r[1] = 168;
                c.r[2] = 0;
                c.r[4] = 0xcafebabe;
                let flags = c.cpsr();
                b.take_cycles();
                assert!(matches!(service(0x1f, &mut c, &mut b), Ok(Effect::Return)));
                assert_eq!(c.r[0], 0x12345678u32.rotate_right(offset * 8) >> 1);
                assert!(b.take_cycles() > 128, "wave read must pay bus wait states");
                assert_eq!(c.r[4], 0xcafebabe);
                assert_eq!(c.cpsr(), flags);
            }
        }
    }
    #[test]
    fn checksum_identifies_gba_and_preserves_non_scratch_registers() {
        let mut b = bus();
        let mut c = Cpu::default();
        c.r.fill(0x12345678);
        let flags = c.cpsr();
        assert!(matches!(service(0x0d, &mut c, &mut b), Ok(Effect::Return)));
        assert_eq!(c.r[0], 0xbaae187f);
        assert_eq!(c.r[1], 1);
        assert_eq!(c.r[3], 0x4000);
        assert_eq!(c.r[2], 0x12345678);
        assert!(c.r[4..].iter().all(|r| *r == 0x12345678));
        assert_eq!(c.cpsr(), flags);
    }
}

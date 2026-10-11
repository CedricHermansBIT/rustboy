//! Original exhaustive code-generation fixtures, consumed by jit_verify.mjs.
#[cfg(feature = "jit")]
fn main() {
    use rustboy_gba::{
        cpu::{Cpu, Memory, T},
        jit,
    };
    use std::io::Write;
    #[derive(Default)]
    struct Bus {
        op: u16,
        arm: u32,
        program: Vec<(u32, u32)>,
        events: Vec<String>,
    }
    impl Memory for Bus {
        fn instruction_context(&mut self, pc: u32, thumb: bool) {
            self.events.push(format!("[0,{pc},{}]", u32::from(thumb)));
        }
        fn fetch16(&mut self, pc: u32, sequential: bool) -> u16 {
            self.events
                .push(format!("[1,{pc},{}]", u32::from(sequential)));
            self.program
                .iter()
                .find(|&&(address, _)| address == pc)
                .map(|&(_, op)| op as u16)
                .unwrap_or(self.op)
        }
        fn fetch32(&mut self, pc: u32, sequential: bool) -> u32 {
            self.events
                .push(format!("[10,{pc},{}]", u32::from(sequential)));
            self.program
                .iter()
                .find(|&&(address, _)| address == pc)
                .map(|&(_, op)| op)
                .unwrap_or(self.arm)
        }
        fn read8(&mut self, a: u32) -> u8 {
            self.events.push(format!("[2,{a},0]"));
            (a ^ (a >> 8) ^ 0xa5) as u8
        }
        fn read16(&mut self, a: u32, s: bool) -> u16 {
            self.events.push(format!("[3,{a},{}]", u32::from(s)));
            (a ^ (a >> 16) ^ 0xb579) as u16
        }
        fn read32(&mut self, a: u32, s: bool) -> u32 {
            self.events.push(format!("[4,{a},{}]", u32::from(s)));
            a.wrapping_mul(0x10203041) ^ 0x87654321
        }
        fn write8(&mut self, a: u32, v: u8) {
            self.events.push(format!("[5,{a},{v}]"));
        }
        fn write16(&mut self, a: u32, v: u16, s: bool) {
            self.events.push(format!("[6,{a},{v},{}]", u32::from(s)));
        }
        fn write32(&mut self, a: u32, v: u32, s: bool) {
            self.events.push(format!("[7,{a},{v},{}]", u32::from(s)));
        }
        fn internal(&mut self, n: u32) {
            self.events.push(format!("[8,{n},0]"));
        }
        fn refill_complete(&mut self, a: u32) {
            self.events.push(format!("[9,{a},0]"));
        }
    }
    let path = std::env::args().nth(1).expect("JSONL path");
    let mut out = std::io::BufWriter::new(std::fs::File::create(path).unwrap());
    let mut count = 0;
    let values = [
        0, 1, 0x7fffffff, 0x80000000, 0xffffffff, 0xfffffffd, 32, 0x02000003, 255, 256, 0xffffff80,
        33,
    ];
    let mut check = |op: u32, thumb: bool, mode: u32| {
        let pc = 0x08000100;
        let bytes = jit::full::compile(pc, &[op], thumb).unwrap();
        let hex: String = bytes.iter().map(|b| format!("{b:02x}")).collect();
        let mut cpu = Cpu::default();
        cpu.set_cpsr(((op & 15) << 28) | if thumb { T } else { 0 } | mode)
            .unwrap();
        for r in 0..15 {
            cpu.r[r] = values[(r + count) % values.len()];
        }
        cpu.r[15] = pc;
        let initial = jit::verification_state(&cpu);
        let mut bus = Bus {
            op: op as u16,
            arm: op,
            ..Bus::default()
        };
        cpu.step(&mut bus);
        let expected = jit::verification_state(&cpu);
        writeln!(out,"{{\"op\":{op},\"thumb\":{thumb},\"wasm\":\"{hex}\",\"initial\":{initial:?},\"expected\":{expected:?},\"layout\":{:?},\"events\":[{}]}}",jit::verification_layout(),bus.events.join(",")).unwrap();
        count += 1;
    };
    for op in 0..=u16::MAX {
        check(u32::from(op), true, if op & 1 == 0 { 0x1f } else { 0x13 });
    }
    // Every ARM encoding class, condition, register alias and addressing flag,
    // plus deterministic random encodings to exercise reserved/special overlaps.
    for kind in 0..16 {
        for rd in 0..16 {
            for shift in 0..32 {
                for cond in [0, 1, 2, 3, 14, 15] {
                    let rn = (rd + shift) & 15;
                    let rm = (rd + 3) & 15;
                    check(
                        cond << 28
                            | kind << 21
                            | 1 << 20
                            | rn << 16
                            | rd << 12
                            | shift << 7
                            | ((shift & 3) << 5)
                            | rm,
                        false,
                        0x1f,
                    );
                    check(
                        cond << 28
                            | 1 << 25
                            | kind << 21
                            | ((shift & 1) << 20)
                            | rn << 16
                            | rd << 12
                            | (shift << 8)
                            | 0xff,
                        false,
                        0x13,
                    );
                }
            }
        }
    }
    let mut seed = 0x12345678u32;
    for _ in 0..16384 {
        seed ^= seed << 13;
        seed ^= seed >> 17;
        seed ^= seed << 5;
        check(seed, false, if seed & 1 == 0 { 0x1f } else { 0x13 });
    }
    for flags in 0..256 {
        for rd in 0..16 {
            for base in [
                0xe0000090, 0xe0800090, 0xe1000090, 0xe00000b0, 0xe4000000, 0xe8000000,
            ] {
                check(
                    base | (flags << 20)
                        | (rd << 12)
                        | (((rd + 3) & 15) << 16)
                        | (((rd + 1) & 15) << 8)
                        | (rd + 2) & 15,
                    false,
                    0x13,
                );
            }
        }
    }
    let programs = [
        (
            true,
            vec![
                0x2001, 0x2102, 0x6041, 0x6843, 0x3001, 0x4343, 0xc00a, 0xc80a, 0xb406, 0xbc06,
                0x2b00, 0xd1f2,
            ],
        ),
        (
            false,
            vec![
                0xe3a00020, 0xe3a01040, 0xe5801000, 0xe5902000, 0xe8800006, 0xe8b00018, 0xe3a0d030,
                0xe92d000e, 0xe8bd0070, 0xe1b00112, 0xe3500000, 0x1afffff3,
            ],
        ),
        (
            true,
            vec![
                0x2000, 0x2101, 0x3001, 0x004a, 0x1883, 0x414b, 0x41cb, 0x434b, 0x6043,
            ],
        ),
        (true, vec![0x4831, 0x6844, 0x1c64, 0x2c00, 0xd1fa]),
        (true, vec![0x203f, 0x4686, 0xf001, 0xf802]),
        (true, vec![0x2001, 0x3001, 0xdf06]),
        (
            false,
            vec![
                0xe3a000ff, 0xe2901001, 0xe0b12000, 0xe0d23001, 0xe1b04413, 0xe0800192, 0xe5804000,
            ],
        ),
        (
            false,
            vec![0xe3a00001, 0xe1a04000, 0xe5945003, 0xe1b06835, 0xe12fff16],
        ),
        (false, vec![0xe3a00000, 0xe2900001, 0x13500002, 0xef060000]),
    ];
    for (thumb, program) in programs {
        for budget in 1..=32 {
            for mode in [0x1f, 0x13] {
                let pc = 0x08000100;
                let width = if thumb { 2 } else { 4 };
                let bytes = jit::full::compile(pc, &program, thumb).unwrap();
                let hex: String = bytes.iter().map(|b| format!("{b:02x}")).collect();
                let mut cpu = Cpu::default();
                cpu.set_cpsr(0xa0000000 | if thumb { T } else { 0 } | mode)
                    .unwrap();
                for r in 0..15 {
                    cpu.r[r] = values[(r + budget) % values.len()];
                }
                cpu.r[15] = pc;
                let initial = jit::verification_state(&cpu);
                let mut bus = Bus {
                    program: program
                        .iter()
                        .enumerate()
                        .map(|(n, &op)| (pc + n as u32 * width, op))
                        .collect(),
                    ..Bus::default()
                };
                for &op in &program {
                    bus.op = op as u16;
                    bus.arm = op;
                    cpu.step(&mut bus);
                    let cost: u32 = bus
                        .events
                        .iter()
                        .map(|event| {
                            let fields: Vec<u32> = event
                                .trim_matches(['[', ']'])
                                .split(',')
                                .map(|x| x.parse().unwrap())
                                .collect();
                            match fields[0] {
                                0 | 9 => 0,
                                8 => fields[1],
                                _ => 1,
                            }
                        })
                        .sum();
                    if cost >= budget as u32 || jit::full::ends_block(pc, op, thumb) {
                        break;
                    }
                }
                let expected = jit::verification_state(&cpu);
                writeln!(out,"{{\"op\":0,\"thumb\":{thumb},\"budget\":{budget},\"wasm\":\"{hex}\",\"initial\":{initial:?},\"expected\":{expected:?},\"layout\":{:?},\"events\":[{}]}}",jit::verification_layout(),bus.events.join(",")).unwrap();
                count += 1;
            }
        }
    }
    for saved_mode in [0x1f, 0x11, 0x12, 0x13, 0x17, 0x1b] {
        for thumb_flag in [0, T] {
            for flags in 0..16 {
                for op in [0xe1b0f00e, 0xe8fd8003, 0xe150f001, 0xe100f001] {
                    let pc = 0x08000100;
                    let mut cpu = Cpu::default();
                    cpu.set_cpsr(0x13).unwrap();
                    cpu.r[0] = saved_mode | thumb_flag | (flags << 28);
                    cpu.r[15] = pc - 4;
                    cpu.step(&mut Bus {
                        arm: 0xe16ff000,
                        ..Bus::default()
                    });
                    for r in 0..15 {
                        cpu.r[r] = values[(r + flags as usize) % values.len()];
                    }
                    cpu.r[15] = pc;
                    cpu.instructions = 0;
                    cpu.exceptions = 0;
                    let initial = jit::verification_state(&cpu);
                    let bytes = jit::full::compile(pc, &[op], false).unwrap();
                    let hex: String = bytes.iter().map(|b| format!("{b:02x}")).collect();
                    let mut bus = Bus {
                        arm: op,
                        ..Bus::default()
                    };
                    cpu.step(&mut bus);
                    let expected = jit::verification_state(&cpu);
                    writeln!(out,"{{\"op\":{op},\"thumb\":false,\"wasm\":\"{hex}\",\"initial\":{initial:?},\"expected\":{expected:?},\"layout\":{:?},\"events\":[{}]}}",jit::verification_layout(),bus.events.join(",")).unwrap();
                    count += 1;
                }
            }
        }
    }
    // A bus stop can end a fetch span before its requested clock budget.
    for stop in 1..=2 {
        let pc = 0x08000100;
        let program = [0x2001, 0x2102, 0x3001];
        let bytes = jit::full::compile(pc, &program, true).unwrap();
        let hex: String = bytes.iter().map(|b| format!("{b:02x}")).collect();
        let mut cpu = Cpu::default();
        cpu.set_cpsr(T | 0x1f).unwrap();
        cpu.r[15] = pc;
        let initial = jit::verification_state(&cpu);
        let mut bus = Bus {
            program: program
                .iter()
                .enumerate()
                .map(|(n, &op)| (pc + n as u32 * 2, op))
                .collect(),
            ..Bus::default()
        };
        for &op in &program[..stop] {
            bus.op = op as u16;
            cpu.step(&mut bus);
        }
        let expected = jit::verification_state(&cpu);
        writeln!(out,"{{\"op\":0,\"thumb\":true,\"stopSpan\":{stop},\"budget\":31,\"wasm\":\"{hex}\",\"initial\":{initial:?},\"expected\":{expected:?},\"layout\":{:?},\"events\":[{}]}}",jit::verification_layout(),bus.events.join(",")).unwrap();
        count += 1;
    }
    eprintln!("Generated {count} full ARM/Thumb fixtures");
}
#[cfg(not(feature = "jit"))]
fn main() {
    eprintln!("Enable --features jit");
}

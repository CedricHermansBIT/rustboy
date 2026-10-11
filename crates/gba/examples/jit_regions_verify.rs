//! Original ARM region fixtures: compare generated WASM to reference execution.
#[cfg(feature = "jit")]
fn main() {
    use rustboy_gba::{
        cpu::{Cpu, Memory},
        jit::arm::Region,
    };
    use std::{collections::BTreeMap, io::Write};
    struct Bus {
        words: BTreeMap<u32, u32>,
        clocks: u32,
    }
    impl Memory for Bus {
        fn fetch32(&mut self, pc: u32, _: bool) -> u32 {
            self.clocks += 1;
            *self.words.get(&pc).unwrap_or(&0xe12fff1e)
        }
        fn read8(&mut self, _: u32) -> u8 {
            panic!("data read")
        }
        fn read16(&mut self, _: u32, _: bool) -> u16 {
            panic!("data read")
        }
        fn read32(&mut self, _: u32, _: bool) -> u32 {
            panic!("data read")
        }
        fn write8(&mut self, _: u32, _: u8) {
            panic!("data write")
        }
        fn write16(&mut self, _: u32, _: u16, _: bool) {
            panic!("data write")
        }
        fn write32(&mut self, _: u32, _: u32, _: bool) {
            panic!("data write")
        }
        fn internal(&mut self, clocks: u32) {
            self.clocks += clocks;
        }
    }
    let path = std::env::args().nth(1).expect("JSONL output path");
    let mut out = std::io::BufWriter::new(std::fs::File::create(path).unwrap());
    let mut count = 0;
    let alternate_entries = std::env::var_os("JIT_REGION_ALL_ENTRIES").is_some();
    let pc = 0x03000100;
    let values = [
        0, 1, 0xff, 0xffffff80, 0x100, 0x7fffffff, 0x80000000, 0xffffffff, 0x12345678,
    ];
    let mut check = |ops: &[u32], flags: u32, budget: u32| {
        let words: BTreeMap<_, _> = ops
            .iter()
            .enumerate()
            .map(|(n, &op)| (pc + n as u32 * 4, op))
            .collect();
        let region =
            Region::discover(pc, |address| *words.get(&address).unwrap_or(&0xe12fff1e)).unwrap();
        let hex: String = region
            .compile()
            .iter()
            .map(|byte| format!("{byte:02x}"))
            .collect();
        let mut cpu = Cpu::default();
        for r in 0..15 {
            cpu.r[r] = values[(r + count) % values.len()];
        }
        cpu.r[15] = pc;
        if alternate_entries {
            cpu.r[15] = *region.words.keys().nth(count % region.words.len()).unwrap();
        }
        cpu.set_cpsr(flags | 0x1f).unwrap();
        let initial = cpu.r;
        let flags = cpu.cpsr();
        let mut bus = Bus { words, clocks: 0 };
        let mut last = cpu.r[15];
        while bus.clocks < budget && region.words.contains_key(&cpu.r[15]) {
            last = cpu.r[15];
            cpu.step(&mut bus);
        }
        writeln!(out,"{{\"wasm\":\"{hex}\",\"budget\":{budget},\"initial\":{initial:?},\"flags\":{flags},\"expected\":{:?},\"expectedFlags\":{},\"clocks\":{},\"last\":{last},\"instructions\":{},\"countOffset\":{}}}",cpu.r,cpu.cpsr(),bus.clocks,cpu.instructions,std::mem::offset_of!(Cpu,instructions)).unwrap();
        count += 1;
    };
    for kind in [0, 1, 2, 3, 4, 8, 9, 10, 11, 12, 13, 14, 15] {
        for cond in 0..15 {
            for flags in 0..16 {
                for shift in 0..4 {
                    let amount = [0, 1, 16, 31][flags as usize % 4];
                    let op = cond << 28
                        | kind << 21
                        | 1 << 20
                        | 2 << 16
                        | 3 << 12
                        | amount << 7
                        | shift << 5
                        | 1;
                    check(&[op, 0xe1a04004], flags << 28, 1);
                    let immediate = cond << 28
                        | 1 << 25
                        | kind << 21
                        | 1 << 20
                        | 2 << 16
                        | 3 << 12
                        | (flags << 8)
                        | 0xa5;
                    check(&[immediate, 0xe1a04004], flags << 28, 1);
                }
                for accumulate in [0, 1] {
                    let op =
                        cond << 28 | accumulate << 21 | 1 << 20 | 2 << 16 | 3 << 12 | 1 << 8 | 0x90;
                    check(&[op, 0xe1a04004], flags << 28, 1);
                }
            }
        }
    }
    // Every writable register, PC operands and aliases in resident locals.
    for rd in 0..15 {
        for rn in 0..16 {
            for rm in 0..16 {
                let kind =
                    [0, 1, 2, 3, 4, 8, 9, 10, 11, 12, 13, 14, 15][(rd + rn + rm) as usize % 13];
                let op =
                    0xe0000000 | kind << 21 | 1 << 20 | rn << 16 | rd << 12 | 31 << 7 | 2 << 5 | rm;
                check(&[op, 0xe1a04004], ((rd + rn + rm) & 15) << 28, 3);
            }
        }
        for rs in 0..15 {
            let op = 0xe0300090 | rd << 16 | ((rd + 1) % 15) << 12 | rs << 8 | ((rs + 1) % 15);
            check(&[op, 0xe1a04004], ((rd + rs) & 15) << 28, 3);
        }
    }
    // Connected forward/back edges, conditional execution, links, unsupported
    // exits, multiplication timing and every whole-instruction budget cutoff.
    let program = [
        0xe2800001, 0xe0010290, 0xe3510000, 0x0a000001, 0xe2811001, 0xea000000, 0xe2811002,
        0xe3500010, 0x3afffff6,
    ];
    for budget in 1..=129 {
        for flags in 0..16 {
            check(&program, flags << 28, budget);
        }
    }
    // Conditional BL writes LR only on the taken path; exits into unsupported code.
    for cond in 0..15 {
        for flags in 0..16 {
            check(
                &[cond << 28 | 0x0b000001, 0xe2800001, 0xe1a04004, 0xe2800002],
                flags << 28,
                32,
            );
        }
    }
    eprintln!("Generated {count} ARM reference fixtures");
}
#[cfg(not(feature = "jit"))]
fn main() {
    eprintln!("Enable --features jit");
}

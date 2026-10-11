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
        events: Vec<String>,
    }
    impl Memory for Bus {
        fn instruction_context(&mut self, pc: u32, thumb: bool) {
            self.events.push(format!("[0,{pc},{}]", u32::from(thumb)));
        }
        fn fetch16(&mut self, pc: u32, sequential: bool) -> u16 {
            self.events
                .push(format!("[1,{pc},{}]", u32::from(sequential)));
            self.op
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
    let path = std::env::args().nth(1).expect("output JSONL path");
    let mut out = std::io::BufWriter::new(std::fs::File::create(path).unwrap());
    let mut count = 0;
    for raw in 0..=u16::MAX {
        if jit::supported(raw).is_none() {
            continue;
        }
        let pc = 0x08000102;
        let bytes = jit::compile(pc, &[raw]).unwrap();
        let hex: String = bytes.iter().map(|b| format!("{b:02x}")).collect();
        // Edge values exercise arithmetic overflow/carry, signed shifts,
        // unaligned memory accesses and every conditional-branch flag state.
        let values = [
            0, 1, 0x7fffffff, 0x80000000, 0xffffffff, 0xfffffffd, 32, 0x02000003,
        ];
        let mut cpu = Cpu::default();
        for r in 0..16 {
            cpu.r[r] = values[(r + usize::from(raw & 7)) % values.len()];
        }
        cpu.r[15] = pc;
        cpu.set_cpsr((u32::from(raw & 15) << 28) | T | 0x1f)
            .unwrap();
        let initial = cpu.r;
        let flags = cpu.cpsr();
        let mut bus = Bus {
            op: raw,
            ..Bus::default()
        };
        cpu.step(&mut bus);
        writeln!(out, "{{\"op\":{raw},\"wasm\":\"{hex}\",\"initial\":{initial:?},\"flags\":{flags},\"expected\":{:?},\"expectedFlags\":{},\"events\":[{}]}}", cpu.r, cpu.cpsr(), bus.events.join(",")).unwrap();
        count += 1;
    }
    let program = [
        0x2000u16, 0x2101, 0x3001, 0x004a, 0x1883, 0x2b05, 0x6043, 0x6844, 0xd1f6,
    ];
    let bytes = jit::compile(0x08000102, &program).unwrap();
    let hex: String = bytes.iter().map(|b| format!("{b:02x}")).collect();
    for budget in 1..=program.len() {
        let mut cpu = Cpu::default();
        cpu.r[15] = 0x08000102;
        cpu.set_cpsr(T | 0x1f).unwrap();
        let initial = cpu.r;
        let flags = cpu.cpsr();
        let mut bus = Bus::default();
        for &op in &program[..budget] {
            bus.op = op;
            cpu.step(&mut bus);
        }
        writeln!(out, "{{\"op\":0,\"budget\":{budget},\"wasm\":\"{hex}\",\"initial\":{initial:?},\"flags\":{flags},\"expected\":{:?},\"expectedFlags\":{},\"events\":[{}]}}", cpu.r, cpu.cpsr(), bus.events.join(",")).unwrap();
        count += 1;
    }
    eprintln!("Generated {count} independent Thumb reference fixtures");
}
#[cfg(not(feature = "jit"))]
fn main() {
    eprintln!("Enable --features jit");
}

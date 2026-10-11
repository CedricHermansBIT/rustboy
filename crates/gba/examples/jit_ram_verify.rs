//! Independent interpreter + real bus oracle for connected RAM code generation.
#[cfg(feature = "jit")]
fn main() {
    use rustboy_gba::{
        bus::Bus,
        cpu::{Cpu, Memory},
        jit::{self, arm::Region},
    };
    use std::{collections::BTreeSet, io::Write};
    const PC: u32 = 0x03000100;
    struct Trace {
        bus: Bus,
        pages: BTreeSet<u32>,
        unsafe_access: bool,
        mapped: bool,
        wait: u32,
        writes: BTreeSet<(u32, usize)>,
        addresses: Vec<u32>,
        undo: Vec<(bool, usize, u8)>,
    }
    impl Trace {
        fn safe(&mut self, address: u32, write: bool) -> bool {
            self.addresses.push(address);
            let region = address >> 24;
            let iwram = region == 3 || region == 2 && self.mapped;
            let safe = matches!(region, 2 | 3)
                && (iwram || self.wait != 0)
                && !(write && iwram && self.pages.contains(&((address & 0x7fff) >> 8)));
            self.unsafe_access |= !safe;
            safe
        }
        fn record(&mut self, address: u32, width: usize) {
            let iwram = address >> 24 == 3 || self.mapped;
            let offset = address as usize & if iwram { 0x7fff } else { 0x3ffff };
            for n in 0..width {
                self.writes.insert((u32::from(iwram), offset + n));
                self.undo.push((
                    iwram,
                    offset + n,
                    if iwram {
                        self.bus.iwram[offset + n]
                    } else {
                        self.bus.ewram[offset + n]
                    },
                ));
            }
        }
    }
    impl Memory for Trace {
        fn instruction_context(&mut self, pc: u32, thumb: bool) {
            self.bus.instruction_context(pc, thumb);
        }
        fn finish_instruction(&mut self) -> bool {
            self.bus.finish_instruction()
        }
        fn refill_complete(&mut self, pc: u32) {
            self.bus.refill_complete(pc);
        }
        fn fetch32(&mut self, a: u32, s: bool) -> u32 {
            self.bus.fetch32(a, s)
        }
        fn read8(&mut self, a: u32) -> u8 {
            if self.safe(a, false) {
                self.bus.read8(a)
            } else {
                0
            }
        }
        fn read16(&mut self, a: u32, s: bool) -> u16 {
            if self.safe(a, false) {
                self.bus.read16(a, s)
            } else {
                0
            }
        }
        fn read32(&mut self, a: u32, s: bool) -> u32 {
            if self.safe(a, false) {
                self.bus.read32(a, s)
            } else {
                0
            }
        }
        fn write8(&mut self, a: u32, v: u8) {
            if self.safe(a, true) {
                self.record(a, 1);
                self.bus.write8(a, v);
            }
        }
        fn write16(&mut self, a: u32, v: u16, s: bool) {
            if self.safe(a, true) {
                self.record(a & !1, 2);
                self.bus.write16(a, v, s);
            }
        }
        fn write32(&mut self, a: u32, v: u32, s: bool) {
            if self.safe(a, true) {
                self.record(a & !3, 4);
                self.bus.write32(a, v, s);
            }
        }
        fn internal(&mut self, n: u32) {
            self.bus.internal(n);
        }
    }
    fn checksum(bytes: &[u8]) -> u32 {
        bytes.iter().fold(2166136261u32, |hash, &b| {
            (hash ^ u32::from(b)).wrapping_mul(16777619)
        })
    }
    let path = std::env::args().nth(1).expect("JSONL path");
    let mut out = std::io::BufWriter::new(std::fs::File::create(path).unwrap());
    let mut count = 0usize;
    let alternate_entries = std::env::var_os("JIT_REGION_ALL_ENTRIES").is_some();
    let mut check = |ops: &[u32], registers: [u32; 16], flags: u32, control: u32, budget: u32| {
        let region = Region::discover(PC, |a| {
            ops.get((a.wrapping_sub(PC) / 4) as usize)
                .copied()
                .unwrap_or(0xe12fff1e)
        })
        .unwrap();
        let hex: String = region
            .compile()
            .iter()
            .map(|b| format!("{b:02x}"))
            .collect();
        let mut bus = Bus::new(vec![0; 192], None).unwrap();
        for (n, b) in bus.ewram.iter_mut().enumerate() {
            *b = (n ^ (n >> 8) ^ 0xa5) as u8;
        }
        for (n, b) in bus.iwram.iter_mut().enumerate() {
            *b = (n ^ (n >> 8) ^ 0xa5) as u8;
        }
        for (n, op) in ops.iter().chain(std::iter::once(&0xe12fff1e)).enumerate() {
            bus.iwram[0x100 + n * 4..0x104 + n * 4].copy_from_slice(&op.to_le_bytes());
        }
        bus.write32(0x04000800, control, false);
        bus.take_cycles();
        let mut cpu = Cpu::default();
        cpu.set_cpsr(flags | 0x1f).unwrap();
        cpu.r = registers;
        cpu.r[15] = PC;
        if alternate_entries {
            cpu.r[15] = *region.words.keys().nth(count % region.words.len()).unwrap();
        }
        let initial = jit::verification_state(&cpu);
        let mut trace = Trace {
            bus,
            pages: region.words.keys().map(|a| (a & 0x7fff) >> 8).collect(),
            unsafe_access: false,
            mapped: control & 0x20 == 0,
            wait: if control >> 24 & 15 == 15 {
                0
            } else {
                16 - (control >> 24 & 15)
            },
            writes: BTreeSet::new(),
            addresses: Vec::new(),
            undo: Vec::new(),
        };
        let mut clocks = 0;
        let mut last = 0;
        while clocks < budget && region.words.contains_key(&cpu.r[15]) {
            let before = cpu.clone();
            let writes = trace.writes.clone();
            trace.addresses.clear();
            trace.undo.clear();
            let address = cpu.r[15];
            cpu.step(&mut trace);
            if let Some(&first) = trace.addresses.first() {
                trace.unsafe_access |= trace.addresses.iter().any(|&a| a >> 15 != first >> 15);
            }
            if trace.unsafe_access {
                for &(iwram, offset, value) in trace.undo.iter().rev() {
                    if iwram {
                        trace.bus.iwram[offset] = value;
                    } else {
                        trace.bus.ewram[offset] = value;
                    }
                }
                trace.writes = writes;
                cpu = before;
                break;
            }
            last = address;
            clocks += trace.bus.take_cycles() as u32;
        }
        let writes: Vec<_> = trace
            .writes
            .iter()
            .map(|&(region, index)| {
                [
                    region,
                    index as u32,
                    u32::from(if region == 1 {
                        trace.bus.iwram[index]
                    } else {
                        trace.bus.ewram[index]
                    }),
                ]
            })
            .collect();
        let hashes = if count % 128 == 0 {
            format!(
                "[{},{}]",
                checksum(&trace.bus.ewram),
                checksum(&trace.bus.iwram)
            )
        } else {
            "null".into()
        };
        writeln!(out,"{{\"ops\":{ops:?},\"wasm\":\"{hex}\",\"initial\":{initial:?},\"expected\":{:?},\"layout\":{:?},\"control\":{control},\"budget\":{budget},\"clocks\":{clocks},\"last\":{last},\"writes\":{writes:?},\"hashes\":{hashes}}}",jit::verification_state(&cpu),jit::verification_layout()).unwrap();
        count += 1;
    };
    let mut regs = [0u32; 16];
    for (r, v) in regs.iter_mut().enumerate() {
        *v = 0x02004003u32.wrapping_add(r as u32 * 0x123);
    }
    regs[1] = 0x02000403;
    regs[2] = 0x03004803;
    regs[3] = 0xffffff80;
    regs[4] = 0x87654321;
    // Every addressing flag, all conditions, shifted offsets and signed widths.
    for load in 0..2 {
        for byte in 0..2 {
            for pre in 0..2 {
                for up in 0..2 {
                    for wb in 0..2 {
                        for cond in 0..15 {
                            for shift in 0..4 {
                                for imm in [true, false] {
                                    let op = cond << 28
                                        | 0x04000000
                                        | pre << 24
                                        | up << 23
                                        | byte << 22
                                        | wb << 21
                                        | load << 20
                                        | 1 << 16
                                        | 4 << 12
                                        | if imm {
                                            3
                                        } else {
                                            1 << 25
                                                | [0, 1, 16, 31][shift as usize] << 7
                                                | shift << 5
                                                | 3
                                        };
                                    check(
                                        &[op, 0xe2844001],
                                        regs,
                                        ((cond + shift) & 15) << 28,
                                        0x0d000020,
                                        20,
                                    );
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    for load in 0..2 {
        for kind in 1..4 {
            if load == 0 && kind != 1 {
                continue;
            }
            for pre in 0..2 {
                for up in 0..2 {
                    for wb in 0..2 {
                        for cond in 0..15 {
                            for immediate in 0..2 {
                                let op = cond << 28
                                    | pre << 24
                                    | up << 23
                                    | immediate << 22
                                    | wb << 21
                                    | load << 20
                                    | 2 << 16
                                    | 4 << 12
                                    | 0x90
                                    | kind << 5
                                    | 3;
                                check(&[op, 0xe2844001], regs, (cond & 15) << 28, 0x0d000020, 20);
                            }
                        }
                    }
                }
            }
        }
    }
    // All base/destination aliases, PC operands and guarded hardware/code exits.
    for rn in 0..16 {
        for rd in 0..16 {
            for op in [
                0xe5900000, 0xe5c00000, 0xe4900004, 0xe4800004, 0xe1d000d0, 0xe1d000f0,
            ] {
                let op = (op & !0xff000) | rn << 16 | rd << 12;
                if !rustboy_gba::jit::arm::supported(PC, op) {
                    continue;
                }
                check(&[op, 0xe2844001], regs, 0xa0000000, 0x0d000020, 20);
            }
        }
    }
    // All block address modes, base-in-list/writeback aliases, conditions and
    // transfers that cross mirrors or overlap a later page of running code.
    for rn in 0..15 {
        for load in 0..2 {
            for pre in 0..2 {
                for up in 0..2 {
                    for wb in 0..2 {
                        for list in [1, 0x7fff, 0x5555, 0x4000, (1 << rn) | 0x81] {
                            let op = 0xe8000000
                                | pre << 24
                                | up << 23
                                | wb << 21
                                | load << 20
                                | rn << 16
                                | list;
                            for address in [0x02004303, 0x03004303, 0x03007ff0, 0x020001fc] {
                                let mut regs = regs;
                                regs[rn as usize] = address;
                                check(&[op, 0xe2844001], regs, 0x20000000, 0x0d000020, 100);
                            }
                        }
                    }
                }
            }
        }
    }
    for cond in 0..15 {
        for flags in [0, 0x20000000, 0xa0000000, 0xf0000000] {
            check(
                &[cond << 28 | 0x09a1401f, 0xe2844001],
                regs,
                flags,
                0x0d000020,
                33,
            );
        }
    }
    for control in [0x0d000020, 0x00000020, 0x0e000020] {
        for load in 0..2 {
            for address in [0x02004003, 0x03004003] {
                let mut regs = regs;
                regs[1] = address;
                for budget in 1..=94 {
                    check(
                        &[0xe8a17fff | load << 20, 0xe2844001],
                        regs,
                        0xa0000000,
                        control,
                        budget,
                    );
                }
            }
        }
    }
    let program = [
        0xe5914000, 0xe2844001, 0xe5824000, 0xe2811004, 0xe2822004, 0xe2555001, 0x1afffff8,
    ];
    for control in [0x0d000020, 0x0f000020, 0x00000020, 0x0e000020, 0x0d000000] {
        for address in [
            0x02000400, 0x02040003, 0x02fffffd, 0x03004000, 0x0300c003, 0x03fffffd, 0x04000100,
            0x03000100, 0x02000100,
        ] {
            let mut regs = regs;
            regs[1] = address;
            regs[2] = address;
            regs[5] = 4;
            for budget in 1..=33 {
                check(&program, regs, 0x20000000, control, budget);
            }
        }
    }
    eprintln!("Generated {count} connected RAM fixtures");
}
#[cfg(not(feature = "jit"))]
fn main() {
    eprintln!("Enable --features jit");
}

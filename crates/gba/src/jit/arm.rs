//! Connected ARM regions with direct, guarded RAM transfers and IWRAM fetches.
//! Hardware accesses and writes to the running code remain interpreter exits.
use super::{name, section, unsigned, Code, A, B, ELAPSED, FLAGS, RESULT, TEMP};
use std::collections::{BTreeMap, BTreeSet};
const NODE: u32 = 19;
const PC: u32 = 27;
const LAST: u32 = 28;
const DONE: u32 = 29;
const CARRY: u32 = 30;
const COST: u32 = 31;
const ADDRESS: u32 = 32;
const ADJUSTED: u32 = 33;
const POINTER: u32 = 34;
const WAIT: u32 = 35;
const IS_IWRAM: u32 = 36;
const WROTE: u32 = 37;
const END_ADDRESS: u32 = 38;
/// Fixed-width ABI shared by generated modules and native verification fixtures.
/// Pointers refer to stable allocations for the duration of one region call.
#[repr(C)]
pub struct RamContext {
    pub ewram: u32,
    pub iwram: u32,
    pub ewram_wait: u32,
    pub mapped_iwram: u32,
    pub iwram_versions: u32,
    pub memory_version: u32,
    pub wrote: u32,
}
fn transfer(op: u32) -> bool {
    op & 0x0c000000 == 0x04000000
        || op & 0x0e000090 == 0x00000090 && op & 0x60 != 0
        || block_transfer(op)
}
fn block_transfer(op: u32) -> bool {
    op & 0x0e000000 == 0x08000000
}
pub(super) fn reg(r: u32) -> u32 {
    if r < 8 {
        3 + r
    } else {
        12 + r
    }
}
fn target(pc: u32, op: u32) -> u32 {
    pc.wrapping_add(8)
        .wrapping_add(((op << 8) as i32 >> 6) as u32)
}
/// Conservative register use for the supported region instructions. Include
/// every possible destination in the entry loads: conditional instructions and
/// guarded exits must retain its old value when they do not execute.
fn registers(op: u32) -> (u32, u32) {
    let bit = |r: u32| 1u32 << r;
    if op & 0x0e000000 == 0x0a000000 {
        let written = if op & 0x01000000 != 0 { bit(14) } else { 0 };
        return (written, written);
    }
    if op & 0x0fc000f0 == 0x00000090 {
        let written = bit(op >> 16 & 15);
        let used = bit(op & 15)
            | bit(op >> 8 & 15)
            | if op & 0x00200000 != 0 {
                bit(op >> 12 & 15)
            } else {
                0
            };
        return ((used | written) & 0x7fff, written);
    }
    if transfer(op) {
        let base = bit(op >> 16 & 15);
        let load = op & 0x00100000 != 0;
        let (used, written) = if block_transfer(op) {
            let list = op & 0x7fff;
            (
                base | list,
                (if load { list } else { 0 }) | if op & 0x00200000 != 0 { base } else { 0 },
            )
        } else {
            let destination = bit(op >> 12 & 15);
            let half = op & 0x0c000000 == 0;
            let register_offset = if half {
                op & 0x00400000 == 0
            } else {
                op & 0x02000000 != 0
            };
            let used = base | destination | if register_offset { bit(op & 15) } else { 0 };
            let written = (if load { destination } else { 0 })
                | if op & 0x01000000 == 0 || op & 0x00200000 != 0 {
                    base
                } else {
                    0
                };
            (used, written)
        };
        return ((used | written) & 0x7fff, written & 0x7fff);
    }
    let kind = op >> 21 & 15;
    let written = if (8..12).contains(&kind) {
        0
    } else {
        bit(op >> 12 & 15)
    };
    let used = bit(op >> 16 & 15)
        | if op & 0x02000000 == 0 {
            bit(op & 15)
        } else {
            0
        };
    ((used | written) & 0x7fff, written & 0x7fff)
}
/// Arithmetic, MUL/MLA, guarded RAM transfers and connected IWRAM branches.
pub fn supported(pc: u32, op: u32) -> bool {
    if pc >> 24 != 3 || pc & 3 != 0 || op >> 28 == 15 {
        return false;
    }
    if op & 0x0e000000 == 0x0a000000 {
        let to = target(pc, op);
        return to >> 24 == 3 && to.wrapping_add(4) >> 24 == 3;
    }
    if op & 0x0fc000f0 == 0x00000090 {
        return [op >> 16 & 15, op >> 12 & 15, op >> 8 & 15, op & 15]
            .iter()
            .all(|&r| r != 15);
    }
    if transfer(op) {
        if block_transfer(op) {
            return op & 0x00408000 == 0 && op & 0x7fff != 0 && op >> 16 & 15 != 15;
        }
        let load = op & 0x00100000 != 0;
        let half = op & 0x0c000000 == 0;
        return !(op >> 16 & 15 == 15 && (op & 0x01000000 == 0 || op & 0x00200000 != 0))
            && !(load && op >> 12 & 15 == 15)
            && (half && (load || op >> 5 & 3 == 1) || !half && op & 0x02000010 != 0x02000010);
    }
    let kind = op >> 21 & 15;
    op & 0x0c000000 == 0
        && op & 0x02000090 != 0x90
        && (op & 0x02000000 != 0 || op & 16 == 0)
        && op >> 12 & 15 != 15
        && !matches!(kind, 5..=7)
        && (!(8..12).contains(&kind) || op & 0x00100000 != 0)
}
#[derive(Debug)]
pub struct Region {
    pub entry: u32,
    pub words: BTreeMap<u32, u32>,
}
impl Region {
    /// Follow observed entry points conservatively, bounded to a 1-KiB window.
    /// Unsupported successors become precise exits rather than guessed code.
    pub fn discover(entry: u32, mut peek: impl FnMut(u32) -> u32) -> Option<Self> {
        Self::discover_with_ram(entry, true, &mut peek)
    }
    pub fn discover_with_ram(
        entry: u32,
        ram: bool,
        mut peek: impl FnMut(u32) -> u32,
    ) -> Option<Self> {
        let mut words = BTreeMap::new();
        let mut pending = vec![entry];
        while let Some(pc) = pending.pop() {
            if words.contains_key(&pc) || pc.abs_diff(entry) > 512 || words.len() >= 128 {
                continue;
            }
            let op = peek(pc);
            if !supported(pc, op) || (!ram && transfer(op)) {
                continue;
            }
            words.insert(pc, op);
            if op & 0x0e000000 == 0x0a000000 {
                pending.push(target(pc, op));
                if op >> 28 == 14 {
                    continue;
                }
            }
            pending.push(pc.wrapping_add(4));
        }
        (words.contains_key(&entry) && words.len() >= 2).then_some(Self { entry, words })
    }
    pub fn compile(&self) -> Vec<u8> {
        let (used, written) = self.words.values().fold((0, 0), |(used, written), &op| {
            let (reads, writes) = registers(op);
            (used | reads, written | writes)
        });
        // Every covered instruction is an entry point. Nested labels share a
        // single copy of its code; sequential execution falls through directly.
        let starts: Vec<_> = self.words.keys().copied().collect();
        let first = starts[0];
        let slots = (starts[starts.len() - 1] - first) / 4 + 1;
        let mut c = Code(Vec::new());
        c.op(1);
        c.op(36);
        c.op(0x7f); // locals 3..38, all i32
        for r in 0..15 {
            if used >> r & 1 != 0 {
                c.load(0, r * 4);
                c.set(reg(r));
            }
        }
        c.load(0, 64);
        c.set(FLAGS);
        c.load(0, 60);
        c.set(PC);
        c.get(PC);
        c.constant(first);
        c.op(0x6b);
        c.constant(2);
        c.op(0x76);
        c.set(NODE);
        c.op(0x02);
        c.op(0x40); // exit
        c.op(0x03);
        c.op(0x40); // dispatch loop
        for _ in &starts {
            c.op(0x02);
            c.op(0x40);
        }
        c.get(NODE);
        c.op(0x0e);
        unsigned(slots, &mut c.0);
        for slot in 0..slots {
            let depth = starts
                .binary_search(&(first + slot * 4))
                .unwrap_or(starts.len() + 1);
            unsigned(depth as u32, &mut c.0);
        }
        unsigned(starts.len() as u32 + 1, &mut c.0);
        for (n, &start) in starts.iter().enumerate() {
            c.op(0x0b);
            let exit_depth = (starts.len() - n) as u32;
            let loop_depth = exit_depth - 1;
            let pc = start;
            let op = self.words[&pc];
            let cond = op >> 28;
            if transfer(op) {
                if cond != 14 {
                    condition(&mut c, cond);
                    c.op(0x04);
                    c.op(0x40);
                }
                memory_guard(
                    &mut c,
                    pc,
                    op,
                    &self.words,
                    exit_depth + u32::from(cond != 14),
                );
                if cond != 14 {
                    c.op(0x0b);
                }
            }
            c.constant(pc);
            c.set(LAST);
            c.constant(pc.wrapping_add(4));
            c.set(PC);
            c.constant(1);
            c.set(COST);
            if cond != 14 {
                condition(&mut c, cond);
                c.op(0x04);
                c.op(0x40);
            }
            emit(&mut c, pc, op);
            if cond != 14 {
                c.op(0x0b);
            }
            c.get(DONE);
            c.constant(1);
            c.op(0x6a);
            c.set(DONE);
            c.get(ELAPSED);
            c.get(COST);
            c.op(0x6a);
            c.set(ELAPSED);
            c.get(ELAPSED);
            c.get(2);
            c.op(0x4f);
            c.op(0x0d);
            unsigned(exit_depth, &mut c.0);
            let next = pc.wrapping_add(4);
            if op & 0x0e000000 == 0x0a000000 {
                // A branch remains within this module when its target is supported.
                condition(&mut c, cond);
                c.op(0x04);
                c.op(0x40);
                jump(
                    &mut c,
                    target(pc, op),
                    &starts,
                    loop_depth + 1,
                    exit_depth + 1,
                );
                c.op(0x0b);
            }
            if starts.get(n + 1) != Some(&next) {
                c.op(0x0c);
                unsigned(exit_depth, &mut c.0);
            }
        }
        c.op(0x0b);
        c.op(0x0b);
        for r in 0..15 {
            if written >> r & 1 != 0 {
                c.store(0, r * 4, reg(r));
            }
        }
        c.store(0, 60, PC);
        c.store(0, 64, FLAGS);
        c.store(1, 24, WROTE);
        // A conservative version change is sufficient to invalidate full blocks.
        c.get(WROTE);
        c.op(0x04);
        c.op(0x40);
        c.load(1, 20);
        c.set(POINTER);
        increment_version(&mut c, POINTER);
        c.op(0x0b);
        // Architectural instruction count is accumulated once per region entry.
        c.get(0);
        c.get(0);
        c.op(0x29);
        unsigned(3, &mut c.0);
        unsigned(
            std::mem::offset_of!(crate::cpu::Cpu, instructions) as u32,
            &mut c.0,
        );
        c.get(DONE);
        c.op(0xad);
        c.op(0x7c);
        c.op(0x37);
        unsigned(3, &mut c.0);
        unsigned(
            std::mem::offset_of!(crate::cpu::Cpu, instructions) as u32,
            &mut c.0,
        );
        c.get(LAST);
        c.op(0xad);
        c.op(0x42);
        super::signed(32, &mut c.0);
        c.op(0x86);
        c.get(ELAPSED);
        c.op(0xad);
        c.op(0x84);
        c.op(0x0b);
        let mut module = b"\0asm\x01\0\0\0".to_vec();
        section(1, vec![1, 0x60, 3, 0x7f, 0x7f, 0x7f, 1, 0x7e], &mut module);
        let mut imports = vec![1];
        name("env", &mut imports);
        name("memory", &mut imports);
        imports.extend([2, 0, 0]);
        section(2, imports, &mut module);
        section(3, vec![1, 0], &mut module);
        let mut exports = vec![1];
        name("run", &mut exports);
        exports.extend([0, 0]);
        section(7, exports, &mut module);
        let mut body = vec![1];
        unsigned(c.0.len() as u32, &mut body);
        body.extend(c.0);
        section(10, body, &mut module);
        module
    }
}
fn jump(c: &mut Code, pc: u32, starts: &[u32], loop_depth: u32, exit_depth: u32) {
    if starts.binary_search(&pc).is_ok() {
        c.constant((pc - starts[0]) / 4);
        c.set(NODE);
        c.op(0x0c);
        unsigned(loop_depth, &mut c.0);
    } else {
        c.op(0x0c);
        unsigned(exit_depth, &mut c.0);
    }
}
fn bit(c: &mut Code, n: u32) {
    c.get(FLAGS);
    c.constant(n);
    c.op(0x76);
    c.constant(1);
    c.op(0x71);
}
pub(super) fn condition(c: &mut Code, cond: u32) {
    match cond {
        0 | 1 => bit(c, 30),
        2 | 3 => bit(c, 29),
        4 | 5 => bit(c, 31),
        6 | 7 => bit(c, 28),
        8 | 9 => {
            bit(c, 29);
            bit(c, 30);
            c.op(0x45);
            c.op(0x71);
        }
        10 | 11 => {
            bit(c, 31);
            bit(c, 28);
            c.op(0x46);
        }
        12 | 13 => {
            bit(c, 31);
            bit(c, 28);
            c.op(0x46);
            bit(c, 30);
            c.op(0x45);
            c.op(0x71);
        }
        _ => c.constant(1),
    }
    if cond < 14 && cond & 1 != 0 {
        c.op(0x45);
    }
}
fn operand(c: &mut Code, r: u32, pc: u32) {
    if r == 15 {
        c.constant(pc.wrapping_add(8));
    } else {
        c.get(reg(r));
    }
}
fn increment_version(c: &mut Code, pointer: u32) {
    c.get(pointer);
    c.get(pointer);
    c.op(0x29);
    unsigned(3, &mut c.0);
    unsigned(0, &mut c.0);
    c.op(0x42);
    super::signed(1, &mut c.0);
    c.op(0x7c);
    c.op(0x37);
    unsigned(3, &mut c.0);
    unsigned(0, &mut c.0);
}
fn memory_guard(c: &mut Code, pc: u32, op: u32, words: &BTreeMap<u32, u32>, exit: u32) {
    if block_transfer(op) {
        let length = (op & 0x7fff).count_ones() * 4;
        operand(c, op >> 16 & 15, pc);
        c.set(A);
        c.get(A);
        c.constant(length);
        c.op(if op & 0x00800000 != 0 { 0x6a } else { 0x6b });
        c.set(ADJUSTED);
        c.get(if op & 0x00800000 != 0 { A } else { ADJUSTED });
        c.constant(if op & 0x01800000 == 0x01800000 || op & 0x01800000 == 0 {
            4
        } else {
            0
        });
        c.op(0x6a);
        c.set(ADDRESS);
        c.get(ADDRESS);
        c.constant(!3);
        c.op(0x71);
        c.constant(length - 1);
        c.op(0x6a);
        c.set(END_ADDRESS);
        // Guard the whole transfer before the first read, write or register change.
        c.get(ADDRESS);
        c.get(END_ADDRESS);
        c.op(0x73);
        c.constant(15);
        c.op(0x76);
        c.op(0x0d);
        unsigned(exit, &mut c.0);
        ram_guard(c, op, words, exit);
        return;
    }
    let half = op & 0x0c000000 == 0;
    if half {
        if op & 0x00400000 != 0 {
            c.constant((op >> 4 & 0xf0) | (op & 15));
        } else {
            operand(c, op & 15, pc);
        }
    } else if op & 0x02000000 == 0 {
        c.constant(op & 4095);
    } else {
        operand(c, op & 15, pc);
        c.set(B);
        let kind = op >> 5 & 3;
        let amount = op >> 7 & 31;
        if kind == 3 && amount == 0 {
            c.get(B);
            c.constant(1);
            c.op(0x76);
            bit(c, 29);
            c.constant(31);
            c.op(0x74);
            c.op(0x72);
        } else if kind == 1 && amount == 0 {
            c.constant(0);
        } else {
            c.get(B);
            c.constant(if amount == 0 && kind == 2 { 31 } else { amount });
            c.op(match kind {
                0 => 0x74,
                1 => 0x76,
                2 => 0x75,
                _ => 0x78,
            });
        }
    }
    c.set(B);
    operand(c, op >> 16 & 15, pc);
    c.set(A);
    c.get(A);
    c.get(B);
    c.op(if op & 0x00800000 != 0 { 0x6a } else { 0x6b });
    c.set(ADJUSTED);
    c.get(if op & 0x01000000 != 0 { ADJUSTED } else { A });
    c.set(ADDRESS);
    c.get(ADDRESS);
    c.set(END_ADDRESS);
    ram_guard(c, op, words, exit);
}
fn ram_guard(c: &mut Code, op: u32, words: &BTreeMap<u32, u32>, exit: u32) {
    c.get(ADDRESS);
    c.constant(24);
    c.op(0x76);
    c.set(TEMP);
    // Exit before fetching/executing this instruction if it would observe hardware.
    c.get(TEMP);
    c.constant(2);
    c.op(0x46);
    c.get(TEMP);
    c.constant(3);
    c.op(0x46);
    c.op(0x72);
    c.op(0x45);
    c.op(0x0d);
    unsigned(exit, &mut c.0);
    c.get(TEMP);
    c.constant(3);
    c.op(0x46);
    c.load(1, 12);
    c.op(0x72);
    c.set(IS_IWRAM);
    c.constant(1);
    c.load(1, 8);
    c.get(IS_IWRAM);
    c.op(0x1b);
    c.set(WAIT);
    c.get(WAIT);
    c.op(0x45);
    c.op(0x0d);
    unsigned(exit, &mut c.0);
    if op & 0x00100000 == 0 {
        c.get(IS_IWRAM);
        c.op(0x04);
        c.op(0x40);
        // Treat any write to a covered code page as an exit, including aliases.
        let pages: BTreeSet<_> = words.keys().map(|pc| (pc & 0x7fff) >> 8).collect();
        let mut ranges = Vec::new();
        for page in pages {
            if let Some((_, end)) = ranges.last_mut() {
                if *end + 1 == page {
                    *end = page;
                    continue;
                }
            }
            ranges.push((page, page));
        }
        c.get(ADDRESS);
        c.constant(0x7fff);
        c.op(0x71);
        c.constant(8);
        c.op(0x76);
        c.set(B);
        for (start, end) in ranges {
            c.get(END_ADDRESS);
            c.constant(0x7fff);
            c.op(0x71);
            c.constant(8);
            c.op(0x76);
            c.constant(start);
            c.op(0x4f);
            c.get(B);
            c.constant(end);
            c.op(0x4d);
            c.op(0x71);
            c.op(0x0d);
            unsigned(exit + 1, &mut c.0);
        }
        c.op(0x0b);
    }
    c.load(1, 4);
    c.load(1, 0);
    c.get(IS_IWRAM);
    c.op(0x1b);
    c.get(ADDRESS);
    c.constant(0x7fff);
    c.constant(0x3ffff);
    c.get(IS_IWRAM);
    c.op(0x1b);
    c.op(0x71);
    c.op(0x6a);
    c.set(POINTER);
}
fn memory_emit(c: &mut Code, pc: u32, op: u32) {
    if block_transfer(op) {
        block_emit(c, op);
        return;
    }
    let half = op & 0x0c000000 == 0;
    let load = op & 0x00100000 != 0;
    let kind = if half {
        op >> 5 & 3
    } else if op & 0x00400000 != 0 {
        2
    } else {
        0
    };
    let width = match kind {
        0 => 4,
        1 | 3 => 2,
        _ => 1,
    };
    c.get(COST);
    c.get(WAIT);
    if width == 4 {
        c.constant(2);
        c.constant(1);
        c.get(IS_IWRAM);
        c.op(0x45);
        c.op(0x1b);
        c.op(0x6c);
    }
    if load {
        c.constant(1);
        c.op(0x6a);
    }
    c.op(0x6a);
    c.set(COST);
    if load {
        if half && kind == 3 {
            c.get(ADDRESS);
            c.constant(1);
            c.op(0x71);
            c.op(0x04);
            c.op(0x7f);
            c.get(POINTER);
            c.op(0x2c);
            unsigned(0, &mut c.0);
            unsigned(0, &mut c.0);
            c.op(0x05);
            c.get(POINTER);
            c.op(0x2e);
            unsigned(1, &mut c.0);
            unsigned(0, &mut c.0);
            c.op(0x0b);
        } else {
            c.get(POINTER);
            if width != 1 {
                c.get(ADDRESS);
                c.constant(width - 1);
                c.op(0x71);
                c.op(0x6b);
            }
            c.op(match kind {
                0 => 0x28,
                1 => 0x2f,
                _ => {
                    if half {
                        0x2c
                    } else {
                        0x2d
                    }
                }
            });
            unsigned(
                if width == 4 {
                    2
                } else if width == 2 {
                    1
                } else {
                    0
                },
                &mut c.0,
            );
            unsigned(0, &mut c.0);
            if width != 1 {
                c.get(ADDRESS);
                c.constant(width - 1);
                c.op(0x71);
                c.constant(8);
                c.op(0x6c);
                c.op(0x78);
            }
        }
        c.set(reg(op >> 12 & 15));
    } else {
        // Align the guest offset, independently of the allocation's alignment.
        c.get(POINTER);
        if width != 1 {
            c.get(ADDRESS);
            c.constant(width - 1);
            c.op(0x71);
            c.op(0x6b);
        }
        let rd = op >> 12 & 15;
        if rd == 15 {
            c.constant(pc.wrapping_add(12));
        } else {
            c.get(reg(rd));
        }
        c.op(match width {
            4 => 0x36,
            2 => 0x3b,
            _ => 0x3a,
        });
        unsigned(
            if width == 4 {
                2
            } else if width == 2 {
                1
            } else {
                0
            },
            &mut c.0,
        );
        unsigned(0, &mut c.0);
        c.constant(1);
        c.set(WROTE);
        c.get(IS_IWRAM);
        c.op(0x04);
        c.op(0x40);
        c.load(1, 16);
        c.get(ADDRESS);
        c.constant(0x7fff);
        c.op(0x71);
        c.constant(8);
        c.op(0x76);
        c.constant(3);
        c.op(0x74);
        c.op(0x6a);
        c.set(POINTER);
        increment_version(c, POINTER);
        c.op(0x0b);
    }
    let rn = op >> 16 & 15;
    if (op & 0x01000000 == 0 || op & 0x00200000 != 0) && !(load && op >> 12 & 15 == rn) {
        c.get(ADJUSTED);
        c.set(reg(rn));
    }
}
fn block_emit(c: &mut Code, op: u32) {
    let load = op & 0x00100000 != 0;
    let wb = op & 0x00200000 != 0;
    let rn = op >> 16 & 15;
    let list = op & 0x7fff;
    c.get(COST);
    c.get(WAIT);
    c.constant(1);
    c.constant(2);
    c.get(IS_IWRAM);
    c.op(0x1b);
    c.op(0x6c);
    c.constant(list.count_ones());
    c.op(0x6c);
    if load {
        c.constant(1);
        c.op(0x6a);
    }
    c.op(0x6a);
    c.set(COST);
    c.get(POINTER);
    c.get(ADDRESS);
    c.constant(3);
    c.op(0x71);
    c.op(0x6b);
    c.set(POINTER);
    let mut offset = 0;
    for r in 0..15 {
        if list >> r & 1 == 0 {
            continue;
        }
        c.get(POINTER);
        if !load {
            c.get(if r == rn && wb && offset != 0 {
                ADJUSTED
            } else {
                reg(r)
            });
        }
        c.op(if load { 0x28 } else { 0x36 });
        unsigned(2, &mut c.0);
        unsigned(offset, &mut c.0);
        if load {
            c.set(reg(r));
        }
        offset += 4;
    }
    if wb && !(load && list >> rn & 1 != 0) {
        c.get(ADJUSTED);
        c.set(reg(rn));
    }
    if !load {
        c.constant(1);
        c.set(WROTE);
        c.get(IS_IWRAM);
        c.op(0x04);
        c.op(0x40);
        // At most two pages are touched by this bounded transfer. Conservative
        // increments are valid even when both endpoints fall on the same page.
        for address in [ADDRESS, END_ADDRESS] {
            c.load(1, 16);
            c.get(address);
            c.constant(0x7fff);
            c.op(0x71);
            c.constant(8);
            c.op(0x76);
            c.constant(3);
            c.op(0x74);
            c.op(0x6a);
            c.set(TEMP);
            increment_version(c, TEMP);
        }
        c.op(0x0b);
    }
}
pub(super) fn emit(c: &mut Code, pc: u32, op: u32) {
    if transfer(op) {
        memory_emit(c, pc, op);
        return;
    }
    if op & 0x0e000000 == 0x0a000000 {
        if op & 0x01000000 != 0 {
            c.constant(pc.wrapping_add(4));
            c.set(reg(14));
        }
        c.constant(target(pc, op));
        c.set(PC);
        c.constant(3);
        c.set(COST);
        return;
    }
    if op & 0x0fc000f0 == 0x00000090 {
        let rs = reg(op >> 8 & 15);
        c.constant(4);
        c.set(TEMP);
        for bytes in (1..=3).rev() {
            c.get(rs);
            c.constant(bytes * 8);
            c.op(0x76);
            c.set(B);
            c.get(B);
            c.op(0x45);
            c.get(B);
            c.constant(u32::MAX >> (bytes * 8));
            c.op(0x46);
            c.op(0x72);
            c.op(0x04);
            c.op(0x40);
            c.constant(bytes);
            c.set(TEMP);
            c.op(0x0b);
        }
        c.get(COST);
        c.get(TEMP);
        c.op(0x6a);
        if op & 0x00200000 != 0 {
            c.constant(1);
            c.op(0x6a);
        }
        c.set(COST);
        c.get(reg(op & 15));
        c.get(rs);
        c.op(0x6c);
        if op & 0x00200000 != 0 {
            c.get(reg(op >> 12 & 15));
            c.op(0x6a);
        }
        c.set(RESULT);
        if op & 0x00100000 != 0 {
            c.nz(0xc0000000);
        }
        c.get(RESULT);
        c.set(reg(op >> 16 & 15));
        return;
    }
    let kind = op >> 21 & 15;
    bit(c, 29);
    c.set(CARRY);
    if op & 0x02000000 != 0 {
        let rotation = (op >> 8 & 15) * 2;
        let value = (op & 255).rotate_right(rotation);
        c.constant(value);
        c.set(B);
        if rotation != 0 {
            c.constant(value >> 31);
            c.set(CARRY);
        }
    } else {
        operand(c, op & 15, pc);
        c.set(B);
        let kind = op >> 5 & 3;
        let amount = op >> 7 & 31;
        if kind != 0 || amount != 0 {
            if kind == 3 && amount == 0 {
                c.get(B);
                c.constant(1);
                c.op(0x76);
                c.get(CARRY);
                c.constant(31);
                c.op(0x74);
                c.op(0x72);
                c.set(TEMP);
                c.get(B);
                c.constant(1);
                c.op(0x71);
                c.set(CARRY);
                c.get(TEMP);
                c.set(B);
            } else {
                let amount = if amount == 0 { 32 } else { amount };
                c.get(B);
                c.constant(if kind == 0 { 32 - amount } else { amount - 1 });
                c.op(0x76);
                c.constant(1);
                c.op(0x71);
                c.set(CARRY);
                if kind == 1 && amount == 32 {
                    c.constant(0);
                } else {
                    c.get(B);
                    c.constant(amount.min(31));
                    c.op(match kind {
                        0 => 0x74,
                        1 => 0x76,
                        2 => 0x75,
                        _ => 0x78,
                    });
                }
                c.set(B);
            }
        }
    }
    operand(c, op >> 16 & 15, pc);
    c.set(A);
    let flags = op & 0x00100000 != 0;
    if matches!(kind, 2..=4 | 10 | 11) {
        if kind == 3 {
            c.get(A);
            c.get(B);
            c.set(A);
            c.set(B);
        }
        if flags {
            c.arithmetic(matches!(kind, 2 | 3 | 10));
        } else {
            c.get(A);
            c.get(B);
            c.op(if matches!(kind, 2 | 3) { 0x6b } else { 0x6a });
            c.set(RESULT);
        }
    } else {
        match kind {
            13 => c.get(B),
            15 => {
                c.get(B);
                c.constant(u32::MAX);
                c.op(0x73);
            }
            _ => {
                c.get(A);
                c.get(B);
                if kind == 14 {
                    c.constant(u32::MAX);
                    c.op(0x73);
                }
                c.op(match kind {
                    0 | 8 | 14 => 0x71,
                    1 | 9 => 0x73,
                    _ => 0x72,
                });
            }
        }
        c.set(RESULT);
        if flags {
            c.nz(0xe0000000);
            c.get(FLAGS);
            c.get(CARRY);
            c.constant(29);
            c.op(0x74);
            c.op(0x72);
            c.set(FLAGS);
        }
    }
    if !(8..12).contains(&kind) {
        c.get(RESULT);
        c.set(reg(op >> 12 & 15));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn exceptional_and_variable_bus_paths_are_interpreter_exits() {
        for op in [
            0xe12fff10, 0xe10f0000, 0xe129f000, 0xe59ff000, 0xe49f0004, 0xe8bd8000, 0xef000000,
            0xe0810213, 0xe0a00001, 0xe1a0f000, 0xe1000090, 0xe0800192, 0xf1a00000, 0xe18010d0,
        ] {
            assert!(!supported(0x03000100, op), "{op:08x}");
        }
        assert!(!supported(0x08000100, 0xe1a00000));
        assert!(!supported(0x03000102, 0xe1a00000));
        assert!(!supported(0x03000000, 0xeafffffe_u32.wrapping_sub(1)));
    }
    #[test]
    fn discovery_follows_both_branch_paths_and_remains_bounded() {
        let words = [0xe2800001, 0xe3500020, 0x3afffffc, 0xe5900000];
        let region = Region::discover(0x03000100, |pc| {
            words
                .get(((pc.wrapping_sub(0x03000100)) / 4) as usize)
                .copied()
                .unwrap_or(0xe12fff1e)
        })
        .unwrap();
        assert_eq!(region.words.len(), 4);
        assert_eq!(
            Region::discover_with_ram(0x03000100, false, |pc| {
                words
                    .get(((pc.wrapping_sub(0x03000100)) / 4) as usize)
                    .copied()
                    .unwrap_or(0xe12fff1e)
            })
            .unwrap()
            .words
            .len(),
            3
        );
        assert!(region.compile().len() > 100);
        let bounded = Region::discover(0x03000100, |_| 0xe1a00000).unwrap();
        assert_eq!(bounded.words.len(), 128);
        assert!(Region::discover(0x03000100, |_| 0xe12fff1e).is_none());
    }
}

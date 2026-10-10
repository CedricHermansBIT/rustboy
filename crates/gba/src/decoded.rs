//! Bounded decoded Thumb blocks, never cached instruction results or bus time.
//! Real opcode fetches validate each cached operation, including mutable RAM.
//! This experiment is opt-in until end-to-end measurements justify production.
#[derive(Clone, Copy, Default)]
pub(crate) enum Operation {
    Shift {
        rd: u8,
        rs: u8,
        kind: u8,
        amount: u8,
    },
    AddSub {
        rd: u8,
        rs: u8,
        operand: u8,
        immediate: bool,
        subtract: bool,
    },
    Immediate {
        rd: u8,
        kind: u8,
        value: u8,
    },
    Alu {
        rd: u8,
        rs: u8,
        kind: u8,
    },
    Memory {
        rd: u8,
        rs: u8,
        offset: u8,
        byte: bool,
        load: bool,
    },
    Halfword {
        rd: u8,
        rs: u8,
        offset: u8,
        load: bool,
    },
    Conditional {
        condition: u8,
        offset: i16,
    },
    #[default]
    Reference,
}
#[derive(Clone, Copy, Default)]
pub(crate) struct Instruction {
    pub raw: u16,
    pub operation: Operation,
}
impl Instruction {
    pub fn decode(raw: u16) -> Self {
        let rd = (raw & 7) as u8;
        let rs = (raw >> 3 & 7) as u8;
        let operation = match raw >> 13 {
            0 if raw & 0x1800 == 0x1800 => Operation::AddSub {
                rd,
                rs,
                operand: (raw >> 6 & 7) as u8,
                immediate: raw & 0x400 != 0,
                subtract: raw & 0x200 != 0,
            },
            0 => Operation::Shift {
                rd,
                rs,
                kind: (raw >> 11 & 3) as u8,
                amount: (raw >> 6 & 31) as u8,
            },
            1 => Operation::Immediate {
                rd: (raw >> 8 & 7) as u8,
                kind: (raw >> 11 & 3) as u8,
                value: raw as u8,
            },
            2 if raw & 0xfc00 == 0x4000 => Operation::Alu {
                rd,
                rs,
                kind: (raw >> 6 & 15) as u8,
            },
            3 => Operation::Memory {
                rd,
                rs,
                offset: ((raw >> 6 & 31) * if raw & 0x1000 != 0 { 1 } else { 4 }) as u8,
                byte: raw & 0x1000 != 0,
                load: raw & 0x800 != 0,
            },
            4 if raw & 0x1000 == 0 => Operation::Halfword {
                rd,
                rs,
                offset: ((raw >> 6 & 31) * 2) as u8,
                load: raw & 0x800 != 0,
            },
            6 if raw & 0x1000 != 0 && raw >> 8 & 15 < 14 => Operation::Conditional {
                condition: (raw >> 8 & 15) as u8,
                offset: i16::from(raw as u8 as i8) * 2,
            },
            _ => Operation::Reference,
        };
        Self { raw, operation }
    }
    fn terminal(self) -> bool {
        matches!(
            self.operation,
            Operation::Conditional { .. } | Operation::Reference
        )
    }
}

#[cfg(feature = "decoded-blocks")]
const BLOCK_LENGTH: usize = 16;
#[cfg(feature = "decoded-blocks")]
const CACHE_LENGTH: usize = 64;
#[cfg(feature = "decoded-blocks")]
#[derive(Clone, Copy, Default)]
struct Block {
    address: u32,
    count: usize,
    instructions: [Instruction; BLOCK_LENGTH],
}
#[cfg(feature = "decoded-blocks")]
pub(crate) struct Cache {
    blocks: [Block; CACHE_LENGTH],
}
#[cfg(feature = "decoded-blocks")]
impl Default for Cache {
    fn default() -> Self {
        Self {
            blocks: [Block::default(); CACHE_LENGTH],
        }
    }
}
#[cfg(feature = "decoded-blocks")]
#[derive(Clone, Copy)]
pub(crate) struct Cursor {
    slot: usize,
    index: usize,
    address: u32,
}

#[cfg(feature = "decoded-blocks")]
impl Cache {
    fn prepare(&mut self, bus: &crate::bus::Bus, address: u32) -> Option<Cursor> {
        if address & 1 != 0 || !matches!(address >> 24, 2 | 3 | 8..=12) {
            return None;
        }
        let slot = ((address >> 1) as usize ^ (address >> 8) as usize) & (CACHE_LENGTH - 1);
        let block = &mut self.blocks[slot];
        if block.count == 0 || block.address != address {
            block.address = address;
            block.count = 0;
            for index in 0..BLOCK_LENGTH {
                let pc = address.wrapping_add(index as u32 * 2);
                if pc >> 24 != address >> 24 {
                    break;
                }
                let instruction = Instruction::decode(bus.peek16(pc));
                block.instructions[index] = instruction;
                block.count += 1;
                if instruction.terminal() {
                    break;
                }
            }
        }
        Some(Cursor {
            slot,
            index: 0,
            address,
        })
    }
}

#[cfg(feature = "decoded-blocks")]
impl crate::machine::GameBoyAdvance {
    pub(crate) fn step_decoded_slice(
        &mut self,
        cursor: &mut Option<Cursor>,
        address: u32,
        thumb: bool,
    ) {
        if !thumb {
            *cursor = None;
            self.cpu.step(&mut self.bus);
            return;
        }
        if cursor.is_none_or(|cursor| cursor.address != address) {
            *cursor = self.decoded.prepare(&self.bus, address);
        }
        let Some(current) = *cursor else {
            self.cpu.step(&mut self.bus);
            return;
        };
        let instruction = self.decoded.blocks[current.slot].instructions[current.index];
        let matched = self.cpu.step_decoded_thumb(&mut self.bus, instruction);
        if !matched {
            // A CPU/DMA/remapping write changed executable RAM. The fetched
            // opcode already ran via reference decoding; invalidate its block.
            self.decoded.blocks[current.slot].count = 0;
        }
        if !matched
            || instruction.terminal()
            || current.index + 1 == self.decoded.blocks[current.slot].count
        {
            *cursor = None;
        } else {
            *cursor = Some(Cursor {
                slot: current.slot,
                index: current.index + 1,
                address: address.wrapping_add(2),
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cpu::{Cpu, Memory, C, N, V, Z};

    #[derive(Clone, Debug, PartialEq, Eq)]
    struct TraceBus {
        opcode: u16,
        data: [u8; 256],
        events: Vec<(u8, u32, u64)>,
    }
    impl TraceBus {
        fn new(opcode: u16) -> Self {
            Self {
                opcode,
                data: std::array::from_fn(|index| (index as u8).wrapping_mul(47)),
                events: Vec::new(),
            }
        }
    }
    impl Memory for TraceBus {
        fn instruction_context(&mut self, address: u32, thumb: bool) {
            self.events.push((0, address, u64::from(thumb)));
        }
        fn fetch16(&mut self, address: u32, sequential: bool) -> u16 {
            self.events.push((
                1,
                address,
                u64::from(self.opcode) | u64::from(sequential) << 32,
            ));
            self.opcode
        }
        fn refill_complete(&mut self, address: u32) {
            self.events.push((9, address, 0));
        }
        fn finish_instruction(&mut self) -> bool {
            self.events.push((10, 0, 0));
            true
        }
        fn read8(&mut self, address: u32) -> u8 {
            let value = self.data[address as usize & 255];
            self.events.push((2, address, u64::from(value)));
            value
        }
        fn read16(&mut self, address: u32, sequential: bool) -> u16 {
            let value = u16::from_le_bytes(std::array::from_fn(|index| {
                self.data[address.wrapping_add(index as u32) as usize & 255]
            }));
            self.events
                .push((3, address, u64::from(value) | u64::from(sequential) << 32));
            value
        }
        fn read32(&mut self, address: u32, sequential: bool) -> u32 {
            let value = u32::from_le_bytes(std::array::from_fn(|index| {
                self.data[address.wrapping_add(index as u32) as usize & 255]
            }));
            self.events
                .push((4, address, u64::from(value) | u64::from(sequential) << 32));
            value
        }
        fn write8(&mut self, address: u32, value: u8) {
            self.events.push((5, address, u64::from(value)));
            self.data[address as usize & 255] = value;
        }
        fn write16(&mut self, address: u32, value: u16, sequential: bool) {
            self.events
                .push((6, address, u64::from(value) | u64::from(sequential) << 32));
            for (index, byte) in value.to_le_bytes().into_iter().enumerate() {
                self.data[address.wrapping_add(index as u32) as usize & 255] = byte;
            }
        }
        fn write32(&mut self, address: u32, value: u32, sequential: bool) {
            self.events
                .push((7, address, u64::from(value) | u64::from(sequential) << 32));
            for (index, byte) in value.to_le_bytes().into_iter().enumerate() {
                self.data[address.wrapping_add(index as u32) as usize & 255] = byte;
            }
        }
        fn internal(&mut self, clocks: u32) {
            self.events.push((8, clocks, 0));
        }
    }
    #[test]
    fn every_thumb_encoding_replays_registers_flags_and_bus_events() {
        for (seed, flags) in [
            (0, 0),
            (1, C),
            (0x7fffffff, Z | V),
            (0x80000000, N | C),
            (u32::MAX, N | Z | C | V),
        ] {
            for raw in 0..=u16::MAX {
                let instruction = Instruction::decode(raw);
                let mut reference = Cpu::default();
                reference.set_cpsr(0x3f | flags).unwrap();
                reference.r = std::array::from_fn(|index| seed.wrapping_add(index as u32 * 17));
                reference.r[15] = 0x08000000;
                let mut decoded = reference.clone();
                let mut reference_bus = TraceBus::new(raw);
                let mut decoded_bus = reference_bus.clone();
                reference.step(&mut reference_bus);
                assert!(decoded.step_decoded_thumb(&mut decoded_bus, instruction));
                assert_eq!(
                    decoded, reference,
                    "raw={raw:04x} seed={seed:08x} flags={flags:08x}"
                );
                assert_eq!(decoded_bus, reference_bus, "raw={raw:04x}");
                if !instruction.terminal() {
                    assert!(decoded.thumb());
                    assert_eq!(decoded.r[15], 0x08000002);
                }
            }
        }
    }
    #[test]
    fn a_changed_fetched_opcode_executes_reference_instead_of_stale_decoding() {
        for (old, new) in [
            (0x2001, 0x2007),
            (0x8008, 0x8808),
            (0x4008, 0x4348),
            (0xd000, 0xdf00),
            (0x2000, 0x4700),
        ] {
            let mut reference = Cpu::default();
            reference.set_cpsr(0x3f).unwrap();
            reference.r[0] = 0x03000001;
            reference.r[1] = 7;
            reference.r[15] = 0x08000000;
            let mut decoded = reference.clone();
            let mut reference_bus = TraceBus::new(new);
            let mut decoded_bus = reference_bus.clone();
            reference.step(&mut reference_bus);
            assert!(!decoded.step_decoded_thumb(&mut decoded_bus, Instruction::decode(old)));
            assert_eq!(decoded, reference);
            assert_eq!(decoded_bus, reference_bus);
        }
    }
    #[cfg(feature = "decoded-blocks")]
    #[test]
    fn executing_ram_block_checks_a_self_modified_following_opcode() {
        use crate::{GameBoyAdvance, FRAME_CYCLES};
        use rustboy_emulator_api::Emulator;
        let mut sliced = GameBoyAdvance::new(vec![0; 192], None).unwrap();
        sliced.cpu.set_cpsr(0x3f).unwrap();
        sliced.cpu.r[15] = 0x03000000;
        sliced.cpu.r[0] = 0x2207;
        sliced.cpu.r[1] = 0x03000002;
        for (index, opcode) in [0x8008, 0x2201, 0xe7fc].into_iter().enumerate() {
            sliced
                .bus
                .write16(0x03000000 + index as u32 * 2, opcode, false);
        }
        sliced.bus.take_cycles();
        let state = sliced.export_state();
        let mut reference = GameBoyAdvance::new(vec![0; 192], None).unwrap();
        reference.import_state(&state).unwrap();
        for budget in [1, 7, 511, FRAME_CYCLES + 1] {
            let clocks = sliced.run(budget);
            let start = reference.clocks;
            while reference.clocks.wrapping_sub(start) < budget {
                assert_ne!(reference.step(), 0);
            }
            assert_eq!(clocks, reference.clocks.wrapping_sub(start));
            assert_eq!(sliced.export_state(), reference.export_state());
        }
        assert_eq!(sliced.cpu.r[2], 7);
        assert!(sliced.decoded.blocks.iter().any(|block| block.count != 0));
        let saved = sliced.export_state();
        sliced.import_state(&saved).unwrap();
        assert!(sliced.decoded.blocks.iter().all(|block| block.count == 0));
    }
}

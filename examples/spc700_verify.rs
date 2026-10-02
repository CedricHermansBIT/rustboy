//! Verify our CPU against externally provided binary instruction vectors.
use rustboy_snes_apu::{spc700::Spc700, SpcRam};
struct Input<'a>(&'a [u8]);
impl Input<'_> {
    fn byte(&mut self) -> u8 {
        let value = self.0[0];
        self.0 = &self.0[1..];
        value
    }
    fn word(&mut self) -> u16 {
        u16::from_le_bytes([self.byte(), self.byte()])
    }
    fn state(&mut self) -> (Spc700, Vec<(u16, u8)>) {
        let cpu = Spc700 {
            pc: self.word(),
            a: self.byte(),
            x: self.byte(),
            y: self.byte(),
            sp: self.byte(),
            psw: self.byte(),
            halted: false,
        };
        let count = self.word();
        (
            cpu,
            (0..count).map(|_| (self.word(), self.byte())).collect(),
        )
    }
}
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let bytes = std::fs::read(
        std::env::args()
            .nth(1)
            .ok_or("Usage: spc700_verify vectors.bin")?,
    )?;
    let mut input = Input(&bytes);
    let mut ram = SpcRam::default();
    let mut tested = 0;
    let mut failures = 0;
    let mut last_failed_opcode = None;
    while !input.0.is_empty() {
        let opcode = input.byte();
        let index = input.word();
        let (mut cpu, initial) = input.state();
        let (expected, memory) = input.state();
        let cycles = input.byte();
        for &(address, value) in &initial {
            ram.write(address, value);
        }
        let mut actual_cycles = cpu.step(&mut ram);
        // These vectors observe SLEEP/STOP for seven clocks: the three-clock
        // instruction followed by two idle bus pairs. Check the idle state too.
        if cpu.halted {
            actual_cycles += cpu.step(&mut ram) + cpu.step(&mut ram);
        }
        // The vector register format has no halted field.
        cpu.halted = false;
        let failed = cpu != expected
            || actual_cycles != u32::from(cycles)
            || memory
                .iter()
                .any(|&(address, value)| ram.read(address) != value);
        if failed {
            failures += 1;
            if last_failed_opcode != Some(opcode) {
                eprintln!("{opcode:02X} #{index}: CPU {cpu:?} expected {expected:?}; clocks {actual_cycles}/{cycles}; memory {:?}", memory.iter().filter(|&&(address, value)| ram.read(address) != value).map(|&(address, value)| (address, ram.read(address), value)).collect::<Vec<_>>());
            }
            last_failed_opcode = Some(opcode);
        }
        for &(address, _) in initial.iter().chain(&memory) {
            ram.write(address, 0);
        }
        tested += 1;
    }
    println!("SPC700 instruction vectors: {tested} tested, {failures} failed");
    if failures != 0 {
        return Err("SPC700 conformance failures".into());
    }
    Ok(())
}

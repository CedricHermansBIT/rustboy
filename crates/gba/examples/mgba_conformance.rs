//! Runs independently authored mGBA hardware tests, never an mGBA emulator.
//! Descriptor ABI comes from mgba-emu/suite/include/suite.h at e6942030.
//! ROMs are supplied externally; no logo, firmware or fixture is embedded.
use rustboy_gba::{cpu::Memory, GameBoyAdvance, CLOCK_HZ};
use std::{error::Error, fs};
const TARGETS: &[(&str, &str)] = &[
    ("memory", "Memory tests"),
    ("io", "I/O read tests"),
    ("timing", "Timing tests"),
    ("timers", "Timer count-up tests"),
    ("timer-irq", "Timer IRQ tests"),
    ("dma", "DMA tests"),
    ("bios-math", "BIOS math tests"),
    ("sio-read", "SIO register R/W tests"),
    ("sio-timing", "SIO timing tests"),
];
fn word(bytes: &[u8], offset: usize) -> Option<u32> {
    Some(u32::from_le_bytes(
        bytes.get(offset..offset + 4)?.try_into().ok()?,
    ))
}
fn ram(address: u32) -> bool {
    matches!(address >> 24, 2 | 3)
}
fn descriptor(bytes: &[u8], name: &str) -> Result<[u32; 7], Box<dyn Error>> {
    let mut needle = name.as_bytes().to_vec();
    needle.push(0);
    let string = bytes
        .windows(needle.len())
        .position(|part| part == needle)
        .ok_or("Suite name not found in independent ROM")? as u32
        + 0x08000000;
    let mut found = Vec::new();
    for offset in (0..bytes.len().saturating_sub(28)).step_by(4) {
        if word(bytes, offset) != Some(string) {
            continue;
        }
        let values = std::array::from_fn(|i| word(bytes, offset + i * 4).unwrap());
        if values[1] >> 24 == 8
            && ram(values[5])
            && ram(values[6])
            && values[4] > 0
            && values[4] < 4096
        {
            found.push(values);
        }
    }
    if found.len() != 1 {
        return Err("Independent suite descriptor is missing or ambiguous".into());
    }
    Ok(found[0])
}
fn execute(bytes: &[u8], descriptor: [u32; 7]) -> Result<(u32, u32, Vec<String>), Box<dyn Error>> {
    let mut m = GameBoyAdvance::new(bytes.to_vec(), None)?;
    m.run(CLOCK_HZ);
    // Enter only after the SDK initialized the IRQ dispatcher and a BIOS wait
    // has resumed. This retains the fixture's original startup/interrupt code.
    for _ in 0..1_000_000 {
        let before = m.cpu.instructions;
        m.step();
        if !m.bus.halted
            && m.cpu.instructions != before
            && m.cpu.r[15] >> 24 == 8
            && m.cpu.cpsr() & 31 == 31
        {
            break;
        }
        if m.fault.is_some() {
            return Err(format!("Independent SDK startup fault: {:?}", m.fault).into());
        }
    }
    if m.bus.halted || m.cpu.cpsr() & 31 != 31 {
        return Err("Independent SDK startup did not reach the main loop".into());
    }
    let sentinel = 0x03007c00;
    m.bus.write32(sentinel, 0xeafffffe, false);
    m.bus.take_cycles();
    m.cpu.r[14] = sentinel;
    let function = descriptor[1];
    m.cpu
        .set_cpsr((m.cpu.cpsr() & !0x20) | ((function & 1) << 5))?;
    m.cpu.r[15] = function & !1;
    let start = m.clocks;
    for _ in 0..200_000_000u32 {
        if m.cpu.r[15] == sentinel && !m.cpu.thumb() {
            let passes = m.bus.peek32(descriptor[5]);
            let total = m.bus.peek32(descriptor[6]);
            if total == 0 || passes > total {
                return Err("Independent test returned invalid result counters".into());
            }
            let failed = failed_names(&mut m, descriptor, sentinel)?;
            return Ok((passes, total, failed));
        }
        if m.step() == 0 || m.fault.is_some() || m.clocks - start > CLOCK_HZ * 60 {
            return Err(format!(
                "Independent test did not return: PC={:08X}, fault={:?}",
                m.cpu.r[15], m.fault
            )
            .into());
        }
    }
    Err("Independent test instruction limit reached".into())
}
fn failed_names(
    m: &mut GameBoyAdvance,
    descriptor: [u32; 7],
    sentinel: u32,
) -> Result<Vec<String>, Box<dyn Error>> {
    let mut failed = Vec::new();
    for offset in (0..descriptor[4]).step_by(64) {
        m.cpu.r[0] = 0x02030000;
        m.cpu.r[1] = 0x02030400;
        m.cpu.r[2] = 64;
        m.cpu.r[3] = offset;
        m.cpu.r[14] = sentinel;
        m.cpu
            .set_cpsr((m.cpu.cpsr() & !0x20) | ((descriptor[2] & 1) << 5))?;
        m.cpu.r[15] = descriptor[2] & !1;
        let mut returned = false;
        for _ in 0..100_000 {
            if m.cpu.r[15] == sentinel && !m.cpu.thumb() {
                returned = true;
                break;
            }
            if m.step() == 0 || m.fault.is_some() {
                break;
            }
        }
        if !returned || m.cpu.r[0] > 64 {
            return Err("Independent result listing did not return safely".into());
        }
        for index in 0..m.cpu.r[0] {
            if m.bus.peek8(0x02030400 + index) != 0 {
                continue;
            }
            let address = m.bus.peek32(0x02030000 + index * 4);
            if address >> 24 != 8 {
                return Err("Invalid independent test name pointer".into());
            }
            let bytes: Vec<u8> = (0..128)
                .map(|n| m.bus.peek8(address + n))
                .take_while(|v| *v != 0)
                .collect();
            failed.push(String::from_utf8_lossy(&bytes).replace(['\n', '\r', '\t'], " "));
        }
    }
    Ok(failed)
}
fn main() -> Result<(), Box<dyn Error>> {
    let mut args = std::env::args().skip(1);
    let bytes = fs::read(args.next().ok_or("usage: mgba_conformance <external suite.gba> [memory io timing timers timer-irq dma bios-math]")?)?;
    let selected: Vec<_> = args.collect();
    if selected
        .iter()
        .any(|name| !TARGETS.iter().any(|(key, _)| key == name))
    {
        return Err("Unknown independent suite".into());
    }
    let mut failures = 0;
    for &(key, name) in TARGETS
        .iter()
        .filter(|(key, _)| selected.is_empty() || selected.iter().any(|name| name == key))
    {
        match descriptor(&bytes, name).and_then(|d| execute(&bytes, d)) {
            Ok((passes, total, failed)) => {
                let passed = passes == total;
                println!(
                    "RESULT\tmgba-{key}\t{}\t{passes}/{total} independent assertions; failed groups: {}",
                    if passed { "PASS" } else { "FAIL" }, failed.join(" | ")
                );
                if !passed {
                    failures += 1;
                }
            }
            Err(error) => {
                failures += 1;
                println!("RESULT\tmgba-{key}\tFAIL\t{error}");
            }
        }
    }
    if failures != 0 {
        return Err(format!("{failures} independent mGBA suites failed").into());
    }
    Ok(())
}

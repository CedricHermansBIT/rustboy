//! Explicit opt-in runner for locally supplied, pinned independent test ROMs.
//! No ROM, Nintendo header/logo or external firmware is embedded or downloaded.
use rustboy_gba::GameBoyAdvance;
use std::{error::Error, path::PathBuf};

struct Suite {
    name: &'static str,
    file: &'static str,
    fingerprint: u64,
    idle: u32,
    result_register: usize,
}
const SUITES: &[Suite] = &[
    Suite {
        name: "arm",
        file: "arm/arm.gba",
        fingerprint: 0xfef473069e5ea6a4,
        idle: 0x08001ec4,
        result_register: 12,
    },
    Suite {
        name: "thumb",
        file: "thumb/thumb.gba",
        fingerprint: 0xbb12b76f55cb88f2,
        idle: 0x08000aac,
        result_register: 7,
    },
    Suite {
        name: "memory",
        file: "memory/memory.gba",
        fingerprint: 0xa3670661bc37ba36,
        idle: 0x080004c8,
        result_register: 12,
    },
    Suite {
        name: "bios",
        file: "bios/bios.gba",
        fingerprint: 0xe7f6370908a1b178,
        idle: 0x080003c0,
        result_register: 12,
    },
];
fn fingerprint(bytes: &[u8]) -> u64 {
    // Fixture identity check, not a cryptographic authenticity guarantee.
    bytes.iter().fold(0xcbf29ce484222325, |hash, byte| {
        (hash ^ u64::from(*byte)).wrapping_mul(0x100000001b3)
    })
}
fn main() -> Result<(), Box<dyn Error>> {
    let mut args = std::env::args().skip(1);
    let directory =
        PathBuf::from(args.next().ok_or(
            "usage: conformance <local jsmolka/gba-tests checkout> [arm thumb memory bios]",
        )?);
    let names: Vec<String> = args.collect();
    if names
        .iter()
        .any(|name| !SUITES.iter().any(|suite| suite.name == name))
    {
        return Err("Unknown suite; expected arm, thumb, memory or bios".into());
    }
    println!("Independent GBA tests: jsmolka/gba-tests a7113b67e63f83a9b321696ddd7042ccfad6c881");
    let mut failures = 0;
    for suite in SUITES
        .iter()
        .filter(|suite| names.is_empty() || names.iter().any(|name| name == suite.name))
    {
        let bytes = std::fs::read(directory.join(suite.file))?;
        if fingerprint(&bytes) != suite.fingerprint {
            return Err(format!(
                "{}: ROM differs from pinned fixture; refusing to interpret its result",
                suite.name
            )
            .into());
        }
        let mut machine = GameBoyAdvance::new(bytes, None)?;
        for _ in 0..10 {
            if machine.run_frame() == 0 {
                break;
            }
        }
        let pc = machine.cpu.r[15];
        let result = machine.cpu.r[suite.result_register];
        // Zero alone is not success: require the source-verified terminal loop,
        // ARM state, expected display and no unsupported firmware exception.
        let terminal = pc == suite.idle
            && machine.bus.peek32(pc) == 0xeafffffe
            && machine.cpu.cpsr() & 0x20 == 0
            && machine.bus.io16(0) == 0x0404
            && machine.fault.is_none();
        if terminal && result == 0 {
            println!(
                "PASS {}: verified terminal loop, result r{}=0",
                suite.name, suite.result_register
            );
        } else {
            failures += 1;
            println!(
                "FAIL {}: terminal={terminal}, PC={pc:08X}, result r{}={result}, fault={:?}",
                suite.name, suite.result_register, machine.fault
            );
        }
    }
    if failures != 0 {
        return Err(format!("{failures} independent GBA suite(s) failed").into());
    }
    Ok(())
}

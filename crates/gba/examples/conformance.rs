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
    display: u16,
}
const SUITES: &[Suite] = &[
    Suite {
        name: "arm",
        file: "arm/arm.gba",
        fingerprint: 0xfef473069e5ea6a4,
        idle: 0x08001ec4,
        result_register: 12,
        display: 0x0404,
    },
    Suite {
        name: "thumb",
        file: "thumb/thumb.gba",
        fingerprint: 0xbb12b76f55cb88f2,
        idle: 0x08000aac,
        result_register: 7,
        display: 0x0404,
    },
    Suite {
        name: "memory",
        file: "memory/memory.gba",
        fingerprint: 0xa3670661bc37ba36,
        idle: 0x080004c8,
        result_register: 12,
        display: 0x0404,
    },
    Suite {
        name: "bios",
        file: "bios/bios.gba",
        fingerprint: 0xe7f6370908a1b178,
        idle: 0x080003c0,
        result_register: 12,
        display: 0x0404,
    },
    Suite {
        name: "sram",
        file: "save/sram.gba",
        fingerprint: 0xcf2735ca5210a726,
        idle: 0x08000470,
        result_register: 12,
        display: 0x0404,
    },
    Suite {
        name: "none",
        file: "save/none.gba",
        fingerprint: 0x32cb110a949a0690,
        idle: 0x080002a8,
        result_register: 12,
        display: 0x0404,
    },
    Suite {
        name: "flash64",
        file: "save/flash64.gba",
        fingerprint: 0xe855ab2b2f4af6c5,
        idle: 0x08000ac8,
        result_register: 12,
        display: 0x0404,
    },
    Suite {
        name: "flash128",
        file: "save/flash128.gba",
        fingerprint: 0xad595d2fc0dcbeef,
        idle: 0x08000c4c,
        result_register: 12,
        display: 0x0404,
    },
    Suite {
        name: "stripes",
        file: "ppu/stripes.gba",
        fingerprint: 0xc53e66d5438e2341,
        idle: 0x08000140,
        result_register: 12,
        display: 0x0100,
    },
    Suite {
        name: "shades",
        file: "ppu/shades.gba",
        fingerprint: 0x74603f1c7a394633,
        idle: 0x0800015c,
        result_register: 12,
        display: 0x0100,
    },
    Suite {
        name: "hello",
        file: "ppu/hello.gba",
        fingerprint: 0xff89dddf3ef339fa,
        idle: 0x08000160,
        result_register: 12,
        display: 0x0404,
    },
];
fn fingerprint(bytes: &[u8]) -> u64 {
    // Fixture identity check, not a cryptographic authenticity guarantee.
    bytes.iter().fold(0xcbf29ce484222325, |hash, byte| {
        (hash ^ u64::from(*byte)).wrapping_mul(0x100000001b3)
    })
}
// Expected images are derived from the independent assembly, never captured
// from RustBoy's renderer. Only the documented RGB555 display expansion is used.
fn expected_pixels(
    name: &str,
    directory: &std::path::Path,
) -> Result<Option<Vec<u8>>, Box<dyn Error>> {
    if !matches!(name, "stripes" | "shades" | "hello") {
        return Ok(None);
    }
    let glyphs = if name == "hello" {
        let source = std::fs::read_to_string(directory.join("lib/glyphs.asm"))?;
        source
            .split(|c: char| c.is_whitespace() || c == ',')
            .filter_map(|word| word.strip_prefix("0x"))
            .map(|word| u32::from_str_radix(word, 16))
            .collect::<Result<Vec<_>, _>>()?
    } else {
        Vec::new()
    };
    let mut pixels = Vec::with_capacity(240 * 160 * 4);
    for y in 0..160 {
        for x in 0..240 {
            let color = match name {
                "stripes" => {
                    if x % 16 < 8 {
                        0x560b
                    } else {
                        0x6290
                    }
                }
                "shades" => ((x / 16) * 2) << 10,
                "hello" if (72..168).contains(&x) && (76..84).contains(&y) => {
                    let character = b"Hello world!"[(x - 72) / 8];
                    let word = glyphs
                        .get(usize::from(character - 32) * 2 + (y - 76) / 4)
                        .ok_or("Missing source glyph")?;
                    if word >> (((y - 76) % 4) * 8 + (x - 72) % 8) & 1 != 0 {
                        0x7fff
                    } else {
                        0
                    }
                }
                _ => 0,
            };
            for shift in [0, 5, 10] {
                let component = (color >> shift) & 31;
                pixels.push(((component << 3) | (component >> 2)) as u8);
            }
            pixels.push(255);
        }
    }
    Ok(Some(pixels))
}
fn main() -> Result<(), Box<dyn Error>> {
    let mut args = std::env::args().skip(1);
    let directory =
        PathBuf::from(args.next().ok_or(
            "usage: conformance <local jsmolka/gba-tests checkout> [arm thumb memory bios sram none flash64 flash128 stripes shades hello]",
        )?);
    let names: Vec<String> = args.collect();
    if names
        .iter()
        .any(|name| !SUITES.iter().any(|suite| suite.name == name))
    {
        return Err("Unknown independent suite name".into());
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
            && machine.bus.io16(0) == suite.display
            && machine.fault.is_none();
        let pixels = expected_pixels(suite.name, &directory)?;
        let image_matches = pixels
            .as_ref()
            .is_none_or(|expected| machine.video_frame().pixels == expected.as_slice());
        let passed = terminal && image_matches && (pixels.is_some() || result == 0);
        println!("RESULT\t{}\t{}\tPC={pc:08X},r{}={result},terminal={terminal},image={image_matches},fault={:?}", suite.name, if passed { "PASS" } else { "FAIL" }, suite.result_register, machine.fault);
        if passed {
            println!(
                "PASS {}: verified terminal loop and source-derived result/image, r{}=0",
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

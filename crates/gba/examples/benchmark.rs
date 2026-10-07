//! Fixed-emulated-work benchmark with identical saved-state starts and replay checks.
//! Local cartridges/state artifacts only; no game/firmware is bundled.
use rustboy_emulator_api::Emulator;
use rustboy_gba::{GameBoyAdvance, CLOCK_HZ, FRAME_CYCLES};
use std::{error::Error, io::Write, time::Instant};

fn hash(bytes: &[u8]) -> u64 {
    bytes.iter().fold(0xcbf29ce484222325, |h, b| {
        (h ^ u64::from(*b)).wrapping_mul(0x100000001b3)
    })
}
fn main() -> Result<(), Box<dyn Error>> {
    let mut args = std::env::args().skip(1);
    let path = args
        .next()
        .ok_or("usage: benchmark <rom.gba> [warmup_frames=300] [measure_frames=120] [trials=5]")?;
    let warmup: u64 = args.next().map(|s| s.parse()).transpose()?.unwrap_or(300);
    let frames: u64 = args.next().map(|s| s.parse()).transpose()?.unwrap_or(120);
    let trials: usize = args.next().map(|s| s.parse()).transpose()?.unwrap_or(5);
    if args.next().is_some()
        || warmup > 36000
        || frames == 0
        || frames > 36000
        || trials == 0
        || trials > 100
    {
        return Err("Use warmup <=36000, measure 1..36000 frames, trials 1..100".into());
    }
    let rom = std::fs::read(path)?;
    let rom_hash = hash(&rom);
    let mut machine = GameBoyAdvance::new(rom, None)?;
    if let Some(path) = std::env::var_os("GBA_BENCH_STATE") {
        machine.import_state(&std::fs::read(path)?)?;
    } else {
        machine.run(FRAME_CYCLES * warmup);
    }
    if machine.fault.is_some() || machine.paused() {
        return Err(format!("Cannot benchmark a stopped machine: {:?}", machine.fault).into());
    }
    let baseline = machine.export_state();
    if let Some(path) = std::env::var_os("GBA_BENCH_SAVE_STATE") {
        std::fs::OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(path)?
            .write_all(&baseline)?;
    }
    println!(
        "benchmark rom={rom_hash:016x} baseline={:016x} measure_frames={frames} trials={trials}",
        hash(&baseline)
    );
    let budget = FRAME_CYCLES * frames;
    let mut times = Vec::with_capacity(trials);
    let mut expected = None;
    for trial in 0..trials {
        machine.import_state(&baseline)?;
        let instructions = machine.cpu.instructions;
        let start = Instant::now();
        let clocks = machine.run(budget);
        let seconds = start.elapsed().as_secs_f64();
        if clocks < budget || machine.fault.is_some() {
            return Err(format!(
                "Benchmark stopped before its workload completed: {:?}",
                machine.fault
            )
            .into());
        }
        let result = machine.export_state();
        if expected
            .as_ref()
            .is_some_and(|expected| expected != &result)
        {
            return Err("Identical starting states did not replay identical ending states".into());
        }
        let result_hash = hash(&result);
        expected = Some(result);
        times.push(seconds);
        println!("trial={} seconds={seconds:.6} speed={:.3} clocks={clocks} instructions={} result={result_hash:016x}", trial + 1,
            clocks as f64 / CLOCK_HZ as f64 / seconds, machine.cpu.instructions - instructions);
    }
    times.sort_by(f64::total_cmp);
    let median = times[times.len() / 2];
    println!("median_seconds={median:.6} median_speed={:.3} (native fixed workload, not browser gameplay)", budget as f64 / CLOCK_HZ as f64 / median);
    Ok(())
}

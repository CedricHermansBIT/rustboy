//! Run locally provided SGB firmware and a captured cartridge score in our APU.
use rustboy_snes_apu::firmware::load_sgb_firmware;
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<_> = std::env::args().collect();
    if args.len() != 3 {
        return Err("Usage: spc700_sgb_audio SGB-FIRMWARE SPC-RAM-DUMP".into());
    }
    let mut apu = load_sgb_firmware(&std::fs::read(&args[1])?)?;
    let ram = std::fs::read(&args[2])?;
    if ram.len() != 65536 {
        return Err("Expected 64 KiB RAM dump".into());
    }
    apu.bus.ram.write_wrapping(0x2b00, &ram[0x2b00..0x4b00]);
    for frame in 0..600 {
        if frame == 30 {
            apu.bus.input[3] = 0x0c;
        }
        if frame == 60 {
            apu.bus.input = [1, 0, 0, 0];
        }
        apu.run(17067);
        let samples = apu.drain_samples();
        if frame % 60 == 0 {
            let energy: f64 = samples
                .iter()
                .flatten()
                .map(|&s| f64::from(s).powi(2))
                .sum();
            println!("frame={frame} pc={:04X} ports={:02X?} flg={:02X} kon={:02X} source={} pitch={} env={} rms={:.1}", apu.cpu.pc,apu.bus.output,apu.bus.dsp.read(0x6c),apu.bus.dsp.read(0x4c),apu.bus.dsp.read(0x04),u16::from_le_bytes([apu.bus.dsp.read(2),apu.bus.dsp.read(3)]),apu.bus.dsp.read(8),(energy/samples.len().max(1) as f64/2.0).sqrt());
            println!(
                "master={:02X},{:02X} envelopes={:?}",
                apu.bus.dsp.read(0x0c),
                apu.bus.dsp.read(0x1c),
                (0..8)
                    .map(|i| apu.bus.dsp.read(i * 16 + 8))
                    .collect::<Vec<_>>()
            );
            println!(
                "echo={:02X} delay={} dir={:02X} score={:02X?} globals={:02X?}",
                apu.bus.dsp.read(0x6d),
                apu.bus.dsp.read(0x7d),
                apu.bus.dsp.read(0x5d),
                &apu.bus.ram.bytes()[0x2b00..0x2b10],
                &apu.bus.ram.bytes()[0..0x30]
            );
            println!(
                "DSP={:02X?}",
                (0..128).map(|r| apu.bus.dsp.read(r)).collect::<Vec<_>>()
            );
        }
    }
    Ok(())
}

//! Local-only two-cartridge cable diagnostic. ROMs are never bundled.
//! Inputs are frame:hex-key-mask timelines, independently supplied per player.
use rustboy_emulator_api::Emulator;
use rustboy_gba::{GameBoyAdvance, FRAME_CYCLES};
use std::{error::Error, fs, path::Path};

fn inputs(text: &str) -> Result<Vec<(u64, u16)>, Box<dyn Error>> {
    if text == "-" {
        return Ok(Vec::new());
    }
    text.split(',')
        .map(|entry| {
            let (frame, keys) = entry.split_once(':').ok_or("Expected frame:hexkeys")?;
            Ok((frame.parse()?, u16::from_str_radix(keys, 16)?))
        })
        .collect()
}
fn screenshot(m: &GameBoyAdvance, path: &Path) -> Result<(), Box<dyn Error>> {
    let mut png = png::Encoder::new(fs::File::create(path)?, 240, 160);
    png.set_color(png::ColorType::Rgba);
    png.set_depth(png::BitDepth::Eight);
    png.write_header()?
        .write_image_data(m.video_frame().pixels)?;
    Ok(())
}
fn peers(m: &mut [GameBoyAdvance; 2]) {
    let states = m
        .each_ref()
        .map(|g| (g.bus.serial.mode(), g.bus.serial.ready(), g.bus.serial.so()));
    for p in 0..2 {
        let (mode, ready, so) = states[1 - p];
        m[p].link_set_peer(mode, ready, so);
    }
}
fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<_> = std::env::args().skip(1).collect();
    if args.len() != 5 {
        return Err(
            "usage: link_gameplay_probe ROM frames timeline1 timeline2 output-directory".into(),
        );
    }
    let rom = fs::read(&args[0])?;
    let frames: u64 = args[1].parse()?;
    let timelines = [
        inputs(&args[2])?,
        inputs(if args[3] == "download" { "-" } else { &args[3] })?,
    ];
    let mut m = [
        GameBoyAdvance::new(rom.clone(), None)?,
        if args[3] == "download" {
            GameBoyAdvance::new_multiboot_client()?
        } else {
            GameBoyAdvance::new(rom, None)?
        },
    ];
    for (p, g) in m.iter_mut().enumerate() {
        g.link_attach(p as u8)?;
    }
    fs::create_dir_all(&args[4])?;
    let out = Path::new(&args[4]);
    let mut transfers = 0;
    let mut absent = 0;
    let mut cursor = [0usize; 2];
    for frame in 0..frames {
        for p in 0..2 {
            if timelines[p]
                .get(cursor[p])
                .is_some_and(|(f, _)| *f == frame)
            {
                m[p].bus.set_keys(timelines[p][cursor[p]].1);
                cursor[p] += 1;
            }
            m[p].link_set_limit((frame + 1) * FRAME_CYCLES)?;
        }
        let mut iterations = 0;
        while m.iter().any(|g| g.clocks < (frame + 1) * FRAME_CYCLES) {
            iterations += 1;
            if iterations > 2_000_000 {
                return Err(format!("Cable stalled at frame {frame}").into());
            }
            peers(&mut m);
            // The cable parent announces its next frame or transfer boundary;
            // the child follows to that exact guest clock before capture.
            // This is the same causal order as the browser coordinator and
            // avoids thousands of tiny CPU slices between cable events.
            let target = (frame + 1) * FRAME_CYCLES;
            m[0].run(target.saturating_sub(m[0].clocks));
            let clock = m[0].clocks;
            m[1].link_set_limit(clock)?;
            let delta = clock.saturating_sub(m[1].clocks);
            m[1].run(delta);
            for host in 0..2 {
                let Some(request) = m[host].link_request() else {
                    continue;
                };
                let child = 1 - host;
                // Clock-aligned peer capture, not whichever wall-clock instant
                // happens to deliver a transport packet.
                if m[child].clocks < m[host].clocks {
                    let delta = m[host].clocks - m[child].clocks;
                    m[child].run(delta);
                }
                peers(&mut m);
                if m[child].bus.serial.mode() != request.mode
                    || request.mode < 2 && !m[child].bus.serial.ready()
                {
                    m[host].link_absent(request.id)?;
                    absent += 1;
                    continue;
                }
                let value = m[child].link_receive(request.id, request.mode, request.baud)?;
                let mut words = [0; 2];
                words[host] = request.value;
                words[child] = value;
                m[host].link_supply(request.id, words)?;
                m[child].link_supply(request.id, words)?;
                transfers += 1;
                if transfers < 140 || transfers % 1000 == 0 {
                    println!("transfer={transfers} frame={frame} clock={} skew={} host={host} mode={} baud={} words={words:08x?} pc={:08x?}", m[host].clocks, m[child].clocks as i64-m[host].clocks as i64, request.mode, request.baud, [m[0].cpu.r[15],m[1].cpu.r[15]]);
                }
            }
            if let Some((p, fault)) = m
                .iter()
                .enumerate()
                .find_map(|(p, g)| g.fault.as_ref().map(|f| (p, f)))
            {
                return Err(format!("Player {p} fault at frame {frame}: {fault:?}").into());
            }
        }
        for g in &mut m {
            if g.drain_audio()
                .samples
                .iter()
                .any(|sample| !sample.is_finite())
            {
                return Err("Nonfinite linked audio".into());
            }
        }
        if frame % 60 == 59 || frame + 1 == frames {
            for (p, g) in m.iter().enumerate() {
                screenshot(g, &out.join(format!("p{}-f{:04}.png", p + 1, frame + 1)))?;
            }
            println!(
                "frame={} transfers={transfers} absent={absent} cnt={:04x?} pc={:08x?}",
                frame + 1,
                m.each_ref().map(|g| g.bus.peek16(0x04000128)),
                m.each_ref().map(|g| g.cpu.r[15])
            );
        }
    }
    for (p, g) in m.iter().enumerate() {
        let hash = g
            .video_frame()
            .pixels
            .iter()
            .fold(0xcbf29ce484222325u64, |hash, byte| {
                (hash ^ u64::from(*byte)).wrapping_mul(0x100000001b3)
            });
        println!("FRAMEHASH\t{}\t{hash:016x}", p + 1);
        fs::write(out.join(format!("p{}-state.bin", p + 1)), g.export_state())?;
        fs::write(out.join(format!("p{}-battery.sav", p + 1)), g.export_save())?;
    }
    println!("RESULT\tlink-gameplay\tOBSERVED\t{frames} frames; {transfers} real cable exchanges; {absent} absent-peer transfers; review screenshots for scene coverage");
    Ok(())
}

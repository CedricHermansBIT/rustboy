//! Original cartridge fixture: the BIOS uploads a small RAM program and the
//! cartridge-free endpoint executes it. Transfers use the real SIO scheduler.
use rustboy_emulator_api::Emulator;
use rustboy_gba::{cpu::Memory, GameBoyAdvance, FRAME_CYCLES};

fn peers(machines: &mut [GameBoyAdvance; 2]) {
    let states = machines
        .each_ref()
        .map(|m| (m.bus.serial.mode(), m.bus.serial.ready(), m.bus.serial.so()));
    for player in 0..2 {
        let (mode, ready, so) = states[1 - player];
        machines[player].link_set_peer(mode, ready, so);
    }
}
fn run(machines: &mut [GameBoyAdvance; 2], clocks: u64) -> usize {
    let target = machines[0].clocks + clocks;
    for machine in machines.iter_mut() {
        machine.link_set_limit(target).unwrap();
    }
    let mut exchanges = 0;
    for _ in 0..2_000_000 {
        peers(machines);
        for host in 0..2 {
            if let Some(request) = machines[host].link_request() {
                let child = 1 - host;
                let delta = machines[host].clocks.saturating_sub(machines[child].clocks);
                machines[child].run(delta);
                let value = machines[child]
                    .link_receive(request.id, request.mode, request.baud)
                    .unwrap();
                let mut words = [0; 2];
                words[host] = request.value;
                words[child] = value;
                for machine in machines.iter_mut() {
                    machine.link_supply(request.id, words).unwrap();
                }
                exchanges += 1;
            }
        }
        if machines.iter().all(|m| m.clocks >= target) {
            return exchanges;
        }
        let player = usize::from(machines[0].clocks > machines[1].clocks);
        machines[player].run(16);
        for machine in machines.iter() {
            assert!(machine.fault.is_none(), "{:?}", machine.fault);
        }
    }
    panic!("Original multiboot fixture stalled");
}
fn exchange(machines: &mut [GameBoyAdvance; 2], value: u16) -> u16 {
    machines[0].bus.write16(0x0400012a, value, false);
    machines[0].bus.write16(0x04000128, 0x2083, false);
    machines[0].bus.take_cycles();
    assert_eq!(run(machines, 20000), 1);
    machines[0].bus.peek16(0x04000122)
}

#[test]
fn arm_bios_upload_preserves_flags_and_restores_during_transfer() {
    upload(false);
}
#[test]
fn thumb_bios_upload_preserves_flags_and_restores_during_transfer() {
    upload(true);
}
fn upload(thumb: bool) {
    let mut cartridge = vec![0; 512];
    cartridge[0xb2] = 0x96;
    cartridge[..4].copy_from_slice(&0xeafffffeu32.to_le_bytes());
    for (index, word) in [0xef250000u32, 0xeafffffe].into_iter().enumerate() {
        cartridge[0x100 + index * 4..0x104 + index * 4].copy_from_slice(&word.to_le_bytes());
    }
    if thumb {
        cartridge[0x100..0x104].copy_from_slice(&[0x25, 0xdf, 0xfe, 0xe7]);
    }
    let returned = if thumb { 0x08000102 } else { 0x08000104 };
    let mut m = [
        GameBoyAdvance::new(cartridge, None).unwrap(),
        GameBoyAdvance::new_multiboot_client().unwrap(),
    ];
    for (player, machine) in m.iter_mut().enumerate() {
        machine.link_attach(player as u8).unwrap();
    }
    m[0].bus.write16(0x04000128, 0x2003, false);
    m[0].bus.take_cycles();
    run(&mut m, 20000);
    for _ in 0..15 {
        assert_eq!(exchange(&mut m, 0x6200), 0x7202);
    }
    assert_eq!(exchange(&mut m, 0x6102), 0x7202);
    for index in 0..96 {
        assert_eq!(
            exchange(&mut m, if index == 0xb2 / 2 { 0x96 } else { 0 }),
            ((96 - index) << 8) | 2
        );
    }
    assert_eq!(exchange(&mut m, 0x6200), 2);
    assert_eq!(exchange(&mut m, 0x6202), 0x7202);
    assert_eq!(exchange(&mut m, 0x6381), 0x7202);
    assert_eq!(exchange(&mut m, 0x6381), 0x735a);
    assert_eq!(exchange(&mut m, 0x6469), 0x735a);
    // Original RAM program: enable display, paint the backdrop green, loop.
    let mut payload = vec![0; 256];
    payload[..4].copy_from_slice(&0xea000006u32.to_le_bytes());
    let code = [
        0xe3a00301u32,
        0xe3a01000,
        0xe1c010b0,
        0xe3a00405,
        0xe3a01e3e,
        0xe1c010b0,
        0xeafffffe,
    ];
    for (index, word) in code.into_iter().enumerate() {
        payload[32 + index * 4..36 + index * 4].copy_from_slice(&word.to_le_bytes());
    }
    m[0].bus.ewram[0x10c0..0x11c0].copy_from_slice(&payload);
    let params = &mut m[0].bus.iwram[0x100..0x14c];
    params[0x19..0x1c].copy_from_slice(&[0x5a, 0xff, 0xff]);
    params[0x1c] = 0x81;
    params[0x1e] = 2;
    params[0x20..0x24].copy_from_slice(&0x020010c0u32.to_le_bytes());
    params[0x24..0x28].copy_from_slice(&0x020011c0u32.to_le_bytes());
    let flags = 0xa000001f | if thumb { 0x20 } else { 0 };
    m[0].cpu.set_cpsr(flags).unwrap();
    let caller_stack = m[0].cpu.r[13];
    m[0].cpu.r[0] = 0x03000100;
    m[0].cpu.r[1] = 1;
    m[0].cpu.r[14] = 0x08000180;
    m[0].cpu.r[15] = 0x08000100;
    let mut exchanges = 0;
    for _ in 0..200 {
        exchanges += run(&mut m, 20000);
        if exchanges >= 30
            && m[0].bus.serial.deadline() == u64::MAX
            && m[1].bus.serial.deadline() == u64::MAX
        {
            break;
        }
    }
    assert!(
        exchanges >= 30,
        "BIOS routine must use guest serial transfers"
    );
    let states = m.each_ref().map(|machine| machine.export_state());
    for player in 0..2 {
        m[player].import_state(&states[player]).unwrap();
        m[player].link_attach(player as u8).unwrap();
    }
    for _ in 0..400 {
        run(&mut m, 20000);
        if m[0].cpu.r[15] == returned {
            break;
        }
    }
    assert_eq!(m[0].cpu.r[15], returned);
    assert_eq!(
        m[0].cpu.cpsr(),
        flags,
        "SWI restores the caller CPSR including NZCV and instruction set"
    );
    assert_eq!(m[0].cpu.r[13], caller_stack);
    assert_eq!(m[0].cpu.r[0], 0, "BIOS verifies the receiver CRC");
    assert_eq!(m[0].cpu.r[14], 0x08000180, "SWI preserves the caller's LR");
    payload[4] = 3;
    payload[5] = 1;
    assert_eq!(&m[1].bus.ewram[0xc0..0x1c0], payload.as_slice());
    assert_eq!(
        m[1].cpu.r[15], 0x020000f8,
        "client executes the downloaded entry, with no cartridge"
    );
    run(&mut m, FRAME_CYCLES);
    assert_eq!(&m[1].video_frame().pixels[..4], &[0, 255, 0, 255]);
}

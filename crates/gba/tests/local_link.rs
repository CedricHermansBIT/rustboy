//! Original multiplayer fixture. Two devices each simulate both cartridges;
//! only frame inputs cross the host interface, never individual serial words.
use rustboy_emulator_api::{Button, Emulator};
use rustboy_gba::{GameBoyAdvance, SaveType, FRAME_CYCLES};
use std::collections::BTreeMap;

struct Arm {
    words: Vec<u32>,
    labels: BTreeMap<&'static str, usize>,
    branches: Vec<(usize, &'static str, u32)>,
    literals: Vec<(usize, u32, u32)>,
}
impl Arm {
    fn new() -> Self {
        Self {
            words: Vec::new(),
            labels: BTreeMap::new(),
            branches: Vec::new(),
            literals: Vec::new(),
        }
    }
    fn emit(&mut self, opcode: u32) {
        self.words.push(opcode);
    }
    fn label(&mut self, name: &'static str) {
        self.labels.insert(name, self.words.len());
    }
    fn branch(&mut self, label: &'static str, condition: u32) {
        self.branches.push((self.words.len(), label, condition));
        self.emit(0);
    }
    fn load(&mut self, register: u32, value: u32) {
        self.literals.push((self.words.len(), register, value));
        self.emit(0);
    }
    fn finish(mut self) -> Vec<u32> {
        for (at, label, condition) in self.branches {
            self.words[at] = condition << 28
                | 0x0a000000
                | (self.labels[label] as u32).wrapping_sub(at as u32 + 2) & 0xffffff;
        }
        for (at, register, value) in self.literals {
            let offset = (self.words.len() - at - 2) * 4;
            assert!(offset <= 0xfff);
            self.words[at] = 0xe59f0000 | register << 12 | offset as u32;
            self.words.push(value);
        }
        self.words
    }
}

fn cartridge() -> Vec<u8> {
    let mut a = Arm::new();
    a.load(0, 0x03007ffc);
    a.load(1, 0x08000800);
    a.emit(0xe5801000); // IRQ function.
    a.load(0, 0x04000200);
    a.emit(0xe3a01080);
    a.emit(0xe1c010b0); // IE serial.
    a.emit(0xe3a01001);
    a.emit(0xe1c010b8); // IME on.
    a.load(0, 0x04000100);
    a.load(1, 0x0080ff00);
    a.emit(0xe5801000); // Running timer.
    a.load(4, 0x04000120);
    a.load(5, 0x02000000);
    a.load(6, 0x040000d4);
    a.load(7, 0x84000001);
    a.load(8, 0x02000010);
    a.load(9, 0x0e000000);
    a.load(1, 0x6003);
    a.emit(0xe1c410b8); // Multiplayer, IRQ, 115200 baud.
    a.label("loop");
    a.emit(0xe1d411b0); // LDRH r1,[r4,#10h]: KEYINPUT.
    a.emit(0xe1c410ba); // SEND.
    a.emit(0xe5c91000); // Battery byte belongs to this endpoint only.
    a.emit(0xe1d420b8);
    a.emit(0xe3120004);
    a.branch("child", 1);
    a.label("ready");
    a.emit(0xe1d420b8);
    a.emit(0xe3120008);
    a.branch("ready", 0);
    a.load(1, 0x6083);
    a.emit(0xe1c410b8);
    a.branch("halt", 14);
    a.label("child");
    a.emit(0xe1d420b8);
    a.emit(0xe3120080);
    a.branch("child", 0);
    a.label("halt");
    a.emit(0xef020000); // SWI Halt, woken by real serial IRQ.
    a.emit(0xe1d420b8);
    a.emit(0xe3120080);
    a.branch("halt", 1);
    a.emit(0xe1d410b0);
    a.emit(0xe1c510b4); // Received parent keys to RAM+4.
    a.emit(0xe1d410b2);
    a.emit(0xe1c510b6); // Received child keys to RAM+6.
    a.emit(0xe5951000);
    a.emit(0xe2811001);
    a.emit(0xe5851000); // Completion counter.
    a.emit(0xe5864000);
    a.emit(0xe5868004);
    a.emit(0xe5867008); // Immediate DMA3 copies both words.
    a.branch("loop", 14);
    let mut rom = vec![0; 4096];
    rom[..4].copy_from_slice(&0xea00003eu32.to_le_bytes());
    rom[0xa0..0xac].copy_from_slice(b"LOCAL CABLE ");
    rom[0xb2] = 0x96;
    for (i, word) in a.finish().into_iter().enumerate() {
        rom[0x100 + i * 4..0x104 + i * 4].copy_from_slice(&word.to_le_bytes());
    }
    let mut irq = Arm::new();
    irq.load(0, 0x04000202);
    irq.emit(0xe3a01080);
    irq.emit(0xe1c010b0); // Clear IF serial.
    irq.load(0, 0x0200000c);
    irq.emit(0xe5901000);
    irq.emit(0xe2811001);
    irq.emit(0xe5801000);
    irq.emit(0xe12fff1e);
    for (i, word) in irq.finish().into_iter().enumerate() {
        rom[0x800 + i * 4..0x804 + i * 4].copy_from_slice(&word.to_le_bytes());
    }
    rom[0x900..0x909].copy_from_slice(b"SRAM_V113");
    rom
}
fn twin_worlds() -> [GameBoyAdvance; 2] {
    let rom = cartridge();
    let mut parent = GameBoyAdvance::new(rom.clone(), None).unwrap();
    let mut child = GameBoyAdvance::new(rom, None).unwrap();
    parent.import_save(&vec![0x11; 32768]).unwrap();
    child.import_save(&vec![0x22; 32768]).unwrap();
    let parent_state = parent.export_state();
    let child_state = child.export_state();
    parent
        .start_local_link(Vec::new(), &child_state, false, 0)
        .unwrap();
    child
        .start_local_link(Vec::new(), &parent_state, false, 1)
        .unwrap();
    [parent, child]
}
fn same_hardware(a: &GameBoyAdvance, b: &GameBoyAdvance) {
    assert_eq!(a.cpu, b.cpu);
    assert_eq!(a.clocks, b.clocks);
    assert_eq!(a.frames, b.frames);
    assert_eq!(a.bus.ewram, b.bus.ewram);
    assert_eq!(a.bus.iwram, b.bus.iwram);
    for offset in 0..0x400 {
        assert_eq!(
            a.bus.peek8(0x04000000 + offset),
            b.bus.peek8(0x04000000 + offset),
            "I/O offset {offset:03x}"
        );
    }
    assert_eq!(a.bus.vram, b.bus.vram);
    assert_eq!(a.bus.oam, b.bus.oam);
    assert_eq!(a.bus.palette, b.bus.palette);
    assert_eq!(a.export_save(), b.export_save());
    assert!(a.fault.is_none(), "{:?}", a.fault);
    assert!(b.fault.is_none(), "{:?}", b.fault);
}
fn same_worlds(worlds: &[GameBoyAdvance; 2]) {
    same_hardware(&worlds[0], worlds[1].local_link_peer().unwrap());
    same_hardware(worlds[0].local_link_peer().unwrap(), &worlds[1]);
}
fn advance(worlds: &mut [GameBoyAdvance; 2], frame: u32, parent: u16, child: u16) {
    for (role, world) in worlds.iter_mut().enumerate() {
        world.queue_local_link_frame(frame, parent, child).unwrap();
        assert!(world.run(if role == 0 { 1 } else { 2 * FRAME_CYCLES }) > 0);
        assert_eq!(world.local_link_status().unwrap().frame, frame + 1);
        assert_eq!(world.local_link_status().unwrap().queued, 0);
        let audio = world.drain_audio();
        assert_eq!((audio.sample_rate, audio.channels), (32768, 2));
        assert!(
            !audio.samples.is_empty(),
            "Visible endpoint keeps its audio"
        );
        assert!(audio
            .samples
            .iter()
            .all(|sample| sample.is_finite() && sample.abs() <= 1.0));
    }
    same_worlds(worlds);
}

#[test]
fn local_pair_runs_thousands_of_irq_dma_transfers_and_independent_inputs() {
    let mut worlds = twin_worlds();
    for frame in 0..160 {
        let parent = (frame as u16 * 17) & 0x3ff;
        let child = (frame as u16 * 31) & 0x3ff;
        advance(&mut worlds, frame, parent, child);
    }
    for frame in 160..164 {
        advance(&mut worlds, frame, 1, 0x12);
    }
    assert!(worlds[0].local_link_status().unwrap().transfers > 3000);
    assert_eq!(
        worlds[0].local_link_status().unwrap().transfers,
        worlds[1].local_link_status().unwrap().transfers
    );
    for world in &worlds {
        assert!(world.bus.peek32(0x02000000) > 3000);
        assert!(
            world.bus.peek32(0x0200000c) > 3000,
            "Guest serial IRQ handler must execute"
        );
        assert_eq!(world.bus.peek16(0x02000004), 0x3fe);
        assert_eq!(world.bus.peek16(0x02000006), 0x3ed);
        assert_eq!(
            world.bus.peek32(0x02000010),
            0x03ed03fe,
            "DMA copies both received words"
        );
    }
    assert_eq!(worlds[0].export_save()[32767], 0x11);
    assert_eq!(worlds[1].export_save()[32767], 0x22);
    assert_eq!(worlds[0].export_save()[0], 0xfe);
    assert_eq!(worlds[1].export_save()[0], 0xed);
}

#[test]
fn missing_frame_input_freezes_both_worlds_and_live_buttons_do_not_rewrite_history() {
    let mut worlds = twin_worlds();
    let before = worlds
        .each_ref()
        .map(|w| (w.cpu.clone(), w.clocks, w.frames));
    for world in &mut worlds {
        assert_eq!(world.run(FRAME_CYCLES), 0);
    }
    for (world, (cpu, clocks, frames)) in worlds.iter().zip(&before) {
        assert_eq!(&world.cpu, cpu);
        assert_eq!(world.clocks, *clocks);
        assert_eq!(world.frames, *frames);
    }
    worlds[0].set_button(0, Button::A, true).unwrap();
    assert_eq!(worlds[0].local_link_status().unwrap().keys & 1, 1);
    // Future live input is sampled by the host, but the shared input packet
    // remains authoritative for this emulated frame on both devices.
    advance(&mut worlds, 0, 0, 0);
    assert_eq!(worlds[0].bus.peek16(0x04000130), 0x3ff);
    assert_eq!(worlds[0].local_link_status().unwrap().keys & 1, 1);
    let clocks = worlds.each_ref().map(|w| w.clocks);
    for world in &mut worlds {
        assert_eq!(world.run(FRAME_CYCLES), 0);
    }
    assert_eq!(worlds.each_ref().map(|w| w.clocks), clocks);
    same_worlds(&worlds);
}

#[test]
fn stepping_restoring_and_resetting_detach_runtime_peer() {
    for action in 0..3 {
        let mut worlds = twin_worlds();
        advance(&mut worlds, 0, 0, 0);
        let primary = &mut worlds[0];
        let state = primary.export_state();
        match action {
            0 => {
                primary.step();
            }
            1 => primary.import_state(&state).unwrap(),
            _ => primary.reset(),
        }
        assert!(primary.local_link_status().is_none());
        assert!(primary.local_link_peer().is_none());
        assert!(!primary.bus.serial.connected());
        assert!(
            worlds[1].local_link_status().is_some(),
            "The other device owns its own runtime peer"
        );
    }
}

#[test]
fn bounded_ordered_input_queue_consumes_exactly_one_frame_per_run() {
    let mut worlds = twin_worlds();
    for world in &mut worlds {
        assert!(world.queue_local_link_frame(1, 0, 0).is_err());
        assert!(world.queue_local_link_frame(0, 0x400, 0).is_err());
        assert!(world.queue_local_link_frame(0, 0, 0x400).is_err());
        assert_eq!(world.local_link_status().unwrap().queued, 0);
        for frame in 0..32 {
            world.queue_local_link_frame(frame, 0, 0).unwrap();
        }
        assert!(world.queue_local_link_frame(32, 0, 0).is_err());
        assert!(world.queue_local_link_frame(0, 0, 0).is_err());
        let before = world.clocks;
        assert_eq!(world.run(0), 0);
        assert_eq!(world.clocks, before);
        assert_eq!(world.local_link_status().unwrap().queued, 32);
        assert!(world.run(10 * FRAME_CYCLES) > 0);
        assert_eq!(world.local_link_status().unwrap().frame, 1);
        assert_eq!(world.local_link_status().unwrap().queued, 31);
        world.queue_local_link_frame(32, 0, 0).unwrap();
    }
    same_worlds(&worlds);
}

#[test]
fn failed_peer_bootstrap_does_not_change_primary_cpu_saves_or_cable() {
    let mut primary = GameBoyAdvance::new(cartridge(), None).unwrap();
    primary.import_save(&vec![0x5a; 32768]).unwrap();
    let before = primary.export_state();
    for player in [0, 1, 2] {
        assert!(primary
            .start_local_link(Vec::new(), &[1, 2, 3], false, player)
            .is_err());
        assert_eq!(primary.export_state(), before);
        assert!(primary.local_link_status().is_none());
        assert!(!primary.bus.serial.connected());
    }
}

#[test]
fn host_save_and_rtc_edits_cannot_diverge_an_active_pair() {
    let mut worlds = twin_worlds();
    for world in &mut worlds {
        let original = world.export_state();
        let peer = world.local_link_peer().unwrap().export_state();
        assert!(world.import_save(&vec![0x99; 32768]).is_err());
        assert!(world.set_save_type(SaveType::None).is_err());
        assert!(world.set_auto_save_type().is_err());
        assert!(world.set_rtc_time(1_700_000_000).is_err());
        world.clear_save();
        assert_eq!(world.export_state(), original);
        assert_eq!(world.local_link_peer().unwrap().export_state(), peer);
        assert!(world.local_link_status().is_some());
    }
    // Guest CPU writes still reach its own battery storage on both devices.
    advance(&mut worlds, 0, 1, 0x12);
    assert_eq!(worlds[0].export_save()[0], 0xfe);
    assert_eq!(worlds[1].export_save()[0], 0xed);
}

use super::*;
use rustboy_snes_apu::apu::Apu;

fn adapter() -> (Vec<u8>, Sgb) {
    let mut rom = vec![0; 32768];
    rom[0x146] = 3;
    rom[0x14B] = 0x33;
    let sgb = Sgb::new(&rom);
    (rom, sgb)
}

fn payload() -> [u8; 4096] {
    let mut data = [0; 4096];
    data[..12].copy_from_slice(&[4, 0, 0xFE, 0xFF, 1, 2, 3, 4, 0, 0, 0, 4]);
    data
}

#[test]
fn sound_upload_packet_lists_write_wrapping_ram_and_retain_jump_without_execution() {
    let mut sound = sound::Sound::default();
    sound.upload(&payload());
    assert_eq!(
        [
            sound.ram.read(0xFFFE),
            sound.ram.read(0xFFFF),
            sound.ram.read(0),
            sound.ram.read(1)
        ],
        [1, 2, 3, 4]
    );
    assert_eq!(sound.jump, Some(0x0400));
    assert_eq!(sound.uploads, 1);
    let mut invalid = [0; 4096];
    invalid[..9].copy_from_slice(&[1, 0, 0, 0x20, 99, 0xFF, 0xFF, 0, 4]);
    let original = sound.ram.clone();
    sound.upload(&invalid);
    assert_eq!(sound.ram, original); // Even the valid first write is not committed.
    assert_eq!(sound.jump, Some(0x0400));
    assert_eq!((sound.uploads, sound.rejected), (1, 1));
}

#[test]
fn sound_commands_use_the_builtin_player_without_external_firmware() {
    let (_, mut sgb) = adapter();
    let mut command = [0; 16];
    command[0] = (8 << 3) | 1;
    command[1..5].copy_from_slice(&[0x17, 0x04, 0xD2, 0x03]);
    super::tests::send(&mut sgb, &command);
    assert_eq!(sgb.sound_request(), [0x17, 0x04, 0xD2, 0x03]);
    assert_eq!(sgb.unsupported[8], 0);
    assert!(sgb.sound_uses_replacement());
    assert!(sgb.sound_playback_available());
}

#[test]
fn sou_trn_uses_lcd_pipeline_five_frame_window_and_snapshot_migration() {
    let (rom, mut sgb) = adapter();
    let mut command = [0; 16];
    command[0] = (9 << 3) | 1;
    super::tests::send(&mut sgb, &command);
    let pixels = border_tests::signal(&payload());
    for _ in 0..4 {
        sgb.start_frame();
        sgb.capture_frame(&pixels);
    }
    assert_eq!(sgb.sound_uploads(), 0);
    let state = sgb.export_state();
    let mut restored = Sgb::import_state(&rom, &state, 7).unwrap();
    restored.start_frame();
    restored.capture_frame(&pixels);
    assert_eq!(restored.sound_uploads(), 1);
    assert_eq!(restored.sound_ram()[..2], [3, 4]);
    assert_eq!(restored.unsupported[9], 0);
    assert_eq!(restored.transfers_pending(), 0);
    let state = restored.export_state();
    assert_eq!(Sgb::import_state(&rom, &state, 7).unwrap(), restored);
    let mut corrupt = state.clone();
    let audio_offset=adapter().1.export_state().len()-sound::AUDIO_STATE_BASE_BYTES-34;
    *corrupt.get_mut(audio_offset - 3).unwrap() = 2;
    assert!(Sgb::import_state(&rom, &corrupt, 7).is_err());
    // Old v4 saves have no audio RAM or uploads, but retain pulse timing.
    let mut legacy = adapter().1;
    legacy.tick_joyp(4);
    legacy.write_joyp_timed(0x20);
    let state = legacy.export_state();
    let old = Sgb::import_state(&rom, &state[..state.len() - sound::STATE_BYTES - sound::AUDIO_STATE_BASE_BYTES - 34], 4).unwrap();
    assert_eq!(old.pulse_lines, legacy.pulse_lines);
    assert_eq!(old.sound, sound::Sound::default());
}

fn singing_adapter() -> (Vec<u8>,Sgb) {
    let (rom,mut sgb) = adapter();
    let mut apu = Apu::default(); apu.cpu.halted=true;
    apu.bus.ram.write_wrapping(0x100,&[0,2,0,2]);
    apu.bus.ram.write_wrapping(0x200,&[0xa3,0x12,0x34,0x56,0x78,0x9a,0xbc,0xde,0xf0]);
    for (address,value) in [(0,127),(1,127),(2,0),(3,16),(4,0),(5,0),(7,127),
        (0x0c,127),(0x1c,127),(0x5d,1),(0x6c,32),(0x4c,1)] { apu.bus.dsp.write(address,value); }
    sgb.sound.apu=Some(Box::new(apu));
    (rom,sgb)
}

#[test]
fn sound_ports_use_music_first_order_and_valid_uploads_reach_the_running_processor() {
    let (_,mut sgb) = singing_adapter();
    let mut command=[0;16]; command[0]=(8<<3)|1;
    command[1..5].copy_from_slice(&[0x17,4,0xD2,3]);
    super::tests::send(&mut sgb,&command);
    assert_eq!(sgb.sound.apu.as_ref().unwrap().bus.input,[3,0x17,4,0xD2]);
    assert_eq!(sgb.unsupported[8],0);
    sgb.sound.upload(&payload());
    let apu=sgb.sound.apu.as_ref().unwrap();
    assert_eq!(apu.bus.ram.read(0xfffe),1);
    assert_eq!(apu.bus.ram.read(0),3);
    assert_eq!(apu.cpu.pc,0x400);
    assert!(!apu.cpu.halted);
}

#[test]
fn snes_sound_clock_and_resampler_replay_exactly_with_v7_and_older_versions_migrate() {
    let (rom,mut sgb)=singing_adapter();
    for _ in 0..501 { sgb.tick_sound(4); }
    assert_ne!(sgb.sound_sample(),[0.0;2]);
    let bytes=sgb.export_state();
    let mut restored=Sgb::import_state(&rom,&bytes,7).unwrap();
    assert_eq!(restored,sgb);
    let v6 = Sgb::import_state(&rom, &bytes[..bytes.len()-34], 6).unwrap();
    assert_eq!(v6.sound, Sgb::import_state(&rom, &bytes, 7).unwrap().sound);
    for _ in 0..10000 {
        sgb.tick_sound(4); restored.tick_sound(4);
        assert_eq!(restored.sound_sample(),sgb.sound_sample());
    }
    assert_eq!(restored,sgb);
    let mut legacy=adapter().1; legacy.sound.upload(&payload());
    let bytes=legacy.export_state();
    let audio_offset=adapter().1.export_state().len()-sound::AUDIO_STATE_BASE_BYTES-34;
    let migrated=Sgb::import_state(&rom,&bytes[..audio_offset],5).unwrap();
    assert_eq!(migrated.sound_ram(),legacy.sound_ram());
    assert_eq!(migrated.sound_uploads(),legacy.sound_uploads());
    assert!(migrated.sound_uses_replacement());
    let mut corrupt=sgb.export_state(); corrupt.push(0);
    assert!(Sgb::import_state(&rom,&corrupt,7).is_err());
}

#[test]
fn powered_off_handheld_apu_still_mixes_snes_music_in_stereo() {
    let (_,sgb)=singing_adapter();
    let mut cpu=crate::cpu::CPU::new();
    cpu.booting=false; cpu.sgb=Some(Box::new(sgb));
    cpu.apu.write_register(0xff26,0);
    cpu.halt=true;
    for _ in 0..10000 { cpu.execute(); }
    let samples=cpu.get_audio_buffer();
    assert!(!samples.is_empty());
    assert!(samples.iter().any(|&v|v!=0.0));
    assert!(samples.chunks_exact(2).all(|pair|pair[0]==pair[1]));
}

#[test]
fn replacement_audio_survives_adapter_snapshot_and_custom_code_runs_on_spc700() {
    let (rom,mut sgb)=adapter();
    sgb.sound.command([0,4,0,0]);
    for _ in 0..40000 {sgb.tick_sound(4);}
    assert_ne!(sgb.sound_sample(),[0.;2]);
    let mut restored=Sgb::import_state(&rom,&sgb.export_state(),7).unwrap();
    for _ in 0..5000 {
        sgb.tick_sound(4);restored.tick_sound(4);
        assert_eq!(sgb.sound_sample(),restored.sound_sample());
    }
    assert_eq!(sgb,restored);
    let mut upload=[0;4096];
    // Our own SPC program: MOV A,#$42; MOV $20,A; SLEEP.
    upload[..13].copy_from_slice(&[5,0,0,4,0xe8,0x42,0xc4,0x20,0xef,0,0,0,4]);
    restored.sound.upload(&upload);
    assert!(!restored.sound_uses_replacement());
    for _ in 0..100 {restored.tick_sound(4);}
    assert_eq!(restored.sound.apu.as_ref().unwrap().bus.ram.read(0x20),0x42);
    assert_eq!(restored.sound_upload_rejections(),0);
}

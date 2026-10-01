//! Adapter around the existing cycle-precise Game Boy implementation.
use crate::cpu::CPU;
use crate::emulator::*;
use std::sync::atomic::Ordering;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum HardwareModel {
    #[default]
    Auto,
    Dmg,
    Cgb,
}

pub struct GameBoy {
    cpu: Box<CPU>,
    rgba: Vec<u8>,
}

impl GameBoy {
    pub fn load(rom: &[u8], boot_rom: &[u8], model: HardwareModel) -> Result<Self, String> {
        crate::cartridge::validate_rom(rom)?;
        let cgb = match model {
            HardwareModel::Auto => rom[0x143] & 0x80 != 0,
            HardwareModel::Dmg => {
                if rom[0x143] == 0xC0 {
                    return Err("CGB-only cartridge cannot run on DMG hardware".into());
                }
                false
            }
            HardwareModel::Cgb => true,
        };
        // An empty override selects the bundled replacement, including animation.
        let boot_rom = if boot_rom.is_empty() {
            if cgb {
                &crate::boot_roms::CGB[..]
            } else {
                &crate::boot_roms::DMG[..]
            }
        } else {
            boot_rom
        };
        crate::cartridge::validate_boot_rom(boot_rom, cgb)?;
        let mut cpu = Box::new(CPU::new());
        cpu.bootload(boot_rom.to_vec());
        cpu.load_rom(rom.to_vec());
        cpu.is_cgb = cgb;
        cpu.apu.set_cgb_mode(cgb);
        Ok(Self {
            cpu,
            rgba: vec![0; 160 * 144 * 4],
        })
    }

    /// Low-level access is for existing Game Boy diagnostics, not frontends.
    pub fn cpu(&self) -> &CPU {
        &self.cpu
    }
    pub fn cpu_mut(&mut self) -> &mut CPU {
        &mut self.cpu
    }

    fn advance(&mut self) -> u64 {
        self.cpu.handle_interrupts();
        self.cpu.execute();
        let cycles = self.cpu.cycles;
        self.cpu.handle_timer(cycles * 4);
        let ticks = if self.cpu.double_speed {
            cycles * 2
        } else {
            cycles * 4
        };
        if self.cpu.booting && self.cpu.program_counter == 0x100 {
            self.cpu.check_boot_finish();
        }
        self.cpu.total_cycles += cycles as u64;
        self.cpu.cycles = 0;
        ticks as u64
    }
}

impl Emulator for GameBoy {
    fn system_name(&self) -> &'static str {
        if self.cpu.is_cgb {
            "Game Boy Color"
        } else {
            "Game Boy"
        }
    }
    fn clock_hz(&self) -> u64 {
        4_194_304
    }
    fn title(&self) -> String {
        self.cpu.rom_title()
    }
    fn run(&mut self, budget: u64) -> RunResult {
        let mut ticks = 0;
        while ticks < budget {
            if self.paused() {
                return RunResult {
                    ticks,
                    reason: StopReason::Paused,
                };
            }
            if self.cpu.check_breakpoints() {
                return RunResult {
                    ticks,
                    reason: StopReason::Breakpoint,
                };
            }
            ticks += self.advance();
        }
        RunResult {
            ticks,
            reason: StopReason::BudgetExhausted,
        }
    }
    fn step(&mut self) -> RunResult {
        RunResult {
            ticks: self.advance(),
            reason: StopReason::Stepped,
        }
    }
    fn paused(&self) -> bool {
        self.cpu.is_paused.load(Ordering::Relaxed)
    }
    fn set_paused(&mut self, paused: bool) {
        self.cpu.is_paused.store(paused, Ordering::Relaxed);
    }
    fn reset(&mut self) {
        // CPU::reset infers hardware from the cart; preserve an explicit model.
        let cgb = self.cpu.is_cgb;
        self.cpu.reset();
        self.cpu.is_cgb = cgb;
        self.cpu.apu.set_cgb_mode(cgb);
    }
    fn set_button(&mut self, port: usize, button: Button, pressed: bool) -> Result<(), String> {
        if port != 0 {
            return Err("Game Boy supports only controller port 0".into());
        }
        let key = match button {
            Button::Left => 37,
            Button::Up => 38,
            Button::Right => 39,
            Button::Down => 40,
            Button::A => 65,
            Button::B => 66,
            Button::Start => 13,
            Button::Select => 16,
            _ => return Err("Button is not present on Game Boy".into()),
        };
        self.cpu.set_keys(key, pressed);
        // Retain existing frontend IRQ behavior during this structural change.
        if pressed {
            self.cpu.request_interrupt(4);
        }
        Ok(())
    }
    fn video_frame(&mut self) -> VideoFrame<'_> {
        for (out, &pixel) in self
            .rgba
            .chunks_exact_mut(4)
            .zip(self.cpu.frame_buffer.iter())
        {
            out.copy_from_slice(&[pixel as u8, (pixel >> 8) as u8, (pixel >> 16) as u8, 255]);
        }
        VideoFrame {
            geometry: VideoGeometry {
                width: 160,
                height: 144,
                aspect_width: 10,
                aspect_height: 9,
            },
            format: PixelFormat::Rgba8888,
            pixels: &self.rgba,
            enabled: self.cpu.memory[0xFF40] & 0x80 != 0,
        }
    }
    fn drain_audio(&mut self) -> AudioChunk {
        AudioChunk {
            sample_rate: 44_100,
            channels: 2,
            samples: self.cpu.get_audio_buffer(),
        }
    }
    fn save_info(&self) -> Option<SaveInfo> {
        self.cpu.has_battery().then(|| SaveInfo {
            key: self.cpu.save_key(),
            dirty: self.cpu.save_ram_is_dirty(),
        })
    }
    fn export_save(&self) -> Vec<u8> {
        self.cpu.export_save_ram()
    }
    fn import_save(&mut self, data: &[u8]) -> Result<(), String> {
        self.cpu.import_save_ram(data);
        Ok(())
    }
    fn mark_save_clean(&mut self) {
        self.cpu.mark_save_ram_clean();
    }
    fn clear_save(&mut self) {
        self.cpu.clear_save_ram();
    }
    fn state_id(&self) -> String {
        self.cpu.state_id()
    }
    fn export_state(&self) -> Vec<u8> {
        self.cpu.export_state()
    }
    fn import_state(&mut self, data: &[u8]) -> Result<(), String> {
        self.cpu.import_state(data).map_err(str::to_owned)
    }
    fn debug_extension(&self) -> Option<&dyn std::any::Any> {
        Some(self)
    }
    fn debug_extension_mut(&mut self) -> Option<&mut dyn std::any::Any> {
        Some(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn machine() -> Box<GameBoy> {
        let mut rom = vec![0; 0x8000];
        rom[0x147] = 0x09;
        rom[0x149] = 0x02;
        let mut gb = GameBoy::load(&rom, &[0; 256], HardwareModel::Auto).unwrap();
        gb.cpu.booting = false;
        gb.cpu.program_counter = 0x150;
        Box::new(gb)
    }

    #[test]
    fn run_matches_the_existing_instruction_loop() {
        let mut gb = machine();
        let mut expected = machine();
        expected.import_state(&gb.export_state()).unwrap();
        // Includes a timer edge and PPU/APU work, not just CPU registers.
        gb.cpu.write_byte(0xFF07, 5);
        expected.cpu.write_byte(0xFF07, 5);
        for _ in 0..100 {
            expected.cpu.handle_interrupts();
            expected.cpu.execute();
            let cycles = expected.cpu.cycles;
            expected.cpu.handle_timer(cycles * 4);
            expected.cpu.total_cycles += cycles as u64;
            expected.cpu.cycles = 0;
        }
        assert_eq!(
            gb.run(400),
            RunResult {
                ticks: 400,
                reason: StopReason::BudgetExhausted
            }
        );
        assert!(
            gb.export_state() == expected.cpu.export_state(),
            "adapter changed instruction-loop state"
        );
        assert_eq!(gb.drain_audio().samples, expected.cpu.get_audio_buffer());
    }

    #[test]
    fn presentation_budget_tracks_double_speed_and_instruction_overshoot() {
        let mut gb = machine();
        assert_eq!(gb.run(1).ticks, 4);
        gb.cpu.is_cgb = true;
        gb.cpu.double_speed = true;
        assert_eq!(gb.run(5).ticks, 6);
        assert_eq!(gb.run(0).ticks, 0);
        assert_eq!(gb.cpu.cycles, 0);
    }

    #[test]
    fn pause_step_and_breakpoints_are_backend_owned() {
        let mut gb = machine();
        gb.set_paused(true);
        let pc = gb.cpu.program_counter;
        assert_eq!(
            gb.run(100),
            RunResult {
                ticks: 0,
                reason: StopReason::Paused
            }
        );
        assert_eq!(gb.cpu.program_counter, pc);
        assert_eq!(
            gb.step(),
            RunResult {
                ticks: 4,
                reason: StopReason::Stepped
            }
        );
        assert!(gb.paused());
        gb.set_paused(false);
        gb.cpu.add_breakpoint_pc(gb.cpu.program_counter);
        assert_eq!(
            gb.run(100),
            RunResult {
                ticks: 0,
                reason: StopReason::Breakpoint
            }
        );
        assert!(gb.paused());
    }

    #[test]
    fn frames_and_audio_describe_their_formats_without_a_browser() {
        let mut backend: Box<dyn Emulator> = machine();
        assert_eq!(backend.clock_hz(), 4_194_304);
        let frame = backend.video_frame();
        assert_eq!(frame.geometry.width, 160);
        assert_eq!(frame.geometry.height, 144);
        assert_eq!(frame.format, PixelFormat::Rgba8888);
        assert_eq!(frame.pixels.len(), 160 * 144 * 4);
        assert!(frame.pixels.chunks_exact(4).all(|pixel| pixel[3] == 255));
        let audio = backend.drain_audio();
        assert_eq!(audio.sample_rate, 44_100);
        assert_eq!(audio.channels, 2);
        assert_eq!(audio.samples.len() % 2, 0);
    }

    #[test]
    fn buttons_use_logical_ports_and_validate_unsupported_controls() {
        let mut gb = machine();
        gb.cpu.memory[0xFF00] = 0x20;
        gb.set_button(0, Button::Right, true).unwrap();
        assert_eq!(gb.cpu.peek_byte(0xFF00) & 1, 0);
        gb.set_button(0, Button::Right, false).unwrap();
        assert_eq!(gb.cpu.peek_byte(0xFF00) & 1, 1);
        assert!(gb.set_button(1, Button::A, true).is_err());
        assert!(gb.set_button(0, Button::X, true).is_err());
    }

    #[test]
    fn wrapper_preserves_existing_state_and_battery_save_bytes() {
        let mut gb = machine();
        gb.cpu.mbc.ram[0][12] = 0xAB;
        let original = gb.cpu.export_state();
        assert!(
            gb.export_state() == original,
            "adapter changed save-state bytes"
        );
        assert_eq!(gb.export_save(), gb.cpu.export_save_ram());
        assert_eq!(gb.save_info().unwrap().key, gb.cpu.save_key());
        gb.cpu.mbc.ram[0][12] = 0;
        gb.import_state(&original).unwrap();
        assert_eq!(gb.cpu.mbc.ram[0][12], 0xAB);
    }

    #[test]
    fn explicit_model_is_independent_of_cart_flag_and_survives_reset() {
        let rom = vec![0; 0x8000];
        let mut gb = GameBoy::load(&rom, &[0; 0x900], HardwareModel::Cgb).unwrap();
        assert!(gb.cpu.is_cgb);
        gb.reset();
        assert!(gb.cpu.is_cgb);
        assert_eq!(gb.system_name(), "Game Boy Color");
        assert!(GameBoy::load(&rom, &[0; 0x100], HardwareModel::Cgb).is_err());
        let mut color_only = rom;
        color_only[0x143] = 0xC0;
        assert!(GameBoy::load(&color_only, &[0; 0x100], HardwareModel::Dmg).is_err());
        assert!(GameBoy::load(&[], &[0; 0x100], HardwareModel::Auto).is_err());
    }

    #[test]
    fn bundled_boot_roms_animate_play_audio_and_handoff_without_external_files() {
        for cgb in [false, true] {
            let mut rom = vec![0; 0x8000];
            // Synthetic cartridge logo, not an embedded Nintendo logo.
            rom[0x104..0x134].fill(0xAA);
            rom[0x143] = if cgb { 0x80 } else { 0 };
            let mut gb = GameBoy::load(&rom, &[], HardwareModel::Auto).unwrap();
            assert!(gb.cpu.booting);
            let mut seen_pixels = false;
            let mut heard_chime = false;
            let mut frames = 0;
            while gb.cpu.booting && frames < 240 {
                let target = gb.cpu.total_cycles + 17_556;
                while gb.cpu.booting && gb.cpu.total_cycles < target {
                    gb.step();
                }
                let first = gb.cpu.frame_buffer[0] & 0xFFFFFF;
                seen_pixels |= gb
                    .cpu
                    .frame_buffer
                    .iter()
                    .any(|pixel| pixel & 0xFFFFFF != first);
                heard_chime |= gb
                    .drain_audio()
                    .samples
                    .iter()
                    .any(|sample| sample.abs() > 0.001);
                frames += 1;
            }
            assert!(!gb.cpu.booting, "replacement did not hand off (cgb={cgb})");
            assert_eq!(gb.cpu.program_counter, 0x100);
            assert_eq!(gb.cpu.get_reg_a(), if cgb { 0x11 } else { 1 });
            assert!(
                seen_pixels,
                "replacement animation did not render (cgb={cgb})"
            );
            assert!(heard_chime, "replacement chime was silent (cgb={cgb})");
            gb.reset();
            assert!(gb.cpu.booting, "reset must replay the replacement boot");
        }
    }
}

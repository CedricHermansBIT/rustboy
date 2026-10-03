//! Development GBA machine with scanline graphics, Direct Sound and SRAM/Flash.
//! PSG, EEPROM, precise raster/bus timing and some BIOS services remain pending.
use crate::{
    bus::Bus,
    cpu::{Cpu, Memory},
    FRAME_CYCLES,
};
use rustboy_emulator_api::{PixelFormat, VideoFrame, VideoGeometry};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MissingBiosException {
    pub instruction_address: u32,
    pub vector: u32,
    pub reason: &'static str,
}

pub struct GameBoyAdvance {
    pub cpu: Cpu,
    pub bus: Bus,
    pub clocks: u64,
    pub frames: u64,
    /// Stop rather than executing zero-filled exception vectors as fake firmware.
    pub fault: Option<MissingBiosException>,
    pub(crate) pixels: Vec<u8>,
    pub(crate) line: u16,
    pub(crate) line_clocks: u16,
    pub(crate) wait_mask: Option<u16>,
    pub(crate) paused: bool,
}
impl GameBoyAdvance {
    pub fn new(rom: Vec<u8>, bios: Option<Vec<u8>>) -> Result<Self, String> {
        let boot = bios.is_some();
        let bus = Bus::new(rom, bios)?;
        let mut cpu = Cpu::default();
        if !boot {
            cpu.set_cpsr(0xd2).unwrap();
            cpu.r[13] = 0x03007fa0;
            cpu.set_cpsr(0xd3).unwrap();
            cpu.r[13] = 0x03007fe0;
            cpu.set_cpsr(0x1f).unwrap();
            cpu.r[13] = 0x03007f00;
            cpu.r[15] = 0x08000000;
        }
        Ok(Self {
            cpu,
            bus,
            clocks: 0,
            frames: 0,
            fault: None,
            pixels: vec![255; 240 * 160 * 4],
            line: 0,
            line_clocks: 0,
            wait_mask: None,
            paused: false,
        })
    }
    pub fn title(&self) -> String {
        String::from_utf8_lossy(&self.bus.rom[0xa0..0xac])
            .trim_end_matches('\0')
            .to_owned()
    }
    pub fn step(&mut self) -> u64 {
        if self.fault.is_some() {
            return 0;
        }
        if self.bus.dma_unit() {
            let clocks = self.bus.take_cycles();
            self.advance_video(clocks);
            return clocks;
        }
        if let Some(mask) = self.wait_mask {
            if self.cpu.cpsr() & 31 != 0x12 {
                let flags = self.bus.peek16(0x03007ff8);
                if flags & mask != 0 {
                    self.bus.write16(0x03007ff8, flags & !mask, false);
                    self.wait_mask = None;
                    self.bus.halted = false;
                } else {
                    self.bus.halted = true;
                }
            }
        }
        if self.bus.pending_irq() {
            self.bus.halted = false;
        }
        if self.bus.halted {
            let clocks = u64::from(if self.line_clocks < 1006 {
                1006 - self.line_clocks
            } else {
                1232 - self.line_clocks
            })
            .min(self.bus.timer_deadline());
            self.advance_video(clocks);
            return clocks;
        }
        let address = self.cpu.r[15];
        let thumb = self.cpu.thumb();
        let exceptions = self.cpu.exceptions;
        if !self.bus.irq_enabled() || !self.cpu.irq(&mut self.bus) {
            self.cpu.step(&mut self.bus);
        }
        if self.cpu.exceptions != exceptions && self.bus.replacement_bios {
            let vector = self.cpu.r[15];
            if vector == 8 {
                let number = if thumb {
                    self.bus.peek8(address)
                } else {
                    self.bus.peek8(address.wrapping_add(2))
                };
                match crate::bios::service(number, &mut self.cpu, &mut self.bus) {
                    Ok(effect) => {
                        if !matches!(effect, crate::bios::Effect::Reset) {
                            self.cpu.return_swi(&mut self.bus);
                        }
                        match effect {
                            crate::bios::Effect::Halt => self.bus.halted = true,
                            crate::bios::Effect::Wait { mask, discard } => {
                                self.bus.set_io(0x208, 1);
                                if discard {
                                    let flags = self.bus.peek16(0x03007ff8);
                                    self.bus.write16(0x03007ff8, flags & !mask, false);
                                }
                                self.wait_mask = Some(mask);
                            }
                            _ => {}
                        }
                    }
                    Err(reason) => {
                        self.fault = Some(MissingBiosException {
                            instruction_address: address,
                            vector,
                            reason,
                        })
                    }
                }
            } else if vector != 0x18 {
                self.fault = Some(MissingBiosException {
                    instruction_address: address,
                    vector,
                    reason: "CPU exception has no replacement handler",
                });
            }
        }
        let clocks = self.bus.take_cycles();
        self.advance_video(clocks);
        clocks
    }
    /// CPU clock budget, including whole-instruction overshoot. No host sleep,
    /// rendering callback or allocation in the execution loop.
    pub fn run(&mut self, budget: u64) -> u64 {
        let start = self.clocks;
        while self.clocks.wrapping_sub(start) < budget && self.fault.is_none() {
            self.step();
        }
        self.clocks.wrapping_sub(start)
    }
    pub fn run_frame(&mut self) -> u64 {
        self.run(FRAME_CYCLES)
    }
    pub fn video_frame(&self) -> VideoFrame<'_> {
        VideoFrame {
            geometry: VideoGeometry {
                width: 240,
                height: 160,
                aspect_width: 3,
                aspect_height: 2,
            },
            format: PixelFormat::Rgba8888,
            pixels: &self.pixels,
            enabled: true,
        }
    }
    fn advance_video(&mut self, mut clocks: u64) {
        self.clocks = self.clocks.wrapping_add(clocks);
        while clocks > 0 {
            let boundary = if self.line_clocks < 1006 { 1006 } else { 1232 };
            let advance = clocks
                .min(u64::from(boundary - self.line_clocks))
                .min(self.bus.timer_deadline())
                .min(self.bus.apu.deadline());
            let overflow = self.bus.advance_timers(advance);
            self.bus.clock_direct_sound(overflow);
            self.bus.clock_audio(advance);
            self.bus.backup.clock_eeprom(advance);
            self.line_clocks += advance as u16;
            clocks -= advance;
            if self.line_clocks == 1006 {
                if self.line < 160 {
                    self.render_line();
                }
                let stat = self.bus.io16(4) | 2;
                self.bus.set_io(4, stat);
                if stat & 0x10 != 0 {
                    self.bus.request_irq(2);
                }
                if self.line < 160 {
                    self.bus.trigger_dma(2);
                }
            } else if self.line_clocks == 1232 {
                self.line_clocks = 0;
                self.line = (self.line + 1) % 228;
                if self.line == 0 || self.line == 160 {
                    self.bus.reload_affine();
                } else if self.line < 160 {
                    self.bus.advance_affine(self.line);
                }
                self.bus.set_io(6, self.line);
                let old = self.bus.io16(4);
                let matched = self.line == old >> 8;
                let stat = (old & !7)
                    | u16::from((160..227).contains(&self.line))
                    | (u16::from(matched) << 2);
                self.bus.set_io(4, stat);
                if self.line == 160 {
                    self.frames += 1;
                    self.bus.trigger_dma(1);
                    if stat & 8 != 0 {
                        self.bus.request_irq(1);
                    }
                }
                if matched && old & 4 == 0 && stat & 0x20 != 0 {
                    self.bus.request_irq(4);
                }
            }
        }
    }
    fn render_line(&mut self) {
        crate::ppu::render_line(&self.bus, self.line, &mut self.pixels);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cpu::Memory;
    fn machine() -> GameBoyAdvance {
        let mut rom = vec![0; 192];
        rom[..4].copy_from_slice(&0xeafffffeu32.to_le_bytes());
        GameBoyAdvance::new(rom, None).unwrap()
    }
    #[test]
    fn affine_scanline_steps_and_reference_writes_do_not_recompute_past_lines() {
        let mut m = machine();
        m.bus.write16(0x04000000, 0x403, false);
        m.bus.write16(0x04000020, 0x100, false);
        m.bus.write16(0x04000026, 0x100, false);
        for (y, color) in [(0, 31u16), (1, 31 << 5), (3, 31 << 10), (6, 0x7fff)] {
            m.bus.write16(0x06000000 + y * 480, color, false);
        }
        m.bus.take_cycles();
        m.advance_video(1006);
        assert_eq!(&m.pixels[..4], &[255, 0, 0, 255]);
        m.advance_video(226);
        assert_eq!(m.bus.affine.current[0], [0, 0x100]);
        m.bus.write16(0x04000026, 0x200, false);
        m.bus.take_cycles();
        m.advance_video(1006);
        assert_eq!(&m.pixels[960..964], &[0, 255, 0, 255]);
        m.advance_video(226);
        assert_eq!(m.bus.affine.current[0], [0, 0x300]);
        m.advance_video(1006);
        assert_eq!(&m.pixels[1920..1924], &[0, 0, 255, 255]);
        m.advance_video(226);
        m.bus.write32(0x0400002c, 0x600, false);
        m.bus.take_cycles();
        m.advance_video(1006);
        assert_eq!(&m.pixels[2880..2884], &[255; 4]);
        assert_eq!(m.bus.peek32(0x0400002c), 0x600);
    }
    #[test]
    fn affine_mosaic_keeps_group_origin_and_vblank_reloads_registers() {
        let mut m = machine();
        m.bus.write16(0x04000000, 0x403, false);
        m.bus.write16(0x0400000c, 0x40, false);
        m.bus.write16(0x0400004c, 0x20, false); // Three scanlines/group.
        m.bus.write16(0x04000026, 0x100, false);
        m.bus.write16(0x06000000, 31, false);
        m.bus.write16(0x06000000 + 3 * 480, 31 << 5, false);
        m.bus.take_cycles();
        m.advance_video(1232 * 3);
        for y in 0..3 {
            assert_eq!(&m.pixels[y * 960..y * 960 + 4], &[255, 0, 0, 255]);
        }
        assert_eq!(m.bus.affine.current[0], [0, 0x300]);
        assert_eq!(m.bus.affine.mosaic[0], [0, 0x300]);
        m.advance_video(1006);
        assert_eq!(&m.pixels[2880..2884], &[0, 255, 0, 255]);
        m.advance_video(160 * 1232 - 3 * 1232 - 1006);
        assert_eq!(m.bus.affine.current[0], [0, 0]);
        m.bus.write32(0x0400002c, 0x12345, false);
        m.bus.take_cycles();
        m.advance_video(68 * 1232);
        assert_eq!(m.line, 0);
        assert_eq!(m.bus.affine.current[0], [0, 0x12345]);
        assert_eq!(m.bus.affine.mosaic, m.bus.affine.current);
    }
    #[test]
    fn affine_register_halves_axes_fraction_and_signed_wrap() {
        let mut m = machine();
        m.bus.write32(0x04000028, 0x07ffff80, false);
        m.bus.write16(0x04000022, 0x100, false);
        m.bus.write32(0x04000038, (-256i32) as u32, false);
        m.bus.write16(0x04000032, 0x80, false);
        m.bus.take_cycles();
        m.advance_video(1232);
        assert_eq!(m.bus.affine.current[0][0], -0x07ffff80);
        assert_eq!(m.bus.affine.current[1][0], -128);
        m.bus.write16(0x04000038, 0x1234, false);
        assert_eq!(m.bus.affine.current[1][0], -0xedcc);
        m.bus.write16(0x0400003a, 0, false);
        assert_eq!(m.bus.affine.current[1][0], 0x1234);
        assert_eq!(m.bus.affine.current[0][1], 0);
    }
    #[test]
    fn direct_boot_and_frame_budget() {
        let mut m = machine();
        assert_eq!(m.cpu.r[15], 0x08000000);
        assert_eq!(m.cpu.r[13], 0x03007f00);
        let elapsed = m.run_frame();
        assert!((FRAME_CYCLES..FRAME_CYCLES + 32).contains(&elapsed));
        assert_eq!(m.frames, 1);
        assert_eq!(m.cpu.exceptions, 0);
    }
    #[test]
    fn video_deadlines_and_interrupts() {
        let mut m = machine();
        m.bus.set_io(4, 0x0138);
        m.advance_video(1005);
        assert_eq!(m.bus.io16(4) & 2, 0);
        m.advance_video(1);
        assert_eq!(m.bus.io16(4) & 2, 2);
        assert_eq!(m.bus.io16(0x202), 2);
        m.advance_video(226);
        assert_eq!(m.bus.io16(6), 1);
        assert_eq!(m.bus.io16(4) & 7, 4);
        assert_eq!(m.bus.io16(0x202), 6);
        m.advance_video(159 * 1232);
        assert_eq!(m.frames, 1);
        assert_eq!(m.bus.io16(4) & 1, 1);
        m.advance_video(67 * 1232);
        assert_eq!(m.bus.io16(4) & 1, 0);
    }
    #[test]
    fn bitmap_modes_and_page_selection() {
        let mut m = machine();
        m.bus.write16(0x04000000, 0x403, false);
        m.bus.write16(0x06000000, 0x001f, false);
        m.render_line();
        assert_eq!(&m.pixels[..4], &[255, 0, 0, 255]);
        m.bus.write16(0x04000000, 0x414, false);
        m.bus.vram[0xa000] = 1;
        m.bus.write16(0x05000002, 0x3e0, false);
        m.render_line();
        assert_eq!(&m.pixels[..4], &[0, 255, 0, 255]);
        m.bus.write16(0x04000000, 0x415, false);
        m.bus.write16(0x0600a000, 0x7c00, false);
        m.render_line();
        assert_eq!(&m.pixels[..4], &[0, 0, 255, 255]);
    }
    #[test]
    fn halted_cpu_skips_to_video_event() {
        let mut m = machine();
        m.bus.halted = true;
        assert_eq!(m.step(), 1006);
        assert_eq!(m.cpu.instructions, 0);
        m.bus.request_irq(1);
        m.bus.set_io(0x200, 1);
        m.step();
        assert!(!m.bus.halted);
        assert_eq!(m.cpu.instructions, 1);
    }
    #[test]
    fn dma_stalls_cpu_but_clocks_timers() {
        let mut m = machine();
        m.bus.write32(0x04000100, 0x00800000, false);
        m.bus.write32(0x03000000, 0xdeadbeef, false);
        m.bus.write32(0x040000d4, 0x03000000, false);
        m.bus.write32(0x040000d8, 0x02000000, false);
        m.bus.write32(0x040000dc, 0x84000001, false);
        m.bus.take_cycles();
        let clocks = m.step();
        assert_eq!(clocks, 9);
        assert_eq!(m.cpu.instructions, 0);
        assert_eq!(m.bus.peek32(0x02000000), 0xdeadbeef);
        assert_eq!(m.bus.peek16(0x04000100), 9);
        m.step();
        assert_eq!(m.cpu.instructions, 1);
    }
    #[test]
    fn halt_wakes_on_timer_deadline() {
        let mut m = machine();
        m.bus.write32(0x04000100, 0x00c0fffd, false);
        m.bus.set_io(0x200, 8);
        m.bus.halted = true;
        assert_eq!(m.step(), 3);
        assert_eq!(m.cpu.instructions, 0);
        assert_eq!(m.bus.io16(0x202) & 8, 8);
        m.bus.take_cycles();
        m.step();
        assert!(!m.bus.halted);
        assert_eq!(m.cpu.instructions, 1);
    }
}

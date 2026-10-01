//! Independent tile-map and bitplane bus transactions.

use super::{registers_at, PpuLineSnapshot, PpuRegChange};
use crate::cpu::CPU;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub(crate) struct FetchBusState {
    pub last_data: u8,
    pub retained_data: Option<u8>,
}

/// Data left on the CGB tile bus is observable when LCDC.4 changes on a
/// bitplane-read edge. OBJ reads share that bus; collecting their timestamps
/// in advance lets the scanline renderer query them in chronological order
/// even though it mixes objects after drawing the background.
pub(super) struct FetchBusLatch {
    sprite_high: [(u16, u8); 10],
    sprite_count: usize,
    retained_data: Option<u8>,
    reset_dot: u16,
    previous_row_data: u8,
    last_data: u8,
    last_read_dot: u16,
    cutoff: u16,
}

impl FetchBusLatch {
    pub fn new(state: FetchBusState) -> Self {
        Self {
            sprite_high: [(0, 0); 10],
            sprite_count: 0,
            retained_data: state.retained_data,
            reset_dot: 0,
            previous_row_data: state.last_data,
            last_data: state.last_data,
            last_read_dot: 0,
            cutoff: u16::MAX,
        }
    }

    pub fn record_sprite_high(&mut self, dot: u16, value: u8) {
        assert!(self.sprite_count < self.sprite_high.len());
        self.sprite_high[self.sprite_count] = (dot, value);
        self.sprite_count += 1;
    }

    pub fn set_cutoff(&mut self, dot: u16) { self.cutoff = dot; }

    pub fn record_static_high(&mut self, value: u8) { self.last_data = value; }

    pub fn state(&self) -> FetchBusState {
        let last_sprite = self.sprite_high[..self.sprite_count].iter()
            .filter(|(dot, _)| *dot <= self.cutoff && *dot > self.last_read_dot)
            .max_by_key(|(dot, _)| *dot);
        FetchBusState {
            last_data: last_sprite.map_or(self.last_data, |(_, value)| *value),
            retained_data: self.retained_data,
        }
    }

    fn collision_value(&self, dot: u16) -> u8 {
        self.sprite_high[..self.sprite_count].iter()
            .filter(|(time, _)| *time <= dot && *time >= self.reset_dot)
            .max_by_key(|(time, _)| *time)
            .map(|(_, value)| *value)
            .or(self.retained_data)
            .unwrap_or(self.previous_row_data)
    }
}

fn fetch_registers(
    cpu: &CPU,
    snap: PpuLineSnapshot,
    changes: &[PpuRegChange],
    before_dot: u16,
) -> PpuLineSnapshot {
    let mut regs = registers_at(snap, changes, before_dot);
    // CGB A-C still sample SCY independently for each bitplane, but the
    // register's signal reaches the fetcher two dots after the CPU write.
    // This also applies to the CGB's monochrome compatibility mode.
    if cpu.is_cgb {
        regs.scy = registers_at(snap, changes, before_dot.saturating_sub(2)).scy;
    }
    regs
}

#[cfg(test)]
pub(super) fn fetch_background_tile(
    cpu: &CPU,
    snap: PpuLineSnapshot,
    changes: &[PpuRegChange],
    ly: u8,
    fetcher_x: u16,
    map_dot: u16,
) -> (u8, u8, u8) {
    fetch_background_tile_inner(cpu, snap, changes, ly, fetcher_x, map_dot, None)
}

pub(super) fn fetch_background_tile_latched(
    cpu: &CPU, snap: PpuLineSnapshot, changes: &[PpuRegChange],
    ly: u8, fetcher_x: u16, map_dot: u16, latch: &mut FetchBusLatch,
) -> (u8, u8, u8) {
    fetch_background_tile_inner(cpu, snap, changes, ly, fetcher_x, map_dot, Some(latch))
}

fn fetch_background_tile_inner(
    cpu: &CPU, snap: PpuLineSnapshot, changes: &[PpuRegChange],
    ly: u8, fetcher_x: u16, map_dot: u16, latch: Option<&mut FetchBusLatch>,
) -> (u8, u8, u8) {
    // Independent SCX and BG-map raster captures place the CGB address
    // latch at the beginning of the two-dot GetTile transaction; DMG takes
    // controls at the read edge. This phase is inferred from those captures,
    // unlike SCY's documented two-dot propagation delay.
    let map_regs = registers_at(
        snap,
        changes,
        map_dot.saturating_sub(if cpu.is_cgb { 3 } else { 1 }),
    );
    let dy = ly.wrapping_add(map_regs.scy);
    let map = if map_regs.lcdc & 8 != 0 {
        0x9C00
    } else {
        0x9800
    };
    let address =
        map + (dy as usize / 8) * 32 + (((map_regs.scx as u16 / 8) + fetcher_x) & 31) as usize;
    let (index, attr) = if cpu.cgb_native_mode() {
        (
            cpu.cgb_vram[0][address - 0x8000],
            cpu.cgb_vram[1][address - 0x8000],
        )
    } else {
        (cpu.memory[address], 0)
    };
    fetch_tile_planes(cpu, snap, changes, index, attr, ly, None, map_dot, latch)
}

#[cfg(test)]
pub(super) fn fetch_window_tile(
    cpu: &CPU,
    snap: PpuLineSnapshot,
    changes: &[PpuRegChange],
    row: u8,
    fetcher_x: u16,
    map_dot: u16,
) -> (u8, u8, u8) {
    fetch_window_tile_inner(cpu, snap, changes, row, fetcher_x, map_dot, None)
}

pub(super) fn fetch_window_tile_latched(
    cpu: &CPU, snap: PpuLineSnapshot, changes: &[PpuRegChange],
    row: u8, fetcher_x: u16, map_dot: u16, latch: &mut FetchBusLatch,
) -> (u8, u8, u8) {
    fetch_window_tile_inner(cpu, snap, changes, row, fetcher_x, map_dot, Some(latch))
}

fn fetch_window_tile_inner(
    cpu: &CPU, snap: PpuLineSnapshot, changes: &[PpuRegChange],
    row: u8, fetcher_x: u16, map_dot: u16, latch: Option<&mut FetchBusLatch>,
) -> (u8, u8, u8) {
    let map_regs = registers_at(
        snap,
        changes,
        map_dot.saturating_sub(if cpu.is_cgb { 3 } else { 1 }),
    );
    let map = if map_regs.lcdc & 0x40 != 0 {
        0x9C00
    } else {
        0x9800
    };
    let address = map + (row as usize / 8) * 32 + (fetcher_x as usize & 31);
    let (index, attr) = if cpu.cgb_native_mode() {
        (
            cpu.cgb_vram[0][address - 0x8000],
            cpu.cgb_vram[1][address - 0x8000],
        )
    } else {
        (cpu.memory[address], 0)
    };
    fetch_tile_planes(cpu, snap, changes, index, attr, 0, Some(row & 7), map_dot, latch)
}

fn fetch_tile_planes(
    cpu: &CPU,
    snap: PpuLineSnapshot,
    changes: &[PpuRegChange],
    index: u8,
    attr: u8,
    ly: u8,
    window_row: Option<u8>,
    map_dot: u16,
    mut latch: Option<&mut FetchBusLatch>,
) -> (u8, u8, u8) {
    let native = cpu.cgb_native_mode();
    let mut planes = [0; 2];
    for plane in 0..2 {
        let read_dot = map_dot + plane as u16 * 2;
        if latch.as_ref().is_some_and(|bus| read_dot > bus.cutoff) { break; }
        // Peripheral reads precede a CPU write recorded at the same dot.
        let control_dot = map_dot + 1 + plane as u16 * 2;
        let mut regs = fetch_registers(cpu, snap, changes, control_dot);
        if cpu.is_cgb {
            // CGB's tile-address mux settles at the beginning of the plane
            // transaction. This two-dot phase is inferred jointly from the
            // BG and window tile-selection captures (CGB A-C).
            regs.lcdc = registers_at(snap, changes, control_dot.saturating_sub(2)).lcdc;
        }
        let base = if regs.lcdc & 0x10 != 0 {
            0x8000
        } else {
            0x8800
        };
        let offset = if base == 0x8000 {
            index as usize * 16
        } else {
            (index as i8 as i16 + 128) as usize * 16
        };
        let row = window_row.unwrap_or_else(|| ly.wrapping_add(regs.scy) & 7);
        let row = if native && attr & 0x40 != 0 {
            7 - row
        } else {
            row
        };
        let address = base + offset + row as usize * 2 + plane;
        planes[plane] = if native {
            cpu.cgb_vram[((attr >> 3) & 1) as usize][address - 0x8000]
        } else {
            cpu.memory[address]
        };
        if let Some(bus) = latch.as_deref_mut() {
            if cpu.is_cgb {
                // The normal peripheral read precedes a simultaneous CPU
                // write. On CGB A-C that write nevertheless corrupts the
                // transaction when it changes the tile-address mux.
                if let Some(write) = changes.iter().find(|change|
                    change.dot == read_dot && change.addr == 0xFF40
                    && (change.value ^ regs.lcdc) & 0x10 != 0) {
                    planes[plane] = if write.value & 0x10 == 0 {
                        // Keep the old VRAM bus byte, not the tile index
                        // substituted into the visible FIFO transaction.
                        bus.retained_data = Some(planes[plane]);
                        bus.reset_dot = read_dot;
                        index
                    } else { bus.collision_value(read_dot) };
                }
            }
            bus.last_data = planes[plane];
            bus.last_read_dot = read_dot;
        }
    }
    (planes[0], planes[1], attr)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn mux_cpu() -> CPU {
        let mut cpu = CPU::new();
        cpu.is_cgb = true;
        cpu.booting = false;
        cpu.memory[0xFF4C] = 4;
        cpu.memory[0x9800] = 1;
        cpu.memory[0x8010] = 0xA5;
        cpu.memory[0x8011] = 0x5A;
        cpu.memory[0x9010] = 0x3C;
        cpu.memory[0x9011] = 0xC3;
        cpu
    }

    #[test]
    fn cgb_reset_substitutes_index_but_retains_the_uncorrupted_bus_read() {
        let cpu = mux_cpu();
        let snap = PpuLineSnapshot { lcdc: 0x91, ..Default::default() };
        let changes = [PpuRegChange { dot: 100, addr: 0xFF40, value: 0x81 },
            PpuRegChange { dot: 108, addr: 0xFF40, value: 0x91 }];
        let mut bus = FetchBusLatch::new(FetchBusState::default());
        bus.record_sprite_high(99, 0xFF);
        assert_eq!(fetch_background_tile_latched(&cpu, snap, &changes, 0, 0, 100, &mut bus), (1, 0xC3, 0));
        assert_eq!(bus.state().retained_data, Some(0xA5));
        // An older OBJ read cannot replace the byte retained by the reset.
        assert_eq!(fetch_background_tile_latched(&cpu, snap, &changes, 0, 0, 108, &mut bus), (0xA5, 0x5A, 0));
    }

    #[test]
    fn cgb_set_collision_uses_a_later_object_but_not_a_future_read() {
        let cpu = mux_cpu();
        let snap = PpuLineSnapshot { lcdc: 0x81, ..Default::default() };
        let changes = [PpuRegChange { dot: 100, addr: 0xFF40, value: 0x91 }];
        let mut bus = FetchBusLatch::new(FetchBusState { last_data: 0x77, retained_data: Some(0x88) });
        bus.record_sprite_high(99, 0x66);
        bus.record_sprite_high(101, 0xFF);
        assert_eq!(fetch_background_tile_latched(&cpu, snap, &changes, 0, 0, 100, &mut bus), (0x66, 0x5A, 0));
    }

    #[test]
    fn cgb_without_objects_reuses_retained_or_previous_row_data() {
        let cpu = mux_cpu();
        let snap = PpuLineSnapshot { lcdc: 0x81, ..Default::default() };
        let changes = [PpuRegChange { dot: 100, addr: 0xFF40, value: 0x91 }];
        for retained in [None, Some(0xA5)] {
            let mut bus = FetchBusLatch::new(FetchBusState { last_data: 0x77, retained_data: retained });
            assert_eq!(fetch_background_tile_latched(&cpu, snap, &changes, 0, 0, 100, &mut bus).0, retained.unwrap_or(0x77));
        }
    }

    #[test]
    fn mode3_cutoff_retains_a_partial_word_without_reading_the_next_plane() {
        let cpu = mux_cpu();
        let snap = PpuLineSnapshot { lcdc: 0x91, ..Default::default() };
        let mut bus = FetchBusLatch::new(FetchBusState::default());
        bus.set_cutoff(101);
        assert_eq!(fetch_background_tile_latched(&cpu, snap, &[], 0, 0, 100, &mut bus), (0xA5, 0, 0));
        assert_eq!(bus.state().last_data, 0xA5);
    }

    #[test]
    fn cgb_map_address_controls_latch_at_transaction_start() {
        let mut cpu = CPU::new();
        cpu.is_cgb = true;
        cpu.booting = false;
        cpu.memory[0xFF4C] = 4;
        let snap = PpuLineSnapshot {
            lcdc: 0x91,
            ..Default::default()
        };
        cpu.memory[0x9800] = 1;
        cpu.memory[0x9C01] = 2;
        cpu.memory[0x8010] = 0xA5;
        cpu.memory[0x8020] = 0x3C;
        let changes = [
            PpuRegChange {
                dot: 98,
                addr: 0xFF40,
                value: 0x99,
            },
            PpuRegChange {
                dot: 98,
                addr: 0xFF43,
                value: 8,
            },
        ];
        assert_eq!(
            fetch_background_tile(&cpu, snap, &changes, 0, 0, 100).0,
            0xA5
        );
        assert_eq!(
            fetch_background_tile(&cpu, snap, &changes, 0, 0, 101).0,
            0x3C
        );
        cpu.is_cgb = false;
        assert_eq!(
            fetch_background_tile(&cpu, snap, &changes, 0, 0, 100).0,
            0x3C
        );
    }

    #[test]
    fn cgb_window_map_uses_the_same_address_latch_phase() {
        let mut cpu = CPU::new();
        cpu.is_cgb = true;
        cpu.booting = false;
        cpu.memory[0xFF4C] = 4;
        let snap = PpuLineSnapshot {
            lcdc: 0xB1,
            ..Default::default()
        };
        cpu.memory[0x9800] = 1;
        cpu.memory[0x9C00] = 2;
        cpu.memory[0x8010] = 0xA5;
        cpu.memory[0x8020] = 0x3C;
        let changes = [PpuRegChange {
            dot: 98,
            addr: 0xFF40,
            value: 0xF1,
        }];
        assert_eq!(fetch_window_tile(&cpu, snap, &changes, 0, 0, 100).0, 0xA5);
        assert_eq!(fetch_window_tile(&cpu, snap, &changes, 0, 0, 101).0, 0x3C);
    }

    #[test]
    fn cgb_scroll_signal_reaches_each_plane_two_dots_later() {
        let mut cpu = CPU::new();
        cpu.is_cgb = true;
        cpu.booting = false;
        cpu.memory[0xFF4C] = 4; // Monochrome software on CGB hardware.
        let snap = PpuLineSnapshot {
            lcdc: 0x91,
            ..Default::default()
        };
        let changes = [PpuRegChange {
            dot: 100,
            addr: 0xFF42,
            value: 1,
        }];
        cpu.memory[0x9800] = 1;
        cpu.memory[0x8010] = 0xA5;
        cpu.memory[0x8011] = 0x5A;
        cpu.memory[0x8012] = 0x3C;
        cpu.memory[0x8013] = 0xC3;
        // Low-plane read at 102 precedes propagation; high-plane at 104
        // sees the newly written row. DMG sees it already at 102.
        assert_eq!(
            fetch_background_tile(&cpu, snap, &changes, 0, 0, 100),
            (0xA5, 0xC3, 0)
        );
        cpu.is_cgb = false;
        assert_eq!(
            fetch_background_tile(&cpu, snap, &changes, 0, 0, 100),
            (0x3C, 0xC3, 0)
        );
    }
}

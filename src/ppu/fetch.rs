//! Independent tile-map and bitplane bus transactions.

use super::{registers_at, PpuLineSnapshot, PpuRegChange};
use crate::cpu::CPU;

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

pub(super) fn fetch_background_tile(
    cpu: &CPU,
    snap: PpuLineSnapshot,
    changes: &[PpuRegChange],
    ly: u8,
    fetcher_x: u16,
    map_dot: u16,
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
    fetch_tile_planes(cpu, snap, changes, index, attr, ly, None, map_dot)
}

pub(super) fn fetch_window_tile(
    cpu: &CPU,
    snap: PpuLineSnapshot,
    changes: &[PpuRegChange],
    row: u8,
    fetcher_x: u16,
    map_dot: u16,
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
    fetch_tile_planes(cpu, snap, changes, index, attr, 0, Some(row & 7), map_dot)
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
) -> (u8, u8, u8) {
    let native = cpu.cgb_native_mode();
    let mut planes = [0; 2];
    for plane in 0..2 {
        // Peripheral reads precede a CPU write recorded at the same dot.
        let regs = fetch_registers(cpu, snap, changes, map_dot + 1 + plane as u16 * 2);
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
    }
    (planes[0], planes[1], attr)
}

#[cfg(test)]
mod tests {
    use super::*;

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

//! Object tile data is latched during fetch, independently of OAM selection.
use super::{registers_at, PpuLineSnapshot, PpuRegChange};
use crate::cpu::CPU;

/// DMG can abort an object fetch until its low-plane/address transaction.
/// CGB ignores LCDC.1 during fetching (but still checks it when mixing pixels).
#[cfg(test)]
pub(super) fn object_fetch_survives(
    cpu: &CPU,
    snap: PpuLineSnapshot,
    changes: &[PpuRegChange],
    start_dot: u16,
    last_cancel_dot: u16,
) -> bool {
    if cpu.is_cgb { return true; }
    if registers_at(snap, changes, start_dot.saturating_sub(1)).lcdc & 2 == 0 {
        return false;
    }
    !changes.iter().any(|change| change.addr == 0xFF40 && change.value & 2 == 0
        && change.dot >= start_dot && change.dot < last_cancel_dot)
}

fn plane_address(cpu: &CPU, ly: u8, oam_index: u8, lcdc: u8) -> usize {
    let base = 0xFE00 + oam_index as usize * 4;
    let height_mask = if lcdc & 4 != 0 { 15 } else { 7 };
    let mut row = ly.wrapping_add(16).wrapping_sub(cpu.memory[base]) & height_mask;
    if cpu.memory[base + 3] & 0x40 != 0 { row ^= height_mask; }
    let tile = if height_mask == 15 { cpu.memory[base + 2] & 0xFE }
        else { cpu.memory[base + 2] };
    0x8000 + tile as usize * 16 + row as usize * 2
}

pub(super) fn fetch_object_planes(
    cpu: &CPU,
    snap: PpuLineSnapshot,
    changes: &[PpuRegChange],
    ly: u8,
    oam_index: u8,
    first_output_dot: u16,
) -> (u8, u8) {
    // A left-clipped object's first visible pixel is not its first FIFO
    // pixel: the hidden columns have already advanced past the mixer.
    let raw_x = cpu.memory[0xFE01 + oam_index as usize * 4];
    let first_output_dot = first_output_dot.saturating_sub(8u8.saturating_sub(raw_x) as u16);
    let low_regs = registers_at(snap, changes, first_output_dot.saturating_sub(3));
    // The low-address step and the exit step each consume a dot. The high
    // address is read at exit without adding another dot before pixel output.
    let high_regs = registers_at(snap, changes, first_output_dot.saturating_sub(1));
    let low = plane_address(cpu, ly, oam_index, low_regs.lcdc);
    let high = plane_address(cpu, ly, oam_index, high_regs.lcdc) + 1;
    let attr = cpu.memory[0xFE03 + oam_index as usize * 4];
    if cpu.cgb_native_mode() {
        let bank = ((attr >> 3) & 1) as usize;
        (cpu.cgb_vram[bank][low - 0x8000], cpu.cgb_vram[bank][high - 0x8000])
    } else { (cpu.memory[low], cpu.memory[high]) }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn object_size_changes_between_plane_reads_do_not_change_oam_selection() {
        let mut cpu = CPU::new();
        cpu.memory[0xFE00] = 16;
        cpu.memory[0xFE01] = 8;
        cpu.memory[0xFE02] = 3;
        cpu.memory[0x8020] = 0xA5;
        cpu.memory[0x8031] = 0x5A;
        let snap = PpuLineSnapshot { lcdc: 0x87, ..Default::default() };
        let changes = [PpuRegChange { dot: 98, addr: 0xFF40, value: 0x83 }];
        assert_eq!(fetch_object_planes(&cpu, snap, &changes, 0, 0, 100), (0xA5, 0x5A));
    }

    #[test]
    fn eight_pixel_fetch_folds_selected_lower_half_and_uses_odd_tile() {
        let mut cpu = CPU::new();
        cpu.memory[0xFE00] = 16;
        cpu.memory[0xFE02] = 3;
        assert_eq!(plane_address(&cpu, 9, 0, 0x83), 0x8032);
        cpu.memory[0xFE03] = 0x40;
        assert_eq!(plane_address(&cpu, 9, 0, 0x83), 0x803C);
        assert_eq!(plane_address(&cpu, 9, 0, 0x87), 0x802C);
    }

    #[test]
    fn clipped_columns_advance_before_the_first_visible_object_pixel() {
        let mut cpu = CPU::new();
        cpu.memory[0xFE00] = 16;
        cpu.memory[0xFE01] = 2; // Six FIFO columns lie left of the screen.
        cpu.memory[0xFE02] = 2;
        cpu.memory[0x8030] = 0xA5;
        cpu.memory[0x8031] = 0x5A;
        let snap = PpuLineSnapshot { lcdc: 0x87, ..Default::default() };
        let changes = [PpuRegChange { dot: 100, addr: 0xFF40, value: 0x83 }];
        assert_eq!(fetch_object_planes(&cpu, snap, &changes, 8, 0, 103), (0xA5, 0x5A));
    }

    #[test]
    fn restoring_object_enable_does_not_resurrect_a_canceled_fetch() {
        let mut cpu = CPU::new();
        let snap = PpuLineSnapshot { lcdc: 0x83, ..Default::default() };
        let changes = [
            PpuRegChange { dot: 96, addr: 0xFF40, value: 0x81 },
            PpuRegChange { dot: 98, addr: 0xFF40, value: 0x83 },
        ];
        assert!(!object_fetch_survives(&cpu, snap, &changes, 94, 99));
        assert!(object_fetch_survives(&cpu, snap, &changes, 99, 104));
        cpu.is_cgb = true;
        assert!(object_fetch_survives(&cpu, snap, &changes, 94, 99));
    }
}

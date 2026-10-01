//! Object tile data is latched during fetch, independently of OAM selection.
use super::{registers_at, PpuLineSnapshot, PpuRegChange};
use crate::cpu::CPU;

pub(super) struct ObjectFetchSchedule {
    pub pauses: [u16; 160],
    pub prefetch: [u16; 160],
    pub fetched: [bool; 40],
    /// Clock immediately following each fetch, expressed at its first visible
    /// column so `fetch_object_planes` can account for left clipping.
    pub first_output: [u16; 40],
}

pub(super) fn fetch_schedule(cpu: &CPU, snap: PpuLineSnapshot, ly: u8) -> ObjectFetchSchedule {
    let mut schedule = ObjectFetchSchedule {
        pauses: [0; 160], prefetch: [0; 160], fetched: [false; 40], first_output: [0; 40],
    };
    let changes = &cpu.ppu_reg_log[..cpu.ppu_reg_log_len];
    let height = if snap.lcdc & 4 != 0 { 16 } else { 8 };
    let mut sprites = [(0i16, 0usize, 0u8); 10];
    let mut count = 0;
    // Mode 2 selects the first ten objects using its latched height. Changing
    // LCDC.2 in Mode 3 changes their tile fetches, not this selection.
    for index in 0..40 {
        let base = 0xFE00 + index * 4;
        let top = cpu.memory[base] as i16 - 16;
        if (ly as i16) < top || ly as i16 >= top + height { continue; }
        let raw_x = cpu.memory[base + 1];
        sprites[count] = (raw_x as i16 - 8, index, raw_x);
        count += 1;
        if count == 10 { break; }
    }
    sprites[..count].sort_unstable();
    let window = snap.lcdc & 0x20 != 0 && ly >= snap.wy && snap.wx <= 166
        && (cpu.cgb_native_mode() || snap.lcdc & 1 != 0);
    let window_left = snap.wx as i16 - 7;
    let mut tiles = [u16::MAX; 10];
    let mut tile_count = 0;
    let mut previous_delay = 0;
    for &(x, index, raw_x) in &sprites[..count] {
        if x >= 160 { continue; }
        let position = x.max(0) as usize;
        let window_delay = if window && (x >= window_left || window_left <= 0) { 6 } else { 0 };
        let first = cpu.ppu_mode3_start_dot + 11 + (snap.scx & 7) as u16
            + position as u16 + previous_delay + window_delay;
        let clipped = 8u8.saturating_sub(raw_x) as u16;
        let entry = first.saturating_sub(clipped);
        if !cpu.is_cgb && registers_at(snap, changes, entry).lcdc & 2 == 0 {
            continue;
        }
        if raw_x == 0 {
            // Fully invisible X=0 objects still stop the startup fetcher.
            schedule.pauses[position] += 11;
            previous_delay += 11;
            schedule.first_output[index] = first + 11;
            schedule.fetched[index] = true;
            continue;
        }
        let (tile, pixel) = if window && x >= window_left {
            let wx = (x - window_left).max(0) as u16;
            (0x100 | (wx / 8), (wx & 7) as u8)
        } else {
            let bx = (x + snap.scx as i16).rem_euclid(256) as u16;
            (bx / 8, (bx & 7) as u8)
        };
        let alignment = if tiles[..tile_count].contains(&tile) { 0 } else {
            tiles[tile_count] = tile;
            tile_count += 1;
            (7u8 - pixel).saturating_sub(2) as u16
        };
        let length = object_fetch_length(cpu, snap, changes, entry + alignment);
        let pause = alignment + length;
        schedule.pauses[position] += pause;
        if x >= 0 { schedule.prefetch[position] += pause; }
        previous_delay += pause;
        schedule.first_output[index] = first + pause;
        schedule.fetched[index] = length == 6;
    }
    schedule
}

/// The object fetch has cancellation checkpoints after its one-dot first
/// step, three-dot second step, and one-dot low-address step. Background
/// alignment time is separate and is retained even when fetching is canceled.
pub(super) fn object_fetch_length(
    cpu: &CPU,
    snap: PpuLineSnapshot,
    changes: &[PpuRegChange],
    begin_dot: u16,
) -> u16 {
    if cpu.is_cgb { return 6; }
    for elapsed in [0, 1, 4, 5] {
        if registers_at(snap, changes, begin_dot + elapsed).lcdc & 2 == 0 {
            return elapsed;
        }
    }
    6
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
            PpuRegChange { dot: 100, addr: 0xFF40, value: 0x83 },
        ];
        assert_eq!(object_fetch_length(&cpu, snap, &changes, 94), 4);
        assert_eq!(object_fetch_length(&cpu, snap, &changes, 101), 6);
        cpu.is_cgb = true;
        assert_eq!(object_fetch_length(&cpu, snap, &changes, 94), 6);
    }

    #[test]
    fn canceled_fetch_keeps_only_the_steps_already_completed() {
        let mut cpu = CPU::new();
        let snap = PpuLineSnapshot { lcdc: 0x83, ..Default::default() };
        for (dot, expected) in [(100, 0), (101, 1), (102, 4), (105, 5), (106, 6)] {
            let changes = [PpuRegChange { dot, addr: 0xFF40, value: 0x81 }];
            assert_eq!(object_fetch_length(&cpu, snap, &changes, 100), expected);
            cpu.is_cgb = true;
            assert_eq!(object_fetch_length(&cpu, snap, &changes, 100), 6);
            cpu.is_cgb = false;
        }
    }

    #[test]
    fn cancellation_preserves_background_alignment_but_removes_object_delay() {
        let mut cpu = CPU::new();
        cpu.ppu_mode3_start_dot = 84;
        cpu.memory[0xFE00] = 16;
        cpu.memory[0xFE01] = 16;
        cpu.ppu_reg_log[0] = PpuRegChange { dot: 108, addr: 0xFF40, value: 0x81 };
        cpu.ppu_reg_log_len = 1;
        let snap = PpuLineSnapshot { lcdc: 0x83, ..Default::default() };
        let schedule = fetch_schedule(&cpu, snap, 0);
        assert_eq!(schedule.pauses[8], 5);
        assert_eq!(schedule.prefetch[8], 5);
        assert!(!schedule.fetched[0]);
        cpu.is_cgb = true;
        let schedule = fetch_schedule(&cpu, snap, 0);
        assert_eq!(schedule.pauses[8], 11);
        assert!(schedule.fetched[0]);
    }

    #[test]
    fn colocated_objects_have_separate_latched_fetch_clocks() {
        let mut cpu = CPU::new();
        cpu.ppu_mode3_start_dot = 84;
        for index in 0..2 {
            cpu.memory[0xFE00 + index * 4] = 16;
            cpu.memory[0xFE01 + index * 4] = 16;
        }
        let snap = PpuLineSnapshot { lcdc: 0x83, ..Default::default() };
        let schedule = fetch_schedule(&cpu, snap, 0);
        assert_eq!(schedule.pauses[8], 17); // Shared 5-dot alignment, two fetches.
        assert_eq!(schedule.first_output[0], 114);
        assert_eq!(schedule.first_output[1], 120);
        assert!(schedule.fetched[0] && schedule.fetched[1]);
    }

    #[test]
    fn clipped_fetch_finishes_before_its_first_visible_pixel() {
        let mut cpu = CPU::new();
        cpu.ppu_mode3_start_dot = 84;
        cpu.memory[0xFE00] = 16;
        cpu.memory[0xFE01] = 2;
        let snap = PpuLineSnapshot { lcdc: 0x83, ..Default::default() };
        // The object starts six columns left of the screen. Its fetch starts
        // at dot 92 and finishes at 98, while visible output starts at 104.
        // Disabling at visible output affects mixing, not this completed fetch.
        cpu.ppu_reg_log[0] = PpuRegChange { dot: 104, addr: 0xFF40, value: 0x81 };
        cpu.ppu_reg_log_len = 1;
        let schedule = fetch_schedule(&cpu, snap, 0);
        assert_eq!(schedule.pauses[0], 9);
        assert_eq!(schedule.prefetch[0], 0);
        assert_eq!(schedule.first_output[0], 104);
        assert!(schedule.fetched[0]);
        // Canceling during the fetch cannot be undone by enabling it again.
        cpu.ppu_reg_log[0].dot = 93;
        cpu.ppu_reg_log[1] = PpuRegChange { dot: 99, addr: 0xFF40, value: 0x83 };
        cpu.ppu_reg_log_len = 2;
        let schedule = fetch_schedule(&cpu, snap, 0);
        assert_eq!(schedule.pauses[0], 4);
        assert!(!schedule.fetched[0]);
    }
}

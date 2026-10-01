//! Window fetcher quirks shared by the raster renderer.

use super::{PpuLineSnapshot, PpuRegChange, registers_at};

/// A left-clipped window is triggered during fetch startup, before the
/// first visible pixel. Subsequent WX writes cannot relocate its FIFO.
/// WX=0 has a distinct startup sequence and is handled by the normal path.
pub(super) fn clipped_initial_origin(
    snapshot: PpuLineSnapshot,
    changes: &[PpuRegChange],
    mode3_start: u16,
    ly: u8,
    native_cgb: bool,
) -> Option<i32> {
    // The comparison counter advances through the clipped pixels during
    // startup. A write can move WX ahead of it, but cannot rewind the beam.
    for wx in 1..7u8 {
        let live = registers_at(snapshot, changes, mode3_start + wx as u16 + 2);
        if live.wx == wx
            && live.lcdc & 0x20 != 0
            && (native_cgb || live.lcdc & 1 != 0)
            && ly >= live.wy
        {
            return Some(wx as i32 - 7);
        }
    }
    None
}

/// A second WX comparison while the window is already drawing does not
/// restart its row. If it coincides with the next tile's nametable read,
/// however, the window FIFO receives a single color-zero pixel. In
/// particular, this is *raw* color zero: objects marked behind the background
/// must be allowed through, irrespective of the shade selected by BGP.
///
/// See Matt Currie's `m3_wx_4_change(_sprites)` / `m3_wx_5_change` tests.
pub(super) fn reactivation_zero_pixel(origin: i32, screen_x: u32, wx: u8) -> bool {
    let comparison = wx as i32 - 7;
    comparison >= 0
        && screen_x as i32 == comparison
        && screen_x as i32 > origin
        && (screen_x as i32 - origin) & 7 == 0
}

/// Consume the extra FIFO pixel without consuming window tile data. Advancing
/// the origin shifts every subsequent tile pixel one position to the right.
pub(super) fn insert_reactivation_zero(origin: &mut Option<i32>, screen_x: u32, wx: u8) -> bool {
    match origin {
        Some(left) if reactivation_zero_pixel(*left, screen_x, wx) => {
            *left += 1;
            true
        }
        _ => false,
    }
}

/// Recover the WX comparator that started a six-dot fetcher restart. Writes
/// during those six dots affect later comparisons, not the restart in flight.
pub(super) fn restart_trigger_origin(
    snapshot: PpuLineSnapshot,
    changes: &[PpuRegChange],
    ly: u8,
    native_cgb: bool,
    screen_x: u32,
    output_dot: u16,
    previous_dot: Option<u16>,
) -> Option<i32> {
    if previous_dot.is_some_and(|previous| output_dot.saturating_sub(previous) < 7) {
        return None;
    }
    // WX reaches the comparator two dots after the CPU write; this phase is
    // inferred from the independent WX-change hardware screenshots.
    let mut comparator = registers_at(snapshot, changes, output_dot.saturating_sub(6));
    comparator.wx = registers_at(snapshot, changes, output_dot.saturating_sub(8)).wx;
    if comparator.wx == 0 && snapshot.scx & 7 != 0 {
        comparator = registers_at(snapshot, changes, output_dot.saturating_sub(7));
        comparator.wx = registers_at(snapshot, changes, output_dot.saturating_sub(9)).wx;
    }
    let origin = comparator.wx as i32 - 7;
    if (comparator.wx == 0 || (7..=166).contains(&comparator.wx))
        && origin.max(0) as u32 == screen_x
        && comparator.lcdc & 0x20 != 0
        && (native_cgb || comparator.lcdc & 1 != 0)
        && ly >= comparator.wy
    {
        Some(origin)
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn repeat_comparison_only_inserts_zero_on_map_read_boundary() {
        // WX=4 starts three pixels left of the viewport. Its next map read
        // coincides with visible pixel 5, not with visible pixel 8.
        assert!(reactivation_zero_pixel(-3, 5, 12));
        assert!(reactivation_zero_pixel(-3, 21, 28));
        assert!(!reactivation_zero_pixel(-3, 6, 13));
        assert!(!reactivation_zero_pixel(-3, 5, 13));
        // The first activation is not a reactivation.
        assert!(!reactivation_zero_pixel(73, 73, 80));
        let mut origin = Some(-3);
        assert!(insert_reactivation_zero(&mut origin, 5, 12));
        assert_eq!(origin, Some(-2));
        assert!(!insert_reactivation_zero(&mut origin, 6, 12));
    }

    #[test]
    fn clipped_window_trigger_is_distinct_from_visible_comparison() {
        let snapshot = PpuLineSnapshot { lcdc: 0xF3, wx: 4, wy: 4, ..Default::default() };
        assert_eq!(clipped_initial_origin(snapshot, &[], 84, 4, false), Some(-3));
        assert_eq!(clipped_initial_origin(snapshot, &[], 84, 3, false), None);
        assert_eq!(clipped_initial_origin(PpuLineSnapshot { wx: 7, ..snapshot }, &[], 84, 4, false), None);
        assert_eq!(clipped_initial_origin(PpuLineSnapshot { wx: 0, ..snapshot }, &[], 84, 4, false), None);
    }

    #[test]
    fn wx_write_during_startup_cannot_move_comparison_behind_the_beam() {
        let snapshot = PpuLineSnapshot { lcdc: 0xF3, wx: 6, wy: 4, ..Default::default() };
        let changes = [PpuRegChange { dot: 92, addr: 0xFF4B, value: 4 }];
        assert_eq!(clipped_initial_origin(snapshot, &changes, 84, 4, false), None);
        let snapshot = PpuLineSnapshot { wx: 4, ..snapshot };
        assert_eq!(clipped_initial_origin(snapshot, &changes, 84, 4, false), Some(-3));
    }

    #[test]
    fn wx_write_during_restart_does_not_cancel_trigger() {
        let snapshot = PpuLineSnapshot { lcdc: 0xF3, wx: 94, wy: 4, ..Default::default() };
        let changes = [PpuRegChange { dot: 188, addr: 0xFF4B, value: 80 }];
        assert_eq!(restart_trigger_origin(snapshot, &changes, 94, false, 87, 188, Some(181)), Some(87));
        assert_eq!(restart_trigger_origin(snapshot, &changes, 94, false, 87, 188, Some(187)), None);
        let changes = [PpuRegChange { dot: 188, addr: 0xFF4B, value: 80 }];
        let snapshot = PpuLineSnapshot { wx: 100, ..snapshot };
        assert_eq!(restart_trigger_origin(snapshot, &changes, 100, false, 93, 194, Some(187)), Some(93));
    }

    #[test]
    fn wx_zero_restart_latches_enable_before_visible_output() {
        let snapshot = PpuLineSnapshot { lcdc: 0xE1, wx: 144, ..Default::default() };
        let changes = [
            PpuRegChange { dot: 84, addr: 0xFF4B, value: 0 },
            PpuRegChange { dot: 96, addr: 0xFF40, value: 0xC1 },
        ];
        assert_eq!(restart_trigger_origin(snapshot, &changes, 0, false, 0, 101, None), Some(-7));
    }

    #[test]
    fn delayed_wx_comparison_survives_a_write_one_dot_before_trigger() {
        let snapshot = PpuLineSnapshot { lcdc: 0xF3, wx: 6, ..Default::default() };
        let changes = [
            PpuRegChange { dot: 92, addr: 0xFF4B, value: 101 },
            PpuRegChange { dot: 188, addr: 0xFF4B, value: 80 },
        ];
        assert_eq!(restart_trigger_origin(snapshot, &changes, 101, false, 94, 195, Some(188)), Some(94));
    }
}

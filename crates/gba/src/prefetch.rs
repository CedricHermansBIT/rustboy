//! Original instruction-granular Game Pak prefetch timing model.
//! The cartridge is immutable: queued halfwords are represented by addresses,
//! not copied data. Exact CPU pipeline and bus arbitration phases remain separate.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub(crate) struct Prefetch {
    pub head: u32,
    pub count: u8,
    pub elapsed: u8,
}
pub(crate) const STATE_BYTES: usize = 6;

pub(crate) fn halfword_cycles(address: u32, sequential: bool, wait: u16) -> u32 {
    let area = ((address >> 25) - 4) as usize;
    if !sequential || address & 0x1ffff == 0 {
        [4, 3, 2, 8][(wait >> (2 + area * 3) & 3) as usize] + 1
    } else if wait >> (4 + area * 3) & 1 != 0 {
        2
    } else {
        [3, 5, 9][area]
    }
}
impl Prefetch {
    fn cartridge(address: u32) -> bool {
        (0x08000000..0x0e000000).contains(&address)
    }
    fn producer(&self) -> u32 {
        self.head + u32::from(self.count) * 2
    }
    pub fn valid(&self, wait: u16) -> bool {
        if self.head == 0 {
            return self.count == 0 && self.elapsed == 0;
        }
        if !Self::cartridge(self.head) || self.head & 1 != 0 || self.count > 8 {
            return false;
        }
        let producer = self.producer();
        if producer > 0x0e000000 {
            return false;
        }
        if self.count == 8 || producer == 0x0e000000 {
            self.elapsed == 0
        } else {
            u32::from(self.elapsed) < halfword_cycles(producer, true, wait)
        }
    }
    pub fn idle(&mut self, mut clocks: u32, wait: u16) {
        if self.head == 0 {
            return;
        }
        while self.count < 8 && Self::cartridge(self.producer()) {
            let cost = halfword_cycles(self.producer(), true, wait);
            let remaining = cost - u32::from(self.elapsed);
            if clocks < remaining {
                self.elapsed += clocks as u8;
                return;
            }
            clocks -= remaining;
            self.count += 1;
            self.elapsed = 0;
            if clocks == 0 {
                break;
            }
        }
    }
    pub fn fetch(&mut self, address: u32, halves: u8, sequential: bool, wait: u16) -> u32 {
        if !sequential || self.head != address {
            *self = Self {
                head: address,
                ..Self::default()
            };
        }
        if self.count >= halves {
            self.count -= halves;
            self.head += u32::from(halves) * 2;
            // A fully buffered CPU opcode takes one bus clock, including ARM.
            self.idle(1, wait);
            if !Self::cartridge(self.head) {
                *self = Self::default();
            }
            return 1;
        }
        let mut clocks = 0;
        for n in 0..halves {
            if self.count != 0 {
                self.count -= 1;
            } else {
                let cost = halfword_cycles(self.head, sequential || n != 0, wait);
                clocks += cost - u32::from(self.elapsed);
                self.elapsed = 0;
            }
            self.head += 2;
        }
        if !Self::cartridge(self.head) {
            *self = Self::default();
        }
        clocks.max(1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn partial_fill_full_capacity_and_arm_consumption() {
        let mut p = Prefetch::default();
        assert_eq!(p.fetch(0x08000000, 2, false, 0), 8);
        p.idle(2, 0);
        assert_eq!((p.count, p.elapsed), (0, 2));
        assert_eq!(p.fetch(0x08000004, 1, true, 0), 1);
        p.idle(100, 0);
        assert_eq!((p.count, p.elapsed), (8, 0));
        assert_eq!(p.fetch(0x08000006, 2, true, 0), 1);
        assert_eq!((p.count, p.elapsed), (6, 1));
        assert!(p.valid(0));
    }
    #[test]
    fn page_boundary_progress_uses_nonsequential_wait() {
        let mut p = Prefetch::default();
        assert_eq!(p.fetch(0x0801fffe, 1, false, 0), 5);
        p.idle(3, 0);
        assert_eq!((p.count, p.elapsed), (0, 3));
        assert_eq!(p.fetch(0x08020000, 1, true, 0), 2);
        assert_eq!(p.fetch(0x08020002, 1, true, 0), 3);
    }
    #[test]
    fn discontinuity_discards_buffer_and_partial_progress() {
        let mut p = Prefetch::default();
        p.fetch(0x08000000, 1, false, 0);
        p.idle(25, 0);
        assert_eq!(p.fetch(0x08001000, 2, false, 0), 8);
        assert_eq!((p.head, p.count, p.elapsed), (0x08001004, 0, 0));
        assert_eq!(p.fetch(0x0a000000, 1, true, 0), 5);
        p.idle(4, 0);
        assert_eq!(p.fetch(0x0a000002, 1, true, 0), 1);
    }
    #[test]
    fn cartridge_end_stops_production_and_leaves_a_canonical_empty_state() {
        let mut p = Prefetch {
            head: 0x0dfffffc,
            ..Prefetch::default()
        };
        p.idle(100, 0);
        assert_eq!((p.count, p.elapsed), (2, 0));
        assert!(p.valid(0));
        assert_eq!(p.fetch(0x0dfffffc, 2, true, 0), 1);
        assert_eq!(p, Prefetch::default());
        assert_eq!(p.fetch(0x0dfffffc, 2, false, 0), 14);
        assert_eq!(p, Prefetch::default());
    }
    #[test]
    fn batched_idle_matches_one_clock_steps_for_all_wait_settings() {
        for wait in 0..0x800 {
            for head in [0x0801fff8, 0x0a000002, 0x0c000002] {
                for clocks in [0, 1, 2, 7, 19, 80] {
                    let mut batch = Prefetch {
                        head,
                        ..Prefetch::default()
                    };
                    let mut single = batch;
                    batch.idle(clocks, wait);
                    for _ in 0..clocks {
                        single.idle(1, wait);
                    }
                    assert_eq!(batch, single);
                    assert!(batch.valid(wait));
                }
            }
        }
    }
}

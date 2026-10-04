//! Original scanline GBA display compositor, derived from register and VRAM
//! formats in GBATEK. Raster-time register latching and precise OBJ fetch phases belong
//! to the video scheduler, rather than this color compositor.
use crate::bus::Bus;

/// Internal scanline origins and vertical-mosaic group origins, in signed 20.8
/// fixed point (28 significant bits). Registers are not modified by stepping.
#[derive(Clone, Copy, Default, Debug, PartialEq, Eq)]
pub(crate) struct Affine {
    pub current: [[i32; 2]; 2],
    pub mosaic: [[i32; 2]; 2],
}
pub(crate) fn wrap_reference(value: i32) -> i32 {
    (value.wrapping_shl(4)) >> 4
}
pub(crate) fn register_reference(bus: &Bus, bg: usize, axis: usize) -> i32 {
    signed_ref(bus, 0x28 + bg * 0x10 + axis * 4)
}
impl Bus {
    pub(crate) fn reload_affine(&mut self) {
        for bg in 0..2 {
            for axis in 0..2 {
                self.affine.current[bg][axis] = register_reference(self, bg, axis);
            }
        }
        self.affine.mosaic = self.affine.current;
    }
    pub(crate) fn advance_affine(&mut self, line: u16) {
        for bg in 0..2 {
            for axis in 0..2 {
                let step = self.io16(0x22 + bg * 0x10 + axis * 4) as i16 as i32;
                self.affine.current[bg][axis] =
                    wrap_reference(self.affine.current[bg][axis].wrapping_add(step));
            }
        }
        if line.is_multiple_of((self.io16(0x4c) >> 4 & 15) + 1) {
            self.affine.mosaic = self.affine.current;
        }
    }
    pub(crate) fn affine_write(&mut self, offset: usize) {
        if !matches!(offset, 0x28..=0x2f | 0x38..=0x3f) {
            return;
        }
        let bg = (offset - 0x28) / 16;
        let axis = (offset & 7) / 4;
        let value = register_reference(self, bg, axis);
        self.affine.current[bg][axis] = value;
        if self.io16(6).is_multiple_of((self.io16(0x4c) >> 4 & 15) + 1) {
            self.affine.mosaic[bg][axis] = value;
        }
    }
    /// Legacy states stored only the public registers. This reconstructs their
    /// old static-origin convention; subsequent scanlines use the new latches.
    pub(crate) fn reconstruct_affine(&mut self, line: u16) {
        let line = if line < 160 { line } else { 0 };
        let first = line - line % ((self.io16(0x4c) >> 4 & 15) + 1);
        for bg in 0..2 {
            for axis in 0..2 {
                let value = register_reference(self, bg, axis);
                let step = self.io16(0x22 + bg * 0x10 + axis * 4) as i16 as i32;
                self.affine.current[bg][axis] =
                    wrap_reference(value.wrapping_add(step * i32::from(line)));
                self.affine.mosaic[bg][axis] =
                    wrap_reference(value.wrapping_add(step * i32::from(first)));
            }
        }
    }
}

#[derive(Clone, Copy)]
struct Dot {
    color: u16,
    layer: u8,
    priority: u8,
    semi: bool,
}
impl Dot {
    fn key(self) -> u8 {
        self.priority * 8 + if self.layer == 4 { 0 } else { self.layer + 1 }
    }
}
fn half(data: &[u8], offset: usize) -> u16 {
    u16::from_le_bytes([data[offset], data[offset + 1]])
}
fn palette(bus: &Bus, index: usize) -> u16 {
    half(&bus.palette, (index & 511) * 2) & 0x7fff
}
fn signed_ref(bus: &Bus, offset: usize) -> i32 {
    let value = u32::from_le_bytes(bus.io[offset..offset + 4].try_into().unwrap());
    (value.wrapping_shl(4) as i32) >> 4
}
fn bg_coordinates(bus: &Bus, affine: Affine, bg: usize, x: i32, mosaic: bool) -> (i32, i32) {
    let base = 0x20 + (bg - 2) * 0x10;
    let a = bus.io16(base) as i16 as i32;
    let c = bus.io16(base + 4) as i16 as i32;
    let origin = if mosaic {
        affine.mosaic[bg - 2]
    } else {
        affine.current[bg - 2]
    };
    (
        wrap_reference(origin[0].wrapping_add(a * x)) >> 8,
        wrap_reference(origin[1].wrapping_add(c * x)) >> 8,
    )
}
fn background(
    bus: &Bus,
    affine: Affine,
    bg: usize,
    mode: u16,
    x: usize,
    line: usize,
) -> Option<Dot> {
    let cnt = bus.io16(8 + bg * 2);
    let mosaic = bus.io16(0x4c);
    let (mut x, mut y) = (x as i32, line as i32);
    if cnt & 0x40 != 0 {
        x -= x % ((mosaic & 15) + 1) as i32;
        y -= y % (((mosaic >> 4) & 15) + 1) as i32;
    }
    let text = mode == 0 || mode == 1 && bg < 2;
    let color = if text {
        let size = cnt >> 14;
        let width = if size & 1 != 0 { 512 } else { 256 };
        let height = if size & 2 != 0 { 512 } else { 256 };
        x = (x + (bus.io16(0x10 + bg * 4) & 511) as i32) % width;
        y = (y + (bus.io16(0x12 + bg * 4) & 511) as i32) % height;
        let block = x / 256 + (y / 256) * (width / 256);
        let map = ((cnt >> 8) & 31) as usize * 0x800
            + block as usize * 0x800
            + (((y as usize / 8) & 31) * 32 + ((x as usize / 8) & 31)) * 2;
        let entry = half(&bus.vram, map & 0xffff);
        let mut px = x as usize & 7;
        let mut py = y as usize & 7;
        if entry & 0x400 != 0 {
            px = 7 - px;
        }
        if entry & 0x800 != 0 {
            py = 7 - py;
        }
        let eight = cnt & 0x80 != 0;
        let address = ((cnt >> 2) & 3) as usize * 0x4000
            + (entry & 1023) as usize * if eight { 64 } else { 32 }
            + py * if eight { 8 } else { 4 }
            + if eight { px } else { px / 2 };
        // Invalid tile addresses do not fetch OBJ RAM. On GBA a single BG
        // repeats its map entry; cross-BG invalid-address bus contention is not
        // modelled here (valid software does not depend on it).
        let byte = if address < 0x10000 {
            bus.vram[address]
        } else {
            (entry >> ((address & 1) * 8)) as u8
        };
        let index = if eight {
            byte as usize
        } else {
            ((byte >> ((px & 1) * 4)) & 15) as usize
        };
        if index == 0 {
            return None;
        }
        palette(
            bus,
            index
                + if eight {
                    0
                } else {
                    ((entry >> 12) & 15) as usize * 16
                },
        )
    } else {
        (x, y) = bg_coordinates(bus, affine, bg, x, cnt & 0x40 != 0);
        if mode <= 2 {
            let size = 128i32 << (cnt >> 14);
            if cnt & 0x2000 != 0 {
                x = x.rem_euclid(size);
                y = y.rem_euclid(size);
            } else if x < 0 || y < 0 || x >= size || y >= size {
                return None;
            }
            let map = ((cnt >> 8) & 31) as usize * 0x800
                + (y as usize / 8) * (size as usize / 8)
                + x as usize / 8;
            let tile = bus.vram[map & 0xffff] as usize;
            let address = ((cnt >> 2) & 3) as usize * 0x4000
                + tile * 64
                + (y as usize & 7) * 8
                + (x as usize & 7);
            let index = bus.vram[address & 0xffff] as usize;
            if index == 0 {
                return None;
            }
            palette(bus, index)
        } else {
            let (width, height) = if mode == 5 { (160, 128) } else { (240, 160) };
            if x < 0 || y < 0 || x >= width || y >= height {
                return None;
            }
            let page = if mode != 3 && bus.io16(0) & 0x10 != 0 {
                0xa000
            } else {
                0
            };
            let index = y as usize * width as usize + x as usize;
            if mode == 4 {
                let index = bus.vram[page + index] as usize;
                if index == 0 {
                    return None;
                }
                palette(bus, index)
            } else {
                half(&bus.vram, page + index * 2) & 0x7fff
            }
        }
    };
    Some(Dot {
        color,
        layer: bg as u8,
        priority: (cnt & 3) as u8,
        semi: false,
    })
}

fn objects(bus: &Bus, line: usize, dots: &mut [Option<Dot>; 240], window: &mut [bool; 240]) {
    let display = bus.io16(0);
    if display & 0x1000 == 0 {
        return;
    }
    let mosaic = bus.io16(0x4c);
    let mut budget: i32 = if display & 0x20 != 0 { 954 } else { 1210 };
    const SIZES: [[(i32, i32); 4]; 3] = [
        [(8, 8), (16, 16), (32, 32), (64, 64)],
        [(16, 8), (32, 8), (32, 16), (64, 32)],
        [(8, 16), (8, 32), (16, 32), (32, 64)],
    ];
    for obj in 0..128 {
        let a0 = half(&bus.oam, obj * 8);
        let a1 = half(&bus.oam, obj * 8 + 2);
        let a2 = half(&bus.oam, obj * 8 + 4);
        let affine = a0 & 0x100 != 0;
        let double = a0 & 0x200 != 0;
        let shape = (a0 >> 14) as usize;
        let kind = (a0 >> 10) & 3;
        if shape == 3 || kind == 3 || (!affine && double) || (kind == 2 && display & 0x8000 == 0) {
            continue;
        }
        let (width, height) = SIZES[shape][(a1 >> 14) as usize];
        let (bound_w, bound_h) = if affine && double {
            (width * 2, height * 2)
        } else {
            (width, height)
        };
        let ox = (a1 & 511) as i32;
        let oy = (a0 & 255) as i32;
        let ox = if ox >= 256 { ox - 512 } else { ox };
        let oy = if oy > 256 - bound_h { oy - 256 } else { oy };
        let row = line as i32 - oy;
        if row < 0 || row >= bound_h {
            continue;
        }
        // OAM order consumes a finite rendering budget, even for transparent
        // or horizontally offscreen objects. A final object may be partial.
        // This is a scanline budget, not a cycle-by-cycle OBJ fetch pipeline.
        let overhead = if affine { 10 } else { 0 };
        let per_pixel = if affine { 2 } else { 1 };
        let rendered_width = ((budget - overhead).max(0) / per_pixel).min(bound_w);
        budget = (budget - overhead - bound_w * per_pixel).max(0);
        if rendered_width == 0 {
            break;
        }
        let eight = a0 & 0x2000 != 0;
        let unit = if eight { 2 } else { 1 };
        let base = (a2 & 1023) as usize
            & if eight && display & 0x40 == 0 {
                !1
            } else {
                usize::MAX
            };
        if display & 7 >= 3 && base < 512 {
            continue;
        }
        let matrix = ((a1 >> 9) & 31) as usize * 32;
        let (pa, pb, pc, pd) = if affine {
            (
                half(&bus.oam, matrix + 6) as i16 as i32,
                half(&bus.oam, matrix + 14) as i16 as i32,
                half(&bus.oam, matrix + 22) as i16 as i32,
                half(&bus.oam, matrix + 30) as i16 as i32,
            )
        } else {
            (0, 0, 0, 0)
        };
        for screen_x in ox.max(0)..(ox + rendered_width).min(240) {
            let mut local_x = screen_x - ox;
            let mut local_y = row;
            if a0 & 0x1000 != 0 {
                local_x -= local_x % (((mosaic >> 8) & 15) + 1) as i32;
                local_y -= local_y % (((mosaic >> 12) & 15) + 1) as i32;
            }
            let (px, py) = if affine {
                let dx = local_x - bound_w / 2;
                let dy = local_y - bound_h / 2;
                (
                    ((pa * dx + pb * dy) >> 8) + width / 2,
                    ((pc * dx + pd * dy) >> 8) + height / 2,
                )
            } else {
                (
                    if a1 & 0x1000 != 0 {
                        width - 1 - local_x
                    } else {
                        local_x
                    },
                    if a1 & 0x2000 != 0 {
                        height - 1 - local_y
                    } else {
                        local_y
                    },
                )
            };
            if px < 0 || py < 0 || px >= width || py >= height {
                continue;
            }
            let stride = if display & 0x40 != 0 {
                width as usize / 8 * unit
            } else {
                32
            };
            let tile = (base + py as usize / 8 * stride + px as usize / 8 * unit) & 1023;
            let address = 0x10000
                + tile * 32
                + (py as usize & 7) * if eight { 8 } else { 4 }
                + if eight {
                    px as usize & 7
                } else {
                    (px as usize & 7) / 2
                };
            let byte = bus.vram[0x10000 + ((address - 0x10000) & 0x7fff)];
            let index = if eight {
                byte as usize
            } else {
                ((byte >> ((px as usize & 1) * 4)) & 15) as usize
            };
            if index == 0 {
                continue;
            }
            let x = screen_x as usize;
            if kind == 2 {
                window[x] = true;
                continue;
            }
            // The OBJ layer is resolved by OAM ordering before BG priority and
            // effects; overlapping sprites never blend with one another.
            if dots[x].is_none() {
                dots[x] = Some(Dot {
                    color: palette(
                        bus,
                        256 + index + if eight { 0 } else { (a2 >> 12) as usize * 16 },
                    ),
                    layer: 4,
                    priority: ((a2 >> 10) & 3) as u8,
                    semi: kind == 1,
                });
            }
        }
    }
}
fn inside(position: usize, range: u16, limit: usize) -> bool {
    let start = (range >> 8) as usize;
    let mut end = (range & 255) as usize;
    if end > limit || start > end {
        end = limit;
    }
    position >= start && position < end
}
fn window_mask(bus: &Bus, x: usize, y: usize, obj_window: bool) -> u8 {
    let display = bus.io16(0);
    if display & 0xe000 == 0 {
        return 63;
    }
    if display & 0x2000 != 0 && inside(x, bus.io16(0x40), 240) && inside(y, bus.io16(0x44), 160) {
        bus.io16(0x48) as u8 & 63
    } else if display & 0x4000 != 0
        && inside(x, bus.io16(0x42), 240)
        && inside(y, bus.io16(0x46), 160)
    {
        (bus.io16(0x48) >> 8) as u8 & 63
    } else if display & 0x8000 != 0 && obj_window {
        (bus.io16(0x4a) >> 8) as u8 & 63
    } else {
        bus.io16(0x4a) as u8 & 63
    }
}
fn blend(a: u16, b: u16, eva: u16, evb: u16) -> u16 {
    let mut result = 0;
    for shift in [0, 5, 10] {
        result |= ((((a >> shift) & 31) * eva + ((b >> shift) & 31) * evb) >> 4).min(31) << shift;
    }
    result
}
fn brighten(color: u16, coefficient: u16, increase: bool) -> u16 {
    let mut result = 0;
    for shift in [0, 5, 10] {
        let channel = (color >> shift) & 31;
        let channel = if increase {
            channel + (((31 - channel) * coefficient) >> 4)
        } else {
            channel - ((channel * coefficient) >> 4)
        };
        result |= channel << shift;
    }
    result
}
pub(crate) fn render_line(bus: &Bus, line: u16, pixels: &mut [u8]) {
    render_line_origins(bus, line, pixels, bus.affine);
}
fn render_line_origins(bus: &Bus, line: u16, pixels: &mut [u8], affine: Affine) {
    if line >= 160 {
        return;
    }
    let row = &mut pixels[line as usize * 240 * 4..(line as usize + 1) * 240 * 4];
    let display = bus.io16(0);
    let mode = display & 7;
    if display & 0x80 != 0 {
        row.fill(255);
        return;
    }
    let mut objs = [None; 240];
    let mut obj_windows = [false; 240];
    objects(bus, line as usize, &mut objs, &mut obj_windows);
    let enabled = match mode {
        0 => 15,
        1 => 7,
        2 => 12,
        3..=5 => 4,
        _ => 0,
    } & ((display >> 8) & 15);
    let bld = bus.io16(0x50);
    let alpha = bus.io16(0x52);
    let eva = (alpha & 31).min(16);
    let evb = ((alpha >> 8) & 31).min(16);
    let evy = (bus.io16(0x54) & 31).min(16);
    let backdrop = Dot {
        color: palette(bus, 0),
        layer: 5,
        priority: 4,
        semi: false,
    };
    for x in 0..240 {
        let mask = window_mask(bus, x, line as usize, obj_windows[x]);
        let mut first = backdrop;
        let mut second: Option<Dot> = None;
        let mut insert = |dot: Dot| {
            if dot.key() < first.key() {
                second = Some(first);
                first = dot;
            } else if second.is_none_or(|below| dot.key() < below.key()) {
                second = Some(dot);
            }
        };
        for bg in 0..4 {
            if enabled & mask as u16 & (1 << bg) != 0 {
                if let Some(dot) = background(bus, affine, bg, mode, x, line as usize) {
                    insert(dot);
                }
            }
        }
        if mask & 16 != 0 {
            if let Some(dot) = objs[x] {
                insert(dot);
            }
        }
        let mut color = first.color;
        if mask & 32 != 0 {
            let target1 = bld & (1 << first.layer) != 0;
            let target2 = second.filter(|below| bld & (1 << (below.layer + 8)) != 0);
            let effect = (bld >> 6) & 3;
            if let Some(below) = target2.filter(|_| first.semi || effect == 1 && target1) {
                color = blend(color, below.color, eva, evb);
            } else if target1 && (effect == 2 || effect == 3) {
                color = brighten(color, evy, effect == 2);
            }
        }
        for component in 0..3 {
            let value = ((color >> (component * 5)) & 31) as u8;
            row[x * 4 + component] = (value << 3) | (value >> 2);
        }
        row[x * 4 + 3] = 255;
    }
    // GREEN_SWAP acts on the final LCD output, not on individual layers or
    // palette entries. Each pair is anchored at an even screen coordinate.
    if bus.io16(2) & 1 != 0 {
        for pair in row.as_chunks_mut::<8>().0 {
            pair.swap(1, 5);
        }
    }
}

#[cfg(test)]
mod tests;

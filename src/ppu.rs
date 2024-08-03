use crate::cpu::CPU;
use wasm_bindgen::Clamped;
use web_sys::console;

static COLORS: [[u8; 4]; 4] = [[255, 255, 255, 255], [0, 255, 0, 255], [0, 128, 0, 255], [0, 0, 0, 255]];
//static COLORS : [(u8, u8, u8); 4] = [(255, 255, 255), (192, 192, 192), (96, 96, 96), (0, 0, 0)];

fn get_palette(cpu: &CPU) -> [[u8; 4]; 4] {
    let palette = cpu.read_byte(0xff47);
    [
        COLORS[((palette >> 0) & 0x03) as usize],
        COLORS[((palette >> 4) & 0x03) as usize],
        COLORS[((palette >> 2) & 0x03) as usize],
        COLORS[((palette >> 6) & 0x03) as usize]
    ]
}

fn get_color_index(b1: u8, b2: u8, x: u8) -> u8 {
    let b1sel = (b1 >> (7 - x)) & 0x01;
    let b2sel = (b2 >> (7 - x)) & 0x01;
    (b1sel << 1) | b2sel
}

pub fn draw_state(context: &web_sys::CanvasRenderingContext2d, cpu: &mut CPU) {
    //console::log_1(&"Drawing state".into());
    let mut buffer = [0; 160 * 144];
    //cpu.write_byte(0xFF44, 0x90);
    // VBLANK interrupt
    //cpu.request_interrupt(0);

    context.clear_rect(0.0, 0.0, 160.0, 144.0);

    let lcd_control = cpu.read_byte(0xFF40);
    let scroll_y = cpu.read_byte(0xFF42);
    let scroll_x = cpu.read_byte(0xFF43);

    if lcd_control & 0x80 == 0x80 {
        let tile_map_base = if lcd_control & 0x08 == 0x08 { 0x9C00 } else { 0x9800 };
        let tile_data_base = if lcd_control & 0x10 == 0x10 { 0x8000 } else { 0x8800 };
        //let palette = cpu.get_palette_data();
        //console::log_1(&format!("Palette: {:02X} {:02X} {:02X} {:02X}", palette[0], palette[1], palette[2], palette[3]).into());
        // Draw the background
        let tile_data = cpu.get_mem_slice(tile_data_base, tile_data_base + 0x2000);
        for y in 0..144u32 {
            for x in 0..160u32 {
                let dx = ((x as u32 + scroll_x as u32) & 0xFF) as u8;
                let dy = ((y as u32 + scroll_y as u32) & 0xFF) as u8;
                let tile_index = cpu.read_byte((tile_map_base + ((dy as u16 / 8) * 32 + (dx as u16 / 8))) as usize);
                let offset: u16 = if tile_data_base == 0x8000 {
                    tile_index as u16 * 16 + ((dy % 8) * 2) as u16
                } else {
                    ((tile_index as i8 as i16 + 128) as u16) * 16 + ((dy % 8) * 2) as u16
                };
                let b1 = tile_data[offset as usize];
                let b2 = tile_data[offset as usize + 1];
                let color = get_color_index(b1, b2, dx as u8);
                // set buffer
                buffer[(y * 160 + x) as usize] = color;
            }
        }

        // Draw the window
        if lcd_control & 0x20 == 0x20 {
            let window_y = cpu.read_byte(0xFF4A);
            let window_x = cpu.read_byte(0xFF4B) - 7;
            let window_tile_map_base = if lcd_control & 0x40 == 0x40 { 0x9C00 } else { 0x9800 };
            //let tile_data = cpu.get_tile_data(tile_data_base);
            if window_x <= 166 && window_y <= 143 {
                for y in 0..144-window_y {
                    for x in 0..160-window_x {
                        let dx = x + window_x + scroll_x;
                        let dy = y + window_y + scroll_y;
                        let tile_index = cpu.read_byte((window_tile_map_base + ((dy / 8) * 32 + (dx / 8)) as u16) as usize);

                        let offset: u16 = if tile_data_base == 0x8000 {
                            tile_index as u16 * 16 + ((dy % 8) * 2) as u16
                        } else {
                            (tile_index as u16 + 128) * 16 + ((dy % 8) * 2) as u16
                        };
                        let b1 = tile_data[offset as usize];
                        let b2 = tile_data[offset as usize + 1];
                        let color = get_color_index(b1, b2, dx as u8);
                        // set buffer
                        buffer[(y * 160 + x) as usize] = color;

                    }
                }
            }
        }

        // Draw the sprites
        //if lcd_control & 0x02 == 0x02 {
            let mode = (lcd_control >> 2) & 0x01;
            for i in 0..40 {
                let sprite = cpu.get_sprite(i);
                //console::log_1(&format!("Sprite {}: {:?}", i, sprite).into());

                let sprite_y = sprite[0] as i16 - 16;
                let sprite_x = sprite[1] as i16 - 8;
                let tile_index = sprite[2];
                let attributes = sprite[3];
                // mode 0: 8x8, mode 1: 8x16
                if mode == 0 {
                    if sprite_x >= -7 && sprite_x < 160 && sprite_y >= -7 && sprite_y < 144 {
                        draw_sprite(&mut buffer, sprite_x, sprite_y, tile_index, attributes, &cpu);
                    }
                } else {
                    if sprite_x >= -7 && sprite_x < 160 && sprite_y >= -15 && sprite_y < 144 {
                        draw_sprite(&mut buffer, sprite_x, sprite_y, tile_index & 0xFE, attributes, &cpu);
                        draw_sprite(&mut buffer, sprite_x, sprite_y + 8, tile_index | 0x01, attributes, &cpu);
                    }
                }
            }
        //}
        
        // convert buffer to image data
        let mut data = Vec::with_capacity(160*144*4);
        let palette = get_palette(&cpu);
        buffer.iter().for_each(|&color_index| {
            data.extend_from_slice(&palette[color_index as usize]);
        });
        let image_data = web_sys::ImageData::new_with_u8_clamped_array_and_sh(Clamped(&data), 160, 144).unwrap();
        context.put_image_data(&image_data, 0.0, 0.0).unwrap();
    }

    // set #FF44 to 90
}

fn draw_sprite(buffer: &mut [u8], sprite_x: i16, sprite_y: i16, tile_index: u8, attributes: u8, cpu: &CPU) {
    let flip_x = attributes & 0x20 == 0x20;
    let flip_y = attributes & 0x40 == 0x40;
    let priority = attributes & 0x80 == 0x80;
    let sprite_addr = 0x8000 + (tile_index as u16 * 16);
    let sprite_data = cpu.get_mem_slice(sprite_addr as usize, (sprite_addr + 16) as usize);
    //console::log_1(&format!("Sprite data: {:x?}", sprite_data).into());

    for y in 0..8u8 {
        for x in 0..8u8 {
            let dx = if flip_x { 7 - x } else { x };
            let dy = if flip_y { 7 - y } else { y };
            let buffer_x = sprite_x as i32 + x as i32;
            let buffer_y = sprite_y as i32 + y as i32;
            
            if buffer_x < 0 || buffer_x >= 160 || buffer_y < 0 || buffer_y >= 144 {
                continue;
            }
            
            let buffer_index = (buffer_y * 160 + buffer_x) as usize;
            
            let b1 = sprite_data[(dy as usize * 2) as usize];
            let b2 = sprite_data[(dy as usize * 2 + 1) as usize];
            let color = get_color_index(b1, b2, dx);

            if color != 0 && (!priority || buffer[buffer_index] == 0) {
                buffer[buffer_index] = color;
            }
        }
    }
}

pub fn draw_vram(context: &web_sys::CanvasRenderingContext2d, cpu: &mut CPU) {
    //16*24 tiles of 8x8 pixels
    console::log_1(&format!("vram slice: {:x?}", cpu.get_mem_slice(0x8000, 0xa000)).into());
    console::log_1(&format!("vram slice: {:x?}", cpu.get_mem_slice(0x9800, 0x9c00)).into());
    let mut buffer = [0; 384 * 8 * 8];
    for tile_index in 0..384 {
        let x = tile_index % 16;
        let y = tile_index / 16;
        let tile_data = cpu.get_tile_data(0x8000);
        let offset: u16 = tile_index as u16 * 16;
        for ty in 0..8 {
            let b1 = tile_data[(offset + (ty * 2) as u16) as usize];
            let b2 = tile_data[(offset + (ty * 2 + 1) as u16) as usize];
            for tx in 0..8 {
                let b1sel = (b1 >> (7 - tx)) & 0x01;
                let b2sel = (b2 >> (7 - tx)) & 0x01;
                let color = ((b1sel << 1) | b2sel) as usize;
                let index = ((y * 8 + ty) * 128 + (x * 8 + tx)) as usize;
                if index < buffer.len() {
                    buffer[index] = color;
                }
            }
        }
    }
    let mut data = Vec::with_capacity(128*192*4);
    let palette = get_palette(&cpu);
    buffer.iter().for_each(|&color_index| {
        data.extend_from_slice(&palette[color_index as usize]);
    });
    let image_data = web_sys::ImageData::new_with_u8_clamped_array_and_sh(Clamped(&data), 128, 192).unwrap();
    context.put_image_data(&image_data, 0.0, 0.0).unwrap();
}
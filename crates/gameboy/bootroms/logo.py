"""Original RustBoy pixel wordmark; no external artwork/font dependencies.

Glyphs are five by seven pixels. DMG expands the 48x8 source to 96x16;
CGB uses a 128x24 canvas with threefold-sized glyphs.
"""

GLYPHS = {
    "R": ["11110", "10001", "10001", "11110", "10100", "10010", "10001"],
    "u": ["00000", "00000", "10001", "10001", "10001", "10011", "01101"],
    "s": ["00000", "00000", "01111", "10000", "01110", "00001", "11110"],
    "t": ["00100", "00100", "11111", "00100", "00100", "00101", "00010"],
    "B": ["11110", "10001", "10001", "11110", "10001", "10001", "11110"],
    "o": ["00000", "00000", "01110", "10001", "10001", "10001", "01110"],
    "y": ["00000", "00000", "10001", "10001", "01111", "00001", "01110"],
}


def bitmap(width, height, scale):
    pixels = [[0] * width for _ in range(height)]
    text = "RustBoy"
    left = (width - (len(text) * 6 - 1) * scale) // 2
    top = (height - 7 * scale) // 2
    for letter, char in enumerate(text):
        for y, row in enumerate(GLYPHS[char]):
            for x, bit in enumerate(row):
                for dy in range(scale):
                    for dx in range(scale):
                        pixels[top + y * scale + dy][left + (letter * 6 + x) * scale + dx] = int(bit)
    return pixels


def dmg_logo():
    pixels = bitmap(48, 8, 1)
    data = bytearray()
    for tile_y in range(0, 8, 4):
        for tile_x in range(0, 48, 4):
            for y in range(tile_y, tile_y + 4, 2):
                byte = 0
                for row in pixels[y:y + 2]:
                    for bit in row[tile_x:tile_x + 4]:
                        byte = (byte << 1) | bit
                data.append(byte)
    return bytes(data)


def cgb_logo():
    pixels = bitmap(128, 24, 3)
    data = bytearray()
    for tile_y in range(0, 24, 8):
        for tile_x in range(0, 128, 8):
            for row in pixels[tile_y:tile_y + 8]:
                byte = 0
                for bit in row[tile_x:tile_x + 8]:
                    byte = (byte << 1) | bit
                # Color 3 is the animated hue; color 0 is the background.
                data.extend((byte, byte))
    return bytes(data)

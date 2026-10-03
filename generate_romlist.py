#!/usr/bin/env python3
"""Generate the ignored local GB/GBC/GBA library manifest (never a Pages asset)."""
import json
from pathlib import Path


def collect_roms(roms_dir):
    roms_dir = Path(roms_dir)
    entries = []
    for rom in roms_dir.rglob('*'):
        if not rom.is_file() or rom.suffix.lower() not in ('.gb', '.gbc', '.gba'):
            continue
        relative = rom.relative_to(roms_dir)
        if relative.parts[0] == 'images':
            continue
        entry = {'name': relative.as_posix()}
        for suffix in ('-thumb.png', '-image.png', '-marquee.png'):
            image = Path('images') / (rom.stem + suffix)
            if (roms_dir / image).is_file():
                entry['img'] = image.as_posix()
                break
        entries.append(entry)
    return sorted(entries, key=lambda entry: entry['name'].lower())


def main():
    roms_dir = Path(__file__).resolve().parent / 'roms'
    roms_dir.mkdir(exist_ok=True)
    entries = collect_roms(roms_dir)
    output = roms_dir / 'romlist.json'
    output.write_text(json.dumps(entries, ensure_ascii=False, indent=1) + '\n', encoding='utf-8')
    print(f'Generated {output} with {len(entries)} local GB/GBC/GBA entries.')


if __name__ == '__main__':
    main()

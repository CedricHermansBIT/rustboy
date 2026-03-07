#!/usr/bin/env python3
"""Generate romlist.json from the roms/ directory.
Run from the rustboy/ directory: python3 generate_romlist.py
"""
import json, os, sys

script_dir = os.path.dirname(os.path.abspath(__file__))
roms_dir = os.path.join(script_dir, "roms")
images_dir = os.path.join(roms_dir, "images")

rom_files = sorted(
    f for f in os.listdir(roms_dir)
    if f.lower().endswith(".gb") and os.path.isfile(os.path.join(roms_dir, f))
)

entries = []
for rom in rom_files:
    base = rom[:-3]  # strip .gb
    entry = {"name": rom}
    # Check for images: thumb > image > marquee
    for suffix in ("-thumb.png", "-image.png", "-marquee.png"):
        img_name = base + suffix
        if os.path.isfile(os.path.join(images_dir, img_name)):
            entry["img"] = "images/" + img_name
            break
    entries.append(entry)

out_path = os.path.join(roms_dir, "romlist.json")
with open(out_path, "w", encoding="utf-8") as f:
    json.dump(entries, f, ensure_ascii=False, indent=1)

print(f"Generated {out_path} with {len(entries)} entries")


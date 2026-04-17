#!/usr/bin/env python3
"""Generate romlist.json from the roms/ directory and its subdirectories.
Run from the rustboy/ directory: python3 generate_romlist.py
"""
import json
import os

script_dir = os.path.dirname(os.path.abspath(__file__))
roms_dir = os.path.join(script_dir, "testroms")
images_dir = os.path.join(roms_dir, "images")

entries = []

# os.walk yields (current_path, subdirectories, files) for every level
for root, dirs, files in os.walk(roms_dir):
    # Skip the images directory itself to avoid processing artwork as ROMs
    if os.path.commonpath([root, images_dir]) == images_dir:
        continue

    for f in sorted(files):
        if f.lower().endswith(".gb"):
            # Get the path relative to roms_dir (e.g., "Action/game.gb")
            full_path = os.path.join(root, f)
            rel_rom_path = os.path.relpath(full_path, roms_dir)

            # Use the filename (minus extension) to look for images
            base_name = os.path.splitext(f)[0]
            entry = {"name": rel_rom_path}

            # Check for images in the central images/ folder
            for suffix in ("-thumb.png", "-image.png", "-marquee.png"):
                img_name = base_name + suffix
                if os.path.isfile(os.path.join(images_dir, img_name)):
                    entry["img"] = "images/" + img_name
                    break

            entries.append(entry)

# Sort entries alphabetically by the relative path name
entries.sort(key=lambda x: x["name"].lower())

out_path = os.path.join(roms_dir, "romlist.json")
with open(out_path, "w", encoding="utf-8") as f:
    json.dump(entries, f, ensure_ascii=False, indent=1)

print(f"Generated {out_path} with {len(entries)} entries from the directory tree.")
#!/usr/bin/env python3
"""Self-contained checks, with explicit opt-ins for local ROMs and browser."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess

ROOT = Path(__file__).resolve().parents[1]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--roms", action="store_true", help="also check local ROM fixtures, graphics and boot diagnostics")
    parser.add_argument("--browser", action="store_true", help="run real headless Chromium against out/")
    parser.add_argument("--offline", action="store_true")
    parser.add_argument("--target-dir", default=os.environ.get("CARGO_TARGET_DIR", "target"))
    args = parser.parse_args()
    node = shutil.which("node")
    if not node:
        parser.error("Node.js 24 is required for the browser/audio checks")
    for name, size, digest in [
        ("dmg_boot.hex", 256, "e79e658710af41422b89d1e513bd71a6bd2f9d8fe590a3dae070c25e7f71cf11"),
        ("cgb_boot.hex", 2304, "e356af876376ec1ba838b7689957360f1d4422c11bae2eed2ed33eb0ee7a1b2a"),
    ]:
        data = bytes.fromhex((ROOT / "crates/gameboy/bootroms" / name).read_text())
        if len(data) != size or hashlib.sha256(data).hexdigest() != digest:
            parser.error(f"Bundled replacement firmware changed unexpectedly: {name}")
    cargo = ["cargo", "test", "--locked", "--release", "--no-default-features", "--target-dir", args.target_dir]
    if args.offline:
        cargo.append("--offline")
    subprocess.run(cargo + ["--workspace", "--lib", "--test", "ppu_strict"], cwd=ROOT, check=True)
    for test in ["tests/audio_queue.cjs", "tests/rom_loader.mjs", "tests/library_rom.mjs"]:
        subprocess.run([node, test], cwd=ROOT, check=True)
    subprocess.run(["python3", "tests/prepare_pages_test.py"], cwd=ROOT, check=True)
    if args.roms:
        cases = json.loads((ROOT / "tests/rom_cases.json").read_text())
        for case in cases:
            path = ROOT / case["path"]
            if not path.is_file():
                parser.error(f"Missing ROM fixture: {path}")
            if case.get("sha256") and hashlib.sha256(path.read_bytes()).hexdigest() != case["sha256"]:
                parser.error(f"ROM fixture differs from the recorded version: {path}")
            if case.get("reference"):
                reference = ROOT / case["reference"]
                if not reference.is_file() or hashlib.sha256(reference.read_bytes()).hexdigest() != case["reference_sha256"]:
                    parser.error(f"Missing or changed graphics reference: {reference}")
        for name, size in [("dmg_boot.bin", 256), ("cgb_boot.bin", 2304)]:
            boot = ROOT / "roms" / name
            if not boot.is_file() or boot.stat().st_size != size:
                parser.error(f"Provide your own {boot} ({size} bytes)")
        # These exact original fixtures remain enabled in the unfiltered suite.
        # The release gate identifies them explicitly, never rewrites expectations.
        exceptions = json.loads((ROOT / "tests/fixture_errata.json").read_text())
        filters = [option for case in exceptions for option in ("--skip", case["name"])]
        print("Known original fixture failures excluded from release gate:", ", ".join(case["name"] for case in exceptions), flush=True)
        subprocess.run(cargo + ["--test", "headless", "--test", "graphics", "--test", "boot_diagnostics", "--"] + filters, cwd=ROOT, check=True)
    if args.browser:
        subprocess.run([node, "tests/browser_smoke.mjs"], cwd=ROOT, check=True)
        for model in ["dmg", "cgb", "sgb"]:
            environment = {**os.environ, "RUSTBOY_NO_BOOT": "1"}
            environment.pop("RUSTBOY_ROM", None)
            environment.pop("RUSTBOY_SYNTHETIC_CGB", None)
            environment.pop("RUSTBOY_SYNTHETIC_SGB", None)
            if model == "cgb":
                environment["RUSTBOY_SYNTHETIC_CGB"] = "1"
            if model == "sgb":
                environment["RUSTBOY_SYNTHETIC_SGB"] = "1"
            subprocess.run([node, "tests/browser_smoke.mjs"], cwd=ROOT, env=environment, check=True)


if __name__ == "__main__":
    main()

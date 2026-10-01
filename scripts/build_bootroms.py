#!/usr/bin/env python3
"""Rebuild RustBoy's licensed replacement firmware (optional RGBDS 1.0.4).

Ordinary Cargo/WASM builds consume checked-in hex and need no assembler.
Use --check to verify reproducibility without modifying checked-in files.
"""
import argparse
import hashlib
import importlib.util
from pathlib import Path
import shutil
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[1]
CORE = ROOT / "crates/gameboy"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--rgbds-dir", type=Path, help="directory containing rgbasm and rgblink")
    parser.add_argument("--cc", default="cc")
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()
    spec = importlib.util.spec_from_file_location("rustboy_logo", CORE / "bootroms/logo.py")
    logo = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(logo)
    assembler = str(args.rgbds_dir / "rgbasm") if args.rgbds_dir else "rgbasm"
    linker = str(args.rgbds_dir / "rgblink") if args.rgbds_dir else "rgblink"
    for tool in (assembler, linker, args.cc):
        if not shutil.which(tool):
            parser.error(f"Missing build tool: {tool}")
    with tempfile.TemporaryDirectory(prefix="rustboy-bootroms-") as directory:
        temporary = Path(directory)
        rows = [logo.dmg_logo()[i:i + 16] for i in range(0, 48, 16)]
        (temporary / "RustBoyLogo.inc").write_text("\n".join(
            "db " + ", ".join(f"${byte:02x}" for byte in row) for row in rows
        ) + "\n")
        compressor = temporary / "pb12"
        subprocess.run([args.cc, "-std=c99", "-Wall", "-Werror",
                        str(CORE / "third_party/sameboy/source/pb12.c"), "-o", str(compressor)], check=True)
        packed = subprocess.run([str(compressor)], input=logo.cgb_logo(), stdout=subprocess.PIPE, check=True).stdout
        (temporary / "RustBoyLogo.pb12").write_bytes(packed)
        raw_dmg = logo.bitmap(48, 8, 1)
        dmg_rows = [row for source in raw_dmg
                    for row in [[bit for value in source for bit in (value, value)]] * 2]
        for model, pixels in (("dmg", dmg_rows), ("cgb", logo.bitmap(128, 24, 3))):
            silhouette = "\n".join("".join(map(str, row)) for row in pixels) + "\n"
            reference = CORE / f"bootroms/logo_{model}.txt"
            if args.check:
                if silhouette != reference.read_text():
                    raise SystemExit(f"Logo reference differs: {reference}")
            else:
                reference.write_text(silhouette)
        for model, size in (("dmg", 256), ("cgb", 2304)):
            obj, binary = temporary / f"{model}.o", temporary / f"{model}.bin"
            subprocess.run([assembler, "-I", str(CORE / "third_party/sameboy/source") + "/",
                            "-I", str(temporary) + "/", "-o", str(obj),
                            str(CORE / f"bootroms/{model}_boot.asm")], check=True)
            subprocess.run([linker, "-x", "-o", str(binary), str(obj)], check=True)
            data = binary.read_bytes()
            if len(data) != size:
                raise SystemExit(f"Invalid {model} firmware size: {len(data)} (expected {size})")
            destination = CORE / f"bootroms/{model}_boot.hex"
            if args.check:
                if data != bytes.fromhex(destination.read_text()):
                    raise SystemExit(f"Rebuilt firmware differs: {destination}")
            else:
                destination.write_text("\n".join(data[i:i + 32].hex(" ") for i in range(0, len(data), 32)) + "\n")
            print(f"{model}: {len(data)} bytes, SHA-256 {hashlib.sha256(data).hexdigest()}")


if __name__ == "__main__":
    main()

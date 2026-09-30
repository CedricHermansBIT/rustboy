#!/usr/bin/env python3
"""Build the browser bundle using the CLI version matching Cargo.lock."""
import argparse
import os
from pathlib import Path
import subprocess
import tomllib

ROOT = Path(__file__).resolve().parents[1]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--offline", action="store_true")
    parser.add_argument("--target-dir", default=os.environ.get("CARGO_TARGET_DIR", "target"))
    args = parser.parse_args()
    lock = tomllib.loads((ROOT / "Cargo.lock").read_text())
    version = next(package["version"] for package in lock["package"] if package["name"] == "wasm-bindgen")
    try:
        actual = subprocess.check_output(["wasm-bindgen", "--version"], text=True).strip().split()[-1]
    except FileNotFoundError:
        actual = "not installed"
    if actual != version:
        parser.error(f"wasm-bindgen-cli is {actual}; install the matching version: cargo install wasm-bindgen-cli --locked --version {version}")
    target = Path(args.target_dir).resolve()
    command = ["cargo", "build", "--locked", "--release", "--target", "wasm32-unknown-unknown", "--target-dir", str(target)]
    if args.offline:
        command.append("--offline")
    subprocess.run(command, cwd=ROOT, check=True)
    subprocess.run(["wasm-bindgen", "--target", "web", "--out-dir", str(ROOT / "out"),
                    str(target / "wasm32-unknown-unknown/release/rustboy.wasm")], cwd=ROOT, check=True)


if __name__ == "__main__":
    main()

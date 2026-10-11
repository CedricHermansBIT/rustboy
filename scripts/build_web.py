#!/usr/bin/env python3
"""Build and optimize the browser bundle with wasm-bindgen and Binaryen."""
import argparse
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
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
    wasm_opt = shutil.which("wasm-opt")
    if wasm_opt is None:
        parser.error("wasm-opt is not installed; install Binaryen and add its bin directory to PATH")
    manifest = tomllib.loads((ROOT / "Cargo.toml").read_text())
    optimization = manifest["package"]["metadata"]["wasm-pack"]["profile"]["release"]["wasm-opt"]
    target = Path(args.target_dir).resolve()
    command = ["cargo", "build", "--locked", "--release", "--target", "wasm32-unknown-unknown", "--target-dir", str(target)]
    if args.offline:
        command.append("--offline")
    subprocess.run(command, cwd=ROOT, check=True)
    subprocess.run(["wasm-bindgen", "--target", "web", "--out-dir", str(ROOT / "out"),
                    str(target / "wasm32-unknown-unknown/release/rustboy.wasm")], cwd=ROOT, check=True)
    wasm = ROOT / "out/rustboy_bg.wasm"
    before = wasm.stat().st_size
    # Share wasm-pack's release flags. Preserve the growable exported table:
    # the browser JIT installs functions at runtime, so don't use closed-world.
    with tempfile.TemporaryDirectory(prefix=".wasm-opt-", dir=wasm.parent) as temporary:
        optimized = Path(temporary) / wasm.name
        subprocess.run([wasm_opt, str(wasm), *optimization, "-o", str(optimized)],
                       cwd=ROOT, check=True)
        optimized.replace(wasm)
    print(f"wasm-opt: {before:,} -> {wasm.stat().st_size:,} bytes", flush=True)


if __name__ == "__main__":
    main()

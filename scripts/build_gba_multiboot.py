#!/usr/bin/env python3
"""Rebuild our original ARM7 multiboot firmware; normal builds use the artifact."""
from pathlib import Path
import shutil
import subprocess
import tempfile

root = Path(__file__).resolve().parent.parent
firmware = root / 'crates/gba/firmware'
sysroot = Path(subprocess.check_output(['rustup', 'run', 'stable', 'rustc', '--print', 'sysroot'], text=True).strip())
linker = next(sysroot.glob('lib/rustlib/*/bin/rust-lld'))
with tempfile.TemporaryDirectory() as directory:
    obj = Path(directory) / 'multiboot.o'
    elf = Path(directory) / 'multiboot.elf'
    subprocess.run([shutil.which('clang') or 'clang', '--target=armv4t-none-eabi', '-mcpu=arm7tdmi', '-marm', '-Oz', '-ffreestanding', '-fno-builtin', '-fno-unwind-tables', '-fno-asynchronous-unwind-tables', '-c', str(firmware / 'multiboot.c'), '-o', str(obj)], check=True)
    subprocess.run([str(linker), '-flavor', 'gnu', '-T', str(firmware / 'multiboot.ld'), str(obj), '-o', str(elf)], check=True)
    subprocess.run([shutil.which('llvm-objcopy') or 'llvm-objcopy', '-O', 'binary', str(elf), str(firmware / 'multiboot.bin')], check=True)
print(f'Original multiboot firmware: {(firmware / "multiboot.bin").stat().st_size} bytes')

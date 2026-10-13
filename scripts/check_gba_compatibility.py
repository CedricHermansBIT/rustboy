#!/usr/bin/env python3
"""Independent GBA release gate. No external ROM or commercial game is bundled.

The default strict gate fails on every independent failure. --baseline permits
only the explicit, measured classifications in the manifest; those results stay
FAIL and release_ready stays false. Missing fixtures are never counted as passes.
"""
import argparse
import hashlib
import io
import json
import os
from pathlib import Path
import re
import shutil
import struct
import subprocess
import sys
import urllib.request
import zipfile

ROOT = Path(__file__).resolve().parents[1]
MANIFEST = ROOT / "tests/gba_compatibility_cases.json"


def digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def download(url):
    request = urllib.request.Request(url, headers={"User-Agent": "RustBoy-independent-compatibility"})
    with urllib.request.urlopen(request, timeout=60) as response:
        return response.read()


def checkout_archive(repository, revision, directory):
    """Extract a pinned source archive only into the explicitly chosen cache."""
    archive = zipfile.ZipFile(io.BytesIO(download(f"{repository}/archive/{revision}.zip")))
    prefix = archive.namelist()[0].split('/')[0] + '/'
    directory.mkdir(parents=True, exist_ok=True)
    for entry in archive.infolist():
        relative = Path(entry.filename.removeprefix(prefix))
        if relative.is_absolute() or '..' in relative.parts:
            raise ValueError("Unsafe external fixture archive")
        if entry.is_dir() or not relative.parts:
            continue
        destination = directory / relative
        destination.parent.mkdir(parents=True, exist_ok=True)
        destination.write_bytes(archive.read(entry))


def verify_jsmolka(directory, fixture):
    for relative, expected in fixture['files'].items():
        path = directory / relative
        if not path.is_file():
            raise ValueError(f"Missing pinned independent fixture: {path}")
        data = path.read_bytes()
        # git checkout may apply CRLF while GitHub archives retain LF. The
        # independent glyph source is pinned by its canonical Git blob text.
        if relative in fixture.get('text_files_lf', []):
            data = data.replace(b'\r\n', b'\n')
        if hashlib.sha256(data).hexdigest() != expected:
            raise ValueError(f"Modified pinned independent fixture: {path}")


def verify_built_elf(rom_path, elf_path):
    """Check an ARM ELF's load segments against its post-gbafix ROM image.

    This establishes ROM/ELF agreement, not a claim of trusted source provenance.
    A caller must also supply the pinned suite source checkout.
    """
    rom, elf = Path(rom_path).read_bytes(), Path(elf_path).read_bytes()
    if len(elf) < 52 or elf[:7] != b'\x7fELF\x01\x01\x01':
        raise ValueError("Expected a little-endian ELF32 executable")
    header = struct.unpack_from('<16sHHIIIIIHHHHHH', elf)
    if header[1] != 2 or header[2] != 40 or header[9] < 32:
        raise ValueError("Expected an ARM executable ELF with load segments")
    matching = 0
    for index in range(header[10]):
        offset = header[5] + index * header[9]
        if offset + 32 > len(elf):
            raise ValueError("Truncated ELF program-header table")
        kind, file_at, _, address, size, _, _, _ = struct.unpack_from('<8I', elf, offset)
        if kind != 1 or address >> 24 != 8 or size == 0:
            continue
        rom_at = address - 0x08000000
        skip = max(0, 192 - rom_at)  # gbafix updates the cartridge header after linking
        if file_at + size > len(elf) or rom_at + size > len(rom):
            raise ValueError("ELF load segment exceeds supplied ROM")
        if elf[file_at + skip:file_at + size] != rom[rom_at + skip:rom_at + size]:
            raise ValueError("Supplied ELF does not describe the supplied test ROM")
        matching += 1
    if not matching:
        raise ValueError("ELF has no matching cartridge load segments")


def parse_results(output):
    results = []
    for line in output.splitlines():
        if not line.startswith('RESULT\t'):
            continue
        _, name, status, detail = line.split('\t', 3)
        if status not in ('PASS', 'FAIL'):
            raise ValueError("Invalid independent result status")
        results.append({'name': name, 'status': status, 'detail': detail})
    return results


def classify(results, known):
    baseline_ok = True
    for result in results:
        if result['status'] == 'PASS':
            result['classification'] = 'passing'
            continue
        rule = known.get(result['name'])
        accepted = False
        if rule and 'minimum_passes' in rule:
            match = re.match(r'(\d+)/(\d+) independent assertions', result['detail'])
            accepted = bool(match and int(match[1]) >= rule['minimum_passes'] and int(match[2]) == rule['total'])
            if accepted and 'allowed_failed_groups' in rule:
                marker = '; failed groups: '
                groups = result['detail'].split(marker, 1)[1].split(' | ') if marker in result['detail'] else None
                accepted = groups is not None and set(groups) <= set(rule['allowed_failed_groups'])
        elif rule:
            accepted = all(part in result['detail'] for part in rule['detail_contains'])
        result['classification'] = 'known compatibility gap' if accepted else 'unclassified regression'
        if accepted:
            result['reason'] = rule['reason']
        else:
            baseline_ok = False
    return baseline_ok


def invoke(binary, arguments, expected, environment=None):
    process = subprocess.run([str(binary), *map(str, arguments)], cwd=ROOT,
                             env=environment, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=600)
    print(process.stdout, end='', flush=True)
    results = parse_results(process.stdout)
    if sorted(result['name'] for result in results) != sorted(expected):
        raise ValueError(f"Runner omitted/duplicated results (exit {process.returncode}); refusing partial success")
    if process.returncode and all(result['status'] == 'PASS' for result in results):
        raise ValueError("Runner failed despite reporting passing results")
    return results


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--fetch', action='store_true', help='fetch pinned public fixtures into ignored target/; never commercial games')
    parser.add_argument('--baseline', action='store_true', help='CI regression baseline; explicit known failures remain FAIL')
    parser.add_argument('--jsmolka-dir', type=Path, default=ROOT / 'target/gba-independent-jsmolka')
    parser.add_argument('--mgba-rom', type=Path, default=ROOT / 'target/gba-independent-binary/suite.gba')
    parser.add_argument('--mgba-elf', type=Path, help='validate a locally built ROM/ELF pair instead of the pinned published binary')
    parser.add_argument('--mgba-source', type=Path, default=ROOT / 'target/gba-independent-suite')
    parser.add_argument('--gameplay', action='store_true', help='also require the local ROM/checkpoint scene matrix')
    parser.add_argument('--rom-dir', type=Path, default=ROOT / 'roms')
    parser.add_argument('--state-dir', type=Path, default=ROOT / 'target')
    parser.add_argument('--cases', type=Path, default=MANIFEST, help='scene manifest; override with reviewed gameplay checkpoints/input/reference hashes')
    parser.add_argument('--target-dir', type=Path, default=ROOT / 'target')
    parser.add_argument('--report', type=Path, default=ROOT / 'target/gba-compatibility-report.json')
    parser.add_argument('--no-build', action='store_true', help='use already built release runner executables')
    args = parser.parse_args()
    config = json.loads(args.cases.read_text())
    independent = config['independent']
    args.report.parent.mkdir(parents=True, exist_ok=True)
    report = {'schema': 1, 'mode': 'baseline' if args.baseline else 'strict', 'release_ready': False,
              'fixtures': independent, 'results': [], 'gameplay_requested': args.gameplay}
    try:
        if args.fetch and not args.jsmolka_dir.is_dir():
            fixture = independent['jsmolka']
            checkout_archive(fixture['repository'], fixture['revision'], args.jsmolka_dir)
        verify_jsmolka(args.jsmolka_dir, independent['jsmolka'])
        mgba = independent['mgba']
        if args.mgba_elf:
            revision = subprocess.check_output(['git', '-C', str(args.mgba_source), 'rev-parse', 'HEAD'], text=True).strip()
            if revision != mgba['source_revision']:
                raise ValueError("Locally built mGBA source checkout differs from pinned revision")
            if subprocess.check_output(['git', '-C', str(args.mgba_source), 'status', '--porcelain'], text=True).strip():
                raise ValueError("Locally built mGBA source checkout has modifications")
            verify_built_elf(args.mgba_rom, args.mgba_elf)
            report['mgba_binary_provenance'] = 'locally supplied ROM/ELF pair, matching segments and clean pinned source checkout'
        else:
            if args.fetch and not args.mgba_rom.is_file():
                archive_bytes = download(mgba['archive_url'])
                if hashlib.sha256(archive_bytes).hexdigest() != mgba['archive_sha256']:
                    raise ValueError("Published mGBA archive changed; review and repin it before running")
                args.mgba_rom.parent.mkdir(parents=True, exist_ok=True)
                args.mgba_rom.write_bytes(zipfile.ZipFile(io.BytesIO(archive_bytes)).read('suite.gba'))
            if not args.mgba_rom.is_file() or digest(args.mgba_rom) != mgba['rom_sha256']:
                raise ValueError("Missing/modified pinned independent mGBA test ROM")
            report['mgba_binary_provenance'] = mgba['provenance']
        environment = dict(os.environ)
        if shutil.which('rustup') and 'RUSTC' not in environment:
            environment['RUSTC'] = subprocess.check_output(['rustup', 'which', 'rustc'], cwd=ROOT, text=True).strip()
        if not args.no_build:
            command = ['cargo', 'build', '--locked', '--release', '-p', 'rustboy-gba', '--target-dir', str(args.target_dir)]
            for name in ['conformance', 'mgba_conformance', 'gameplay_regression']:
                command += ['--example', name]
            subprocess.run(command, cwd=ROOT, env=environment, check=True)
        executables = args.target_dir / 'release/examples'
        names = independent['jsmolka']['required'] + independent['jsmolka']['diagnostic']
        report['results'] += invoke(executables / 'conformance', [args.jsmolka_dir, *names], names, environment)
        names = mgba['suites']
        report['results'] += invoke(executables / 'mgba_conformance', [args.mgba_rom, *names], ['mgba-' + name for name in names], environment)
        if args.gameplay:
            for case in config['gameplay']:
                candidates = [path for path in args.rom_dir.rglob('*.gba') if path.name == case['rom']]
                state = args.state_dir / case['checkpoint']
                if len(candidates) != 1 or digest(candidates[0]) != case['sha256'] or not state.is_file():
                    report['results'].append({'name': case['id'], 'status': 'FAIL', 'detail': 'Required local ROM/checkpoint missing or ROM identity differs', 'coverage': case['coverage']})
                    continue
                if case.get('checkpoint_sha256') and digest(state) != case['checkpoint_sha256']:
                    raise ValueError(f"Gameplay checkpoint differs from reviewed scene: {state}")
                case_environment = dict(environment)
                if case.get('frame_hash'):
                    case_environment['GBA_EXPECT_FRAME_HASH'] = case['frame_hash']
                result = invoke(executables / 'gameplay_regression', [candidates[0], case['frames'], case['inputs'], state, args.report.parent / 'gba-gameplay' / case['id']], ['gameplay'], case_environment)[0]
                result.update(name=case['id'], coverage=case['coverage'])
                report['results'].append(result)
        baseline_ok = classify(report['results'], config['known_failures'])
        report['baseline_ok'] = baseline_ok
        report['release_ready'] = all(result['status'] == 'PASS' for result in report['results'])
        print(f"Independent compatibility: {sum(r['status'] == 'PASS' for r in report['results'])}/{len(report['results'])} suites pass; release_ready={report['release_ready']}; baseline_ok={baseline_ok}")
        return 0 if (baseline_ok if args.baseline else report['release_ready']) else 1
    except (OSError, ValueError, subprocess.SubprocessError, zipfile.BadZipFile) as error:
        report['error'] = str(error)
        print(f"Compatibility gate error: {error}", file=sys.stderr)
        return 2
    finally:
        args.report.write_text(json.dumps(report, indent=2) + '\n')
        print(f"Compatibility report: {args.report}")


if __name__ == '__main__':
    raise SystemExit(main())

#!/usr/bin/env python3
"""Optional local commercial cable regression. No cartridge bytes are bundled.

Each reviewed scene boots both endpoints twice. PASS requires the pinned local
ROM, both reviewed final RGBA hashes, and identical final snapshots/battery data.
The native probe's OBSERVED message alone is never interpreted as success.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import tempfile

ROOT = Path(__file__).resolve().parents[1]


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def validate(case):
    if not re.fullmatch(r'[a-z0-9-]+', case.get('id', '')):
        raise ValueError('Invalid link gameplay case identifier')
    if not isinstance(case.get('rom'), str) or '/' in case['rom'] or '\\' in case['rom']:
        raise ValueError('Expected a local cartridge filename')
    if not re.fullmatch(r'[0-9a-f]{64}', case.get('sha256', '')):
        raise ValueError('Missing pinned local ROM identity')
    if type(case.get('frames')) is not int or not 1 <= case['frames'] <= 36000:
        raise ValueError('Frame budget must be 1–36000')
    for field in ['player1', 'player2']:
        text = case[field]
        if text == '-' or field == 'player2' and text == 'download':
            continue
        previous = -1
        for event in text.split(','):
            frame, mask = event.split(':')
            frame, mask = int(frame), int(mask, 16)
            if not previous < frame < case['frames'] or not 0 <= mask <= 0x3ff:
                raise ValueError('Input events must be ordered and inside the frame budget')
            previous = frame
    expected = case.get('frame_hashes', {})
    if set(expected) != {'player1', 'player2'} or any(not re.fullmatch(r'[0-9a-f]{16}', value) for value in expected.values()):
        raise ValueError('Both reviewed endpoint frame hashes are required')


def run_probe(binary, rom, case, output, environment):
    process = subprocess.run([str(binary), str(rom), str(case['frames']), case['player1'], case['player2'], str(output)],
                             cwd=ROOT, env=environment, text=True, stdout=subprocess.PIPE,
                             stderr=subprocess.STDOUT, timeout=600)
    (output / 'probe.log').write_text(process.stdout)
    if process.returncode:
        raise ValueError(f"Native cable probe failed (exit {process.returncode}); see {output / 'probe.log'}")
    frames = {}
    for line in process.stdout.splitlines():
        match = re.fullmatch(r'FRAMEHASH\t(player[12]|[12])\t([0-9a-f]{16})', line)
        if match:
            player = match[1] if match[1].startswith('player') else 'player' + match[1]
            if player in frames:
                raise ValueError('Duplicate endpoint frame result')
            frames[player] = match[2]
    if frames != case['frame_hashes']:
        raise ValueError(f"Reviewed scene changed: expected {case['frame_hashes']}, got {frames}; inspect {output}")
    observed = [re.fullmatch(r'RESULT\tlink-gameplay\tOBSERVED\t(\d+) frames; (\d+) real cable exchanges; (\d+) absent-peer transfers;.*', line) for line in process.stdout.splitlines()]
    observed = [value for value in observed if value]
    if len(observed) != 1 or int(observed[0][1]) != case['frames'] or int(observed[0][2]) < case.get('minimum_transfers', 1):
        raise ValueError('Native probe did not finish the requested frame budget with actual cable exchanges')
    artifacts = {}
    for player in (1, 2):
        for kind in ('state.bin', 'battery.sav'):
            path = output / f'p{player}-{kind}'
            if not path.is_file() or kind == 'state.bin' and path.stat().st_size < 500000:
                raise ValueError(f'Missing or incomplete final endpoint artifact: {path}')
            artifacts[path.name] = digest(path)
    return {'frame_hashes': frames, 'artifacts': artifacts}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--rom-dir', type=Path, default=ROOT / 'roms')
    parser.add_argument('--manifest', type=Path, default=ROOT / 'tests/gba_link_gameplay_cases.json')
    parser.add_argument('--target-dir', type=Path, default=ROOT / 'target')
    parser.add_argument('--output', type=Path, default=ROOT / 'target/gba-link-gameplay-regressions')
    parser.add_argument('--case', action='append', default=[], help='run one or more named cases')
    parser.add_argument('--no-build', action='store_true', help='use an already built matching release probe')
    args = parser.parse_args()
    for field in ('rom_dir', 'manifest', 'target_dir', 'output'):
        setattr(args, field, getattr(args, field).resolve())
    args.output.mkdir(parents=True, exist_ok=True)
    report = {'schema': 1, 'scope': 'local native commercial scene regression; remote-browser commercial gameplay is unverified', 'results': []}
    try:
        cases = json.loads(args.manifest.read_text())['cases']
        if args.case and set(args.case) - {case['id'] for case in cases}:
            raise ValueError('Unknown local link gameplay case')
        cases = [case for case in cases if not args.case or case['id'] in args.case]
        if not cases or len({case['id'] for case in cases}) != len(cases):
            raise ValueError('Manifest must contain distinct reviewed scenes')
        located = {}
        for case in cases:
            validate(case)
            matches = [path for path in args.rom_dir.rglob('*.gba') if path.name == case['rom']]
            if len(matches) != 1 or digest(matches[0]) != case['sha256']:
                raise ValueError(f"Required local cartridge is absent/ambiguous or differs from pinned identity: {case['rom']}")
            located[case['id']] = matches[0]
        environment = dict(os.environ)
        if shutil.which('rustup') and 'RUSTC' not in environment:
            environment['RUSTC'] = subprocess.check_output(['rustup', 'which', 'rustc'], cwd=ROOT, text=True).strip()
        if not args.no_build:
            subprocess.run(['cargo', 'build', '--locked', '--release', '-p', 'rustboy-gba', '--example', 'link_gameplay_probe', '--target-dir', str(args.target_dir)], cwd=ROOT, env=environment, check=True)
        binary = args.target_dir / 'release/examples' / ('link_gameplay_probe.exe' if os.name == 'nt' else 'link_gameplay_probe')
        for case in cases:
            directory = Path(tempfile.mkdtemp(prefix=case['id'] + '-', dir=args.output))
            runs = []
            try:
                for index in (1, 2):
                    output = directory / f'run{index}'
                    output.mkdir()
                    runs.append(run_probe(binary, located[case['id']], case, output, environment))
                if runs[0] != runs[1]:
                    raise ValueError('Full-startup replay changed endpoint state, battery data or displayed pixels')
                result = {'case': case['id'], 'status': 'PASS', 'coverage': case['coverage'], 'frames_per_run': case['frames'], 'runs': runs, 'output': str(directory)}
                print(f"PASS {case['id']}: both reviewed endpoint scenes, identical full state/battery after two startup runs")
            except (OSError, ValueError, subprocess.SubprocessError) as error:
                result = {'case': case['id'], 'status': 'FAIL', 'error': str(error), 'output': str(directory)}
                print(f"FAIL {case['id']}: {error}")
            report['results'].append(result)
        return 0 if all(result['status'] == 'PASS' for result in report['results']) else 1
    except (OSError, ValueError, KeyError, subprocess.SubprocessError) as error:
        report['error'] = str(error)
        print(f'Local link gameplay gate error: {error}', file=sys.stderr)
        return 2
    finally:
        (args.output / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
        print(f"Local link gameplay report: {args.output / 'report.json'}")


if __name__ == '__main__':
    raise SystemExit(main())

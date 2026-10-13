"""Release-gate failure classification must never turn failures into passes."""
import importlib.util
from pathlib import Path
import struct
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location('gba_gate', ROOT / 'scripts/check_gba_compatibility.py')
gate = importlib.util.module_from_spec(spec)
spec.loader.exec_module(gate)


class CompatibilityGateTests(unittest.TestCase):
    def test_known_failure_stays_failed_and_new_regressions_block_baseline(self):
        known = {'timing': {'minimum_passes': 564, 'total': 2020, 'reason': 'measured timing gap'}}
        result = gate.parse_results('RESULT\ttiming\tFAIL\t564/2020 independent assertions\n')
        self.assertTrue(gate.classify(result, known))
        self.assertEqual(result[0]['status'], 'FAIL')
        self.assertEqual(result[0]['classification'], 'known compatibility gap')
        for counters in ['563/2020', '564/2019', '565/2021']:
            result = gate.parse_results(f'RESULT\ttiming\tFAIL\t{counters} independent assertions\n')
            self.assertFalse(gate.classify(result, known), counters)
        unknown = gate.parse_results('RESULT\tdma\tFAIL\t1/2 independent assertions\n')
        self.assertFalse(gate.classify(unknown, known))
        known['timing']['allowed_failed_groups'] = ['calibration']
        regression = gate.parse_results('RESULT\ttiming\tFAIL\t600/2020 independent assertions; failed groups: new IRQ failure\n')
        self.assertFalse(gate.classify(regression, known), 'A numerical improvement cannot hide a newly failing group')

    def test_fault_exceptions_require_the_specific_verified_failure(self):
        known = {'bios': {'detail_contains': ['r12=1,terminal=true', 'fault=None'], 'reason': 'different firmware identity'}}
        valid = gate.parse_results('RESULT\tbios\tFAIL\tPC=080003C0,r12=1,terminal=true,fault=None\n')
        self.assertTrue(gate.classify(valid, known))
        for detail in ['r12=2,terminal=true,fault=None', 'r12=1,terminal=false,fault=None', 'r12=1,terminal=true,fault=Some(failure)']:
            result = gate.parse_results('RESULT\tbios\tFAIL\t' + detail)
            self.assertFalse(gate.classify(result, known))

    def test_incomplete_or_inconsistent_runner_output_cannot_pass(self):
        with tempfile.TemporaryDirectory() as temporary:
            executable = Path(temporary) / 'runner'
            executable.write_text('#!/usr/bin/env python3\nprint("RESULT\\tarm\\tPASS\\tverified")\n')
            executable.chmod(0o755)
            with self.assertRaisesRegex(ValueError, 'omitted/duplicated'):
                gate.invoke(executable, [], ['arm', 'thumb'])
            executable.write_text('#!/usr/bin/env python3\nprint("RESULT\\tarm\\tPASS\\tverified")\nraise SystemExit(1)\n')
            with self.assertRaisesRegex(ValueError, 'failed despite'):
                gate.invoke(executable, [], ['arm'])

    def test_supplied_elf_must_match_test_rom_segments(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            rom = bytearray(256)
            rom[192:] = bytes(range(64))
            elf = bytearray(512)
            struct.pack_into('<16sHHIIIIIHHHHHH', elf, 0, b'\x7fELF\x01\x01\x01' + bytes(9), 2, 40, 1, 0x08000000, 52, 0, 0, 52, 32, 1, 0, 0, 0)
            struct.pack_into('<8I', elf, 52, 1, 256, 0x08000000, 0x08000000, 256, 256, 5, 4)
            elf[256:] = rom
            (root / 'suite.gba').write_bytes(rom)
            (root / 'suite.elf').write_bytes(elf)
            gate.verify_built_elf(root / 'suite.gba', root / 'suite.elf')
            rom[200] ^= 1
            (root / 'suite.gba').write_bytes(rom)
            with self.assertRaisesRegex(ValueError, 'does not describe'):
                gate.verify_built_elf(root / 'suite.gba', root / 'suite.elf')


if __name__ == '__main__':
    unittest.main()

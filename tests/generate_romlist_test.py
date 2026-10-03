"""Local ROM picker manifests include GBA without reading or publishing games."""
import importlib.util
from pathlib import Path
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location('generate_romlist', ROOT / 'generate_romlist.py')
generator = importlib.util.module_from_spec(spec)
spec.loader.exec_module(generator)


class ManifestTests(unittest.TestCase):
    def test_all_formats_nested_paths_artwork_and_sorting(self):
        with tempfile.TemporaryDirectory(prefix='rustboy-romlist-test-') as temporary:
            root = Path(temporary)
            for name in ('Zelda.gbc', 'Tetris.gb', 'Advance/Fire Red.GBA', 'images/ignored.gba',
                         'images/Fire Red-thumb.png', 'images/Fire Red-image.png', 'firmware.bin'):
                path = root / name
                path.parent.mkdir(parents=True, exist_ok=True)
                path.touch()
            self.assertEqual(generator.collect_roms(root), [
                {'name': 'Advance/Fire Red.GBA', 'img': 'images/Fire Red-thumb.png'},
                {'name': 'Tetris.gb'}, {'name': 'Zelda.gbc'},
            ])

    def test_missing_directory_is_an_empty_library(self):
        with tempfile.TemporaryDirectory(prefix='rustboy-romlist-test-') as temporary:
            self.assertEqual(generator.collect_roms(Path(temporary) / 'missing'), [])


if __name__ == '__main__':
    unittest.main()

"""Offline tests of packaging, integrity and license/source inclusion."""
import hashlib
import importlib.util
import json
from pathlib import Path
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("prepare_pages", ROOT / "scripts/prepare_pages.py")
pages = importlib.util.module_from_spec(spec)
spec.loader.exec_module(pages)


class PackagingTests(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory(prefix="rustboy-pages-test-")
        self.addCleanup(self.directory.cleanup)
        self.root = Path(self.directory.name) / "repo"
        for folder in ("out", "web", "homebrew/licenses", "roms", "testroms"):
            (self.root / folder).mkdir(parents=True)
        for name in ("index.html", "LICENSE", "out/rustboy.js", "out/rustboy_bg.wasm",
                     "web/library-rom.mjs", "homebrew/licenses/GPL.txt", "roms/private.gb",
                     "roms/cgb_boot.bin", "testroms/private.gb"):
            (self.root / name).write_text("fixture")
        self.rom, self.source = b"ROM", b"source archive"
        artifact = lambda name, data: {"name": name, "url": "https://example.org/" + name,
                                     "size": len(data), "sha256": hashlib.sha256(data).hexdigest()}
        self.catalog = [{"id": "demo", "title": "Demo <game>", "author": "Author",
                         "description": "A game", "platform": "GBC", "license": "GPL-3.0",
                         "homepage": "https://example.org/", "source_page": "https://example.org/source",
                         "notices": ["licenses/GPL.txt"], "rom": artifact("demo.gbc", self.rom),
                         "source": artifact("sources/demo.zip", self.source)}]
        self.output = Path(self.directory.name) / "site"

    def stage(self, download=None):
        (self.root / "homebrew/catalog.json").write_text(json.dumps(self.catalog))
        pages.prepare(self.output, self.root, download or (lambda a: self.rom if a["name"].endswith(".gbc") else self.source))

    def test_site_contains_only_public_artifacts_and_corresponding_source(self):
        self.stage()
        self.assertFalse((self.output / "roms").exists())
        self.assertFalse((self.output / "testroms").exists())
        self.assertEqual((self.output / "homebrew/demo.gbc").read_bytes(), self.rom)
        self.assertEqual((self.output / "homebrew/sources/demo.zip").read_bytes(), self.source)
        self.assertTrue((self.output / "homebrew/licenses/GPL.txt").is_file())
        self.assertTrue((self.output / "web/library-rom.mjs").is_file())
        credits = (self.output / "homebrew/credits.html").read_text()
        self.assertIn("Demo &lt;game&gt;", credits)
        self.assertIn('href="sources/demo.zip"', credits)
        self.assertIn('href="licenses/GPL.txt"', credits)
        manifest = json.loads((self.output / "homebrew/romlist.json").read_text())
        self.assertEqual(manifest[0]["sha256"], self.catalog[0]["rom"]["sha256"])

    def test_bad_download_publishes_nothing(self):
        with self.assertRaisesRegex(ValueError, "mismatch"):
            self.stage(lambda a: b"corrupt")
        self.assertFalse(self.output.exists())

    def test_gpl_game_without_source_is_rejected(self):
        del self.catalog[0]["source"]
        with self.assertRaisesRegex(ValueError, "corresponding source"):
            self.stage()

    def test_missing_notice_is_rejected(self):
        self.catalog[0]["notices"] = ["licenses/missing.txt"]
        with self.assertRaisesRegex(ValueError, "license notice"):
            self.stage()

    def test_unsafe_paths_and_duplicates_are_rejected(self):
        for name in ("../bad.gb", "/bad.gb", "a/../../bad.gb", "a\\bad.gb"):
            with self.assertRaises(ValueError):
                pages.safe_path(name)
        self.catalog.append(self.catalog[0])
        with self.assertRaisesRegex(ValueError, "Duplicate"):
            self.stage()

    def test_no_overwriting_existing_output(self):
        self.output.mkdir()
        (self.output / "keep.txt").write_text("user data")
        with self.assertRaisesRegex(ValueError, "empty/new"):
            self.stage()
        self.assertEqual((self.output / "keep.txt").read_text(), "user data")

    def test_actual_catalog_is_well_formed_with_available_license_notices(self):
        pages.validate_catalog(json.loads((ROOT / "homebrew/catalog.json").read_text()), ROOT / "homebrew")


if __name__ == "__main__":
    unittest.main()

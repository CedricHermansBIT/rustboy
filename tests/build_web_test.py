"""Build-tool compatibility checks without compiling or replacing the bundle."""
import contextlib
import importlib.util
import io
from pathlib import Path
import sys
import tempfile
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("build_web", ROOT / "scripts/build_web.py")
build = importlib.util.module_from_spec(spec)
spec.loader.exec_module(build)


class BuildToolTests(unittest.TestCase):
    def test_tested_optimizer_versions_are_accepted(self):
        for output in ["wasm-opt version 117 (version_117)\n", "wasm-opt version 126\n"]:
            with self.subTest(output=output), patch.object(build.subprocess, "check_output", return_value=output):
                self.assertGreaterEqual(build.check_wasm_opt_version("wasm-opt"), 117)

    def test_old_and_unidentified_optimizer_versions_are_rejected(self):
        for output in ["wasm-opt version 108 (version_108)", "wasm-opt version 116", "unknown", ""]:
            with self.subTest(output=output), patch.object(build.subprocess, "check_output", return_value=output):
                with self.assertRaises(ValueError):
                    build.check_wasm_opt_version("wasm-opt")

    def test_unsupported_optimizer_cannot_build_or_replace_existing_output(self):
        with tempfile.TemporaryDirectory(prefix="rustboy-build-test-") as directory:
            root = Path(directory)
            (root / "Cargo.lock").write_text('[[package]]\nname="wasm-bindgen"\nversion="0.2.118"\n')
            (root / "out").mkdir()
            bundle = root / "out/rustboy_bg.wasm"
            bundle.write_bytes(b"previous working bundle")
            output = io.StringIO()

            def version(command, **kwargs):
                return "wasm-bindgen 0.2.118" if command[0] == "wasm-bindgen" else "wasm-opt version 108"

            with patch.object(build, "ROOT", root), patch.object(sys, "argv", ["build_web.py"]), \
                    patch.object(build.shutil, "which", return_value="/tools/wasm-opt"), \
                    patch.object(build.subprocess, "check_output", side_effect=version), \
                    patch.object(build.subprocess, "run") as run, contextlib.redirect_stderr(output):
                with self.assertRaises(SystemExit) as stopped:
                    build.main()
                self.assertEqual(stopped.exception.code, 2)
                run.assert_not_called()
            self.assertIn("117 or newer is required; found 108", output.getvalue())
            self.assertEqual(bundle.read_bytes(), b"previous working bundle")


if __name__ == "__main__":
    unittest.main()

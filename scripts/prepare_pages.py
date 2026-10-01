#!/usr/bin/env python3
"""Stage a clean Pages site with hash-pinned, redistributable homebrew.

No commercial/local ROM directories or external boot firmware are copied.
ROMs and corresponding source archives are downloaded into the deployment
artifact only. Run after scripts/build_web.py; --output must be empty/new.
"""
import argparse
from concurrent.futures import ThreadPoolExecutor
import hashlib
import html
import json
from pathlib import Path, PurePosixPath
import re
import shutil
import urllib.parse
import urllib.request

ROOT = Path(__file__).resolve().parents[1]
MAX_DOWNLOAD = 8 * 1024 * 1024


def safe_path(name):
    if not isinstance(name, str) or not name or "\\" in name:
        raise ValueError(f"Invalid artifact path: {name!r}")
    path = PurePosixPath(name)
    if path.is_absolute() or any(part in ("", ".", "..") for part in name.split("/")):
        raise ValueError(f"Unsafe artifact path: {name}")
    return path


def validate_catalog(catalog, notice_root):
    if not isinstance(catalog, list) or not catalog:
        raise ValueError("Homebrew catalog must be a nonempty array")
    ids, names = set(), set()
    for entry in catalog:
        if not isinstance(entry, dict) or not re.fullmatch(r"[a-z0-9-]+", entry.get("id", "")):
            raise ValueError("Invalid homebrew identifier")
        if entry["id"] in ids:
            raise ValueError("Duplicate homebrew identifier")
        ids.add(entry["id"])
        for field in ("title", "author", "description", "platform", "license"):
            if not isinstance(entry.get(field), str) or not entry[field]:
                raise ValueError(f"Missing {field} for {entry['id']}")
        for field in ("homepage", "source_page"):
            if urllib.parse.urlparse(entry.get(field, "")).scheme != "https":
                raise ValueError(f"Invalid {field} for {entry['id']}")
        if not entry.get("notices"):
            raise ValueError(f"Missing license notices for {entry['id']}")
        for name in entry["notices"]:
            path = safe_path(name)
            if path.parts[0] != "licenses" or not (notice_root / path).is_file():
                raise ValueError(f"Missing local license notice: {name}")
        if "GPL-" in entry["license"] and "source" not in entry:
            raise ValueError(f"GPL game requires corresponding source: {entry['id']}")
        for kind in ("rom", "source"):
            if kind not in entry and kind == "source":
                continue
            artifact = entry[kind]
            name = str(safe_path(artifact["name"]))
            if name in names:
                raise ValueError(f"Duplicate artifact: {name}")
            names.add(name)
            if kind == "rom" and ("/" in name or not name.endswith((".gb", ".gbc"))):
                raise ValueError(f"Invalid ROM filename: {name}")
            if kind == "source" and not name.startswith("sources/"):
                raise ValueError(f"Invalid source archive path: {name}")
            if urllib.parse.urlparse(artifact["url"]).scheme != "https":
                raise ValueError(f"Downloads must use HTTPS: {name}")
            if not re.fullmatch(r"[0-9a-f]{64}", artifact["sha256"]):
                raise ValueError(f"Missing/invalid SHA-256: {name}")
            size = artifact.get("size", MAX_DOWNLOAD)
            if type(size) is not int or not 0 < size <= MAX_DOWNLOAD:
                raise ValueError(f"Invalid download size: {name}")


def fetch_artifact(artifact):
    request = urllib.request.Request(artifact["url"], headers={"User-Agent": "RustBoy-homebrew-build/1"})
    with urllib.request.urlopen(request, timeout=45) as response:
        if urllib.parse.urlparse(response.url).scheme != "https":
            raise ValueError("Download redirected away from HTTPS")
        data = response.read(MAX_DOWNLOAD + 1)
    verify_artifact(artifact, data)
    return data


def verify_artifact(artifact, data):
    if len(data) > MAX_DOWNLOAD or ("size" in artifact and len(data) != artifact["size"]):
        raise ValueError(f"Download size mismatch: {artifact['name']}")
    if hashlib.sha256(data).hexdigest() != artifact["sha256"]:
        raise ValueError(f"SHA-256 mismatch: {artifact['name']}")


def credits_page(catalog):
    escape = html.escape
    sections = []
    for entry in catalog:
        links = " · ".join(f'<a href="{escape(name)}">{escape(Path(name).name)}</a>'
                           for name in entry["notices"])
        source = entry.get("source")
        source_download = (f'<p><a href="{escape(source["name"])}">Download corresponding source</a>'
                           f' (SHA-256: <code>{source["sha256"]}</code>)</p>') if source else ""
        sections.append(f'''<section id="{escape(entry['id'])}">
<h2>{escape(entry['title'])}</h2><p>{escape(entry['author'])}</p>
<p>{escape(entry['license'])}. Games and assets are unmodified.</p>
<p><a href="{escape(entry['homepage'])}">Creator / project</a> ·
<a href="{escape(entry['source_page'])}">Pinned upstream source</a></p>
{source_download}<p>Full license notices: {links}</p>
<p>{escape(entry.get('provenance', 'Unmodified upstream ROM at the pinned commit.'))}</p>
<p>ROM SHA-256: <code>{entry['rom']['sha256']}</code></p></section>''')
    return '''<!doctype html><html lang="en"><meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>RustBoy homebrew credits and licenses</title>
<style>body{max-width:850px;margin:2rem auto;padding:0 1rem;font:16px/1.6 system-ui;background:#161c16;color:#e4eee4}a{color:#a8e8a8}code{overflow-wrap:anywhere}section{border-top:1px solid #506050;margin-top:2rem}</style>
<h1>Homebrew credits and licenses</h1><p><a href="../index.html">Back to RustBoy</a></p>
<p>These independently authored games are redistributable under their respective
licenses, not owned by or endorsed by RustBoy. Support their creators through
the project links below. License terms apply to downloaded ROMs too.</p>
''' + "\n".join(sections) + "\n</html>\n"


def prepare(destination, root=ROOT, download=fetch_artifact):
    root, destination = root.resolve(), Path(destination).resolve()
    if destination == root or destination in root.parents:
        raise ValueError("The output cannot be the repository or an ancestor")
    if destination.exists() and any(destination.iterdir()):
        raise ValueError("Output must be empty/new; choose another --output directory")
    catalog = json.loads((root / "homebrew/catalog.json").read_text())
    validate_catalog(catalog, root / "homebrew")
    for name in ("index.html", "out/rustboy.js", "out/rustboy_bg.wasm", "LICENSE"):
        if not (root / name).is_file():
            raise ValueError(f"Missing {name}; run scripts/build_web.py first")
    artifacts = [entry[kind] for entry in catalog for kind in ("rom", "source") if kind in entry]
    # Verify every payload before publishing any part of the site.
    with ThreadPoolExecutor(max_workers=4) as pool:
        payloads = list(pool.map(download, artifacts))
    for artifact, data in zip(artifacts, payloads):
        verify_artifact(artifact, data)
    destination.mkdir(parents=True, exist_ok=True)
    for name in ("index.html", "LICENSE"):
        shutil.copy2(root / name, destination / name)
    for name in ("out", "web"):
        shutil.copytree(root / name, destination / name, ignore=shutil.ignore_patterns("__pycache__"))
    output = destination / "homebrew"
    shutil.copytree(root / "homebrew/licenses", output / "licenses")
    for artifact, data in zip(artifacts, payloads):
        target = output / artifact["name"]
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes(data)
        print(f"Verified {artifact['name']}: {len(data)} bytes", flush=True)
    manifest = [{**{k: entry[k] for k in ("id", "title", "author", "platform", "description", "license")},
                 **{k: entry["rom"][k] for k in ("name", "size", "sha256")},
                 "credits": f"credits.html#{entry['id']}"} for entry in catalog]
    (output / "romlist.json").write_text(json.dumps(manifest, ensure_ascii=False, indent=2) + "\n")
    (output / "credits.html").write_text(credits_page(catalog))
    shutil.copy2(root / "homebrew/catalog.json", output / "catalog.json")
    print(f"Pages site ready: {destination} (no private ROMs or external firmware copied)", flush=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, default=ROOT / "dist")
    args = parser.parse_args()
    try:
        prepare(args.output)
    except (ValueError, OSError, KeyError) as error:
        parser.exit(1, f"Could not stage Pages site: {error}\n")


if __name__ == "__main__":
    main()

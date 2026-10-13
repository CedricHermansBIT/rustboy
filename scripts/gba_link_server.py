#!/usr/bin/env python3
"""Serve RustBoy and relay two-player WebRTC signaling (never game data).

Run behind an HTTPS reverse proxy for remote devices. The default bind is local.
Only public frontend assets are served; cartridge/save directories are excluded.
"""
import argparse
import base64
import hashlib
import json
import mimetypes
from pathlib import Path
import secrets
import struct
import threading
import time
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from urllib.parse import unquote, urlsplit

ROOT = Path(__file__).resolve().parent.parent
MAX_MESSAGE = 200_000
ROOMS = {}
LOCK = threading.RLock()


class Peer:
    def __init__(self, handler):
        self.socket = handler.connection
        self.lock = threading.Lock()
        self.room = None
        self.player = None

    def send(self, packet, opcode=1):
        body = json.dumps(packet, separators=(",", ":")).encode() if opcode == 1 else packet
        length = len(body)
        header = bytes([0x80 | opcode])
        header += bytes([length]) if length < 126 else b"\x7e" + struct.pack(">H", length) if length < 65536 else b"\x7f" + struct.pack(">Q", length)
        with self.lock:
            self.socket.sendall(header + body)


class Handler(BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def do_GET(self):
        path = unquote(urlsplit(self.path).path)
        if path == "/gba-link/rooms":
            self.websocket()
            return
        if path == "/":
            path = "/index.html"
        parts = Path(path.lstrip("/")).parts
        allowed = path == "/index.html" or parts and parts[0] in {"out", "web", "assets", "img", "images", "fonts"}
        if path in {"/roms/romlist.json", "/testroms/romlist.json", "/homebrew/romlist.json"}:
            self.respond(b"[]", "application/json")
            return
        file = (ROOT / path.lstrip("/")).resolve()
        resolved_public = file.is_relative_to(ROOT) and (file == ROOT / "index.html" or file.relative_to(ROOT).parts and file.relative_to(ROOT).parts[0] in {"out", "web", "assets", "img", "images", "fonts"})
        if not allowed or ".." in parts or not resolved_public or not file.is_file():
            self.send_error(404)
            return
        self.respond(file.read_bytes(), mimetypes.guess_type(file.name)[0] or "application/octet-stream")

    def respond(self, body, content_type):
        if content_type == "text/javascript" or self.path.endswith(".mjs"):
            content_type = "application/javascript"
        self.send_response(200)
        self.send_header("Content-Type", content_type)
        self.send_header("Content-Length", str(len(body)))
        self.send_header("Cache-Control", "no-cache")
        self.end_headers()
        self.wfile.write(body)

    def websocket(self):
        key = self.headers.get("Sec-WebSocket-Key", "")
        origin = urlsplit(self.headers.get("Origin", ""))
        try:
            valid_key = len(base64.b64decode(key, validate=True)) == 16
        except ValueError:
            valid_key = False
        origin_header = self.headers.get("Origin")
        valid_origin = origin_header is None or origin_header in self.server.allowed_origins or origin.scheme in {"http", "https"} and bool(origin.netloc) and origin.netloc == self.headers.get("Host")
        if not valid_key or self.headers.get("Upgrade", "").lower() != "websocket" or self.headers.get("Sec-WebSocket-Version") != "13" or not valid_origin:
            self.send_error(400)
            return
        accept = base64.b64encode(hashlib.sha1((key + "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").encode()).digest()).decode()
        self.send_response(101)
        self.send_header("Upgrade", "websocket")
        self.send_header("Connection", "Upgrade")
        self.send_header("Sec-WebSocket-Accept", accept)
        self.end_headers()
        peer = Peer(self)
        self.connection.settimeout(600)
        try:
            while True:
                head = self.read_exact(2)
                opcode = head[0] & 15
                if head[0] & 0xf0 != 0x80 or not head[1] & 0x80:
                    raise ValueError("Unsupported WebSocket frame")
                size = head[1] & 127
                if size == 126:
                    size = struct.unpack(">H", self.read_exact(2))[0]
                elif size == 127:
                    size = struct.unpack(">Q", self.read_exact(8))[0]
                if size > MAX_MESSAGE or opcode >= 8 and size > 125:
                    raise ValueError("Signaling message too large")
                mask = self.read_exact(4)
                body = bytes(byte ^ mask[i % 4] for i, byte in enumerate(self.read_exact(size)))
                if opcode == 8:
                    peer.send(body, 8)
                    break
                if opcode == 9:
                    peer.send(body, 10)
                    continue
                if opcode == 10:
                    continue
                if opcode != 1:
                    raise ValueError("Only text signaling is supported")
                self.signal(peer, json.loads(body))
        except (OSError, EOFError, ValueError, TypeError, KeyError):
            pass
        finally:
            with LOCK:
                room = ROOMS.get(peer.room)
                if room and peer in room["peers"]:
                    room["peers"].remove(peer)
                    for other in room["peers"]:
                        try:
                            other.send({"type": "peer-left"})
                        except OSError:
                            pass
                    if not room["peers"]:
                        del ROOMS[peer.room]
            self.close_connection = True

    def read_exact(self, count):
        data = self.rfile.read(count)
        if len(data) != count:
            raise EOFError()
        return data

    def signal(self, peer, packet):
        if not isinstance(packet, dict):
            raise ValueError("Invalid signaling message")
        with LOCK:
            kind = packet.get("type")
            if kind in {"create", "join"}:
                if peer.room is not None:
                    raise ValueError("Already in a room")
                if kind == "create":
                    if len(ROOMS) >= 256:
                        peer.send({"type": "error", "message": "Room server is full"})
                        return
                    name = secrets.token_urlsafe(9)
                    ROOMS[name] = {"peers": [peer], "created": time.monotonic()}
                    peer.room, peer.player = name, 0
                    peer.send({"type": "joined", "room": name, "player": 0})
                else:
                    name = packet.get("room")
                    room = ROOMS.get(name) if isinstance(name, str) else None
                    if not room or len(room["peers"]) != 1 or room["peers"][0].player != 0 or time.monotonic() - room["created"] > 1200:
                        peer.send({"type": "error", "message": "Room is unavailable or already full"})
                        return
                    peer.room, peer.player = name, 1
                    room["peers"].append(peer)
                    peer.send({"type": "joined", "room": name, "player": 1})
                    room["peers"][0].send({"type": "peer-joined"})
            elif kind == "signal":
                room = ROOMS.get(peer.room)
                code = packet.get("code")
                if not room or len(room["peers"]) != 2 or not isinstance(code, str) or len(code) > MAX_MESSAGE - 100:
                    raise ValueError("Invalid pairing signal")
                for other in room["peers"]:
                    if other is not peer:
                        other.send({"type": "signal", "code": code})
            else:
                raise ValueError("Unknown signaling message")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--bind", default="127.0.0.1")
    parser.add_argument("--port", type=int, default=8765)
    parser.add_argument("--origin", action="append", default=[], help="Allow a static frontend origin, e.g. https://your-site.example")
    args = parser.parse_args()
    server = ThreadingHTTPServer((args.bind, args.port), Handler)
    server.daemon_threads = True
    server.allowed_origins = set(args.origin)
    print(f"RustBoy room server: http://{args.bind}:{server.server_address[1]}", flush=True)
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        pass
    finally:
        server.server_close()


if __name__ == "__main__":
    main()

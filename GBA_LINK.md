# GBA link across devices

RustBoy has an experimental two-player cable between browser instances. Devices
pair with handshake codes, including on GitHub Pages, and exchange data over an
ordered, reliable WebRTC channel. Invite rooms are optional. During play, each browser runs both its own GBA and a temporary local
copy of the other player's GBA. The two local machines exchange serial data
without waiting for a network round trip at every hardware transfer.

This is a RustBoy-to-RustBoy prototype. Synthetic transfer, IRQ and download
regressions pass. Local native startup probes now reach active Mario Kart Super
Circuit races in both Single Pak and Multi Pak modes. Commercial races across
real browser devices are still unverified, and this does not establish universal
game compatibility. The actual browser WASM regression sustained 59.9 fps on both endpoints with
80 ms simulated one-way network delay (160 ms round trip), completing 10,215
serial transfers in five seconds with 299 input packets per device. This original
test cartridge is lightweight; running two commercial games increases local work.

## Connect with handshake codes

1. Choose **Link** in the quick toolbar or ROM picker. Pairing does not require
   a loaded cartridge or a room server, and works with static hosting.
2. Player 1 chooses **Create link**, then **Copy code**, and sends the code to
   player 2 through their usual messaging app.
3. Player 2 pastes it into the peer-code field and chooses **Connect code**,
   then copies the generated reply code back to player 1.
4. Player 1 pastes the reply and chooses **Connect code**. Wait for both pages
   to report that their devices are connected.
5. Load compatible games on both pages, or choose **Send current game** on
   player 1 and **Receive and load game** on player 2. The received cartridge
   loads after size/header and SHA-256 checks.
6. Both choose **Start cable (restart games)**, close Settings and select the
   game's multiplayer mode. Starting preserves each device's own battery save
   and exchanges temporary session state, including RAM, RTC and save data.
   Resume either page if it was paused before opening Settings.

Both devices need the current build; previous pairing codes cannot be reused.
Some networks require the TURN relay configuration described below.

## Optional invite rooms

Expand **Invite rooms (optional)** to create a room and copy an invite link.
Opening that link automatically exchanges the handshake and opens link Settings.
This requires a reachable room server; GitHub Pages alone cannot run one.
The invite carries the frontend URL and configured room-server address. A room
admits two devices; a full, disconnected or expired room needs a new invite.
ROM sharing and starting the cable work the same way after pairing.

## Single Pak download play

ROM sharing and Single Pak are different paths. Sharing a game gives the other
browser a complete cartridge. Single Pak keeps the cartridge on player 1; the
host game uploads a smaller executable into player 2's GBA RAM through the
emulated serial cable.

For Single Pak, pair the devices, keep the host cartridge loaded on player 1,
and let player 2 choose **Join Single Pak (no cartridge)** instead of receiving a
ROM. Player 1 chooses **Start cable (restart games)**. Close Settings and choose
the host game's Single Pak mode. The invited page runs an original replacement
BIOS download client, not a Nintendo BIOS image or the host cartridge.

The host BIOS MultiBoot service supports normal 32-bit and multiplayer 16-bit
protocols. The built-in cartridge-free client currently supports multiplayer
16-bit. Original regressions cover serial upload, the download checksum, RAM
entry execution and snapshots during the upload. This establishes the protocol
path, not compatibility with every commercial download payload. The optional
Mario Kart Super Circuit native regression additionally boots the host, uploads
the RAM client through the serial protocol and reaches an active race on both
endpoints. Reviewed images show Lap 1/5 at about four seconds; a complete race
and the same session over a remote browser network remain unverified.
The paired simulation carries the download through its local serial model.
Network traffic during play carries frame inputs rather than every download word.

## Run a room server

Build `out/` first, then run from the repository:

```bash
python3 scripts/build_web.py
python3 scripts/gba_link_server.py --port 8765
```

Open `http://127.0.0.1:8765/` for a local test. The server binds to loopback by
default; that address is not reachable from another computer or phone.

For remote devices, expose the frontend and signaling endpoint through an HTTPS
reverse proxy or equivalent secure hosting. Forward WebSocket upgrades for
`/gba-link/rooms` to the Python server; preserve the external Host header and
allow sufficient idle time for pairing. Nginx requires explicit forwarding of
its Upgrade/Connection headers; see its
[WebSocket proxy documentation](https://nginx.org/en/docs/http/websocket.html).
The Python script itself does not terminate TLS.

A separately hosted static frontend, including Pages, needs a reachable WSS room
service. Allow that frontend's exact origin on the signaling server, for example:

```bash
python3 scripts/gba_link_server.py --port 8765 --origin https://your-site.example
```

Enter `wss://your-room-service.example/gba-link/rooms` under **Invite rooms (optional)
→ Room server** before creating a room. The invite includes this address, so the
other player can open it without entering a server themselves. Static hosting
alone does not run the room server. HTTPS pages require a WSS endpoint.

The server serves the frontend and public asset directories only. ROM/test-ROM
manifests are empty, cartridge/save directories and repository files are excluded,
and decoded traversal paths and symlink escapes are rejected. Rooms are held in
memory. The room service exchanges pairing descriptions; cartridge bytes, saves
and session snapshots are not uploaded to that service by this frontend.

## Network and file behavior

The default Google STUN server discovers connection addresses. Some cellular or
restrictive networks require a TURN relay. Enter its configuration under
**Network relay settings** on the devices that need it:

```json
[{"urls":"turn:your-relay:3478","username":"user","credential":"password"}]
```

Relay configuration stays in the current page. Pairing failures are visible;
transport failure during play detaches the cable and pauses emulation. Verified
ROM sharing requires HTTPS or localhost because browser
[Web Crypto](https://developer.mozilla.org/en-US/docs/Web/API/Crypto/subtle)
requires a secure context.

**File transfer options** retains **Send GBA file (loads here)**, separate
**Receive ROM** and **Load received ROM** actions, plus rejection/cancellation.
Files are bounded to 192 bytes–32 MiB, checked for the GBA header marker and
filename, sent in 16 KiB chunks with backpressure, and SHA-256 verified before
loading. Incorrect sequence, corruption, rejection and cancellation cannot make
a partial file loadable. Pairing and explicit game-file sharing do not send saves
or snapshots. Starting the cable separately exchanges temporary session state,
which includes cartridge save data. Received cartridges and peer session state
stay in memory and do not enter the installed library or the peer's save slots.

Each device persists and exports the save of its own GBA under its existing
cartridge key. The temporary peer GBA has no separate autosave destination.
Sharing session state allows the other player to inspect that state; WebRTC
encryption protects transport, not data from the receiving player. Start the
cable only with a peer with whom you want to share this session state.

Ordinary ROM changes, reset and save-state imports disconnect the devices and
discard the temporary peer GBA. Loading through the link panel preserves the
device connection but stops the emulated cable; choose Start cable again. A
normal snapshot contains only the device's own GBA. Restoring it does not restore
a peer machine or a live network session.

## Hardware and synchronization limits

The serial model implements normal 8-bit, normal 32-bit and two-player
multiplayer 16-bit transfers, parent/child roles, readiness, busy state, receive
registers and enabled completion IRQs. Normal transfers use emulated serial
clock periods. Multiplayer baud rates are implemented; physical inter-word delay
and missing-player timeout details remain approximations.

At Start, both devices exchange initial snapshots and validate the cartridge and
BIOS identities before creating the temporary peer machines. Matching cartridges
avoid another ROM transfer; different cartridges are bounded to 32 MiB. Both
browsers must use matching BIOS images. Pairing alone does not trigger this
exchange; both players must explicitly start the cable.

Each browser then simulates the same ordered pair of GBAs. Hardware serial
transfers complete locally, while the reliable network channel exchanges input
once per frame. Inputs take effect six frames later, about 100 ms at normal GBA
speed. This adds a fixed control delay so ordinary network latency can fit inside
the input buffer. If a required peer input has not arrived, emulation waits;
there is no rollback or prediction, and high latency can still slow play.
Linked emulators cap requested speed at 1×. The architecture removes a network
round trip per serial transfer, and the original browser latency regression sustains normal speed. Commercial
game throughput on remote devices remains to be measured. These local serial transfers still use the emulator's timing model,
not a cycle-perfect physical cable. Readiness, mode switches and initialization sequences still need broader commercial-game
coverage. Four-player cables, wireless adapters, UART, JoyBus, sensor cables and
other emulators' network protocols remain outside the supported scope.

Hardware behavior is independently authored from
[GBATEK's communication-port documentation](https://rust-console.github.io/gbatek-gbaonly/#gba-communication-ports).

## Regression and local gameplay diagnostics

```bash
cargo test --locked --release -p rustboy-gba --all-features serial::
cargo test --locked --release -p rustboy-gba --test multiboot
node tests/gba_link.mjs
node tests/gba_link_rooms.mjs
node tests/gba_link_local.mjs
node tests/gba_link_browser.mjs
```

The browser check requires a built `out/` bundle and Chromium (`RUSTBOY_BROWSER`
can select its executable). It pairs real WebRTC endpoints before either has a
cartridge, verifies accepted ROM transfer, then runs two independent browser WASM
instances. An original guest checks both multiplayer words and the serial IRQ
before rendering green. The checks also cover the speed cap and disconnection
after state restoration. A continuous guest additionally exercises Halt, IRQs,
DMA and save writes under 80 ms simulated one-way delay, requiring over 50 fps
on both endpoints and thousands of local serial transfers with only frame input
packets crossing the network. These are local-browser regressions, not a physical
phone/network or complete commercial multiplayer result.

A local two-emulator diagnostic accepts a privately supplied cartridge, frame
budget and input timelines (GBA key masks in hexadecimal):

```bash
cargo run --locked --release -p rustboy-gba --example link_gameplay_probe -- \
  /path/to/game.gba 600 120:8,122:0 120:8,122:0 target/link-probe
```

Use `download` instead of the second timeline for the original cartridge-free
client. The probe produces transfer logs, screenshots and final state/battery
files under the selected output directory. Its result is **OBSERVED**, requiring
scene inspection; reaching a menu or completing an upload is not counted as a
successful race, battle or trade. Commercial ROMs are never included in the tests
or public site.

The reviewed local commercial scene gate is optional and never downloads games:

```bash
python3 scripts/check_gba_link_gameplay.py --rom-dir /path/to/local/roms
```

It requires the exact locally supplied ROM identity recorded in
`tests/gba_link_gameplay_cases.json`. Each scene starts both emulators twice from
boot and compares full final snapshots, cartridge save bytes and the reviewed
RGBA hashes for both endpoints. A changed image, nondeterministic state, invalid
audio, emulator fault or missing local cartridge fails the gate. The pinned image
references are reviewed emulator output, not independent hardware captures.
The Single Pak case checks racing at about four seconds; the Multi Pak case
checks both players on the starting grid with different selected characters.
A separate manual continuation also verified independent steering during a
25-second Multi Pak race; it is not included in the cold-start image gate.
Generated logs, screenshots and reports remain under ignored `target/`.

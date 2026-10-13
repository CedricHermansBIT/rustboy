//! A complete cable pair simulated locally. Transport queues player inputs,
//! never individual SIO words, and both endpoints advance in a fixed order.
use crate::{GameBoyAdvance, FRAME_CYCLES};
use rustboy_emulator_api::Emulator;
use std::collections::VecDeque;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct LocalLinkStatus {
    /// Number of completed frames; queued indices begin at zero.
    pub frame: u32,
    pub queued: u32,
    pub keys: u16,
    pub transfers: u64,
}

pub(crate) struct LocalLink {
    pub peer: Box<GameBoyAdvance>,
    pub player: u8,
    pub frame: u32,
    pub inputs: VecDeque<[u16; 2]>,
    pub keys: u16,
    pub transfers: u64,
    origins: [u64; 2],
}

impl GameBoyAdvance {
    /// Import the other endpoint only into temporary session memory. Its
    /// cartridge bytes and battery data never replace this endpoint's saves.
    pub fn start_local_link(
        &mut self,
        peer_rom: Vec<u8>,
        peer_state: &[u8],
        peer_download: bool,
        player: u8,
    ) -> Result<(), String> {
        if player > 1 || self.local_link.is_some() {
            return Err("Invalid or already started local cable".into());
        }
        let bios = (!self.bus.replacement_bios).then(|| self.bus.bios.clone());
        let mut peer = if peer_download {
            if !peer_rom.is_empty() {
                return Err("Download receiver does not have a cartridge".into());
            }
            if bios.is_some() {
                return Err("Download play currently needs the replacement BIOS".into());
            }
            Self::new_multiboot_client()?
        } else {
            let rom = if peer_rom.is_empty() {
                if self.is_multiboot_client() {
                    return Err("Host cartridge is required for download play".into());
                }
                self.bus.rom.clone()
            } else {
                crate::validate_cartridge(&peer_rom)?;
                peer_rom
            };
            Self::new(rom, bios)?
        };
        // Validation is atomic and requires the peer's original ROM/firmware
        // identity and checked snapshot. A malformed bootstrap leaves us intact.
        peer.import_state(peer_state)?;
        peer.link_attach(1 - player).map_err(str::to_owned)?;
        self.link_attach(player).map_err(str::to_owned)?;
        let keys = !self.bus.io16(0x130) & 0x3ff;
        let origins = if player == 0 {
            [self.clocks, peer.clocks]
        } else {
            [peer.clocks, self.clocks]
        };
        // The peer display is invisible here. Video skipping preserves PPU,
        // IRQ and DMA time, while each device renders its own visible endpoint.
        peer.set_video_frame_stride(16);
        self.local_link = Some(LocalLink {
            peer: Box::new(peer),
            player,
            frame: 0,
            inputs: VecDeque::new(),
            keys,
            transfers: 0,
            origins,
        });
        Ok(())
    }
    pub fn queue_local_link_frame(
        &mut self,
        frame: u32,
        parent: u16,
        child: u16,
    ) -> Result<(), String> {
        let link = self
            .local_link
            .as_mut()
            .ok_or("Local cable is disconnected")?;
        if parent > 0x3ff
            || child > 0x3ff
            || link.inputs.len() >= 32
            || frame
                != link
                    .frame
                    .checked_add(link.inputs.len() as u32)
                    .ok_or("Cable frame overflow")?
        {
            return Err("Out-of-order or invalid cable input frame".into());
        }
        link.inputs.push_back([parent, child]);
        Ok(())
    }
    pub fn local_link_status(&self) -> Option<LocalLinkStatus> {
        self.local_link.as_ref().map(|link| LocalLinkStatus {
            frame: link.frame,
            queued: link.inputs.len() as u32,
            keys: link.keys,
            transfers: link.transfers,
        })
    }
    pub fn local_link_peer(&self) -> Option<&Self> {
        self.local_link.as_ref().map(|link| link.peer.as_ref())
    }
    pub(crate) fn run_local_frame(&mut self, link: &mut LocalLink) -> Result<u64, &'static str> {
        let Some(inputs) = link.inputs.pop_front() else {
            return Ok(0);
        };
        let start = self.clocks;
        let (parent, child) = if link.player == 0 {
            (&mut *self, link.peer.as_mut())
        } else {
            (link.peer.as_mut(), &mut *self)
        };
        parent.bus.set_keys(inputs[0]);
        child.bus.set_keys(inputs[1]);
        let frame = link.frame.checked_add(1).ok_or("Cable frame overflow")?;
        let target = u64::from(frame) * FRAME_CYCLES;
        parent.link_set_limit(link.origins[0] + target)?;
        child.link_set_limit(link.origins[1] + target)?;
        let mut finished = false;
        for _ in 0..1_000_000 {
            signals(parent, child);
            let parent_remaining = (link.origins[0] + target).saturating_sub(parent.clocks);
            parent.run_core(parent_remaining);
            let parent_time = parent.clocks.saturating_sub(link.origins[0]);
            child.link_set_limit(link.origins[1] + parent_time.max(target))?;
            child.run_core((link.origins[1] + parent_time).saturating_sub(child.clocks));
            signals(parent, child);
            if exchange(parent, child, link.origins, 0)? {
                link.transfers += 1;
                continue;
            }
            if exchange(child, parent, [link.origins[1], link.origins[0]], 1)? {
                link.transfers += 1;
                continue;
            }
            if parent.fault.is_some()
                || child.fault.is_some()
                || parent.bus.memory_locked
                || child.bus.memory_locked
            {
                return Err("A linked cartridge stopped emulation");
            }
            if parent.clocks >= link.origins[0] + target && child.clocks >= link.origins[1] + target
            {
                finished = true;
                break;
            }
            // STOP cannot accumulate future guest time without a hardware wake.
            if parent_remaining != 0 && parent.bus.stopped && !parent.bus.stop_wake_pending()
                || child.bus.stopped && !child.bus.stop_wake_pending()
            {
                return Err("A linked cartridge entered STOP without a wake source");
            }
        }
        if !finished {
            return Err("Local cable did not reach its frame boundary");
        }
        link.frame = frame;
        // Only the visible endpoint's samples go to this device's audio output.
        link.peer.drain_audio();
        Ok(self.clocks.saturating_sub(start))
    }
}

fn signals(parent: &mut GameBoyAdvance, child: &mut GameBoyAdvance) {
    let p = (
        parent.bus.serial.mode(),
        parent.bus.serial.ready(),
        parent.bus.serial.so(),
    );
    let c = (
        child.bus.serial.mode(),
        child.bus.serial.ready(),
        child.bus.serial.so(),
    );
    parent.link_set_peer(c.0, c.1, c.2);
    child.link_set_peer(p.0, p.1, p.2);
}

fn exchange(
    host: &mut GameBoyAdvance,
    receiver: &mut GameBoyAdvance,
    origins: [u64; 2],
    player: usize,
) -> Result<bool, &'static str> {
    let Some(request) = host.link_request() else {
        return Ok(false);
    };
    let time = host.clocks.saturating_sub(origins[0]);
    receiver.link_set_limit(origins[1] + time)?;
    receiver.run_core((origins[1] + time).saturating_sub(receiver.clocks));
    if receiver.bus.serial.mode() != request.mode
        || request.mode < 2 && !receiver.bus.serial.ready()
    {
        host.link_absent(request.id)?;
        return Ok(true);
    }
    let value = receiver.link_receive(request.id, request.mode, request.baud)?;
    let mut words = [0; 2];
    words[player] = request.value;
    words[1 - player] = value;
    host.link_supply(request.id, words)?;
    receiver.link_supply(request.id, words)?;
    Ok(true)
}

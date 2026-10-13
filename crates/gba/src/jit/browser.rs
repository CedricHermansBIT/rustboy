use crate::{
    bus::Bus,
    cpu::{Cpu, Memory},
    GameBoyAdvance,
};
use js_sys::{Function, Object, Reflect, Uint8Array, WebAssembly};
use std::{cell::RefCell, collections::HashMap};
use wasm_bindgen::{JsCast, JsValue};

thread_local! {
    static IMPORTS: RefCell<Option<Object>> = const { RefCell::new(None) };
    static SLOTS: RefCell<Option<Slots>> = const { RefCell::new(None) };
}
/// Raw WASM exports are imported directly; there is no JS call per instruction.
pub fn configure(imports: Object) {
    let table = wasm_bindgen::function_table().unchecked_into::<WebAssembly::Table>();
    let placeholder = (1..table.length())
        .find_map(|index| table.get(index).ok())
        .expect("core function table");
    SLOTS.with(|slots| {
        *slots.borrow_mut() = Some(Slots {
            table,
            placeholder,
            free: Vec::new(),
        })
    });
    IMPORTS.with(|slot| *slot.borrow_mut() = Some(imports));
}
struct Slots {
    table: WebAssembly::Table,
    placeholder: Function,
    free: Vec<u32>,
}
fn release(index: u32) {
    SLOTS.with(|slots| {
        if let Some(slots) = slots.borrow_mut().as_mut() {
            // Replace the function reference so a discarded cache releases the
            // generated module; recycle only slots owned by this JIT.
            if slots.table.set(index, &slots.placeholder).is_ok() {
                slots.free.push(index);
            }
        }
    });
}
pub(crate) struct Cache {
    // One slot per physical IWRAM word and RAM policy. The PC tag still
    // distinguishes mirrors, whose architectural addresses affect branches.
    arm: Vec<Option<Box<ArmEntry>>>,
    arm_aliases: Vec<Option<ArmAlias>>,
    arm_generation: u32,
    arm_count: usize,
    full: HashMap<(u32, bool), FullEntry>,
    pub full_enabled: bool,
    pub ram_enabled: bool,
    epoch: u64,
    pub(crate) batch_active: bool,
    pub enabled: bool,
    pub compiled: u64,
    pub instructions: u64,
    pub calls: u64,
    pub failures: u64,
    pub invalidations: u64,
    pub full_instructions: u64,
    pub full_calls: u64,
}
impl Default for Cache {
    fn default() -> Self {
        Self {
            arm: (0..16384).map(|_| None).collect(),
            arm_aliases: vec![None; 16384],
            arm_generation: 0,
            arm_count: 0,
            full: HashMap::new(),
            full_enabled: false,
            ram_enabled: true,
            epoch: 0,
            batch_active: false,
            enabled: true,
            compiled: 0,
            instructions: 0,
            calls: 0,
            failures: 0,
            invalidations: 0,
            full_instructions: 0,
            full_calls: 0,
        }
    }
}
impl Drop for Cache {
    fn drop(&mut self) {
        for entry in self.full.values() {
            if let Some(function) = entry.function {
                release(function);
            }
        }
        for entry in self.arm.iter().flatten() {
            if let Some(function) = entry.function {
                release(function);
            }
        }
    }
}
impl Cache {
    pub fn new_epoch(&mut self) {
        self.epoch = self.epoch.wrapping_add(1);
    }
}
struct ArmEntry {
    pc: u32,
    generation: u32,
    visits: u32,
    region: Option<super::arm::Region>,
    function: Option<u32>,
    pages: Vec<(usize, u64)>,
    validated: u64,
}
#[derive(Clone, Copy)]
struct ArmAlias {
    pc: u32,
    owner: usize,
    generation: u32,
}
fn install(bytes: &[u8]) -> Result<u32, JsValue> {
    IMPORTS.with(|slot| {
        let imports = slot.borrow();
        let module = WebAssembly::Module::new(&Uint8Array::from(bytes).into())?;
        let instance = WebAssembly::Instance::new(&module, imports.as_ref().ok_or(JsValue::NULL)?)?;
        let function = Reflect::get(&instance.exports(), &"run".into())?.dyn_into::<Function>()?;
        SLOTS.with(|slots| {
            let mut slots = slots.borrow_mut();
            let slots = slots.as_mut().ok_or(JsValue::NULL)?;
            let index = match slots.free.pop() {
                Some(index) => index,
                None => slots.table.grow(1)?,
            };
            slots.table.set(index, &function)?;
            Ok(index)
        })
    })
}
impl Cache {
    fn prepare_arm(&mut self, bus: &Bus, pc: u32) -> Option<u32> {
        if !self.enabled
            || pc >> 24 != 3
            || pc & 3 != 0
            || bus.memory_control & 1 != 0
            || bus.memory_locked
            || bus.execution.barrier
        {
            return None;
        }
        let physical = ((pc as usize & 0x7fff) >> 2) + usize::from(self.ram_enabled) * 8192;
        let alias = self.arm_aliases[physical].filter(|alias| {
            alias.pc == pc
                && self.arm[alias.owner].as_ref().is_some_and(|entry| {
                    entry.generation == alias.generation && entry.function.is_some()
                })
        });
        let index = alias.map_or(physical, |alias| alias.owner);
        let slot = &mut self.arm[index];
        if alias.is_none() && slot.as_ref().is_none_or(|entry| entry.pc != pc) {
            if (slot.is_none() && self.arm_count >= 1024)
                || !super::arm::supported(pc, bus.peek32(pc))
            {
                return None;
            }
            if let Some(old) = slot.take() {
                if let Some(function) = old.function {
                    release(function);
                }
            } else {
                self.arm_count += 1;
            }
            *slot = Some(Box::new(ArmEntry {
                pc,
                generation: 0,
                visits: 0,
                region: None,
                function: None,
                pages: Vec::new(),
                validated: 0,
            }));
        }
        let entry = slot.as_mut().unwrap();
        let changed = entry.validated != self.epoch
            || entry
                .pages
                .iter()
                .any(|&(page, version)| bus.jit_iwram_versions[page] != version);
        if changed {
            if entry
                .region
                .as_ref()
                .is_some_and(|region| region.words.iter().any(|(&pc, &op)| bus.peek32(pc) != op))
            {
                self.invalidations += 1;
                if let Some(function) = entry.function.take() {
                    release(function);
                }
                entry.region = None;
                entry.visits = 0;
            }
            entry.validated = self.epoch;
            for (page, version) in &mut entry.pages {
                *version = bus.jit_iwram_versions[*page];
            }
        }
        if let Some(function) = entry.function {
            return Some(function);
        }
        // Invalidating a shared owner may remove this entry point. Interpret
        // this instruction; the next lookup can learn a new region here.
        if entry.pc != pc {
            return None;
        }
        entry.visits = entry.visits.saturating_add(1);
        if entry.visits != 32 {
            return None;
        }
        let region =
            super::arm::Region::discover_with_ram(pc, self.ram_enabled, |pc| bus.peek32(pc))?;
        for &pc in region.words.keys() {
            let page = (pc as usize & 0x7fff) >> 8;
            if !entry.pages.iter().any(|&(p, _)| p == page) {
                entry.pages.push((page, bus.jit_iwram_versions[page]));
            }
        }
        let result = install(&region.compile());
        entry.region = Some(region);
        match result {
            Ok(function) => {
                entry.function = Some(function);
                self.arm_generation = self.arm_generation.wrapping_add(1);
                if self.arm_generation == 0 {
                    self.arm_aliases.fill(None);
                }
                entry.generation = self.arm_generation;
                for &address in entry.region.as_ref().unwrap().words.keys() {
                    let physical =
                        ((address as usize & 0x7fff) >> 2) + usize::from(self.ram_enabled) * 8192;
                    self.arm_aliases[physical] = Some(ArmAlias {
                        pc: address,
                        owner: index,
                        generation: entry.generation,
                    });
                }
                self.compiled += 1;
                Some(function)
            }
            Err(_) => {
                self.failures += 1;
                None
            }
        }
    }
}
struct FullEntry {
    visits: u32,
    words: Vec<u32>,
    function: Option<u32>,
    epoch: u64,
    version: u64,
}
impl Cache {
    fn prepare_full(&mut self, bus: &Bus, pc: u32, thumb: bool) -> Option<u32> {
        if !self.enabled
            || !self.full_enabled
            || !(matches!(pc >> 24, 2 | 3 | 5..=12) || pc < bus.bios.len() as u32 && pc < 0x4000)
            || bus.memory_locked
            || bus.execution.barrier
            || pc & if thumb { 1 } else { 3 } != 0
            || (matches!(pc >> 24, 2 | 3) && bus.memory_control & 1 != 0)
        {
            return None;
        }
        let full = self.full.len() >= 4096;
        let entry = match self.full.entry((pc, thumb)) {
            std::collections::hash_map::Entry::Occupied(entry) => entry.into_mut(),
            std::collections::hash_map::Entry::Vacant(entry) => {
                if full {
                    return None;
                }
                entry.insert(FullEntry {
                    visits: 0,
                    words: Vec::new(),
                    function: None,
                    epoch: 0,
                    version: 0,
                })
            }
        };
        let width = if thumb { 2 } else { 4 };
        let peek = |pc| {
            if thumb {
                u32::from(bus.peek16(pc))
            } else {
                bus.peek32(pc)
            }
        };
        if entry.epoch != self.epoch
            || (matches!(pc >> 24, 2 | 3 | 5..=7) && entry.version != bus.jit_memory_version)
        {
            if entry
                .words
                .iter()
                .enumerate()
                .any(|(n, &op)| peek(pc.wrapping_add(n as u32 * width)) != op)
            {
                self.invalidations += 1;
                if let Some(index) = entry.function.take() {
                    release(index);
                }
                entry.visits = 0;
                entry.words.clear();
            }
            entry.epoch = self.epoch;
            entry.version = bus.jit_memory_version;
        }
        if let Some(function) = entry.function {
            return Some(function);
        }
        entry.visits = entry.visits.saturating_add(1);
        if entry.visits != 32 {
            return None;
        }
        for n in 0..32 {
            let address = pc.wrapping_add(n * width);
            if address >> 24 != pc >> 24
                || (pc < 0x4000 && address + width > bus.bios.len() as u32)
                || (pc >> 24 >= 8
                    && (address as usize & bus.rom_address_mask) + width as usize > bus.rom.len())
            {
                break;
            }
            let op = peek(address);
            entry.words.push(op);
            if super::full::ends_block(pc, op, thumb) {
                break;
            }
        }
        let bytes = super::full::compile(pc, &entry.words, thumb)?;
        match install(&bytes) {
            Ok(index) => {
                entry.function = Some(index);
                self.compiled += 1;
                Some(index)
            }
            Err(_) => {
                self.failures += 1;
                None
            }
        }
    }
}
impl GameBoyAdvance {
    pub(crate) fn run_jit_block(&mut self, budget: u64, thumb: bool) -> Option<u64> {
        let fast = if thumb {
            None
        } else {
            self.jit.prepare_arm(&self.bus, self.cpu.r[15])
        };
        if fast.is_none() {
            let function = self.jit.prepare_full(&self.bus, self.cpu.r[15], thumb)?;
            let before = self.cpu.instructions;
            let exceptions = self.cpu.exceptions;
            let entry: extern "C" fn(u32, u32, u32) -> u32 =
                unsafe { std::mem::transmute(function as usize) };
            let clocks = entry(
                &mut self.cpu as *mut Cpu as u32,
                &mut self.bus as *mut Bus as u32,
                budget as u32,
            );
            if self.cpu.exceptions != exceptions {
                self.cpu.sequential &= self.bus.finish_instruction();
            }
            self.jit.calls += 1;
            self.jit.instructions += self.cpu.instructions.wrapping_sub(before);
            self.jit.full_calls += 1;
            self.jit.full_instructions += self.cpu.instructions.wrapping_sub(before);
            return Some(u64::from(clocks));
        }
        let function = fast?;
        let before = self.cpu.instructions;
        let address = self.cpu.r[15];
        let mapped = self.bus.memory_control & 0x20 == 0;
        let setting = self.bus.memory_control >> 24 & 15;
        let mut context = super::arm::RamContext {
            ewram: self.bus.ewram.as_mut_ptr() as u32,
            iwram: self.bus.iwram.as_mut_ptr() as u32,
            ewram_wait: if setting == 15 { 0 } else { 16 - setting },
            mapped_iwram: u32::from(mapped),
            iwram_versions: self.bus.jit_iwram_versions.as_mut_ptr() as u32,
            memory_version: &mut self.bus.jit_memory_version as *mut u64 as u32,
            wrote: 0,
        };
        let cpu = &mut self.cpu as *mut Cpu as u32;
        let entry: extern "C" fn(u32, u32, u32) -> u64 =
            unsafe { std::mem::transmute(function as usize) };
        let packed = entry(
            cpu,
            &mut context as *mut super::arm::RamContext as u32,
            budget as u32,
        );
        let clocks = u64::from(packed as u32);
        let count = self.cpu.instructions.wrapping_sub(before);
        if count == 0 {
            return None;
        }
        self.bus.instruction_context(address, false);
        self.bus.finish_instruction();
        self.cpu.sequential = true;
        #[cfg(feature = "idle-loops")]
        {
            self.bus.execution.idle_unsafe |= context.wrote != 0;
        }
        self.bus
            .jit_finish_arm((packed >> 32) as u32, count, clocks);
        self.jit.calls += 1;
        self.jit.instructions += count;
        Some(clocks)
    }
    pub fn set_full_jit_enabled(&mut self, enabled: bool) {
        self.jit.full_enabled = enabled;
    }
    pub fn set_jit_ram_enabled(&mut self, enabled: bool) {
        self.jit.ram_enabled = enabled;
    }
    pub fn set_jit_enabled(&mut self, enabled: bool) {
        self.jit.enabled = enabled;
    }
    pub fn jit_statistics(&self) -> String {
        format!(
            "enabled={} full={} ram={} compiled={} calls={} instructions={} failures={} invalidations={} full_calls={} full_instructions={}",
            self.jit.enabled,
            self.jit.full_enabled,
            self.jit.ram_enabled,
            self.jit.compiled,
            self.jit.calls,
            self.jit.instructions,
            self.jit.failures,
            self.jit.invalidations,
            self.jit.full_calls,
            self.jit.full_instructions
        )
    }
}

// Only compiled modules call these helpers with live core-owned pointers. The
// main module exports them through web.rs for direct WASM-to-WASM imports.
// SAFETY: cpu/bus must point to the exclusive live state passed to run_jit_block.
/// # Safety
/// Pointers must be the live, exclusively owned CPU/bus passed by the compiled block.
pub unsafe fn fetch(cpu: u32, bus: u32, pc: u32) {
    let cpu = &mut *(cpu as *mut Cpu);
    let bus = &mut *(bus as *mut Bus);
    cpu.instructions = cpu.instructions.wrapping_add(1);
    bus.instruction_context(pc, true);
    bus.fetch16(pc, cpu.sequential);
    cpu.r[15] = pc.wrapping_add(2);
    cpu.sequential = true;
}
/// # Safety
/// Pointers must be the live, exclusively owned CPU/bus passed by the compiled block.
pub unsafe fn finish(cpu: u32, bus: u32) -> i32 {
    let cpu = &mut *(cpu as *mut Cpu);
    let bus = &mut *(bus as *mut Bus);
    cpu.sequential &= bus.finish_instruction();
    let clocks = bus.take_cycles();
    bus.execution.pending += clocks;
    if bus.execution.barrier || bus.memory_locked {
        -(clocks as i32)
    } else {
        clocks as i32
    }
}
/// # Safety
/// Pointers must be the live, exclusively owned CPU/bus passed by the compiled block.
pub unsafe fn read(bus: u32, address: u32, kind: u32) -> u32 {
    let bus = &mut *(bus as *mut Bus);
    let value = match kind {
        0 => bus
            .read32_unaligned(address, false)
            .rotate_right((address & 3) * 8),
        1 => u32::from(bus.read8(address)),
        _ => u32::from(bus.read16_unaligned(address, false)).rotate_right((address & 1) * 8),
    };
    bus.internal(1);
    value
}
/// # Safety
/// Pointers must be the live, exclusively owned CPU/bus passed by the compiled block.
pub unsafe fn write(bus: u32, address: u32, value: u32, kind: u32) {
    let bus = &mut *(bus as *mut Bus);
    match kind {
        0 => bus.write32_unaligned(address, value, false),
        1 => bus.write8(address, value as u8),
        _ => bus.write16_unaligned(address, value as u16, false),
    }
}
/// # Safety
/// Pointers must be the live, exclusively owned CPU/bus passed by the compiled block.
pub unsafe fn internal(bus: u32, clocks: u32) {
    (&mut *(bus as *mut Bus)).internal(clocks);
}
/// # Safety
/// Pointers must be the live, exclusively owned CPU/bus passed by the compiled block.
pub unsafe fn branch(cpu: u32, bus: u32, target: u32, condition: u32) {
    let cpu = &mut *(cpu as *mut Cpu);
    if cpu.condition(condition) {
        cpu.branch(&mut *(bus as *mut Bus), target, false);
    }
}

/// # Safety
/// Pointers must be the live, exclusively owned state passed by the compiled block.
pub unsafe fn fetch_run(cpu: u32, bus: u32, pc: u32, count: u32, budget: u32) -> u64 {
    let cpu = &mut *(cpu as *mut Cpu);
    let bus = &mut *(bus as *mut Bus);
    let mut elapsed = 0u32;
    let mut completed = 0u32;
    while completed < count {
        let address = pc.wrapping_add(completed * 2);
        bus.instruction_context(address, true);
        bus.fetch16(address, cpu.sequential);
        cpu.sequential = bus.finish_instruction();
        elapsed += bus.take_cycles() as u32;
        completed += 1;
        if elapsed >= budget {
            break;
        }
    }
    cpu.r[15] = pc.wrapping_add(completed * 2);
    cpu.instructions = cpu.instructions.wrapping_add(u64::from(completed));
    bus.execution.pending += u64::from(elapsed);
    (u64::from(completed) << 32) | u64::from(elapsed)
}

/// # Safety
/// Arguments are exclusively owned live core pointers from generated code.
pub unsafe fn fetch_mode(cpu: u32, bus: u32, pc: u32, thumb: u32) {
    let cpu = &mut *(cpu as *mut Cpu);
    let bus = &mut *(bus as *mut Bus);
    cpu.instructions = cpu.instructions.wrapping_add(1);
    bus.instruction_context(pc, thumb != 0);
    if thumb != 0 {
        bus.fetch16(pc, cpu.sequential);
    } else {
        bus.fetch32(pc, cpu.sequential);
    }
    cpu.r[15] = pc.wrapping_add(if thumb != 0 { 2 } else { 4 });
    cpu.sequential = true;
}
/// # Safety
/// Arguments are exclusively owned live core pointers from generated code.
pub unsafe fn read_full(bus: u32, address: u32, kind: u32, sequential: u32) -> u32 {
    let bus = &mut *(bus as *mut Bus);
    match kind {
        0 => bus
            .read32_unaligned(address, sequential != 0)
            .rotate_right((address & 3) * 8),
        1 => u32::from(bus.read8(address)),
        2 => u32::from(bus.read16_unaligned(address, sequential != 0))
            .rotate_right((address & 1) * 8),
        3 => bus.read8(address) as i8 as i32 as u32,
        4 => {
            if address & 1 != 0 {
                bus.read8(address) as i8 as i32 as u32
            } else {
                bus.read16(address, false) as i16 as i32 as u32
            }
        }
        _ => bus.read32_unaligned(address, sequential != 0),
    }
}
/// # Safety
/// Arguments are exclusively owned live core pointers from generated code.
pub unsafe fn write_full(bus: u32, address: u32, value: u32, kind: u32, sequential: u32) {
    let bus = &mut *(bus as *mut Bus);
    match kind {
        1 => bus.write8(address, value as u8),
        2 => bus.write16_unaligned(address, value as u16, sequential != 0),
        _ => bus.write32_unaligned(address, value, sequential != 0),
    }
}
/// # Safety
/// Arguments are exclusively owned live core pointers from generated code.
pub unsafe fn control(cpu: u32, bus: u32, value: u32, kind: u32) {
    let cpu = &mut *(cpu as *mut Cpu);
    let bus = &mut *(bus as *mut Bus);
    if kind == 1 {
        cpu.branch(bus, value, true);
    } else {
        cpu.write_result(bus, 15, value, kind == 2);
    }
}
/// # Safety
/// Arguments are exclusively owned live core pointers from generated code.
pub unsafe fn status(cpu: u32, bus: u32, value: u32, fields: u32, kind: u32) -> u32 {
    let cpu = &mut *(cpu as *mut Cpu);
    let bus = &mut *(bus as *mut Bus);
    let mode = crate::cpu::bank(cpu.cpsr).unwrap();
    match kind {
        0 => cpu.cpsr,
        1 => cpu.spsr[mode],
        2 | 3 => {
            let mut mask = if fields & 8 != 0 { 0xf0000000 } else { 0 };
            if fields & 1 != 0 && cpu.cpsr & 31 != 0x10 {
                mask |= if kind == 3 { 0xff } else { 0xdf };
            }
            if kind == 3 {
                if mode != 0 {
                    cpu.spsr[mode] = (cpu.spsr[mode] & !mask) | (value & mask);
                }
            } else {
                let _ = cpu.set_cpsr((cpu.cpsr & !mask) | (value & mask));
            }
            0
        }
        _ => {
            if mode != 0 && cpu.set_cpsr(cpu.spsr[mode]).is_err() {
                cpu.undefined(bus);
            }
            0
        }
    }
}
/// # Safety
/// Arguments are exclusively owned live core pointers from generated code.
pub unsafe fn bank_read(cpu: u32, reg: u32) -> u32 {
    (&*(cpu as *const Cpu)).user_read(reg as usize)
}
/// # Safety
/// Arguments are exclusively owned live core pointers from generated code.
pub unsafe fn bank_write(cpu: u32, reg: u32, value: u32) {
    (&mut *(cpu as *mut Cpu)).user_write(reg as usize, value);
}
/// # Safety
/// Arguments are exclusively owned live core pointers from generated code.
pub unsafe fn exception(cpu: u32, bus: u32, swi: u32) {
    let cpu = &mut *(cpu as *mut Cpu);
    let bus = &mut *(bus as *mut Bus);
    if swi != 0 {
        cpu.exception(bus, 0x13, 8, cpu.r[15]);
    } else {
        cpu.undefined(bus);
    }
}
/// # Safety
/// Arguments are exclusively owned live core pointers from generated code.
pub unsafe fn fetch_span(cpu: u32, bus: u32, pc: u32, count: u32, budget: u32, thumb: u32) -> u64 {
    let mut clocks = 0;
    let mut done = 0;
    let mut stop = false;
    while done < count {
        fetch_mode(
            cpu,
            bus,
            pc.wrapping_add(done * if thumb != 0 { 2 } else { 4 }),
            thumb,
        );
        let cost = finish(cpu, bus);
        clocks += cost.unsigned_abs();
        done += 1;
        if cost < 0 || clocks >= budget {
            stop = cost < 0;
            break;
        }
    }
    (u64::from(done) << 32) | u64::from(clocks | if stop { 0x80000000 } else { 0 })
}

/// # Safety
/// Arguments are exclusively owned live core pointers from generated code.
pub unsafe fn refill(bus: u32, target: u32, thumb: u32) {
    let bus = &mut *(bus as *mut Bus);
    bus.flush_pipeline();
    if thumb != 0 {
        bus.fetch16(target, false);
        bus.fetch16(target.wrapping_add(2), true);
        bus.refill_complete(target.wrapping_add(2));
    } else {
        bus.fetch32(target, false);
        bus.fetch32(target.wrapping_add(4), true);
        bus.refill_complete(target.wrapping_add(4));
    }
    bus.latch_pipeline(target, thumb != 0);
}

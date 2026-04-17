/* tslint:disable */
/* eslint-disable */

/**
 * Add a breakpoint based on CB-prefixed instruction opcode (e.g., 0x7C for BIT 7,H)
 */
export function add_breakpoint_cb_opcode(opcode: number): void;

/**
 * Add a breakpoint that triggers when memory at `addr` equals `value`.
 */
export function add_breakpoint_mem(addr: number, value: number): void;

/**
 * Add a breakpoint based on instruction opcode (e.g., 0xDA for JP C,a16)
 */
export function add_breakpoint_opcode(opcode: number): void;

/**
 * Add a breakpoint that triggers when PC reaches the given address.
 * Address is in decimal; use JS `0x1234` for hex.
 */
export function add_breakpoint_pc(addr: number): void;

/**
 * Add a breakpoint that triggers when a register equals a value.
 * reg: "a","b","c","d","e","h","l","f","af","bc","de","hl","sp","pc"
 */
export function add_breakpoint_reg(reg: string, value: number): void;

/**
 * Clear all breakpoints.
 */
export function clear_breakpoints(): void;

/**
 * Clear the trace buffer.
 */
export function clear_trace(): void;

/**
 * Returns current CPU debug state as a string (called from JS)
 */
export function get_debug_state(): string;

/**
 * Returns the current emulation speed multiplier (1, 2, 4, or 8)
 */
export function get_speed(): number;

/**
 * Get the full trace buffer as a single string (lines separated by \n).
 */
export function get_trace(): string;

/**
 * Returns true if tracing is currently active.
 */
export function is_tracing(): boolean;

/**
 * List all breakpoints (returns string for console display).
 */
export function list_breakpoints(): string;

/**
 * Called from JS with the ROM bytes to load and run a new ROM
 */
export function load_rom_data(rom_data: Uint8Array): void;

export function main_js(): void;

/**
 * Read a single byte at the given address (respects MBC banking).
 */
export function peek(addr: number): number;

/**
 * Print all register values.
 */
export function peek_regs(): string;

/**
 * Hex-dump `len` bytes starting at `start`. Max 256 bytes.
 */
export function peek_slice(start: number, len: number): string;

/**
 * Remove a breakpoint by index.
 */
export function remove_breakpoint(index: number): void;

/**
 * Save the current game's external RAM to persistent storage
 */
export function save_game(): void;

export function set_key_state(key_code: number, is_pressed: boolean): void;

/**
 * Toggle CPU instruction tracing on/off.
 */
export function toggle_trace(): void;

/**
 * Get the number of lines currently in the trace buffer.
 */
export function trace_len(): number;

export type InitInput = RequestInfo | URL | Response | BufferSource | WebAssembly.Module;

export interface InitOutput {
    readonly memory: WebAssembly.Memory;
    readonly add_breakpoint_cb_opcode: (a: number) => void;
    readonly add_breakpoint_mem: (a: number, b: number) => void;
    readonly add_breakpoint_opcode: (a: number) => void;
    readonly add_breakpoint_pc: (a: number) => void;
    readonly add_breakpoint_reg: (a: number, b: number, c: number) => void;
    readonly clear_breakpoints: () => void;
    readonly clear_trace: () => void;
    readonly get_debug_state: () => [number, number];
    readonly get_speed: () => number;
    readonly get_trace: () => [number, number];
    readonly is_tracing: () => number;
    readonly list_breakpoints: () => [number, number];
    readonly load_rom_data: (a: number, b: number) => void;
    readonly main_js: () => void;
    readonly peek: (a: number) => number;
    readonly peek_regs: () => [number, number];
    readonly peek_slice: (a: number, b: number) => [number, number];
    readonly remove_breakpoint: (a: number) => void;
    readonly save_game: () => void;
    readonly set_key_state: (a: number, b: number) => void;
    readonly toggle_trace: () => void;
    readonly trace_len: () => number;
    readonly wasm_bindgen__convert__closures_____invoke__hacfed0661991feae: (a: number, b: number) => void;
    readonly __wbindgen_malloc: (a: number, b: number) => number;
    readonly __wbindgen_realloc: (a: number, b: number, c: number, d: number) => number;
    readonly __wbindgen_exn_store: (a: number) => void;
    readonly __externref_table_alloc: () => number;
    readonly __wbindgen_externrefs: WebAssembly.Table;
    readonly __wbindgen_free: (a: number, b: number, c: number) => void;
    readonly __wbindgen_destroy_closure: (a: number, b: number) => void;
    readonly __wbindgen_start: () => void;
}

export type SyncInitInput = BufferSource | WebAssembly.Module;

/**
 * Instantiates the given `module`, which can either be bytes or
 * a precompiled `WebAssembly.Module`.
 *
 * @param {{ module: SyncInitInput }} module - Passing `SyncInitInput` directly is deprecated.
 *
 * @returns {InitOutput}
 */
export function initSync(module: { module: SyncInitInput } | SyncInitInput): InitOutput;

/**
 * If `module_or_path` is {RequestInfo} or {URL}, makes a request and
 * for everything else, calls `WebAssembly.instantiate` directly.
 *
 * @param {{ module_or_path: InitInput | Promise<InitInput> }} module_or_path - Passing `InitInput` directly is deprecated.
 *
 * @returns {Promise<InitOutput>}
 */
export default function __wbg_init (module_or_path?: { module_or_path: InitInput | Promise<InitInput> } | InitInput | Promise<InitInput>): Promise<InitOutput>;

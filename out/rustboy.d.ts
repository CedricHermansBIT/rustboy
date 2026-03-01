/* tslint:disable */
/* eslint-disable */

/**
 * Add a breakpoint that triggers when memory at `addr` equals `value`.
 */
export function add_breakpoint_mem(addr: number, value: number): void;

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
 * Returns current CPU debug state as a string (called from JS)
 */
export function get_debug_state(): string;

/**
 * Returns the current emulation speed multiplier (1, 2, 4, or 8)
 */
export function get_speed(): number;

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

export type InitInput = RequestInfo | URL | Response | BufferSource | WebAssembly.Module;

export interface InitOutput {
    readonly memory: WebAssembly.Memory;
    readonly add_breakpoint_mem: (a: number, b: number) => void;
    readonly add_breakpoint_pc: (a: number) => void;
    readonly add_breakpoint_reg: (a: number, b: number, c: number) => void;
    readonly clear_breakpoints: () => void;
    readonly get_debug_state: () => [number, number];
    readonly get_speed: () => number;
    readonly list_breakpoints: () => [number, number];
    readonly load_rom_data: (a: number, b: number) => void;
    readonly main_js: () => void;
    readonly peek: (a: number) => number;
    readonly peek_regs: () => [number, number];
    readonly peek_slice: (a: number, b: number) => [number, number];
    readonly remove_breakpoint: (a: number) => void;
    readonly save_game: () => void;
    readonly set_key_state: (a: number, b: number) => void;
    readonly wasm_bindgen__closure__destroy__h30ea1fee59aedf62: (a: number, b: number) => void;
    readonly wasm_bindgen__convert__closures_____invoke__h1be67b495a4498d7: (a: number, b: number) => void;
    readonly __wbindgen_malloc: (a: number, b: number) => number;
    readonly __wbindgen_realloc: (a: number, b: number, c: number, d: number) => number;
    readonly __wbindgen_exn_store: (a: number) => void;
    readonly __externref_table_alloc: () => number;
    readonly __wbindgen_externrefs: WebAssembly.Table;
    readonly __wbindgen_free: (a: number, b: number, c: number) => void;
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

/* tslint:disable */
/* eslint-disable */

export function add_breakpoint_cb_opcode(opcode: number): void;

export function add_breakpoint_mem(addr: number, value: number): void;

export function add_breakpoint_opcode(opcode: number): void;

export function add_breakpoint_pc(addr: number): void;

export function add_breakpoint_reg(reg: string, value: number): void;

export function clear_breakpoints(): void;

export function clear_save_data(): void;

export function clear_trace(): void;

export function export_save_data(): Uint8Array;

export function export_state(): Uint8Array;

/**
 * Keep the replacement firmware's attribution available in binary deployments.
 */
export function get_boot_rom_license(): string;

export function get_debug_flags(): number;

export function get_debug_state(): string;

export function get_is_cgb(): boolean;

export function get_rom_title(): string;

export function get_save_key(): string;

export function get_sgb_status(): string;

export function get_speed(): number;

export function get_state_id(): string;

export function get_trace(): string;

export function import_save_data(data: Uint8Array): void;

export function import_state(data: Uint8Array): void;

export function is_paused(): boolean;

export function is_tracing(): boolean;

export function list_breakpoints(): string;

export function load_rom_data(rom: Uint8Array, boot_rom: Uint8Array): void;

export function load_rom_data_with_model(rom: Uint8Array, boot_rom: Uint8Array, model: string): void;

export function main_js(): void;

export function peek(addr: number): number;

export function peek_regs(): string;

export function peek_slice(start: number, len: number): string;

export function remove_breakpoint(index: number): void;

export function reset_emulator(): void;

export function save_game(): void;

export function set_border_visible(visible: boolean): void;

export function set_debug_enabled(enabled: boolean): void;

export function set_key_state(key_code: number, pressed: boolean): void;

export function set_paused(paused: boolean): void;

export function set_speed(speed: number): void;

export function set_vram_view(view: string): void;

export function toggle_trace(): void;

export function trace_len(): number;

export type InitInput = RequestInfo | URL | Response | BufferSource | WebAssembly.Module;

export interface InitOutput {
    readonly memory: WebAssembly.Memory;
    readonly add_breakpoint_cb_opcode: (a: number) => void;
    readonly add_breakpoint_mem: (a: number, b: number) => void;
    readonly add_breakpoint_opcode: (a: number) => void;
    readonly add_breakpoint_pc: (a: number) => void;
    readonly add_breakpoint_reg: (a: number, b: number, c: number) => void;
    readonly export_save_data: () => [number, number];
    readonly export_state: () => [number, number];
    readonly get_boot_rom_license: () => [number, number];
    readonly get_debug_flags: () => number;
    readonly get_debug_state: () => [number, number];
    readonly get_is_cgb: () => number;
    readonly get_rom_title: () => [number, number];
    readonly get_save_key: () => [number, number];
    readonly get_sgb_status: () => [number, number];
    readonly get_state_id: () => [number, number];
    readonly get_trace: () => [number, number];
    readonly import_save_data: (a: number, b: number) => [number, number];
    readonly import_state: (a: number, b: number) => [number, number];
    readonly is_paused: () => number;
    readonly is_tracing: () => number;
    readonly list_breakpoints: () => [number, number];
    readonly load_rom_data: (a: number, b: number, c: number, d: number) => [number, number];
    readonly load_rom_data_with_model: (a: number, b: number, c: number, d: number, e: number, f: number) => [number, number];
    readonly main_js: () => void;
    readonly peek: (a: number) => number;
    readonly peek_regs: () => [number, number];
    readonly peek_slice: (a: number, b: number) => [number, number];
    readonly remove_breakpoint: (a: number) => void;
    readonly set_border_visible: (a: number) => void;
    readonly set_debug_enabled: (a: number) => void;
    readonly set_key_state: (a: number, b: number) => void;
    readonly set_paused: (a: number) => void;
    readonly set_vram_view: (a: number, b: number) => [number, number];
    readonly trace_len: () => number;
    readonly toggle_trace: () => void;
    readonly get_speed: () => number;
    readonly set_speed: (a: number) => void;
    readonly reset_emulator: () => void;
    readonly clear_save_data: () => void;
    readonly clear_breakpoints: () => void;
    readonly clear_trace: () => void;
    readonly save_game: () => void;
    readonly wasm_bindgen_5aa949e133d73c3e___convert__closures_____invoke___f64______true_: (a: number, b: number, c: number) => void;
    readonly __wbindgen_malloc: (a: number, b: number) => number;
    readonly __wbindgen_realloc: (a: number, b: number, c: number, d: number) => number;
    readonly __wbindgen_exn_store: (a: number) => void;
    readonly __externref_table_alloc: () => number;
    readonly __wbindgen_externrefs: WebAssembly.Table;
    readonly __wbindgen_free: (a: number, b: number, c: number) => void;
    readonly __wbindgen_destroy_closure: (a: number, b: number) => void;
    readonly __externref_table_dealloc: (a: number) => void;
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

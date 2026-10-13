/* tslint:disable */
/* eslint-disable */

export function add_breakpoint_cb_opcode(opcode: number): void;

export function add_breakpoint_mem(addr: number, value: number): void;

export function add_breakpoint_opcode(opcode: number): void;

export function add_breakpoint_pc(addr: number): void;

export function add_breakpoint_reg(reg: string, value: number): void;

/**
 * Fixed-work diagnostic, available only in the experimental JIT bundle.
 */
export function benchmark_gba_clocks(budget: number): number;

export function clear_breakpoints(): void;

export function clear_save_data(): void;

export function clear_sgb_sound_firmware(): void;

export function clear_sgb_user_palette(): void;

export function clear_trace(): void;

export function configure_gba_jit(imports: object): void;

export function export_gba_rom(): Uint8Array;

export function export_save_data(): Uint8Array;

export function export_state(): Uint8Array;

export function gba_jit_bank_read(cpu: number, reg: number): number;

export function gba_jit_bank_write(cpu: number, reg: number, value: number): void;

export function gba_jit_branch(cpu: number, bus: number, target: number, condition: number): void;

export function gba_jit_control(cpu: number, bus: number, value: number, kind: number): void;

export function gba_jit_exception(cpu: number, bus: number, swi: number): void;

export function gba_jit_fetch(cpu: number, bus: number, pc: number): void;

export function gba_jit_fetch_mode(cpu: number, bus: number, pc: number, thumb: number): void;

export function gba_jit_fetch_run(cpu: number, bus: number, pc: number, count: number, budget: number): bigint;

export function gba_jit_fetch_span(cpu: number, bus: number, pc: number, count: number, budget: number, thumb: number): bigint;

export function gba_jit_finish(cpu: number, bus: number): number;

export function gba_jit_internal(bus: number, clocks: number): void;

export function gba_jit_read(bus: number, address: number, kind: number): number;

export function gba_jit_read_full(bus: number, address: number, kind: number, sequential: number): number;

export function gba_jit_refill(bus: number, target: number, thumb: number): void;

export function gba_jit_status(cpu: number, bus: number, value: number, fields: number, kind: number): number;

export function gba_jit_write(bus: number, address: number, value: number, kind: number): void;

export function gba_jit_write_full(bus: number, address: number, value: number, kind: number, sequential: number): void;

export function gba_link_absent(id: number): void;

export function gba_link_attach(player: number): void;

export function gba_link_detach(): void;

export function gba_link_receive(id: number, mode: number, baud: number): number;

export function gba_link_set_limit(clock: number): void;

export function gba_link_set_peer(mode: number, ready: boolean, so: boolean): void;

export function gba_link_status(): string;

export function gba_link_supply(id: number, parent: number, child: number): void;

export function gba_local_link_identity(): string;

export function gba_local_link_input(frame: number, parent: number, child: number): void;

export function gba_local_link_start(peer_rom: Uint8Array, peer_state: Uint8Array, peer_download: boolean, player: number): void;

export function gba_local_link_status(): string;

export function get_actual_speed(): number;

export function get_backend_status(): string;

/**
 * Keep the replacement firmware's attribution available in binary deployments.
 */
export function get_boot_rom_license(): string;

export function get_compatible_state_ids(): string[];

export function get_debug_flags(): number;

export function get_debug_state(): string;

export function get_gba_jit_statistics(): string;

export function get_gba_render_statistics(): string;

export function get_gba_rom_save_key(rom: Uint8Array): string;

export function get_gba_save_type(): string;

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

export function load_gba_data(rom: Uint8Array, bios: Uint8Array): void;

export function load_gba_data_with_save_type(rom: Uint8Array, bios: Uint8Array, kind: string): void;

export function load_gba_download_client(): void;

export function load_rom_data(rom: Uint8Array, boot_rom: Uint8Array): void;

export function load_rom_data_with_model(rom: Uint8Array, boot_rom: Uint8Array, model: string): void;

export function main_js(): void;

export function peek(addr: number): number;

export function peek_regs(): string;

export function peek_slice(start: number, len: number): string;

export function release_controller_inputs(): void;

export function remove_breakpoint(index: number): void;

export function reset_emulator(): void;

export function save_game(): void;

export function set_border_visible(visible: boolean): void;

/**
 * Port 0 is player one; SGB additionally supports ports 1 through 3.
 */
export function set_controller_button(port: number, button: string, pressed: boolean): void;

export function set_debug_enabled(enabled: boolean): void;

export function set_gba_fast_render_enabled(enabled: boolean): void;

export function set_gba_full_jit_enabled(enabled: boolean): void;

export function set_gba_jit_enabled(enabled: boolean): void;

export function set_gba_jit_ram_enabled(enabled: boolean): void;

export function set_gba_save_type(kind: string): void;

export function set_gba_video_stride(stride: number): void;

export function set_key_state(key_code: number, pressed: boolean): void;

export function set_paused(paused: boolean): void;

/**
 * User-provided SNES firmware, distinct from the handheld startup override.
 */
export function set_sgb_sound_firmware(data: Uint8Array): boolean;

export function set_sgb_user_palette(colors: Uint16Array): void;

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
    readonly benchmark_gba_clocks: (a: number) => [number, number, number];
    readonly clear_breakpoints: () => void;
    readonly clear_save_data: () => [number, number];
    readonly clear_sgb_sound_firmware: () => void;
    readonly clear_sgb_user_palette: () => [number, number];
    readonly clear_trace: () => void;
    readonly export_gba_rom: () => [number, number];
    readonly export_save_data: () => [number, number];
    readonly export_state: () => [number, number];
    readonly gba_jit_finish: (a: number, b: number) => number;
    readonly gba_link_absent: (a: number) => [number, number];
    readonly gba_link_attach: (a: number) => [number, number];
    readonly gba_link_detach: () => void;
    readonly gba_link_receive: (a: number, b: number, c: number) => [number, number, number];
    readonly gba_link_set_limit: (a: number) => [number, number];
    readonly gba_link_set_peer: (a: number, b: number, c: number) => [number, number];
    readonly gba_link_status: () => [number, number];
    readonly gba_link_supply: (a: number, b: number, c: number) => [number, number];
    readonly gba_local_link_identity: () => [number, number];
    readonly gba_local_link_input: (a: number, b: number, c: number) => [number, number];
    readonly gba_local_link_start: (a: number, b: number, c: number, d: number, e: number, f: number) => [number, number];
    readonly gba_local_link_status: () => [number, number];
    readonly get_actual_speed: () => number;
    readonly get_backend_status: () => [number, number];
    readonly get_boot_rom_license: () => [number, number];
    readonly get_compatible_state_ids: () => [number, number];
    readonly get_debug_flags: () => number;
    readonly get_debug_state: () => [number, number];
    readonly get_gba_jit_statistics: () => [number, number];
    readonly get_gba_render_statistics: () => [number, number];
    readonly get_gba_rom_save_key: (a: number, b: number) => [number, number, number, number];
    readonly get_gba_save_type: () => [number, number];
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
    readonly load_gba_data: (a: number, b: number, c: number, d: number) => [number, number];
    readonly load_gba_data_with_save_type: (a: number, b: number, c: number, d: number, e: number, f: number) => [number, number];
    readonly load_gba_download_client: () => [number, number];
    readonly load_rom_data: (a: number, b: number, c: number, d: number) => [number, number];
    readonly load_rom_data_with_model: (a: number, b: number, c: number, d: number, e: number, f: number) => [number, number];
    readonly main_js: () => void;
    readonly peek: (a: number) => number;
    readonly peek_regs: () => [number, number];
    readonly peek_slice: (a: number, b: number) => [number, number];
    readonly release_controller_inputs: () => void;
    readonly remove_breakpoint: (a: number) => void;
    readonly reset_emulator: () => void;
    readonly save_game: () => void;
    readonly set_border_visible: (a: number) => void;
    readonly set_controller_button: (a: number, b: number, c: number, d: number) => [number, number];
    readonly set_debug_enabled: (a: number) => void;
    readonly set_gba_fast_render_enabled: (a: number) => void;
    readonly set_gba_full_jit_enabled: (a: number) => void;
    readonly set_gba_jit_enabled: (a: number) => void;
    readonly set_gba_jit_ram_enabled: (a: number) => void;
    readonly set_gba_save_type: (a: number, b: number) => [number, number];
    readonly set_key_state: (a: number, b: number) => void;
    readonly set_paused: (a: number) => void;
    readonly set_sgb_sound_firmware: (a: number, b: number) => [number, number, number];
    readonly set_sgb_user_palette: (a: number, b: number) => [number, number];
    readonly set_speed: (a: number) => void;
    readonly set_vram_view: (a: number, b: number) => [number, number];
    readonly toggle_trace: () => void;
    readonly trace_len: () => number;
    readonly gba_jit_fetch_span: (a: number, b: number, c: number, d: number, e: number, f: number) => bigint;
    readonly gba_jit_fetch_run: (a: number, b: number, c: number, d: number, e: number) => bigint;
    readonly gba_jit_status: (a: number, b: number, c: number, d: number, e: number) => number;
    readonly gba_jit_write_full: (a: number, b: number, c: number, d: number, e: number) => void;
    readonly set_gba_video_stride: (a: number) => void;
    readonly gba_jit_bank_write: (a: number, b: number, c: number) => void;
    readonly gba_jit_exception: (a: number, b: number, c: number) => void;
    readonly gba_jit_fetch: (a: number, b: number, c: number) => void;
    readonly gba_jit_read: (a: number, b: number, c: number) => number;
    readonly gba_jit_refill: (a: number, b: number, c: number) => void;
    readonly gba_jit_bank_read: (a: number, b: number) => number;
    readonly gba_jit_internal: (a: number, b: number) => void;
    readonly gba_jit_branch: (a: number, b: number, c: number, d: number) => void;
    readonly gba_jit_control: (a: number, b: number, c: number, d: number) => void;
    readonly gba_jit_fetch_mode: (a: number, b: number, c: number, d: number) => void;
    readonly gba_jit_read_full: (a: number, b: number, c: number, d: number) => number;
    readonly gba_jit_write: (a: number, b: number, c: number, d: number) => void;
    readonly configure_gba_jit: (a: any) => void;
    readonly get_speed: () => number;
    readonly wasm_bindgen_a3ec65c79b425d3b___convert__closures_____invoke___f64______true_: (a: number, b: number, c: number) => void;
    readonly __wbindgen_malloc: (a: number, b: number) => number;
    readonly __wbindgen_realloc: (a: number, b: number, c: number, d: number) => number;
    readonly __wbindgen_export: WebAssembly.Table;
    readonly __wbindgen_exn_store: (a: number) => void;
    readonly __externref_table_alloc: () => number;
    readonly __wbindgen_externrefs: WebAssembly.Table;
    readonly __wbindgen_free: (a: number, b: number, c: number) => void;
    readonly __wbindgen_destroy_closure: (a: number, b: number) => void;
    readonly __externref_table_dealloc: (a: number) => void;
    readonly __externref_drop_slice: (a: number, b: number) => void;
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

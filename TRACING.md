# Instruction-Level Cycle Tracing Guide

This guide explains how to use the optional instruction-level tracing system in rustboy to debug test failures and analyze CPU execution.

## Overview

The instruction-level tracer captures detailed information about every CPU instruction executed, including:
- **Instruction number** (sequential counter)
- **Program counter (PC)** before instruction execution
- **Opcode** (1 byte)
- **Mnemonic** (instruction name like "jrnz", "ldaa16")
- **Register state** (A, B, C, D, E, H, L, F) after instruction execution
- **Stack pointer (SP)** after execution
- **M-cycle count** for this instruction
- **Total M-cycles** accumulated so far
- **Interrupt Master Enable (IME)** flag state

This is **optional** and **disabled by default** to avoid performance impact. It only activates when explicitly requested.

## Usage in Tests

### Basic Usage: Enable Tracing for a Test

Set the `TRACE_TEST` environment variable to match any test ROM path:

```bash
# Enable tracing for mooneye_basic test
TRACE_TEST=basic cargo test --test headless mooneye_basic --no-default-features -- --nocapture

# Enable tracing for a failing test
TRACE_TEST=boot_div cargo test --test headless mooneye_boot_div_dmgabcmgb --no-default-features -- --nocapture

# Enable tracing for any test with "div" in the name
TRACE_TEST=div cargo test --test headless -- --no-default-features -- --nocapture
```

### Output: Live Trace Summary

When tracing is enabled, you'll see:

```
🟢 Instruction tracing ENABLED for: testroms/basic.gb

📊 Trace Statistics:
  Total instructions: 2309589
  === Execution Statistics ===
  Total Instructions: 2309589
  Total M-Cycles: 6046681
  Average Cycles/Instr: 2.62
  
  Top 10 Most Executed Instructions:
    jrnz: 738860
    ldhaa8: 726432
    cp: 722581
    ...

📍 Last 20 Instructions:
  [2309569] PC:489F F0    | A:03 B:03 C:05 D:08 E:0D H:15 L:22 F:10 SP:DFFE | ldhaa8 (3) M-cycles | total: 6046638 M-cycles
  [2309570] PC:48A0 3D    | A:02 B:03 C:05 D:08 E:0D H:15 L:22 F:50 SP:DFFE | deca (1) M-cycles | total: 6046639 M-cycles
  ...
```

### Export Trace as CSV

Export the complete execution trace to a CSV file for analysis in spreadsheet software:

```bash
# Enable tracing AND CSV export
TRACE_TEST=basic TRACE_CSV=1 cargo test --test headless mooneye_basic --no-default-features -- --nocapture

# Output: ✅ Trace exported to: trace_basic.csv
```

The CSV file contains columns:
- `instruction` - Instruction time (0-indexed)
- `pc` - Program counter in hex
- `opcode` - Opcode in hex
- `mnemonic` - Instruction name
- `a,b,c,d,e,h,l,f` - All 8 registers in hex
- `sp` - Stack pointer in hex
- `m_cycles` - M-cycles for this instruction
- `total_cycles` - Cumulative M-cycles
- `ime` - Interrupt Master Enable (0=disabled, 1=enabled)

### Using CSV for Analysis

Open the CSV in your favorite spreadsheet application:

```bash
# Linux/macOS - open with LibreOffice Calc
libreoffice trace_basic.csv &

# macOS - open with Excel or Numbers
open trace_basic.csv

# Windows - double-click the file
```

You can then:
- **Sort by instruction** to see all instances of a specific opcode
- **Filter by register values** to find when registers change unexpectedly
- **Compare two traces** to identify divergence points
- **Plot register changes** to visualize execution patterns
- **Search for specific PC addresses** to see what happens at critical points

## Example: Debugging a Failing Test

### Scenario: Test `mooneye_boot_div` Fails

```bash
# Run the test normally - it fails
cargo test --test headless mooneye_boot_div_dmgabcmgb --no-default-features 2>&1 | tail -5

# Output:
# thread 'mooneye_boot_div_dmgabcmgb' panicked at 'FAIL: testroms/boot_div-dmgABCmgb.gb — 
# B=42 C=42 D=42 E=42 H=42 L=42 A=42 F=42 PC=0140'
```

### Enable Tracing to See What Happened

```bash
TRACE_TEST=boot_div cargo test --test headless mooneye_boot_div_dmgabcmgb --no-default-features -- --nocapture 2>&1 | tail -100
```

### Analyze the Trace

Look at the last 20 instructions to see:
1. Was the CPU stuck in a loop (same PC repeated)?
2. Did a register get corrupted early?
3. Did execution jump to an unexpected address?
4. Did the DIV register (0xFF04) get incorrect value?

### Export and Compare

```bash
# First, generate reference trace from a known-good emulator (if available)
# Then generate trace from rustboy
TRACE_TEST=boot_div TRACE_CSV=1 cargo test --test headless mooneye_boot_div_dmgabcmgb --no-default-features

# Open trace_boot_div.csv and compare with reference
# Row-by-row comparison shows EXACTLY where execution diverges
```

## Implementation Details

### How It Works

1. **Capture Phase**: After each instruction executes, CPU state is captured
2. **Storage**: Instruction trace is stored in `CPU.instruction_tracer`
3. **Export Phase**: After test completes, traces are exported to:
   - Console output (last 20 instructions + statistics)
   - CSV file (if `TRACE_CSV=1` is set)

### Finding Your Test Match

The `TRACE_TEST` environment variable uses **substring matching**:

| Variable | Matches |
|----------|---------|
| `di_timing` | `mooneye_di_timing.gb`, `mooneye_di_timing_gs.gb` |
| `boot` | any test with "boot" in path |
| `512kb` | `mooneye_rom_512kb.gb` |
| `blargg` | all blargg tests |

### Performance Impact

- **Tracing disabled (default)**: No overhead (~100% performance)
- **Tracing enabled**: ~5-10% overhead per test (due to instruction capture)

Tracing is **completely disabled by default** to avoid impacting normal test runs.

## API Usage (For Custom Tests)

### Enabling Tracing Programmatically

```rust
let mut cpu = CPU::new();
cpu.set_instruction_tracing(true);  // Enable tracing

// ... run emulation ...

// Get trace data
let trace_count = cpu.trace_count();
let stats = cpu.get_trace_statistics();
let last_20 = cpu.get_last_traces(20);
let csv_data = cpu.export_instruction_traces_csv();
```

### Checking Specific Instructions

```rust
// Check if instruction at position N changed state
let traces = &cpu.instruction_tracer.traces;
if traces.len() > 100 {
    let instr = &traces[100];
    println!("Instruction {}: PC={:04X} {} (A={:02X})", 
        instr.instr_number, instr.pc, instr.mnemonic, instr.regs[0]);
}
```

## Troubleshooting

### Q: Tracing isn't working - no output appears
**A:** Make sure:
1. `TRACE_TEST` matches part of the ROM path
2. You're running with `-- --nocapture` flag to see console output
3. The test actually runs (some tests are ignored with `#[ignore]`)

### Q: CSV file is huge and slow to open
**A:** The tracer captures **every** instruction:
- Basic test: ~2.3M instructions = ~2.3M rows
- Use filtering/pivot tables in your spreadsheet
- Or use command-line tools: `head -1000 trace_*.csv | grep "ldaa16"`

### Q: I want to record only part of the execution
**A:** You can modify headless.rs to:
```rust
// Start tracing after boot sequence
if cpu.total_cycles > 10_000_000 {
    cpu.set_instruction_tracing(true);
}
```

### Q: Can I compare two traces?
**A:** Not yet, but you can:
1. Export both as CSV
2. Load in Excel/LibreOffice
3. Use conditional formatting to highlight differences
4. Or write a Python script to compare rows

**Future enhancement**: Add `find_divergence()` method to find first difference between two test runs.

## Advanced: Customizing Trace Output

### Modify headless.rs to Enable Tracing for Specific Tests

```rust
fn run_rom(path: &str, max_cycles: u64) -> CPU {
    // ... existing code ...
    
    // Enable tracing for timer tests specifically
    if path.contains("timer") || path.contains("div") {
        cpu.set_instruction_tracing(true);
        eprintln!("🟢 Timer test tracing enabled");
    }
    
    // ... rest of code ...
}
```

### Filter CSV Output by Register Change

```bash
# Show only instructions where register A changed
awk -F, 'NR>1 && $5 != prev_a {print NR": A="$5} {prev_a=$5}' trace_basic.csv | head -50
```

### Find Instruction Loops

```bash
# Show PC values and count occurrences
awk -F, 'NR>1 {printf "%s\n", $2}' trace_basic.csv | sort | uniq -c | sort -rn | head -20
```

## Performance Considerations

### Memory Usage
- Each trace entry: ~100 bytes (instruction number, PC, registers, cycles, etc.)
- 2.3M instructions ≈ 230 MB in memory
- CSV export: ~200-300 MB on disk (compressed to ~50 MB)

### CPU Usage
- Capture overhead: ~5-10% per test
- Export/statistics generation: <1% (mostly CSV write)

### Storage
Always clean up trace files after analysis:
```bash
rm -f trace_*.csv
```

## Example Workflows

### Workflow 1: Find Why a Test Hangs

```bash
# Run test with timeout
timeout 10s bash -c 'TRACE_TEST=hanging_test cargo test --test headless mooneye_xyz --no-default-features -- --nocapture' || true

# Check if stuck in loop
tail -20 trace_xyz.csv | awk -F, '{print $2}' | sort | uniq -c
# If same PC repeats: infinite loop detected
```

### Workflow 2: Track DIV Timer Bug

```bash
# Capture full trace of DIV-sensitive test
TRACE_TEST=div TRACE_CSV=1 cargo test --test headless mooneye_div_timing --no-default-features

# Filter for DIV register writes (0xFF04 writes)
# Then check M-cycle count before/after
grep "div\|tima" trace_div_timing.csv
```

### Workflow 3: Verify Boot Sequence

```bash
# Capture first 1000 instructions only
# (Edit headless.rs to limit capture)
TRACE_TEST=boot TRACE_CSV=1 cargo test --test headless mooneye_boot_hwio --no-default-features

# Verify early register initialization
head -50 trace_boot_hwio.csv
```

## See Also

- [AGENTS.md](AGENTS.md) - Emulator architecture guide
- [README.md](README.md) - Overall project information
- `src/debug_tracer.rs` - Tracer implementation
- `tests/headless.rs` - Test framework

## Contributing

To improve the tracing system:
1. Add new fields to `InstructionTrace` struct
2. Extend `capture_instruction_final()` to record new data
3. Update CSV export in `export_as_csv()`
4. Document the new fields in this guide



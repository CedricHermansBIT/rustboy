/// Cycle-level instruction tracer for debugging test failures.
///
/// This module provides detailed tracking of CPU execution on a per-instruction basis,
/// including register state, memory changes, and cycle counts. It's designed for
/// optional debugging of test failures without performance impact when disabled.

use std::fmt;

/// Represents a single instruction execution snapshot
#[derive(Clone, Debug)]
pub struct InstructionTrace {
    /// Absolute instruction count (0-indexed)
    pub instr_number: u64,
    /// Total M-cycles executed so far
    pub total_m_cycles: u64,
    /// Program counter at start of instruction
    pub pc: u16,
    /// Opcode executed
    pub opcode: u8,
    /// Was this a CB-prefixed instruction?
    pub is_cb: bool,
    /// Mnemonic name of instruction
    pub mnemonic: String,
    /// Register state: [A, B, C, D, E, H, L, F]
    pub regs: [u8; 8],
    /// Stack pointer
    pub sp: u16,
    /// M-cycles for this instruction
    pub m_cycles: u32,
    /// Interrupt master enable state
    pub ime: bool,
    /// Memory location of highest stack access (if any)
    pub stack_access: Option<u16>,
}

impl fmt::Display for InstructionTrace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[{:06}] PC:{:04X} {:02X} {} | A:{:02X} B:{:02X} C:{:02X} D:{:02X} E:{:02X} H:{:02X} L:{:02X} F:{:02X} SP:{:04X} | {} ({}) M-cycles | total: {} M-cycles",
            self.instr_number,
            self.pc,
            self.opcode,
            if self.is_cb { "CB" } else { "  " },
            self.regs[0], self.regs[1], self.regs[2], self.regs[3],
            self.regs[4], self.regs[5], self.regs[6], self.regs[7],
            self.sp,
            self.mnemonic,
            self.m_cycles,
            self.total_m_cycles,
        )
    }
}

/// Debug tracer for capturing instruction-level execution detail
pub struct InstructionTracer {
    /// All captured traces
    pub traces: Vec<InstructionTrace>,
    /// Whether tracing is currently active
    pub enabled: bool,
    /// Instruction counter
    pub instr_count: u64,
    /// Previous cycle count
    pub prev_cycles: u64,
}

impl InstructionTracer {
    pub fn new() -> Self {
        InstructionTracer {
            traces: Vec::new(),
            enabled: false,
            instr_count: 0,
            prev_cycles: 0,
        }
    }

    /// Enable or disable tracing
    pub fn set_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
        if enabled {
            self.reset();
        }
    }

    /// Reset tracer state
    pub fn reset(&mut self) {
        self.traces.clear();
        self.instr_count = 0;
        self.prev_cycles = 0;
    }

    /// Capture instruction with pre-extracted data (avoids borrow checker issues)
    pub fn capture_instruction_final(
        &mut self,
        instr_number: u64,
        prev_cycles: u64,
        pc: u16,
        opcode: u8,
        is_cb: bool,
        mnemonic: &str,
        regs: [u8; 8],
        sp: u16,
        m_cycles: u32,
        ime: bool,
    ) {
        if !self.enabled {
            return;
        }

        let total_m_cycles = prev_cycles + m_cycles as u64;

        let trace = InstructionTrace {
            instr_number,
            total_m_cycles,
            pc,
            opcode,
            is_cb,
            mnemonic: mnemonic.to_string(),
            regs,
            sp,
            m_cycles,
            ime,
            stack_access: None,
        };

        self.traces.push(trace);
    }

    /// Get trace summary (last N instructions)
    pub fn get_last_n_traces(&self, n: usize) -> Vec<String> {
        let start = if self.traces.len() > n {
            self.traces.len() - n
        } else {
            0
        };

        self.traces[start..]
            .iter()
            .map(|t| t.to_string())
            .collect()
    }

    /// Find first divergence between two tracers
    pub fn find_divergence(&self, other: &InstructionTracer) -> Option<(usize, String)> {
        let min_len = self.traces.len().min(other.traces.len());

        for i in 0..min_len {
            let t1 = &self.traces[i];
            let t2 = &other.traces[i];

            // Check if execution path diverged
            if t1.pc != t2.pc || t1.opcode != t2.opcode {
                return Some((
                    i,
                    format!(
                        "Divergence at instruction {}: expected PC={:04X} opcode={:02X}, got PC={:04X} opcode={:02X}",
                        i, t1.pc, t1.opcode, t2.pc, t2.opcode
                    ),
                ));
            }

            // Check if register state diverged
            if t1.regs != t2.regs {
                let mut diffs = Vec::new();
                let reg_names = ["A", "B", "C", "D", "E", "H", "L", "F"];
                for (idx, name) in reg_names.iter().enumerate() {
                    if t1.regs[idx] != t2.regs[idx] {
                        diffs.push(format!(
                            "{}:{:02X}→{:02X}",
                            name, t1.regs[idx], t2.regs[idx]
                        ));
                    }
                }
                return Some((
                    i,
                    format!("Register divergence at instruction {}: {}", i, diffs.join(" ")),
                ));
            }

            // Check if SP diverged
            if t1.sp != t2.sp {
                return Some((
                    i,
                    format!(
                        "Stack pointer divergence at instruction {}: expected SP={:04X}, got SP={:04X}",
                        i, t1.sp, t2.sp
                    ),
                ));
            }

            // Check if cycle count diverged
            if t1.m_cycles != t2.m_cycles {
                return Some((
                    i,
                    format!(
                        "Cycle count divergence at instruction {}: expected {} cycles, got {} cycles",
                        i, t1.m_cycles, t2.m_cycles
                    ),
                ));
            }
        }

        // Check if one tracer went further
        if self.traces.len() != other.traces.len() {
            return Some((
                min_len,
                format!(
                    "Execution length divergence: expected {} instructions, got {}",
                    self.traces.len(),
                    other.traces.len()
                ),
            ));
        }

        None
    }

    /// Export traces as human-readable string
    pub fn export_as_string(&self) -> String {
        self.traces
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// Export traces as CSV (for analysis in spreadsheet)
    pub fn export_as_csv(&self) -> String {
        let mut csv = String::from(
            "instruction,pc,opcode,mnemonic,a,b,c,d,e,h,l,f,sp,m_cycles,total_cycles,ime\n",
        );

        for trace in &self.traces {
            csv.push_str(&format!(
                "{},{:04X},{:02X},{},{:02X},{:02X},{:02X},{:02X},{:02X},{:02X},{:02X},{:02X},{:04X},{},{},{}\n",
                trace.instr_number,
                trace.pc,
                trace.opcode,
                trace.mnemonic,
                trace.regs[0], trace.regs[1], trace.regs[2], trace.regs[3],
                trace.regs[4], trace.regs[5], trace.regs[6], trace.regs[7],
                trace.sp,
                trace.m_cycles,
                trace.total_m_cycles,
                if trace.ime { 1 } else { 0 }
            ));
        }

        csv
    }

    /// Get statistics about execution
    pub fn get_statistics(&self) -> String {
        if self.traces.is_empty() {
            return "No traces captured".to_string();
        }

        let total_instructions = self.traces.len();
        let total_cycles: u64 = self.traces.iter().map(|t| t.m_cycles as u64).sum();

        let mut instr_counts = std::collections::HashMap::new();
        for trace in &self.traces {
            *instr_counts.entry(trace.mnemonic.clone()).or_insert(0) += 1;
        }

        let mut most_common: Vec<_> = instr_counts.iter().collect();
        most_common.sort_by_key(|&(_, count)| std::cmp::Reverse(*count));

        let mut stats = format!(
            "=== Execution Statistics ===\nTotal Instructions: {}\nTotal M-Cycles: {}\nAverage Cycles/Instr: {:.2}\n",
            total_instructions,
            total_cycles,
            total_cycles as f64 / total_instructions as f64
        );

        stats.push_str("\nTop 10 Most Executed Instructions:\n");
        for (instr, count) in most_common.iter().take(10) {
            stats.push_str(&format!("  {}: {}\n", instr, count));
        }

        stats
    }
}

impl Default for InstructionTracer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tracer_enable_disable() {
        let mut tracer = InstructionTracer::new();
        assert!(!tracer.enabled);

        tracer.set_enabled(true);
        assert!(tracer.enabled);

        tracer.set_enabled(false);
        assert!(!tracer.enabled);
    }
}








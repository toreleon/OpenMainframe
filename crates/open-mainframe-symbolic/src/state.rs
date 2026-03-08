//! Symbolic execution state.
//!
//! An [`ExecutionState`] captures the complete snapshot of a symbolic
//! execution at a given program point: variable bindings, the current
//! path condition, the call stack (for PERFORM), and loop iteration
//! counters for bounding.

use std::collections::HashMap;

use crate::path::PathCondition;
use crate::value::SymbolicValue;

/// Complete symbolic execution state at one program point.
#[derive(Debug, Clone)]
pub struct ExecutionState {
    /// Variable name → symbolic value.
    pub variables: HashMap<String, SymbolicValue>,

    /// Accumulated path condition.
    pub path_condition: PathCondition,

    /// Program counter (index into the flattened statement list).
    pub pc: usize,

    /// Call stack for nested PERFORM / CALL statements.
    pub call_stack: Vec<CallFrame>,

    /// Per-loop iteration counters, keyed by a unique loop identifier.
    pub loop_counts: HashMap<String, usize>,

    /// Whether this state has reached a terminal point (STOP RUN / GOBACK).
    pub terminated: bool,
}

/// A frame on the PERFORM / CALL stack.
#[derive(Debug, Clone)]
pub struct CallFrame {
    /// Name of the paragraph or section being performed.
    pub paragraph_name: String,

    /// PC to resume at when the performed paragraph returns.
    pub return_pc: usize,

    /// Snapshot of variables at call site (used if we need to model
    /// pass-by-reference semantics or paragraph-local storage).
    pub saved_variables: HashMap<String, SymbolicValue>,
}

// ---------------------------------------------------------------------------
// Default
// ---------------------------------------------------------------------------

impl Default for ExecutionState {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// Implementation
// ---------------------------------------------------------------------------

impl ExecutionState {
    /// Create a fresh, empty execution state.
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            path_condition: PathCondition::new(),
            pc: 0,
            call_stack: Vec::new(),
            loop_counts: HashMap::new(),
            terminated: false,
        }
    }

    /// Look up a variable.
    pub fn get(&self, name: &str) -> Option<&SymbolicValue> {
        self.variables.get(name)
    }

    /// Get a variable, returning `Unknown` if not present.
    pub fn get_or_unknown(&self, name: &str) -> SymbolicValue {
        self.variables
            .get(name)
            .cloned()
            .unwrap_or(SymbolicValue::Unknown)
    }

    /// Set a variable.
    pub fn set(&mut self, name: impl Into<String>, value: SymbolicValue) {
        self.variables.insert(name.into(), value);
    }

    /// Whether the path condition is still satisfiable.
    pub fn is_feasible(&self) -> bool {
        self.path_condition.is_feasible
    }

    /// Mark this state as infeasible (dead path).
    pub fn mark_infeasible(&mut self) {
        self.path_condition.is_feasible = false;
    }

    /// Increment the iteration counter for `loop_id`.
    ///
    /// Returns `true` if the count is still within the bound, `false` if
    /// the maximum has been exceeded (callers should prune this path).
    pub fn increment_loop(&mut self, loop_id: &str, max_iterations: usize) -> bool {
        let count = self.loop_counts.entry(loop_id.to_string()).or_insert(0);
        *count += 1;
        *count <= max_iterations
    }

    /// Push a call frame onto the stack (entering a PERFORM).
    pub fn push_frame(&mut self, paragraph_name: String, return_pc: usize) {
        self.call_stack.push(CallFrame {
            paragraph_name,
            return_pc,
            saved_variables: HashMap::new(),
        });
    }

    /// Pop the top call frame (returning from a PERFORM).
    pub fn pop_frame(&mut self) -> Option<CallFrame> {
        self.call_stack.pop()
    }

    /// Current call depth.
    pub fn call_depth(&self) -> usize {
        self.call_stack.len()
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_state() {
        let s = ExecutionState::new();
        assert_eq!(s.pc, 0);
        assert!(s.is_feasible());
        assert!(s.variables.is_empty());
        assert!(s.call_stack.is_empty());
        assert!(!s.terminated);
    }

    #[test]
    fn get_set() {
        let mut s = ExecutionState::new();
        s.set("X", SymbolicValue::int(42));
        assert_eq!(s.get("X"), Some(&SymbolicValue::int(42)));
        assert_eq!(s.get("Y"), None);
        assert!(s.get_or_unknown("Y").is_unknown());
    }

    #[test]
    fn loop_bounding() {
        let mut s = ExecutionState::new();
        assert!(s.increment_loop("LOOP-1", 3));
        assert!(s.increment_loop("LOOP-1", 3));
        assert!(s.increment_loop("LOOP-1", 3));
        assert!(!s.increment_loop("LOOP-1", 3)); // 4th > 3
    }

    #[test]
    fn call_stack() {
        let mut s = ExecutionState::new();
        assert_eq!(s.call_depth(), 0);

        s.push_frame("PARA-A".into(), 10);
        assert_eq!(s.call_depth(), 1);

        s.push_frame("PARA-B".into(), 20);
        assert_eq!(s.call_depth(), 2);

        let frame = s.pop_frame().unwrap();
        assert_eq!(frame.paragraph_name, "PARA-B");
        assert_eq!(frame.return_pc, 20);
        assert_eq!(s.call_depth(), 1);
    }

    #[test]
    fn mark_infeasible() {
        let mut s = ExecutionState::new();
        assert!(s.is_feasible());
        s.mark_infeasible();
        assert!(!s.is_feasible());
    }
}

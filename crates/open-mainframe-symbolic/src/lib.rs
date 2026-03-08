//! Symbolic execution and formal verification engine for OpenMainframe.
//!
//! This crate provides a symbolic execution engine that can:
//!
//! 1. Execute COBOL programs symbolically (not just concrete values)
//! 2. Explore all execution paths automatically
//! 3. Verify properties (invariants, pre/postconditions)
//! 4. Generate counterexamples for violated properties
//! 5. Prove equivalence between old and new implementations
//!
//! # Architecture
//!
//! ```text
//! ┌────────────┐   ┌──────────────┐   ┌─────────────┐
//! │  spec.rs   │──▶│model_checker │──▶│ interpreter │
//! │ (Property) │   │  (verify)    │   │  (execute)  │
//! └────────────┘   └──────────────┘   └──────┬──────┘
//!                                            │
//!                  ┌──────────────┐   ┌──────▼──────┐
//!                  │  solver.rs   │◀──│  state.rs   │
//!                  │ (Z3 ↔ SMT)  │   │  path.rs    │
//!                  └──────────────┘   │  value.rs   │
//!                                     │  sort.rs    │
//!                                     └─────────────┘
//! ```

pub mod cli;
pub mod interpreter;
pub mod lowering;
pub mod model_checker;
pub mod path;
pub mod solver;
pub mod sort;
pub mod spec;
pub mod state;
pub mod value;

// Re-export key types at crate root for convenience.
pub use interpreter::{
    ExecutionResult, ExplorationStrategy, FlatProgramBuilder, FlatStatement, InterpreterConfig,
    SymbolicInterpreter,
};
pub use model_checker::{CoverageMetrics, ModelChecker, PropertyViolation, VerificationResult};
pub use path::{BranchDecision, Constraint, PathCondition};
pub use solver::{CheckResult, SymbolicSolver};
pub use sort::Sort;
pub use lowering::{lower_cobol_file, lower_cobol_source, LoweringResult, LoweringStats};
pub use spec::{Property, PropertyLocation};
pub use state::{CallFrame, ExecutionState};
pub use value::{ExprOp, SymbolicValue};

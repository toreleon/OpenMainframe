# Symbolic Execution Engine - Full Implementation Prompt

**Copy toàn bộ prompt này và đưa cho Claude Code:**

---

## Context

You are building a **Formal Verification Engine for Legacy Systems** on top of OpenMainframe - a z/OSMF-compatible mainframe emulator written in Rust (41 crates).

**Goal:** Create `crates/open-mainframe-symbolic` - a symbolic execution engine that can:
1. Execute COBOL programs symbolically (not just concrete values)
2. Explore all execution paths automatically
3. Verify properties (invariants, pre/postconditions)
4. Generate counterexamples for violated properties
5. Prove equivalence between old and new implementations

**Reference:** Read `crates/open-mainframe-cobol/` for COBOL AST structure and `crates/open-mainframe-lang-core/` for shared types.

---

## Phase 1: Core Data Structures

### 1.1 Create Crate Structure

```bash
cd /home/code/open-mainframe-project/OpenMainframe
mkdir -p crates/open-mainframe-symbolic/src
```

Create `crates/open-mainframe-symbolic/Cargo.toml`:
```toml
[package]
name = "open-mainframe-symbolic"
version.workspace = true
edition.workspace = true
rust-version.workspace = true
license.workspace = true

[dependencies]
# Workspace deps
open-mainframe-lang-core.workspace = true
open-mainframe-cobol.workspace = true

# External deps
z3 = { version = "0.12", features = ["static-link-z3"] }
serde = { workspace = true, features = ["derive"] }
serde_json.workspace = true
miette.workspace = true
thiserror.workspace = true
indexmap = "2"

[dev-dependencies]
proptest = "1"
```

### 1.2 Implement SymbolicValue (`src/value.rs`)

```rust
use serde::{Deserialize, Serialize};

/// A symbolic value that can be concrete or symbolic
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum SymbolicValue {
    /// Concrete integer value
    Concrete(i64),
    
    /// Concrete boolean value
    ConcreteBool(bool),
    
    /// Concrete string value
    ConcreteStr(String),
    
    /// Symbolic variable (e.g., input parameter X)
    Symbolic {
        name: String,
        sort: Sort,
    },
    
    /// Expression (e.g., X + Y * 2)
    Expression {
        op: ExprOp,
        args: Vec<SymbolicValue>,
    },
    
    /// Unknown/undefined (for uninitialized variables)
    Unknown,
}

/// Sort (type) system for symbolic values
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Sort {
    Int,
    Bool,
    String,
    BitVec(usize),  // bit width
    Array(Box<Sort>, Box<Sort>),  // (key_type, value_type)
}

/// Expression operators
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ExprOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Neg,
    
    // Comparison
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    
    // Boolean
    And,
    Or,
    Not,
    
    // String
    Concat,
    Substring,
    
    // Array
    Select,  // array[index]
    Store,   // array.with(index, value)
}

impl SymbolicValue {
    /// Create a concrete integer
    pub fn int(v: i64) -> Self {
        SymbolicValue::Concrete(v)
    }
    
    /// Create a symbolic integer variable
    pub fn sym_int(name: impl Into<String>) -> Self {
        SymbolicValue::Symbolic {
            name: name.into(),
            sort: Sort::Int,
        }
    }
    
    /// Create a symbolic boolean variable
    pub fn sym_bool(name: impl Into<String>) -> Self {
        SymbolicValue::Symbolic {
            name: name.into(),
            sort: Sort::Bool,
        }
    }
    
    /// Add two symbolic values
    pub fn add(self, other: Self) -> Self {
        match (self, other) {
            (SymbolicValue::Concrete(a), SymbolicValue::Concrete(b)) => {
                SymbolicValue::Concrete(a + b)
            }
            (a, b) => SymbolicValue::Expression {
                op: ExprOp::Add,
                args: vec![a, b],
            },
        }
    }
    
    /// Check if value is concrete
    pub fn is_concrete(&self) -> bool {
        matches!(self, SymbolicValue::Concrete(_) | SymbolicValue::ConcreteBool(_) | SymbolicValue::ConcreteStr(_))
    }
    
    /// Get concrete integer value if concrete
    pub fn as_concrete_int(&self) -> Option<i64> {
        match self {
            SymbolicValue::Concrete(v) => Some(*v),
            _ => None,
        }
    }
}
```

### 1.3 Implement Path Conditions (`src/path.rs`)

```rust
use crate::value::SymbolicValue;
use open_mainframe_lang_core::SourceSpan;

/// A path condition represents constraints accumulated along an execution path
#[derive(Debug, Clone, Default)]
pub struct PathCondition {
    constraints: Vec<Constraint>,
    pub is_feasible: bool,
}

/// A single constraint (e.g., X > 0)
#[derive(Debug, Clone)]
pub struct Constraint {
    pub condition: SymbolicValue,  // Should be boolean expression
    pub source: SourceSpan,
    pub description: Option<String>,
}

/// Branch decision at a conditional
#[derive(Debug, Clone)]
pub struct BranchDecision {
    pub condition: SymbolicValue,
    pub taken: bool,  // true = then branch, false = else branch
    pub source: SourceSpan,
    pub path_condition_before: PathCondition,
    pub path_condition_after: PathCondition,
}

impl PathCondition {
    pub fn new() -> Self {
        Self {
            constraints: Vec::new(),
            is_feasible: true,
        }
    }
    
    /// Add a constraint to this path
    pub fn add(&mut self, constraint: Constraint) {
        self.constraints.push(constraint);
    }
    
    /// Fork this path condition for a branch
    pub fn fork(&self, condition: SymbolicValue, taken: bool, source: SourceSpan) -> PathCondition {
        let mut forked = self.clone();
        forked.add(Constraint {
            condition: if taken { condition } else { negate(condition) },
            source,
            description: None,
        });
        forked
    }
    
    /// Merge two path conditions (for phi nodes / state merging)
    pub fn merge(&self, other: &PathCondition) -> PathCondition {
        // Conservative: union of constraints
        let mut merged = self.clone();
        for constraint in &other.constraints {
            if !merged.constraints.iter().any(|c| c.condition == constraint.condition) {
                merged.constraints.push(constraint.clone());
            }
        }
        merged
    }
    
    /// Get all constraints
    pub fn constraints(&self) -> &[Constraint] {
        &self.constraints
    }
}

fn negate(condition: SymbolicValue) -> SymbolicValue {
    SymbolicValue::Expression {
        op: ExprOp::Not,
        args: vec![condition],
    }
}
```

### 1.4 Implement Execution State (`src/state.rs`)

```rust
use std::collections::HashMap;
use crate::value::SymbolicValue;
use crate::path::PathCondition;

/// Symbolic execution state
#[derive(Debug, Clone)]
pub struct ExecutionState {
    /// Variable name -> symbolic value
    pub variables: HashMap<String, SymbolicValue>,
    
    /// Current path condition
    pub path_condition: PathCondition,
    
    /// Program counter (or AST node reference)
    pub pc: usize,
    
    /// Call stack (for nested PERFORMs)
    pub call_stack: Vec<CallFrame>,
    
    /// Loop iteration counts (for bounding)
    pub loop_counts: HashMap<String, usize>,
}

/// Call frame for PERFORM statements
#[derive(Debug, Clone)]
pub struct CallFrame {
    pub paragraph_name: String,
    pub return_pc: usize,
    pub saved_variables: HashMap<String, SymbolicValue>,
}

impl ExecutionState {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            path_condition: PathCondition::new(),
            pc: 0,
            call_stack: Vec::new(),
            loop_counts: HashMap::new(),
        }
    }
    
    /// Get variable value
    pub fn get(&self, name: &str) -> Option<&SymbolicValue> {
        self.variables.get(name)
    }
    
    /// Set variable value
    pub fn set(&mut self, name: String, value: SymbolicValue) {
        self.variables.insert(name, value);
    }
    
    /// Check if state is feasible (path condition is satisfiable)
    pub fn is_feasible(&self) -> bool {
        self.path_condition.is_feasible
    }
    
    /// Increment loop counter, return false if exceeded bound
    pub fn increment_loop(&mut self, loop_id: &str, max_iterations: usize) -> bool {
        let count = self.loop_counts.entry(loop_id.to_string()).or_insert(0);
        *count += 1;
        *count <= max_iterations
    }
}
```

---

## Phase 2: Z3 Integration

### 2.1 Z3 Solver Wrapper (`src/solver.rs`)

```rust
use z3::{Context, Solver, Model, SatResult, Config};
use crate::value::SymbolicValue;
use crate::path::PathCondition;

/// Z3 solver context
pub struct SymbolicSolver {
    ctx: Context,
    solver: Solver,
}

impl SymbolicSolver {
    pub fn new() -> Self {
        let cfg = Config::new();
        let ctx = Context::new(&cfg);
        let solver = Solver::new(&ctx);
        Self { ctx, solver }
    }
    
    /// Check if path condition is satisfiable
    pub fn check(&mut self, path: &PathCondition) -> SatResult {
        // Push to save current state
        self.solver.push();
        
        // Assert all constraints
        for constraint in path.constraints() {
            let z3_expr = self.to_z3_bool(&constraint.condition);
            self.solver.assert(&z3_expr);
        }
        
        // Check satisfiability
        let result = self.solver.check();
        
        // Pop to restore state
        self.solver.pop(1);
        
        result
    }
    
    /// Get a model (concrete values) for satisfiable path
    pub fn get_model(&mut self, path: &PathCondition) -> Option<Model> {
        if self.check(path) == SatResult::Sat {
            // Re-assert for model extraction
            self.solver.push();
            for constraint in path.constraints() {
                let z3_expr = self.to_z3_bool(&constraint.condition);
                self.solver.assert(&z3_expr);
            }
            
            if self.solver.check() == SatResult::Sat {
                let model = self.solver.get_model()?;
                self.solver.pop(1);
                return Some(model);
            }
            self.solver.pop(1);
        }
        None
    }
    
    /// Get unsat core for conflicting constraints
    pub fn get_unsat_core(&mut self, path: &PathCondition) -> Vec<usize> {
        // Enable unsat core tracking
        self.solver.push();
        
        let mut indices = Vec::new();
        for (i, constraint) in path.constraints().iter().enumerate() {
            let z3_expr = self.to_z3_bool(&constraint.condition);
            let name = format!("c{}", i);
            let const_decl = z3::ConstDecl::new(&self.ctx, &name);
            // ... (simplified for brevity)
            indices.push(i);
        }
        
        self.solver.pop(1);
        indices
    }
    
    /// Convert SymbolicValue to Z3 expression
    fn to_z3_bool(&mut self, value: &SymbolicValue) -> z3::Bool {
        // Implement conversion logic
        // This is complex - need to handle all SymbolicValue variants
        // For now, return a placeholder
        z3::Bool::from_bool(&self.ctx, true)
    }
}
```

---

## Phase 3: Symbolic Interpreter

### 3.1 Worklist-based Interpreter (`src/interpreter.rs`)

```rust
use std::collections::VecDeque;
use open_mainframe_cobol::ast::Program;
use crate::state::ExecutionState;
use crate::value::SymbolicValue;

/// Exploration strategy
#[derive(Debug, Clone, Copy)]
pub enum ExplorationStrategy {
    DFS,  // Depth-first search
    BFS,  // Breadth-first search
}

/// Symbolic interpreter configuration
#[derive(Debug, Clone)]
pub struct InterpreterConfig {
    pub max_loop_iterations: usize,
    pub max_paths: usize,
    pub max_depth: usize,
    pub strategy: ExplorationStrategy,
}

impl Default for InterpreterConfig {
    fn default() -> Self {
        Self {
            max_loop_iterations: 10,
            max_paths: 1000,
            max_depth: 100,
            strategy: ExplorationStrategy::DFS,
        }
    }
}

/// Result of symbolic execution
#[derive(Debug)]
pub struct ExecutionResult {
    pub paths_explored: usize,
    pub paths_feasible: usize,
    pub paths_infeasible: usize,
    pub paths_timeout: usize,
    pub final_states: Vec<ExecutionState>,
}

/// Symbolic interpreter
pub struct SymbolicInterpreter {
    config: InterpreterConfig,
    program: Program,
}

impl SymbolicInterpreter {
    pub fn new(program: Program, config: InterpreterConfig) -> Self {
        Self { config, program }
    }
    
    /// Execute symbolically with given initial state
    pub fn execute(&self, initial_state: ExecutionState) -> ExecutionResult {
        let mut worklist: VecDeque<ExecutionState> = VecDeque::new();
        worklist.push_back(initial_state);
        
        let mut final_states = Vec::new();
        let mut paths_explored = 0;
        let mut paths_feasible = 0;
        let mut paths_infeasible = 0;
        let mut paths_timeout = 0;
        
        while let Some(state) = match self.config.strategy {
            ExplorationStrategy::DFS => worklist.pop_back(),
            ExplorationStrategy::BFS => worklist.pop_front(),
        } {
            if paths_explored >= self.config.max_paths {
                paths_timeout += 1;
                continue;
            }
            
            if !state.is_feasible() {
                paths_infeasible += 1;
                continue;
            }
            
            // Execute one step
            let next_states = self.execute_step(state);
            
            if next_states.is_empty() {
                // Program terminated
                final_states.push(next_states.into_iter().next().unwrap());
                paths_feasible += 1;
            } else {
                worklist.extend(next_states);
            }
            
            paths_explored += 1;
        }
        
        ExecutionResult {
            paths_explored,
            paths_feasible,
            paths_infeasible,
            paths_timeout,
            final_states,
        }
    }
    
    /// Execute one step, return successor states (0, 1, or 2 for branches)
    fn execute_step(&self, state: ExecutionState) -> Vec<ExecutionState> {
        // Get current AST node
        // Match on statement type:
        // - MOVE: update variable
        // - IF: fork into two states
        // - PERFORM: push call frame
        // - etc.
        
        // This is the core logic - need to handle all COBOL statements
        vec![]  // Placeholder
    }
}
```

---

## Phase 4: Property Specification

### 4.1 Property Types (`src/spec.rs`)

```rust
use crate::value::SymbolicValue;

/// Verification property
#[derive(Debug, Clone)]
pub enum Property {
    /// Invariant: always true at specified location
    Invariant {
        name: String,
        condition: SymbolicValue,
        location: PropertyLocation,
    },
    
    /// Implication: precondition -> postcondition
    Implication {
        name: String,
        precondition: SymbolicValue,
        postcondition: SymbolicValue,
    },
    
    /// Safety: condition that must never be violated
    Safety {
        name: String,
        violation_condition: SymbolicValue,
    },
    
    /// Equivalence: two programs produce same output
    Equivalence {
        name: String,
        program_a_output: SymbolicValue,
        program_b_output: SymbolicValue,
        tolerance: Option<f64>,
    },
}

/// Where to check the property
#[derive(Debug, Clone)]
pub enum PropertyLocation {
    /// At start of program
    Entry,
    /// At end of program
    Exit,
    /// At specific line/column
    Source { line: usize, column: usize },
    /// At specific paragraph
    Paragraph(String),
    /// Everywhere
    Global,
}

/// Parse properties from YAML
pub fn parse_properties(yaml: &str) -> Result<Vec<Property>, ParseError> {
    // Implement YAML parsing
    // Expected format:
    // properties:
    //   - name: "balance_non_negative"
    //     type: invariant
    //     condition: "BALANCE >= 0"
    //     location: exit
    todo!()
}

/// Parse COBOL annotations
pub fn parse_annotations(source: &str) -> Vec<Property> {
    // Look for comments like:
    // *> @INVARIANT: BALANCE >= 0
    // *> @PRE: AMOUNT > 0
    // *> @POST: NEW_BALANCE = OLD_BALANCE + AMOUNT
    todo!()
}
```

---

## Phase 5: Model Checker

### 5.1 Verification Engine (`src/model_checker.rs`)

```rust
use crate::interpreter::SymbolicInterpreter;
use crate::spec::Property;
use crate::state::ExecutionState;

/// Verification result
#[derive(Debug)]
pub struct VerificationResult {
    pub properties_checked: usize,
    pub properties_passed: usize,
    pub properties_failed: Vec<PropertyViolation>,
    pub coverage: CoverageMetrics,
}

/// Property violation with counterexample
#[derive(Debug)]
pub struct PropertyViolation {
    pub property: Property,
    pub counterexample: ExecutionState,
    pub message: String,
}

/// Coverage metrics
#[derive(Debug, Default)]
pub struct CoverageMetrics {
    pub branch_coverage: f64,  // 0.0 to 1.0
    pub path_coverage: f64,
    pub total_branches: usize,
    pub covered_branches: usize,
}

/// Model checker
pub struct ModelChecker {
    interpreter: SymbolicInterpreter,
}

impl ModelChecker {
    pub fn new(interpreter: SymbolicInterpreter) -> Self {
        Self { interpreter }
    }
    
    /// Verify all properties
    pub fn verify(&self, properties: &[Property]) -> VerificationResult {
        let mut result = VerificationResult {
            properties_checked: properties.len(),
            properties_passed: 0,
            properties_failed: Vec::new(),
            coverage: CoverageMetrics::default(),
        };
        
        for property in properties {
            match self.check_property(property) {
                Ok(_) => result.properties_passed += 1,
                Err(violation) => result.properties_failed.push(violation),
            }
        }
        
        result
    }
    
    fn check_property(&self, property: &Property) -> Result<(), PropertyViolation> {
        // Execute symbolically
        // Check property against all final states
        // Return counterexample if violated
        todo!()
    }
}
```

---

## Phase 6: CLI & Integration

### 6.1 CLI Command (`src/cli.rs`)

```rust
use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(name = "openmf")]
#[command(about = "OpenMainframe Symbolic Execution Engine")]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,
}

#[derive(Subcommand)]
pub enum Commands {
    /// Verify a COBOL program against properties
    Verify {
        /// COBOL source file
        #[arg(short, long)]
        program: String,
        
        /// Properties file (YAML)
        #[arg(short, long)]
        properties: Option<String>,
        
        /// Output format (json, text)
        #[arg(short, long, default_value = "text")]
        output: String,
        
        /// Max paths to explore
        #[arg(long, default_value = "1000")]
        max_paths: usize,
    },
    
    /// Generate test cases from symbolic execution
    Testgen {
        #[arg(short, long)]
        program: String,
        
        /// Output directory
        #[arg(short, long)]
        output: String,
    },
    
    /// Check equivalence between two programs
    Equivalence {
        #[arg(long)]
        old_program: String,
        
        #[arg(long)]
        new_program: String,
    },
}
```

---

## Testing

Write comprehensive tests:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_symbolic_addition() {
        let x = SymbolicValue::sym_int("X");
        let y = SymbolicValue::sym_int("Y");
        let sum = x.add(y.clone());
        
        assert!(matches!(sum, SymbolicValue::Expression { op: ExprOp::Add, .. }));
    }
    
    #[test]
    fn test_path_forking() {
        let mut path = PathCondition::new();
        let condition = SymbolicValue::sym_bool("X_POSITIVE");
        
        let then_branch = path.fork(condition.clone(), true, SourceSpan::default());
        let else_branch = path.fork(condition, false, SourceSpan::default());
        
        // Then branch should have X_POSITIVE
        // Else branch should have NOT X_POSITIVE
    }
    
    #[test]
    fn test_simple_cobol_program() {
        // Parse a simple COBOL program
        // Execute symbolically
        // Verify properties
    }
}
```

---

## Deliverables Checklist

- [ ] `crates/open-mainframe-symbolic/Cargo.toml`
- [ ] `src/lib.rs` - Module root with exports
- [ ] `src/value.rs` - SymbolicValue, Sort, ExprOp
- [ ] `src/path.rs` - PathCondition, BranchDecision, Constraint
- [ ] `src/state.rs` - ExecutionState, CallFrame
- [ ] `src/solver.rs` - SymbolicSolver with Z3 integration
- [ ] `src/interpreter.rs` - SymbolicInterpreter with worklist
- [ ] `src/spec.rs` - Property types and parsers
- [ ] `src/model_checker.rs` - Verification engine
- [ ] `src/cli.rs` - CLI commands
- [ ] `src/main.rs` - Binary entry point
- [ ] Unit tests for all modules
- [ ] Integration test with sample COBOL program
- [ ] Documentation (rustdoc comments)

---

## Start Now

1. Read existing crates to understand workspace conventions
2. Create the crate structure
3. Implement Phase 1 (data structures)
4. Implement Phase 2 (Z3 integration)
5. Implement Phase 3 (interpreter)
6. Implement Phase 4-6 (properties, model checker, CLI)
7. Write tests
8. Run `cargo clippy` and `cargo fmt`

**Ask for clarification if stuck. Report progress after each phase.**

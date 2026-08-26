//! Worklist-based symbolic interpreter for COBOL programs.
//!
//! The [`SymbolicInterpreter`] traverses the COBOL AST symbolically,
//! forking execution states at branches and accumulating path conditions.
//! Exploration is bounded by configurable limits on loop iterations,
//! path count, and call depth.

use std::collections::VecDeque;

use open_mainframe_lang_core::Span;

use crate::solver::SymbolicSolver;
use crate::state::ExecutionState;
use crate::value::SymbolicValue;

/// Exploration strategy for the worklist.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExplorationStrategy {
    /// Depth-first search — explores deep paths before wide.
    DFS,
    /// Breadth-first search — explores wide before deep.
    BFS,
}

/// Configuration for the symbolic interpreter.
#[derive(Debug, Clone)]
pub struct InterpreterConfig {
    /// Maximum loop iterations before pruning a path.
    pub max_loop_iterations: usize,
    /// Maximum total paths to explore.
    pub max_paths: usize,
    /// Maximum call stack depth.
    pub max_depth: usize,
    /// Exploration strategy.
    pub strategy: ExplorationStrategy,
    /// Whether to check constraint feasibility at every branch.
    pub eager_feasibility: bool,
}

impl Default for InterpreterConfig {
    fn default() -> Self {
        Self {
            max_loop_iterations: 10,
            max_paths: 1000,
            max_depth: 100,
            strategy: ExplorationStrategy::DFS,
            eager_feasibility: true,
        }
    }
}

/// Aggregated result of symbolic execution.
#[derive(Debug)]
pub struct ExecutionResult {
    /// Total paths explored (including pruned).
    pub paths_explored: usize,
    /// Paths that reached a terminal state and are feasible.
    pub paths_feasible: usize,
    /// Paths pruned because they were infeasible.
    pub paths_infeasible: usize,
    /// Paths pruned because of resource bounds.
    pub paths_bounded: usize,
    /// Terminal states (one per feasible completed path).
    pub final_states: Vec<ExecutionState>,
}

/// A flattened statement for the interpreter to step through.
///
/// The interpreter converts the nested COBOL AST into a linear list of
/// these items so the worklist can use a simple program counter.
#[derive(Debug, Clone)]
pub enum FlatStatement {
    /// Assign: `target = value`.
    Assign {
        target: String,
        value: SymbolicValue,
        span: Span,
    },
    /// Conditional branch.
    Branch {
        condition: SymbolicValue,
        then_pc: usize,
        else_pc: usize,
        span: Span,
    },
    /// Enter a performed paragraph.
    PerformEnter {
        paragraph: String,
        return_pc: usize,
        span: Span,
    },
    /// Return from a performed paragraph.
    PerformReturn,
    /// Loop header.
    LoopHeader {
        loop_id: String,
        condition: SymbolicValue,
        body_pc: usize,
        exit_pc: usize,
        test_before: bool,
        span: Span,
    },
    /// Loop back-edge (jump back to loop header).
    LoopBack { header_pc: usize },
    /// Stop the program.
    Stop,
    /// No-op placeholder.
    Nop,
}

/// The symbolic interpreter.
pub struct SymbolicInterpreter {
    config: InterpreterConfig,
    statements: Vec<FlatStatement>,
    solver: SymbolicSolver,
}

impl SymbolicInterpreter {
    /// Create a new interpreter from a flat statement list.
    pub fn new(statements: Vec<FlatStatement>, config: InterpreterConfig) -> Self {
        Self {
            solver: SymbolicSolver::new(),
            config,
            statements,
        }
    }

    /// Run symbolic execution from an initial state.
    pub fn execute(&self, initial_state: ExecutionState) -> ExecutionResult {
        let mut worklist: VecDeque<ExecutionState> = VecDeque::new();
        worklist.push_back(initial_state);

        let mut final_states = Vec::new();
        let mut paths_explored: usize = 0;
        let mut paths_feasible: usize = 0;
        let mut paths_infeasible: usize = 0;
        let mut paths_bounded: usize = 0;

        while let Some(state) = self.pop(&mut worklist) {
            if paths_explored >= self.config.max_paths {
                paths_bounded += 1;
                continue;
            }

            if !state.is_feasible() {
                paths_infeasible += 1;
                continue;
            }

            paths_explored += 1;

            let next_states = self.step(state);

            if next_states.is_empty() {
                // No successors means the last step was a terminal.
                paths_feasible += 1;
            } else {
                for ns in next_states {
                    if ns.terminated {
                        final_states.push(ns);
                        paths_feasible += 1;
                    } else {
                        worklist.push_back(ns);
                    }
                }
            }
        }

        ExecutionResult {
            paths_explored,
            paths_feasible,
            paths_infeasible,
            paths_bounded,
            final_states,
        }
    }

    /// Access the flat statement list.
    pub fn statements(&self) -> &[FlatStatement] {
        &self.statements
    }

    // -----------------------------------------------------------------------
    // Internal helpers
    // -----------------------------------------------------------------------

    /// Pop from the worklist according to the chosen strategy.
    fn pop(&self, worklist: &mut VecDeque<ExecutionState>) -> Option<ExecutionState> {
        match self.config.strategy {
            ExplorationStrategy::DFS => worklist.pop_back(),
            ExplorationStrategy::BFS => worklist.pop_front(),
        }
    }

    /// Execute one step, returning 0, 1, or 2 successor states.
    fn step(&self, mut state: ExecutionState) -> Vec<ExecutionState> {
        if state.pc >= self.statements.len() {
            state.terminated = true;
            return vec![state];
        }

        let stmt = &self.statements[state.pc];

        match stmt {
            FlatStatement::Assign { target, value, .. } => {
                state.set(target.clone(), value.clone());
                state.pc += 1;
                vec![state]
            }

            FlatStatement::Branch {
                condition,
                then_pc,
                else_pc,
                span,
            } => self.fork_branch(state, condition.clone(), *then_pc, *else_pc, *span),

            FlatStatement::PerformEnter {
                paragraph,
                return_pc,
                ..
            } => {
                if state.call_depth() >= self.config.max_depth {
                    // Prune: too deep.
                    state.terminated = true;
                    return vec![state];
                }
                state.push_frame(paragraph.clone(), *return_pc);
                state.pc += 1;
                vec![state]
            }

            FlatStatement::PerformReturn => {
                if let Some(frame) = state.pop_frame() {
                    state.pc = frame.return_pc;
                } else {
                    // No frame to pop — treat as program end.
                    state.terminated = true;
                }
                vec![state]
            }

            FlatStatement::LoopHeader {
                loop_id,
                condition,
                body_pc,
                exit_pc,
                test_before,
                span,
            } => {
                if !state.increment_loop(loop_id, self.config.max_loop_iterations) {
                    // Exceeded bound — take the exit.
                    state.pc = *exit_pc;
                    return vec![state];
                }

                if *test_before {
                    self.fork_branch(state, condition.clone(), *body_pc, *exit_pc, *span)
                } else {
                    // TEST AFTER: enter body unconditionally, check after.
                    state.pc = *body_pc;
                    vec![state]
                }
            }

            FlatStatement::LoopBack { header_pc } => {
                state.pc = *header_pc;
                vec![state]
            }

            FlatStatement::Stop => {
                state.terminated = true;
                vec![state]
            }

            FlatStatement::Nop => {
                state.pc += 1;
                vec![state]
            }
        }
    }

    /// Fork the state into then/else branches, checking feasibility if
    /// `eager_feasibility` is enabled.
    fn fork_branch(
        &self,
        state: ExecutionState,
        condition: SymbolicValue,
        then_pc: usize,
        else_pc: usize,
        span: Span,
    ) -> Vec<ExecutionState> {
        let mut results = Vec::with_capacity(2);

        // Then branch.
        let then_feasible = !self.config.eager_feasibility
            || self.solver.is_feasible(&state.path_condition, &condition);

        if then_feasible {
            let mut then_state = state.clone();
            then_state.path_condition = state.path_condition.fork(condition.clone(), true, span);
            then_state.pc = then_pc;
            results.push(then_state);
        }

        // Else branch.
        let neg_condition = condition.clone().not();
        let else_feasible = !self.config.eager_feasibility
            || self
                .solver
                .is_feasible(&state.path_condition, &neg_condition);

        if else_feasible {
            let mut else_state = state;
            else_state.path_condition = else_state.path_condition.fork(condition, false, span);
            else_state.pc = else_pc;
            results.push(else_state);
        }

        results
    }
}

// ---------------------------------------------------------------------------
// Statement flattening helpers
// ---------------------------------------------------------------------------

/// Builder that helps convert nested COBOL-like structures into a flat
/// [`FlatStatement`] list.
pub struct FlatProgramBuilder {
    stmts: Vec<FlatStatement>,
}

impl FlatProgramBuilder {
    pub fn new() -> Self {
        Self { stmts: Vec::new() }
    }

    /// Current emission offset (next PC).
    pub fn next_pc(&self) -> usize {
        self.stmts.len()
    }

    /// Emit a statement, returning its PC.
    pub fn emit(&mut self, stmt: FlatStatement) -> usize {
        let pc = self.stmts.len();
        self.stmts.push(stmt);
        pc
    }

    /// Patch a branch target after emission (used for forward jumps).
    pub fn patch_then(&mut self, pc: usize, target: usize) {
        if let FlatStatement::Branch { then_pc, .. } = &mut self.stmts[pc] {
            *then_pc = target;
        }
    }

    pub fn patch_else(&mut self, pc: usize, target: usize) {
        if let FlatStatement::Branch { else_pc, .. } = &mut self.stmts[pc] {
            *else_pc = target;
        }
    }

    /// Finish building, consuming the builder.
    pub fn build(self) -> Vec<FlatStatement> {
        self.stmts
    }
}

impl Default for FlatProgramBuilder {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn span() -> Span {
        Span::default()
    }

    #[test]
    fn linear_program() {
        // X = 1; Y = 2; STOP
        let stmts = vec![
            FlatStatement::Assign {
                target: "X".into(),
                value: SymbolicValue::int(1),
                span: span(),
            },
            FlatStatement::Assign {
                target: "Y".into(),
                value: SymbolicValue::int(2),
                span: span(),
            },
            FlatStatement::Stop,
        ];

        let interp = SymbolicInterpreter::new(stmts, InterpreterConfig::default());
        let result = interp.execute(ExecutionState::new());

        assert_eq!(result.paths_feasible, 1);
        assert_eq!(result.final_states.len(), 1);
        let state = &result.final_states[0];
        assert_eq!(state.get("X"), Some(&SymbolicValue::int(1)));
        assert_eq!(state.get("Y"), Some(&SymbolicValue::int(2)));
    }

    #[test]
    fn simple_branch() {
        // IF FLAG THEN X = 1 ELSE X = 2; STOP
        let stmts = vec![
            /* 0 */
            FlatStatement::Branch {
                condition: SymbolicValue::sym_bool("FLAG"),
                then_pc: 1,
                else_pc: 3,
                span: span(),
            },
            /* 1 */
            FlatStatement::Assign {
                target: "X".into(),
                value: SymbolicValue::int(1),
                span: span(),
            },
            /* 2 */
            FlatStatement::Branch {
                // unconditional jump to STOP
                condition: SymbolicValue::bool(true),
                then_pc: 4,
                else_pc: 4,
                span: span(),
            },
            /* 3 */
            FlatStatement::Assign {
                target: "X".into(),
                value: SymbolicValue::int(2),
                span: span(),
            },
            /* 4 */ FlatStatement::Stop,
        ];

        let interp = SymbolicInterpreter::new(stmts, InterpreterConfig::default());
        let result = interp.execute(ExecutionState::new());

        assert_eq!(result.final_states.len(), 2);
        let xs: Vec<i64> = result
            .final_states
            .iter()
            .filter_map(|s| s.get("X")?.as_concrete_int())
            .collect();
        assert!(xs.contains(&1));
        assert!(xs.contains(&2));
    }

    #[test]
    fn loop_bounding() {
        // PERFORM UNTIL DONE: X = X + 1; DONE
        let stmts = vec![
            /* 0 */
            FlatStatement::LoopHeader {
                loop_id: "L1".into(),
                condition: SymbolicValue::sym_bool("DONE"),
                body_pc: 1,
                exit_pc: 3,
                test_before: true,
                span: span(),
            },
            /* 1 */
            FlatStatement::Assign {
                target: "X".into(),
                value: SymbolicValue::int(1),
                span: span(),
            },
            /* 2 */ FlatStatement::LoopBack { header_pc: 0 },
            /* 3 */ FlatStatement::Stop,
        ];

        let mut config = InterpreterConfig::default();
        config.max_loop_iterations = 3;
        config.eager_feasibility = false; // skip constraint solving for speed

        let interp = SymbolicInterpreter::new(stmts, config);
        let result = interp.execute(ExecutionState::new());

        // Should terminate (not infinite loop).
        assert!(result.paths_explored > 0);
    }

    #[test]
    fn builder_basic() {
        let mut b = FlatProgramBuilder::new();
        let pc0 = b.emit(FlatStatement::Assign {
            target: "X".into(),
            value: SymbolicValue::int(42),
            span: span(),
        });
        assert_eq!(pc0, 0);

        let branch_pc = b.emit(FlatStatement::Branch {
            condition: SymbolicValue::bool(true),
            then_pc: 0,
            else_pc: 0,
            span: span(),
        });
        b.patch_then(branch_pc, 99);
        b.patch_else(branch_pc, 100);

        let stmts = b.build();
        assert!(matches!(
            &stmts[branch_pc],
            FlatStatement::Branch {
                then_pc: 99,
                else_pc: 100,
                ..
            }
        ));
    }

    #[test]
    fn perform_enter_return() {
        let stmts = vec![
            /* 0 */
            FlatStatement::PerformEnter {
                paragraph: "PARA-A".into(),
                return_pc: 3,
                span: span(),
            },
            /* 1 */
            FlatStatement::Assign {
                target: "X".into(),
                value: SymbolicValue::int(99),
                span: span(),
            },
            /* 2 */ FlatStatement::PerformReturn,
            /* 3 */ FlatStatement::Stop,
        ];

        let interp = SymbolicInterpreter::new(stmts, InterpreterConfig::default());
        let result = interp.execute(ExecutionState::new());

        assert_eq!(result.final_states.len(), 1);
        assert_eq!(
            result.final_states[0].get("X"),
            Some(&SymbolicValue::int(99))
        );
    }
}

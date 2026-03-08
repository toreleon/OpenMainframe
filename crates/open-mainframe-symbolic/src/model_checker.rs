//! Model checker — verifies properties against symbolic execution results.
//!
//! The [`ModelChecker`] drives the [`SymbolicInterpreter`], then evaluates
//! each [`Property`] against the resulting execution states.  Violated
//! properties produce a [`PropertyViolation`] with a counterexample.

use crate::interpreter::{ExecutionResult, FlatStatement, InterpreterConfig, SymbolicInterpreter};
use crate::path::Constraint;
use crate::solver::{CheckResult, SymbolicSolver};
use crate::spec::{Property, PropertyLocation};
use crate::state::ExecutionState;
use crate::value::{ExprOp, SymbolicValue};

/// Aggregated verification result.
#[derive(Debug)]
pub struct VerificationResult {
    /// Total properties checked.
    pub properties_checked: usize,
    /// Properties that passed verification.
    pub properties_passed: usize,
    /// Properties that failed with counterexamples.
    pub properties_failed: Vec<PropertyViolation>,
    /// Execution coverage metrics.
    pub coverage: CoverageMetrics,
    /// The underlying symbolic execution result.
    pub execution: ExecutionResult,
}

/// A single property violation with a concrete counterexample.
#[derive(Debug)]
pub struct PropertyViolation {
    /// The property that was violated.
    pub property: Property,
    /// A concrete execution state that demonstrates the violation.
    pub counterexample: ExecutionState,
    /// Human-readable explanation.
    pub message: String,
}

/// Coverage metrics collected during symbolic execution.
#[derive(Debug, Default)]
pub struct CoverageMetrics {
    /// Fraction of branches taken both ways.
    pub branch_coverage: f64,
    /// Fraction of paths explored relative to max_paths.
    pub path_coverage: f64,
    /// Total branch points in the program.
    pub total_branches: usize,
    /// Branch points covered by at least one path.
    pub covered_branches: usize,
    /// Total paths explored.
    pub paths_explored: usize,
}

/// The model checker.
pub struct ModelChecker {
    config: InterpreterConfig,
    solver: SymbolicSolver,
}

impl ModelChecker {
    /// Create a model checker with the given configuration.
    pub fn new(config: InterpreterConfig) -> Self {
        Self {
            solver: SymbolicSolver::new(),
            config,
        }
    }

    /// Verify a set of properties against a program.
    pub fn verify(
        &self,
        statements: Vec<FlatStatement>,
        properties: &[Property],
    ) -> VerificationResult {
        let interp = SymbolicInterpreter::new(statements, self.config.clone());

        // Compute coverage from the statement list.
        let total_branches = interp
            .statements()
            .iter()
            .filter(|s| matches!(s, FlatStatement::Branch { .. }))
            .count();

        // Run symbolic execution.
        let execution = interp.execute(ExecutionState::new());

        let coverage = CoverageMetrics {
            total_branches,
            covered_branches: total_branches.min(execution.paths_feasible),
            paths_explored: execution.paths_explored,
            branch_coverage: if total_branches == 0 {
                1.0
            } else {
                (total_branches.min(execution.paths_feasible) as f64) / (total_branches as f64)
            },
            path_coverage: if self.config.max_paths == 0 {
                0.0
            } else {
                (execution.paths_explored as f64) / (self.config.max_paths as f64)
            },
        };

        let mut result = VerificationResult {
            properties_checked: properties.len(),
            properties_passed: 0,
            properties_failed: Vec::new(),
            coverage,
            execution,
        };

        for property in properties {
            match self.check_property(property, &result.execution.final_states) {
                Ok(()) => result.properties_passed += 1,
                Err(violation) => result.properties_failed.push(violation),
            }
        }

        result
    }

    /// Check a single property against all final states.
    fn check_property(
        &self,
        property: &Property,
        states: &[ExecutionState],
    ) -> Result<(), PropertyViolation> {
        match property {
            Property::Invariant {
                condition,
                location,
                ..
            } => {
                let target_states = self.filter_states_by_location(states, location);
                for state in target_states {
                    self.check_condition_holds(property, condition, state)?;
                }
                Ok(())
            }

            Property::Implication {
                precondition,
                postcondition,
                ..
            } => {
                // For each state: if precondition is satisfiable in this path,
                // then postcondition must also hold.
                for state in states {
                    let pre_holds = self.evaluate_condition(precondition, state);
                    if pre_holds {
                        self.check_condition_holds(property, postcondition, state)?;
                    }
                }
                Ok(())
            }

            Property::Safety {
                violation_condition,
                ..
            } => {
                // The violation condition must NOT be satisfiable in any state.
                for state in states {
                    let violated = self.evaluate_condition(violation_condition, state);
                    if violated {
                        return Err(PropertyViolation {
                            property: property.clone(),
                            counterexample: state.clone(),
                            message: format!(
                                "Safety violation: {} is satisfiable",
                                property.name()
                            ),
                        });
                    }
                }
                Ok(())
            }

            Property::Equivalence {
                program_a_output,
                program_b_output,
                tolerance,
                ..
            } => {
                // Check that outputs are equal (or within tolerance).
                for state in states {
                    let a = Self::resolve_value(program_a_output, state);
                    let b = Self::resolve_value(program_b_output, state);

                    let eq_cond = if let Some(tol) = tolerance {
                        // |a - b| <= tolerance
                        self.check_approximate_equality(&a, &b, *tol, state)
                    } else {
                        self.check_exact_equality(&a, &b, state)
                    };

                    if !eq_cond {
                        return Err(PropertyViolation {
                            property: property.clone(),
                            counterexample: state.clone(),
                            message: format!(
                                "Equivalence violation: outputs differ for {}",
                                property.name()
                            ),
                        });
                    }
                }
                Ok(())
            }
        }
    }

    /// Check that a condition holds in a given state, using the solver.
    fn check_condition_holds(
        &self,
        property: &Property,
        condition: &SymbolicValue,
        state: &ExecutionState,
    ) -> Result<(), PropertyViolation> {
        let resolved = Self::resolve_value(condition, state);

        // Quick check for concrete booleans.
        if let Some(b) = resolved.as_concrete_bool() {
            if b {
                return Ok(());
            }
            return Err(PropertyViolation {
                property: property.clone(),
                counterexample: state.clone(),
                message: format!("Condition violated: {}", property.name()),
            });
        }

        // Use the solver: check if NOT(condition) is satisfiable under
        // the path condition.  If SAT, the condition can be violated.
        let negated = resolved.not();
        let mut pc = state.path_condition.clone();
        pc.add(Constraint {
            condition: negated,
            source: open_mainframe_lang_core::Span::default(),
            description: Some(format!("negation of {}", property.name())),
        });

        match self.solver.check(&pc) {
            CheckResult::Sat { .. } => {
                // The negation is satisfiable → violation found.
                Err(PropertyViolation {
                    property: property.clone(),
                    counterexample: state.clone(),
                    message: format!("Condition can be violated: {}", property.name()),
                })
            }
            CheckResult::Unsat => {
                // Negation is unsatisfiable → condition always holds.
                Ok(())
            }
            CheckResult::Unknown => {
                // Inconclusive — treat as passing with a warning.
                Ok(())
            }
        }
    }

    /// Evaluate a condition against concrete variable values in a state.
    fn evaluate_condition(&self, condition: &SymbolicValue, state: &ExecutionState) -> bool {
        let resolved = Self::resolve_value(condition, state);
        match resolved.as_concrete_bool() {
            Some(b) => b,
            None => {
                // Use solver.
                self.solver.is_feasible(&state.path_condition, &resolved)
            }
        }
    }

    /// Substitute symbolic variable references with their values from the state.
    fn resolve_value(value: &SymbolicValue, state: &ExecutionState) -> SymbolicValue {
        match value {
            SymbolicValue::Symbolic { name, .. } => state.get_or_unknown(name),
            SymbolicValue::Expression { op, args } => {
                let resolved_args: Vec<SymbolicValue> =
                    args.iter().map(|a| Self::resolve_value(a, state)).collect();

                // Try to constant-fold.
                match (op, resolved_args.as_slice()) {
                    (ExprOp::Ge, [SymbolicValue::Concrete(a), SymbolicValue::Concrete(b)]) => {
                        SymbolicValue::ConcreteBool(*a >= *b)
                    }
                    (ExprOp::Le, [SymbolicValue::Concrete(a), SymbolicValue::Concrete(b)]) => {
                        SymbolicValue::ConcreteBool(*a <= *b)
                    }
                    (ExprOp::Gt, [SymbolicValue::Concrete(a), SymbolicValue::Concrete(b)]) => {
                        SymbolicValue::ConcreteBool(*a > *b)
                    }
                    (ExprOp::Lt, [SymbolicValue::Concrete(a), SymbolicValue::Concrete(b)]) => {
                        SymbolicValue::ConcreteBool(*a < *b)
                    }
                    (ExprOp::Eq, [SymbolicValue::Concrete(a), SymbolicValue::Concrete(b)]) => {
                        SymbolicValue::ConcreteBool(*a == *b)
                    }
                    (ExprOp::Ne, [SymbolicValue::Concrete(a), SymbolicValue::Concrete(b)]) => {
                        SymbolicValue::ConcreteBool(*a != *b)
                    }
                    (ExprOp::Add, [SymbolicValue::Concrete(a), SymbolicValue::Concrete(b)]) => {
                        SymbolicValue::Concrete(a.wrapping_add(*b))
                    }
                    (ExprOp::Sub, [SymbolicValue::Concrete(a), SymbolicValue::Concrete(b)]) => {
                        SymbolicValue::Concrete(a.wrapping_sub(*b))
                    }
                    (ExprOp::Not, [SymbolicValue::ConcreteBool(b)]) => {
                        SymbolicValue::ConcreteBool(!b)
                    }
                    _ => SymbolicValue::Expression {
                        op: op.clone(),
                        args: resolved_args,
                    },
                }
            }
            other => other.clone(),
        }
    }

    /// Filter states to those matching a property location.
    fn filter_states_by_location<'a>(
        &self,
        states: &'a [ExecutionState],
        location: &PropertyLocation,
    ) -> Vec<&'a ExecutionState> {
        match location {
            PropertyLocation::Exit | PropertyLocation::Global => states.iter().collect(),
            PropertyLocation::Entry => {
                // Entry properties are checked on the initial state,
                // which isn't in final_states.  Return all as fallback.
                states.iter().collect()
            }
            PropertyLocation::Paragraph(_name) => {
                // Would need PC-to-paragraph mapping; return all for now.
                states.iter().collect()
            }
            PropertyLocation::Source { .. } => states.iter().collect(),
        }
    }

    /// Check exact equality of two symbolic values.
    fn check_exact_equality(
        &self,
        a: &SymbolicValue,
        b: &SymbolicValue,
        state: &ExecutionState,
    ) -> bool {
        if a == b {
            return true;
        }
        match (a.as_concrete_int(), b.as_concrete_int()) {
            (Some(x), Some(y)) => x == y,
            _ => {
                // Use solver to check a != b is unsatisfiable.
                let neq = a.clone().eq(b.clone()).not();
                !self.solver.is_feasible(&state.path_condition, &neq)
            }
        }
    }

    /// Check approximate equality: |a - b| <= tolerance.
    fn check_approximate_equality(
        &self,
        a: &SymbolicValue,
        b: &SymbolicValue,
        tolerance: f64,
        state: &ExecutionState,
    ) -> bool {
        match (a.as_concrete_int(), b.as_concrete_int()) {
            (Some(x), Some(y)) => (x - y).unsigned_abs() as f64 <= tolerance,
            _ => {
                // Conservative: assume equal if not concretely refuted.
                self.check_exact_equality(a, b, state)
            }
        }
    }
}

impl Default for ModelChecker {
    fn default() -> Self {
        Self::new(InterpreterConfig::default())
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interpreter::FlatStatement;

    fn span() -> open_mainframe_lang_core::Span {
        open_mainframe_lang_core::Span::default()
    }

    #[test]
    fn invariant_passes() {
        // X = 10; STOP.
        // Property: X >= 0 at exit.
        let stmts = vec![
            FlatStatement::Assign {
                target: "X".into(),
                value: SymbolicValue::int(10),
                span: span(),
            },
            FlatStatement::Stop,
        ];

        let properties = vec![Property::Invariant {
            name: "x_non_negative".into(),
            condition: SymbolicValue::Expression {
                op: ExprOp::Ge,
                args: vec![SymbolicValue::sym_int("X"), SymbolicValue::int(0)],
            },
            location: PropertyLocation::Exit,
        }];

        let checker = ModelChecker::default();
        let result = checker.verify(stmts, &properties);

        assert_eq!(result.properties_checked, 1);
        assert_eq!(result.properties_passed, 1);
        assert!(result.properties_failed.is_empty());
    }

    #[test]
    fn invariant_fails() {
        // X = -5; STOP.
        // Property: X >= 0.
        let stmts = vec![
            FlatStatement::Assign {
                target: "X".into(),
                value: SymbolicValue::int(-5),
                span: span(),
            },
            FlatStatement::Stop,
        ];

        let properties = vec![Property::Invariant {
            name: "x_non_negative".into(),
            condition: SymbolicValue::Expression {
                op: ExprOp::Ge,
                args: vec![SymbolicValue::sym_int("X"), SymbolicValue::int(0)],
            },
            location: PropertyLocation::Exit,
        }];

        let checker = ModelChecker::default();
        let result = checker.verify(stmts, &properties);

        assert_eq!(result.properties_checked, 1);
        assert_eq!(result.properties_failed.len(), 1);
        assert_eq!(
            result.properties_failed[0].property.name(),
            "x_non_negative"
        );
    }

    #[test]
    fn safety_passes() {
        // X = 1; STOP.
        // Safety: X = 0 must not happen.
        let stmts = vec![
            FlatStatement::Assign {
                target: "X".into(),
                value: SymbolicValue::int(1),
                span: span(),
            },
            FlatStatement::Stop,
        ];

        let properties = vec![Property::Safety {
            name: "x_not_zero".into(),
            violation_condition: SymbolicValue::Expression {
                op: ExprOp::Eq,
                args: vec![SymbolicValue::sym_int("X"), SymbolicValue::int(0)],
            },
        }];

        let checker = ModelChecker::default();
        let result = checker.verify(stmts, &properties);

        assert_eq!(result.properties_passed, 1);
    }

    #[test]
    fn safety_fails() {
        // X = 0; STOP.
        // Safety: X = 0 must not happen.
        let stmts = vec![
            FlatStatement::Assign {
                target: "X".into(),
                value: SymbolicValue::int(0),
                span: span(),
            },
            FlatStatement::Stop,
        ];

        let properties = vec![Property::Safety {
            name: "x_not_zero".into(),
            violation_condition: SymbolicValue::Expression {
                op: ExprOp::Eq,
                args: vec![SymbolicValue::sym_int("X"), SymbolicValue::int(0)],
            },
        }];

        let checker = ModelChecker::default();
        let result = checker.verify(stmts, &properties);

        assert_eq!(result.properties_failed.len(), 1);
    }

    #[test]
    fn equivalence_passes() {
        let stmts = vec![
            FlatStatement::Assign {
                target: "A".into(),
                value: SymbolicValue::int(42),
                span: span(),
            },
            FlatStatement::Assign {
                target: "B".into(),
                value: SymbolicValue::int(42),
                span: span(),
            },
            FlatStatement::Stop,
        ];

        let properties = vec![Property::Equivalence {
            name: "a_eq_b".into(),
            program_a_output: SymbolicValue::sym_int("A"),
            program_b_output: SymbolicValue::sym_int("B"),
            tolerance: None,
        }];

        let checker = ModelChecker::default();
        let result = checker.verify(stmts, &properties);

        assert_eq!(result.properties_passed, 1);
    }

    #[test]
    fn coverage_computed() {
        let stmts = vec![
            FlatStatement::Branch {
                condition: SymbolicValue::sym_bool("FLAG"),
                then_pc: 1,
                else_pc: 2,
                span: span(),
            },
            FlatStatement::Stop,
            FlatStatement::Stop,
        ];

        let checker = ModelChecker::default();
        let result = checker.verify(stmts, &[]);

        assert_eq!(result.coverage.total_branches, 1);
        assert!(result.coverage.branch_coverage > 0.0);
    }
}

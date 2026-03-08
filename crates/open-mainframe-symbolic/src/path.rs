//! Path conditions accumulated during symbolic execution.
//!
//! A [`PathCondition`] is an ordered conjunction of [`Constraint`]s that
//! together describe the conditions under which a particular execution
//! path is taken.  At every branch point the engine *forks* the path
//! condition into two children — one for the "then" arm and one for the
//! "else" arm — using [`PathCondition::fork`].

use open_mainframe_lang_core::Span;

use crate::value::{ExprOp, SymbolicValue};

/// A path condition is a conjunction of constraints gathered along one
/// execution path through the program.
#[derive(Debug, Clone)]
pub struct PathCondition {
    constraints: Vec<Constraint>,
    /// Cached feasibility flag.  Set to `false` when the solver proves
    /// the conjunction unsatisfiable, so we can short-circuit further
    /// exploration.
    pub is_feasible: bool,
}

/// A single constraint within a path condition.
#[derive(Debug, Clone)]
pub struct Constraint {
    /// The boolean expression that must hold.
    pub condition: SymbolicValue,
    /// Source location that introduced this constraint (e.g. an IF statement).
    pub source: Span,
    /// Optional human-readable description.
    pub description: Option<String>,
}

/// Records a branch decision made during symbolic execution.
#[derive(Debug, Clone)]
pub struct BranchDecision {
    /// The branch condition as a symbolic boolean.
    pub condition: SymbolicValue,
    /// Whether the "then" (`true`) or "else" (`false`) arm was taken.
    pub taken: bool,
    /// Source location of the branch.
    pub source: Span,
    /// Path condition *before* the branch.
    pub path_condition_before: PathCondition,
    /// Path condition *after* the branch (includes the new constraint).
    pub path_condition_after: PathCondition,
}

// ---------------------------------------------------------------------------
// Default
// ---------------------------------------------------------------------------

impl Default for PathCondition {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// Construction & manipulation
// ---------------------------------------------------------------------------

impl PathCondition {
    /// Create an empty (unconstrained) path condition.
    pub fn new() -> Self {
        Self {
            constraints: Vec::new(),
            is_feasible: true,
        }
    }

    /// Append a constraint.
    pub fn add(&mut self, constraint: Constraint) {
        self.constraints.push(constraint);
    }

    /// Fork this path condition for a branch.
    ///
    /// If `taken` is `true` the condition itself is added; if `false` its
    /// negation is added.
    pub fn fork(&self, condition: SymbolicValue, taken: bool, source: Span) -> PathCondition {
        let mut forked = self.clone();
        let branch_cond = if taken { condition } else { negate(condition) };
        forked.add(Constraint {
            condition: branch_cond,
            source,
            description: None,
        });
        forked
    }

    /// Merge two path conditions (conservative union, useful at join points).
    pub fn merge(&self, other: &PathCondition) -> PathCondition {
        let mut merged = self.clone();
        for constraint in &other.constraints {
            if !merged
                .constraints
                .iter()
                .any(|c| c.condition == constraint.condition)
            {
                merged.constraints.push(constraint.clone());
            }
        }
        merged.is_feasible = self.is_feasible && other.is_feasible;
        merged
    }

    /// Borrow the constraint list.
    pub fn constraints(&self) -> &[Constraint] {
        &self.constraints
    }

    /// Number of constraints.
    pub fn len(&self) -> usize {
        self.constraints.len()
    }

    /// Whether the constraint list is empty.
    pub fn is_empty(&self) -> bool {
        self.constraints.is_empty()
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Negate a symbolic boolean expression.
pub(crate) fn negate(condition: SymbolicValue) -> SymbolicValue {
    match condition {
        SymbolicValue::ConcreteBool(b) => SymbolicValue::ConcreteBool(!b),
        other => SymbolicValue::Expression {
            op: ExprOp::Not,
            args: vec![other],
        },
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn dummy_span() -> Span {
        Span::default()
    }

    #[test]
    fn new_path_is_empty_and_feasible() {
        let pc = PathCondition::new();
        assert!(pc.is_empty());
        assert!(pc.is_feasible);
    }

    #[test]
    fn add_constraint() {
        let mut pc = PathCondition::new();
        pc.add(Constraint {
            condition: SymbolicValue::bool(true),
            source: dummy_span(),
            description: Some("always true".into()),
        });
        assert_eq!(pc.len(), 1);
    }

    #[test]
    fn fork_then_branch() {
        let pc = PathCondition::new();
        let cond = SymbolicValue::sym_bool("X_POSITIVE");
        let then_pc = pc.fork(cond.clone(), true, dummy_span());
        assert_eq!(then_pc.len(), 1);
        // The constraint should be the condition itself.
        assert_eq!(then_pc.constraints()[0].condition, cond);
    }

    #[test]
    fn fork_else_branch() {
        let pc = PathCondition::new();
        let cond = SymbolicValue::sym_bool("X_POSITIVE");
        let else_pc = pc.fork(cond.clone(), false, dummy_span());
        assert_eq!(else_pc.len(), 1);
        // The constraint should be NOT(condition).
        assert!(matches!(
            &else_pc.constraints()[0].condition,
            SymbolicValue::Expression {
                op: ExprOp::Not,
                ..
            }
        ));
    }

    #[test]
    fn negate_concrete_bool() {
        assert_eq!(
            negate(SymbolicValue::bool(true)),
            SymbolicValue::bool(false)
        );
        assert_eq!(
            negate(SymbolicValue::bool(false)),
            SymbolicValue::bool(true)
        );
    }

    #[test]
    fn merge_deduplicates() {
        let mut a = PathCondition::new();
        let mut b = PathCondition::new();

        let c1 = SymbolicValue::sym_bool("A");
        let c2 = SymbolicValue::sym_bool("B");

        a.add(Constraint {
            condition: c1.clone(),
            source: dummy_span(),
            description: None,
        });
        b.add(Constraint {
            condition: c1,
            source: dummy_span(),
            description: None,
        });
        b.add(Constraint {
            condition: c2,
            source: dummy_span(),
            description: None,
        });

        let merged = a.merge(&b);
        assert_eq!(merged.len(), 2); // c1 only once + c2
    }
}

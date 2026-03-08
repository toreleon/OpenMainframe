//! Z3-backed constraint solver.
//!
//! [`SymbolicSolver`] wraps a Z3 context and solver, providing methods to
//! check satisfiability of [`PathCondition`]s, extract models (concrete
//! assignments), and convert [`SymbolicValue`] expression trees into the
//! corresponding Z3 AST.

use std::collections::HashMap;

use z3::ast::{Ast, Bool, Int, BV};
use z3::{Config, Context, Model, SatResult, Solver};

use crate::path::PathCondition;
use crate::sort::Sort;
use crate::value::{ExprOp, SymbolicValue};

/// Result of a satisfiability check, enriched with extracted concrete values.
#[derive(Debug)]
pub enum CheckResult {
    /// The constraints are satisfiable.  `assignments` maps symbolic
    /// variable names to the concrete values the solver found.
    Sat {
        assignments: HashMap<String, SymbolicValue>,
    },
    /// The constraints are unsatisfiable (no valid assignment exists).
    Unsat,
    /// The solver returned "unknown" (timeout, resource limit, etc.).
    Unknown,
}

/// Wrapper around a Z3 context + solver that translates our symbolic domain
/// into Z3 expressions and back.
pub struct SymbolicSolver {
    ctx: Context,
}

impl SymbolicSolver {
    /// Create a new solver with default Z3 configuration.
    pub fn new() -> Self {
        let mut cfg = Config::new();
        cfg.set_timeout_msec(10_000); // 10 s per query
        let ctx = Context::new(&cfg);
        Self { ctx }
    }

    /// Create a solver with a custom timeout (in milliseconds).
    pub fn with_timeout(timeout_ms: u64) -> Self {
        let mut cfg = Config::new();
        cfg.set_timeout_msec(timeout_ms);
        let ctx = Context::new(&cfg);
        Self { ctx }
    }

    /// Check satisfiability of a path condition.
    pub fn check_sat(&self, path: &PathCondition) -> SatResult {
        let solver = Solver::new(&self.ctx);
        for constraint in path.constraints() {
            let z3_expr = self.value_to_z3_bool(&constraint.condition);
            solver.assert(&z3_expr);
        }
        solver.check()
    }

    /// Check satisfiability and, if SAT, return concrete assignments.
    pub fn check(&self, path: &PathCondition) -> CheckResult {
        let solver = Solver::new(&self.ctx);
        for constraint in path.constraints() {
            let z3_expr = self.value_to_z3_bool(&constraint.condition);
            solver.assert(&z3_expr);
        }

        match solver.check() {
            SatResult::Sat => {
                let assignments = solver
                    .get_model()
                    .map(|m| self.extract_assignments(&m, path))
                    .unwrap_or_default();
                CheckResult::Sat { assignments }
            }
            SatResult::Unsat => CheckResult::Unsat,
            SatResult::Unknown => CheckResult::Unknown,
        }
    }

    /// Get the raw Z3 model for a satisfiable path condition.
    pub fn get_model(&self, path: &PathCondition) -> Option<Model<'_>> {
        let solver = Solver::new(&self.ctx);
        for constraint in path.constraints() {
            let z3_expr = self.value_to_z3_bool(&constraint.condition);
            solver.assert(&z3_expr);
        }
        if solver.check() == SatResult::Sat {
            solver.get_model()
        } else {
            None
        }
    }

    /// Check whether adding `extra` to `path` is still satisfiable.
    pub fn is_feasible(&self, path: &PathCondition, extra: &SymbolicValue) -> bool {
        let solver = Solver::new(&self.ctx);
        for constraint in path.constraints() {
            let z3_expr = self.value_to_z3_bool(&constraint.condition);
            solver.assert(&z3_expr);
        }
        let extra_expr = self.value_to_z3_bool(extra);
        solver.assert(&extra_expr);
        solver.check() == SatResult::Sat
    }

    // -----------------------------------------------------------------------
    // Conversion: SymbolicValue → Z3 AST
    // -----------------------------------------------------------------------

    /// Convert a symbolic value expected to be boolean into a Z3 `Bool`.
    fn value_to_z3_bool<'a>(&'a self, value: &SymbolicValue) -> Bool<'a> {
        match value {
            SymbolicValue::ConcreteBool(b) => Bool::from_bool(&self.ctx, *b),

            SymbolicValue::Symbolic { name, sort } => match sort {
                Sort::Bool => Bool::new_const(&self.ctx, name.as_str()),
                _ => {
                    // A non-bool symbolic used in boolean context: treat as != 0.
                    let int_val = self.value_to_z3_int(value);
                    let zero = Int::from_i64(&self.ctx, 0);
                    int_val._eq(&zero).not()
                }
            },

            SymbolicValue::Expression { op, args } => self.expr_to_z3_bool(op, args),

            // Concrete ints in boolean position: non-zero is true.
            SymbolicValue::Concrete(n) => Bool::from_bool(&self.ctx, *n != 0),

            // Fallback: unconstrained.
            _ => Bool::from_bool(&self.ctx, true),
        }
    }

    /// Convert a symbolic value into a Z3 `Int`.
    fn value_to_z3_int<'a>(&'a self, value: &SymbolicValue) -> Int<'a> {
        match value {
            SymbolicValue::Concrete(n) => Int::from_i64(&self.ctx, *n),

            SymbolicValue::Symbolic { name, sort } => match sort {
                Sort::Int => Int::new_const(&self.ctx, name.as_str()),
                Sort::BitVec(w) => {
                    // Promote BV to Int for arithmetic.
                    let bv = BV::new_const(&self.ctx, name.as_str(), *w);
                    bv.to_int(true) // signed
                }
                _ => Int::new_const(&self.ctx, name.as_str()),
            },

            SymbolicValue::Expression { op, args } => self.expr_to_z3_int(op, args),

            SymbolicValue::ConcreteBool(b) => Int::from_i64(&self.ctx, if *b { 1 } else { 0 }),

            _ => Int::fresh_const(&self.ctx, "unknown"),
        }
    }

    /// Translate a boolean expression to Z3.
    fn expr_to_z3_bool<'a>(&'a self, op: &ExprOp, args: &[SymbolicValue]) -> Bool<'a> {
        match op {
            // -- Comparison operators (produce Bool from Int operands) --
            ExprOp::Eq => {
                let lhs = self.value_to_z3_int(&args[0]);
                let rhs = self.value_to_z3_int(&args[1]);
                lhs._eq(&rhs)
            }
            ExprOp::Ne => {
                let lhs = self.value_to_z3_int(&args[0]);
                let rhs = self.value_to_z3_int(&args[1]);
                lhs._eq(&rhs).not()
            }
            ExprOp::Lt => {
                let lhs = self.value_to_z3_int(&args[0]);
                let rhs = self.value_to_z3_int(&args[1]);
                lhs.lt(&rhs)
            }
            ExprOp::Le => {
                let lhs = self.value_to_z3_int(&args[0]);
                let rhs = self.value_to_z3_int(&args[1]);
                lhs.le(&rhs)
            }
            ExprOp::Gt => {
                let lhs = self.value_to_z3_int(&args[0]);
                let rhs = self.value_to_z3_int(&args[1]);
                lhs.gt(&rhs)
            }
            ExprOp::Ge => {
                let lhs = self.value_to_z3_int(&args[0]);
                let rhs = self.value_to_z3_int(&args[1]);
                lhs.ge(&rhs)
            }

            // -- Boolean operators --
            ExprOp::And => {
                let lhs = self.value_to_z3_bool(&args[0]);
                let rhs = self.value_to_z3_bool(&args[1]);
                Bool::and(&self.ctx, &[&lhs, &rhs])
            }
            ExprOp::Or => {
                let lhs = self.value_to_z3_bool(&args[0]);
                let rhs = self.value_to_z3_bool(&args[1]);
                Bool::or(&self.ctx, &[&lhs, &rhs])
            }
            ExprOp::Not => {
                let inner = self.value_to_z3_bool(&args[0]);
                inner.not()
            }

            // Arithmetic ops in boolean context: result != 0.
            _ => {
                let int_val = self.expr_to_z3_int(op, args);
                let zero = Int::from_i64(&self.ctx, 0);
                int_val._eq(&zero).not()
            }
        }
    }

    /// Translate an arithmetic expression to Z3.
    fn expr_to_z3_int<'a>(&'a self, op: &ExprOp, args: &[SymbolicValue]) -> Int<'a> {
        match op {
            ExprOp::Add => {
                let lhs = self.value_to_z3_int(&args[0]);
                let rhs = self.value_to_z3_int(&args[1]);
                Int::add(&self.ctx, &[&lhs, &rhs])
            }
            ExprOp::Sub => {
                let lhs = self.value_to_z3_int(&args[0]);
                let rhs = self.value_to_z3_int(&args[1]);
                Int::sub(&self.ctx, &[&lhs, &rhs])
            }
            ExprOp::Mul => {
                let lhs = self.value_to_z3_int(&args[0]);
                let rhs = self.value_to_z3_int(&args[1]);
                Int::mul(&self.ctx, &[&lhs, &rhs])
            }
            ExprOp::Div => {
                let lhs = self.value_to_z3_int(&args[0]);
                let rhs = self.value_to_z3_int(&args[1]);
                lhs.div(&rhs)
            }
            ExprOp::Mod => {
                let lhs = self.value_to_z3_int(&args[0]);
                let rhs = self.value_to_z3_int(&args[1]);
                lhs.modulo(&rhs)
            }
            ExprOp::Neg => {
                let inner = self.value_to_z3_int(&args[0]);
                inner.unary_minus()
            }
            // Boolean ops coerced to int: true → 1, false → 0.
            _ => Int::fresh_const(&self.ctx, "expr"),
        }
    }

    // -----------------------------------------------------------------------
    // Model extraction
    // -----------------------------------------------------------------------

    /// Walk the path condition, collecting all symbolic variable names,
    /// then read their values from the Z3 model.
    fn extract_assignments(
        &self,
        model: &Model<'_>,
        path: &PathCondition,
    ) -> HashMap<String, SymbolicValue> {
        let mut names: HashMap<String, Sort> = HashMap::new();
        for constraint in path.constraints() {
            collect_symbolic_names(&constraint.condition, &mut names);
        }

        let mut assignments = HashMap::new();
        for (name, sort) in &names {
            match sort {
                Sort::Int => {
                    let z3_var = Int::new_const(&self.ctx, name.as_str());
                    if let Some(val) = model.eval(&z3_var, true) {
                        if let Some(n) = val.as_i64() {
                            assignments.insert(name.clone(), SymbolicValue::Concrete(n));
                        }
                    }
                }
                Sort::Bool => {
                    let z3_var = Bool::new_const(&self.ctx, name.as_str());
                    if let Some(val) = model.eval(&z3_var, true) {
                        if let Some(b) = val.as_bool() {
                            assignments.insert(name.clone(), SymbolicValue::ConcreteBool(b));
                        }
                    }
                }
                Sort::BitVec(w) => {
                    let z3_var = BV::new_const(&self.ctx, name.as_str(), *w);
                    if let Some(val) = model.eval(&z3_var, true) {
                        if let Some(n) = val.as_i64() {
                            assignments.insert(name.clone(), SymbolicValue::Concrete(n));
                        }
                    }
                }
                Sort::Array { .. } => {
                    // Array models are complex; skip for now.
                }
            }
        }
        assignments
    }
}

impl Default for SymbolicSolver {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Recursively collect all `Symbolic` variable names and sorts from an expression.
fn collect_symbolic_names(value: &SymbolicValue, out: &mut HashMap<String, Sort>) {
    match value {
        SymbolicValue::Symbolic { name, sort } => {
            out.entry(name.clone()).or_insert_with(|| sort.clone());
        }
        SymbolicValue::Expression { args, .. } => {
            for arg in args {
                collect_symbolic_names(arg, out);
            }
        }
        _ => {}
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::path::Constraint;
    use open_mainframe_lang_core::Span;

    fn dummy_span() -> Span {
        Span::default()
    }

    #[test]
    fn trivially_sat() {
        let solver = SymbolicSolver::new();
        let pc = PathCondition::new(); // empty = trivially satisfiable
        assert_eq!(solver.check_sat(&pc), SatResult::Sat);
    }

    #[test]
    fn trivially_unsat() {
        let solver = SymbolicSolver::new();
        let mut pc = PathCondition::new();
        // X > 0 AND X < 0
        let x = SymbolicValue::sym_int("X");
        let zero = SymbolicValue::int(0);
        pc.add(Constraint {
            condition: x.clone().gt(zero.clone()),
            source: dummy_span(),
            description: None,
        });
        pc.add(Constraint {
            condition: x.lt(zero),
            source: dummy_span(),
            description: None,
        });
        assert_eq!(solver.check_sat(&pc), SatResult::Unsat);
    }

    #[test]
    fn sat_with_model() {
        let solver = SymbolicSolver::new();
        let mut pc = PathCondition::new();
        // X > 5
        let x = SymbolicValue::sym_int("X");
        let five = SymbolicValue::int(5);
        pc.add(Constraint {
            condition: x.gt(five),
            source: dummy_span(),
            description: None,
        });

        match solver.check(&pc) {
            CheckResult::Sat { assignments } => {
                let x_val = assignments.get("X").expect("X should be in model");
                let n = x_val.as_concrete_int().expect("X should be concrete");
                assert!(n > 5, "X must be > 5, got {n}");
            }
            other => panic!("Expected Sat, got {other:?}"),
        }
    }

    #[test]
    fn feasibility_check() {
        let solver = SymbolicSolver::new();
        let mut pc = PathCondition::new();
        let x = SymbolicValue::sym_int("X");
        let ten = SymbolicValue::int(10);
        pc.add(Constraint {
            condition: x.clone().gt(ten),
            source: dummy_span(),
            description: None,
        });

        // X > 10 AND X > 5 is feasible.
        let five = SymbolicValue::int(5);
        assert!(solver.is_feasible(&pc, &x.clone().gt(five)));

        // X > 10 AND X < 0 is not feasible.
        let zero = SymbolicValue::int(0);
        assert!(!solver.is_feasible(&pc, &x.lt(zero)));
    }

    #[test]
    fn boolean_constraint() {
        let solver = SymbolicSolver::new();
        let mut pc = PathCondition::new();
        let flag = SymbolicValue::sym_bool("FLAG");
        pc.add(Constraint {
            condition: flag,
            source: dummy_span(),
            description: None,
        });

        match solver.check(&pc) {
            CheckResult::Sat { assignments } => {
                let val = assignments.get("FLAG").expect("FLAG should be in model");
                assert_eq!(val, &SymbolicValue::ConcreteBool(true));
            }
            other => panic!("Expected Sat, got {other:?}"),
        }
    }
}

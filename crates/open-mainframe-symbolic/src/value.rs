//! Symbolic values for the execution engine.
//!
//! A [`SymbolicValue`] is the fundamental unit of the symbolic domain.
//! It can be a concrete literal, a named symbolic variable, or a
//! compound expression tree built from [`ExprOp`] operators.

use std::fmt;

use serde::{Deserialize, Serialize};

use crate::sort::Sort;

/// A symbolic value that may be concrete or abstract.
///
/// Concrete values are evaluated eagerly (e.g. `Concrete(42)`).
/// Symbolic variables and expressions are carried through execution
/// and eventually fed to the constraint solver for satisfiability checking.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum SymbolicValue {
    /// Concrete integer value.
    Concrete(i64),

    /// Concrete boolean value.
    ConcreteBool(bool),

    /// Concrete string value.
    ConcreteStr(String),

    /// A named symbolic variable (e.g. an unconstrained input parameter).
    Symbolic { name: String, sort: Sort },

    /// A compound expression built from an operator and operands.
    Expression {
        op: ExprOp,
        args: Vec<SymbolicValue>,
    },

    /// Unknown / uninitialized — represents a variable that has not been set.
    Unknown,
}

/// Operators used in symbolic [`SymbolicValue::Expression`] nodes.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ExprOp {
    // -- Arithmetic --
    /// Addition (`a + b`).
    Add,
    /// Subtraction (`a - b`).
    Sub,
    /// Multiplication (`a * b`).
    Mul,
    /// Integer division (`a / b`).
    Div,
    /// Modulo (`a MOD b`).
    Mod,
    /// Unary negation (`-a`).
    Neg,

    // -- Comparison --
    /// Equal (`a = b`).
    Eq,
    /// Not equal (`a <> b`).
    Ne,
    /// Less than (`a < b`).
    Lt,
    /// Less than or equal (`a <= b`).
    Le,
    /// Greater than (`a > b`).
    Gt,
    /// Greater than or equal (`a >= b`).
    Ge,

    // -- Boolean --
    /// Logical AND.
    And,
    /// Logical OR.
    Or,
    /// Logical NOT (unary).
    Not,

    // -- String --
    /// String concatenation.
    Concat,
    /// Substring extraction (`SUBSTRING(str, pos, len)`).
    Substring,

    // -- Array --
    /// Array read: `Select(array, index)`.
    Select,
    /// Array write: `Store(array, index, value)`.
    Store,
}

// ---------------------------------------------------------------------------
// Constructors
// ---------------------------------------------------------------------------

impl SymbolicValue {
    /// Create a concrete integer value.
    pub fn int(v: i64) -> Self {
        SymbolicValue::Concrete(v)
    }

    /// Create a concrete boolean value.
    pub fn bool(v: bool) -> Self {
        SymbolicValue::ConcreteBool(v)
    }

    /// Create a concrete string value.
    pub fn str(v: impl Into<String>) -> Self {
        SymbolicValue::ConcreteStr(v.into())
    }

    /// Create a symbolic integer variable.
    pub fn sym_int(name: impl Into<String>) -> Self {
        SymbolicValue::Symbolic {
            name: name.into(),
            sort: Sort::Int,
        }
    }

    /// Create a symbolic boolean variable.
    pub fn sym_bool(name: impl Into<String>) -> Self {
        SymbolicValue::Symbolic {
            name: name.into(),
            sort: Sort::Bool,
        }
    }

    /// Create a symbolic bit-vector variable.
    pub fn sym_bitvec(name: impl Into<String>, width: u32) -> Self {
        SymbolicValue::Symbolic {
            name: name.into(),
            sort: Sort::bitvec(width),
        }
    }
}

// ---------------------------------------------------------------------------
// Predicates
// ---------------------------------------------------------------------------

impl SymbolicValue {
    /// Returns `true` if this value is fully concrete.
    pub fn is_concrete(&self) -> bool {
        matches!(
            self,
            SymbolicValue::Concrete(_)
                | SymbolicValue::ConcreteBool(_)
                | SymbolicValue::ConcreteStr(_)
        )
    }

    /// Returns the concrete integer if this is `Concrete(n)`.
    pub fn as_concrete_int(&self) -> Option<i64> {
        match self {
            SymbolicValue::Concrete(v) => Some(*v),
            _ => None,
        }
    }

    /// Returns the concrete boolean if this is `ConcreteBool(b)`.
    pub fn as_concrete_bool(&self) -> Option<bool> {
        match self {
            SymbolicValue::ConcreteBool(v) => Some(*v),
            _ => None,
        }
    }

    /// Returns the concrete string if this is `ConcreteStr(s)`.
    pub fn as_concrete_str(&self) -> Option<&str> {
        match self {
            SymbolicValue::ConcreteStr(s) => Some(s),
            _ => None,
        }
    }

    /// Returns `true` if this is `Unknown`.
    pub fn is_unknown(&self) -> bool {
        matches!(self, SymbolicValue::Unknown)
    }
}

// ---------------------------------------------------------------------------
// Arithmetic / logic builder helpers (constant-fold when possible)
// ---------------------------------------------------------------------------

#[allow(clippy::should_implement_trait)]
impl SymbolicValue {
    /// `self + other`, constant-folding when both sides are concrete.
    pub fn add(self, other: Self) -> Self {
        match (&self, &other) {
            (SymbolicValue::Concrete(a), SymbolicValue::Concrete(b)) => {
                SymbolicValue::Concrete(a.wrapping_add(*b))
            }
            _ => SymbolicValue::Expression {
                op: ExprOp::Add,
                args: vec![self, other],
            },
        }
    }

    /// `self - other`.
    pub fn sub(self, other: Self) -> Self {
        match (&self, &other) {
            (SymbolicValue::Concrete(a), SymbolicValue::Concrete(b)) => {
                SymbolicValue::Concrete(a.wrapping_sub(*b))
            }
            _ => SymbolicValue::Expression {
                op: ExprOp::Sub,
                args: vec![self, other],
            },
        }
    }

    /// `self * other`.
    pub fn mul(self, other: Self) -> Self {
        match (&self, &other) {
            (SymbolicValue::Concrete(a), SymbolicValue::Concrete(b)) => {
                SymbolicValue::Concrete(a.wrapping_mul(*b))
            }
            _ => SymbolicValue::Expression {
                op: ExprOp::Mul,
                args: vec![self, other],
            },
        }
    }

    /// `self / other` (integer division).
    pub fn div(self, other: Self) -> Self {
        match (&self, &other) {
            (SymbolicValue::Concrete(a), SymbolicValue::Concrete(b)) if *b != 0 => {
                SymbolicValue::Concrete(a / b)
            }
            _ => SymbolicValue::Expression {
                op: ExprOp::Div,
                args: vec![self, other],
            },
        }
    }

    /// `self == other` (yields a boolean symbolic value).
    pub fn eq(self, other: Self) -> Self {
        match (&self, &other) {
            (SymbolicValue::Concrete(a), SymbolicValue::Concrete(b)) => {
                SymbolicValue::ConcreteBool(a == b)
            }
            (SymbolicValue::ConcreteBool(a), SymbolicValue::ConcreteBool(b)) => {
                SymbolicValue::ConcreteBool(a == b)
            }
            _ => SymbolicValue::Expression {
                op: ExprOp::Eq,
                args: vec![self, other],
            },
        }
    }

    /// `self < other`.
    pub fn lt(self, other: Self) -> Self {
        match (&self, &other) {
            (SymbolicValue::Concrete(a), SymbolicValue::Concrete(b)) => {
                SymbolicValue::ConcreteBool(a < b)
            }
            _ => SymbolicValue::Expression {
                op: ExprOp::Lt,
                args: vec![self, other],
            },
        }
    }

    /// `self > other`.
    pub fn gt(self, other: Self) -> Self {
        match (&self, &other) {
            (SymbolicValue::Concrete(a), SymbolicValue::Concrete(b)) => {
                SymbolicValue::ConcreteBool(a > b)
            }
            _ => SymbolicValue::Expression {
                op: ExprOp::Gt,
                args: vec![self, other],
            },
        }
    }

    /// `self <= other`.
    pub fn le(self, other: Self) -> Self {
        match (&self, &other) {
            (SymbolicValue::Concrete(a), SymbolicValue::Concrete(b)) => {
                SymbolicValue::ConcreteBool(a <= b)
            }
            _ => SymbolicValue::Expression {
                op: ExprOp::Le,
                args: vec![self, other],
            },
        }
    }

    /// `self >= other`.
    pub fn ge(self, other: Self) -> Self {
        match (&self, &other) {
            (SymbolicValue::Concrete(a), SymbolicValue::Concrete(b)) => {
                SymbolicValue::ConcreteBool(a >= b)
            }
            _ => SymbolicValue::Expression {
                op: ExprOp::Ge,
                args: vec![self, other],
            },
        }
    }

    /// Logical NOT.
    pub fn not(self) -> Self {
        match self {
            SymbolicValue::ConcreteBool(b) => SymbolicValue::ConcreteBool(!b),
            other => SymbolicValue::Expression {
                op: ExprOp::Not,
                args: vec![other],
            },
        }
    }

    /// Logical AND.
    pub fn and(self, other: Self) -> Self {
        match (&self, &other) {
            (SymbolicValue::ConcreteBool(a), SymbolicValue::ConcreteBool(b)) => {
                SymbolicValue::ConcreteBool(*a && *b)
            }
            _ => SymbolicValue::Expression {
                op: ExprOp::And,
                args: vec![self, other],
            },
        }
    }

    /// Logical OR.
    pub fn or(self, other: Self) -> Self {
        match (&self, &other) {
            (SymbolicValue::ConcreteBool(a), SymbolicValue::ConcreteBool(b)) => {
                SymbolicValue::ConcreteBool(*a || *b)
            }
            _ => SymbolicValue::Expression {
                op: ExprOp::Or,
                args: vec![self, other],
            },
        }
    }
}

// ---------------------------------------------------------------------------
// Display
// ---------------------------------------------------------------------------

impl fmt::Display for SymbolicValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SymbolicValue::Concrete(n) => write!(f, "{n}"),
            SymbolicValue::ConcreteBool(b) => write!(f, "{b}"),
            SymbolicValue::ConcreteStr(s) => write!(f, "\"{s}\""),
            SymbolicValue::Symbolic { name, sort } => write!(f, "{name}:{sort}"),
            SymbolicValue::Expression { op, args } => {
                write!(f, "({op:?}")?;
                for arg in args {
                    write!(f, " {arg}")?;
                }
                write!(f, ")")
            }
            SymbolicValue::Unknown => write!(f, "?"),
        }
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn concrete_int_operations() {
        let a = SymbolicValue::int(10);
        let b = SymbolicValue::int(3);

        assert_eq!(a.clone().add(b.clone()), SymbolicValue::Concrete(13));
        assert_eq!(a.clone().sub(b.clone()), SymbolicValue::Concrete(7));
        assert_eq!(a.clone().mul(b.clone()), SymbolicValue::Concrete(30));
        assert_eq!(a.clone().div(b.clone()), SymbolicValue::Concrete(3));
    }

    #[test]
    fn symbolic_addition_produces_expression() {
        let x = SymbolicValue::sym_int("X");
        let y = SymbolicValue::sym_int("Y");
        let sum = x.add(y);

        assert!(matches!(
            sum,
            SymbolicValue::Expression {
                op: ExprOp::Add,
                ..
            }
        ));
    }

    #[test]
    fn concrete_comparison() {
        let a = SymbolicValue::int(5);
        let b = SymbolicValue::int(10);

        assert_eq!(a.clone().lt(b.clone()), SymbolicValue::ConcreteBool(true));
        assert_eq!(a.clone().gt(b.clone()), SymbolicValue::ConcreteBool(false));
        assert_eq!(a.clone().eq(b.clone()), SymbolicValue::ConcreteBool(false));
        assert_eq!(a.clone().eq(a.clone()), SymbolicValue::ConcreteBool(true));
    }

    #[test]
    fn boolean_logic() {
        let t = SymbolicValue::bool(true);
        let f = SymbolicValue::bool(false);

        assert_eq!(t.clone().and(f.clone()), SymbolicValue::ConcreteBool(false));
        assert_eq!(t.clone().or(f.clone()), SymbolicValue::ConcreteBool(true));
        assert_eq!(t.not(), SymbolicValue::ConcreteBool(false));
    }

    #[test]
    fn predicates() {
        assert!(SymbolicValue::int(1).is_concrete());
        assert!(SymbolicValue::bool(true).is_concrete());
        assert!(SymbolicValue::str("hi").is_concrete());
        assert!(!SymbolicValue::sym_int("X").is_concrete());
        assert!(SymbolicValue::Unknown.is_unknown());

        assert_eq!(SymbolicValue::int(42).as_concrete_int(), Some(42));
        assert_eq!(SymbolicValue::bool(true).as_concrete_bool(), Some(true));
        assert_eq!(SymbolicValue::str("hi").as_concrete_str(), Some("hi"));
    }

    #[test]
    fn display() {
        assert_eq!(SymbolicValue::int(42).to_string(), "42");
        assert_eq!(SymbolicValue::bool(true).to_string(), "true");
        assert_eq!(SymbolicValue::str("hi").to_string(), "\"hi\"");
        assert_eq!(SymbolicValue::sym_int("X").to_string(), "X:Int");
        assert_eq!(SymbolicValue::Unknown.to_string(), "?");
    }

    #[test]
    fn div_by_zero_stays_symbolic() {
        let a = SymbolicValue::int(10);
        let b = SymbolicValue::int(0);
        let result = a.div(b);
        assert!(matches!(
            result,
            SymbolicValue::Expression {
                op: ExprOp::Div,
                ..
            }
        ));
    }
}

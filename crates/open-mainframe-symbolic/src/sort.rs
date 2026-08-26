//! Sort (type) system for symbolic values.
//!
//! A [`Sort`] classifies every value in the symbolic constraint domain.
//! The type system covers integers, booleans, fixed-width bit-vectors,
//! and arrays (maps from one sort to another), which together suffice to
//! model the COBOL data items encountered during symbolic execution.

use std::fmt;

use serde::{Deserialize, Serialize};

/// A sort (type) in the symbolic domain.
///
/// Each variant describes the intended semantics of a symbolic value.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Sort {
    /// Signed integer.
    Int,

    /// Boolean.
    Bool,

    /// Fixed-width bit-vector of the given width in bits.
    ///
    /// COBOL `PIC 9(n) COMP` items are modelled as `BitVec(n * 4)` or
    /// an appropriate power-of-two width.
    BitVec(u32),

    /// Array mapping keys of one sort to values of another.
    ///
    /// Useful for modelling COBOL tables / OCCURS arrays symbolically.
    Array {
        /// Sort of the index (key).
        index: Box<Sort>,
        /// Sort of each element (value).
        element: Box<Sort>,
    },
}

impl Sort {
    /// Convenience constructor for `BitVec` sorts.
    pub fn bitvec(width: u32) -> Self {
        assert!(width > 0, "BitVec width must be > 0");
        Sort::BitVec(width)
    }

    /// Convenience constructor for `Array` sorts.
    pub fn array(index: Sort, element: Sort) -> Self {
        Sort::Array {
            index: Box::new(index),
            element: Box::new(element),
        }
    }

    /// Returns `true` if this sort is numeric (Int or BitVec).
    pub fn is_numeric(&self) -> bool {
        matches!(self, Sort::Int | Sort::BitVec(_))
    }

    /// Returns `true` if this sort is a boolean.
    pub fn is_bool(&self) -> bool {
        matches!(self, Sort::Bool)
    }

    /// Returns `true` if this sort is an array.
    pub fn is_array(&self) -> bool {
        matches!(self, Sort::Array { .. })
    }

    /// For `BitVec`, returns the width in bits. Returns `None` for other sorts.
    pub fn bitvec_width(&self) -> Option<u32> {
        match self {
            Sort::BitVec(w) => Some(*w),
            _ => None,
        }
    }
}

impl fmt::Display for Sort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Sort::Int => write!(f, "Int"),
            Sort::Bool => write!(f, "Bool"),
            Sort::BitVec(w) => write!(f, "BitVec({w})"),
            Sort::Array { index, element } => {
                write!(f, "Array({index}, {element})")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sort_equality() {
        assert_eq!(Sort::Int, Sort::Int);
        assert_eq!(Sort::Bool, Sort::Bool);
        assert_eq!(Sort::BitVec(32), Sort::BitVec(32));
        assert_ne!(Sort::BitVec(32), Sort::BitVec(64));
        assert_ne!(Sort::Int, Sort::Bool);
    }

    #[test]
    fn sort_display() {
        assert_eq!(Sort::Int.to_string(), "Int");
        assert_eq!(Sort::Bool.to_string(), "Bool");
        assert_eq!(Sort::bitvec(16).to_string(), "BitVec(16)");
        assert_eq!(
            Sort::array(Sort::Int, Sort::bitvec(8)).to_string(),
            "Array(Int, BitVec(8))"
        );
    }

    #[test]
    fn sort_predicates() {
        assert!(Sort::Int.is_numeric());
        assert!(Sort::bitvec(32).is_numeric());
        assert!(!Sort::Bool.is_numeric());
        assert!(!Sort::array(Sort::Int, Sort::Int).is_numeric());

        assert!(Sort::Bool.is_bool());
        assert!(!Sort::Int.is_bool());

        assert!(Sort::array(Sort::Int, Sort::Bool).is_array());
        assert!(!Sort::Int.is_array());
    }

    #[test]
    fn bitvec_width() {
        assert_eq!(Sort::bitvec(64).bitvec_width(), Some(64));
        assert_eq!(Sort::Int.bitvec_width(), None);
    }

    #[test]
    #[should_panic(expected = "BitVec width must be > 0")]
    fn bitvec_zero_width_panics() {
        Sort::bitvec(0);
    }

    #[test]
    fn nested_array_sort() {
        let nested = Sort::array(Sort::Int, Sort::array(Sort::Int, Sort::bitvec(8)));
        assert!(nested.is_array());
        assert_eq!(nested.to_string(), "Array(Int, Array(Int, BitVec(8)))");
    }
}

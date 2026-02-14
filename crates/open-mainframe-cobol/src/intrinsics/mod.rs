//! COBOL Intrinsic Functions.
//!
//! Implements COBOL-2014 intrinsic functions for runtime evaluation.
//! These functions are used by the generated code at runtime.

mod string;
mod numeric;
mod datetime;

pub use string::*;
pub use numeric::*;
pub use datetime::*;

/// Function category for intrinsic functions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionCategory {
    /// String manipulation functions
    String,
    /// Numeric/mathematical functions
    Numeric,
    /// Date and time functions
    DateTime,
    /// General functions
    General,
}

/// Function result type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionResultType {
    /// Returns alphanumeric string
    Alphanumeric,
    /// Returns national string (UTF-16)
    National,
    /// Returns numeric value
    Numeric,
    /// Returns integer
    Integer,
}

/// Intrinsic function definition.
#[derive(Debug, Clone)]
pub struct IntrinsicFunction {
    /// Function name
    pub name: &'static str,
    /// Category
    pub category: FunctionCategory,
    /// Result type
    pub result_type: FunctionResultType,
    /// Minimum number of arguments
    pub min_args: usize,
    /// Maximum number of arguments (None = unlimited)
    pub max_args: Option<usize>,
    /// Description
    pub description: &'static str,
}

/// Registry of all intrinsic functions.
pub static INTRINSIC_FUNCTIONS: &[IntrinsicFunction] = &[
    // String functions
    IntrinsicFunction {
        name: "TRIM",
        category: FunctionCategory::String,
        result_type: FunctionResultType::Alphanumeric,
        min_args: 1,
        max_args: Some(2),
        description: "Remove leading and/or trailing spaces",
    },
    IntrinsicFunction {
        name: "SUBSTITUTE",
        category: FunctionCategory::String,
        result_type: FunctionResultType::Alphanumeric,
        min_args: 3,
        max_args: None,
        description: "Replace substrings",
    },
    IntrinsicFunction {
        name: "CONCATENATE",
        category: FunctionCategory::String,
        result_type: FunctionResultType::Alphanumeric,
        min_args: 1,
        max_args: None,
        description: "Join strings together",
    },
    IntrinsicFunction {
        name: "LENGTH",
        category: FunctionCategory::String,
        result_type: FunctionResultType::Integer,
        min_args: 1,
        max_args: Some(1),
        description: "Get string length",
    },
    IntrinsicFunction {
        name: "REVERSE",
        category: FunctionCategory::String,
        result_type: FunctionResultType::Alphanumeric,
        min_args: 1,
        max_args: Some(1),
        description: "Reverse string",
    },
    IntrinsicFunction {
        name: "UPPER-CASE",
        category: FunctionCategory::String,
        result_type: FunctionResultType::Alphanumeric,
        min_args: 1,
        max_args: Some(1),
        description: "Convert to uppercase",
    },
    IntrinsicFunction {
        name: "LOWER-CASE",
        category: FunctionCategory::String,
        result_type: FunctionResultType::Alphanumeric,
        min_args: 1,
        max_args: Some(1),
        description: "Convert to lowercase",
    },
    IntrinsicFunction {
        name: "DISPLAY-OF",
        category: FunctionCategory::String,
        result_type: FunctionResultType::Alphanumeric,
        min_args: 1,
        max_args: Some(2),
        description: "Convert national to alphanumeric (UTF-16 to UTF-8)",
    },
    IntrinsicFunction {
        name: "NATIONAL-OF",
        category: FunctionCategory::String,
        result_type: FunctionResultType::National,
        min_args: 1,
        max_args: Some(2),
        description: "Convert alphanumeric to national (UTF-8 to UTF-16)",
    },
    // Numeric functions
    IntrinsicFunction {
        name: "E",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 0,
        max_args: Some(0),
        description: "Euler's number (2.71828...)",
    },
    IntrinsicFunction {
        name: "PI",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 0,
        max_args: Some(0),
        description: "Pi (3.14159...)",
    },
    IntrinsicFunction {
        name: "EXP",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: Some(1),
        description: "e raised to power",
    },
    IntrinsicFunction {
        name: "EXP10",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: Some(1),
        description: "10 raised to power",
    },
    IntrinsicFunction {
        name: "LOG",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: Some(1),
        description: "Natural logarithm",
    },
    IntrinsicFunction {
        name: "LOG10",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: Some(1),
        description: "Base-10 logarithm",
    },
    IntrinsicFunction {
        name: "SQRT",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: Some(1),
        description: "Square root",
    },
    IntrinsicFunction {
        name: "ABS",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: Some(1),
        description: "Absolute value",
    },
    IntrinsicFunction {
        name: "SIN",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: Some(1),
        description: "Sine",
    },
    IntrinsicFunction {
        name: "COS",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: Some(1),
        description: "Cosine",
    },
    IntrinsicFunction {
        name: "TAN",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: Some(1),
        description: "Tangent",
    },
    IntrinsicFunction {
        name: "MIN",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: None,
        description: "Minimum value",
    },
    IntrinsicFunction {
        name: "MAX",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: None,
        description: "Maximum value",
    },
    IntrinsicFunction {
        name: "SUM",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: None,
        description: "Sum of values",
    },
    IntrinsicFunction {
        name: "MEAN",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: None,
        description: "Arithmetic mean",
    },
    IntrinsicFunction {
        name: "MEDIAN",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: None,
        description: "Median value",
    },
    IntrinsicFunction {
        name: "VARIANCE",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: None,
        description: "Variance",
    },
    IntrinsicFunction {
        name: "STANDARD-DEVIATION",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 1,
        max_args: None,
        description: "Standard deviation",
    },
    IntrinsicFunction {
        name: "MOD",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Integer,
        min_args: 2,
        max_args: Some(2),
        description: "Modulo operation",
    },
    IntrinsicFunction {
        name: "REM",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Numeric,
        min_args: 2,
        max_args: Some(2),
        description: "Remainder",
    },
    IntrinsicFunction {
        name: "INTEGER",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Integer,
        min_args: 1,
        max_args: Some(1),
        description: "Truncate to integer",
    },
    IntrinsicFunction {
        name: "INTEGER-PART",
        category: FunctionCategory::Numeric,
        result_type: FunctionResultType::Integer,
        min_args: 1,
        max_args: Some(1),
        description: "Integer part",
    },
    // DateTime functions
    IntrinsicFunction {
        name: "CURRENT-DATE",
        category: FunctionCategory::DateTime,
        result_type: FunctionResultType::Alphanumeric,
        min_args: 0,
        max_args: Some(0),
        description: "Current date and time (21 chars)",
    },
    IntrinsicFunction {
        name: "WHEN-COMPILED",
        category: FunctionCategory::DateTime,
        result_type: FunctionResultType::Alphanumeric,
        min_args: 0,
        max_args: Some(0),
        description: "Compilation date and time",
    },
    IntrinsicFunction {
        name: "DATE-OF-INTEGER",
        category: FunctionCategory::DateTime,
        result_type: FunctionResultType::Integer,
        min_args: 1,
        max_args: Some(1),
        description: "Convert integer date to YYYYMMDD",
    },
    IntrinsicFunction {
        name: "INTEGER-OF-DATE",
        category: FunctionCategory::DateTime,
        result_type: FunctionResultType::Integer,
        min_args: 1,
        max_args: Some(1),
        description: "Convert YYYYMMDD to integer date",
    },
    IntrinsicFunction {
        name: "DAY-OF-INTEGER",
        category: FunctionCategory::DateTime,
        result_type: FunctionResultType::Integer,
        min_args: 1,
        max_args: Some(1),
        description: "Day number from integer date",
    },
    IntrinsicFunction {
        name: "INTEGER-OF-DAY",
        category: FunctionCategory::DateTime,
        result_type: FunctionResultType::Integer,
        min_args: 1,
        max_args: Some(1),
        description: "Integer date from YYYYDDD",
    },
];

/// Look up an intrinsic function by name.
pub fn lookup_function(name: &str) -> Option<&'static IntrinsicFunction> {
    let upper = name.to_uppercase();
    INTRINSIC_FUNCTIONS.iter().find(|f| f.name == upper)
}

/// Check if a name is an intrinsic function.
pub fn is_intrinsic_function(name: &str) -> bool {
    lookup_function(name).is_some()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lookup_function() {
        assert!(lookup_function("TRIM").is_some());
        assert!(lookup_function("trim").is_some());
        assert!(lookup_function("CONCATENATE").is_some());
        assert!(lookup_function("UNKNOWN").is_none());
    }

    #[test]
    fn test_function_registry() {
        let trim = lookup_function("TRIM").unwrap();
        assert_eq!(trim.name, "TRIM");
        assert_eq!(trim.category, FunctionCategory::String);
        assert_eq!(trim.min_args, 1);
        assert_eq!(trim.max_args, Some(2));
    }

    #[test]
    fn test_numeric_functions() {
        assert!(lookup_function("SIN").is_some());
        assert!(lookup_function("COS").is_some());
        assert!(lookup_function("LOG10").is_some());
    }
}

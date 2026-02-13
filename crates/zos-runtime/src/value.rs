//! COBOL data value representation.
//!
//! This module provides runtime representation of COBOL data items,
//! supporting both numeric and alphanumeric types with various
//! storage formats (DISPLAY, BINARY, PACKED-DECIMAL).

use rust_decimal::Decimal;
use std::fmt;

/// A runtime COBOL value.
#[derive(Debug, Clone, PartialEq)]
pub enum CobolValue {
    /// Alphanumeric value (string).
    Alphanumeric(String),
    /// Numeric value with decimal precision.
    Numeric(NumericValue),
    /// Group item (collection of subordinate values).
    Group(Vec<u8>),
}

impl CobolValue {
    /// Create an alphanumeric value from a string.
    pub fn alphanumeric(s: impl Into<String>) -> Self {
        CobolValue::Alphanumeric(s.into())
    }

    /// Create a numeric value from a Decimal.
    pub fn numeric(value: Decimal, decimal_places: u8, is_signed: bool) -> Self {
        CobolValue::Numeric(NumericValue {
            value,
            decimal_places,
            is_signed,
        })
    }

    /// Create a numeric value from an integer.
    pub fn from_i64(value: i64) -> Self {
        CobolValue::Numeric(NumericValue {
            value: Decimal::from(value),
            decimal_places: 0,
            is_signed: true,
        })
    }

    /// Create a group value from raw bytes.
    pub fn group(bytes: Vec<u8>) -> Self {
        CobolValue::Group(bytes)
    }

    /// Get the value as a string for display purposes.
    pub fn to_display_string(&self) -> String {
        match self {
            CobolValue::Alphanumeric(s) => s.clone(),
            CobolValue::Numeric(n) => n.to_display_string(),
            CobolValue::Group(bytes) => {
                // Display as ASCII (or replacement char for non-printable)
                bytes
                    .iter()
                    .map(|&b| {
                        if b.is_ascii_graphic() || b == b' ' {
                            b as char
                        } else {
                            '.'
                        }
                    })
                    .collect()
            }
        }
    }

    /// Get the numeric value if this is numeric.
    pub fn as_numeric(&self) -> Option<&NumericValue> {
        match self {
            CobolValue::Numeric(n) => Some(n),
            _ => None,
        }
    }

    /// Get the alphanumeric value if this is alphanumeric.
    pub fn as_alphanumeric(&self) -> Option<&str> {
        match self {
            CobolValue::Alphanumeric(s) => Some(s),
            _ => None,
        }
    }

    /// Get the raw bytes of this value.
    pub fn as_bytes(&self) -> Vec<u8> {
        match self {
            CobolValue::Alphanumeric(s) => s.as_bytes().to_vec(),
            CobolValue::Numeric(n) => n.to_display_string().into_bytes(),
            CobolValue::Group(bytes) => bytes.clone(),
        }
    }
}

impl fmt::Display for CobolValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_display_string())
    }
}

/// A numeric value with COBOL semantics.
#[derive(Debug, Clone, PartialEq)]
pub struct NumericValue {
    /// The decimal value.
    pub value: Decimal,
    /// Number of decimal places.
    pub decimal_places: u8,
    /// Whether the value is signed.
    pub is_signed: bool,
}

impl NumericValue {
    /// Create a new numeric value.
    pub fn new(value: Decimal, decimal_places: u8, is_signed: bool) -> Self {
        Self {
            value,
            decimal_places,
            is_signed,
        }
    }

    /// Create from an integer.
    pub fn from_i64(value: i64) -> Self {
        Self {
            value: Decimal::from(value),
            decimal_places: 0,
            is_signed: true,
        }
    }

    /// Get the integer portion of the value.
    pub fn integer_part(&self) -> i64 {
        self.value.trunc().to_string().parse().unwrap_or(0)
    }

    /// Get the formatted display string.
    pub fn to_display_string(&self) -> String {
        if self.decimal_places == 0 {
            format!("{}", self.value.trunc())
        } else {
            format!("{:.prec$}", self.value, prec = self.decimal_places as usize)
        }
    }

    /// Add two numeric values.
    pub fn add(&self, other: &NumericValue) -> NumericValue {
        let result = self.value + other.value;
        let decimal_places = self.decimal_places.max(other.decimal_places);
        NumericValue {
            value: result,
            decimal_places,
            is_signed: self.is_signed || other.is_signed,
        }
    }

    /// Subtract another numeric value.
    pub fn subtract(&self, other: &NumericValue) -> NumericValue {
        let result = self.value - other.value;
        let decimal_places = self.decimal_places.max(other.decimal_places);
        NumericValue {
            value: result,
            decimal_places,
            is_signed: true, // Result of subtraction can be negative
        }
    }

    /// Multiply by another numeric value.
    pub fn multiply(&self, other: &NumericValue) -> NumericValue {
        let result = self.value * other.value;
        let decimal_places = self.decimal_places + other.decimal_places;
        NumericValue {
            value: result,
            decimal_places,
            is_signed: self.is_signed || other.is_signed,
        }
    }

    /// Divide by another numeric value.
    pub fn divide(&self, other: &NumericValue) -> Option<NumericValue> {
        if other.value.is_zero() {
            return None;
        }
        let result = self.value / other.value;
        // For division, we keep a reasonable number of decimal places
        let decimal_places = 18; // Maximum IBM COBOL precision
        Some(NumericValue {
            value: result,
            decimal_places,
            is_signed: self.is_signed || other.is_signed,
        })
    }

    /// Round the value to specified decimal places.
    pub fn round(&self, decimal_places: u8) -> NumericValue {
        let scale = Decimal::from(10i64.pow(decimal_places as u32));
        let rounded = (self.value * scale).round() / scale;
        NumericValue {
            value: rounded,
            decimal_places,
            is_signed: self.is_signed,
        }
    }

    /// Truncate the value to specified decimal places.
    pub fn truncate(&self, decimal_places: u8) -> NumericValue {
        let scale = Decimal::from(10i64.pow(decimal_places as u32));
        let truncated = (self.value * scale).trunc() / scale;
        NumericValue {
            value: truncated,
            decimal_places,
            is_signed: self.is_signed,
        }
    }

    /// Check if the value is zero.
    pub fn is_zero(&self) -> bool {
        self.value.is_zero()
    }

    /// Check if the value is positive.
    pub fn is_positive(&self) -> bool {
        self.value.is_sign_positive() && !self.value.is_zero()
    }

    /// Check if the value is negative.
    pub fn is_negative(&self) -> bool {
        self.value.is_sign_negative()
    }

    /// Get the absolute value.
    pub fn abs(&self) -> NumericValue {
        NumericValue {
            value: self.value.abs(),
            decimal_places: self.decimal_places,
            is_signed: false,
        }
    }
}

impl Default for NumericValue {
    fn default() -> Self {
        Self {
            value: Decimal::ZERO,
            decimal_places: 0,
            is_signed: true,
        }
    }
}

impl fmt::Display for NumericValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_display_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_numeric_arithmetic() {
        let a = NumericValue::from_i64(100);
        let b = NumericValue::from_i64(50);

        let sum = a.add(&b);
        assert_eq!(sum.value, Decimal::from(150));

        let diff = a.subtract(&b);
        assert_eq!(diff.value, Decimal::from(50));

        let product = a.multiply(&b);
        assert_eq!(product.value, Decimal::from(5000));

        let quotient = a.divide(&b).unwrap();
        assert_eq!(quotient.value, Decimal::from(2));
    }

    #[test]
    fn test_division_by_zero() {
        let a = NumericValue::from_i64(100);
        let zero = NumericValue::from_i64(0);
        assert!(a.divide(&zero).is_none());
    }

    #[test]
    fn test_rounding() {
        let value = NumericValue::new(Decimal::from_str_exact("123.456").unwrap(), 3, true);
        let rounded = value.round(2);
        assert_eq!(rounded.to_display_string(), "123.46");
    }

    #[test]
    fn test_truncation() {
        let value = NumericValue::new(Decimal::from_str_exact("123.456").unwrap(), 3, true);
        let truncated = value.truncate(2);
        assert_eq!(truncated.to_display_string(), "123.45");
    }

    #[test]
    fn test_display_string() {
        let alpha = CobolValue::alphanumeric("HELLO");
        assert_eq!(alpha.to_display_string(), "HELLO");

        let num = CobolValue::from_i64(42);
        assert_eq!(num.to_display_string(), "42");
    }
}

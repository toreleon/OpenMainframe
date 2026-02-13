//! Decimal arithmetic with IBM COBOL compatibility.
//!
//! This module implements decimal arithmetic operations that match
//! IBM COBOL behavior, including proper rounding and truncation.

use crate::value::NumericValue;
use rust_decimal::Decimal;

/// Result of an arithmetic operation that may overflow.
#[derive(Debug)]
pub struct ArithmeticResult {
    /// The result value (if no overflow).
    pub value: Option<NumericValue>,
    /// Whether a size error (overflow) occurred.
    pub size_error: bool,
}

impl ArithmeticResult {
    /// Create a successful result.
    pub fn ok(value: NumericValue) -> Self {
        Self {
            value: Some(value),
            size_error: false,
        }
    }

    /// Create a size error result.
    pub fn size_error() -> Self {
        Self {
            value: None,
            size_error: true,
        }
    }
}

/// Rounding mode for arithmetic operations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum RoundingMode {
    /// Truncate toward zero.
    #[default]
    Truncate,
    /// Round to nearest, ties away from zero.
    Round,
}

/// Add two numeric values with size checking.
pub fn add(
    operands: &[NumericValue],
    target_size: u8,
    target_decimals: u8,
    rounding: RoundingMode,
) -> ArithmeticResult {
    let mut sum = NumericValue::from_i64(0);

    for operand in operands {
        sum = sum.add(operand);
    }

    apply_target_precision(sum, target_size, target_decimals, rounding)
}

/// Add values to a target (ADD ... TO).
pub fn add_to(
    operands: &[NumericValue],
    target: &NumericValue,
    target_size: u8,
    target_decimals: u8,
    rounding: RoundingMode,
) -> ArithmeticResult {
    let mut sum = target.clone();

    for operand in operands {
        sum = sum.add(operand);
    }

    apply_target_precision(sum, target_size, target_decimals, rounding)
}

/// Subtract values from a target (SUBTRACT ... FROM).
pub fn subtract_from(
    operands: &[NumericValue],
    target: &NumericValue,
    target_size: u8,
    target_decimals: u8,
    rounding: RoundingMode,
) -> ArithmeticResult {
    let mut result = target.clone();

    for operand in operands {
        result = result.subtract(operand);
    }

    apply_target_precision(result, target_size, target_decimals, rounding)
}

/// Multiply two values.
pub fn multiply(
    left: &NumericValue,
    right: &NumericValue,
    target_size: u8,
    target_decimals: u8,
    rounding: RoundingMode,
) -> ArithmeticResult {
    let result = left.multiply(right);
    apply_target_precision(result, target_size, target_decimals, rounding)
}

/// Divide with optional remainder.
pub fn divide(
    dividend: &NumericValue,
    divisor: &NumericValue,
    target_size: u8,
    target_decimals: u8,
    rounding: RoundingMode,
) -> (ArithmeticResult, Option<NumericValue>) {
    if divisor.is_zero() {
        return (ArithmeticResult::size_error(), None);
    }

    let quotient = dividend.divide(divisor);
    match quotient {
        Some(q) => {
            let remainder = compute_remainder(dividend, divisor, &q);
            (
                apply_target_precision(q, target_size, target_decimals, rounding),
                Some(remainder),
            )
        }
        None => (ArithmeticResult::size_error(), None),
    }
}

/// Compute remainder from division.
fn compute_remainder(
    dividend: &NumericValue,
    divisor: &NumericValue,
    quotient: &NumericValue,
) -> NumericValue {
    let product = quotient.truncate(0).multiply(divisor);
    dividend.subtract(&product)
}

/// Apply target precision with overflow checking.
fn apply_target_precision(
    value: NumericValue,
    target_size: u8,
    target_decimals: u8,
    rounding: RoundingMode,
) -> ArithmeticResult {
    // Apply rounding or truncation
    let adjusted = match rounding {
        RoundingMode::Truncate => value.truncate(target_decimals),
        RoundingMode::Round => value.round(target_decimals),
    };

    // Check for overflow
    let integer_digits = target_size.saturating_sub(target_decimals);
    let max_integer = Decimal::from(10i64.pow(integer_digits as u32)) - Decimal::ONE;
    let min_integer = -max_integer;

    let integer_part = adjusted.value.trunc();

    if integer_part > max_integer || integer_part < min_integer {
        ArithmeticResult::size_error()
    } else {
        ArithmeticResult::ok(adjusted)
    }
}

/// Evaluate a COMPUTE expression.
pub fn compute(
    value: Decimal,
    target_size: u8,
    target_decimals: u8,
    rounding: RoundingMode,
) -> ArithmeticResult {
    let num_value = NumericValue {
        value,
        decimal_places: target_decimals,
        is_signed: true,
    };

    apply_target_precision(num_value, target_size, target_decimals, rounding)
}

/// Perform power operation (exponentiation).
pub fn power(base: &NumericValue, exponent: i32) -> NumericValue {
    let result = if exponent >= 0 {
        let mut acc = Decimal::ONE;
        for _ in 0..exponent {
            acc *= base.value;
        }
        acc
    } else {
        let mut acc = Decimal::ONE;
        for _ in 0..(-exponent) {
            acc *= base.value;
        }
        Decimal::ONE / acc
    };

    NumericValue {
        value: result,
        decimal_places: 18, // Max precision
        is_signed: base.is_signed,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add_simple() {
        let a = NumericValue::from_i64(100);
        let b = NumericValue::from_i64(50);

        let result = add(&[a, b], 5, 0, RoundingMode::Truncate);

        assert!(!result.size_error);
        assert_eq!(result.value.unwrap().value, Decimal::from(150));
    }

    #[test]
    fn test_add_overflow() {
        let a = NumericValue::from_i64(99999);
        let b = NumericValue::from_i64(1);

        // Target can only hold 5 digits
        let result = add(&[a, b], 5, 0, RoundingMode::Truncate);

        assert!(result.size_error);
    }

    #[test]
    fn test_subtract() {
        let target = NumericValue::from_i64(100);
        let a = NumericValue::from_i64(30);
        let b = NumericValue::from_i64(20);

        let result = subtract_from(&[a, b], &target, 5, 0, RoundingMode::Truncate);

        assert!(!result.size_error);
        assert_eq!(result.value.unwrap().value, Decimal::from(50));
    }

    #[test]
    fn test_multiply() {
        let a = NumericValue::from_i64(10);
        let b = NumericValue::from_i64(5);

        let result = multiply(&a, &b, 5, 0, RoundingMode::Truncate);

        assert!(!result.size_error);
        assert_eq!(result.value.unwrap().value, Decimal::from(50));
    }

    #[test]
    fn test_divide() {
        let a = NumericValue::from_i64(100);
        let b = NumericValue::from_i64(3);

        let (result, remainder) = divide(&a, &b, 5, 2, RoundingMode::Truncate);

        assert!(!result.size_error);
        let quotient = result.value.unwrap();
        assert_eq!(quotient.to_display_string(), "33.33");

        // Remainder: 100 - (33 * 3) = 100 - 99 = 1
        let rem = remainder.unwrap();
        assert_eq!(rem.integer_part(), 1);
    }

    #[test]
    fn test_divide_by_zero() {
        let a = NumericValue::from_i64(100);
        let b = NumericValue::from_i64(0);

        let (result, _) = divide(&a, &b, 5, 0, RoundingMode::Truncate);

        assert!(result.size_error);
    }

    #[test]
    fn test_rounding() {
        // 10 / 3 = 3.333...
        let a = NumericValue::from_i64(10);
        let b = NumericValue::from_i64(3);

        // With truncation
        let (truncated, _) = divide(&a, &b, 5, 2, RoundingMode::Truncate);
        assert_eq!(truncated.value.unwrap().to_display_string(), "3.33");

        // With rounding
        let (rounded, _) = divide(&a, &b, 5, 2, RoundingMode::Round);
        assert_eq!(rounded.value.unwrap().to_display_string(), "3.33");
    }

    #[test]
    fn test_power() {
        let base = NumericValue::from_i64(2);

        let result = power(&base, 3);
        assert_eq!(result.value.trunc(), Decimal::from(8));

        let result_neg = power(&base, -2);
        assert_eq!(result_neg.to_display_string(), "0.250000000000000000");
    }

    #[test]
    fn test_precision_18_digits() {
        // Test IBM's 18-digit precision
        let a = NumericValue::new(
            Decimal::from_str_exact("123456789012345678").unwrap(),
            18,
            true,
        );
        let b = NumericValue::from_i64(1);

        let result = add(&[a, b], 18, 0, RoundingMode::Truncate);

        assert!(!result.size_error);
        assert_eq!(
            result.value.unwrap().value,
            Decimal::from_str_exact("123456789012345679").unwrap()
        );
    }
}

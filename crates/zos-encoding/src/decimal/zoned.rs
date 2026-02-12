//! Zoned Decimal (DISPLAY) encoding and decoding.
//!
//! Zoned decimal format stores one digit per byte, with the zone nibble
//! in the high position and the digit in the low position. This is the
//! default COBOL DISPLAY format for numeric data.
//!
//! # Format
//!
//! For a PIC S9(n) DISPLAY declaration:
//! - Each digit occupies one byte
//! - Zone nibble is 0xF for digits 0-8
//! - Sign is encoded in the zone nibble of the rightmost byte:
//!   - 0xC = positive (preferred)
//!   - 0xD = negative
//!   - 0xF = unsigned
//!
//! Example: +12345 in PIC S9(5)
//! - Binary: 0xF1 0xF2 0xF3 0xF4 0xC5 (5 bytes)
//! - The 'C' zone in last byte indicates positive

use rust_decimal::Decimal;
use std::str::FromStr;

use super::{Result, Sign};
use crate::error::EncodingError;

/// Zoned decimal value with metadata.
#[derive(Debug, Clone, PartialEq)]
pub struct ZonedDecimal {
    /// The decimal value
    pub value: Decimal,
    /// Number of integer digits (before decimal point)
    pub integer_digits: usize,
    /// Number of fractional digits (after decimal point)
    pub decimal_digits: usize,
    /// Whether the value is signed
    pub signed: bool,
}

impl ZonedDecimal {
    /// Create a new zoned decimal specification.
    pub fn new(value: Decimal, integer_digits: usize, decimal_digits: usize, signed: bool) -> Self {
        Self {
            value,
            integer_digits,
            decimal_digits,
            signed,
        }
    }

    /// Calculate the storage size in bytes.
    pub fn storage_size(&self) -> usize {
        self.integer_digits + self.decimal_digits
    }

    /// Encode the value to zoned decimal bytes.
    pub fn encode(&self) -> Result<Vec<u8>> {
        zone_decimal(
            &self.value,
            self.integer_digits,
            self.decimal_digits,
            self.signed,
        )
    }

    /// Decode zoned decimal bytes to a value.
    pub fn decode(bytes: &[u8], decimal_digits: usize) -> Result<Self> {
        let (value, sign) = unzone_decimal(bytes, decimal_digits)?;
        let integer_digits = if bytes.len() > decimal_digits {
            bytes.len() - decimal_digits
        } else {
            0
        };
        Ok(Self {
            value,
            integer_digits,
            decimal_digits,
            signed: sign != Sign::Unsigned,
        })
    }
}

/// Encode a decimal value to zoned decimal bytes.
///
/// # Arguments
/// * `value` - The decimal value to encode
/// * `integer_digits` - Number of digits before the decimal point
/// * `decimal_digits` - Number of digits after the decimal point
/// * `signed` - Whether to include a sign in the zone nibble
///
/// # Returns
/// A vector of bytes in zoned decimal format.
pub fn zone_decimal(
    value: &Decimal,
    integer_digits: usize,
    decimal_digits: usize,
    signed: bool,
) -> Result<Vec<u8>> {
    let total_digits = integer_digits + decimal_digits;

    // Determine sign
    let is_negative = value.is_sign_negative();
    let sign = if signed {
        if is_negative {
            Sign::Negative
        } else {
            Sign::Positive
        }
    } else {
        Sign::Unsigned
    };

    // Scale the value to remove decimal point
    let scale_factor = if decimal_digits > 0 {
        Decimal::from_str(&format!("1{}", "0".repeat(decimal_digits))).map_err(|e| {
            EncodingError::ConversionFailed {
                message: format!("Failed to create scale factor: {}", e),
            }
        })?
    } else {
        Decimal::from(1)
    };

    let scaled = (value.abs() * scale_factor).trunc();

    // Convert to string of digits
    let digit_string = scaled.to_string();
    let digit_string = digit_string.trim_start_matches('-');

    // Check if it fits
    if digit_string.len() > total_digits {
        return Err(EncodingError::ConversionFailed {
            message: format!(
                "Value {} requires {} digits but only {} available",
                value,
                digit_string.len(),
                total_digits
            ),
        });
    }

    // Pad with leading zeros
    let padded = format!("{:0>width$}", digit_string, width = total_digits);

    // Build the zoned bytes
    let mut result = Vec::with_capacity(total_digits);
    let digits: Vec<u8> = padded
        .chars()
        .map(|c| c.to_digit(10).unwrap() as u8)
        .collect();

    for (i, &digit) in digits.iter().enumerate() {
        let zone = if i == digits.len() - 1 {
            // Last digit: zone contains sign
            match sign {
                Sign::Positive => 0xC0,
                Sign::Negative => 0xD0,
                Sign::Unsigned => 0xF0,
            }
        } else {
            // Other digits: zone is always 0xF
            0xF0
        };
        result.push(zone | digit);
    }

    Ok(result)
}

/// Decode zoned decimal bytes to a decimal value.
///
/// # Arguments
/// * `bytes` - The zoned decimal bytes
/// * `decimal_digits` - Number of digits after the decimal point
///
/// # Returns
/// A tuple of (Decimal value, Sign).
pub fn unzone_decimal(bytes: &[u8], decimal_digits: usize) -> Result<(Decimal, Sign)> {
    if bytes.is_empty() {
        return Err(EncodingError::ConversionFailed {
            message: "Empty zoned decimal".to_string(),
        });
    }

    let mut digits = Vec::with_capacity(bytes.len());
    let mut sign = Sign::Unsigned;

    for (i, &byte) in bytes.iter().enumerate() {
        let zone = (byte >> 4) & 0x0F;
        let digit = byte & 0x0F;

        // Validate digit
        if digit > 9 {
            return Err(EncodingError::ConversionFailed {
                message: format!("Invalid digit nibble: 0x{:X} in byte 0x{:02X}", digit, byte),
            });
        }

        digits.push(digit);

        // Check for sign in zone nibble (usually only in last byte)
        if i == bytes.len() - 1 {
            sign = match zone {
                0x0C | 0x0A | 0x0E => Sign::Positive,
                0x0D | 0x0B => Sign::Negative,
                0x0F => Sign::Unsigned,
                _ => {
                    // Some systems use other zone values, treat as unsigned
                    Sign::Unsigned
                }
            };
        }
    }

    // Build the numeric string
    let digit_string: String = digits
        .iter()
        .map(|d| char::from_digit(*d as u32, 10).unwrap())
        .collect();

    // Insert decimal point if needed
    let numeric_string = if decimal_digits > 0 && digit_string.len() > decimal_digits {
        let int_part = &digit_string[..digit_string.len() - decimal_digits];
        let dec_part = &digit_string[digit_string.len() - decimal_digits..];
        format!("{}.{}", int_part, dec_part)
    } else if decimal_digits > 0 {
        let padded = format!("{:0>width$}", digit_string, width = decimal_digits + 1);
        let int_part = &padded[..padded.len() - decimal_digits];
        let dec_part = &padded[padded.len() - decimal_digits..];
        format!("{}.{}", int_part, dec_part)
    } else {
        digit_string
    };

    let mut value =
        Decimal::from_str(&numeric_string).map_err(|e| EncodingError::ConversionFailed {
            message: format!("Failed to parse decimal '{}': {}", numeric_string, e),
        })?;

    // Apply sign
    if sign == Sign::Negative {
        value = -value;
    }

    Ok((value, sign))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_zone_positive_integer() {
        let value = Decimal::from(12345);
        let zoned = zone_decimal(&value, 5, 0, true).unwrap();
        // +12345 in zoned: 0xF1 0xF2 0xF3 0xF4 0xC5
        assert_eq!(zoned, vec![0xF1, 0xF2, 0xF3, 0xF4, 0xC5]);
    }

    #[test]
    fn test_zone_negative_integer() {
        let value = Decimal::from(-12345);
        let zoned = zone_decimal(&value, 5, 0, true).unwrap();
        // -12345 in zoned: 0xF1 0xF2 0xF3 0xF4 0xD5
        assert_eq!(zoned, vec![0xF1, 0xF2, 0xF3, 0xF4, 0xD5]);
    }

    #[test]
    fn test_zone_unsigned() {
        let value = Decimal::from(12345);
        let zoned = zone_decimal(&value, 5, 0, false).unwrap();
        // 12345 unsigned: 0xF1 0xF2 0xF3 0xF4 0xF5
        assert_eq!(zoned, vec![0xF1, 0xF2, 0xF3, 0xF4, 0xF5]);
    }

    #[test]
    fn test_unzone_positive() {
        let bytes = vec![0xF1, 0xF2, 0xF3, 0xF4, 0xC5];
        let (value, sign) = unzone_decimal(&bytes, 0).unwrap();
        assert_eq!(value, Decimal::from(12345));
        assert_eq!(sign, Sign::Positive);
    }

    #[test]
    fn test_unzone_negative() {
        let bytes = vec![0xF1, 0xF2, 0xF3, 0xF4, 0xD5];
        let (value, sign) = unzone_decimal(&bytes, 0).unwrap();
        assert_eq!(value, Decimal::from(-12345));
        assert_eq!(sign, Sign::Negative);
    }

    #[test]
    fn test_unzone_with_decimals() {
        let bytes = vec![0xF1, 0xF2, 0xF3, 0xF4, 0xC5];
        let (value, _sign) = unzone_decimal(&bytes, 2).unwrap();
        assert_eq!(value, Decimal::from_str("123.45").unwrap());
    }

    #[test]
    fn test_roundtrip() {
        let original = Decimal::from_str("9876.54").unwrap();
        let zoned = zone_decimal(&original, 4, 2, true).unwrap();
        let (unzoned, _sign) = unzone_decimal(&zoned, 2).unwrap();
        assert_eq!(original, unzoned);
    }

    #[test]
    fn test_zoned_decimal_struct() {
        let zd = ZonedDecimal::new(Decimal::from(12345), 5, 0, true);
        assert_eq!(zd.storage_size(), 5);

        let encoded = zd.encode().unwrap();
        let decoded = ZonedDecimal::decode(&encoded, 0).unwrap();
        assert_eq!(zd.value, decoded.value);
    }
}

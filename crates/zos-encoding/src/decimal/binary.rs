//! Binary (COMP/COMP-4) encoding and decoding.
//!
//! Binary format stores integers in big-endian two's complement format.
//! This is used for COBOL COMP and COMP-4 data items.
//!
//! # Format
//!
//! COBOL binary storage sizes are determined by the PIC clause:
//! - PIC S9(1) to S9(4): 2 bytes (halfword)
//! - PIC S9(5) to S9(9): 4 bytes (fullword)
//! - PIC S9(10) to S9(18): 8 bytes (doubleword)
//!
//! All values are stored in big-endian (network) byte order, which is
//! the native format on IBM mainframes.

use rust_decimal::Decimal;
use std::str::FromStr;

use super::Result;
use crate::error::EncodingError;

/// Binary integer value with metadata.
#[derive(Debug, Clone, PartialEq)]
pub struct BinaryInteger {
    /// The integer value (as Decimal for compatibility)
    pub value: Decimal,
    /// Number of digits in the PIC clause
    pub digits: usize,
    /// Whether the value is signed
    pub signed: bool,
}

impl BinaryInteger {
    /// Create a new binary integer specification.
    pub fn new(value: Decimal, digits: usize, signed: bool) -> Self {
        Self {
            value,
            digits,
            signed,
        }
    }

    /// Calculate the storage size in bytes based on COBOL rules.
    ///
    /// - 1-4 digits: 2 bytes (halfword)
    /// - 5-9 digits: 4 bytes (fullword)
    /// - 10-18 digits: 8 bytes (doubleword)
    pub fn storage_size(&self) -> usize {
        storage_size_for_digits(self.digits)
    }

    /// Encode the value to binary bytes.
    pub fn encode(&self) -> Result<Vec<u8>> {
        encode_binary(&self.value, self.digits, self.signed)
    }

    /// Decode binary bytes to a value.
    pub fn decode(bytes: &[u8], signed: bool) -> Result<Self> {
        let value = decode_binary(bytes, signed)?;
        let digits = digits_for_storage_size(bytes.len());
        Ok(Self {
            value,
            digits,
            signed,
        })
    }
}

/// Calculate storage size for a given number of digits.
pub fn storage_size_for_digits(digits: usize) -> usize {
    match digits {
        1..=4 => 2,
        5..=9 => 4,
        10..=18 => 8,
        _ => 8, // Default to doubleword for larger values
    }
}

/// Calculate the number of digits for a given storage size.
fn digits_for_storage_size(size: usize) -> usize {
    match size {
        2 => 4,
        4 => 9,
        8 => 18,
        _ => 18,
    }
}

/// Encode a decimal value to binary bytes.
///
/// # Arguments
/// * `value` - The value to encode (must be an integer)
/// * `digits` - Number of digits in the PIC clause
/// * `signed` - Whether the value is signed
///
/// # Returns
/// A vector of bytes in big-endian format.
pub fn encode_binary(value: &Decimal, digits: usize, signed: bool) -> Result<Vec<u8>> {
    let size = storage_size_for_digits(digits);

    // Convert to integer
    let int_value: i128 =
        value
            .trunc()
            .to_string()
            .parse()
            .map_err(|e| EncodingError::ConversionFailed {
                message: format!("Failed to convert decimal to integer: {}", e),
            })?;

    // Validate range
    let (min, max) = if signed {
        match size {
            2 => (i16::MIN as i128, i16::MAX as i128),
            4 => (i32::MIN as i128, i32::MAX as i128),
            8 => (i64::MIN as i128, i64::MAX as i128),
            _ => (i64::MIN as i128, i64::MAX as i128),
        }
    } else {
        match size {
            2 => (0, u16::MAX as i128),
            4 => (0, u32::MAX as i128),
            8 => (0, u64::MAX as i128),
            _ => (0, u64::MAX as i128),
        }
    };

    if int_value < min || int_value > max {
        return Err(EncodingError::ConversionFailed {
            message: format!(
                "Value {} out of range for {} {} bytes ({}..{})",
                int_value,
                if signed { "signed" } else { "unsigned" },
                size,
                min,
                max
            ),
        });
    }

    // Encode to big-endian bytes
    let result = match size {
        2 => (int_value as i16).to_be_bytes().to_vec(),
        4 => (int_value as i32).to_be_bytes().to_vec(),
        8 => (int_value as i64).to_be_bytes().to_vec(),
        _ => (int_value as i64).to_be_bytes().to_vec(),
    };

    Ok(result)
}

/// Decode binary bytes to a decimal value.
///
/// # Arguments
/// * `bytes` - The big-endian binary bytes
/// * `signed` - Whether to interpret as signed
///
/// # Returns
/// A Decimal value.
pub fn decode_binary(bytes: &[u8], signed: bool) -> Result<Decimal> {
    if bytes.is_empty() {
        return Err(EncodingError::ConversionFailed {
            message: "Empty binary value".to_string(),
        });
    }

    let int_value: i64 = match bytes.len() {
        2 => {
            let arr: [u8; 2] = bytes
                .try_into()
                .map_err(|_| EncodingError::ConversionFailed {
                    message: "Invalid halfword length".to_string(),
                })?;
            if signed {
                i16::from_be_bytes(arr) as i64
            } else {
                u16::from_be_bytes(arr) as i64
            }
        }
        4 => {
            let arr: [u8; 4] = bytes
                .try_into()
                .map_err(|_| EncodingError::ConversionFailed {
                    message: "Invalid fullword length".to_string(),
                })?;
            if signed {
                i32::from_be_bytes(arr) as i64
            } else {
                u32::from_be_bytes(arr) as i64
            }
        }
        8 => {
            let arr: [u8; 8] = bytes
                .try_into()
                .map_err(|_| EncodingError::ConversionFailed {
                    message: "Invalid doubleword length".to_string(),
                })?;
            if signed {
                i64::from_be_bytes(arr)
            } else {
                // For unsigned 64-bit, we need to handle overflow
                let unsigned = u64::from_be_bytes(arr);
                if unsigned > i64::MAX as u64 {
                    return Err(EncodingError::ConversionFailed {
                        message: format!("Unsigned value {} too large for Decimal", unsigned),
                    });
                }
                unsigned as i64
            }
        }
        _ => {
            return Err(EncodingError::ConversionFailed {
                message: format!("Invalid binary length: {} bytes", bytes.len()),
            })
        }
    };

    Decimal::from_str(&int_value.to_string()).map_err(|e| EncodingError::ConversionFailed {
        message: format!("Failed to convert integer to Decimal: {}", e),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_storage_size() {
        assert_eq!(storage_size_for_digits(1), 2);
        assert_eq!(storage_size_for_digits(4), 2);
        assert_eq!(storage_size_for_digits(5), 4);
        assert_eq!(storage_size_for_digits(9), 4);
        assert_eq!(storage_size_for_digits(10), 8);
        assert_eq!(storage_size_for_digits(18), 8);
    }

    #[test]
    fn test_encode_halfword_positive() {
        let value = Decimal::from(12345);
        let encoded = encode_binary(&value, 4, true).unwrap();
        // 12345 in big-endian 16-bit = 0x3039
        assert_eq!(encoded, vec![0x30, 0x39]);
    }

    #[test]
    fn test_encode_halfword_negative() {
        let value = Decimal::from(-12345);
        let encoded = encode_binary(&value, 4, true).unwrap();
        // -12345 in two's complement big-endian 16-bit = 0xCFC7
        assert_eq!(encoded, vec![0xCF, 0xC7]);
    }

    #[test]
    fn test_encode_fullword() {
        let value = Decimal::from(123456789);
        let encoded = encode_binary(&value, 9, true).unwrap();
        // 123456789 in big-endian 32-bit = 0x075BCD15
        assert_eq!(encoded, vec![0x07, 0x5B, 0xCD, 0x15]);
    }

    #[test]
    fn test_encode_doubleword() {
        let value = Decimal::from(1234567890123456789_i64);
        let encoded = encode_binary(&value, 18, true).unwrap();
        // 1234567890123456789 in big-endian 64-bit
        assert_eq!(
            encoded,
            vec![0x11, 0x22, 0x10, 0xF4, 0x7D, 0xE9, 0x81, 0x15]
        );
    }

    #[test]
    fn test_decode_halfword_positive() {
        let bytes = vec![0x30, 0x39];
        let value = decode_binary(&bytes, true).unwrap();
        assert_eq!(value, Decimal::from(12345));
    }

    #[test]
    fn test_decode_halfword_negative() {
        let bytes = vec![0xCF, 0xC7];
        let value = decode_binary(&bytes, true).unwrap();
        assert_eq!(value, Decimal::from(-12345));
    }

    #[test]
    fn test_decode_fullword() {
        let bytes = vec![0x07, 0x5B, 0xCD, 0x15];
        let value = decode_binary(&bytes, true).unwrap();
        assert_eq!(value, Decimal::from(123456789));
    }

    #[test]
    fn test_roundtrip_halfword() {
        let original = Decimal::from(-9999);
        let encoded = encode_binary(&original, 4, true).unwrap();
        let decoded = decode_binary(&encoded, true).unwrap();
        assert_eq!(original, decoded);
    }

    #[test]
    fn test_roundtrip_fullword() {
        let original = Decimal::from(999999999);
        let encoded = encode_binary(&original, 9, true).unwrap();
        let decoded = decode_binary(&encoded, true).unwrap();
        assert_eq!(original, decoded);
    }

    #[test]
    fn test_unsigned() {
        let value = Decimal::from(65535);
        let encoded = encode_binary(&value, 4, false).unwrap();
        assert_eq!(encoded, vec![0xFF, 0xFF]);

        let decoded = decode_binary(&encoded, false).unwrap();
        assert_eq!(decoded, Decimal::from(65535));
    }

    #[test]
    fn test_binary_integer_struct() {
        let bi = BinaryInteger::new(Decimal::from(12345), 4, true);
        assert_eq!(bi.storage_size(), 2);

        let encoded = bi.encode().unwrap();
        let decoded = BinaryInteger::decode(&encoded, true).unwrap();
        assert_eq!(bi.value, decoded.value);
    }
}

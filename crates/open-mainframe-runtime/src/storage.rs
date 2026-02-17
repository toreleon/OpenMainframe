//! Binary storage format support for COBOL USAGE clauses.
//!
//! Implements conversion between the runtime's `NumericValue` (backed by
//! `rust_decimal::Decimal`) and the low-level byte representations used
//! for COBOL COMP, COMP-3, and COMP-5 storage formats.
//!
//! The encoding/decoding logic is delegated to the `open-mainframe-encoding`
//! crate; this module provides the integration layer that connects runtime
//! `NumericValue` with those byte-level operations.
//!
//! # Storage Formats
//!
//! | COBOL USAGE  | Format                | Module                       |
//! |-------------|------------------------|------------------------------|
//! | DISPLAY     | Zoned decimal          | (default — no conversion)    |
//! | COMP / COMP-4 | Binary big-endian    | `open_mainframe_encoding::encode_binary` |
//! | COMP-3      | Packed decimal (BCD)   | `open_mainframe_encoding::pack_decimal`  |
//! | COMP-5      | Native binary          | `open_mainframe_encoding::encode_native_binary` |

use rust_decimal::Decimal;

use crate::error::RuntimeError;
use crate::value::NumericValue;

/// COBOL USAGE storage format.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StorageFormat {
    /// DISPLAY — default zoned decimal (one digit per byte).
    Display,
    /// COMP / COMP-4 — big-endian binary integer.
    Binary,
    /// COMP-3 — packed decimal (BCD with sign nibble).
    PackedDecimal,
    /// COMP-5 — native binary (full storage range).
    NativeBinary,
}

impl StorageFormat {
    /// Return the COBOL keyword for this format.
    pub fn keyword(&self) -> &'static str {
        match self {
            StorageFormat::Display => "DISPLAY",
            StorageFormat::Binary => "COMP",
            StorageFormat::PackedDecimal => "COMP-3",
            StorageFormat::NativeBinary => "COMP-5",
        }
    }
}

/// A numeric value paired with its storage format and PIC metadata.
///
/// This struct carries the information needed to convert between the
/// runtime `Decimal` representation and the on-disk/in-memory byte format.
#[derive(Debug, Clone, PartialEq)]
pub struct StoredNumeric {
    /// The numeric value.
    pub value: NumericValue,
    /// The storage format (USAGE clause).
    pub format: StorageFormat,
    /// Total number of digits in the PIC clause.
    pub total_digits: usize,
}

impl StoredNumeric {
    /// Create a new stored numeric.
    pub fn new(value: NumericValue, format: StorageFormat, total_digits: usize) -> Self {
        Self {
            value,
            format,
            total_digits,
        }
    }

    /// Encode the value to its byte representation.
    pub fn to_bytes(&self) -> Result<Vec<u8>, RuntimeError> {
        encode_value(
            &self.value.value,
            self.format,
            self.total_digits,
            self.value.decimal_places as usize,
            self.value.is_signed,
        )
    }

    /// Decode from bytes into a stored numeric.
    pub fn from_bytes(
        bytes: &[u8],
        format: StorageFormat,
        total_digits: usize,
        decimal_places: usize,
        signed: bool,
    ) -> Result<Self, RuntimeError> {
        let value = decode_value(bytes, format, decimal_places, signed)?;
        Ok(Self {
            value: NumericValue {
                value,
                decimal_places: decimal_places as u8,
                is_signed: signed,
            },
            format,
            total_digits,
        })
    }

    /// Get the storage size in bytes.
    pub fn byte_length(&self) -> usize {
        storage_byte_length(
            self.format,
            self.total_digits,
            self.value.decimal_places as usize,
        )
    }
}

/// Calculate the storage size in bytes for a given format and PIC specification.
pub fn storage_byte_length(
    format: StorageFormat,
    total_digits: usize,
    decimal_places: usize,
) -> usize {
    match format {
        StorageFormat::Display => {
            // One byte per digit (zoned decimal).
            total_digits
        }
        StorageFormat::Binary | StorageFormat::NativeBinary => {
            // Integer digits determine storage size (COBOL halfword/fullword/doubleword).
            let int_digits = total_digits - decimal_places;
            binary_storage_size(int_digits)
        }
        StorageFormat::PackedDecimal => {
            // ceil((total_digits + 1) / 2)
            (total_digits + 2) / 2
        }
    }
}

/// Encode a `Decimal` value to bytes according to the storage format.
pub fn encode_value(
    value: &Decimal,
    format: StorageFormat,
    total_digits: usize,
    decimal_places: usize,
    signed: bool,
) -> Result<Vec<u8>, RuntimeError> {
    match format {
        StorageFormat::Display => {
            // Zoned decimal encoding.
            let int_digits = total_digits - decimal_places;
            open_mainframe_encoding::zone_decimal(value, int_digits, decimal_places, signed)
                .map_err(|e| RuntimeError::DataException {
                    field: format!("zone_decimal: {}", e),
                })
        }
        StorageFormat::Binary => {
            // COMP — big-endian binary. Scale to integer first.
            let int_digits = total_digits - decimal_places;
            let scaled = scale_to_integer(value, decimal_places);
            open_mainframe_encoding::encode_binary(&scaled, int_digits, signed).map_err(|e| {
                RuntimeError::DataException {
                    field: format!("encode_binary: {}", e),
                }
            })
        }
        StorageFormat::NativeBinary => {
            // COMP-5 — same encoding, full storage range.
            let int_digits = total_digits - decimal_places;
            let scaled = scale_to_integer(value, decimal_places);
            open_mainframe_encoding::encode_native_binary(&scaled, int_digits, signed).map_err(
                |e| RuntimeError::DataException {
                    field: format!("encode_native_binary: {}", e),
                },
            )
        }
        StorageFormat::PackedDecimal => {
            // COMP-3 — packed decimal.
            let int_digits = total_digits - decimal_places;
            open_mainframe_encoding::pack_decimal(value, int_digits, decimal_places, signed)
                .map_err(|e| RuntimeError::DataException {
                    field: format!("pack_decimal: {}", e),
                })
        }
    }
}

/// Decode bytes to a `Decimal` value according to the storage format.
pub fn decode_value(
    bytes: &[u8],
    format: StorageFormat,
    decimal_places: usize,
    signed: bool,
) -> Result<Decimal, RuntimeError> {
    match format {
        StorageFormat::Display => {
            // Zoned decimal decoding.
            let (value, _sign) =
                open_mainframe_encoding::unzone_decimal(bytes, decimal_places).map_err(|e| {
                    RuntimeError::DataException {
                        field: format!("unzone_decimal: {}", e),
                    }
                })?;
            Ok(value)
        }
        StorageFormat::Binary | StorageFormat::NativeBinary => {
            // Binary decoding — then unscale from integer.
            let int_value =
                open_mainframe_encoding::decode_binary(bytes, signed).map_err(|e| {
                    RuntimeError::DataException {
                        field: format!("decode_binary: {}", e),
                    }
                })?;
            Ok(unscale_from_integer(&int_value, decimal_places))
        }
        StorageFormat::PackedDecimal => {
            // COMP-3 decoding.
            let (value, _sign) =
                open_mainframe_encoding::unpack_decimal(bytes, decimal_places).map_err(|e| {
                    RuntimeError::DataException {
                        field: format!("unpack_decimal: {}", e),
                    }
                })?;
            Ok(value)
        }
    }
}

/// Calculate binary storage size in bytes from the number of digits.
///
/// Mirrors the COBOL standard:
/// - 1-4 digits: 2 bytes (halfword)
/// - 5-9 digits: 4 bytes (fullword)
/// - 10-18 digits: 8 bytes (doubleword)
fn binary_storage_size(digits: usize) -> usize {
    match digits {
        1..=4 => 2,
        5..=9 => 4,
        10..=18 => 8,
        _ => 8,
    }
}

/// Scale a decimal value to an integer by multiplying by 10^decimal_places.
/// E.g., 123.45 with 2 decimal places becomes 12345.
fn scale_to_integer(value: &Decimal, decimal_places: usize) -> Decimal {
    if decimal_places == 0 {
        value.trunc()
    } else {
        let scale = Decimal::from(10i64.pow(decimal_places as u32));
        (*value * scale).trunc()
    }
}

/// Unscale an integer value to a decimal by dividing by 10^decimal_places.
/// E.g., 12345 with 2 decimal places becomes 123.45.
fn unscale_from_integer(value: &Decimal, decimal_places: usize) -> Decimal {
    if decimal_places == 0 {
        *value
    } else {
        let scale = Decimal::from(10i64.pow(decimal_places as u32));
        *value / scale
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    // ---- StorageFormat basics ----

    #[test]
    fn test_storage_format_keywords() {
        assert_eq!(StorageFormat::Display.keyword(), "DISPLAY");
        assert_eq!(StorageFormat::Binary.keyword(), "COMP");
        assert_eq!(StorageFormat::PackedDecimal.keyword(), "COMP-3");
        assert_eq!(StorageFormat::NativeBinary.keyword(), "COMP-5");
    }

    // ---- Byte length calculations ----

    #[test]
    fn test_display_byte_length() {
        // PIC 9(5) DISPLAY = 5 bytes
        assert_eq!(storage_byte_length(StorageFormat::Display, 5, 0), 5);
        // PIC 9(5)V99 DISPLAY = 7 bytes
        assert_eq!(storage_byte_length(StorageFormat::Display, 7, 2), 7);
    }

    #[test]
    fn test_binary_byte_length() {
        // PIC S9(4) COMP = 2 bytes (halfword)
        assert_eq!(storage_byte_length(StorageFormat::Binary, 4, 0), 2);
        // PIC S9(9) COMP = 4 bytes (fullword)
        assert_eq!(storage_byte_length(StorageFormat::Binary, 9, 0), 4);
        // PIC S9(18) COMP = 8 bytes (doubleword)
        assert_eq!(storage_byte_length(StorageFormat::Binary, 18, 0), 8);
    }

    #[test]
    fn test_packed_byte_length() {
        // PIC S9(5) COMP-3 = ceil((5+1)/2) = 3 bytes
        assert_eq!(storage_byte_length(StorageFormat::PackedDecimal, 5, 0), 3);
        // PIC S9(7) COMP-3 = ceil((7+1)/2) = 4 bytes
        assert_eq!(storage_byte_length(StorageFormat::PackedDecimal, 7, 0), 4);
        // PIC S9(5)V99 COMP-3 = ceil((7+1)/2) = 4 bytes
        assert_eq!(storage_byte_length(StorageFormat::PackedDecimal, 7, 2), 4);
    }

    // ---- COMP-3 (Packed Decimal) encoding/decoding ----

    #[test]
    fn test_comp3_encode_positive() {
        // PIC S9(5) COMP-3, value +1234
        let val = Decimal::from(1234);
        let bytes = encode_value(&val, StorageFormat::PackedDecimal, 5, 0, true).unwrap();
        // 01234C
        assert_eq!(bytes, vec![0x01, 0x23, 0x4C]);
    }

    #[test]
    fn test_comp3_encode_negative() {
        // PIC S9(5) COMP-3, value -5678
        let val = Decimal::from(-5678);
        let bytes = encode_value(&val, StorageFormat::PackedDecimal, 5, 0, true).unwrap();
        // 05678D
        assert_eq!(bytes, vec![0x05, 0x67, 0x8D]);
    }

    #[test]
    fn test_comp3_decode_positive() {
        // 01234C = +1234
        let bytes = vec![0x01, 0x23, 0x4C];
        let val = decode_value(&bytes, StorageFormat::PackedDecimal, 0, true).unwrap();
        assert_eq!(val, Decimal::from(1234));
    }

    #[test]
    fn test_comp3_decode_negative() {
        // 05678D = -5678
        let bytes = vec![0x05, 0x67, 0x8D];
        let val = decode_value(&bytes, StorageFormat::PackedDecimal, 0, true).unwrap();
        assert_eq!(val, Decimal::from(-5678));
    }

    #[test]
    fn test_comp3_roundtrip() {
        let original = Decimal::from(-9999);
        let bytes = encode_value(&original, StorageFormat::PackedDecimal, 5, 0, true).unwrap();
        let decoded = decode_value(&bytes, StorageFormat::PackedDecimal, 0, true).unwrap();
        assert_eq!(original, decoded);
    }

    #[test]
    fn test_comp3_with_decimals() {
        // PIC S9(3)V99 COMP-3, value 123.45
        let val = Decimal::from_str("123.45").unwrap();
        let bytes = encode_value(&val, StorageFormat::PackedDecimal, 5, 2, true).unwrap();
        let decoded = decode_value(&bytes, StorageFormat::PackedDecimal, 2, true).unwrap();
        assert_eq!(val, decoded);
    }

    // ---- COMP (Binary) encoding/decoding ----

    #[test]
    fn test_comp_encode_halfword() {
        // PIC S9(4) COMP, value 1234
        let val = Decimal::from(1234);
        let bytes = encode_value(&val, StorageFormat::Binary, 4, 0, true).unwrap();
        // 1234 in big-endian 16-bit = 0x04D2
        assert_eq!(bytes, vec![0x04, 0xD2]);
    }

    #[test]
    fn test_comp_encode_fullword_negative() {
        // PIC S9(9) COMP, value -100000
        let val = Decimal::from(-100000);
        let bytes = encode_value(&val, StorageFormat::Binary, 9, 0, true).unwrap();
        // -100000 in big-endian 32-bit
        let expected = (-100000_i32).to_be_bytes().to_vec();
        assert_eq!(bytes, expected);
    }

    #[test]
    fn test_comp_decode_halfword() {
        let bytes = vec![0x04, 0xD2]; // 1234
        let val = decode_value(&bytes, StorageFormat::Binary, 0, true).unwrap();
        assert_eq!(val, Decimal::from(1234));
    }

    #[test]
    fn test_comp_roundtrip() {
        let original = Decimal::from(12345);
        let bytes = encode_value(&original, StorageFormat::Binary, 9, 0, true).unwrap();
        let decoded = decode_value(&bytes, StorageFormat::Binary, 0, true).unwrap();
        assert_eq!(original, decoded);
    }

    #[test]
    fn test_comp_with_decimal_places() {
        // PIC S9(4)V99 COMP — value 12.34 is stored as 1234 in binary
        let val = Decimal::from_str("12.34").unwrap();
        let bytes = encode_value(&val, StorageFormat::Binary, 4, 2, true).unwrap();
        // 1234 in 2-byte big-endian
        assert_eq!(bytes, vec![0x04, 0xD2]);

        let decoded = decode_value(&bytes, StorageFormat::Binary, 2, true).unwrap();
        assert_eq!(decoded, val);
    }

    // ---- COMP-5 (Native Binary) encoding/decoding ----

    #[test]
    fn test_comp5_encode() {
        // PIC S9(4) COMP-5, value 30000 (exceeds COMP's 9999 limit)
        let val = Decimal::from(30000);
        let bytes = encode_value(&val, StorageFormat::NativeBinary, 4, 0, true).unwrap();
        assert_eq!(bytes, vec![0x75, 0x30]); // 30000 in big-endian 16-bit
    }

    #[test]
    fn test_comp5_roundtrip() {
        let original = Decimal::from(32767);
        let bytes = encode_value(&original, StorageFormat::NativeBinary, 4, 0, true).unwrap();
        let decoded = decode_value(&bytes, StorageFormat::NativeBinary, 0, true).unwrap();
        assert_eq!(original, decoded);
    }

    // ---- StoredNumeric struct ----

    #[test]
    fn test_stored_numeric_comp3() {
        let nv = NumericValue::new(Decimal::from(1234), 0, true);
        let stored = StoredNumeric::new(nv, StorageFormat::PackedDecimal, 5);

        assert_eq!(stored.byte_length(), 3); // ceil((5+1)/2) = 3
        let bytes = stored.to_bytes().unwrap();
        assert_eq!(bytes, vec![0x01, 0x23, 0x4C]);

        let decoded =
            StoredNumeric::from_bytes(&bytes, StorageFormat::PackedDecimal, 5, 0, true).unwrap();
        assert_eq!(decoded.value.value, Decimal::from(1234));
    }

    #[test]
    fn test_stored_numeric_binary() {
        let nv = NumericValue::new(Decimal::from(42), 0, true);
        let stored = StoredNumeric::new(nv, StorageFormat::Binary, 4);

        assert_eq!(stored.byte_length(), 2);
        let bytes = stored.to_bytes().unwrap();
        assert_eq!(bytes, vec![0x00, 0x2A]); // 42 big-endian

        let decoded =
            StoredNumeric::from_bytes(&bytes, StorageFormat::Binary, 4, 0, true).unwrap();
        assert_eq!(decoded.value.value, Decimal::from(42));
    }

    // ---- Scale/unscale helpers ----

    #[test]
    fn test_scale_to_integer() {
        let val = Decimal::from_str("123.45").unwrap();
        let scaled = scale_to_integer(&val, 2);
        assert_eq!(scaled, Decimal::from(12345));
    }

    #[test]
    fn test_unscale_from_integer() {
        let val = Decimal::from(12345);
        let unscaled = unscale_from_integer(&val, 2);
        assert_eq!(unscaled, Decimal::from_str("123.45").unwrap());
    }

    #[test]
    fn test_scale_no_decimals() {
        let val = Decimal::from(999);
        let scaled = scale_to_integer(&val, 0);
        assert_eq!(scaled, Decimal::from(999));
    }
}

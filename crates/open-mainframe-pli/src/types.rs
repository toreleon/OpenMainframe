//! PL/I Type System — runtime types, values, and implicit conversion rules.
//!
//! Implements PL/I's comprehensive type system including arithmetic types
//! (FIXED/FLOAT × DECIMAL/BINARY), string types (CHARACTER, BIT, GRAPHIC,
//! WIDECHAR with fixed/varying), pointer/special types (POINTER, OFFSET,
//! AREA, HANDLE, LABEL, ENTRY, FILE), and automatic conversion rules.

use serde::{Deserialize, Serialize};
use std::fmt;

// ---------------------------------------------------------------------------
//  Type descriptors
// ---------------------------------------------------------------------------

/// A resolved PL/I data type with full attributes.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum PliType {
    /// FIXED DECIMAL(precision, scale).
    FixedDecimal(u8, u8),
    /// FIXED BINARY(precision, scale).
    FixedBinary(u8, u8),
    /// FLOAT DECIMAL(precision).
    FloatDecimal(u8),
    /// FLOAT BINARY(precision).
    FloatBinary(u8),
    /// CHARACTER(length).
    Character(u32),
    /// CHARACTER(length) VARYING.
    CharacterVarying(u32),
    /// BIT(length).
    Bit(u32),
    /// BIT(length) VARYING.
    BitVarying(u32),
    /// GRAPHIC(length).
    Graphic(u32),
    /// WIDECHAR(length).
    Widechar(u32),
    /// POINTER.
    Pointer,
    /// OFFSET.
    Offset,
    /// HANDLE(structure_name).
    Handle(String),
    /// AREA(size).
    Area(u32),
    /// LABEL.
    Label,
    /// ENTRY.
    Entry,
    /// FILE.
    File,
    /// FORMAT.
    Format,
    /// PICTURE 'spec'.
    Picture(String),
    /// Structure (aggregate with members).
    Structure(Vec<StructureMember>),
    /// Union (overlay members).
    Union(Vec<StructureMember>),
}

/// A member of a structure or union.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct StructureMember {
    /// Level number (1-15).
    pub level: u8,
    /// Member name.
    pub name: String,
    /// Member type.
    pub pli_type: PliType,
}

impl PliType {
    /// Return the type category for conversion rule lookup.
    pub fn category(&self) -> TypeCategory {
        match self {
            PliType::FixedDecimal(..) | PliType::FixedBinary(..) => TypeCategory::Arithmetic,
            PliType::FloatDecimal(..) | PliType::FloatBinary(..) => TypeCategory::Arithmetic,
            PliType::Character(..) | PliType::CharacterVarying(..) => TypeCategory::String,
            PliType::Bit(..) | PliType::BitVarying(..) => TypeCategory::Bit,
            PliType::Graphic(..) | PliType::Widechar(..) => TypeCategory::String,
            PliType::Picture(..) => TypeCategory::String,
            PliType::Pointer | PliType::Offset | PliType::Handle(..) => TypeCategory::Pointer,
            PliType::Area(..) => TypeCategory::Area,
            PliType::Label | PliType::Entry => TypeCategory::Program,
            PliType::File | PliType::Format => TypeCategory::Program,
            PliType::Structure(..) | PliType::Union(..) => TypeCategory::Aggregate,
        }
    }

    /// Check if this type is arithmetic.
    pub fn is_arithmetic(&self) -> bool {
        self.category() == TypeCategory::Arithmetic
    }

    /// Check if this type is a string type.
    pub fn is_string(&self) -> bool {
        matches!(self.category(), TypeCategory::String | TypeCategory::Bit)
    }

    /// Check if conversion to the target type is possible.
    pub fn can_convert_to(&self, target: &PliType) -> bool {
        // Same type is always convertible.
        if self == target {
            return true;
        }
        let src = self.category();
        let dst = target.category();

        matches!(
            (src, dst),
            // Arithmetic ↔ Arithmetic
            (TypeCategory::Arithmetic, TypeCategory::Arithmetic)
            // Arithmetic ↔ String
            | (TypeCategory::Arithmetic, TypeCategory::String)
            | (TypeCategory::String, TypeCategory::Arithmetic)
            // Arithmetic ↔ Bit
            | (TypeCategory::Arithmetic, TypeCategory::Bit)
            | (TypeCategory::Bit, TypeCategory::Arithmetic)
            // String ↔ String
            | (TypeCategory::String, TypeCategory::String)
            // String ↔ Bit
            | (TypeCategory::String, TypeCategory::Bit)
            | (TypeCategory::Bit, TypeCategory::String)
            // Bit ↔ Bit
            | (TypeCategory::Bit, TypeCategory::Bit)
        )
    }

    /// Get storage size in bytes for this type.
    pub fn storage_size(&self) -> usize {
        match self {
            PliType::FixedDecimal(p, _) => {
                // Packed decimal: (p + 2) / 2 bytes.
                (*p as usize + 2) / 2
            }
            PliType::FixedBinary(p, _) => {
                if *p <= 7 {
                    1
                } else if *p <= 15 {
                    2
                } else if *p <= 31 {
                    4
                } else {
                    8
                }
            }
            PliType::FloatDecimal(p) => {
                if *p <= 6 {
                    4 // single precision
                } else if *p <= 16 {
                    8 // double precision
                } else {
                    16 // extended
                }
            }
            PliType::FloatBinary(p) => {
                if *p <= 21 {
                    4
                } else if *p <= 53 {
                    8
                } else {
                    16
                }
            }
            PliType::Character(n) | PliType::CharacterVarying(n) => *n as usize,
            PliType::Bit(n) | PliType::BitVarying(n) => (*n as usize + 7) / 8,
            PliType::Graphic(n) | PliType::Widechar(n) => *n as usize * 2,
            PliType::Pointer | PliType::Offset | PliType::Handle(..) => 8,
            PliType::Area(n) => *n as usize,
            PliType::Label | PliType::Entry => 8,
            PliType::File | PliType::Format => 8,
            PliType::Picture(spec) => spec.chars().filter(|c| !matches!(c, 'V' | '.' | '+' | '-' | 'B' | '/' | ',')).count(),
            PliType::Structure(members) | PliType::Union(members) => {
                members.iter().map(|m| m.pli_type.storage_size()).sum()
            }
        }
    }
}

impl fmt::Display for PliType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PliType::FixedDecimal(p, s) => write!(f, "FIXED DECIMAL({p},{s})"),
            PliType::FixedBinary(p, s) => write!(f, "FIXED BINARY({p},{s})"),
            PliType::FloatDecimal(p) => write!(f, "FLOAT DECIMAL({p})"),
            PliType::FloatBinary(p) => write!(f, "FLOAT BINARY({p})"),
            PliType::Character(n) => write!(f, "CHARACTER({n})"),
            PliType::CharacterVarying(n) => write!(f, "CHARACTER({n}) VARYING"),
            PliType::Bit(n) => write!(f, "BIT({n})"),
            PliType::BitVarying(n) => write!(f, "BIT({n}) VARYING"),
            PliType::Graphic(n) => write!(f, "GRAPHIC({n})"),
            PliType::Widechar(n) => write!(f, "WIDECHAR({n})"),
            PliType::Pointer => write!(f, "POINTER"),
            PliType::Offset => write!(f, "OFFSET"),
            PliType::Handle(s) => write!(f, "HANDLE({s})"),
            PliType::Area(n) => write!(f, "AREA({n})"),
            PliType::Label => write!(f, "LABEL"),
            PliType::Entry => write!(f, "ENTRY"),
            PliType::File => write!(f, "FILE"),
            PliType::Format => write!(f, "FORMAT"),
            PliType::Picture(s) => write!(f, "PICTURE '{s}'"),
            PliType::Structure(m) => write!(f, "STRUCTURE({} members)", m.len()),
            PliType::Union(m) => write!(f, "UNION({} members)", m.len()),
        }
    }
}

/// Type categories for conversion rule lookup.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeCategory {
    Arithmetic,
    String,
    Bit,
    Pointer,
    Area,
    Program,
    Aggregate,
}

// ---------------------------------------------------------------------------
//  Runtime values
// ---------------------------------------------------------------------------

/// A PL/I runtime value.
#[derive(Debug, Clone, PartialEq)]
pub enum PliValue {
    /// Fixed-point decimal: stored as i128 with implicit scale.
    /// E.g., FIXED DEC(7,2) value 12345.67 → raw=1234567, scale=2.
    FixedDecimal { raw: i128, precision: u8, scale: u8 },
    /// Fixed-point binary: stored as i64.
    FixedBinary(i64),
    /// Floating-point decimal: stored as f64.
    FloatDecimal(f64),
    /// Floating-point binary: stored as f64.
    FloatBinary(f64),
    /// Character string.
    Character(String),
    /// Bit string (stored as Vec<bool>).
    Bit(Vec<bool>),
    /// Graphic / widechar string.
    Graphic(Vec<u16>),
    /// Pointer value (opaque address).
    Pointer(u64),
    /// Offset value.
    Offset(u64),
    /// Label reference.
    Label(String),
    /// Entry reference.
    Entry(String),
    /// File reference.
    File(String),
    /// Null value.
    Null,
}

impl PliValue {
    /// Get the PL/I type of this value.
    pub fn pli_type(&self) -> PliType {
        match self {
            PliValue::FixedDecimal { precision, scale, .. } => PliType::FixedDecimal(*precision, *scale),
            PliValue::FixedBinary(n) => {
                let p = if *n >= i8::MIN as i64 && *n <= i8::MAX as i64 {
                    7
                } else if *n >= i16::MIN as i64 && *n <= i16::MAX as i64 {
                    15
                } else if *n >= i32::MIN as i64 && *n <= i32::MAX as i64 {
                    31
                } else {
                    63
                };
                PliType::FixedBinary(p, 0)
            }
            PliValue::FloatDecimal(_) => PliType::FloatDecimal(16),
            PliValue::FloatBinary(_) => PliType::FloatBinary(53),
            PliValue::Character(s) => PliType::Character(s.len() as u32),
            PliValue::Bit(bits) => PliType::Bit(bits.len() as u32),
            PliValue::Graphic(g) => PliType::Graphic(g.len() as u32),
            PliValue::Pointer(_) => PliType::Pointer,
            PliValue::Offset(_) => PliType::Offset,
            PliValue::Label(_) => PliType::Label,
            PliValue::Entry(_) => PliType::Entry,
            PliValue::File(_) => PliType::File,
            PliValue::Null => PliType::Pointer,
        }
    }

    /// Convert this value to an f64 (for arithmetic operations).
    pub fn to_f64(&self) -> Option<f64> {
        match self {
            PliValue::FixedDecimal { raw, scale, .. } => {
                let divisor = 10_f64.powi(*scale as i32);
                Some(*raw as f64 / divisor)
            }
            PliValue::FixedBinary(n) => Some(*n as f64),
            PliValue::FloatDecimal(f) | PliValue::FloatBinary(f) => Some(*f),
            PliValue::Character(s) => s.trim().parse::<f64>().ok(),
            PliValue::Bit(bits) => {
                let mut val: u64 = 0;
                for b in bits {
                    val = (val << 1) | (*b as u64);
                }
                Some(val as f64)
            }
            _ => None,
        }
    }

    /// Convert this value to an i64 (for integer operations).
    pub fn to_i64(&self) -> Option<i64> {
        self.to_f64().map(|f| f as i64)
    }

    /// Convert this value to a string representation.
    pub fn to_string_value(&self) -> String {
        match self {
            PliValue::FixedDecimal { raw, scale, .. } => {
                if *scale == 0 {
                    raw.to_string()
                } else {
                    let s = raw.abs().to_string();
                    let sc = *scale as usize;
                    let sign = if *raw < 0 { "-" } else { "" };
                    if s.len() <= sc {
                        let zeros = "0".repeat(sc - s.len());
                        format!("{sign}0.{zeros}{s}")
                    } else {
                        let (int_part, frac_part) = s.split_at(s.len() - sc);
                        format!("{sign}{int_part}.{frac_part}")
                    }
                }
            }
            PliValue::FixedBinary(n) => n.to_string(),
            PliValue::FloatDecimal(f) | PliValue::FloatBinary(f) => format!("{f:E}"),
            PliValue::Character(s) => s.clone(),
            PliValue::Bit(bits) => bits.iter().map(|b| if *b { '1' } else { '0' }).collect(),
            PliValue::Graphic(g) => g.iter().map(|c| char::from(*c as u8)).collect(),
            PliValue::Pointer(addr) => format!("{addr:016X}"),
            PliValue::Offset(off) => format!("{off:016X}"),
            PliValue::Label(s) | PliValue::Entry(s) | PliValue::File(s) => s.clone(),
            PliValue::Null => "NULL".to_string(),
        }
    }

    /// Convert this value to a bit string.
    pub fn to_bit_value(&self) -> Vec<bool> {
        match self {
            PliValue::Bit(bits) => bits.clone(),
            PliValue::Character(s) => {
                // Each character → 8 bits (EBCDIC/ASCII).
                let mut bits = Vec::new();
                for byte in s.bytes() {
                    for i in (0..8).rev() {
                        bits.push((byte >> i) & 1 == 1);
                    }
                }
                bits
            }
            PliValue::FixedBinary(n) => {
                let mut bits = Vec::new();
                for i in (0..64).rev() {
                    bits.push((*n >> i) & 1 == 1);
                }
                // Trim leading zeros.
                while bits.len() > 1 && !bits[0] {
                    bits.remove(0);
                }
                bits
            }
            _ => {
                // Convert through string.
                let s = self.to_string_value();
                let mut bits = Vec::new();
                for byte in s.bytes() {
                    for i in (0..8).rev() {
                        bits.push((byte >> i) & 1 == 1);
                    }
                }
                bits
            }
        }
    }
}

impl fmt::Display for PliValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string_value())
    }
}

// ---------------------------------------------------------------------------
//  Type conversion engine
// ---------------------------------------------------------------------------

/// Errors during type conversion.
#[derive(Debug, Clone, thiserror::Error)]
pub enum ConversionError {
    #[error("cannot convert {from} to {to}")]
    Incompatible { from: String, to: String },

    #[error("conversion overflow: {0}")]
    Overflow(String),

    #[error("invalid character for numeric conversion: '{0}'")]
    InvalidCharacter(String),
}

/// Convert a PliValue to match a target PliType.
pub fn convert_value(value: &PliValue, target: &PliType) -> Result<PliValue, ConversionError> {
    let src_type = value.pli_type();
    if src_type == *target {
        return Ok(value.clone());
    }

    if !src_type.can_convert_to(target) {
        return Err(ConversionError::Incompatible {
            from: src_type.to_string(),
            to: target.to_string(),
        });
    }

    match target {
        // ─── Arithmetic targets ───
        PliType::FixedDecimal(p, s) => {
            let f = value.to_f64().ok_or_else(|| ConversionError::Incompatible {
                from: src_type.to_string(),
                to: target.to_string(),
            })?;
            let scale_factor = 10_f64.powi(*s as i32);
            let raw = (f * scale_factor).round() as i128;
            // Check precision.
            let max = 10_i128.pow(*p as u32) - 1;
            if raw.abs() > max {
                return Err(ConversionError::Overflow(format!(
                    "value {} exceeds FIXED DEC({p},{s})",
                    f
                )));
            }
            Ok(PliValue::FixedDecimal {
                raw,
                precision: *p,
                scale: *s,
            })
        }
        PliType::FixedBinary(p, _) => {
            let f = value.to_f64().ok_or_else(|| ConversionError::Incompatible {
                from: src_type.to_string(),
                to: target.to_string(),
            })?;
            let n = f as i64;
            let max = if *p >= 63 {
                i64::MAX
            } else {
                (1_i64 << *p) - 1
            };
            let min = if *p >= 63 {
                i64::MIN
            } else {
                -(1_i64 << *p)
            };
            if n > max || n < min {
                return Err(ConversionError::Overflow(format!(
                    "value {} exceeds FIXED BIN({p})",
                    n
                )));
            }
            Ok(PliValue::FixedBinary(n))
        }
        PliType::FloatDecimal(_) => {
            let f = value.to_f64().ok_or_else(|| ConversionError::Incompatible {
                from: src_type.to_string(),
                to: target.to_string(),
            })?;
            Ok(PliValue::FloatDecimal(f))
        }
        PliType::FloatBinary(_) => {
            let f = value.to_f64().ok_or_else(|| ConversionError::Incompatible {
                from: src_type.to_string(),
                to: target.to_string(),
            })?;
            Ok(PliValue::FloatBinary(f))
        }

        // ─── String targets ───
        PliType::Character(max_len) => {
            let s = value.to_string_value();
            let result = if s.len() > *max_len as usize {
                s[..*max_len as usize].to_string()
            } else {
                format!("{:<width$}", s, width = *max_len as usize)
            };
            Ok(PliValue::Character(result))
        }
        PliType::CharacterVarying(max_len) => {
            let s = value.to_string_value();
            let result = if s.len() > *max_len as usize {
                s[..*max_len as usize].to_string()
            } else {
                s
            };
            Ok(PliValue::Character(result))
        }

        // ─── Bit targets ───
        PliType::Bit(len) => {
            let mut bits = value.to_bit_value();
            bits.resize(*len as usize, false);
            Ok(PliValue::Bit(bits))
        }
        PliType::BitVarying(max_len) => {
            let mut bits = value.to_bit_value();
            if bits.len() > *max_len as usize {
                bits.truncate(*max_len as usize);
            }
            Ok(PliValue::Bit(bits))
        }

        // ─── Graphic targets ───
        PliType::Graphic(len) | PliType::Widechar(len) => {
            let s = value.to_string_value();
            let mut result: Vec<u16> = s.encode_utf16().collect();
            result.resize(*len as usize, 0x0020); // pad with spaces
            Ok(PliValue::Graphic(result))
        }

        // Non-convertible targets.
        _ => Err(ConversionError::Incompatible {
            from: src_type.to_string(),
            to: target.to_string(),
        }),
    }
}

/// Determine the result type of a binary arithmetic operation.
///
/// PL/I rules:
/// - If either operand is FLOAT, result is FLOAT
/// - If either operand is BINARY, result is BINARY
/// - DECIMAL + DECIMAL = DECIMAL
/// - Precision is max(p1, p2) + 1 for addition/subtraction
pub fn arithmetic_result_type(left: &PliType, right: &PliType) -> PliType {
    match (left, right) {
        // Both FIXED DECIMAL.
        (PliType::FixedDecimal(p1, s1), PliType::FixedDecimal(p2, s2)) => {
            let s = (*s1).max(*s2);
            let p = ((*p1 - *s1).max(*p2 - *s2) + s + 1).min(31);
            PliType::FixedDecimal(p, s)
        }
        // Both FIXED BINARY.
        (PliType::FixedBinary(p1, _), PliType::FixedBinary(p2, _)) => {
            let p = ((*p1).max(*p2) + 1).min(63);
            PliType::FixedBinary(p, 0)
        }
        // FIXED DEC + FIXED BIN → FIXED BIN (binary wins).
        (PliType::FixedDecimal(..), PliType::FixedBinary(p, _))
        | (PliType::FixedBinary(p, _), PliType::FixedDecimal(..)) => {
            PliType::FixedBinary((*p + 1).min(63), 0)
        }
        // Any FLOAT → result is FLOAT.
        (PliType::FloatDecimal(p1), PliType::FloatDecimal(p2)) => {
            PliType::FloatDecimal((*p1).max(*p2))
        }
        (PliType::FloatBinary(p1), PliType::FloatBinary(p2)) => {
            PliType::FloatBinary((*p1).max(*p2))
        }
        (PliType::FloatDecimal(p), _) | (_, PliType::FloatDecimal(p)) => {
            PliType::FloatDecimal(*p)
        }
        (PliType::FloatBinary(p), _) | (_, PliType::FloatBinary(p)) => {
            PliType::FloatBinary(*p)
        }
        // Fallback: FLOAT DEC(16).
        _ => PliType::FloatDecimal(16),
    }
}

/// Determine the result type of a string concatenation.
pub fn concat_result_type(left: &PliType, right: &PliType) -> PliType {
    match (left, right) {
        (PliType::Character(l1), PliType::Character(l2))
        | (PliType::Character(l1), PliType::CharacterVarying(l2))
        | (PliType::CharacterVarying(l1), PliType::Character(l2))
        | (PliType::CharacterVarying(l1), PliType::CharacterVarying(l2)) => {
            PliType::CharacterVarying(l1 + l2)
        }
        (PliType::Bit(l1), PliType::Bit(l2))
        | (PliType::Bit(l1), PliType::BitVarying(l2))
        | (PliType::BitVarying(l1), PliType::Bit(l2))
        | (PliType::BitVarying(l1), PliType::BitVarying(l2)) => {
            PliType::BitVarying(l1 + l2)
        }
        // Mixed char and bit → char.
        _ => {
            let l1 = match left {
                PliType::Character(n) | PliType::CharacterVarying(n) => *n,
                PliType::Bit(n) | PliType::BitVarying(n) => (*n + 7) / 8,
                _ => 0,
            };
            let l2 = match right {
                PliType::Character(n) | PliType::CharacterVarying(n) => *n,
                PliType::Bit(n) | PliType::BitVarying(n) => (*n + 7) / 8,
                _ => 0,
            };
            PliType::CharacterVarying(l1 + l2)
        }
    }
}

/// Determine the result type of a comparison operation.
///
/// PL/I compares by converting both sides to a common type:
/// - Two arithmetic: use arithmetic_result_type
/// - Two strings: compare as strings (padded to same length)
/// - Mixed arithmetic+string: convert string to arithmetic
pub fn comparison_common_type(left: &PliType, right: &PliType) -> PliType {
    let lcat = left.category();
    let rcat = right.category();

    match (lcat, rcat) {
        (TypeCategory::Arithmetic, TypeCategory::Arithmetic) => {
            arithmetic_result_type(left, right)
        }
        (TypeCategory::String, TypeCategory::String) => {
            // Pad to max length.
            let l1 = match left {
                PliType::Character(n) | PliType::CharacterVarying(n) => *n,
                _ => 0,
            };
            let l2 = match right {
                PliType::Character(n) | PliType::CharacterVarying(n) => *n,
                _ => 0,
            };
            PliType::Character(l1.max(l2))
        }
        (TypeCategory::Bit, TypeCategory::Bit) => {
            let l1 = match left {
                PliType::Bit(n) | PliType::BitVarying(n) => *n,
                _ => 0,
            };
            let l2 = match right {
                PliType::Bit(n) | PliType::BitVarying(n) => *n,
                _ => 0,
            };
            PliType::Bit(l1.max(l2))
        }
        // Mixed: convert to arithmetic.
        (TypeCategory::Arithmetic, _) | (_, TypeCategory::Arithmetic) => {
            PliType::FloatDecimal(16)
        }
        _ => PliType::Character(256),
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // ─── P101.1: Arithmetic types ───

    #[test]
    fn test_fixed_decimal_storage() {
        let val = PliValue::FixedDecimal {
            raw: 1234567,
            precision: 7,
            scale: 2,
        };
        assert_eq!(val.to_f64(), Some(12345.67));
        assert_eq!(val.to_string_value(), "12345.67");
    }

    #[test]
    fn test_fixed_decimal_no_scale() {
        let val = PliValue::FixedDecimal {
            raw: 42,
            precision: 5,
            scale: 0,
        };
        assert_eq!(val.to_f64(), Some(42.0));
        assert_eq!(val.to_string_value(), "42");
    }

    #[test]
    fn test_fixed_decimal_leading_zero() {
        let val = PliValue::FixedDecimal {
            raw: 5,
            precision: 5,
            scale: 3,
        };
        assert_eq!(val.to_string_value(), "0.005");
    }

    #[test]
    fn test_fixed_binary_storage() {
        let val = PliValue::FixedBinary(2147483647);
        assert_eq!(val.to_f64(), Some(2147483647.0));
        let t = val.pli_type();
        assert_eq!(t, PliType::FixedBinary(31, 0));
    }

    #[test]
    fn test_float_decimal() {
        let val = PliValue::FloatDecimal(3.14159265358979);
        assert!(val.to_f64().unwrap() - 3.14159265358979 < 1e-14);
    }

    #[test]
    fn test_float_binary() {
        let val = PliValue::FloatBinary(1.5);
        assert_eq!(val.to_f64(), Some(1.5));
        assert_eq!(val.pli_type(), PliType::FloatBinary(53));
    }

    #[test]
    fn test_fixed_decimal_type_size() {
        // FIXED DEC(7,2): (7+2)/2 = 4 bytes (packed decimal)
        assert_eq!(PliType::FixedDecimal(7, 2).storage_size(), 4);
        // FIXED DEC(15,0): (15+2)/2 = 8 bytes
        assert_eq!(PliType::FixedDecimal(15, 0).storage_size(), 8);
    }

    #[test]
    fn test_fixed_binary_type_size() {
        assert_eq!(PliType::FixedBinary(7, 0).storage_size(), 1);
        assert_eq!(PliType::FixedBinary(15, 0).storage_size(), 2);
        assert_eq!(PliType::FixedBinary(31, 0).storage_size(), 4);
        assert_eq!(PliType::FixedBinary(63, 0).storage_size(), 8);
    }

    // ─── P101.2: String types ───

    #[test]
    fn test_character_string() {
        let val = PliValue::Character("John".to_string());
        assert_eq!(val.to_string_value(), "John");
        assert_eq!(val.pli_type(), PliType::Character(4));
    }

    #[test]
    fn test_bit_string() {
        let val = PliValue::Bit(vec![true, false, true, true, false, false, false, false]);
        assert_eq!(val.to_string_value(), "10110000");
        assert_eq!(val.pli_type(), PliType::Bit(8));
    }

    #[test]
    fn test_bit_to_numeric() {
        let val = PliValue::Bit(vec![true, false, true, true]);
        assert_eq!(val.to_f64(), Some(11.0)); // 1011 = 11
    }

    #[test]
    fn test_character_type_size() {
        assert_eq!(PliType::Character(30).storage_size(), 30);
        assert_eq!(PliType::CharacterVarying(100).storage_size(), 100);
    }

    #[test]
    fn test_bit_type_size() {
        assert_eq!(PliType::Bit(8).storage_size(), 1);
        assert_eq!(PliType::Bit(1).storage_size(), 1);
        assert_eq!(PliType::Bit(16).storage_size(), 2);
    }

    // ─── P101.3: Pointer and special types ───

    #[test]
    fn test_pointer_value() {
        let val = PliValue::Pointer(0x00007FFF_00010000);
        assert_eq!(val.pli_type(), PliType::Pointer);
        assert_eq!(PliType::Pointer.storage_size(), 8);
    }

    #[test]
    fn test_area_type_size() {
        assert_eq!(PliType::Area(10000).storage_size(), 10000);
    }

    #[test]
    fn test_null_value() {
        let val = PliValue::Null;
        assert_eq!(val.to_string_value(), "NULL");
    }

    #[test]
    fn test_label_value() {
        let val = PliValue::Label("LOOP_START".to_string());
        assert_eq!(val.to_string_value(), "LOOP_START");
        assert_eq!(val.pli_type(), PliType::Label);
    }

    #[test]
    fn test_file_value() {
        let val = PliValue::File("SYSPRINT".to_string());
        assert_eq!(val.pli_type(), PliType::File);
    }

    #[test]
    fn test_type_category() {
        assert_eq!(PliType::FixedDecimal(7, 2).category(), TypeCategory::Arithmetic);
        assert_eq!(PliType::FloatBinary(53).category(), TypeCategory::Arithmetic);
        assert_eq!(PliType::Character(30).category(), TypeCategory::String);
        assert_eq!(PliType::Bit(8).category(), TypeCategory::Bit);
        assert_eq!(PliType::Pointer.category(), TypeCategory::Pointer);
        assert_eq!(PliType::Area(1000).category(), TypeCategory::Area);
        assert_eq!(PliType::Label.category(), TypeCategory::Program);
    }

    // ─── P101.4: Implicit type conversion rules ───

    #[test]
    fn test_can_convert_arithmetic_to_arithmetic() {
        assert!(PliType::FixedDecimal(7, 2).can_convert_to(&PliType::FixedBinary(31, 0)));
        assert!(PliType::FixedBinary(15, 0).can_convert_to(&PliType::FloatDecimal(16)));
        assert!(PliType::FloatDecimal(6).can_convert_to(&PliType::FixedDecimal(15, 0)));
    }

    #[test]
    fn test_can_convert_arithmetic_to_string() {
        assert!(PliType::FixedDecimal(5, 0).can_convert_to(&PliType::Character(10)));
        assert!(PliType::Character(10).can_convert_to(&PliType::FixedDecimal(5, 0)));
    }

    #[test]
    fn test_can_convert_arithmetic_to_bit() {
        assert!(PliType::FixedBinary(31, 0).can_convert_to(&PliType::Bit(32)));
        assert!(PliType::Bit(8).can_convert_to(&PliType::FixedBinary(15, 0)));
    }

    #[test]
    fn test_can_convert_string_bit() {
        assert!(PliType::Character(8).can_convert_to(&PliType::Bit(64)));
        assert!(PliType::Bit(8).can_convert_to(&PliType::Character(1)));
    }

    #[test]
    fn test_cannot_convert_pointer_to_arithmetic() {
        assert!(!PliType::Pointer.can_convert_to(&PliType::FixedBinary(31, 0)));
    }

    #[test]
    fn test_convert_fixed_dec_to_fixed_bin() {
        let val = PliValue::FixedDecimal {
            raw: 100,
            precision: 5,
            scale: 0,
        };
        let result = convert_value(&val, &PliType::FixedBinary(31, 0)).unwrap();
        assert_eq!(result, PliValue::FixedBinary(100));
    }

    #[test]
    fn test_convert_fixed_bin_to_fixed_dec() {
        let val = PliValue::FixedBinary(42);
        let result = convert_value(&val, &PliType::FixedDecimal(5, 0)).unwrap();
        if let PliValue::FixedDecimal { raw, precision, scale } = result {
            assert_eq!(raw, 42);
            assert_eq!(precision, 5);
            assert_eq!(scale, 0);
        } else {
            panic!("Expected FixedDecimal");
        }
    }

    #[test]
    fn test_convert_numeric_to_character() {
        let val = PliValue::FixedDecimal {
            raw: 42,
            precision: 5,
            scale: 0,
        };
        let result = convert_value(&val, &PliType::Character(10)).unwrap();
        if let PliValue::Character(s) = result {
            assert_eq!(s.len(), 10);
            assert!(s.starts_with("42"));
        } else {
            panic!("Expected Character");
        }
    }

    #[test]
    fn test_convert_character_to_numeric() {
        let val = PliValue::Character("  123  ".to_string());
        let result = convert_value(&val, &PliType::FixedDecimal(5, 0)).unwrap();
        if let PliValue::FixedDecimal { raw, .. } = result {
            assert_eq!(raw, 123);
        } else {
            panic!("Expected FixedDecimal");
        }
    }

    #[test]
    fn test_convert_to_bit() {
        let val = PliValue::FixedBinary(5); // 101 in binary
        let result = convert_value(&val, &PliType::Bit(8)).unwrap();
        if let PliValue::Bit(bits) = result {
            assert_eq!(bits.len(), 8);
            // Should be 101 followed by zeros.
            assert!(bits[0]); // 1
            assert!(!bits[1]); // 0
            assert!(bits[2]); // 1
        } else {
            panic!("Expected Bit");
        }
    }

    #[test]
    fn test_convert_overflow() {
        let val = PliValue::FixedBinary(100000);
        let result = convert_value(&val, &PliType::FixedDecimal(3, 0));
        assert!(result.is_err());
    }

    #[test]
    fn test_convert_incompatible() {
        let val = PliValue::Pointer(0x1234);
        let result = convert_value(&val, &PliType::FixedDecimal(5, 0));
        assert!(result.is_err());
    }

    // ─── Arithmetic result type rules ───

    #[test]
    fn test_arithmetic_result_fixed_dec_plus_fixed_dec() {
        let result = arithmetic_result_type(
            &PliType::FixedDecimal(5, 2),
            &PliType::FixedDecimal(7, 3),
        );
        // max(5-2, 7-3) + max(2,3) + 1 = 4 + 3 + 1 = 8
        assert!(matches!(result, PliType::FixedDecimal(8, 3)));
    }

    #[test]
    fn test_arithmetic_result_fixed_bin_plus_fixed_bin() {
        let result = arithmetic_result_type(
            &PliType::FixedBinary(15, 0),
            &PliType::FixedBinary(31, 0),
        );
        assert!(matches!(result, PliType::FixedBinary(32, 0)));
    }

    #[test]
    fn test_arithmetic_result_float_wins() {
        let result = arithmetic_result_type(
            &PliType::FixedDecimal(7, 2),
            &PliType::FloatDecimal(6),
        );
        assert!(matches!(result, PliType::FloatDecimal(6)));
    }

    #[test]
    fn test_concat_result_type() {
        let result = concat_result_type(
            &PliType::Character(10),
            &PliType::Character(20),
        );
        assert_eq!(result, PliType::CharacterVarying(30));
    }

    #[test]
    fn test_display_types() {
        assert_eq!(format!("{}", PliType::FixedDecimal(7, 2)), "FIXED DECIMAL(7,2)");
        assert_eq!(format!("{}", PliType::Character(30)), "CHARACTER(30)");
        assert_eq!(format!("{}", PliType::Pointer), "POINTER");
        assert_eq!(format!("{}", PliType::Area(10000)), "AREA(10000)");
    }

    #[test]
    fn test_structure_member() {
        let rec = PliType::Structure(vec![
            StructureMember {
                level: 2,
                name: "NAME".to_string(),
                pli_type: PliType::CharacterVarying(30),
            },
            StructureMember {
                level: 2,
                name: "AGE".to_string(),
                pli_type: PliType::FixedBinary(31, 0),
            },
        ]);
        assert_eq!(rec.category(), TypeCategory::Aggregate);
        // Storage = 30 + 4 = 34
        assert_eq!(rec.storage_size(), 34);
    }
}

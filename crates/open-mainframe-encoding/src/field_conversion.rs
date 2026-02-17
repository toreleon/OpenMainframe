//! PIC-clause-driven field conversion pipeline.
//!
//! Maps COBOL PIC clauses and USAGE types to the appropriate encoder/decoder,
//! enabling automatic conversion of entire records based on copybook definitions.

/// COBOL USAGE clause values that determine storage format.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CobolUsage {
    /// DISPLAY (default) — zoned decimal for numeric, EBCDIC for alphanumeric
    Display,
    /// COMP / COMP-4 / BINARY — big-endian binary integer
    Comp,
    /// COMP-3 / PACKED-DECIMAL — BCD packed decimal
    Comp3,
    /// COMP-5 — native binary (platform byte order, but we use big-endian for z/OS)
    Comp5,
    /// COMP-1 — IBM HFP short (4 bytes)
    Comp1,
    /// COMP-2 — IBM HFP long (8 bytes)
    Comp2,
    /// FLOAT-SHORT — IEEE 754 single precision (4 bytes)
    FloatShort,
    /// FLOAT-LONG — IEEE 754 double precision (8 bytes)
    FloatLong,
    /// NATIONAL — UTF-16 big-endian (PIC N)
    National,
}

/// The type of encoder/decoder for a field.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FieldEncoder {
    /// EBCDIC alphanumeric (PIC X). Length in bytes.
    Alphanumeric { length: usize },
    /// Zoned decimal (PIC 9 DISPLAY). Digits, decimal places, signed.
    ZonedDecimal {
        digits: usize,
        decimal: usize,
        signed: bool,
    },
    /// Packed decimal (COMP-3). Digits, decimal places, signed.
    PackedDecimal {
        digits: usize,
        decimal: usize,
        signed: bool,
    },
    /// Binary integer (COMP/COMP-4). Storage size in bytes, signed.
    Binary { size: usize, signed: bool },
    /// Native binary (COMP-5). Storage size in bytes, signed.
    NativeBinary { size: usize, signed: bool },
    /// IBM HFP short (COMP-1, 4 bytes).
    HfpShort,
    /// IBM HFP long (COMP-2, 8 bytes).
    HfpLong,
    /// IEEE 754 single precision (FLOAT-SHORT, 4 bytes).
    IeeeShort,
    /// IEEE 754 double precision (FLOAT-LONG, 8 bytes).
    IeeeLong,
    /// NATIONAL (PIC N, UTF-16 big-endian). Character count.
    National { char_count: usize },
}

impl FieldEncoder {
    /// Get the storage size in bytes for this field encoder.
    pub fn byte_length(&self) -> usize {
        match self {
            FieldEncoder::Alphanumeric { length } => *length,
            FieldEncoder::ZonedDecimal { digits, .. } => *digits,
            FieldEncoder::PackedDecimal { digits, .. } => (digits + 2) / 2,
            FieldEncoder::Binary { size, .. } => *size,
            FieldEncoder::NativeBinary { size, .. } => *size,
            FieldEncoder::HfpShort => 4,
            FieldEncoder::HfpLong => 8,
            FieldEncoder::IeeeShort => 4,
            FieldEncoder::IeeeLong => 8,
            FieldEncoder::National { char_count } => char_count * 2,
        }
    }
}

/// Parsed PIC clause information.
#[derive(Debug, Clone)]
pub struct PicClause {
    /// Total digit count (integer + decimal).
    pub digits: usize,
    /// Decimal places (after V).
    pub decimal: usize,
    /// Whether the field is signed (S prefix).
    pub signed: bool,
    /// Whether it's alphanumeric (X) vs numeric (9).
    pub is_alpha: bool,
    /// Whether it's NATIONAL (N).
    pub is_national: bool,
}

/// Parse a simplified PIC clause string.
///
/// Handles forms like:
/// - `X(30)` — alphanumeric, 30 bytes
/// - `S9(5)V99` — signed numeric, 5 integer digits + 2 decimal
/// - `9(4)` — unsigned numeric, 4 digits
/// - `N(10)` — national, 10 characters
/// - `S9(9)` — signed numeric, 9 digits
///
/// Does NOT handle the full COBOL PIC clause grammar (e.g., Z, *, $, etc.)
/// but covers the data-storage forms used in WORKING-STORAGE and FILE SECTION.
pub fn parse_pic(pic: &str) -> PicClause {
    let s = pic.trim().to_uppercase();
    let mut signed = false;
    let mut is_alpha = false;
    let mut is_national = false;
    let mut digits = 0usize;
    let mut decimal = 0usize;
    let mut after_v = false;

    let chars: Vec<char> = s.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        match chars[i] {
            'S' if i == 0 => {
                signed = true;
                i += 1;
            }
            'X' => {
                is_alpha = true;
                let count = read_repeat(&chars, &mut i);
                digits += count;
            }
            'N' => {
                is_national = true;
                let count = read_repeat(&chars, &mut i);
                digits += count;
            }
            '9' => {
                let count = read_repeat(&chars, &mut i);
                if after_v {
                    decimal += count;
                } else {
                    digits += count;
                }
            }
            'V' => {
                after_v = true;
                i += 1;
            }
            _ => {
                i += 1;
            }
        }
    }

    PicClause {
        digits: digits + decimal,
        decimal,
        signed,
        is_alpha,
        is_national,
    }
}

/// Read a character and optional repeat count like `9(5)` or `X(30)`.
/// Returns the repeat count (1 if no parentheses).
fn read_repeat(chars: &[char], i: &mut usize) -> usize {
    *i += 1; // consume the character (X, 9, N)
    if *i < chars.len() && chars[*i] == '(' {
        *i += 1; // consume '('
        let start = *i;
        while *i < chars.len() && chars[*i] != ')' {
            *i += 1;
        }
        let num_str: String = chars[start..*i].iter().collect();
        if *i < chars.len() {
            *i += 1; // consume ')'
        }
        num_str.parse().unwrap_or(1)
    } else {
        1
    }
}

/// Determine the field encoder from a PIC clause and USAGE.
///
/// # Examples
/// - `PIC S9(5)V99 COMP-3` → `PackedDecimal { digits: 7, decimal: 2, signed: true }`
/// - `PIC X(30)` → `Alphanumeric { length: 30 }`
/// - `PIC S9(9) COMP-5` → `NativeBinary { size: 4, signed: true }` (fullword)
pub fn resolve_encoder(pic: &PicClause, usage: &CobolUsage) -> FieldEncoder {
    match usage {
        CobolUsage::Display if pic.is_alpha => FieldEncoder::Alphanumeric {
            length: pic.digits,
        },
        CobolUsage::Display if pic.is_national => FieldEncoder::National {
            char_count: pic.digits,
        },
        CobolUsage::Display => FieldEncoder::ZonedDecimal {
            digits: pic.digits,
            decimal: pic.decimal,
            signed: pic.signed,
        },
        CobolUsage::Comp3 => FieldEncoder::PackedDecimal {
            digits: pic.digits,
            decimal: pic.decimal,
            signed: pic.signed,
        },
        CobolUsage::Comp | CobolUsage::Comp5 => {
            let size = binary_size_for_digits(pic.digits);
            if matches!(usage, CobolUsage::Comp5) {
                FieldEncoder::NativeBinary {
                    size,
                    signed: pic.signed,
                }
            } else {
                FieldEncoder::Binary {
                    size,
                    signed: pic.signed,
                }
            }
        }
        CobolUsage::Comp1 => FieldEncoder::HfpShort,
        CobolUsage::Comp2 => FieldEncoder::HfpLong,
        CobolUsage::FloatShort => FieldEncoder::IeeeShort,
        CobolUsage::FloatLong => FieldEncoder::IeeeLong,
        CobolUsage::National => FieldEncoder::National {
            char_count: pic.digits,
        },
    }
}

/// Determine binary storage size from digit count (COBOL rules).
fn binary_size_for_digits(digits: usize) -> usize {
    match digits {
        0..=4 => 2,  // halfword
        5..=9 => 4,  // fullword
        _ => 8,      // doubleword
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // --- Epic 706: PIC Clause to Encoder Mapping ---

    #[test]
    fn test_parse_pic_packed_decimal() {
        // AC: Given PIC S9(5)V99 COMP-3
        // When the field definition is parsed
        // Then a packed decimal encoder with 5 integer digits and 2 decimal digits is returned
        let pic = parse_pic("S9(5)V99");
        assert!(pic.signed);
        assert!(!pic.is_alpha);
        assert_eq!(pic.digits, 7); // 5 + 2
        assert_eq!(pic.decimal, 2);

        let encoder = resolve_encoder(&pic, &CobolUsage::Comp3);
        assert_eq!(
            encoder,
            FieldEncoder::PackedDecimal {
                digits: 7,
                decimal: 2,
                signed: true,
            }
        );
    }

    #[test]
    fn test_parse_pic_alphanumeric() {
        // AC: Given PIC X(30)
        // When the field definition is parsed
        // Then an EBCDIC alphanumeric encoder with length 30 is returned
        let pic = parse_pic("X(30)");
        assert!(pic.is_alpha);
        assert_eq!(pic.digits, 30);

        let encoder = resolve_encoder(&pic, &CobolUsage::Display);
        assert_eq!(encoder, FieldEncoder::Alphanumeric { length: 30 });
    }

    #[test]
    fn test_parse_pic_native_binary() {
        // AC: Given PIC S9(9) COMP-5
        // When the field definition is parsed
        // Then a native binary encoder (fullword) is returned
        let pic = parse_pic("S9(9)");
        assert!(pic.signed);
        assert_eq!(pic.digits, 9);

        let encoder = resolve_encoder(&pic, &CobolUsage::Comp5);
        assert_eq!(
            encoder,
            FieldEncoder::NativeBinary {
                size: 4,
                signed: true,
            }
        );
    }

    #[test]
    fn test_parse_pic_binary_halfword() {
        let pic = parse_pic("S9(4)");
        let encoder = resolve_encoder(&pic, &CobolUsage::Comp);
        assert_eq!(
            encoder,
            FieldEncoder::Binary {
                size: 2,
                signed: true,
            }
        );
    }

    #[test]
    fn test_parse_pic_binary_doubleword() {
        let pic = parse_pic("S9(18)");
        let encoder = resolve_encoder(&pic, &CobolUsage::Comp);
        assert_eq!(
            encoder,
            FieldEncoder::Binary {
                size: 8,
                signed: true,
            }
        );
    }

    #[test]
    fn test_parse_pic_unsigned() {
        let pic = parse_pic("9(4)");
        assert!(!pic.signed);
        assert_eq!(pic.digits, 4);
    }

    #[test]
    fn test_parse_pic_zoned_decimal() {
        let pic = parse_pic("9(5)V99");
        let encoder = resolve_encoder(&pic, &CobolUsage::Display);
        assert_eq!(
            encoder,
            FieldEncoder::ZonedDecimal {
                digits: 7,
                decimal: 2,
                signed: false,
            }
        );
    }

    #[test]
    fn test_parse_pic_comp1() {
        let pic = parse_pic("S9(9)");
        let encoder = resolve_encoder(&pic, &CobolUsage::Comp1);
        assert_eq!(encoder, FieldEncoder::HfpShort);
    }

    #[test]
    fn test_parse_pic_comp2() {
        let pic = parse_pic("S9(18)");
        let encoder = resolve_encoder(&pic, &CobolUsage::Comp2);
        assert_eq!(encoder, FieldEncoder::HfpLong);
    }

    #[test]
    fn test_parse_pic_float_short() {
        let pic = parse_pic("S9(9)");
        let encoder = resolve_encoder(&pic, &CobolUsage::FloatShort);
        assert_eq!(encoder, FieldEncoder::IeeeShort);
    }

    #[test]
    fn test_parse_pic_float_long() {
        let pic = parse_pic("S9(18)");
        let encoder = resolve_encoder(&pic, &CobolUsage::FloatLong);
        assert_eq!(encoder, FieldEncoder::IeeeLong);
    }

    #[test]
    fn test_parse_pic_national() {
        let pic = parse_pic("N(10)");
        assert!(pic.is_national);
        assert_eq!(pic.digits, 10);

        let encoder = resolve_encoder(&pic, &CobolUsage::National);
        assert_eq!(encoder, FieldEncoder::National { char_count: 10 });
    }

    #[test]
    fn test_parse_pic_national_display() {
        // PIC N with DISPLAY usage should also resolve to National
        let pic = parse_pic("N(5)");
        let encoder = resolve_encoder(&pic, &CobolUsage::Display);
        assert_eq!(encoder, FieldEncoder::National { char_count: 5 });
    }

    #[test]
    fn test_field_encoder_byte_length() {
        assert_eq!(FieldEncoder::Alphanumeric { length: 30 }.byte_length(), 30);
        assert_eq!(
            FieldEncoder::PackedDecimal {
                digits: 7,
                decimal: 2,
                signed: true
            }
            .byte_length(),
            4 // (7 + 2) / 2 = 4 (rounded down, but +1 for sign nibble = (7+1)/2=4)
        );
        assert_eq!(
            FieldEncoder::ZonedDecimal {
                digits: 5,
                decimal: 0,
                signed: true
            }
            .byte_length(),
            5
        );
        assert_eq!(FieldEncoder::Binary { size: 4, signed: true }.byte_length(), 4);
        assert_eq!(FieldEncoder::HfpShort.byte_length(), 4);
        assert_eq!(FieldEncoder::HfpLong.byte_length(), 8);
        assert_eq!(FieldEncoder::IeeeShort.byte_length(), 4);
        assert_eq!(FieldEncoder::IeeeLong.byte_length(), 8);
        assert_eq!(FieldEncoder::National { char_count: 10 }.byte_length(), 20);
    }

    #[test]
    fn test_parse_pic_repeated_nines() {
        // PIC 999 (no parentheses)
        let pic = parse_pic("999");
        assert_eq!(pic.digits, 3);
    }

    #[test]
    fn test_parse_pic_case_insensitive() {
        let pic = parse_pic("s9(5)v99");
        assert!(pic.signed);
        assert_eq!(pic.digits, 7);
        assert_eq!(pic.decimal, 2);
    }
}

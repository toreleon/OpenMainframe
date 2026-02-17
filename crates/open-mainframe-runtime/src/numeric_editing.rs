//! COBOL numeric editing per PIC clause patterns.
//!
//! Implements the numeric editing features of the COBOL PICTURE clause:
//! - **Z** — zero suppression (replace leading zeros with spaces)
//! - **\*** — check protection (replace leading zeros with asterisks)
//! - **$** — floating currency symbol
//! - **+** — floating plus sign
//! - **-** — floating minus sign
//! - **CR** — credit symbol (for negative values)
//! - **DB** — debit symbol (for negative values)
//! - **B** — blank insertion
//! - **0** — zero insertion
//! - **,** — comma (grouping separator) insertion
//! - **.** — decimal point position
//!
//! # Example
//!
//! ```
//! use open_mainframe_runtime::numeric_editing::format_numeric;
//! use rust_decimal::Decimal;
//! use std::str::FromStr;
//!
//! let val = Decimal::from_str("1234.56").unwrap();
//! let result = format_numeric(&val, "ZZ,ZZ9.99");
//! assert_eq!(result, " 1,234.56");
//! ```

use rust_decimal::Decimal;

/// Format a numeric value according to a COBOL PIC editing pattern.
///
/// Supports these editing characters:
/// - `9` — always display digit
/// - `Z` — zero-suppress (leading zero → space)
/// - `*` — check-protect (leading zero → asterisk)
/// - `$` — single or floating currency sign
/// - `+` — floating plus sign (show `+` for positive, `-` for negative)
/// - `-` — floating minus sign (show `-` for negative, space for positive)
/// - `CR` — append "CR" if negative, spaces if positive
/// - `DB` — append "DB" if negative, spaces if positive
/// - `B` — insert blank
/// - `0` — insert literal zero
/// - `,` — insert comma (suppressed if all digits to left are suppressed)
/// - `.` — decimal point position
pub fn format_numeric(value: &Decimal, pic: &str) -> String {
    let is_negative = value.is_sign_negative();
    let abs_value = value.abs();

    // Check for trailing CR/DB
    let upper = pic.to_uppercase();
    let (pic_body, suffix) = if upper.ends_with("CR") {
        (&pic[..pic.len() - 2], if is_negative { "CR" } else { "  " })
    } else if upper.ends_with("DB") {
        (&pic[..pic.len() - 2], if is_negative { "DB" } else { "  " })
    } else {
        (pic, "")
    };

    // Parse the picture into integer and decimal portions.
    let (int_pic, dec_pic) = split_at_decimal(pic_body);

    // Determine the editing mode from the picture.
    let mode = detect_editing_mode(&int_pic);

    // Count digit positions. For floating symbols, N floating chars represent
    // N-1 value digits (one position is consumed by the symbol itself).
    let int_positions = count_digit_positions(&int_pic);
    let dec_digit_count = count_digit_positions(&dec_pic);

    let int_value_digits = match mode {
        EditMode::FloatingCurrency | EditMode::FloatingPlus | EditMode::FloatingMinus => {
            // One of the floating positions is for the symbol; the rest are digits.
            int_positions.saturating_sub(1)
        }
        _ => int_positions,
    };

    // Extract digit strings from the absolute value.
    let int_digits = extract_integer_digits(&abs_value, int_value_digits);
    let dec_digits = extract_decimal_digits(&abs_value, dec_digit_count);

    // Format integer portion.
    let int_result = format_integer_part(&int_pic, &int_digits, is_negative, &mode);

    // Format decimal portion (if any).
    let dec_result = if !dec_pic.is_empty() {
        format!(".{}", format_decimal_part(&dec_pic, &dec_digits))
    } else {
        String::new()
    };

    format!("{}{}{}", int_result, dec_result, suffix)
}

/// The editing mode for zero-suppression/replacement.
#[derive(Debug, Clone, Copy, PartialEq)]
enum EditMode {
    /// No suppression — all 9s.
    None,
    /// Z editing — suppress leading zeros with spaces.
    ZeroSuppress,
    /// * editing — suppress leading zeros with asterisks.
    CheckProtect,
    /// Floating $ — the $ floats to just before the first significant digit.
    FloatingCurrency,
    /// Floating + — + or - floats before the first significant digit.
    FloatingPlus,
    /// Floating - — - or space floats before the first significant digit.
    FloatingMinus,
}

/// Detect the editing mode from an integer picture.
fn detect_editing_mode(pic: &str) -> EditMode {
    let upper: Vec<char> = pic.to_uppercase().chars().collect();
    for &ch in &upper {
        match ch {
            'Z' => return EditMode::ZeroSuppress,
            '*' => return EditMode::CheckProtect,
            '$' => {
                if upper.iter().filter(|&&c| c == '$').count() > 1 {
                    return EditMode::FloatingCurrency;
                }
            }
            '+' => {
                if upper.iter().filter(|&&c| c == '+').count() > 1 {
                    return EditMode::FloatingPlus;
                }
            }
            '-' => {
                if upper.iter().filter(|&&c| c == '-').count() > 1 {
                    return EditMode::FloatingMinus;
                }
            }
            _ => {}
        }
    }
    EditMode::None
}

/// Split a picture at the decimal point.
fn split_at_decimal(pic: &str) -> (String, String) {
    if let Some(pos) = pic.find('.') {
        (pic[..pos].to_string(), pic[pos + 1..].to_string())
    } else {
        (pic.to_string(), String::new())
    }
}

/// Count how many digit positions exist in a picture portion.
/// Digit positions are: 9, Z, *, and floating $, +, - characters.
fn count_digit_positions(pic: &str) -> usize {
    pic.to_uppercase()
        .chars()
        .filter(|c| matches!(c, '9' | 'Z' | '*' | '$' | '+' | '-'))
        .count()
}

/// Extract integer digits as a zero-padded vector (left-padded to `width`).
fn extract_integer_digits(value: &Decimal, width: usize) -> Vec<u8> {
    let int_str = value.trunc().abs().to_string();
    let mut digits: Vec<u8> = int_str.bytes().map(|b| b - b'0').collect();

    while digits.len() < width {
        digits.insert(0, 0);
    }
    if digits.len() > width {
        digits = digits[digits.len() - width..].to_vec();
    }
    digits
}

/// Extract decimal digits as a zero-padded vector.
fn extract_decimal_digits(value: &Decimal, width: usize) -> Vec<u8> {
    if width == 0 {
        return vec![];
    }

    let scale = Decimal::from(10i64.pow(width as u32));
    let frac = (value.abs() * scale).trunc() % scale;
    let frac_str = frac.abs().to_string();

    let mut digits: Vec<u8> = frac_str.bytes().map(|b| b - b'0').collect();
    while digits.len() < width {
        digits.insert(0, 0);
    }
    digits
}

/// Format the integer portion of a number given a PIC pattern and digits.
///
/// For non-floating modes (None, ZeroSuppress, CheckProtect), `digits` has
/// the same count as the number of digit positions in `pic`.
///
/// For floating modes (FloatingCurrency, FloatingPlus, FloatingMinus),
/// `digits` has one fewer element than the number of digit positions in `pic`
/// because one floating position is used for the symbol.
fn format_integer_part(
    pic: &str,
    digits: &[u8],
    is_negative: bool,
    mode: &EditMode,
) -> String {
    let pic_chars: Vec<char> = pic.chars().collect();
    let mut result = String::with_capacity(pic_chars.len());

    // For non-floating modes, digit_idx tracks into `digits` directly.
    // For floating modes, we need to figure out where the symbol goes.
    match mode {
        EditMode::None => {
            format_plain(&pic_chars, digits, &mut result);
        }
        EditMode::ZeroSuppress => {
            format_suppress(&pic_chars, digits, ' ', &mut result);
        }
        EditMode::CheckProtect => {
            format_suppress(&pic_chars, digits, '*', &mut result);
        }
        EditMode::FloatingCurrency => {
            format_floating(&pic_chars, digits, is_negative, '$', '$', &mut result);
        }
        EditMode::FloatingPlus => {
            let sym = if is_negative { '-' } else { '+' };
            format_floating(&pic_chars, digits, is_negative, '+', sym, &mut result);
        }
        EditMode::FloatingMinus => {
            let sym = if is_negative { '-' } else { ' ' };
            format_floating(&pic_chars, digits, is_negative, '-', sym, &mut result);
        }
    }

    result
}

/// Format with no suppression (all 9s, plus insertion characters).
fn format_plain(pic_chars: &[char], digits: &[u8], result: &mut String) {
    let mut digit_idx = 0;
    for &ch in pic_chars {
        match ch.to_ascii_uppercase() {
            '9' => {
                result.push(digit_char(digits, digit_idx));
                digit_idx += 1;
            }
            'B' => result.push(' '),
            '0' => result.push('0'),
            ',' => result.push(','),
            '+' => result.push('+'),
            '-' => result.push('-'),
            '$' => result.push('$'),
            _ => result.push(ch),
        }
    }
}

/// Format with zero suppression (Z or *).
fn format_suppress(pic_chars: &[char], digits: &[u8], fill: char, result: &mut String) {
    let mut digit_idx = 0;
    let first_significant = digits.iter().position(|&d| d != 0).unwrap_or(digits.len());

    for &ch in pic_chars {
        let upper = ch.to_ascii_uppercase();
        match upper {
            '9' => {
                result.push(digit_char(digits, digit_idx));
                digit_idx += 1;
            }
            'Z' | '*' => {
                if digit_idx < first_significant {
                    result.push(fill);
                } else {
                    result.push(digit_char(digits, digit_idx));
                }
                digit_idx += 1;
            }
            ',' => {
                // Comma is suppressed if all digits so far are suppressed.
                if digit_idx <= first_significant {
                    result.push(fill);
                } else {
                    result.push(',');
                }
            }
            'B' => result.push(' '),
            '0' => result.push('0'),
            _ => result.push(ch),
        }
    }
}

/// Format with floating symbol ($, +, -).
///
/// `digits` has N-1 elements where N is the number of floating symbol chars
/// plus 9 chars. The floating symbol occupies one position.
///
/// The algorithm:
/// 1. Find first significant digit in `digits`.
/// 2. The symbol is placed at the position just before the first significant digit.
/// 3. Positions before the symbol show the fill char (space).
/// 4. Positions after the symbol show digits.
fn format_floating(
    pic_chars: &[char],
    digits: &[u8],
    _is_negative: bool,
    float_char: char,
    symbol: char,
    result: &mut String,
) {
    let float_char_upper = float_char.to_ascii_uppercase();

    // Count how many floating positions exist.
    let float_count = pic_chars
        .iter()
        .filter(|&&c| c.to_ascii_uppercase() == float_char_upper)
        .count();

    // first_significant is the index in `digits` of the first non-zero.
    let first_significant = digits.iter().position(|&d| d != 0).unwrap_or(digits.len());

    // The symbol goes in the floating position just before the first significant digit.
    // If all digits are zero, the symbol goes in the last floating position.
    // `symbol_float_idx` is the 0-based index among the floating positions.
    let symbol_float_idx = if first_significant >= digits.len() {
        // All zeros — symbol goes in the last floating position.
        float_count - 1
    } else if first_significant == 0 {
        // First digit is significant — symbol goes in the first floating position (0).
        0
    } else {
        // Symbol goes at the floating position corresponding to digit index
        // (first_significant - 1), but offset by the fact that one floating
        // position is consumed by the symbol itself... Actually simpler:
        // the symbol takes the position that would have held the digit at
        // first_significant, but shifted left by 1 to make room.
        // Position = first_significant (since positions 0..first_significant-1
        // are blank, position first_significant is the symbol, and
        // positions first_significant+1.. hold the significant digits).
        // But we cap at float_count - 1.
        first_significant.min(float_count - 1)
    };

    let mut float_idx = 0; // tracks current floating position index
    // After the symbol position, digits should start from the first significant
    // digit, skipping any leading zeros that were suppressed by the floating.
    let mut digit_idx = first_significant;

    for &ch in pic_chars {
        let upper = ch.to_ascii_uppercase();
        if upper == float_char_upper {
            match float_idx.cmp(&symbol_float_idx) {
                std::cmp::Ordering::Less => {
                    // Before the symbol — space fill.
                    result.push(' ');
                }
                std::cmp::Ordering::Equal => {
                    // The symbol position.
                    result.push(symbol);
                }
                std::cmp::Ordering::Greater => {
                    // After the symbol — show digit.
                    result.push(digit_char(digits, digit_idx));
                    digit_idx += 1;
                }
            }
            float_idx += 1;
        } else {
            match upper {
                '9' => {
                    result.push(digit_char(digits, digit_idx));
                    digit_idx += 1;
                }
                ',' => {
                    // Comma is suppressed if we haven't placed the symbol yet.
                    if float_idx <= symbol_float_idx {
                        result.push(' ');
                    } else {
                        result.push(',');
                    }
                }
                'B' => result.push(' '),
                '0' => result.push('0'),
                _ => result.push(ch),
            }
        }
    }
}

/// Get digit character from digits array, or '0' if out of bounds.
fn digit_char(digits: &[u8], idx: usize) -> char {
    if idx < digits.len() {
        (b'0' + digits[idx]) as char
    } else {
        '0'
    }
}

/// Format the decimal portion of a number (after the decimal point).
fn format_decimal_part(pic: &str, digits: &[u8]) -> String {
    let pic_chars: Vec<char> = pic.chars().collect();
    let mut result = String::with_capacity(pic_chars.len());
    let mut digit_idx = 0;

    for &ch in &pic_chars {
        let upper = ch.to_ascii_uppercase();
        match upper {
            '9' | 'Z' | '*' | '$' | '+' | '-' => {
                if digit_idx < digits.len() {
                    result.push((b'0' + digits[digit_idx]) as char);
                } else {
                    result.push('0');
                }
                digit_idx += 1;
            }
            'B' => result.push(' '),
            '0' => result.push('0'),
            _ => result.push(ch),
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn test_basic_9_pattern() {
        let val = Decimal::from_str("1234").unwrap();
        assert_eq!(format_numeric(&val, "9999"), "1234");
    }

    #[test]
    fn test_9_pattern_with_leading_zeros() {
        let val = Decimal::from_str("42").unwrap();
        assert_eq!(format_numeric(&val, "9999"), "0042");
    }

    #[test]
    fn test_z_zero_suppress() {
        let val = Decimal::from_str("42").unwrap();
        assert_eq!(format_numeric(&val, "ZZZ9"), "  42");
    }

    #[test]
    fn test_z_zero_suppress_all_zeros() {
        let val = Decimal::from_str("0").unwrap();
        assert_eq!(format_numeric(&val, "ZZZ9"), "   0");
    }

    #[test]
    fn test_z_with_decimal() {
        let val = Decimal::from_str("1234.56").unwrap();
        assert_eq!(format_numeric(&val, "ZZ,ZZ9.99"), " 1,234.56");
    }

    #[test]
    fn test_z_with_decimal_small_value() {
        let val = Decimal::from_str("42.00").unwrap();
        assert_eq!(format_numeric(&val, "ZZ,ZZ9.99"), "    42.00");
    }

    #[test]
    fn test_asterisk_check_protect() {
        let val = Decimal::from_str("42.00").unwrap();
        assert_eq!(format_numeric(&val, "***,**9.99"), "*****42.00");
    }

    #[test]
    fn test_asterisk_all_nines() {
        let val = Decimal::from_str("123456.78").unwrap();
        assert_eq!(format_numeric(&val, "***,**9.99"), "123,456.78");
    }

    #[test]
    fn test_floating_dollar() {
        // PIC $$,$$$,$$9.99 with 1234.56
        let val = Decimal::from_str("1234.56").unwrap();
        let result = format_numeric(&val, "$$,$$$,$$9.99");
        assert_eq!(result, "    $1,234.56");
    }

    #[test]
    fn test_floating_dollar_negative_with_cr() {
        let val = Decimal::from_str("-1234.56").unwrap();
        let result = format_numeric(&val, "$$,$$$,$$9.99CR");
        assert_eq!(result, "    $1,234.56CR");
    }

    #[test]
    fn test_cr_positive() {
        let val = Decimal::from_str("1234.56").unwrap();
        let result = format_numeric(&val, "ZZ,ZZ9.99CR");
        assert_eq!(result, " 1,234.56  ");
    }

    #[test]
    fn test_cr_negative() {
        let val = Decimal::from_str("-1234.56").unwrap();
        let result = format_numeric(&val, "ZZ,ZZ9.99CR");
        assert_eq!(result, " 1,234.56CR");
    }

    #[test]
    fn test_db_negative() {
        let val = Decimal::from_str("-500").unwrap();
        let result = format_numeric(&val, "ZZ9DB");
        assert_eq!(result, "500DB");
    }

    #[test]
    fn test_db_positive() {
        let val = Decimal::from_str("500").unwrap();
        let result = format_numeric(&val, "ZZ9DB");
        assert_eq!(result, "500  ");
    }

    #[test]
    fn test_floating_plus_positive() {
        let val = Decimal::from_str("123").unwrap();
        let result = format_numeric(&val, "+++9");
        assert_eq!(result, "+123");
    }

    #[test]
    fn test_floating_plus_negative() {
        let val = Decimal::from_str("-123").unwrap();
        let result = format_numeric(&val, "+++9");
        assert_eq!(result, "-123");
    }

    #[test]
    fn test_floating_plus_small() {
        let val = Decimal::from_str("5").unwrap();
        let result = format_numeric(&val, "++++9");
        assert_eq!(result, "   +5");
    }

    #[test]
    fn test_floating_minus_positive() {
        let val = Decimal::from_str("123").unwrap();
        let result = format_numeric(&val, "---9");
        assert_eq!(result, " 123");
    }

    #[test]
    fn test_floating_minus_negative() {
        let val = Decimal::from_str("-123").unwrap();
        let result = format_numeric(&val, "---9");
        assert_eq!(result, "-123");
    }

    #[test]
    fn test_blank_insertion() {
        let val = Decimal::from_str("123456").unwrap();
        let result = format_numeric(&val, "999B999");
        assert_eq!(result, "123 456");
    }

    #[test]
    fn test_zero_insertion() {
        // PIC 990099: 4 digit positions with two literal 0s inserted between.
        // Value 1234 → digits [1,2,3,4] → "12" + "00" + "34" = "120034"
        let val = Decimal::from_str("1234").unwrap();
        let result = format_numeric(&val, "990099");
        assert_eq!(result, "120034");
    }

    #[test]
    fn test_large_number_with_commas() {
        let val = Decimal::from_str("1234567").unwrap();
        let result = format_numeric(&val, "9,999,999");
        assert_eq!(result, "1,234,567");
    }

    #[test]
    fn test_zero_with_asterisks() {
        let val = Decimal::from_str("0").unwrap();
        let result = format_numeric(&val, "***,**9");
        assert_eq!(result, "******0");
    }

    #[test]
    fn test_decimal_only_nines() {
        let val = Decimal::from_str("3.14").unwrap();
        let result = format_numeric(&val, "9.99");
        assert_eq!(result, "3.14");
    }

    #[test]
    fn test_format_zero_value_z() {
        let val = Decimal::ZERO;
        let result = format_numeric(&val, "ZZZZ");
        assert_eq!(result, "    ");
    }

    #[test]
    fn test_floating_dollar_small() {
        let val = Decimal::from_str("5.00").unwrap();
        let result = format_numeric(&val, "$$$,$$9.99");
        assert_eq!(result, "     $5.00");
    }
}

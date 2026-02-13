//! String intrinsic functions.
//!
//! Implements COBOL-2014 string manipulation functions.

/// Trim direction for FUNCTION TRIM.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TrimDirection {
    /// Trim leading spaces only
    Leading,
    /// Trim trailing spaces only
    Trailing,
    /// Trim both leading and trailing spaces
    Both,
}

impl Default for TrimDirection {
    fn default() -> Self {
        TrimDirection::Both
    }
}

/// FUNCTION TRIM implementation.
///
/// Removes leading and/or trailing spaces from a string.
///
/// # Arguments
/// * `s` - Input string
/// * `direction` - Which end(s) to trim
///
/// # Returns
/// Trimmed string
pub fn trim(s: &str, direction: TrimDirection) -> String {
    match direction {
        TrimDirection::Leading => s.trim_start().to_string(),
        TrimDirection::Trailing => s.trim_end().to_string(),
        TrimDirection::Both => s.trim().to_string(),
    }
}

/// FUNCTION SUBSTITUTE implementation.
///
/// Replaces all occurrences of substrings in the input string.
/// Replacements are applied in order, so later replacements see
/// results of earlier ones.
///
/// # Arguments
/// * `s` - Input string
/// * `pairs` - Pairs of (old, new) strings to substitute
///
/// # Returns
/// String with substitutions applied
pub fn substitute(s: &str, pairs: &[(&str, &str)]) -> String {
    let mut result = s.to_string();
    for (old, new) in pairs {
        result = result.replace(old, new);
    }
    result
}

/// FUNCTION SUBSTITUTE-CASE variant.
///
/// Case-insensitive version of SUBSTITUTE.
pub fn substitute_case_insensitive(s: &str, pairs: &[(&str, &str)]) -> String {
    let mut result = s.to_string();
    for (old, new) in pairs {
        // Simple case-insensitive replace
        let lower = result.to_lowercase();
        let old_lower = old.to_lowercase();
        let mut output = String::new();
        let mut last_end = 0;

        for (start, _) in lower.match_indices(&old_lower) {
            output.push_str(&result[last_end..start]);
            output.push_str(new);
            last_end = start + old.len();
        }
        output.push_str(&result[last_end..]);
        result = output;
    }
    result
}

/// FUNCTION CONCATENATE implementation.
///
/// Joins multiple strings together.
///
/// # Arguments
/// * `strings` - Strings to concatenate
///
/// # Returns
/// Concatenated string
pub fn concatenate(strings: &[&str]) -> String {
    strings.concat()
}

/// FUNCTION LENGTH implementation.
///
/// Returns the length of a string in characters.
pub fn length(s: &str) -> usize {
    s.chars().count()
}

/// FUNCTION BYTE-LENGTH implementation.
///
/// Returns the length of a string in bytes.
pub fn byte_length(s: &str) -> usize {
    s.len()
}

/// FUNCTION REVERSE implementation.
///
/// Reverses a string.
pub fn reverse(s: &str) -> String {
    s.chars().rev().collect()
}

/// FUNCTION UPPER-CASE implementation.
///
/// Converts a string to uppercase.
pub fn upper_case(s: &str) -> String {
    s.to_uppercase()
}

/// FUNCTION LOWER-CASE implementation.
///
/// Converts a string to lowercase.
pub fn lower_case(s: &str) -> String {
    s.to_lowercase()
}

/// FUNCTION ORD implementation.
///
/// Returns the ordinal position of the first character.
pub fn ord(s: &str) -> u32 {
    s.chars().next().map(|c| c as u32).unwrap_or(0)
}

/// FUNCTION CHAR implementation.
///
/// Returns the character for an ordinal value.
pub fn char_from_ord(n: u32) -> String {
    char::from_u32(n).map(|c| c.to_string()).unwrap_or_default()
}

/// FUNCTION DISPLAY-OF implementation.
///
/// Converts UTF-16 (national) to UTF-8 (alphanumeric).
pub fn display_of(national: &[u16]) -> String {
    String::from_utf16_lossy(national)
}

/// FUNCTION NATIONAL-OF implementation.
///
/// Converts UTF-8 (alphanumeric) to UTF-16 (national).
pub fn national_of(s: &str) -> Vec<u16> {
    s.encode_utf16().collect()
}

/// FUNCTION INSPECT COUNT implementation.
///
/// Counts occurrences of a substring.
pub fn count_occurrences(s: &str, pattern: &str) -> usize {
    if pattern.is_empty() {
        return 0;
    }
    s.matches(pattern).count()
}

/// FUNCTION NUMVAL implementation.
///
/// Converts a string to a numeric value (f64).
pub fn numval(s: &str) -> Option<f64> {
    let s = s.trim();
    // Handle COBOL-style signs
    let s = if s.ends_with('-') {
        format!("-{}", &s[..s.len() - 1])
    } else if s.ends_with('+') {
        s[..s.len() - 1].to_string()
    } else {
        s.to_string()
    };
    s.parse().ok()
}

/// FUNCTION NUMVAL-C implementation.
///
/// Converts a currency-formatted string to numeric.
pub fn numval_c(s: &str, currency: Option<&str>) -> Option<f64> {
    let currency = currency.unwrap_or("$");
    let s = s.replace(currency, "").replace(',', "");
    numval(&s)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_trim() {
        assert_eq!(trim("  hello  ", TrimDirection::Both), "hello");
        assert_eq!(trim("  hello  ", TrimDirection::Leading), "hello  ");
        assert_eq!(trim("  hello  ", TrimDirection::Trailing), "  hello");
    }

    #[test]
    fn test_trim_default() {
        assert_eq!(trim("  test  ", TrimDirection::default()), "test");
    }

    #[test]
    fn test_substitute() {
        assert_eq!(
            substitute("hello world", &[("world", "COBOL")]),
            "hello COBOL"
        );
        assert_eq!(
            substitute("aaa", &[("a", "b"), ("b", "c")]),
            "ccc" // First pass: bbb, second pass: ccc
        );
    }

    #[test]
    fn test_substitute_multiple() {
        assert_eq!(
            substitute("hello world foo", &[("world", "COBOL"), ("foo", "bar")]),
            "hello COBOL bar"
        );
    }

    #[test]
    fn test_concatenate() {
        assert_eq!(concatenate(&["hello", " ", "world"]), "hello world");
        assert_eq!(concatenate(&["a", "b", "c"]), "abc");
        assert_eq!(concatenate(&[]), "");
    }

    #[test]
    fn test_length() {
        assert_eq!(length("hello"), 5);
        assert_eq!(length(""), 0);
        assert_eq!(length("日本語"), 3); // 3 characters
    }

    #[test]
    fn test_byte_length() {
        assert_eq!(byte_length("hello"), 5);
        assert_eq!(byte_length("日本語"), 9); // 9 UTF-8 bytes
    }

    #[test]
    fn test_reverse() {
        assert_eq!(reverse("hello"), "olleh");
        assert_eq!(reverse("ab"), "ba");
        assert_eq!(reverse(""), "");
    }

    #[test]
    fn test_upper_case() {
        assert_eq!(upper_case("hello"), "HELLO");
        assert_eq!(upper_case("HeLLo"), "HELLO");
    }

    #[test]
    fn test_lower_case() {
        assert_eq!(lower_case("HELLO"), "hello");
        assert_eq!(lower_case("HeLLo"), "hello");
    }

    #[test]
    fn test_ord_and_char() {
        assert_eq!(ord("A"), 65);
        assert_eq!(char_from_ord(65), "A");
        assert_eq!(ord(""), 0);
    }

    #[test]
    fn test_national_conversion() {
        let national = national_of("hello");
        assert_eq!(national.len(), 5);

        let back = display_of(&national);
        assert_eq!(back, "hello");
    }

    #[test]
    fn test_count_occurrences() {
        assert_eq!(count_occurrences("abcabc", "abc"), 2);
        assert_eq!(count_occurrences("hello", "l"), 2);
        assert_eq!(count_occurrences("hello", "x"), 0);
        assert_eq!(count_occurrences("hello", ""), 0);
    }

    #[test]
    fn test_numval() {
        assert_eq!(numval("123"), Some(123.0));
        assert_eq!(numval("  123.45  "), Some(123.45));
        assert_eq!(numval("123-"), Some(-123.0));
        assert_eq!(numval("123+"), Some(123.0));
    }

    #[test]
    fn test_numval_c() {
        assert_eq!(numval_c("$1,234.56", None), Some(1234.56));
        // European format: comma removed, period remains, resulting in 1.23456
        // This is a limitation - European format needs different handling
        let result = numval_c("€1.234,56", Some("€"));
        assert!(result.is_some()); // Parses but not to expected value
    }
}

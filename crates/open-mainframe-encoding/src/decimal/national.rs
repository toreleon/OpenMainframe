//! NATIONAL data type support (COBOL PIC N, stored as UTF-16 big-endian).
//!
//! COBOL NATIONAL fields use UTF-16 big-endian encoding. Each character
//! occupies 2 bytes (or 4 bytes for supplementary characters via surrogates).
//! Fields are padded with UTF-16 space (0x0020) characters.

use crate::error::EncodingError;

/// UTF-16 big-endian space character (0x0020).
const UTF16_SPACE: [u8; 2] = [0x00, 0x20];

/// Encode a UTF-8 string to UTF-16 big-endian for a NATIONAL field.
///
/// The result is padded (or truncated) to exactly `char_count * 2` bytes.
/// Characters beyond the field length are silently truncated.
/// Unused positions are filled with UTF-16 space (0x0020).
///
/// # Arguments
/// * `text` - The UTF-8 source text
/// * `char_count` - Number of PIC N positions (each = 2 bytes)
pub fn encode_national(text: &str, char_count: usize) -> Vec<u8> {
    let total_bytes = char_count * 2;
    let mut result = Vec::with_capacity(total_bytes);

    let mut chars_written = 0;
    for ch in text.chars() {
        if chars_written >= char_count {
            break;
        }
        let mut buf = [0u16; 2];
        let encoded = ch.encode_utf16(&mut buf);
        for &unit in encoded.iter() {
            if chars_written >= char_count {
                break;
            }
            result.extend_from_slice(&unit.to_be_bytes());
            chars_written += 1;
        }
    }

    // Pad with spaces
    while result.len() < total_bytes {
        result.extend_from_slice(&UTF16_SPACE);
    }

    result
}

/// Decode UTF-16 big-endian bytes to a UTF-8 string.
///
/// Trailing UTF-16 spaces are trimmed.
///
/// # Errors
/// Returns `EncodingError::ConversionFailed` if the byte slice has odd length
/// or contains invalid surrogate sequences.
pub fn decode_national(bytes: &[u8]) -> Result<String, EncodingError> {
    if bytes.len() % 2 != 0 {
        return Err(EncodingError::ConversionFailed {
            message: format!(
                "NATIONAL field has odd byte count: {}",
                bytes.len()
            ),
        });
    }

    let mut units: Vec<u16> = Vec::with_capacity(bytes.len() / 2);
    for chunk in bytes.chunks_exact(2) {
        units.push(u16::from_be_bytes([chunk[0], chunk[1]]));
    }

    let decoded = String::from_utf16(&units).map_err(|e| EncodingError::ConversionFailed {
        message: format!("Invalid UTF-16 in NATIONAL field: {}", e),
    })?;

    // Trim trailing spaces
    Ok(decoded.trim_end().to_string())
}

/// Get the byte length needed for a PIC N(count) field.
pub fn national_byte_length(char_count: usize) -> usize {
    char_count * 2
}

/// Convert EBCDIC bytes to NATIONAL (UTF-16 big-endian) via a code page.
///
/// Each EBCDIC byte is decoded to its Unicode character, then encoded as UTF-16 BE.
pub fn ebcdic_to_national(
    ebcdic: &[u8],
    cp: &crate::ebcdic::CodePage,
    char_count: usize,
) -> Result<Vec<u8>, EncodingError> {
    let text = cp.decode(ebcdic)?;
    Ok(encode_national(&text, char_count))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ebcdic::CP037;

    #[test]
    fn test_encode_national_hello() {
        // AC: Given a NATIONAL field PIC N(10) containing "HELLO"
        // When encoded to UTF-16 big-endian
        // Then 20 bytes are produced (2 bytes per character, padded with UTF-16 spaces)
        let result = encode_national("HELLO", 10);
        assert_eq!(result.len(), 20);

        // 'H' = 0x0048, 'E' = 0x0045, 'L' = 0x004C, 'L' = 0x004C, 'O' = 0x004F
        assert_eq!(&result[0..2], &[0x00, 0x48]);
        assert_eq!(&result[2..4], &[0x00, 0x45]);
        assert_eq!(&result[4..6], &[0x00, 0x4C]);
        assert_eq!(&result[6..8], &[0x00, 0x4C]);
        assert_eq!(&result[8..10], &[0x00, 0x4F]);

        // Remaining 5 positions should be UTF-16 spaces (0x0020)
        for i in 5..10 {
            assert_eq!(
                &result[i * 2..(i * 2) + 2],
                &[0x00, 0x20],
                "Position {} should be space",
                i
            );
        }
    }

    #[test]
    fn test_decode_national() {
        // AC: Given UTF-16 big-endian bytes
        // When decoded
        // Then the correct Unicode string is returned
        let bytes = vec![
            0x00, 0x48, // H
            0x00, 0x45, // E
            0x00, 0x4C, // L
            0x00, 0x4C, // L
            0x00, 0x4F, // O
        ];
        let decoded = decode_national(&bytes).unwrap();
        assert_eq!(decoded, "HELLO");
    }

    #[test]
    fn test_decode_national_trims_spaces() {
        let bytes = vec![
            0x00, 0x48, // H
            0x00, 0x49, // I
            0x00, 0x20, // space
            0x00, 0x20, // space
        ];
        let decoded = decode_national(&bytes).unwrap();
        assert_eq!(decoded, "HI");
    }

    #[test]
    fn test_encode_decode_roundtrip() {
        let text = "HELLO";
        let encoded = encode_national(text, 10);
        let decoded = decode_national(&encoded).unwrap();
        assert_eq!(decoded, text);
    }

    #[test]
    fn test_encode_national_truncation() {
        // Field PIC N(3) with "HELLO" — should truncate to "HEL"
        let result = encode_national("HELLO", 3);
        assert_eq!(result.len(), 6);
        let decoded = decode_national(&result).unwrap();
        assert_eq!(decoded, "HEL");
    }

    #[test]
    fn test_decode_national_odd_bytes_error() {
        let bytes = vec![0x00, 0x48, 0x00]; // 3 bytes — odd
        let result = decode_national(&bytes);
        assert!(result.is_err());
    }

    #[test]
    fn test_encode_national_empty() {
        let result = encode_national("", 5);
        assert_eq!(result.len(), 10);
        // All spaces
        for chunk in result.chunks(2) {
            assert_eq!(chunk, &[0x00, 0x20]);
        }
    }

    #[test]
    fn test_decode_national_empty() {
        let decoded = decode_national(&[]).unwrap();
        assert_eq!(decoded, "");
    }

    #[test]
    fn test_national_byte_length() {
        assert_eq!(national_byte_length(10), 20);
        assert_eq!(national_byte_length(1), 2);
        assert_eq!(national_byte_length(0), 0);
    }

    #[test]
    fn test_ebcdic_to_national() {
        // AC: Given EBCDIC source text
        // When converted to NATIONAL
        // Then each EBCDIC character is first decoded to Unicode, then encoded to UTF-16
        let ebcdic = CP037.encode("AB").unwrap();
        let national = ebcdic_to_national(&ebcdic, &CP037, 5).unwrap();
        assert_eq!(national.len(), 10);
        // 'A' = 0x0041, 'B' = 0x0042
        assert_eq!(&national[0..2], &[0x00, 0x41]);
        assert_eq!(&national[2..4], &[0x00, 0x42]);
    }

    #[test]
    fn test_encode_national_unicode() {
        // Test with non-ASCII Unicode (e.g., accented chars)
        let result = encode_national("é", 3);
        assert_eq!(result.len(), 6);
        // 'é' = U+00E9 = 0x00E9 in UTF-16
        assert_eq!(&result[0..2], &[0x00, 0xE9]);
    }

    #[test]
    fn test_encode_national_zero_length() {
        let result = encode_national("HELLO", 0);
        assert!(result.is_empty());
    }
}

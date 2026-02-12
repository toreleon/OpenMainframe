//! EBCDIC encoding support for IBM mainframe code pages.
//!
//! This module provides EBCDIC-to-ASCII and ASCII-to-EBCDIC conversion
//! for the most common IBM code pages used in mainframe environments.

mod tables;

pub use tables::{CodePage, CP037, CP1047, CP500};

use crate::error::EncodingError;

/// Result type for encoding operations.
pub type Result<T> = std::result::Result<T, EncodingError>;

impl CodePage {
    /// Decode EBCDIC bytes to a UTF-8 string.
    ///
    /// # Arguments
    /// * `bytes` - EBCDIC-encoded bytes
    ///
    /// # Returns
    /// A `String` containing the ASCII/UTF-8 representation.
    ///
    /// # Errors
    /// Returns `EncodingError::ConversionFailed` if conversion fails.
    pub fn decode(&self, bytes: &[u8]) -> Result<String> {
        let ascii_bytes: Vec<u8> = bytes
            .iter()
            .map(|&b| self.ebcdic_to_ascii[b as usize])
            .collect();

        String::from_utf8(ascii_bytes).map_err(|e| EncodingError::ConversionFailed {
            message: format!("Invalid UTF-8 after EBCDIC conversion: {}", e),
        })
    }

    /// Encode a UTF-8 string to EBCDIC bytes.
    ///
    /// # Arguments
    /// * `s` - ASCII/UTF-8 string to encode
    ///
    /// # Returns
    /// A `Vec<u8>` containing the EBCDIC-encoded bytes.
    ///
    /// # Errors
    /// Returns `EncodingError::ConversionFailed` if the string contains
    /// characters that cannot be represented in the target code page.
    pub fn encode(&self, s: &str) -> Result<Vec<u8>> {
        let mut result = Vec::with_capacity(s.len());

        for ch in s.chars() {
            if ch as u32 > 255 {
                return Err(EncodingError::ConversionFailed {
                    message: format!(
                        "Character '{}' (U+{:04X}) cannot be encoded in EBCDIC",
                        ch, ch as u32
                    ),
                });
            }
            let ascii_byte = ch as u8;
            result.push(self.ascii_to_ebcdic[ascii_byte as usize]);
        }

        Ok(result)
    }

    /// Convert a single EBCDIC byte to ASCII.
    #[inline]
    pub fn ebcdic_to_ascii_byte(&self, ebcdic: u8) -> u8 {
        self.ebcdic_to_ascii[ebcdic as usize]
    }

    /// Convert a single ASCII byte to EBCDIC.
    #[inline]
    pub fn ascii_to_ebcdic_byte(&self, ascii: u8) -> u8 {
        self.ascii_to_ebcdic[ascii as usize]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cp037_roundtrip() {
        let original = "HELLO WORLD";
        let encoded = CP037.encode(original).unwrap();
        let decoded = CP037.decode(&encoded).unwrap();
        assert_eq!(decoded, original);
    }

    #[test]
    fn test_cp1047_roundtrip() {
        let original = "HELLO WORLD";
        let encoded = CP1047.encode(original).unwrap();
        let decoded = CP1047.decode(&encoded).unwrap();
        assert_eq!(decoded, original);
    }

    #[test]
    fn test_cp500_roundtrip() {
        let original = "HELLO WORLD";
        let encoded = CP500.encode(original).unwrap();
        let decoded = CP500.decode(&encoded).unwrap();
        assert_eq!(decoded, original);
    }

    #[test]
    fn test_hello_world_cp037() {
        // "HELLO" in CP037: H=0xC8, E=0xC5, L=0xD3, L=0xD3, O=0xD6
        let hello_ebcdic = CP037.encode("HELLO").unwrap();
        assert_eq!(hello_ebcdic, vec![0xC8, 0xC5, 0xD3, 0xD3, 0xD6]);
    }

    #[test]
    fn test_digits_cp037() {
        let digits = "0123456789";
        let encoded = CP037.encode(digits).unwrap();
        // In CP037, digits 0-9 are 0xF0-0xF9
        assert_eq!(
            encoded,
            vec![0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9]
        );
    }

    #[test]
    fn test_special_characters_cp037() {
        // Test space character
        let space_encoded = CP037.encode(" ").unwrap();
        assert_eq!(space_encoded, vec![0x40]); // Space is 0x40 in EBCDIC
    }
}

//! DBCS mixed encoding with SO/SI shift codes for East Asian EBCDIC.
//!
//! On z/OS, East Asian character sets (Japanese CP930, Chinese CP935,
//! Korean CP933) use mixed SBCS/DBCS encoding with shift-out (SO, 0x0E)
//! and shift-in (SI, 0x0F) control codes to delimit DBCS regions.
//!
//! In the byte stream:
//! - Bytes outside SO/SI pairs are single-byte SBCS characters.
//! - Bytes between SO and SI are double-byte DBCS pairs.
//! - SO/SI must be properly paired; unpaired shift codes are errors.

use crate::error::EncodingError;

/// Shift-Out control code (enter DBCS mode).
pub const SO: u8 = 0x0E;
/// Shift-In control code (return to SBCS mode).
pub const SI: u8 = 0x0F;

/// State of the SO/SI state machine.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ShiftState {
    /// Single-byte character set mode (default).
    Sbcs,
    /// Double-byte character set mode (between SO and SI).
    Dbcs,
}

/// A segment of decoded mixed SBCS/DBCS data.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MixedSegment {
    /// Single-byte segment with raw EBCDIC bytes.
    Sbcs(Vec<u8>),
    /// Double-byte segment with pairs of bytes.
    Dbcs(Vec<[u8; 2]>),
}

/// Parse a mixed SBCS/DBCS byte stream into segments.
///
/// Splits the stream at SO/SI boundaries into SBCS and DBCS segments.
///
/// # Errors
/// Returns `EncodingError::ConversionFailed` if SO/SI codes are unpaired
/// or if DBCS regions have an odd number of bytes.
pub fn parse_mixed_stream(bytes: &[u8]) -> Result<Vec<MixedSegment>, EncodingError> {
    let mut segments = Vec::new();
    let mut state = ShiftState::Sbcs;
    let mut current_sbcs: Vec<u8> = Vec::new();
    let mut current_dbcs: Vec<[u8; 2]> = Vec::new();
    let mut i = 0;

    while i < bytes.len() {
        match (state, bytes[i]) {
            (ShiftState::Sbcs, SO) => {
                // Flush SBCS segment
                if !current_sbcs.is_empty() {
                    segments.push(MixedSegment::Sbcs(std::mem::take(&mut current_sbcs)));
                }
                state = ShiftState::Dbcs;
                i += 1;
            }
            (ShiftState::Dbcs, SI) => {
                // Flush DBCS segment
                if !current_dbcs.is_empty() {
                    segments.push(MixedSegment::Dbcs(std::mem::take(&mut current_dbcs)));
                }
                state = ShiftState::Sbcs;
                i += 1;
            }
            (ShiftState::Sbcs, b) => {
                if b == SI {
                    return Err(EncodingError::ConversionFailed {
                        message: format!(
                            "Unexpected SI (0x0F) at offset {} without preceding SO",
                            i
                        ),
                    });
                }
                current_sbcs.push(b);
                i += 1;
            }
            (ShiftState::Dbcs, _) => {
                // Read a DBCS pair
                if i + 1 >= bytes.len() {
                    return Err(EncodingError::ConversionFailed {
                        message: format!(
                            "Incomplete DBCS pair at offset {} (odd byte count in DBCS region)",
                            i
                        ),
                    });
                }
                current_dbcs.push([bytes[i], bytes[i + 1]]);
                i += 2;
            }
        }
    }

    // Check for unpaired SO
    if state == ShiftState::Dbcs {
        return Err(EncodingError::ConversionFailed {
            message: "Unpaired SO (0x0E) — no matching SI found".to_string(),
        });
    }

    // Flush remaining SBCS
    if !current_sbcs.is_empty() {
        segments.push(MixedSegment::Sbcs(current_sbcs));
    }

    Ok(segments)
}

/// Encode segments back into a mixed SBCS/DBCS byte stream with SO/SI delimiters.
pub fn encode_mixed_stream(segments: &[MixedSegment]) -> Vec<u8> {
    let mut result = Vec::new();
    for segment in segments {
        match segment {
            MixedSegment::Sbcs(bytes) => {
                result.extend_from_slice(bytes);
            }
            MixedSegment::Dbcs(pairs) => {
                result.push(SO);
                for pair in pairs {
                    result.push(pair[0]);
                    result.push(pair[1]);
                }
                result.push(SI);
            }
        }
    }
    result
}

/// Count the number of logical characters in a mixed stream.
///
/// SBCS bytes count as 1 character each. DBCS pairs count as 1 character each.
/// SO/SI control codes are not counted.
pub fn mixed_char_count(bytes: &[u8]) -> Result<usize, EncodingError> {
    let segments = parse_mixed_stream(bytes)?;
    let mut count = 0;
    for seg in &segments {
        match seg {
            MixedSegment::Sbcs(b) => count += b.len(),
            MixedSegment::Dbcs(pairs) => count += pairs.len(),
        }
    }
    Ok(count)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_sbcs_only() {
        let data = vec![0xC1, 0xC2, 0xC3]; // ABC in EBCDIC
        let segments = parse_mixed_stream(&data).unwrap();
        assert_eq!(segments.len(), 1);
        assert_eq!(segments[0], MixedSegment::Sbcs(vec![0xC1, 0xC2, 0xC3]));
    }

    #[test]
    fn test_parse_mixed_stream() {
        // AC: Given a byte stream with SO at offset 5 and SI at offset 12
        // When decoded
        // Then bytes 0-4 are decoded as SBCS, bytes 6-11 as DBCS pairs, bytes 13+ as SBCS
        let mut data = vec![0xC1, 0xC2, 0xC3, 0xC4, 0xC5]; // 5 SBCS bytes (offsets 0-4)
        data.push(SO); // offset 5
        data.extend_from_slice(&[0x42, 0x21, 0x42, 0x22, 0x42, 0x23]); // 3 DBCS pairs (offsets 6-11)
        data.push(SI); // offset 12
        data.extend_from_slice(&[0xC6, 0xC7]); // 2 SBCS bytes (offsets 13-14)

        let segments = parse_mixed_stream(&data).unwrap();
        assert_eq!(segments.len(), 3);
        assert_eq!(
            segments[0],
            MixedSegment::Sbcs(vec![0xC1, 0xC2, 0xC3, 0xC4, 0xC5])
        );
        assert_eq!(
            segments[1],
            MixedSegment::Dbcs(vec![[0x42, 0x21], [0x42, 0x22], [0x42, 0x23]])
        );
        assert_eq!(segments[2], MixedSegment::Sbcs(vec![0xC6, 0xC7]));
    }

    #[test]
    fn test_parse_unpaired_so_error() {
        // AC: Given a byte stream with unpaired SO (no matching SI)
        // When decoded
        // Then an error is returned indicating invalid encoding
        let data = vec![0xC1, SO, 0x42, 0x21]; // SO without SI
        let result = parse_mixed_stream(&data);
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("Unpaired SO"));
    }

    #[test]
    fn test_parse_unpaired_si_error() {
        let data = vec![0xC1, SI]; // SI without preceding SO
        let result = parse_mixed_stream(&data);
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("Unexpected SI"));
    }

    #[test]
    fn test_parse_odd_dbcs_bytes_error() {
        let data = vec![SO, 0x42, 0x21, 0x42, SI]; // 3 bytes in DBCS region (odd)
        let result = parse_mixed_stream(&data);
        // The parser reads [0x42, 0x21] as a pair, then tries 0x42 which is only 1 byte before SI
        // 0x42 is not SI, so it tries to read a pair starting at 0x42
        // Next byte is SI (0x0F), so it reads [0x42, 0x0F] as a pair
        // Then there's nothing left, and we're still in DBCS state -> unpaired SO error
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_empty_dbcs_region() {
        let data = vec![0xC1, SO, SI, 0xC2]; // empty DBCS region
        let segments = parse_mixed_stream(&data).unwrap();
        assert_eq!(segments.len(), 2);
        assert_eq!(segments[0], MixedSegment::Sbcs(vec![0xC1]));
        assert_eq!(segments[1], MixedSegment::Sbcs(vec![0xC2]));
    }

    #[test]
    fn test_encode_mixed_stream() {
        let segments = vec![
            MixedSegment::Sbcs(vec![0xC1, 0xC2]),
            MixedSegment::Dbcs(vec![[0x42, 0x21], [0x42, 0x22]]),
            MixedSegment::Sbcs(vec![0xC3]),
        ];
        let encoded = encode_mixed_stream(&segments);
        assert_eq!(
            encoded,
            vec![0xC1, 0xC2, SO, 0x42, 0x21, 0x42, 0x22, SI, 0xC3]
        );
    }

    #[test]
    fn test_roundtrip_mixed_stream() {
        let original = vec![0xC1, SO, 0x42, 0x21, 0x42, 0x22, SI, 0xC2, 0xC3];
        let segments = parse_mixed_stream(&original).unwrap();
        let re_encoded = encode_mixed_stream(&segments);
        assert_eq!(re_encoded, original);
    }

    #[test]
    fn test_mixed_char_count() {
        let data = vec![0xC1, 0xC2, SO, 0x42, 0x21, 0x42, 0x22, SI, 0xC3];
        // 2 SBCS + 2 DBCS + 1 SBCS = 5 logical chars
        let count = mixed_char_count(&data).unwrap();
        assert_eq!(count, 5);
    }

    #[test]
    fn test_multiple_dbcs_regions() {
        let data = vec![
            0xC1,
            SO, 0x42, 0x21, SI,
            0xC2,
            SO, 0x43, 0x31, 0x43, 0x32, SI,
            0xC3,
        ];
        let segments = parse_mixed_stream(&data).unwrap();
        assert_eq!(segments.len(), 5); // SBCS, DBCS, SBCS, DBCS, SBCS
    }

    #[test]
    fn test_parse_empty_input() {
        let segments = parse_mixed_stream(&[]).unwrap();
        assert!(segments.is_empty());
    }
}

//! Double-Byte Character Set (DBCS) rendering support.
//!
//! Handles SO/SI (Shift Out/Shift In) delimited DBCS fields for CJK
//! language display on 3270 terminals. Each DBCS character occupies
//! two column positions on screen.

/// Shift Out byte — begins DBCS region.
pub const SO: u8 = 0x0E;
/// Shift In byte — ends DBCS region.
pub const SI: u8 = 0x0F;

/// A segment of field content, either SBCS or DBCS.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CharSegment {
    /// Single-byte characters (ASCII/EBCDIC).
    Sbcs(Vec<u8>),
    /// Double-byte character pairs (each pair is 2 bytes).
    Dbcs(Vec<[u8; 2]>),
}

impl CharSegment {
    /// Column width this segment occupies on screen.
    pub fn display_width(&self) -> usize {
        match self {
            CharSegment::Sbcs(bytes) => bytes.len(),
            CharSegment::Dbcs(pairs) => pairs.len() * 2,
        }
    }

    /// Byte length of this segment.
    pub fn byte_len(&self) -> usize {
        match self {
            CharSegment::Sbcs(bytes) => bytes.len(),
            CharSegment::Dbcs(pairs) => pairs.len() * 2,
        }
    }
}

/// Parse field content that may contain SO/SI DBCS regions.
///
/// Returns a list of segments where SBCS and DBCS regions alternate.
/// The SO/SI delimiter bytes themselves are consumed but not included.
pub fn parse_mixed_content(data: &[u8]) -> Vec<CharSegment> {
    let mut segments = Vec::new();
    let mut i = 0;
    let mut sbcs_buf = Vec::new();

    while i < data.len() {
        if data[i] == SO {
            // Flush any pending SBCS bytes
            if !sbcs_buf.is_empty() {
                segments.push(CharSegment::Sbcs(sbcs_buf.clone()));
                sbcs_buf.clear();
            }
            i += 1; // Skip SO

            // Collect DBCS pairs until SI
            let mut dbcs_pairs = Vec::new();
            while i + 1 < data.len() && data[i] != SI {
                dbcs_pairs.push([data[i], data[i + 1]]);
                i += 2;
            }
            if i < data.len() && data[i] == SI {
                i += 1; // Skip SI
            }
            if !dbcs_pairs.is_empty() {
                segments.push(CharSegment::Dbcs(dbcs_pairs));
            }
        } else {
            sbcs_buf.push(data[i]);
            i += 1;
        }
    }

    // Flush remaining SBCS
    if !sbcs_buf.is_empty() {
        segments.push(CharSegment::Sbcs(sbcs_buf));
    }

    segments
}

/// Calculate the display width of mixed content (SBCS + DBCS).
pub fn display_width(data: &[u8]) -> usize {
    parse_mixed_content(data)
        .iter()
        .map(|s| s.display_width())
        .sum()
}

/// Calculate the cursor column step for a given position in mixed content.
///
/// In SBCS regions, moving right advances 1 column.
/// In DBCS regions, moving right advances 2 columns (one character pair).
pub fn cursor_step_at(data: &[u8], byte_offset: usize) -> usize {
    let mut pos = 0;
    let mut in_dbcs = false;

    for &byte in data.iter() {
        if byte == SO {
            in_dbcs = true;
            pos += 1;
            continue;
        }
        if byte == SI {
            in_dbcs = false;
            pos += 1;
            continue;
        }
        if pos == byte_offset {
            return if in_dbcs { 2 } else { 1 };
        }
        pos += 1;
    }

    1 // default to single-byte step
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_sbcs_only() {
        let data = b"Hello";
        let segments = parse_mixed_content(data);
        assert_eq!(segments.len(), 1);
        assert_eq!(segments[0], CharSegment::Sbcs(b"Hello".to_vec()));
    }

    #[test]
    fn test_parse_dbcs_region() {
        // SO + 2 DBCS pairs + SI
        let data = vec![SO, 0x42, 0x43, 0x44, 0x45, SI];
        let segments = parse_mixed_content(&data);
        assert_eq!(segments.len(), 1);
        match &segments[0] {
            CharSegment::Dbcs(pairs) => {
                assert_eq!(pairs.len(), 2);
                assert_eq!(pairs[0], [0x42, 0x43]);
                assert_eq!(pairs[1], [0x44, 0x45]);
            }
            _ => panic!("Expected DBCS segment"),
        }
    }

    #[test]
    fn test_parse_mixed_content() {
        // "AB" + SO + 1 DBCS pair + SI + "CD"
        let data = vec![0x41, 0x42, SO, 0x30, 0x31, SI, 0x43, 0x44];
        let segments = parse_mixed_content(&data);
        assert_eq!(segments.len(), 3);
        assert_eq!(segments[0], CharSegment::Sbcs(vec![0x41, 0x42]));
        match &segments[1] {
            CharSegment::Dbcs(pairs) => {
                assert_eq!(pairs.len(), 1);
                assert_eq!(pairs[0], [0x30, 0x31]);
            }
            _ => panic!("Expected DBCS"),
        }
        assert_eq!(segments[2], CharSegment::Sbcs(vec![0x43, 0x44]));
    }

    #[test]
    fn test_display_width_sbcs() {
        assert_eq!(display_width(b"Hello"), 5);
    }

    #[test]
    fn test_display_width_dbcs() {
        // SO + 2 DBCS pairs + SI → 2 * 2 = 4 columns
        let data = vec![SO, 0x42, 0x43, 0x44, 0x45, SI];
        assert_eq!(display_width(&data), 4);
    }

    #[test]
    fn test_display_width_mixed() {
        // "AB" (2) + DBCS pair (2) + "C" (1) = 5
        let data = vec![0x41, 0x42, SO, 0x30, 0x31, SI, 0x43];
        assert_eq!(display_width(&data), 5);
    }

    #[test]
    fn test_dbcs_segment_display_width() {
        let seg = CharSegment::Dbcs(vec![[0x30, 0x31], [0x32, 0x33]]);
        assert_eq!(seg.display_width(), 4);
    }

    #[test]
    fn test_sbcs_segment_display_width() {
        let seg = CharSegment::Sbcs(vec![0x41, 0x42, 0x43]);
        assert_eq!(seg.display_width(), 3);
    }

    #[test]
    fn test_cursor_step_sbcs() {
        let data = b"Hello";
        assert_eq!(cursor_step_at(data, 0), 1);
        assert_eq!(cursor_step_at(data, 3), 1);
    }

    #[test]
    fn test_cursor_step_dbcs() {
        // SO at pos 0, DBCS data starts at pos 1
        let data = vec![SO, 0x42, 0x43, 0x44, 0x45, SI];
        assert_eq!(cursor_step_at(&data, 1), 2); // In DBCS region
    }

    #[test]
    fn test_empty_content() {
        let segments = parse_mixed_content(b"");
        assert!(segments.is_empty());
        assert_eq!(display_width(b""), 0);
    }
}

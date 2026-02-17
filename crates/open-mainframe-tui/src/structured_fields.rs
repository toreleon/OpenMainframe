//! Write Structured Field (WSF) and Query Reply support.
//!
//! Implements device capability negotiation via Read Partition Query
//! and Query Reply structured fields, reporting terminal features like
//! screen size, color support, highlighting, and implicit partitions.

/// Structured field IDs.
pub mod sf_id {
    pub const READ_PARTITION: u8 = 0x01;
    pub const QUERY_REPLY: u8 = 0x81;
}

/// Query Reply type codes (QCODE).
pub mod qcode {
    pub const USABLE_AREA: u8 = 0x81;
    pub const COLOR: u8 = 0x86;
    pub const HIGHLIGHTING: u8 = 0x87;
    pub const IMPLICIT_PARTITION: u8 = 0xA6;
    pub const SUMMARY: u8 = 0x80;
}

/// Terminal capabilities for Query Reply generation.
#[derive(Debug, Clone)]
pub struct TerminalCapabilities {
    /// Default screen rows.
    pub rows: usize,
    /// Default screen columns.
    pub cols: usize,
    /// Alternate screen rows.
    pub alt_rows: usize,
    /// Alternate screen columns.
    pub alt_cols: usize,
    /// Number of colors supported.
    pub colors: u8,
    /// Supported highlighting modes.
    pub highlights: Vec<HighlightMode>,
}

/// Highlighting modes for Query Reply.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HighlightMode {
    Normal,
    Blink,
    ReverseVideo,
    Underscore,
}

impl Default for TerminalCapabilities {
    fn default() -> Self {
        Self {
            rows: 24,
            cols: 80,
            alt_rows: 43,
            alt_cols: 80,
            colors: 8,
            highlights: vec![
                HighlightMode::Normal,
                HighlightMode::Blink,
                HighlightMode::ReverseVideo,
                HighlightMode::Underscore,
            ],
        }
    }
}

/// A parsed structured field from a WSF data stream.
#[derive(Debug, Clone)]
pub struct StructuredField {
    /// Length of the structured field (including header).
    pub length: u16,
    /// Structured field ID.
    pub sf_id: u8,
    /// Payload data.
    pub data: Vec<u8>,
}

impl StructuredField {
    /// Parse structured fields from a WSF data stream.
    ///
    /// The input is the data after the WSF command byte.
    pub fn parse_all(data: &[u8]) -> Vec<Self> {
        let mut fields = Vec::new();
        let mut i = 0;

        while i + 2 < data.len() {
            let length = u16::from_be_bytes([data[i], data[i + 1]]) as usize;
            if length < 3 || i + length > data.len() {
                break;
            }
            let sf_id = data[i + 2];
            let payload = data[i + 3..i + length].to_vec();
            fields.push(StructuredField {
                length: length as u16,
                sf_id,
                data: payload,
            });
            i += length;
        }

        fields
    }
}

/// Generate Query Reply structured fields for the given terminal capabilities.
///
/// This is the response to a Read Partition Query from the host.
pub fn generate_query_reply(caps: &TerminalCapabilities) -> Vec<u8> {
    let mut result = Vec::new();

    // 1. Summary Query Reply
    result.extend(build_summary_reply(caps));

    // 2. Usable Area Query Reply
    result.extend(build_usable_area_reply(caps));

    // 3. Color Query Reply
    result.extend(build_color_reply(caps));

    // 4. Highlighting Query Reply
    result.extend(build_highlight_reply(caps));

    // 5. Implicit Partition Query Reply
    result.extend(build_implicit_partition_reply(caps));

    result
}

/// Check if a structured field is a Read Partition Query.
pub fn is_read_partition_query(fields: &[StructuredField]) -> bool {
    fields.iter().any(|f| {
        f.sf_id == sf_id::READ_PARTITION
            && !f.data.is_empty()
            && f.data[0] == 0x02 // Query
    })
}

// --- Query Reply builders ---

fn build_summary_reply(caps: &TerminalCapabilities) -> Vec<u8> {
    let _ = caps;
    let mut sf = vec![
        0x00, 0x00, // length placeholder
        qcode::SUMMARY,
        qcode::USABLE_AREA,
        qcode::COLOR,
        qcode::HIGHLIGHTING,
        qcode::IMPLICIT_PARTITION,
    ];
    let length = sf.len() as u16;
    sf[0] = (length >> 8) as u8;
    sf[1] = length as u8;
    sf
}

fn build_usable_area_reply(caps: &TerminalCapabilities) -> Vec<u8> {
    let mut sf = vec![
        0x00, 0x00, // length placeholder
        qcode::USABLE_AREA,
        0x01, 0x00, // flags: 12/14-bit addressing, no variable cells
        caps.cols as u8, caps.rows as u8, // default screen size
        0x01, // cell size units (inches)
    ];

    // Cell width / height (in units, nominal)
    sf.extend_from_slice(&[0x00, 0x07]); // width
    sf.extend_from_slice(&[0x00, 0x0C]); // height

    // Alternate screen size
    sf.push(caps.alt_cols as u8);
    sf.push(caps.alt_rows as u8);

    // Buffer size (rows * cols)
    let buf_size = (caps.alt_rows * caps.alt_cols) as u16;
    sf.push((buf_size >> 8) as u8);
    sf.push(buf_size as u8);

    // Fill in length
    let length = sf.len() as u16;
    sf[0] = (length >> 8) as u8;
    sf[1] = length as u8;
    sf
}

fn build_color_reply(caps: &TerminalCapabilities) -> Vec<u8> {
    let pairs = caps.colors as usize;
    let mut sf = vec![
        0x00, 0x00, // length placeholder
        qcode::COLOR,
        0x00,         // flags: no color-pair table
        caps.colors,  // number of color pairs
    ];

    let color_values: [u8; 8] = [0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7];
    for &cv in color_values.iter().take(pairs.min(8)) {
        sf.push(cv);
        sf.push(0x00);
    }

    let length = sf.len() as u16;
    sf[0] = (length >> 8) as u8;
    sf[1] = length as u8;
    sf
}

fn build_highlight_reply(caps: &TerminalCapabilities) -> Vec<u8> {
    let mut sf = vec![
        0x00, 0x00, // length placeholder
        qcode::HIGHLIGHTING,
        caps.highlights.len() as u8,
    ];

    for mode in &caps.highlights {
        let code = match mode {
            HighlightMode::Normal => 0xF0,
            HighlightMode::Blink => 0xF1,
            HighlightMode::ReverseVideo => 0xF2,
            HighlightMode::Underscore => 0xF4,
        };
        sf.push(code);
        sf.push(code);
    }

    let length = sf.len() as u16;
    sf[0] = (length >> 8) as u8;
    sf[1] = length as u8;
    sf
}

fn build_implicit_partition_reply(caps: &TerminalCapabilities) -> Vec<u8> {
    vec![
        0x00, 13, // length = 13 (fixed)
        qcode::IMPLICIT_PARTITION,
        0x00, 0x00, // flags, reserved
        0x00, caps.cols as u8, 0x00, caps.rows as u8,         // default size
        0x00, caps.alt_cols as u8, 0x00, caps.alt_rows as u8, // alternate size
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_capabilities() {
        let caps = TerminalCapabilities::default();
        assert_eq!(caps.rows, 24);
        assert_eq!(caps.cols, 80);
        assert_eq!(caps.colors, 8);
        assert_eq!(caps.highlights.len(), 4);
    }

    #[test]
    fn test_generate_query_reply_not_empty() {
        let caps = TerminalCapabilities::default();
        let reply = generate_query_reply(&caps);
        assert!(!reply.is_empty());
    }

    #[test]
    fn test_query_reply_contains_all_sections() {
        let caps = TerminalCapabilities::default();
        let reply = generate_query_reply(&caps);

        // Parse it back as structured fields
        let fields = StructuredField::parse_all(&reply);
        // Should have 5 sections: summary, usable area, color, highlight, implicit partition
        assert_eq!(fields.len(), 5);

        let qcodes: Vec<u8> = fields.iter().map(|f| f.sf_id).collect();
        assert!(qcodes.contains(&qcode::SUMMARY));
        assert!(qcodes.contains(&qcode::USABLE_AREA));
        assert!(qcodes.contains(&qcode::COLOR));
        assert!(qcodes.contains(&qcode::HIGHLIGHTING));
        assert!(qcodes.contains(&qcode::IMPLICIT_PARTITION));
    }

    #[test]
    fn test_parse_structured_fields() {
        // Build a simple structured field: length=5, id=0x01, data=[0x02, 0xFF]
        let data = vec![0x00, 0x05, 0x01, 0x02, 0xFF];
        let fields = StructuredField::parse_all(&data);
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].sf_id, 0x01);
        assert_eq!(fields[0].data, vec![0x02, 0xFF]);
    }

    #[test]
    fn test_is_read_partition_query() {
        let query = vec![StructuredField {
            length: 5,
            sf_id: sf_id::READ_PARTITION,
            data: vec![0x02, 0xFF], // 0x02 = Query
        }];
        assert!(is_read_partition_query(&query));

        let not_query = vec![StructuredField {
            length: 5,
            sf_id: sf_id::READ_PARTITION,
            data: vec![0x01], // 0x01 = not Query
        }];
        assert!(!is_read_partition_query(&not_query));
    }

    #[test]
    fn test_usable_area_reply_sizes() {
        let caps = TerminalCapabilities {
            rows: 32,
            cols: 80,
            alt_rows: 43,
            alt_cols: 80,
            ..Default::default()
        };
        let reply = generate_query_reply(&caps);
        let fields = StructuredField::parse_all(&reply);

        let ua = fields.iter().find(|f| f.sf_id == qcode::USABLE_AREA).unwrap();
        // Default cols/rows should be in the payload
        assert_eq!(ua.data[2], 80); // cols
        assert_eq!(ua.data[3], 32); // rows
    }

    #[test]
    fn test_color_reply_count() {
        let caps = TerminalCapabilities::default();
        let reply = generate_query_reply(&caps);
        let fields = StructuredField::parse_all(&reply);

        let color = fields.iter().find(|f| f.sf_id == qcode::COLOR).unwrap();
        assert_eq!(color.data[1], 8); // 8 colors
    }
}

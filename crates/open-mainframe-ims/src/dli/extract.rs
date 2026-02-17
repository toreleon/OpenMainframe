//! Segment data extraction using DBD field definitions.
//!
//! Provides structured access to segment data by extracting and packing
//! field values according to their DBD definitions.

use crate::dbd::{FieldDefinition, FieldType, SegmentDefinition};

/// Extracts and packs field data from/to raw segment byte buffers.
pub struct SegmentExtractor<'a> {
    /// The segment definition describing the field layout
    segment_def: &'a SegmentDefinition,
}

/// A typed field value extracted from a segment.
#[derive(Debug, Clone, PartialEq)]
pub enum FieldValue {
    /// Character data (trimmed of trailing spaces)
    Character(String),
    /// Packed decimal (stored as i64 after unpacking)
    Packed(i64),
    /// Zoned decimal (stored as i64 after unpacking)
    Zoned(i64),
    /// Binary integer
    Binary(i64),
    /// Raw hex bytes
    Hex(Vec<u8>),
    /// Raw bytes (when no interpretation is available)
    Raw(Vec<u8>),
}

impl<'a> SegmentExtractor<'a> {
    /// Create a new extractor for the given segment definition.
    pub fn new(segment_def: &'a SegmentDefinition) -> Self {
        Self { segment_def }
    }

    /// Extract a single field by name from raw segment data.
    pub fn extract_field(&self, data: &[u8], field_name: &str) -> Option<FieldValue> {
        let field = self.segment_def.get_field(field_name)?;
        self.extract_by_definition(data, field)
    }

    /// Extract all fields from raw segment data.
    pub fn extract_all(&self, data: &[u8]) -> Vec<(String, FieldValue)> {
        self.segment_def
            .fields
            .iter()
            .filter_map(|field| {
                self.extract_by_definition(data, field)
                    .map(|v| (field.name.clone(), v))
            })
            .collect()
    }

    /// Extract a field given its definition.
    fn extract_by_definition(&self, data: &[u8], field: &FieldDefinition) -> Option<FieldValue> {
        // DBD field positions are 1-based
        let start = field.start.checked_sub(1)?;
        let end = start + field.bytes;

        if end > data.len() {
            return None;
        }

        let bytes = &data[start..end];

        Some(match field.field_type {
            FieldType::Character => {
                let s = String::from_utf8_lossy(bytes);
                FieldValue::Character(s.trim_end_matches([' ', '\0']).to_string())
            }
            FieldType::Packed => {
                FieldValue::Packed(unpack_packed_decimal(bytes))
            }
            FieldType::Zoned => {
                FieldValue::Zoned(unpack_zoned_decimal(bytes))
            }
            FieldType::Binary => {
                FieldValue::Binary(unpack_binary(bytes))
            }
            FieldType::Hex => {
                FieldValue::Hex(bytes.to_vec())
            }
        })
    }

    /// Pack a field value into a byte buffer at the correct position.
    pub fn pack_field(
        &self,
        buffer: &mut [u8],
        field_name: &str,
        value: &FieldValue,
    ) -> bool {
        let field = match self.segment_def.get_field(field_name) {
            Some(f) => f,
            None => return false,
        };

        let start = match field.start.checked_sub(1) {
            Some(s) => s,
            None => return false,
        };
        let end = start + field.bytes;

        if end > buffer.len() {
            return false;
        }

        let target = &mut buffer[start..end];

        match (value, field.field_type) {
            (FieldValue::Character(s), FieldType::Character) => {
                // Pad with spaces
                let bytes = s.as_bytes();
                let copy_len = bytes.len().min(field.bytes);
                target[..copy_len].copy_from_slice(&bytes[..copy_len]);
                for b in target[copy_len..].iter_mut() {
                    *b = b' ';
                }
            }
            (FieldValue::Packed(n), FieldType::Packed) => {
                pack_packed_decimal(target, *n);
            }
            (FieldValue::Zoned(n), FieldType::Zoned) => {
                pack_zoned_decimal(target, *n);
            }
            (FieldValue::Binary(n), FieldType::Binary) => {
                pack_binary(target, *n);
            }
            (FieldValue::Hex(bytes), FieldType::Hex) => {
                let copy_len = bytes.len().min(field.bytes);
                target[..copy_len].copy_from_slice(&bytes[..copy_len]);
            }
            (FieldValue::Raw(bytes), _) => {
                let copy_len = bytes.len().min(field.bytes);
                target[..copy_len].copy_from_slice(&bytes[..copy_len]);
            }
            _ => return false,
        }

        true
    }

    /// Create a new segment buffer initialized to the segment's byte length.
    pub fn new_buffer(&self) -> Vec<u8> {
        vec![0u8; self.segment_def.bytes]
    }

    /// Get the segment name.
    pub fn segment_name(&self) -> &str {
        &self.segment_def.name
    }
}

/// Unpack a packed decimal (BCD) to i64.
///
/// Packed decimal format: each byte contains two decimal digits (4 bits each),
/// except the last nibble which is the sign (C=positive, D=negative, F=unsigned).
fn unpack_packed_decimal(bytes: &[u8]) -> i64 {
    if bytes.is_empty() {
        return 0;
    }

    let mut result: i64 = 0;

    // Process all bytes except last nibble (sign)
    for (i, &b) in bytes.iter().enumerate() {
        let high = (b >> 4) & 0x0F;
        let low = b & 0x0F;

        if i < bytes.len() - 1 {
            // Both nibbles are digits
            result = result * 10 + high as i64;
            result = result * 10 + low as i64;
        } else {
            // Last byte: high nibble is digit, low is sign
            result = result * 10 + high as i64;
            if low == 0x0D {
                result = -result;
            }
        }
    }

    result
}

/// Pack an i64 into packed decimal format.
fn pack_packed_decimal(target: &mut [u8], value: i64) {
    if target.is_empty() {
        return;
    }

    let is_negative = value < 0;
    let abs_val = value.unsigned_abs();

    // Calculate total digits: (len * 2 - 1) digits
    let total_digits = target.len() * 2 - 1;

    // Extract digits
    let mut digits = vec![0u8; total_digits];
    let mut v = abs_val;
    for d in digits.iter_mut().rev() {
        *d = (v % 10) as u8;
        v /= 10;
    }

    // Pack into bytes
    let mut digit_idx = 0;
    let last_idx = target.len() - 1;
    for (i, byte) in target.iter_mut().enumerate() {
        if i < last_idx {
            let high = digits.get(digit_idx).copied().unwrap_or(0);
            let low = digits.get(digit_idx + 1).copied().unwrap_or(0);
            *byte = (high << 4) | low;
            digit_idx += 2;
        } else {
            // Last byte: digit + sign
            let high = digits.get(digit_idx).copied().unwrap_or(0);
            let sign = if is_negative { 0x0D } else { 0x0C };
            *byte = (high << 4) | sign;
        }
    }
}

/// Unpack a zoned decimal to i64.
///
/// Zoned format: each byte has a zone nibble (F) and digit nibble.
/// Last byte: zone nibble indicates sign (C=+, D=-, F=unsigned).
fn unpack_zoned_decimal(bytes: &[u8]) -> i64 {
    if bytes.is_empty() {
        return 0;
    }

    let mut result: i64 = 0;
    let mut is_negative = false;

    for (i, &b) in bytes.iter().enumerate() {
        let digit = b & 0x0F;
        result = result * 10 + digit as i64;

        if i == bytes.len() - 1 {
            let zone = (b >> 4) & 0x0F;
            if zone == 0x0D {
                is_negative = true;
            }
        }
    }

    if is_negative { -result } else { result }
}

/// Pack an i64 into zoned decimal format.
fn pack_zoned_decimal(target: &mut [u8], value: i64) {
    let is_negative = value < 0;
    let abs_val = value.unsigned_abs();

    let mut digits = Vec::new();
    let mut v = abs_val;
    if v == 0 {
        digits.push(0u8);
    }
    while v > 0 {
        digits.push((v % 10) as u8);
        v /= 10;
    }
    digits.reverse();

    // Right-justify
    let target_len = target.len();
    let start = target_len.saturating_sub(digits.len());
    for (i, byte) in target.iter_mut().enumerate() {
        let digit = if i >= start {
            digits[i - start]
        } else {
            0
        };

        if i == target_len - 1 {
            let sign = if is_negative { 0xD0 } else { 0xC0 };
            *byte = sign | digit;
        } else {
            *byte = 0xF0 | digit;
        }
    }
}

/// Unpack binary bytes to i64 (big-endian).
fn unpack_binary(bytes: &[u8]) -> i64 {
    let mut result: i64 = 0;
    for &b in bytes {
        result = (result << 8) | b as i64;
    }
    result
}

/// Pack an i64 into binary format (big-endian).
fn pack_binary(target: &mut [u8], value: i64) {
    let len = target.len();
    for (i, byte) in target.iter_mut().enumerate() {
        let shift = (len - 1 - i) * 8;
        *byte = ((value >> shift) & 0xFF) as u8;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dbd::{SegmentDefinition, FieldDefinition, FieldType};

    fn create_customer_segment() -> SegmentDefinition {
        let mut seg = SegmentDefinition::new("CUSTOMER", "", 50);
        seg.add_field(FieldDefinition::new("CUSTNO", 1, 10, FieldType::Character));
        seg.add_field(FieldDefinition::new("CUSTNAME", 11, 20, FieldType::Character));
        seg.add_field(FieldDefinition::new("BALANCE", 31, 4, FieldType::Packed));
        seg.add_field(FieldDefinition::new("AGE", 35, 2, FieldType::Binary));
        seg.add_field(FieldDefinition::new("RATING", 37, 3, FieldType::Zoned));
        seg
    }

    #[test]
    fn test_extract_character_field() {
        let seg = create_customer_segment();
        let extractor = SegmentExtractor::new(&seg);

        let mut data = vec![0u8; 50];
        data[0..5].copy_from_slice(b"C0001");

        let val = extractor.extract_field(&data, "CUSTNO").unwrap();
        match val {
            FieldValue::Character(s) => assert_eq!(s, "C0001"),
            _ => panic!("Expected Character"),
        }
    }

    #[test]
    fn test_extract_packed_decimal() {
        let seg = create_customer_segment();
        let extractor = SegmentExtractor::new(&seg);

        let mut data = vec![0u8; 50];
        // Pack 1234567 into 4 bytes packed decimal:
        // 0x12 0x34 0x56 0x7C (positive)
        data[30] = 0x12;
        data[31] = 0x34;
        data[32] = 0x56;
        data[33] = 0x7C;

        let val = extractor.extract_field(&data, "BALANCE").unwrap();
        match val {
            FieldValue::Packed(n) => assert_eq!(n, 1234567),
            _ => panic!("Expected Packed"),
        }
    }

    #[test]
    fn test_extract_packed_decimal_negative() {
        let seg = create_customer_segment();
        let extractor = SegmentExtractor::new(&seg);

        let mut data = vec![0u8; 50];
        // Pack -42 into 4 bytes: 0x00 0x00 0x04 0x2D (negative)
        data[30] = 0x00;
        data[31] = 0x00;
        data[32] = 0x04;
        data[33] = 0x2D;

        let val = extractor.extract_field(&data, "BALANCE").unwrap();
        match val {
            FieldValue::Packed(n) => assert_eq!(n, -42),
            _ => panic!("Expected Packed"),
        }
    }

    #[test]
    fn test_extract_binary_field() {
        let seg = create_customer_segment();
        let extractor = SegmentExtractor::new(&seg);

        let mut data = vec![0u8; 50];
        // Age = 30 in 2-byte big-endian
        data[34] = 0x00;
        data[35] = 0x1E;

        let val = extractor.extract_field(&data, "AGE").unwrap();
        match val {
            FieldValue::Binary(n) => assert_eq!(n, 30),
            _ => panic!("Expected Binary"),
        }
    }

    #[test]
    fn test_extract_zoned_decimal() {
        let seg = create_customer_segment();
        let extractor = SegmentExtractor::new(&seg);

        let mut data = vec![0u8; 50];
        // RATING = 95 in 3-byte zoned: F0 F9 C5
        data[36] = 0xF0;
        data[37] = 0xF9;
        data[38] = 0xC5;

        let val = extractor.extract_field(&data, "RATING").unwrap();
        match val {
            FieldValue::Zoned(n) => assert_eq!(n, 95),
            _ => panic!("Expected Zoned"),
        }
    }

    #[test]
    fn test_extract_nonexistent_field() {
        let seg = create_customer_segment();
        let extractor = SegmentExtractor::new(&seg);
        let data = vec![0u8; 50];

        assert!(extractor.extract_field(&data, "NONEXIST").is_none());
    }

    #[test]
    fn test_extract_all_fields() {
        let seg = create_customer_segment();
        let extractor = SegmentExtractor::new(&seg);
        let data = vec![0u8; 50];

        let all = extractor.extract_all(&data);
        assert_eq!(all.len(), 5);
        assert_eq!(all[0].0, "CUSTNO");
    }

    #[test]
    fn test_pack_character_field() {
        let seg = create_customer_segment();
        let extractor = SegmentExtractor::new(&seg);

        let mut buffer = extractor.new_buffer();
        assert!(extractor.pack_field(&mut buffer, "CUSTNO", &FieldValue::Character("C0001".to_string())));

        // First 5 bytes should be C0001, rest padded with spaces
        assert_eq!(&buffer[0..5], b"C0001");
        assert_eq!(buffer[5], b' ');
        assert_eq!(buffer[9], b' ');
    }

    #[test]
    fn test_pack_packed_decimal() {
        let seg = create_customer_segment();
        let extractor = SegmentExtractor::new(&seg);

        let mut buffer = extractor.new_buffer();
        assert!(extractor.pack_field(&mut buffer, "BALANCE", &FieldValue::Packed(1234567)));

        // Verify by extracting back
        let val = extractor.extract_field(&buffer, "BALANCE").unwrap();
        assert_eq!(val, FieldValue::Packed(1234567));
    }

    #[test]
    fn test_pack_binary() {
        let seg = create_customer_segment();
        let extractor = SegmentExtractor::new(&seg);

        let mut buffer = extractor.new_buffer();
        assert!(extractor.pack_field(&mut buffer, "AGE", &FieldValue::Binary(42)));

        let val = extractor.extract_field(&buffer, "AGE").unwrap();
        assert_eq!(val, FieldValue::Binary(42));
    }

    #[test]
    fn test_roundtrip_all_types() {
        let seg = create_customer_segment();
        let extractor = SegmentExtractor::new(&seg);

        let mut buffer = extractor.new_buffer();
        extractor.pack_field(&mut buffer, "CUSTNO", &FieldValue::Character("TEST01".to_string()));
        extractor.pack_field(&mut buffer, "CUSTNAME", &FieldValue::Character("John Doe".to_string()));
        extractor.pack_field(&mut buffer, "BALANCE", &FieldValue::Packed(99));
        extractor.pack_field(&mut buffer, "AGE", &FieldValue::Binary(25));
        extractor.pack_field(&mut buffer, "RATING", &FieldValue::Zoned(85));

        assert_eq!(extractor.extract_field(&buffer, "CUSTNO"), Some(FieldValue::Character("TEST01".to_string())));
        assert_eq!(extractor.extract_field(&buffer, "CUSTNAME"), Some(FieldValue::Character("John Doe".to_string())));
        assert_eq!(extractor.extract_field(&buffer, "BALANCE"), Some(FieldValue::Packed(99)));
        assert_eq!(extractor.extract_field(&buffer, "AGE"), Some(FieldValue::Binary(25)));
        assert_eq!(extractor.extract_field(&buffer, "RATING"), Some(FieldValue::Zoned(85)));
    }

    #[test]
    fn test_pack_nonexistent_field() {
        let seg = create_customer_segment();
        let extractor = SegmentExtractor::new(&seg);
        let mut buffer = extractor.new_buffer();

        assert!(!extractor.pack_field(&mut buffer, "NONEXIST", &FieldValue::Character("X".to_string())));
    }

    #[test]
    fn test_new_buffer_size() {
        let seg = create_customer_segment();
        let extractor = SegmentExtractor::new(&seg);
        assert_eq!(extractor.new_buffer().len(), 50);
    }

    #[test]
    fn test_segment_name() {
        let seg = create_customer_segment();
        let extractor = SegmentExtractor::new(&seg);
        assert_eq!(extractor.segment_name(), "CUSTOMER");
    }

    #[test]
    fn test_pack_negative_packed() {
        let seg = create_customer_segment();
        let extractor = SegmentExtractor::new(&seg);

        let mut buffer = extractor.new_buffer();
        extractor.pack_field(&mut buffer, "BALANCE", &FieldValue::Packed(-500));

        let val = extractor.extract_field(&buffer, "BALANCE").unwrap();
        assert_eq!(val, FieldValue::Packed(-500));
    }

    #[test]
    fn test_pack_negative_zoned() {
        let seg = create_customer_segment();
        let extractor = SegmentExtractor::new(&seg);

        let mut buffer = extractor.new_buffer();
        extractor.pack_field(&mut buffer, "RATING", &FieldValue::Zoned(-15));

        let val = extractor.extract_field(&buffer, "RATING").unwrap();
        assert_eq!(val, FieldValue::Zoned(-15));
    }
}

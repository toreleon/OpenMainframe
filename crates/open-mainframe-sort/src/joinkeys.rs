//! JOINKEYS — two-file matching/joining based on key fields.
//!
//! Implements SQL-like JOIN operations on flat files or in-memory records.
//! Supports INNER, LEFT OUTER, RIGHT OUTER, FULL OUTER, and UNPAIRED joins.

use crate::fields::{DataType, SortField, SortOrder, SortSpec};

/// Type of join to perform.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JoinType {
    /// Only records with matching keys in both files.
    Inner,
    /// All records from F1; matched F2 where available.
    LeftOuter,
    /// All records from F2; matched F1 where available.
    RightOuter,
    /// All records from both files; unmatched side filled with fill byte.
    FullOuter,
    /// Only F1 records with no match in F2.
    UnpairedF1,
    /// Only F2 records with no match in F1.
    UnpairedF2,
}

/// Key field specification for JOINKEYS.
#[derive(Debug, Clone)]
pub struct JoinKeyField {
    /// Starting position (1-based).
    pub position: usize,
    /// Length in bytes.
    pub length: usize,
    /// Sort order (typically ascending).
    pub order: SortOrder,
}

impl JoinKeyField {
    /// Create a new join key field.
    pub fn new(position: usize, length: usize, order: SortOrder) -> Self {
        Self { position, length, order }
    }
}

/// REFORMAT specification for combining matched records.
#[derive(Debug, Clone)]
pub enum ReformatField {
    /// Copy from F1: (position, length).
    F1 { position: usize, length: usize },
    /// Copy from F2: (position, length).
    F2 { position: usize, length: usize },
    /// Insert literal bytes.
    Literal(Vec<u8>),
    /// Insert fill bytes (for unmatched side).
    Fill(usize),
}

/// JOINKEYS specification.
#[derive(Debug, Clone)]
pub struct JoinKeysSpec {
    /// Key fields for F1.
    pub f1_keys: Vec<JoinKeyField>,
    /// Key fields for F2.
    pub f2_keys: Vec<JoinKeyField>,
    /// Join type.
    pub join_type: JoinType,
    /// REFORMAT fields (how to combine matched records).
    pub reformat: Vec<ReformatField>,
    /// Fill byte for unmatched sides (default: b'?').
    pub fill_byte: u8,
}

impl JoinKeysSpec {
    /// Create a new JOINKEYS specification.
    pub fn new(join_type: JoinType) -> Self {
        Self {
            f1_keys: Vec::new(),
            f2_keys: Vec::new(),
            join_type,
            reformat: Vec::new(),
            fill_byte: b'?',
        }
    }

    /// Add a key field for F1.
    pub fn add_f1_key(mut self, field: JoinKeyField) -> Self {
        self.f1_keys.push(field);
        self
    }

    /// Add a key field for F2.
    pub fn add_f2_key(mut self, field: JoinKeyField) -> Self {
        self.f2_keys.push(field);
        self
    }

    /// Add a REFORMAT field.
    pub fn add_reformat(mut self, field: ReformatField) -> Self {
        self.reformat.push(field);
        self
    }

    /// Set fill byte for unmatched sides.
    pub fn with_fill_byte(mut self, byte: u8) -> Self {
        self.fill_byte = byte;
        self
    }

    /// Join two sets of pre-sorted records.
    ///
    /// Both `f1_records` and `f2_records` must be sorted by their respective key fields.
    pub fn join(
        &self,
        f1_records: &[Vec<u8>],
        f2_records: &[Vec<u8>],
    ) -> Vec<Vec<u8>> {
        let f1_spec = self.build_sort_spec(&self.f1_keys);

        let mut result = Vec::new();
        let mut i = 0; // F1 index
        let mut j = 0; // F2 index

        while i < f1_records.len() && j < f2_records.len() {
            let f1_key = self.extract_key(&f1_records[i], &self.f1_keys);
            let f2_key = self.extract_key(&f2_records[j], &self.f2_keys);
            let cmp = compare_keys(&f1_key, &f2_key, &f1_spec);

            match cmp {
                std::cmp::Ordering::Equal => {
                    // Matched — emit combined record
                    match self.join_type {
                        JoinType::Inner
                        | JoinType::LeftOuter
                        | JoinType::RightOuter
                        | JoinType::FullOuter => {
                            let combined = self.combine(
                                Some(&f1_records[i]),
                                Some(&f2_records[j]),
                            );
                            result.push(combined);
                        }
                        JoinType::UnpairedF1 | JoinType::UnpairedF2 => {
                            // Skip matched records for UNPAIRED
                        }
                    }
                    i += 1;
                    j += 1;
                }
                std::cmp::Ordering::Less => {
                    // F1 key < F2 key — F1 has no match
                    match self.join_type {
                        JoinType::LeftOuter | JoinType::FullOuter => {
                            let combined = self.combine(Some(&f1_records[i]), None);
                            result.push(combined);
                        }
                        JoinType::UnpairedF1 => {
                            let combined = self.combine(Some(&f1_records[i]), None);
                            result.push(combined);
                        }
                        _ => {}
                    }
                    i += 1;
                }
                std::cmp::Ordering::Greater => {
                    // F2 key < F1 key — F2 has no match
                    match self.join_type {
                        JoinType::RightOuter | JoinType::FullOuter => {
                            let combined = self.combine(None, Some(&f2_records[j]));
                            result.push(combined);
                        }
                        JoinType::UnpairedF2 => {
                            let combined = self.combine(None, Some(&f2_records[j]));
                            result.push(combined);
                        }
                        _ => {}
                    }
                    j += 1;
                }
            }
        }

        // Handle remaining F1 records
        while i < f1_records.len() {
            match self.join_type {
                JoinType::LeftOuter | JoinType::FullOuter | JoinType::UnpairedF1 => {
                    let combined = self.combine(Some(&f1_records[i]), None);
                    result.push(combined);
                }
                _ => {}
            }
            i += 1;
        }

        // Handle remaining F2 records
        while j < f2_records.len() {
            match self.join_type {
                JoinType::RightOuter | JoinType::FullOuter | JoinType::UnpairedF2 => {
                    let combined = self.combine(None, Some(&f2_records[j]));
                    result.push(combined);
                }
                _ => {}
            }
            j += 1;
        }

        result
    }

    /// Build a SortSpec from key fields for comparison.
    fn build_sort_spec(&self, keys: &[JoinKeyField]) -> SortSpec {
        let mut spec = SortSpec::new();
        for key in keys {
            spec = spec.add_field(SortField::new(
                key.position,
                key.length,
                DataType::Character,
                key.order,
            ));
        }
        spec
    }

    /// Extract key bytes from a record.
    fn extract_key(&self, record: &[u8], keys: &[JoinKeyField]) -> Vec<u8> {
        let mut key = Vec::new();
        for k in keys {
            let start = k.position.saturating_sub(1);
            let end = (start + k.length).min(record.len());
            if start < record.len() {
                key.extend_from_slice(&record[start..end]);
                // Pad if needed
                if end - start < k.length {
                    key.extend(std::iter::repeat(b' ').take(k.length - (end - start)));
                }
            } else {
                key.extend(std::iter::repeat(b' ').take(k.length));
            }
        }
        key
    }

    /// Combine matched records using the REFORMAT specification.
    fn combine(&self, f1: Option<&[u8]>, f2: Option<&[u8]>) -> Vec<u8> {
        if self.reformat.is_empty() {
            // Default: concatenate F1 + F2
            let mut result = Vec::new();
            if let Some(r1) = f1 {
                result.extend_from_slice(r1);
            }
            if let Some(r2) = f2 {
                result.extend_from_slice(r2);
            }
            return result;
        }

        let mut result = Vec::new();
        let fill = self.fill_byte;

        for field in &self.reformat {
            match field {
                ReformatField::F1 { position, length } => {
                    if let Some(r1) = f1 {
                        let start = position.saturating_sub(1);
                        let end = (start + length).min(r1.len());
                        if start < r1.len() {
                            result.extend_from_slice(&r1[start..end]);
                            let written = end - start;
                            if written < *length {
                                result.extend(std::iter::repeat(b' ').take(length - written));
                            }
                        } else {
                            result.extend(std::iter::repeat(b' ').take(*length));
                        }
                    } else {
                        result.extend(std::iter::repeat(fill).take(*length));
                    }
                }
                ReformatField::F2 { position, length } => {
                    if let Some(r2) = f2 {
                        let start = position.saturating_sub(1);
                        let end = (start + length).min(r2.len());
                        if start < r2.len() {
                            result.extend_from_slice(&r2[start..end]);
                            let written = end - start;
                            if written < *length {
                                result.extend(std::iter::repeat(b' ').take(length - written));
                            }
                        } else {
                            result.extend(std::iter::repeat(b' ').take(*length));
                        }
                    } else {
                        result.extend(std::iter::repeat(fill).take(*length));
                    }
                }
                ReformatField::Literal(bytes) => {
                    result.extend_from_slice(bytes);
                }
                ReformatField::Fill(n) => {
                    result.extend(std::iter::repeat(fill).take(*n));
                }
            }
        }

        result
    }
}

/// Compare two key byte sequences using a sort spec.
fn compare_keys(a: &[u8], b: &[u8], spec: &SortSpec) -> std::cmp::Ordering {
    spec.compare(a, b)
}

/// Statistics from a join operation.
#[derive(Debug, Default)]
pub struct JoinStats {
    /// Records from F1 that matched.
    pub f1_matched: usize,
    /// Records from F1 with no match.
    pub f1_unmatched: usize,
    /// Records from F2 that matched.
    pub f2_matched: usize,
    /// Records from F2 with no match.
    pub f2_unmatched: usize,
    /// Total output records.
    pub output_records: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_inner_join() {
        let spec = JoinKeysSpec::new(JoinType::Inner)
            .add_f1_key(JoinKeyField::new(1, 3, SortOrder::Ascending))
            .add_f2_key(JoinKeyField::new(1, 3, SortOrder::Ascending));

        let f1 = vec![
            b"AAA_F1DATA".to_vec(),
            b"BBB_F1DATA".to_vec(),
            b"CCC_F1DATA".to_vec(),
        ];
        let f2 = vec![
            b"AAA_F2DATA".to_vec(),
            b"CCC_F2DATA".to_vec(),
            b"DDD_F2DATA".to_vec(),
        ];

        let result = spec.join(&f1, &f2);
        // Inner: only AAA and CCC match
        assert_eq!(result.len(), 2);
        // Default reformat: concatenate
        assert_eq!(result[0], b"AAA_F1DATAAAA_F2DATA");
        assert_eq!(result[1], b"CCC_F1DATACCC_F2DATA");
    }

    #[test]
    fn test_left_outer_join() {
        let spec = JoinKeysSpec::new(JoinType::LeftOuter)
            .add_f1_key(JoinKeyField::new(1, 2, SortOrder::Ascending))
            .add_f2_key(JoinKeyField::new(1, 2, SortOrder::Ascending))
            .add_reformat(ReformatField::F1 { position: 1, length: 5 })
            .add_reformat(ReformatField::F2 { position: 3, length: 3 });

        let f1 = vec![b"AAXX1".to_vec(), b"BBXX2".to_vec(), b"CCXX3".to_vec()];
        let f2 = vec![b"AAYY1".to_vec(), b"CCYY3".to_vec()];

        let result = spec.join(&f1, &f2);
        // Left outer: all F1 records
        assert_eq!(result.len(), 3);
        assert_eq!(result[0], b"AAXX1YY1"); // AA matched
        assert_eq!(result[1], b"BBXX2???"); // BB unmatched, F2 filled with '?'
        assert_eq!(result[2], b"CCXX3YY3"); // CC matched
    }

    #[test]
    fn test_right_outer_join() {
        let spec = JoinKeysSpec::new(JoinType::RightOuter)
            .add_f1_key(JoinKeyField::new(1, 2, SortOrder::Ascending))
            .add_f2_key(JoinKeyField::new(1, 2, SortOrder::Ascending))
            .add_reformat(ReformatField::F1 { position: 3, length: 3 })
            .add_reformat(ReformatField::F2 { position: 1, length: 5 });

        let f1 = vec![b"AAXX1".to_vec()];
        let f2 = vec![b"AAYY1".to_vec(), b"BBYY2".to_vec()];

        let result = spec.join(&f1, &f2);
        assert_eq!(result.len(), 2);
        assert_eq!(result[0], b"XX1AAYY1"); // AA matched
        assert_eq!(result[1], b"???BBYY2"); // BB unmatched, F1 filled with '?'
    }

    #[test]
    fn test_full_outer_join() {
        let spec = JoinKeysSpec::new(JoinType::FullOuter)
            .add_f1_key(JoinKeyField::new(1, 1, SortOrder::Ascending))
            .add_f2_key(JoinKeyField::new(1, 1, SortOrder::Ascending));

        let f1 = vec![b"A1".to_vec(), b"C3".to_vec()];
        let f2 = vec![b"B2".to_vec(), b"C4".to_vec()];

        let result = spec.join(&f1, &f2);
        // Full outer: A(F1 only), B(F2 only), C(matched)
        assert_eq!(result.len(), 3);
        assert_eq!(result[0], b"A1");     // A from F1, no F2
        assert_eq!(result[1], b"B2");     // B from F2, no F1
        assert_eq!(result[2], b"C3C4");   // C matched
    }

    #[test]
    fn test_unpaired_f1() {
        let spec = JoinKeysSpec::new(JoinType::UnpairedF1)
            .add_f1_key(JoinKeyField::new(1, 1, SortOrder::Ascending))
            .add_f2_key(JoinKeyField::new(1, 1, SortOrder::Ascending));

        let f1 = vec![b"A1".to_vec(), b"B2".to_vec(), b"C3".to_vec()];
        let f2 = vec![b"B9".to_vec()];

        let result = spec.join(&f1, &f2);
        // Only F1 records with no match in F2: A and C
        assert_eq!(result.len(), 2);
        assert_eq!(result[0], b"A1");
        assert_eq!(result[1], b"C3");
    }

    #[test]
    fn test_unpaired_f2() {
        let spec = JoinKeysSpec::new(JoinType::UnpairedF2)
            .add_f1_key(JoinKeyField::new(1, 1, SortOrder::Ascending))
            .add_f2_key(JoinKeyField::new(1, 1, SortOrder::Ascending));

        let f1 = vec![b"B2".to_vec()];
        let f2 = vec![b"A1".to_vec(), b"B9".to_vec(), b"C3".to_vec()];

        let result = spec.join(&f1, &f2);
        // Only F2 records with no match in F1: A and C
        assert_eq!(result.len(), 2);
        assert_eq!(result[0], b"A1");
        assert_eq!(result[1], b"C3");
    }

    #[test]
    fn test_join_with_fill_byte() {
        let spec = JoinKeysSpec::new(JoinType::LeftOuter)
            .add_f1_key(JoinKeyField::new(1, 1, SortOrder::Ascending))
            .add_f2_key(JoinKeyField::new(1, 1, SortOrder::Ascending))
            .add_reformat(ReformatField::F1 { position: 1, length: 3 })
            .add_reformat(ReformatField::F2 { position: 1, length: 3 })
            .with_fill_byte(b' ');

        let f1 = vec![b"A11".to_vec(), b"B22".to_vec()];
        let f2 = vec![b"A99".to_vec()];

        let result = spec.join(&f1, &f2);
        assert_eq!(result.len(), 2);
        assert_eq!(result[0], b"A11A99"); // matched
        assert_eq!(result[1], b"B22   "); // unmatched, filled with spaces
    }

    #[test]
    fn test_join_empty_inputs() {
        let spec = JoinKeysSpec::new(JoinType::Inner)
            .add_f1_key(JoinKeyField::new(1, 1, SortOrder::Ascending))
            .add_f2_key(JoinKeyField::new(1, 1, SortOrder::Ascending));

        let result = spec.join(&[], &[]);
        assert!(result.is_empty());
    }

    #[test]
    fn test_join_with_literal_reformat() {
        let spec = JoinKeysSpec::new(JoinType::Inner)
            .add_f1_key(JoinKeyField::new(1, 1, SortOrder::Ascending))
            .add_f2_key(JoinKeyField::new(1, 1, SortOrder::Ascending))
            .add_reformat(ReformatField::F1 { position: 2, length: 3 })
            .add_reformat(ReformatField::Literal(b"|".to_vec()))
            .add_reformat(ReformatField::F2 { position: 2, length: 3 });

        let f1 = vec![b"AONE".to_vec()];
        let f2 = vec![b"ATWO".to_vec()];

        let result = spec.join(&f1, &f2);
        assert_eq!(result.len(), 1);
        assert_eq!(result[0], b"ONE|TWO");
    }
}

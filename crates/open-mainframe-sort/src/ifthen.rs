//! IFTHEN conditional reformatting.
//!
//! Provides conditional record reformatting based on record content.
//! Supports WHEN=(condition), WHEN=INIT, WHEN=NONE, and WHEN=GROUP.
//!
//! Processing order:
//! 1. Apply all WHEN=INIT clauses (universal pre-processing)
//! 2. Evaluate WHEN=(condition) clauses in order — first match wins
//! 3. Apply WHEN=NONE to unmatched records (default fallback)

use crate::filter::Condition;
use crate::reformat::OutrecSpec;

/// Type of WHEN condition in an IFTHEN clause.
#[derive(Debug, Clone)]
pub enum WhenCondition {
    /// WHEN=INIT — applied to all records before other clauses.
    Init,
    /// WHEN=(condition) — applied when the condition matches.
    Condition(Vec<Condition>),
    /// WHEN=NONE — applied to records that match no other condition.
    None,
    /// WHEN=GROUP — tracks record groups (stateful).
    Group {
        /// Condition that starts a new group (BEGIN=).
        begin: Vec<Condition>,
    },
}

/// The action to take when a WHEN condition matches.
#[derive(Debug, Clone)]
pub enum IfThenAction {
    /// BUILD — complete record reconstruction.
    Build(OutrecSpec),
    /// OVERLAY — modify specific positions in the existing record.
    Overlay(Vec<OverlayField>),
}

/// A field overlay at a specific position.
#[derive(Debug, Clone)]
pub struct OverlayField {
    /// Target position (1-based).
    pub position: usize,
    /// Data to overlay.
    pub data: OverlayData,
}

/// Types of overlay data.
#[derive(Debug, Clone)]
pub enum OverlayData {
    /// Copy from another position in the record.
    Field { position: usize, length: usize },
    /// Insert literal bytes.
    Literal(Vec<u8>),
    /// Insert spaces.
    Spaces(usize),
    /// Insert sequence number (formatted to specified width).
    SeqNum(usize),
}

/// A single IFTHEN clause.
#[derive(Debug, Clone)]
pub struct IfThenClause {
    /// The WHEN condition.
    pub when: WhenCondition,
    /// The action (BUILD or OVERLAY).
    pub action: IfThenAction,
}

impl IfThenClause {
    /// Create a new IFTHEN clause.
    pub fn new(when: WhenCondition, action: IfThenAction) -> Self {
        Self { when, action }
    }
}

/// Complete IFTHEN specification with ordered clauses.
#[derive(Debug, Clone)]
pub struct IfThenSpec {
    /// Ordered list of IFTHEN clauses.
    pub clauses: Vec<IfThenClause>,
    /// Sequence number counter (for SEQNUM overlays).
    seq_counter: u64,
    /// Current group ID (for WHEN=GROUP).
    group_id: u64,
}

impl IfThenSpec {
    /// Create a new IFTHEN specification.
    pub fn new() -> Self {
        Self {
            clauses: Vec::new(),
            seq_counter: 0,
            group_id: 0,
        }
    }

    /// Add an IFTHEN clause.
    pub fn add_clause(mut self, clause: IfThenClause) -> Self {
        self.clauses.push(clause);
        self
    }

    /// Apply IFTHEN clauses to a record.
    ///
    /// Processing order:
    /// 1. Apply all WHEN=INIT clauses
    /// 2. Evaluate WHEN=(condition) clauses — first match wins
    /// 3. Apply WHEN=NONE if no condition matched
    pub fn apply(&self, record: &[u8]) -> Vec<u8> {
        let mut result = record.to_vec();

        // Phase 1: Apply all WHEN=INIT clauses
        for clause in &self.clauses {
            if matches!(clause.when, WhenCondition::Init) {
                result = apply_action(&clause.action, &result, 0, 0);
            }
        }

        // Phase 2: Evaluate WHEN=(condition) clauses — first match wins
        let mut matched = false;
        for clause in &self.clauses {
            if let WhenCondition::Condition(ref conditions) = clause.when {
                if conditions.iter().all(|c| c.evaluate(&result)) {
                    result = apply_action(&clause.action, &result, 0, 0);
                    matched = true;
                    break;
                }
            }
        }

        // Phase 3: Apply WHEN=NONE if unmatched
        if !matched {
            for clause in &self.clauses {
                if matches!(clause.when, WhenCondition::None) {
                    result = apply_action(&clause.action, &result, 0, 0);
                    break;
                }
            }
        }

        result
    }

    /// Apply IFTHEN clauses with stateful processing (sequence numbers, groups).
    ///
    /// This variant maintains state across records for SEQNUM and GROUP features.
    pub fn apply_stateful(&mut self, record: &[u8]) -> Vec<u8> {
        self.seq_counter += 1;
        let seq = self.seq_counter;

        let mut result = record.to_vec();

        // Check for GROUP boundaries
        for clause in &self.clauses {
            if let WhenCondition::Group { ref begin } = clause.when {
                if begin.iter().all(|c| c.evaluate(&result)) {
                    self.group_id += 1;
                }
                result = apply_action(&clause.action, &result, seq, self.group_id);
            }
        }

        // Phase 1: Apply all WHEN=INIT clauses
        for clause in &self.clauses {
            if matches!(clause.when, WhenCondition::Init) {
                result = apply_action(&clause.action, &result, seq, self.group_id);
            }
        }

        // Phase 2: Evaluate WHEN=(condition) clauses — first match wins
        let mut matched = false;
        for clause in &self.clauses {
            if let WhenCondition::Condition(ref conditions) = clause.when {
                if conditions.iter().all(|c| c.evaluate(&result)) {
                    result = apply_action(&clause.action, &result, seq, self.group_id);
                    matched = true;
                    break;
                }
            }
        }

        // Phase 3: Apply WHEN=NONE if unmatched
        if !matched {
            for clause in &self.clauses {
                if matches!(clause.when, WhenCondition::None) {
                    result = apply_action(&clause.action, &result, seq, self.group_id);
                    break;
                }
            }
        }

        result
    }

    /// Reset stateful counters.
    pub fn reset(&mut self) {
        self.seq_counter = 0;
        self.group_id = 0;
    }
}

impl Default for IfThenSpec {
    fn default() -> Self {
        Self::new()
    }
}

/// Apply an IFTHEN action to a record.
fn apply_action(action: &IfThenAction, record: &[u8], seq: u64, group_id: u64) -> Vec<u8> {
    match action {
        IfThenAction::Build(ref spec) => spec.reformat(record),
        IfThenAction::Overlay(ref fields) => {
            let mut result = record.to_vec();
            for overlay in fields {
                let target_pos = overlay.position.saturating_sub(1);
                let data = match &overlay.data {
                    OverlayData::Field { position, length } => {
                        let src_start = position.saturating_sub(1);
                        let src_end = (src_start + length).min(record.len());
                        if src_start < record.len() {
                            record[src_start..src_end].to_vec()
                        } else {
                            vec![b' '; *length]
                        }
                    }
                    OverlayData::Literal(bytes) => bytes.clone(),
                    OverlayData::Spaces(n) => vec![b' '; *n],
                    OverlayData::SeqNum(width) => {
                        let formatted = format!("{:0>width$}", seq, width = *width);
                        formatted.into_bytes()
                    }
                };

                // Extend record if overlay extends beyond current length
                let needed = target_pos + data.len();
                if needed > result.len() {
                    result.resize(needed, b' ');
                }
                result[target_pos..target_pos + data.len()].copy_from_slice(&data);
            }

            // Handle group ID overlays
            if group_id > 0 {
                // Group ID is available but only applied by explicit PUSH overlays
                // The apply_action function receives it for future use
            }

            result
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fields::DataType;
    use crate::filter::{CompareOp, Condition};
    use crate::reformat::OutrecField;

    #[test]
    fn test_ifthen_when_condition_build() {
        let spec = IfThenSpec::new()
            .add_clause(IfThenClause::new(
                WhenCondition::Condition(vec![Condition {
                    position: 1,
                    length: 1,
                    data_type: DataType::Character,
                    op: CompareOp::Eq,
                    value: b"H".to_vec(),
                }]),
                IfThenAction::Build(
                    OutrecSpec::new()
                        .add_field(OutrecField::Field {
                            position: 1,
                            length: 5,
                        })
                        .add_field(OutrecField::Literal(b" HEADER".to_vec())),
                ),
            ))
            .add_clause(IfThenClause::new(
                WhenCondition::None,
                IfThenAction::Build(
                    OutrecSpec::new().add_field(OutrecField::Field {
                        position: 1,
                        length: 5,
                    }),
                ),
            ));

        // Header record
        let result = spec.apply(b"HDATA");
        assert_eq!(result, b"HDATA HEADER");

        // Non-header record
        let result = spec.apply(b"DDATA");
        assert_eq!(result, b"DDATA");
    }

    #[test]
    fn test_ifthen_when_none_as_default() {
        let spec = IfThenSpec::new()
            .add_clause(IfThenClause::new(
                WhenCondition::Condition(vec![Condition {
                    position: 1,
                    length: 1,
                    data_type: DataType::Character,
                    op: CompareOp::Eq,
                    value: b"X".to_vec(),
                }]),
                IfThenAction::Build(
                    OutrecSpec::new().add_field(OutrecField::Literal(b"MATCHED".to_vec())),
                ),
            ))
            .add_clause(IfThenClause::new(
                WhenCondition::None,
                IfThenAction::Build(
                    OutrecSpec::new().add_field(OutrecField::Literal(b"DEFAULT".to_vec())),
                ),
            ));

        assert_eq!(spec.apply(b"X123"), b"MATCHED");
        assert_eq!(spec.apply(b"A123"), b"DEFAULT");
        assert_eq!(spec.apply(b"B456"), b"DEFAULT");
    }

    #[test]
    fn test_ifthen_first_match_wins() {
        let spec = IfThenSpec::new()
            .add_clause(IfThenClause::new(
                WhenCondition::Condition(vec![Condition {
                    position: 1,
                    length: 1,
                    data_type: DataType::Character,
                    op: CompareOp::Eq,
                    value: b"A".to_vec(),
                }]),
                IfThenAction::Build(
                    OutrecSpec::new().add_field(OutrecField::Literal(b"FIRST".to_vec())),
                ),
            ))
            .add_clause(IfThenClause::new(
                WhenCondition::Condition(vec![Condition {
                    position: 1,
                    length: 1,
                    data_type: DataType::Character,
                    op: CompareOp::Eq,
                    value: b"A".to_vec(),
                }]),
                IfThenAction::Build(
                    OutrecSpec::new().add_field(OutrecField::Literal(b"SECOND".to_vec())),
                ),
            ));

        // Should match first clause only
        assert_eq!(spec.apply(b"A123"), b"FIRST");
    }

    #[test]
    fn test_ifthen_when_init() {
        let spec = IfThenSpec::new()
            .add_clause(IfThenClause::new(
                WhenCondition::Init,
                IfThenAction::Overlay(vec![OverlayField {
                    position: 6,
                    data: OverlayData::Literal(b"INIT".to_vec()),
                }]),
            ))
            .add_clause(IfThenClause::new(
                WhenCondition::Condition(vec![Condition {
                    position: 1,
                    length: 1,
                    data_type: DataType::Character,
                    op: CompareOp::Eq,
                    value: b"A".to_vec(),
                }]),
                IfThenAction::Overlay(vec![OverlayField {
                    position: 10,
                    data: OverlayData::Literal(b"OK".to_vec()),
                }]),
            ));

        // Record starts with A: should get both INIT and condition overlay
        let result = spec.apply(b"A1234");
        assert_eq!(&result[0..5], b"A1234");
        assert_eq!(&result[5..9], b"INIT");
        assert_eq!(&result[9..11], b"OK");
    }

    #[test]
    fn test_ifthen_overlay_field_copy() {
        let spec = IfThenSpec::new().add_clause(IfThenClause::new(
            WhenCondition::Init,
            IfThenAction::Overlay(vec![OverlayField {
                position: 6,
                data: OverlayData::Field {
                    position: 1,
                    length: 3,
                },
            }]),
        ));

        let result = spec.apply(b"ABCDE");
        assert_eq!(&result[0..5], b"ABCDE");
        assert_eq!(&result[5..8], b"ABC");
    }

    #[test]
    fn test_ifthen_overlay_spaces() {
        let spec = IfThenSpec::new().add_clause(IfThenClause::new(
            WhenCondition::Init,
            IfThenAction::Overlay(vec![OverlayField {
                position: 1,
                data: OverlayData::Spaces(3),
            }]),
        ));

        let result = spec.apply(b"ABCDE");
        assert_eq!(&result[0..3], b"   ");
        assert_eq!(&result[3..5], b"DE");
    }

    #[test]
    fn test_ifthen_stateful_seqnum() {
        let mut spec = IfThenSpec::new().add_clause(IfThenClause::new(
            WhenCondition::Init,
            IfThenAction::Overlay(vec![OverlayField {
                position: 6,
                data: OverlayData::SeqNum(4),
            }]),
        ));

        let r1 = spec.apply_stateful(b"REC01");
        assert_eq!(&r1[5..9], b"0001");

        let r2 = spec.apply_stateful(b"REC02");
        assert_eq!(&r2[5..9], b"0002");

        let r3 = spec.apply_stateful(b"REC03");
        assert_eq!(&r3[5..9], b"0003");
    }

    #[test]
    fn test_ifthen_group_detection() {
        let mut spec = IfThenSpec::new()
            .add_clause(IfThenClause::new(
                WhenCondition::Group {
                    begin: vec![Condition {
                        position: 1,
                        length: 3,
                        data_type: DataType::Character,
                        op: CompareOp::Eq,
                        value: b"HDR".to_vec(),
                    }],
                },
                IfThenAction::Overlay(vec![]),
            ))
            .add_clause(IfThenClause::new(
                WhenCondition::Init,
                IfThenAction::Overlay(vec![OverlayField {
                    position: 6,
                    data: OverlayData::SeqNum(4),
                }]),
            ));

        let r1 = spec.apply_stateful(b"HDR01");
        assert_eq!(&r1[0..5], b"HDR01");

        let r2 = spec.apply_stateful(b"DTL01");
        assert_eq!(&r2[0..5], b"DTL01");

        // New group
        let r3 = spec.apply_stateful(b"HDR02");
        assert_eq!(&r3[0..5], b"HDR02");

        // Verify group_id was incremented (group 1 started at HDR01, group 2 at HDR02)
        assert_eq!(spec.group_id, 2);
    }

    #[test]
    fn test_ifthen_reset() {
        let mut spec = IfThenSpec::new().add_clause(IfThenClause::new(
            WhenCondition::Init,
            IfThenAction::Overlay(vec![OverlayField {
                position: 1,
                data: OverlayData::SeqNum(3),
            }]),
        ));

        spec.apply_stateful(b"A");
        spec.apply_stateful(b"B");
        assert_eq!(spec.seq_counter, 2);

        spec.reset();
        assert_eq!(spec.seq_counter, 0);
        assert_eq!(spec.group_id, 0);

        let r = spec.apply_stateful(b"C");
        assert_eq!(&r[0..3], b"001");
    }

    #[test]
    fn test_ifthen_multiple_conditions_on_when() {
        let spec = IfThenSpec::new()
            .add_clause(IfThenClause::new(
                WhenCondition::Condition(vec![
                    Condition {
                        position: 1,
                        length: 1,
                        data_type: DataType::Character,
                        op: CompareOp::Eq,
                        value: b"A".to_vec(),
                    },
                    Condition {
                        position: 2,
                        length: 1,
                        data_type: DataType::Character,
                        op: CompareOp::Eq,
                        value: b"B".to_vec(),
                    },
                ]),
                IfThenAction::Build(
                    OutrecSpec::new().add_field(OutrecField::Literal(b"BOTH".to_vec())),
                ),
            ))
            .add_clause(IfThenClause::new(
                WhenCondition::None,
                IfThenAction::Build(
                    OutrecSpec::new().add_field(OutrecField::Literal(b"NOPE".to_vec())),
                ),
            ));

        assert_eq!(spec.apply(b"AB"), b"BOTH");
        assert_eq!(spec.apply(b"AC"), b"NOPE");
        assert_eq!(spec.apply(b"BB"), b"NOPE");
    }
}

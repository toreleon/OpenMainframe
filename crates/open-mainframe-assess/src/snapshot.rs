//! Assessment snapshot and comparison for migration progress tracking.
//!
//! Saves assessment results as JSON snapshots and provides diff reports
//! to track migration progress across multiple assessment runs.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use crate::MigrationComplexity;

/// A snapshot of an assessment run, serializable to/from JSON.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AssessmentSnapshot {
    /// Timestamp (ISO 8601 format).
    pub timestamp: String,
    /// Label for this snapshot (e.g., "baseline", "sprint-3").
    pub label: String,
    /// Per-program assessment data.
    pub programs: Vec<ProgramSnapshot>,
    /// Total lines across all programs.
    pub total_lines: usize,
    /// Total issues across all programs.
    pub total_issues: usize,
}

/// Per-program data within a snapshot.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProgramSnapshot {
    /// Program file name.
    pub file_name: String,
    /// Program ID (if detected).
    pub program_id: Option<String>,
    /// Lines of code.
    pub lines: usize,
    /// Cyclomatic complexity.
    pub cyclomatic_complexity: usize,
    /// Number of issues.
    pub issue_count: usize,
    /// Migration complexity rating.
    pub complexity: String,
    /// Whether this program is considered migrated.
    pub migrated: bool,
}

impl AssessmentSnapshot {
    /// Create a new snapshot with the given label.
    pub fn new(label: &str, timestamp: &str) -> Self {
        Self {
            timestamp: timestamp.to_string(),
            label: label.to_string(),
            programs: Vec::new(),
            total_lines: 0,
            total_issues: 0,
        }
    }

    /// Add a program to the snapshot.
    pub fn add_program(&mut self, program: ProgramSnapshot) {
        self.total_lines += program.lines;
        self.total_issues += program.issue_count;
        self.programs.push(program);
    }

    /// Serialize to JSON string.
    pub fn to_json(&self) -> Result<String, serde_json::Error> {
        serde_json::to_string_pretty(self)
    }

    /// Deserialize from JSON string.
    pub fn from_json(json: &str) -> Result<Self, serde_json::Error> {
        serde_json::from_str(json)
    }

    /// Get number of migrated programs.
    pub fn migrated_count(&self) -> usize {
        self.programs.iter().filter(|p| p.migrated).count()
    }

    /// Get number of programs.
    pub fn program_count(&self) -> usize {
        self.programs.len()
    }
}

/// Comparison result between two assessment snapshots.
#[derive(Debug, Clone, Serialize)]
pub struct SnapshotDiff {
    /// Baseline snapshot label.
    pub baseline_label: String,
    /// Current snapshot label.
    pub current_label: String,
    /// Programs that were resolved (issues dropped to zero).
    pub resolved_programs: Vec<String>,
    /// Programs with new issues (not in baseline).
    pub new_programs: Vec<String>,
    /// Programs where complexity changed.
    pub complexity_changes: Vec<ComplexityChange>,
    /// Programs marked as migrated in current but not in baseline.
    pub newly_migrated: Vec<String>,
    /// Overall progress metrics.
    pub progress: ProgressSummary,
}

/// A change in complexity rating for a program.
#[derive(Debug, Clone, Serialize)]
pub struct ComplexityChange {
    /// Program file name.
    pub file_name: String,
    /// Previous complexity rating.
    pub from: String,
    /// Current complexity rating.
    pub to: String,
}

/// Summary of migration progress between two snapshots.
#[derive(Debug, Clone, Serialize)]
pub struct ProgressSummary {
    /// Total programs in baseline.
    pub baseline_total: usize,
    /// Total programs in current snapshot.
    pub current_total: usize,
    /// Number of programs migrated.
    pub migrated_count: usize,
    /// Migration progress as percentage.
    pub progress_percent: f64,
    /// Issues resolved since baseline.
    pub issues_resolved: i64,
    /// Lines of code remaining to migrate.
    pub remaining_lines: usize,
    /// Estimated remaining effort (based on complexity multipliers).
    pub remaining_effort_hours: f64,
}

/// Compare two snapshots to produce a diff report.
pub fn compare_snapshots(baseline: &AssessmentSnapshot, current: &AssessmentSnapshot) -> SnapshotDiff {
    // Index baseline programs by file name
    let baseline_map: HashMap<&str, &ProgramSnapshot> = baseline
        .programs
        .iter()
        .map(|p| (p.file_name.as_str(), p))
        .collect();

    let current_map: HashMap<&str, &ProgramSnapshot> = current
        .programs
        .iter()
        .map(|p| (p.file_name.as_str(), p))
        .collect();

    // Resolved: programs in baseline with issues > 0, now issues == 0
    let mut resolved_programs = Vec::new();
    for (name, bp) in &baseline_map {
        if bp.issue_count > 0 {
            if let Some(cp) = current_map.get(name) {
                if cp.issue_count == 0 {
                    resolved_programs.push(name.to_string());
                }
            }
        }
    }

    // New: programs in current but not in baseline
    let new_programs: Vec<String> = current_map
        .keys()
        .filter(|k| !baseline_map.contains_key(*k))
        .map(|k| k.to_string())
        .collect();

    // Complexity changes
    let mut complexity_changes = Vec::new();
    for (name, cp) in &current_map {
        if let Some(bp) = baseline_map.get(name) {
            if bp.complexity != cp.complexity {
                complexity_changes.push(ComplexityChange {
                    file_name: name.to_string(),
                    from: bp.complexity.clone(),
                    to: cp.complexity.clone(),
                });
            }
        }
    }

    // Newly migrated
    let newly_migrated: Vec<String> = current_map
        .iter()
        .filter(|(name, cp)| {
            cp.migrated
                && baseline_map
                    .get(*name)
                    .map_or(true, |bp| !bp.migrated)
        })
        .map(|(name, _)| name.to_string())
        .collect();

    // Progress summary
    let migrated_count = current.migrated_count();
    let baseline_total = baseline.program_count();
    let current_total = current.program_count();
    let progress_percent = if baseline_total > 0 {
        (migrated_count as f64 / baseline_total as f64) * 100.0
    } else {
        0.0
    };

    let issues_resolved =
        baseline.total_issues as i64 - current.total_issues as i64;

    let remaining_lines: usize = current
        .programs
        .iter()
        .filter(|p| !p.migrated)
        .map(|p| p.lines)
        .sum();

    let remaining_effort_hours: f64 = current
        .programs
        .iter()
        .filter(|p| !p.migrated)
        .map(|p| {
            let mult = complexity_to_multiplier(&p.complexity);
            (p.lines as f64 / 100.0) * mult
        })
        .sum();

    SnapshotDiff {
        baseline_label: baseline.label.clone(),
        current_label: current.label.clone(),
        resolved_programs,
        new_programs,
        complexity_changes,
        newly_migrated,
        progress: ProgressSummary {
            baseline_total,
            current_total,
            migrated_count,
            progress_percent,
            issues_resolved,
            remaining_lines,
            remaining_effort_hours,
        },
    }
}

/// Map complexity string to effort multiplier.
fn complexity_to_multiplier(complexity: &str) -> f64 {
    match complexity {
        "Low" => MigrationComplexity::Low.effort_multiplier(),
        "Medium" => MigrationComplexity::Medium.effort_multiplier(),
        "High" => MigrationComplexity::High.effort_multiplier(),
        "VeryHigh" => MigrationComplexity::VeryHigh.effort_multiplier(),
        _ => 1.0,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_baseline() -> AssessmentSnapshot {
        let mut snap = AssessmentSnapshot::new("baseline", "2025-01-01T00:00:00Z");
        snap.add_program(ProgramSnapshot {
            file_name: "PROG-A.cbl".to_string(),
            program_id: Some("PROGA".to_string()),
            lines: 500,
            cyclomatic_complexity: 12,
            issue_count: 3,
            complexity: "Medium".to_string(),
            migrated: false,
        });
        snap.add_program(ProgramSnapshot {
            file_name: "PROG-B.cbl".to_string(),
            program_id: Some("PROGB".to_string()),
            lines: 200,
            cyclomatic_complexity: 5,
            issue_count: 1,
            complexity: "Low".to_string(),
            migrated: false,
        });
        snap.add_program(ProgramSnapshot {
            file_name: "PROG-C.cbl".to_string(),
            program_id: Some("PROGC".to_string()),
            lines: 800,
            cyclomatic_complexity: 30,
            issue_count: 7,
            complexity: "High".to_string(),
            migrated: false,
        });
        snap
    }

    fn make_current() -> AssessmentSnapshot {
        let mut snap = AssessmentSnapshot::new("sprint-3", "2025-03-01T00:00:00Z");
        snap.add_program(ProgramSnapshot {
            file_name: "PROG-A.cbl".to_string(),
            program_id: Some("PROGA".to_string()),
            lines: 500,
            cyclomatic_complexity: 12,
            issue_count: 0, // resolved
            complexity: "Low".to_string(), // improved
            migrated: true, // migrated
        });
        snap.add_program(ProgramSnapshot {
            file_name: "PROG-B.cbl".to_string(),
            program_id: Some("PROGB".to_string()),
            lines: 200,
            cyclomatic_complexity: 5,
            issue_count: 1,
            complexity: "Low".to_string(),
            migrated: false,
        });
        snap.add_program(ProgramSnapshot {
            file_name: "PROG-C.cbl".to_string(),
            program_id: Some("PROGC".to_string()),
            lines: 800,
            cyclomatic_complexity: 25,
            issue_count: 4, // reduced
            complexity: "Medium".to_string(), // improved
            migrated: false,
        });
        snap
    }

    #[test]
    fn test_snapshot_serialization_roundtrip() {
        let snap = make_baseline();
        let json = snap.to_json().unwrap();
        let restored = AssessmentSnapshot::from_json(&json).unwrap();
        assert_eq!(restored.label, "baseline");
        assert_eq!(restored.programs.len(), 3);
        assert_eq!(restored.total_lines, 1500);
        assert_eq!(restored.total_issues, 11);
    }

    #[test]
    fn test_snapshot_migrated_count() {
        let snap = make_current();
        assert_eq!(snap.migrated_count(), 1);
    }

    #[test]
    fn test_compare_resolved_programs() {
        let baseline = make_baseline();
        let current = make_current();
        let diff = compare_snapshots(&baseline, &current);
        assert!(diff.resolved_programs.contains(&"PROG-A.cbl".to_string()));
    }

    #[test]
    fn test_compare_complexity_changes() {
        let baseline = make_baseline();
        let current = make_current();
        let diff = compare_snapshots(&baseline, &current);
        assert_eq!(diff.complexity_changes.len(), 2); // PROG-A: Medium->Low, PROG-C: High->Medium
        let prog_a = diff
            .complexity_changes
            .iter()
            .find(|c| c.file_name == "PROG-A.cbl")
            .unwrap();
        assert_eq!(prog_a.from, "Medium");
        assert_eq!(prog_a.to, "Low");
    }

    #[test]
    fn test_compare_newly_migrated() {
        let baseline = make_baseline();
        let current = make_current();
        let diff = compare_snapshots(&baseline, &current);
        assert!(diff.newly_migrated.contains(&"PROG-A.cbl".to_string()));
        assert_eq!(diff.newly_migrated.len(), 1);
    }

    #[test]
    fn test_progress_summary() {
        let baseline = make_baseline();
        let current = make_current();
        let diff = compare_snapshots(&baseline, &current);
        assert_eq!(diff.progress.baseline_total, 3);
        assert_eq!(diff.progress.migrated_count, 1);
        assert!((diff.progress.progress_percent - 33.33).abs() < 1.0);
        assert_eq!(diff.progress.issues_resolved, 6); // 11 - 5 = 6
    }

    #[test]
    fn test_new_programs_detected() {
        let baseline = make_baseline();
        let mut current = make_current();
        current.add_program(ProgramSnapshot {
            file_name: "PROG-D.cbl".to_string(),
            program_id: Some("PROGD".to_string()),
            lines: 100,
            cyclomatic_complexity: 3,
            issue_count: 0,
            complexity: "Low".to_string(),
            migrated: false,
        });
        let diff = compare_snapshots(&baseline, &current);
        assert!(diff.new_programs.contains(&"PROG-D.cbl".to_string()));
    }

    #[test]
    fn test_remaining_effort_calculation() {
        let baseline = make_baseline();
        let current = make_current();
        let diff = compare_snapshots(&baseline, &current);
        // PROG-B: 200 lines, Low (1.0x) → 2.0 hours
        // PROG-C: 800 lines, Medium (2.0x) → 16.0 hours
        // Total: 18.0 hours
        assert!((diff.progress.remaining_effort_hours - 18.0).abs() < 0.01);
    }

    #[test]
    fn test_empty_snapshots() {
        let baseline = AssessmentSnapshot::new("empty-base", "2025-01-01T00:00:00Z");
        let current = AssessmentSnapshot::new("empty-cur", "2025-02-01T00:00:00Z");
        let diff = compare_snapshots(&baseline, &current);
        assert_eq!(diff.progress.progress_percent, 0.0);
        assert!(diff.resolved_programs.is_empty());
        assert!(diff.newly_migrated.is_empty());
    }
}

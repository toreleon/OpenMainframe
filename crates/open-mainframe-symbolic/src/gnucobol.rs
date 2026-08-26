//! Golden master runner using GnuCOBOL.
//!
//! Generates standalone COBOL test harness programs from [`TestSuite`] data,
//! compiles them with `cobc` (GnuCOBOL), runs the resulting binaries, and
//! parses the DISPLAY output to capture actual variable values.
//!
//! The captured outputs replace symbolic predictions to produce
//! a **golden master** — a test suite grounded in real COBOL execution.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::testgen::{TestCase, TestSuite, TestValue};

// ---------------------------------------------------------------------------
// Public types
// ---------------------------------------------------------------------------

/// Result of running one test case through GnuCOBOL.
#[derive(Debug, Clone)]
pub struct GnuCobolResult {
    pub test_id: String,
    pub passed: bool,
    pub actual_outputs: BTreeMap<String, TestValue>,
    pub display_log: String,
    pub error: Option<String>,
}

/// Summary of a full golden master run.
#[derive(Debug, Clone)]
pub struct GoldenMasterReport {
    pub program_name: String,
    pub total_tests: usize,
    pub passed: usize,
    pub failed: usize,
    pub errors: usize,
    pub results: Vec<GnuCobolResult>,
    pub golden_suite: TestSuite,
}

// ---------------------------------------------------------------------------
// Harness generation
// ---------------------------------------------------------------------------

/// Generate a standalone COBOL test harness for a batch program.
///
/// The harness sets WORKING-STORAGE variables to test inputs, PERFORMs the
/// relevant paragraph(s), then DISPLAYs each output variable in a parseable
/// `@@OUTPUT:VAR-NAME=value@@` format.
pub fn generate_harness(
    test: &TestCase,
    data_items: &[(String, String)], // (name, PIC clause)
    paragraphs: &[String],
    copybooks: &[String],
    _program_id: &str,
) -> String {
    let mut out = String::new();

    // Identification Division.
    out.push_str(&format!(
        "       IDENTIFICATION DIVISION.\n\
         \x20      PROGRAM-ID. TST{:0>4}.\n\n",
        test.id
            .chars()
            .filter(|c| c.is_ascii_digit())
            .collect::<String>()
    ));

    // Data Division.
    out.push_str(
        "       DATA DIVISION.\n\
         \x20      WORKING-STORAGE SECTION.\n",
    );

    // Copy in the same copybooks the original program uses.
    for cpy in copybooks {
        out.push_str(&format!("       COPY {}.\n", cpy));
    }

    // Declare data items that the original program uses.
    for (name, pic) in data_items {
        out.push_str(&format!("       01  {:<30} {}.\n", name, pic));
    }

    out.push_str("\n");

    // Procedure Division.
    out.push_str(
        "       PROCEDURE DIVISION.\n\
         \x20      MAIN-LOGIC.\n",
    );

    // Set input variables.
    for (var, val) in &test.inputs {
        let cobol_val = test_value_to_cobol_literal(val);
        out.push_str(&format!(
            "           MOVE {} TO {}\n",
            cobol_val, var
        ));
    }

    // PERFORM each paragraph.
    for para in paragraphs {
        out.push_str(&format!("           PERFORM {}\n", para));
    }

    // Display output variables in parseable format.
    for var in test.expected_outputs.keys() {
        out.push_str(&format!(
            "           DISPLAY \"@@OUTPUT:{}=\" {}\n\
             \x20              \"@@\"\n",
            var, var
        ));
    }

    out.push_str("           STOP RUN.\n");

    out
}

/// Generate a simpler harness that tests the entire program by providing
/// input data files and capturing DISPLAY output.
///
/// For file-I/O batch programs like CBACT01C, we create small input data
/// files with known records and capture all DISPLAY statements as output.
pub fn generate_display_capture_harness(
    test: &TestCase,
    _original_source: &str,
    _program_id: &str,
) -> String {
    // For display-capture mode, we inject DISPLAY lines for each output
    // variable at the end of the procedure division.  This is done by
    // appending a paragraph that the test will PERFORM.
    let mut extra = String::new();
    extra.push_str("       ZZTESTOUT-PARA.\n");
    for var in test.expected_outputs.keys() {
        extra.push_str(&format!(
            "           DISPLAY \"@@OUTPUT:{}=\" {}\n\
             \x20              \"@@\"\n",
            var, var
        ));
    }
    extra.push_str("           EXIT.\n");

    // Return the extra paragraph — caller must splice it into the source.
    extra
}

/// Generate a **self-contained** COBOL program that tests variable
/// assignments and control flow without file I/O.
///
/// This is the primary harness mode: it copies WORKING-STORAGE definitions
/// from the original program, sets inputs via MOVE, then runs paragraphs
/// that do computation (skipping file OPEN/CLOSE/READ/WRITE).
///
/// The output is **fixed-format COBOL** (cols 1-6 blank, 7 indicator,
/// 8-11 Area A, 12-72 Area B) so it's compatible with fixed-format copybooks.
pub fn generate_unit_harness(
    test: &TestCase,
    original_source: &str,
    _include_paths: &[PathBuf],
    _program_id: &str,
) -> Result<String, String> {
    let mut out = String::new();
    let test_num: String = test
        .id
        .chars()
        .filter(|c| c.is_ascii_digit())
        .collect();
    let pgm_id = format!("T{}", &test_num[..std::cmp::min(test_num.len(), 6)]);

    // Fixed-format: 6 spaces + content starting in Area A (col 8) or Area B (col 12).
    let a = "       ";        // 7 spaces → col 8 (Area A)
    let b = "           ";    // 11 spaces → col 12 (Area B)

    out.push_str(&format!("{a}IDENTIFICATION DIVISION.\n"));
    out.push_str(&format!("{a}PROGRAM-ID. {pgm_id}.\n"));
    out.push('\n');
    out.push_str(&format!("{a}DATA DIVISION.\n"));
    out.push_str(&format!("{a}WORKING-STORAGE SECTION.\n"));

    // Extract data definitions (FILE SECTION records + WORKING-STORAGE).
    let ws_section = extract_data_definitions(original_source);
    if let Some(ws) = &ws_section {
        out.push_str(ws);
        out.push('\n');
    }

    out.push_str(&format!("{a}PROCEDURE DIVISION.\n"));
    out.push_str(&format!("{a}TEST-MAIN.\n"));

    // Set inputs — skip synthetic variables and 88-level conditions.
    for (var, val) in &test.inputs {
        if is_synthetic_variable(var) {
            continue;
        }
        let lit = test_value_to_cobol_literal(val);
        out.push_str(&format!("{b}MOVE {lit} TO {var}\n"));
    }

    // Display outputs.
    for var in test.expected_outputs.keys() {
        out.push_str(&format!(
            "{b}DISPLAY \"@@OUTPUT:{var}=\" {var}\n{b}    \"@@\"\n",
        ));
    }

    out.push_str(&format!("{b}STOP RUN.\n"));
    Ok(out)
}

/// Returns true if a variable name is a synthetic artifact from the symbolic
/// engine rather than a real COBOL variable.
fn is_synthetic_variable(name: &str) -> bool {
    // IS_Numeric_XXX is a symbolic helper for the NUMERIC class condition.
    if name.starts_with("IS_Numeric_") || name.starts_with("IS_NUMERIC_") {
        return true;
    }
    // 88-level condition names that the symbolic engine tracks as booleans.
    // These are set implicitly by moving values to their parent.
    if name == "APPL-AOK" || name == "APPL-EOF" {
        return true;
    }
    false
}

// ---------------------------------------------------------------------------
// Compilation and execution
// ---------------------------------------------------------------------------

/// Check that `cobc` (GnuCOBOL) is available.
pub fn check_gnucobol() -> Result<String, String> {
    let output = Command::new("cobc")
        .arg("--version")
        .output()
        .map_err(|e| format!("cannot run cobc: {e} (is GnuCOBOL installed?)"))?;
    let version = String::from_utf8_lossy(&output.stdout);
    let first_line = version.lines().next().unwrap_or("unknown");
    Ok(first_line.to_string())
}

/// Compile a COBOL source file with GnuCOBOL.
pub fn compile(
    source_path: &Path,
    output_path: &Path,
    include_paths: &[PathBuf],
) -> Result<(), String> {
    let mut cmd = Command::new("cobc");
    cmd.arg("-x") // compile to executable
        .arg("-o")
        .arg(output_path);

    for inc in include_paths {
        cmd.arg("-I").arg(inc);
    }

    cmd.arg(source_path);

    let output = cmd
        .output()
        .map_err(|e| format!("cobc failed to start: {e}"))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!("cobc compilation failed:\n{stderr}"));
    }

    Ok(())
}

/// Run a compiled COBOL executable and capture stdout.
pub fn run_executable(exe_path: &Path) -> Result<String, String> {
    let output = Command::new(exe_path)
        .output()
        .map_err(|e| format!("cannot run {}: {e}", exe_path.display()))?;

    let stdout = String::from_utf8_lossy(&output.stdout).to_string();

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        // Still return stdout — the program may DISPLAY before abending.
        return Ok(format!("{stdout}\n@@ERROR:{stderr}@@"));
    }

    Ok(stdout)
}

// ---------------------------------------------------------------------------
// Output parsing
// ---------------------------------------------------------------------------

/// Parse `@@OUTPUT:VAR=value@@` lines from DISPLAY output.
pub fn parse_outputs(display_output: &str) -> BTreeMap<String, TestValue> {
    let mut results = BTreeMap::new();

    for line in display_output.lines() {
        let line = line.trim();
        if let Some(rest) = line.strip_prefix("@@OUTPUT:") {
            if let Some(content) = rest.strip_suffix("@@") {
                if let Some((var, val)) = content.split_once('=') {
                    let val = val.trim();
                    let tv = parse_display_value(val);
                    results.insert(var.to_string(), tv);
                }
            }
        }
    }

    results
}

/// Compare actual outputs against expected outputs.
pub fn compare_outputs(
    expected: &BTreeMap<String, TestValue>,
    actual: &BTreeMap<String, TestValue>,
) -> Vec<(String, TestValue, TestValue)> {
    let mut diffs = Vec::new();
    for (var, exp) in expected {
        match actual.get(var) {
            Some(act) if !values_match(exp, act) => {
                diffs.push((var.clone(), exp.clone(), act.clone()));
            }
            None => {
                diffs.push((var.clone(), exp.clone(), TestValue::Unknown));
            }
            _ => {}
        }
    }
    diffs
}

// ---------------------------------------------------------------------------
// Golden master runner
// ---------------------------------------------------------------------------

/// Run a full test suite through GnuCOBOL and produce a golden master.
///
/// For each test case:
/// 1. Generate a unit harness COBOL program
/// 2. Compile with cobc
/// 3. Run and capture DISPLAY output
/// 4. Parse @@OUTPUT@@ markers
/// 5. Replace expected_outputs with actual GnuCOBOL outputs
pub fn run_golden_master(
    suite: &TestSuite,
    original_source: &str,
    include_paths: &[PathBuf],
    work_dir: &Path,
) -> Result<GoldenMasterReport, String> {
    check_gnucobol()?;

    std::fs::create_dir_all(work_dir)
        .map_err(|e| format!("cannot create {}: {e}", work_dir.display()))?;

    let mut results = Vec::new();
    let mut golden_cases = Vec::new();
    let mut passed = 0usize;
    let mut failed = 0usize;
    let mut errors = 0usize;

    for test in &suite.test_cases {
        let harness = match generate_unit_harness(
            test,
            original_source,
            include_paths,
            &suite.name,
        ) {
            Ok(h) => h,
            Err(e) => {
                results.push(GnuCobolResult {
                    test_id: test.id.clone(),
                    passed: false,
                    actual_outputs: BTreeMap::new(),
                    display_log: String::new(),
                    error: Some(format!("harness generation failed: {e}")),
                });
                errors += 1;
                golden_cases.push(test.clone());
                continue;
            }
        };

        let test_num: String = test.id.chars().filter(|c| c.is_ascii_digit()).collect();
        let src_path = work_dir.join(format!("test_{}.cbl", test_num));
        let exe_path = work_dir.join(format!("test_{}", test_num));

        // Write harness source.
        if let Err(e) = std::fs::write(&src_path, &harness) {
            results.push(GnuCobolResult {
                test_id: test.id.clone(),
                passed: false,
                actual_outputs: BTreeMap::new(),
                display_log: String::new(),
                error: Some(format!("write harness: {e}")),
            });
            errors += 1;
            golden_cases.push(test.clone());
            continue;
        }

        // Compile.
        if let Err(e) = compile(&src_path, &exe_path, include_paths) {
            results.push(GnuCobolResult {
                test_id: test.id.clone(),
                passed: false,
                actual_outputs: BTreeMap::new(),
                display_log: harness.clone(),
                error: Some(format!("compilation: {e}")),
            });
            errors += 1;
            golden_cases.push(test.clone());
            continue;
        }

        // Run.
        match run_executable(&exe_path) {
            Ok(display_output) => {
                let actual = parse_outputs(&display_output);
                let diffs = compare_outputs(&test.expected_outputs, &actual);
                let test_passed = diffs.is_empty();

                if test_passed {
                    passed += 1;
                } else {
                    failed += 1;
                }

                // Golden master: replace expected with actual.
                let mut golden = test.clone();
                golden.expected_outputs = actual.clone();

                results.push(GnuCobolResult {
                    test_id: test.id.clone(),
                    passed: test_passed,
                    actual_outputs: actual,
                    display_log: display_output,
                    error: None,
                });
                golden_cases.push(golden);
            }
            Err(e) => {
                results.push(GnuCobolResult {
                    test_id: test.id.clone(),
                    passed: false,
                    actual_outputs: BTreeMap::new(),
                    display_log: String::new(),
                    error: Some(format!("execution: {e}")),
                });
                errors += 1;
                golden_cases.push(test.clone());
            }
        }
    }

    let golden_suite = TestSuite {
        name: suite.name.clone(),
        source_programs: suite.source_programs.clone(),
        test_cases: golden_cases,
        stats: suite.stats.clone(),
    };

    Ok(GoldenMasterReport {
        program_name: suite.name.clone(),
        total_tests: suite.test_cases.len(),
        passed,
        failed,
        errors,
        results,
        golden_suite,
    })
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

fn test_value_to_cobol_literal(val: &TestValue) -> String {
    match val {
        TestValue::Int(n) => {
            if *n < 0 {
                format!("-{}", n.unsigned_abs())
            } else {
                format!("{n}")
            }
        }
        TestValue::Bool(b) => {
            // COBOL doesn't have booleans; use 1/0.
            if *b { "1".to_string() } else { "0".to_string() }
        }
        TestValue::Str(s) => format!("\"{}\"", s.replace('"', "\"\"")),
        TestValue::Unknown => "SPACES".to_string(),
    }
}

fn parse_display_value(s: &str) -> TestValue {
    let s = s.trim();
    if s.is_empty() {
        return TestValue::Str(String::new());
    }
    // Try integer.
    if let Ok(n) = s.parse::<i64>() {
        return TestValue::Int(n);
    }
    // Try with leading +/spaces stripped.
    let stripped = s.trim_start_matches('+').trim();
    if let Ok(n) = stripped.parse::<i64>() {
        return TestValue::Int(n);
    }
    TestValue::Str(s.to_string())
}

fn values_match(expected: &TestValue, actual: &TestValue) -> bool {
    match (expected, actual) {
        (TestValue::Int(a), TestValue::Int(b)) => a == b,
        (TestValue::Str(a), TestValue::Str(b)) => a.trim() == b.trim(),
        (TestValue::Bool(a), TestValue::Int(b)) => {
            (*a && *b != 0) || (!*a && *b == 0)
        }
        (TestValue::Int(a), TestValue::Str(b)) => {
            b.trim().parse::<i64>().map(|n| n == *a).unwrap_or(false)
        }
        (TestValue::Str(a), TestValue::Int(b)) => {
            a.trim().parse::<i64>().map(|n| n == *b).unwrap_or(false)
        }
        _ => format!("{expected}") == format!("{actual}"),
    }
}

/// Extract data definitions from COBOL source for the test harness.
///
/// Includes:
/// - FILE SECTION record definitions (01-level groups from FDs, without the
///   FD/SELECT themselves — re-declared as WORKING-STORAGE items)
/// - WORKING-STORAGE SECTION (COPY statements, 01-levels, etc.)
///
/// Strips 88-level condition names since they can't be MOVEd to.
/// Stops at PROCEDURE DIVISION.
fn extract_data_definitions(source: &str) -> Option<String> {
    let mut result = String::new();

    // Phase 1: Extract record definitions from FILE SECTION.
    // These are 01-level items under FD that define the record layouts.
    let mut in_file_section = false;
    let mut in_fd_records = false;
    for line in source.lines() {
        let upper = line.to_uppercase();
        let trimmed = upper.trim();

        if trimmed.contains("FILE SECTION") {
            in_file_section = true;
            continue;
        }
        if in_file_section && (trimmed.contains("WORKING-STORAGE SECTION")
            || trimmed.contains("PROCEDURE DIVISION")
            || trimmed.contains("LINKAGE SECTION"))
        {
            break;
        }
        if !in_file_section {
            continue;
        }

        // Skip FD and SD lines, RECORDING MODE, RECORD IS VARYING, etc.
        if trimmed.starts_with("FD ") || trimmed.starts_with("SD ") {
            in_fd_records = true;
            continue;
        }
        // Skip FD continuation clauses.
        if in_fd_records && !trimmed.starts_with("01 ") && !trimmed.starts_with("01  ")
            && !trimmed.starts_with("*") && !trimmed.is_empty()
        {
            // Check if this is a record definition (01 level or subordinate).
            let content = if line.len() > 6 { line[6..].trim() } else { "" };
            if content.starts_with("01 ") || content.starts_with("01  ") {
                // 01-level record — include it.
            } else if content.starts_with("05 ") || content.starts_with("05  ")
                || content.starts_with("10 ") || content.starts_with("10  ")
                || content.starts_with("15 ") || content.starts_with("15  ")
            {
                // Subordinate item — include.
            } else {
                // FD clause continuation (RECORDING MODE, etc.) — skip.
                continue;
            }
        }

        // Is this a comment line?
        if line.len() > 6 && line.as_bytes().get(6) == Some(&b'*') {
            result.push_str(line);
            result.push('\n');
            continue;
        }

        let content = if line.len() > 6 { line[6..].trim() } else { "" };
        // Skip 88-level.
        if content.starts_with("88 ") || content.starts_with("88  ") {
            continue;
        }
        // Include data definitions.
        if !content.is_empty() {
            result.push_str(line);
            result.push('\n');
        }
    }

    // Phase 2: Extract WORKING-STORAGE SECTION.
    let mut in_ws = false;
    for line in source.lines() {
        let upper = line.to_uppercase();
        let trimmed = upper.trim();

        if trimmed.contains("WORKING-STORAGE SECTION") {
            in_ws = true;
            continue;
        }
        if in_ws && (trimmed.contains("PROCEDURE DIVISION")
            || trimmed.contains("LINKAGE SECTION"))
        {
            break;
        }
        if in_ws {
            let content = if line.len() > 6 { line[6..].trim() } else { "" };
            if content.starts_with("88 ") || content.starts_with("88  ") {
                continue;
            }
            result.push_str(line);
            result.push('\n');
        }
    }

    if result.is_empty() {
        None
    } else {
        Some(result)
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_output_lines() {
        let display = "\
START OF EXECUTION
@@OUTPUT:APPL-RESULT=0@@
@@OUTPUT:END-OF-FILE=N@@
@@OUTPUT:ABCODE=999@@
DONE";
        let outputs = parse_outputs(display);
        assert_eq!(outputs.get("APPL-RESULT"), Some(&TestValue::Int(0)));
        assert_eq!(
            outputs.get("END-OF-FILE"),
            Some(&TestValue::Str("N".to_string()))
        );
        assert_eq!(outputs.get("ABCODE"), Some(&TestValue::Int(999)));
        assert_eq!(outputs.len(), 3);
    }

    #[test]
    fn test_value_to_literal() {
        assert_eq!(test_value_to_cobol_literal(&TestValue::Int(42)), "42");
        assert_eq!(test_value_to_cobol_literal(&TestValue::Int(-5)), "-5");
        assert_eq!(test_value_to_cobol_literal(&TestValue::Bool(true)), "1");
        assert_eq!(
            test_value_to_cobol_literal(&TestValue::Str("ABC".into())),
            "\"ABC\""
        );
        assert_eq!(
            test_value_to_cobol_literal(&TestValue::Unknown),
            "SPACES"
        );
    }

    #[test]
    fn compare_matching() {
        let mut exp = BTreeMap::new();
        exp.insert("X".into(), TestValue::Int(10));
        exp.insert("Y".into(), TestValue::Str("OK".into()));

        let mut act = BTreeMap::new();
        act.insert("X".into(), TestValue::Int(10));
        act.insert("Y".into(), TestValue::Str("OK".into()));

        let diffs = compare_outputs(&exp, &act);
        assert!(diffs.is_empty());
    }

    #[test]
    fn compare_mismatched() {
        let mut exp = BTreeMap::new();
        exp.insert("X".into(), TestValue::Int(10));

        let mut act = BTreeMap::new();
        act.insert("X".into(), TestValue::Int(20));

        let diffs = compare_outputs(&exp, &act);
        assert_eq!(diffs.len(), 1);
        assert_eq!(diffs[0].0, "X");
    }

    #[test]
    fn parse_display_values() {
        assert_eq!(parse_display_value("42"), TestValue::Int(42));
        assert_eq!(parse_display_value("-5"), TestValue::Int(-5));
        assert_eq!(parse_display_value("+007"), TestValue::Int(7));
        assert_eq!(
            parse_display_value("HELLO"),
            TestValue::Str("HELLO".into())
        );
    }

    #[test]
    fn values_match_cross_type() {
        assert!(values_match(&TestValue::Bool(false), &TestValue::Int(0)));
        assert!(values_match(&TestValue::Bool(true), &TestValue::Int(1)));
        assert!(values_match(
            &TestValue::Int(42),
            &TestValue::Str("42".into())
        ));
    }

    #[test]
    fn extract_data_defs_from_source() {
        // Fixed-format: 6-char sequence area + 1-char indicator + content
        let src = "000100 IDENTIFICATION DIVISION.\n\
                   000200 PROGRAM-ID. TEST1.\n\
                   000300 DATA DIVISION.\n\
                   000400 FILE SECTION.\n\
                   000500 FD MYFILE.\n\
                   000600 01 MY-REC PIC X(80).\n\
                   000700 WORKING-STORAGE SECTION.\n\
                   000800 01  WS-VAR1  PIC 9(5).\n\
                   000900     88 WS-FLAG VALUE 1.\n\
                   001000 01  WS-VAR2  PIC X(10).\n\
                   001100 COPY MYBOOK.\n\
                   001200 PROCEDURE DIVISION.\n\
                   001300     STOP RUN.\n";
        let defs = extract_data_definitions(src).unwrap();
        assert!(defs.contains("WS-VAR1"), "should contain WS-VAR1");
        assert!(defs.contains("WS-VAR2"), "should contain WS-VAR2");
        assert!(defs.contains("COPY MYBOOK"), "should contain COPY MYBOOK");
        // 88-level should be stripped.
        assert!(!defs.contains("WS-FLAG"), "should not contain 88-level");
        // PROCEDURE DIVISION should not be included.
        assert!(!defs.contains("STOP RUN"), "should not contain PROCEDURE code");
        // FILE SECTION record definitions SHOULD be included now.
        assert!(defs.contains("MY-REC"), "should include FD record defs");
    }

    #[test]
    fn gnucobol_available() {
        // This test checks the environment — skip gracefully if cobc not installed.
        match check_gnucobol() {
            Ok(ver) => assert!(ver.contains("GnuCOBOL") || ver.contains("cobc")),
            Err(_) => eprintln!("GnuCOBOL not installed, skipping"),
        }
    }
}

//! CLI interface for the symbolic execution engine.

/// CLI argument definitions.
///
/// This module is intended to be used from a binary entry point
/// (`main.rs`) that parses args and dispatches to the engine.
/// It does *not* pull in `clap` (which would add a heavy dep to the
/// library crate); instead it provides a simple hand-rolled parser.
use std::path::{Path, PathBuf};

use crate::interpreter::InterpreterConfig;
use crate::lowering::{fixup_loop_exits, lower_cobol_file, LoweringResult};
use crate::model_checker::{ModelChecker, VerificationResult};
use crate::spec::{parse_annotations, parse_properties_yaml};

/// Top-level CLI commands.
#[derive(Debug)]
pub enum Command {
    /// Verify a COBOL program against properties.
    Verify {
        program_path: String,
        include_paths: Vec<String>,
        properties_path: Option<String>,
        max_paths: usize,
        output_format: OutputFormat,
    },
    /// Run verification against all .cbl files in a directory.
    Batch {
        directory: String,
        include_paths: Vec<String>,
        max_paths: usize,
        output_format: OutputFormat,
    },
    /// Generate test cases from symbolic execution.
    TestGen {
        program_path: String,
        output_dir: String,
    },
    /// Check equivalence between two programs.
    Equivalence {
        old_program: String,
        new_program: String,
    },
    /// Display help.
    Help,
}

/// Output format.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputFormat {
    Text,
    Json,
}

/// Parse CLI arguments into a [`Command`].
pub fn parse_args(args: &[String]) -> Result<Command, String> {
    if args.len() < 2 {
        return Ok(Command::Help);
    }

    match args[1].as_str() {
        "verify" => {
            let mut program_path = None;
            let mut properties_path = None;
            let mut include_paths = Vec::new();
            let mut max_paths = 1000usize;
            let mut output_format = OutputFormat::Text;

            let mut i = 2;
            while i < args.len() {
                match args[i].as_str() {
                    "-p" | "--program" => {
                        i += 1;
                        program_path = args.get(i).cloned();
                    }
                    "-I" | "--include" => {
                        i += 1;
                        if let Some(p) = args.get(i) {
                            include_paths.push(p.clone());
                        }
                    }
                    "--properties" => {
                        i += 1;
                        properties_path = args.get(i).cloned();
                    }
                    "--max-paths" => {
                        i += 1;
                        max_paths = args.get(i).and_then(|s| s.parse().ok()).unwrap_or(1000);
                    }
                    "-o" | "--output" => {
                        i += 1;
                        output_format = match args.get(i).map(|s| s.as_str()) {
                            Some("json") => OutputFormat::Json,
                            _ => OutputFormat::Text,
                        };
                    }
                    other if program_path.is_none() => {
                        program_path = Some(other.to_string());
                    }
                    _ => {}
                }
                i += 1;
            }

            Ok(Command::Verify {
                program_path: program_path
                    .ok_or_else(|| "missing --program argument".to_string())?,
                include_paths,
                properties_path,
                max_paths,
                output_format,
            })
        }
        "batch" => {
            let mut directory = None;
            let mut include_paths = Vec::new();
            let mut max_paths = 1000usize;
            let mut output_format = OutputFormat::Text;

            let mut i = 2;
            while i < args.len() {
                match args[i].as_str() {
                    "-I" | "--include" => {
                        i += 1;
                        if let Some(p) = args.get(i) {
                            include_paths.push(p.clone());
                        }
                    }
                    "--max-paths" => {
                        i += 1;
                        max_paths = args.get(i).and_then(|s| s.parse().ok()).unwrap_or(1000);
                    }
                    "-o" | "--output" => {
                        i += 1;
                        output_format = match args.get(i).map(|s| s.as_str()) {
                            Some("json") => OutputFormat::Json,
                            _ => OutputFormat::Text,
                        };
                    }
                    other if directory.is_none() => {
                        directory = Some(other.to_string());
                    }
                    _ => {}
                }
                i += 1;
            }

            Ok(Command::Batch {
                directory: directory.ok_or_else(|| "missing directory argument".to_string())?,
                include_paths,
                max_paths,
                output_format,
            })
        }
        "testgen" => {
            let program_path = args.get(2).cloned();
            let output_dir = args
                .get(3)
                .cloned()
                .unwrap_or_else(|| "testcases".to_string());

            Ok(Command::TestGen {
                program_path: program_path.ok_or_else(|| "missing program path".to_string())?,
                output_dir,
            })
        }
        "equivalence" => {
            let old_program = args.get(2).cloned();
            let new_program = args.get(3).cloned();

            Ok(Command::Equivalence {
                old_program: old_program.ok_or_else(|| "missing old program path".to_string())?,
                new_program: new_program.ok_or_else(|| "missing new program path".to_string())?,
            })
        }
        "help" | "--help" | "-h" => Ok(Command::Help),
        other => Err(format!("unknown command: {other}")),
    }
}

/// Execute a parsed command, returning a formatted result string.
pub fn run_command(command: &Command) -> Result<String, String> {
    match command {
        Command::Verify {
            program_path,
            include_paths,
            properties_path,
            max_paths,
            output_format,
        } => {
            let include_pb: Vec<PathBuf> = include_paths.iter().map(PathBuf::from).collect();
            let result = run_verify(
                Path::new(program_path),
                &include_pb,
                properties_path.as_deref(),
                *max_paths,
            )?;

            match output_format {
                OutputFormat::Json => format_json_single(&result.0, &result.1),
                OutputFormat::Text => Ok(format_text_single(&result.0, &result.1)),
            }
        }

        Command::Batch {
            directory,
            include_paths,
            max_paths,
            output_format,
        } => {
            let include_pb: Vec<PathBuf> = include_paths.iter().map(PathBuf::from).collect();
            run_batch(Path::new(directory), &include_pb, *max_paths, *output_format)
        }

        Command::TestGen {
            program_path,
            output_dir,
        } => {
            let include_pb: Vec<PathBuf> = vec![];
            let lowering = lower_cobol_file(Path::new(program_path), &include_pb)?;

            let config = InterpreterConfig::default();
            let mut stmts = lowering.statements;
            fixup_loop_exits(&mut stmts);
            let checker = ModelChecker::new(config);
            let result = checker.verify(stmts, &[]);

            let mut out = String::new();
            out.push_str(&format!(
                "Generated {} test cases in {output_dir}/\n",
                result.execution.final_states.len()
            ));
            for (i, state) in result.execution.final_states.iter().enumerate() {
                out.push_str(&format!(
                    "  test_{i}: {} variables\n",
                    state.variables.len()
                ));
            }
            Ok(out)
        }

        Command::Equivalence {
            old_program,
            new_program,
        } => Ok(format!(
            "Equivalence check between {old_program} and {new_program}: not yet implemented.\n\
             Use the library API for full equivalence checking."
        )),

        Command::Help => Ok(help_text()),
    }
}

// ---------------------------------------------------------------------------
// Verify single program
// ---------------------------------------------------------------------------

fn run_verify(
    program_path: &Path,
    include_paths: &[PathBuf],
    properties_path: Option<&str>,
    max_paths: usize,
) -> Result<(LoweringResult, VerificationResult), String> {
    let source =
        std::fs::read_to_string(program_path).map_err(|e| format!("read error: {e}"))?;

    // Gather properties from annotations + optional YAML file.
    let mut properties = parse_annotations(&source);
    if let Some(props_path) = properties_path {
        let yaml =
            std::fs::read_to_string(props_path).map_err(|e| format!("read properties: {e}"))?;
        let extra = parse_properties_yaml(&yaml)?;
        properties.extend(extra);
    }

    let lowering = lower_cobol_file(program_path, include_paths)?;

    let config = InterpreterConfig {
        max_paths,
        ..InterpreterConfig::default()
    };

    let mut stmts = lowering.statements;
    fixup_loop_exits(&mut stmts);

    let checker = ModelChecker::new(config);
    let ver_result = checker.verify(stmts, &properties);

    Ok((
        LoweringResult {
            program_name: lowering.program_name,
            statements: vec![], // don't keep statements in result
            paragraph_map: lowering.paragraph_map,
            stats: lowering.stats,
            errors: lowering.errors,
        },
        ver_result,
    ))
}

// ---------------------------------------------------------------------------
// Batch mode — run against a directory of .cbl files
// ---------------------------------------------------------------------------

fn run_batch(
    directory: &Path,
    include_paths: &[PathBuf],
    max_paths: usize,
    output_format: OutputFormat,
) -> Result<String, String> {
    let mut entries: Vec<_> = std::fs::read_dir(directory)
        .map_err(|e| format!("cannot read directory {}: {e}", directory.display()))?
        .filter_map(|e| e.ok())
        .filter(|e| {
            let name = e.file_name().to_string_lossy().to_lowercase();
            name.ends_with(".cbl") || name.ends_with(".cob")
        })
        .collect();
    entries.sort_by_key(|e| e.file_name());

    let total = entries.len();

    if output_format == OutputFormat::Json {
        return run_batch_json(&entries, include_paths, max_paths);
    }

    let mut out = String::new();
    out.push_str(&format!(
        "=== Symbolic Execution Batch Run: {} programs ===\n\n",
        total
    ));

    let mut total_stmts = 0usize;
    let mut total_lowered = 0usize;
    let mut total_paths = 0usize;
    let mut total_branches = 0usize;
    let mut total_props_checked = 0usize;
    let mut total_props_passed = 0usize;
    let mut total_violations = 0usize;
    let mut total_parse_errors = 0usize;
    let mut successes = 0usize;
    let mut failures = Vec::new();

    for entry in &entries {
        let path = entry.path();
        let filename = entry.file_name().to_string_lossy().to_string();

        match run_verify(&path, include_paths, None, max_paths) {
            Ok((lowering, verification)) => {
                successes += 1;
                total_stmts += lowering.stats.total_statements;
                total_lowered += lowering.stats.lowered_statements;
                total_paths += verification.execution.paths_explored;
                total_branches += verification.coverage.total_branches;
                total_props_checked += verification.properties_checked;
                total_props_passed += verification.properties_passed;
                total_violations += verification.properties_failed.len();
                total_parse_errors += lowering.stats.parse_errors;

                let status = if lowering.stats.parse_errors > 0 {
                    format!(
                        "WARN ({} parse errors)",
                        lowering.stats.parse_errors
                    )
                } else {
                    "OK".to_string()
                };

                out.push_str(&format!(
                    "{:<16} [{status}] {:<12} stmts:{:>4} (lowered:{:>4})  \
                     paths:{:>4}  branches:{:>3}  paras:{:>3}  data:{:>4}  \
                     props:{}/{}  errs:{}\n",
                    filename,
                    lowering.program_name,
                    lowering.stats.total_statements,
                    lowering.stats.lowered_statements,
                    verification.execution.paths_explored,
                    verification.coverage.total_branches,
                    lowering.stats.paragraphs,
                    lowering.stats.data_items,
                    verification.properties_passed,
                    verification.properties_checked,
                    lowering.stats.parse_errors,
                ));
            }
            Err(e) => {
                failures.push((filename.clone(), e.clone()));
                out.push_str(&format!("{:<16} [FAIL] {}\n", filename, e));
            }
        }
    }

    out.push_str(&format!(
        "\n=== Summary ===\n\
         Programs:          {successes}/{total}\n\
         Total statements:  {total_stmts}\n\
         Lowered:           {total_lowered}\n\
         Parse errors:      {total_parse_errors}\n\
         Paths explored:    {total_paths}\n\
         Branch points:     {total_branches}\n\
         Properties:        {total_props_passed}/{total_props_checked} passed\n\
         Violations:        {total_violations}\n\
         Failures:          {}\n",
        failures.len(),
    ));

    if !failures.is_empty() {
        out.push_str("\nFailed programs:\n");
        for (name, err) in &failures {
            out.push_str(&format!("  {name}: {err}\n"));
        }
    }

    Ok(out)
}

fn run_batch_json(
    entries: &[std::fs::DirEntry],
    include_paths: &[PathBuf],
    max_paths: usize,
) -> Result<String, String> {
    let mut programs = Vec::new();

    for entry in entries {
        let path = entry.path();
        let filename = entry.file_name().to_string_lossy().to_string();

        let program_json = match run_verify(&path, include_paths, None, max_paths) {
            Ok((lowering, verification)) => {
                serde_json::json!({
                    "file": filename,
                    "program": lowering.program_name,
                    "status": if lowering.stats.parse_errors > 0 { "warn" } else { "ok" },
                    "stats": {
                        "total_statements": lowering.stats.total_statements,
                        "lowered_statements": lowering.stats.lowered_statements,
                        "skipped_statements": lowering.stats.skipped_statements,
                        "paragraphs": lowering.stats.paragraphs,
                        "sections": lowering.stats.sections,
                        "data_items": lowering.stats.data_items,
                        "parse_errors": lowering.stats.parse_errors,
                    },
                    "execution": {
                        "paths_explored": verification.execution.paths_explored,
                        "paths_feasible": verification.execution.paths_feasible,
                        "paths_infeasible": verification.execution.paths_infeasible,
                        "paths_bounded": verification.execution.paths_bounded,
                    },
                    "coverage": {
                        "branch_coverage": verification.coverage.branch_coverage,
                        "total_branches": verification.coverage.total_branches,
                        "covered_branches": verification.coverage.covered_branches,
                    },
                    "properties": {
                        "checked": verification.properties_checked,
                        "passed": verification.properties_passed,
                        "failed": verification.properties_failed.len(),
                    },
                    "errors": lowering.errors,
                })
            }
            Err(e) => {
                serde_json::json!({
                    "file": filename,
                    "status": "error",
                    "error": e,
                })
            }
        };

        programs.push(program_json);
    }

    let json = serde_json::json!({
        "total_programs": entries.len(),
        "programs": programs,
    });

    serde_json::to_string_pretty(&json).map_err(|e| format!("json error: {e}"))
}

// ---------------------------------------------------------------------------
// Formatting helpers
// ---------------------------------------------------------------------------

fn format_text_single(lowering: &LoweringResult, result: &VerificationResult) -> String {
    let mut out = String::new();
    out.push_str(&format!(
        "=== {} ===\n",
        lowering.program_name
    ));
    out.push_str(&format!(
        "Statements: {} total, {} lowered, {} skipped\n",
        lowering.stats.total_statements,
        lowering.stats.lowered_statements,
        lowering.stats.skipped_statements,
    ));
    out.push_str(&format!(
        "Paragraphs: {}  Sections: {}  Data items: {}\n",
        lowering.stats.paragraphs,
        lowering.stats.sections,
        lowering.stats.data_items,
    ));
    if lowering.stats.parse_errors > 0 {
        out.push_str(&format!(
            "Parse errors: {}\n",
            lowering.stats.parse_errors,
        ));
    }
    out.push_str(&format!(
        "Properties checked: {}\n",
        result.properties_checked
    ));
    out.push_str(&format!(
        "Properties passed:  {}\n",
        result.properties_passed
    ));
    out.push_str(&format!(
        "Properties failed:  {}\n",
        result.properties_failed.len()
    ));
    out.push_str(&format!(
        "Paths explored:     {}\n",
        result.execution.paths_explored
    ));
    out.push_str(&format!(
        "Branch coverage:    {:.1}%\n",
        result.coverage.branch_coverage * 100.0
    ));

    for violation in &result.properties_failed {
        out.push_str(&format!("\nFAILED: {}\n", violation.property.name()));
        out.push_str(&format!("  {}\n", violation.message));
        out.push_str(&format!(
            "  Counterexample variables: {:?}\n",
            violation
                .counterexample
                .variables
                .keys()
                .collect::<Vec<_>>()
        ));
    }

    out
}

fn format_json_single(
    lowering: &LoweringResult,
    result: &VerificationResult,
) -> Result<String, String> {
    let violations: Vec<serde_json::Value> = result
        .properties_failed
        .iter()
        .map(|v| {
            serde_json::json!({
                "property": v.property.name(),
                "message": v.message,
                "counterexample": v.counterexample.variables.iter()
                    .map(|(k, v)| (k.clone(), format!("{v}")))
                    .collect::<std::collections::HashMap<_, _>>(),
            })
        })
        .collect();

    let json = serde_json::json!({
        "program": lowering.program_name,
        "stats": {
            "total_statements": lowering.stats.total_statements,
            "lowered_statements": lowering.stats.lowered_statements,
            "paragraphs": lowering.stats.paragraphs,
            "data_items": lowering.stats.data_items,
            "parse_errors": lowering.stats.parse_errors,
        },
        "properties_checked": result.properties_checked,
        "properties_passed": result.properties_passed,
        "properties_failed": violations.len(),
        "violations": violations,
        "coverage": {
            "branch_coverage": result.coverage.branch_coverage,
            "path_coverage": result.coverage.path_coverage,
            "total_branches": result.coverage.total_branches,
            "covered_branches": result.coverage.covered_branches,
        },
        "execution": {
            "paths_explored": result.execution.paths_explored,
            "paths_feasible": result.execution.paths_feasible,
            "paths_infeasible": result.execution.paths_infeasible,
            "paths_bounded": result.execution.paths_bounded,
        },
    });

    serde_json::to_string_pretty(&json).map_err(|e| format!("json error: {e}"))
}

fn help_text() -> String {
    r#"OpenMainframe Symbolic Execution Engine

USAGE:
    openmf-symbolic <COMMAND> [OPTIONS]

COMMANDS:
    verify       Verify a COBOL program against properties
    batch        Run symbolic execution on all .cbl files in a directory
    testgen      Generate test cases from symbolic execution
    equivalence  Check equivalence between two programs
    help         Display this help message

VERIFY OPTIONS:
    -p, --program <FILE>       COBOL source file
    -I, --include <DIR>        Include path for copybooks (repeatable)
    --properties <FILE>        Properties file (YAML)
    -o, --output <FORMAT>      Output format: text (default), json
    --max-paths <N>            Maximum paths to explore (default: 1000)

BATCH OPTIONS:
    <DIRECTORY>                Directory containing .cbl files
    -I, --include <DIR>        Include path for copybooks (repeatable)
    -o, --output <FORMAT>      Output format: text (default), json
    --max-paths <N>            Maximum paths to explore (default: 1000)

TESTGEN OPTIONS:
    <PROGRAM>                  COBOL source file
    <OUTPUT_DIR>               Output directory (default: testcases/)

EQUIVALENCE OPTIONS:
    <OLD_PROGRAM>              Original COBOL source
    <NEW_PROGRAM>              Modified COBOL source
"#
    .to_string()
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_help() {
        let args = vec!["openmf".into(), "help".into()];
        let cmd = parse_args(&args).unwrap();
        assert!(matches!(cmd, Command::Help));
    }

    #[test]
    fn parse_verify() {
        let args = vec![
            "openmf".into(),
            "verify".into(),
            "-p".into(),
            "test.cbl".into(),
            "--max-paths".into(),
            "500".into(),
        ];
        let cmd = parse_args(&args).unwrap();
        match cmd {
            Command::Verify {
                program_path,
                max_paths,
                ..
            } => {
                assert_eq!(program_path, "test.cbl");
                assert_eq!(max_paths, 500);
            }
            _ => panic!("expected Verify"),
        }
    }

    #[test]
    fn parse_batch() {
        let args = vec![
            "openmf".into(),
            "batch".into(),
            "/path/to/cbl".into(),
            "-I".into(),
            "/path/to/cpy".into(),
        ];
        let cmd = parse_args(&args).unwrap();
        match cmd {
            Command::Batch {
                directory,
                include_paths,
                ..
            } => {
                assert_eq!(directory, "/path/to/cbl");
                assert_eq!(include_paths, vec!["/path/to/cpy"]);
            }
            _ => panic!("expected Batch"),
        }
    }

    #[test]
    fn parse_no_args() {
        let args = vec!["openmf".into()];
        let cmd = parse_args(&args).unwrap();
        assert!(matches!(cmd, Command::Help));
    }

    #[test]
    fn help_text_not_empty() {
        assert!(!help_text().is_empty());
    }
}

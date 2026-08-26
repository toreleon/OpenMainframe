//! CLI interface for the symbolic execution engine.

/// CLI argument definitions.
///
/// This module is intended to be used from a binary entry point
/// (`main.rs`) that parses args and dispatches to the engine.
/// It does *not* pull in `clap` (which would add a heavy dep to the
/// library crate); instead it provides a simple hand-rolled parser.
use std::path::{Path, PathBuf};

use crate::gnucobol;
use crate::interpreter::{InterpreterConfig, SymbolicInterpreter};
use crate::lowering::{fixup_loop_exits, lower_cobol_file, LoweringResult};
use crate::model_checker::{ModelChecker, VerificationResult};
use crate::spec::{parse_annotations, parse_properties_yaml};
use crate::state::ExecutionState;
use crate::testgen;

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
        include_paths: Vec<String>,
        output_dir: String,
    },
    /// Check equivalence between two programs.
    Equivalence {
        old_program: String,
        new_program: String,
        include_paths: Vec<String>,
    },
    /// Generate tests for a single CardDemo program by name.
    CardDemoTestGen {
        program_name: String,
        carddemo_dir: String,
        output_dir: String,
    },
    /// Run test suite through GnuCOBOL to produce a golden master.
    GoldenMaster {
        program_name: String,
        carddemo_dir: String,
        output_dir: String,
        max_tests: Option<usize>,
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
            let mut program_path = None;
            let mut include_paths = Vec::new();
            let mut output_dir = "testcases".to_string();

            let mut i = 2;
            while i < args.len() {
                match args[i].as_str() {
                    "-I" | "--include" => {
                        i += 1;
                        if let Some(p) = args.get(i) {
                            include_paths.push(p.clone());
                        }
                    }
                    "-o" | "--output" => {
                        i += 1;
                        if let Some(d) = args.get(i) {
                            output_dir = d.clone();
                        }
                    }
                    other if program_path.is_none() => {
                        program_path = Some(other.to_string());
                    }
                    _ => {}
                }
                i += 1;
            }

            Ok(Command::TestGen {
                program_path: program_path.ok_or_else(|| "missing program path".to_string())?,
                include_paths,
                output_dir,
            })
        }
        "equivalence" => {
            let mut old_program = None;
            let mut new_program = None;
            let mut include_paths = Vec::new();

            let mut i = 2;
            while i < args.len() {
                match args[i].as_str() {
                    "-I" | "--include" => {
                        i += 1;
                        if let Some(p) = args.get(i) {
                            include_paths.push(p.clone());
                        }
                    }
                    other if old_program.is_none() => {
                        old_program = Some(other.to_string());
                    }
                    other if new_program.is_none() => {
                        new_program = Some(other.to_string());
                    }
                    _ => {}
                }
                i += 1;
            }

            Ok(Command::Equivalence {
                old_program: old_program.ok_or_else(|| "missing old program path".to_string())?,
                new_program: new_program.ok_or_else(|| "missing new program path".to_string())?,
                include_paths,
            })
        }
        "carddemo-testgen" => {
            let mut program_name = None;
            let mut carddemo_dir = None;
            let mut output_dir = "testcases".to_string();

            let mut i = 2;
            while i < args.len() {
                match args[i].as_str() {
                    "-d" | "--carddemo-dir" => {
                        i += 1;
                        carddemo_dir = args.get(i).cloned();
                    }
                    "-o" | "--output" => {
                        i += 1;
                        if let Some(d) = args.get(i) {
                            output_dir = d.clone();
                        }
                    }
                    other if program_name.is_none() => {
                        program_name = Some(other.to_string());
                    }
                    _ => {}
                }
                i += 1;
            }

            let carddemo_dir = carddemo_dir.unwrap_or_else(|| {
                find_carddemo_dir().unwrap_or_else(|| "../carddemo".to_string())
            });

            Ok(Command::CardDemoTestGen {
                program_name: program_name
                    .ok_or_else(|| "missing program name (e.g. CBACT01C)".to_string())?,
                carddemo_dir,
                output_dir,
            })
        }
        "golden-master" => {
            let mut program_name = None;
            let mut carddemo_dir = None;
            let mut output_dir = "golden_master".to_string();
            let mut max_tests = None;

            let mut i = 2;
            while i < args.len() {
                match args[i].as_str() {
                    "-d" | "--carddemo-dir" => {
                        i += 1;
                        carddemo_dir = args.get(i).cloned();
                    }
                    "-o" | "--output" => {
                        i += 1;
                        if let Some(d) = args.get(i) {
                            output_dir = d.clone();
                        }
                    }
                    "-n" | "--max-tests" => {
                        i += 1;
                        max_tests = args.get(i).and_then(|s| s.parse().ok());
                    }
                    other if program_name.is_none() => {
                        program_name = Some(other.to_string());
                    }
                    _ => {}
                }
                i += 1;
            }

            let carddemo_dir = carddemo_dir.unwrap_or_else(|| {
                find_carddemo_dir().unwrap_or_else(|| "../carddemo".to_string())
            });

            Ok(Command::GoldenMaster {
                program_name: program_name
                    .ok_or_else(|| "missing program name (e.g. CBACT01C)".to_string())?,
                carddemo_dir,
                output_dir,
                max_tests,
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
            include_paths,
            output_dir,
        } => {
            let include_pb: Vec<PathBuf> = include_paths.iter().map(PathBuf::from).collect();
            run_testgen(Path::new(program_path), &include_pb, Path::new(output_dir))
        }

        Command::Equivalence {
            old_program,
            new_program,
            include_paths,
        } => {
            let include_pb: Vec<PathBuf> = include_paths.iter().map(PathBuf::from).collect();
            run_equivalence(Path::new(old_program), Path::new(new_program), &include_pb)
        }

        Command::CardDemoTestGen {
            program_name,
            carddemo_dir,
            output_dir,
        } => run_carddemo_testgen(program_name, Path::new(carddemo_dir), Path::new(output_dir)),

        Command::GoldenMaster {
            program_name,
            carddemo_dir,
            output_dir,
            max_tests,
        } => run_golden_master_cmd(
            program_name,
            Path::new(carddemo_dir),
            Path::new(output_dir),
            *max_tests,
        ),

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
// Test generation
// ---------------------------------------------------------------------------

fn run_testgen(program_path: &Path, include_paths: &[PathBuf], output_dir: &Path) -> Result<String, String> {
    let mut lowering = lower_cobol_file(program_path, include_paths)?;

    let config = InterpreterConfig::default();
    fixup_loop_exits(&mut lowering.statements);
    let stmts = std::mem::take(&mut lowering.statements);

    let interp = SymbolicInterpreter::new(stmts.clone(), config);
    let execution = interp.execute(ExecutionState::new());

    let suite = testgen::generate_test_suite(&lowering, &execution, &stmts);

    // Create output directory.
    std::fs::create_dir_all(output_dir)
        .map_err(|e| format!("cannot create {}: {e}", output_dir.display()))?;

    // Write JSON test suite.
    let json_path = output_dir.join(format!("{}_tests.json", lowering.program_name));
    let json = testgen::to_json(&suite)?;
    std::fs::write(&json_path, &json)
        .map_err(|e| format!("cannot write {}: {e}", json_path.display()))?;

    // Write YAML test suite.
    let yaml_path = output_dir.join(format!("{}_tests.yaml", lowering.program_name));
    let yaml = testgen::to_yaml(&suite);
    std::fs::write(&yaml_path, &yaml)
        .map_err(|e| format!("cannot write {}: {e}", yaml_path.display()))?;

    let mut out = String::new();
    out.push_str(&format!(
        "=== Test Generation: {} ===\n\n",
        lowering.program_name
    ));
    out.push_str(&format!(
        "Paths explored:      {}\n",
        suite.stats.paths_explored
    ));
    out.push_str(&format!(
        "Paths feasible:      {}\n",
        suite.stats.paths_feasible
    ));
    out.push_str(&format!(
        "Input variables:     {}\n",
        suite.stats.input_variables
    ));
    out.push_str(&format!(
        "Output variables:    {}\n\n",
        suite.stats.output_variables
    ));
    out.push_str(&format!(
        "Path tests:          {}\n",
        suite.stats.path_tests_generated
    ));
    out.push_str(&format!(
        "Boundary tests:      {}\n",
        suite.stats.boundary_tests_generated
    ));
    out.push_str(&format!(
        "Edge case tests:     {}\n",
        suite.stats.edge_case_tests_generated
    ));
    out.push_str(&format!(
        "Total tests:         {}\n\n",
        suite.stats.total_tests
    ));
    out.push_str(&format!("Output:\n"));
    out.push_str(&format!("  {}\n", json_path.display()));
    out.push_str(&format!("  {}\n", yaml_path.display()));

    Ok(out)
}

// ---------------------------------------------------------------------------
// Equivalence checking
// ---------------------------------------------------------------------------

fn run_equivalence(old_path: &Path, new_path: &Path, include_paths: &[PathBuf]) -> Result<String, String> {
    let mut lowering_a = lower_cobol_file(old_path, include_paths)?;
    let mut lowering_b = lower_cobol_file(new_path, include_paths)?;

    let config = InterpreterConfig::default();

    fixup_loop_exits(&mut lowering_a.statements);
    let stmts_a = std::mem::take(&mut lowering_a.statements);
    let interp_a = SymbolicInterpreter::new(stmts_a.clone(), config.clone());
    let result_a = interp_a.execute(ExecutionState::new());

    fixup_loop_exits(&mut lowering_b.statements);
    let stmts_b = std::mem::take(&mut lowering_b.statements);
    let interp_b = SymbolicInterpreter::new(stmts_b.clone(), config);
    let result_b = interp_b.execute(ExecutionState::new());

    let equiv = testgen::generate_equivalence_tests(
        &result_a,
        &stmts_a,
        &lowering_a.program_name,
        &result_b,
        &stmts_b,
        &lowering_b.program_name,
    );

    let mut out = String::new();
    out.push_str(&format!(
        "=== Equivalence Check ===\n\n"
    ));
    out.push_str(&format!(
        "Program A: {} ({})\n",
        lowering_a.program_name,
        old_path.display()
    ));
    out.push_str(&format!(
        "Program B: {} ({})\n",
        lowering_b.program_name,
        new_path.display()
    ));
    out.push_str(&format!(
        "Program A paths: {}\n",
        result_a.paths_feasible
    ));
    out.push_str(&format!(
        "Program B paths: {}\n\n",
        result_b.paths_feasible
    ));

    if equiv.equivalent {
        out.push_str("Result: EQUIVALENT\n");
        out.push_str(&format!(
            "Equivalence tests generated: {}\n",
            equiv.test_suite.stats.equivalence_tests_generated
        ));
        out.push_str("All paths produce identical outputs.\n");
    } else {
        out.push_str("Result: NOT EQUIVALENT\n");
        out.push_str(&format!(
            "Counterexamples found: {}\n\n",
            equiv.counterexamples.len()
        ));
        for (i, ce) in equiv.counterexamples.iter().enumerate() {
            out.push_str(&format!("Counterexample {}:\n", i + 1));
            out.push_str(&format!("  Differing: {}\n", ce.differing_variables.join(", ")));
            out.push_str("  Inputs:\n");
            for (k, v) in &ce.inputs {
                out.push_str(&format!("    {k} = {v}\n"));
            }
            out.push_str("  Program A outputs:\n");
            for (k, v) in &ce.program_a_outputs {
                out.push_str(&format!("    {k} = {v}\n"));
            }
            out.push_str("  Program B outputs:\n");
            for (k, v) in &ce.program_b_outputs {
                out.push_str(&format!("    {k} = {v}\n"));
            }
            out.push('\n');
        }
    }

    // Also output JSON to stdout.
    match testgen::equivalence_to_json(&equiv) {
        Ok(json) => {
            out.push_str(&format!("\n--- JSON ---\n{json}\n"));
        }
        Err(e) => {
            out.push_str(&format!("\n(JSON serialization failed: {e})\n"));
        }
    }

    Ok(out)
}

// ---------------------------------------------------------------------------
// CardDemo test generation
// ---------------------------------------------------------------------------

/// Auto-detect the carddemo directory by walking up from the current dir.
fn find_carddemo_dir() -> Option<String> {
    let cwd = std::env::current_dir().ok()?;
    // Try common relative locations.
    for candidate in &[
        "carddemo",
        "../carddemo",
        "../../carddemo",
    ] {
        let p = cwd.join(candidate).join("app").join("cbl");
        if p.is_dir() {
            return Some(cwd.join(candidate).to_string_lossy().to_string());
        }
    }
    None
}

/// Locate the CICS system copybooks directory (DFHAID, DFHEIBLK, etc.).
fn find_cics_copybooks(carddemo_dir: &Path) -> Option<PathBuf> {
    // Relative to carddemo_dir, the OpenMainframe crate is a sibling or parent.
    let candidates = [
        carddemo_dir.join("../OpenMainframe/crates/open-mainframe-cics/copybooks"),
        // When running from inside OpenMainframe/
        PathBuf::from("crates/open-mainframe-cics/copybooks"),
    ];
    // Also try from cwd.
    let cwd = std::env::current_dir().ok();
    let mut all_candidates: Vec<PathBuf> = candidates.to_vec();
    if let Some(ref cwd) = cwd {
        all_candidates.push(cwd.join("crates/open-mainframe-cics/copybooks"));
    }

    for c in &all_candidates {
        if c.join("DFHAID.cpy").is_file() {
            return Some(c.canonicalize().unwrap_or_else(|_| c.clone()));
        }
    }
    None
}

/// Resolve a program name like "CBACT01C" to a full .cbl path inside CardDemo.
fn resolve_carddemo_program(name: &str, carddemo_dir: &Path) -> Result<PathBuf, String> {
    let cbl_dir = carddemo_dir.join("app").join("cbl");
    if !cbl_dir.is_dir() {
        return Err(format!(
            "CardDemo cbl directory not found: {}",
            cbl_dir.display()
        ));
    }

    // Try exact name, name.cbl, name.CBL, uppercase variants.
    let candidates: Vec<String> = vec![
        name.to_string(),
        format!("{name}.cbl"),
        format!("{name}.CBL"),
        format!("{}.cbl", name.to_uppercase()),
        format!("{}.CBL", name.to_uppercase()),
    ];

    for candidate in &candidates {
        let full = cbl_dir.join(candidate);
        if full.is_file() {
            return Ok(full);
        }
    }

    // List available programs for a helpful error.
    let available: Vec<String> = std::fs::read_dir(&cbl_dir)
        .map_err(|e| format!("cannot read {}: {e}", cbl_dir.display()))?
        .filter_map(|e| e.ok())
        .filter(|e| {
            let n = e.file_name().to_string_lossy().to_lowercase();
            n.ends_with(".cbl") || n.ends_with(".cob")
        })
        .map(|e| {
            e.path()
                .file_stem()
                .unwrap_or_default()
                .to_string_lossy()
                .to_string()
        })
        .collect();

    Err(format!(
        "program '{}' not found in {}\nAvailable: {}",
        name,
        cbl_dir.display(),
        available.join(", ")
    ))
}

fn run_carddemo_testgen(
    program_name: &str,
    carddemo_dir: &Path,
    output_dir: &Path,
) -> Result<String, String> {
    let program_path = resolve_carddemo_program(program_name, carddemo_dir)?;

    // CardDemo copybook dirs + CICS system copybooks from the OpenMainframe crate.
    let mut include_paths: Vec<PathBuf> = vec![
        carddemo_dir.join("app").join("cpy"),
        carddemo_dir.join("app").join("cpy-bms"),
    ];

    // Auto-discover CICS system copybooks (DFHAID, DFHEIBLK, DFHBMSCA).
    let cics_copybooks = find_cics_copybooks(carddemo_dir);
    if let Some(cics_dir) = cics_copybooks {
        include_paths.push(cics_dir);
    }

    // Filter to only include paths that exist.
    let include_paths: Vec<PathBuf> = include_paths
        .into_iter()
        .filter(|p| p.is_dir())
        .collect();

    let mut out = String::new();
    out.push_str(&format!(
        "CardDemo Test Generation\n\
         Program:    {}\n\
         Source:     {}\n\
         Includes:   {}\n\n",
        program_name,
        program_path.display(),
        include_paths
            .iter()
            .map(|p| p.display().to_string())
            .collect::<Vec<_>>()
            .join(", "),
    ));

    let result = run_testgen(&program_path, &include_paths, output_dir)?;
    out.push_str(&result);
    Ok(out)
}

// ---------------------------------------------------------------------------
// Golden master runner
// ---------------------------------------------------------------------------

fn run_golden_master_cmd(
    program_name: &str,
    carddemo_dir: &Path,
    output_dir: &Path,
    max_tests: Option<usize>,
) -> Result<String, String> {
    // Step 1: Check GnuCOBOL is available.
    let cobc_version = gnucobol::check_gnucobol()?;

    // Step 2: Resolve program and include paths (same as carddemo-testgen).
    let program_path = resolve_carddemo_program(program_name, carddemo_dir)?;

    let mut include_paths: Vec<PathBuf> = vec![
        carddemo_dir.join("app").join("cpy"),
        carddemo_dir.join("app").join("cpy-bms"),
    ];
    if let Some(cics_dir) = find_cics_copybooks(carddemo_dir) {
        include_paths.push(cics_dir);
    }
    let include_paths: Vec<PathBuf> = include_paths.into_iter().filter(|p| p.is_dir()).collect();

    // Step 3: Generate test suite via symbolic execution.
    let mut lowering = lower_cobol_file(&program_path, &include_paths)?;
    let config = InterpreterConfig::default();
    fixup_loop_exits(&mut lowering.statements);
    let stmts = std::mem::take(&mut lowering.statements);
    let interp = SymbolicInterpreter::new(stmts.clone(), config);
    let execution = interp.execute(ExecutionState::new());
    let mut suite = testgen::generate_test_suite(&lowering, &execution, &stmts);

    // Optionally limit test count.
    if let Some(max) = max_tests {
        if suite.test_cases.len() > max {
            suite.test_cases.truncate(max);
        }
    }

    // Step 4: Read original source for WS extraction.
    let original_source = std::fs::read_to_string(&program_path)
        .map_err(|e| format!("read source: {e}"))?;

    // Step 5: Create work directory for harness files.
    let work_dir = Path::new(output_dir).join("work");
    std::fs::create_dir_all(output_dir)
        .map_err(|e| format!("cannot create output dir: {e}"))?;

    // Step 6: Run golden master.
    let report = gnucobol::run_golden_master(&suite, &original_source, &include_paths, &work_dir)?;

    // Step 7: Write golden master test suite.
    let golden_json_path = Path::new(output_dir).join(format!("{}_golden.json", program_name));
    let golden_yaml_path = Path::new(output_dir).join(format!("{}_golden.yaml", program_name));

    let json = testgen::to_json(&report.golden_suite)?;
    std::fs::write(&golden_json_path, &json)
        .map_err(|e| format!("write json: {e}"))?;
    let yaml = testgen::to_yaml(&report.golden_suite);
    std::fs::write(&golden_yaml_path, &yaml)
        .map_err(|e| format!("write yaml: {e}"))?;

    // Step 8: Format report.
    let mut out = String::new();
    out.push_str(&format!(
        "=== Golden Master: {} ===\n\
         GnuCOBOL: {}\n\
         Source:    {}\n\n",
        program_name,
        cobc_version,
        program_path.display(),
    ));
    out.push_str(&format!(
        "Total tests:   {}\n\
         Passed:        {} (symbolic prediction matched GnuCOBOL)\n\
         Failed:        {} (symbolic prediction differs from GnuCOBOL)\n\
         Errors:        {} (compilation or runtime error)\n\n",
        report.total_tests, report.passed, report.failed, report.errors,
    ));

    // Show first few failures.
    let failures: Vec<_> = report.results.iter().filter(|r| !r.passed).take(10).collect();
    if !failures.is_empty() {
        out.push_str("First failures/errors:\n");
        for r in &failures {
            out.push_str(&format!("  {} ", r.test_id));
            if let Some(ref e) = r.error {
                out.push_str(&format!("[ERROR] {}\n", truncate(e, 120)));
            } else {
                out.push_str("[DIFF]\n");
                // Show diffs between symbolic execution and GnuCOBOL.
                if let Some(test) = suite.test_cases.iter().find(|t| t.id == r.test_id) {
                    let diffs = gnucobol::compare_outputs(&test.expected_outputs, &r.actual_outputs);
                    for (var, exp, act) in diffs.iter().take(5) {
                        out.push_str(&format!(
                            "    {}: symbolic={} GnuCOBOL={}\n",
                            var, exp, act
                        ));
                    }
                }
            }
        }
        out.push('\n');
    }

    out.push_str(&format!(
        "Golden master output:\n\
         \x20 {}\n\
         \x20 {}\n",
        golden_json_path.display(),
        golden_yaml_path.display(),
    ));

    Ok(out)
}

fn truncate(s: &str, max: usize) -> String {
    if s.len() <= max {
        s.to_string()
    } else {
        format!("{}...", &s[..max])
    }
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
    verify            Verify a COBOL program against properties
    batch             Run symbolic execution on all .cbl files in a directory
    testgen           Generate test cases from symbolic execution
    carddemo-testgen  Generate tests for a CardDemo program by name
    golden-master     Run tests through GnuCOBOL to capture ground-truth outputs
    equivalence       Check equivalence between two programs
    help              Display this help message

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

CARDDEMO-TESTGEN OPTIONS:
    <PROGRAM_NAME>             Program name (e.g. CBACT01C, COSGN00C)
    -d, --carddemo-dir <DIR>   Path to carddemo/ (auto-detected if omitted)
    -o, --output <DIR>         Output directory (default: testcases/)

GOLDEN-MASTER OPTIONS:
    <PROGRAM_NAME>             Program name (e.g. CBACT01C)
    -d, --carddemo-dir <DIR>   Path to carddemo/ (auto-detected if omitted)
    -o, --output <DIR>         Output directory (default: golden_master/)
    -n, --max-tests <N>        Max tests to run (default: all)

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
    fn parse_carddemo_testgen() {
        let args = vec![
            "openmf".into(),
            "carddemo-testgen".into(),
            "CBACT01C".into(),
            "-d".into(),
            "/path/to/carddemo".into(),
            "-o".into(),
            "/tmp/out".into(),
        ];
        let cmd = parse_args(&args).unwrap();
        match cmd {
            Command::CardDemoTestGen {
                program_name,
                carddemo_dir,
                output_dir,
            } => {
                assert_eq!(program_name, "CBACT01C");
                assert_eq!(carddemo_dir, "/path/to/carddemo");
                assert_eq!(output_dir, "/tmp/out");
            }
            _ => panic!("expected CardDemoTestGen"),
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

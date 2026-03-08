//! Test case generation from symbolic execution results.
//!
//! This module generates concrete test cases for migration equivalence
//! verification. It takes the `final_states` from symbolic execution,
//! solves path conditions with Z3 to get concrete inputs, classifies
//! variables as inputs vs outputs, extracts decision boundaries, and
//! emits structured test case files (JSON/YAML).
//!
//! # Test Case Levels (from SYSTEM_LEVEL_EQUIVALENCE.md)
//!
//! - **L1 Path Tests**: One test per feasible execution path
//! - **L1 Boundary Tests**: Tests at decision thresholds (e.g., `BALANCE > 10000` → test 9999, 10000, 10001)
//! - **Equivalence Tests**: Compare outputs of two program versions on the same inputs

use std::collections::{BTreeMap, HashSet};

use serde::{Deserialize, Serialize};

use crate::interpreter::{ExecutionResult, FlatStatement};
use crate::lowering::LoweringResult;
use crate::path::PathCondition;
use crate::solver::{CheckResult, SymbolicSolver};
use crate::state::ExecutionState;
use crate::value::{ExprOp, SymbolicValue};

// ---------------------------------------------------------------------------
// Test case data model
// ---------------------------------------------------------------------------

/// A single generated test case.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestCase {
    /// Unique identifier (e.g., "PATH-0001", "BOUNDARY-0003").
    pub id: String,
    /// Human-readable description of what this test covers.
    pub description: String,
    /// Category of the test case.
    pub category: TestCategory,
    /// Concrete input values (variable name → value).
    pub inputs: BTreeMap<String, TestValue>,
    /// Expected output values (variable name → value).
    pub expected_outputs: BTreeMap<String, TestValue>,
    /// Path conditions that lead to this test case (human-readable).
    pub path_conditions: Vec<String>,
    /// Source program this test was generated from.
    pub source_program: String,
}

/// Category of a generated test case.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum TestCategory {
    /// Tests a specific execution path through the program.
    PathCoverage,
    /// Tests a decision boundary (boundary value analysis).
    BoundaryValue,
    /// Tests equivalence between two program versions.
    Equivalence,
    /// Tests extracted from negated path conditions (edge cases).
    EdgeCase,
}

/// A concrete test value.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(untagged)]
pub enum TestValue {
    Int(i64),
    Bool(bool),
    Str(String),
    Unknown,
}

impl std::fmt::Display for TestValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TestValue::Int(n) => write!(f, "{n}"),
            TestValue::Bool(b) => write!(f, "{b}"),
            TestValue::Str(s) => write!(f, "\"{s}\""),
            TestValue::Unknown => write!(f, "?"),
        }
    }
}

/// A complete test suite generated from one or more programs.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestSuite {
    /// Name of the test suite.
    pub name: String,
    /// Source program(s).
    pub source_programs: Vec<String>,
    /// Generated test cases.
    pub test_cases: Vec<TestCase>,
    /// Generation statistics.
    pub stats: GenerationStats,
}

/// Statistics about the test generation process.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct GenerationStats {
    pub paths_explored: usize,
    pub paths_feasible: usize,
    pub path_tests_generated: usize,
    pub boundary_tests_generated: usize,
    pub edge_case_tests_generated: usize,
    pub equivalence_tests_generated: usize,
    pub total_tests: usize,
    pub input_variables: usize,
    pub output_variables: usize,
}

/// Result of an equivalence comparison between two programs.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EquivalenceResult {
    pub program_a: String,
    pub program_b: String,
    pub equivalent: bool,
    pub test_suite: TestSuite,
    pub counterexamples: Vec<Counterexample>,
}

/// A concrete counterexample showing where two programs differ.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Counterexample {
    pub inputs: BTreeMap<String, TestValue>,
    pub program_a_outputs: BTreeMap<String, TestValue>,
    pub program_b_outputs: BTreeMap<String, TestValue>,
    pub differing_variables: Vec<String>,
}

// ---------------------------------------------------------------------------
// Variable classification
// ---------------------------------------------------------------------------

/// Classifies variables in an execution state as inputs or outputs.
///
/// **Inputs**: Variables that remain symbolic (were never concretely assigned)
/// or were assigned from symbolic sources (ACCEPT, CALL RETURN).
///
/// **Outputs**: Variables that were assigned concrete or computed values
/// during execution.
pub fn classify_variables(
    state: &ExecutionState,
    statements: &[FlatStatement],
) -> (HashSet<String>, HashSet<String>) {
    // Collect all variables that appear as assignment targets.
    let mut assigned: HashSet<String> = HashSet::new();
    for stmt in statements {
        if let FlatStatement::Assign { target, .. } = stmt {
            assigned.insert(target.clone());
        }
    }

    let mut inputs = HashSet::new();
    let mut outputs = HashSet::new();

    for (name, value) in &state.variables {
        // Skip internal loop counters.
        if name.starts_with("_LOOP_CTR_") {
            continue;
        }

        if is_input_variable(name, value) {
            inputs.insert(name.clone());
        } else {
            outputs.insert(name.clone());
        }
    }

    (inputs, outputs)
}

/// A variable is an "input" if its value is still symbolic (came from outside
/// the program) or if its name indicates it was injected (INPUT_*, CALL_RETURN_*).
fn is_input_variable(name: &str, value: &SymbolicValue) -> bool {
    // Explicitly injected symbolic inputs from ACCEPT/CALL.
    if name.starts_with("INPUT_") || name.starts_with("CALL_RETURN_") {
        return true;
    }
    // If the value is still symbolic (not concretized), it's an input.
    matches!(value, SymbolicValue::Symbolic { .. })
}

// ---------------------------------------------------------------------------
// Test case generation from symbolic execution
// ---------------------------------------------------------------------------

/// Generate test cases from a completed symbolic execution result.
pub fn generate_path_tests(
    execution: &ExecutionResult,
    statements: &[FlatStatement],
    program_name: &str,
) -> Vec<TestCase> {
    let solver = SymbolicSolver::new();
    let mut tests = Vec::new();

    for (i, state) in execution.final_states.iter().enumerate() {
        let (input_vars, output_vars) = classify_variables(state, statements);

        // Solve the path condition to get concrete input values.
        let concrete_inputs = solve_for_inputs(&solver, &state.path_condition, &input_vars);

        // Collect output values.
        let outputs = collect_outputs(state, &output_vars);

        // Format path conditions as human-readable strings.
        let path_desc: Vec<String> = state
            .path_condition
            .constraints()
            .iter()
            .filter_map(|c| {
                let desc = format_condition(&c.condition);
                if desc == "true" || desc == "false" {
                    None
                } else {
                    Some(desc)
                }
            })
            .collect();

        let description = if path_desc.is_empty() {
            "Unconditional path (no branches)".to_string()
        } else {
            format!("Path where: {}", path_desc.join(" AND "))
        };

        tests.push(TestCase {
            id: format!("PATH-{:04}", i + 1),
            description,
            category: TestCategory::PathCoverage,
            inputs: concrete_inputs,
            expected_outputs: outputs,
            path_conditions: path_desc,
            source_program: program_name.to_string(),
        });
    }

    tests
}

/// Generate boundary value tests from decision points in the program.
///
/// For each comparison like `X > 10000`, generates tests at:
/// - Below boundary: X = 9999
/// - At boundary: X = 10000
/// - Above boundary: X = 10001
pub fn generate_boundary_tests(
    statements: &[FlatStatement],
    program_name: &str,
) -> Vec<TestCase> {
    let mut tests = Vec::new();
    let mut boundaries = Vec::new();

    // Extract comparison thresholds from branch conditions.
    for stmt in statements {
        if let FlatStatement::Branch { condition, .. }
        | FlatStatement::LoopHeader { condition, .. } = stmt
        {
            extract_boundaries(condition, &mut boundaries);
        }
    }

    // Deduplicate boundaries.
    boundaries.sort_by_key(|b| (b.variable.clone(), b.threshold));
    boundaries.dedup_by_key(|b| (b.variable.clone(), b.threshold));

    for (i, boundary) in boundaries.iter().enumerate() {
        let base_id = i * 3;

        // Below boundary.
        tests.push(TestCase {
            id: format!("BOUNDARY-{:04}", base_id + 1),
            description: format!(
                "{} = {} (below boundary {} {})",
                boundary.variable,
                boundary.threshold - 1,
                boundary.op_str(),
                boundary.threshold
            ),
            category: TestCategory::BoundaryValue,
            inputs: BTreeMap::from([(
                boundary.variable.clone(),
                TestValue::Int(boundary.threshold - 1),
            )]),
            expected_outputs: BTreeMap::new(),
            path_conditions: vec![format!(
                "{} = {} (below {} {})",
                boundary.variable,
                boundary.threshold - 1,
                boundary.op_str(),
                boundary.threshold
            )],
            source_program: program_name.to_string(),
        });

        // At boundary.
        tests.push(TestCase {
            id: format!("BOUNDARY-{:04}", base_id + 2),
            description: format!(
                "{} = {} (at boundary {} {})",
                boundary.variable,
                boundary.threshold,
                boundary.op_str(),
                boundary.threshold
            ),
            category: TestCategory::BoundaryValue,
            inputs: BTreeMap::from([(
                boundary.variable.clone(),
                TestValue::Int(boundary.threshold),
            )]),
            expected_outputs: BTreeMap::new(),
            path_conditions: vec![format!(
                "{} = {} (at {} {})",
                boundary.variable,
                boundary.threshold,
                boundary.op_str(),
                boundary.threshold
            )],
            source_program: program_name.to_string(),
        });

        // Above boundary.
        tests.push(TestCase {
            id: format!("BOUNDARY-{:04}", base_id + 3),
            description: format!(
                "{} = {} (above boundary {} {})",
                boundary.variable,
                boundary.threshold + 1,
                boundary.op_str(),
                boundary.threshold
            ),
            category: TestCategory::BoundaryValue,
            inputs: BTreeMap::from([(
                boundary.variable.clone(),
                TestValue::Int(boundary.threshold + 1),
            )]),
            expected_outputs: BTreeMap::new(),
            path_conditions: vec![format!(
                "{} = {} (above {} {})",
                boundary.variable,
                boundary.threshold + 1,
                boundary.op_str(),
                boundary.threshold
            )],
            source_program: program_name.to_string(),
        });
    }

    tests
}

/// Generate edge case tests by exploring negated/unusual path conditions.
pub fn generate_edge_case_tests(
    execution: &ExecutionResult,
    statements: &[FlatStatement],
    program_name: &str,
) -> Vec<TestCase> {
    let solver = SymbolicSolver::new();
    let mut tests = Vec::new();
    let (input_vars, _) = if let Some(state) = execution.final_states.first() {
        classify_variables(state, statements)
    } else {
        return tests;
    };

    // For each path, try to find inputs at extreme values.
    for (i, state) in execution.final_states.iter().enumerate() {
        // Try to maximize each input variable.
        for var in &input_vars {
            let mut pc = state.path_condition.clone();
            // Add constraint: var > 999999 (large value test).
            let large_constraint = SymbolicValue::sym_int(var).gt(SymbolicValue::int(999_999));
            if solver.is_feasible(&pc, &large_constraint) {
                pc.add(crate::path::Constraint {
                    condition: large_constraint,
                    source: open_mainframe_lang_core::Span::default(),
                    description: Some(format!("edge: {var} > 999999")),
                });
                let inputs = solve_for_inputs(&solver, &pc, &input_vars);
                if !inputs.is_empty() {
                    tests.push(TestCase {
                        id: format!("EDGE-{:04}", tests.len() + 1),
                        description: format!("Large value for {var} on path {}", i + 1),
                        category: TestCategory::EdgeCase,
                        inputs,
                        expected_outputs: BTreeMap::new(),
                        path_conditions: vec![format!("{var} > 999999")],
                        source_program: program_name.to_string(),
                    });
                }
            }

            // Try negative value.
            let neg_constraint = SymbolicValue::sym_int(var).lt(SymbolicValue::int(0));
            if solver.is_feasible(&state.path_condition, &neg_constraint) {
                let mut pc2 = state.path_condition.clone();
                pc2.add(crate::path::Constraint {
                    condition: neg_constraint,
                    source: open_mainframe_lang_core::Span::default(),
                    description: Some(format!("edge: {var} < 0")),
                });
                let inputs = solve_for_inputs(&solver, &pc2, &input_vars);
                if !inputs.is_empty() {
                    tests.push(TestCase {
                        id: format!("EDGE-{:04}", tests.len() + 1),
                        description: format!("Negative value for {var} on path {}", i + 1),
                        category: TestCategory::EdgeCase,
                        inputs,
                        expected_outputs: BTreeMap::new(),
                        path_conditions: vec![format!("{var} < 0")],
                        source_program: program_name.to_string(),
                    });
                }
            }

            // Try zero.
            let zero_constraint = SymbolicValue::sym_int(var).eq(SymbolicValue::int(0));
            if solver.is_feasible(&state.path_condition, &zero_constraint) {
                let mut pc3 = state.path_condition.clone();
                pc3.add(crate::path::Constraint {
                    condition: zero_constraint,
                    source: open_mainframe_lang_core::Span::default(),
                    description: Some(format!("edge: {var} = 0")),
                });
                let inputs = solve_for_inputs(&solver, &pc3, &input_vars);
                if !inputs.is_empty() {
                    tests.push(TestCase {
                        id: format!("EDGE-{:04}", tests.len() + 1),
                        description: format!("Zero value for {var} on path {}", i + 1),
                        category: TestCategory::EdgeCase,
                        inputs,
                        expected_outputs: BTreeMap::new(),
                        path_conditions: vec![format!("{var} = 0")],
                        source_program: program_name.to_string(),
                    });
                }
            }
        }
    }

    tests
}

// ---------------------------------------------------------------------------
// Equivalence test generation
// ---------------------------------------------------------------------------

/// Generate equivalence tests by comparing two programs' symbolic execution.
///
/// Runs symbolic execution on both programs, then for each path in program A,
/// finds the corresponding path in program B (by solving A's path condition
/// as inputs to B) and checks that outputs match.
pub fn generate_equivalence_tests(
    result_a: &ExecutionResult,
    stmts_a: &[FlatStatement],
    name_a: &str,
    result_b: &ExecutionResult,
    stmts_b: &[FlatStatement],
    name_b: &str,
) -> EquivalenceResult {
    let solver = SymbolicSolver::new();
    let mut test_cases = Vec::new();
    let mut counterexamples = Vec::new();
    let mut all_equivalent = true;

    // For each path in program A, get concrete inputs from Z3.
    for (i, state_a) in result_a.final_states.iter().enumerate() {
        let (inputs_a, outputs_a) = classify_variables(state_a, stmts_a);
        let concrete_inputs = solve_for_inputs(&solver, &state_a.path_condition, &inputs_a);
        let outputs_a_vals = collect_outputs(state_a, &outputs_a);

        // Find the matching path in program B: the path whose condition is
        // satisfiable given the same concrete inputs.
        let mut matched = false;
        for state_b in &result_b.final_states {
            let (_, outputs_b) = classify_variables(state_b, stmts_b);

            // Check if B's path condition is compatible with A's concrete inputs.
            if !is_path_compatible(&solver, &state_b.path_condition, &concrete_inputs) {
                continue;
            }

            let outputs_b_vals = collect_outputs(state_b, &outputs_b);

            // Compare outputs.
            let common_vars: HashSet<&String> = outputs_a_vals
                .keys()
                .filter(|k| outputs_b_vals.contains_key(*k))
                .collect();

            let mut differing = Vec::new();
            for var in &common_vars {
                let val_a = &outputs_a_vals[*var];
                let val_b = &outputs_b_vals[*var];
                if val_a != val_b {
                    differing.push((*var).clone());
                }
            }

            let path_desc: Vec<String> = state_a
                .path_condition
                .constraints()
                .iter()
                .filter_map(|c| {
                    let d = format_condition(&c.condition);
                    if d == "true" { None } else { Some(d) }
                })
                .collect();

            if differing.is_empty() {
                test_cases.push(TestCase {
                    id: format!("EQUIV-{:04}", test_cases.len() + 1),
                    description: format!(
                        "Path {} equivalent: {} == {}",
                        i + 1, name_a, name_b
                    ),
                    category: TestCategory::Equivalence,
                    inputs: concrete_inputs.clone(),
                    expected_outputs: outputs_a_vals.clone(),
                    path_conditions: path_desc,
                    source_program: format!("{name_a} vs {name_b}"),
                });
            } else {
                all_equivalent = false;
                counterexamples.push(Counterexample {
                    inputs: concrete_inputs.clone(),
                    program_a_outputs: outputs_a_vals.clone(),
                    program_b_outputs: outputs_b_vals,
                    differing_variables: differing.clone(),
                });
                test_cases.push(TestCase {
                    id: format!("EQUIV-{:04}", test_cases.len() + 1),
                    description: format!(
                        "Path {} DIFFERS: {} vs {} on [{}]",
                        i + 1,
                        name_a,
                        name_b,
                        differing.join(", ")
                    ),
                    category: TestCategory::Equivalence,
                    inputs: concrete_inputs.clone(),
                    expected_outputs: outputs_a_vals.clone(),
                    path_conditions: path_desc,
                    source_program: format!("{name_a} vs {name_b}"),
                });
            }

            matched = true;
            break;
        }

        if !matched && !concrete_inputs.is_empty() {
            all_equivalent = false;
            counterexamples.push(Counterexample {
                inputs: concrete_inputs.clone(),
                program_a_outputs: outputs_a_vals.clone(),
                program_b_outputs: BTreeMap::new(),
                differing_variables: vec!["(no matching path in program B)".to_string()],
            });
        }
    }

    let stats = GenerationStats {
        equivalence_tests_generated: test_cases.len(),
        total_tests: test_cases.len(),
        ..Default::default()
    };

    EquivalenceResult {
        program_a: name_a.to_string(),
        program_b: name_b.to_string(),
        equivalent: all_equivalent,
        test_suite: TestSuite {
            name: format!("Equivalence: {name_a} vs {name_b}"),
            source_programs: vec![name_a.to_string(), name_b.to_string()],
            test_cases,
            stats,
        },
        counterexamples,
    }
}

// ---------------------------------------------------------------------------
// Full test suite generation
// ---------------------------------------------------------------------------

/// Generate a complete test suite from a lowered COBOL program.
///
/// Produces path coverage tests, boundary value tests, and edge case tests.
pub fn generate_test_suite(
    lowering: &LoweringResult,
    execution: &ExecutionResult,
    statements: &[FlatStatement],
) -> TestSuite {
    let name = &lowering.program_name;

    let path_tests = generate_path_tests(execution, statements, name);
    let boundary_tests = generate_boundary_tests(statements, name);
    let edge_tests = generate_edge_case_tests(execution, statements, name);

    // Classify variables from first state for stats.
    let (input_count, output_count) = execution
        .final_states
        .first()
        .map(|s| {
            let (i, o) = classify_variables(s, statements);
            (i.len(), o.len())
        })
        .unwrap_or((0, 0));

    let stats = GenerationStats {
        paths_explored: execution.paths_explored,
        paths_feasible: execution.paths_feasible,
        path_tests_generated: path_tests.len(),
        boundary_tests_generated: boundary_tests.len(),
        edge_case_tests_generated: edge_tests.len(),
        equivalence_tests_generated: 0,
        total_tests: path_tests.len() + boundary_tests.len() + edge_tests.len(),
        input_variables: input_count,
        output_variables: output_count,
    };

    let mut all_tests = path_tests;
    all_tests.extend(boundary_tests);
    all_tests.extend(edge_tests);

    TestSuite {
        name: format!("Migration Equivalence Tests: {name}"),
        source_programs: vec![name.clone()],
        test_cases: all_tests,
        stats,
    }
}

// ---------------------------------------------------------------------------
// Serialization
// ---------------------------------------------------------------------------

/// Serialize a test suite to JSON.
pub fn to_json(suite: &TestSuite) -> Result<String, String> {
    serde_json::to_string_pretty(suite).map_err(|e| format!("JSON serialization error: {e}"))
}

/// Serialize a test suite to YAML-like format (structured text).
pub fn to_yaml(suite: &TestSuite) -> String {
    let mut out = String::new();
    out.push_str(&format!("# {}\n", suite.name));
    out.push_str(&format!(
        "# Generated: {} test cases\n",
        suite.stats.total_tests
    ));
    out.push_str(&format!(
        "# Source: {}\n\n",
        suite.source_programs.join(", ")
    ));

    out.push_str("stats:\n");
    out.push_str(&format!(
        "  paths_explored: {}\n",
        suite.stats.paths_explored
    ));
    out.push_str(&format!(
        "  paths_feasible: {}\n",
        suite.stats.paths_feasible
    ));
    out.push_str(&format!(
        "  path_tests: {}\n",
        suite.stats.path_tests_generated
    ));
    out.push_str(&format!(
        "  boundary_tests: {}\n",
        suite.stats.boundary_tests_generated
    ));
    out.push_str(&format!(
        "  edge_case_tests: {}\n",
        suite.stats.edge_case_tests_generated
    ));
    out.push_str(&format!(
        "  input_variables: {}\n",
        suite.stats.input_variables
    ));
    out.push_str(&format!(
        "  output_variables: {}\n\n",
        suite.stats.output_variables
    ));

    out.push_str("test_cases:\n");
    for tc in &suite.test_cases {
        out.push_str(&format!("  - id: \"{}\"\n", tc.id));
        out.push_str(&format!("    description: \"{}\"\n", tc.description));
        out.push_str(&format!("    category: {:?}\n", tc.category));
        out.push_str(&format!("    source: \"{}\"\n", tc.source_program));

        if !tc.inputs.is_empty() {
            out.push_str("    inputs:\n");
            for (k, v) in &tc.inputs {
                out.push_str(&format!("      {k}: {v}\n"));
            }
        }

        if !tc.expected_outputs.is_empty() {
            out.push_str("    expected_outputs:\n");
            for (k, v) in &tc.expected_outputs {
                out.push_str(&format!("      {k}: {v}\n"));
            }
        }

        if !tc.path_conditions.is_empty() {
            out.push_str("    path_conditions:\n");
            for pc in &tc.path_conditions {
                out.push_str(&format!("      - \"{pc}\"\n"));
            }
        }
        out.push('\n');
    }

    out
}

/// Serialize an equivalence result to JSON.
pub fn equivalence_to_json(result: &EquivalenceResult) -> Result<String, String> {
    serde_json::to_string_pretty(result).map_err(|e| format!("JSON serialization error: {e}"))
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

/// Solve path conditions to get concrete values for input variables.
fn solve_for_inputs(
    solver: &SymbolicSolver,
    path_condition: &PathCondition,
    input_vars: &HashSet<String>,
) -> BTreeMap<String, TestValue> {
    let mut result = BTreeMap::new();

    match solver.check(path_condition) {
        CheckResult::Sat { assignments } => {
            for (name, value) in &assignments {
                if input_vars.contains(name) || !name.starts_with("_LOOP_CTR_") {
                    result.insert(name.clone(), symbolic_to_test_value(value));
                }
            }
        }
        _ => {}
    }

    // For input variables not in the model (unconstrained), use a default.
    for var in input_vars {
        result.entry(var.clone()).or_insert(TestValue::Int(0));
    }

    result
}

/// Collect output variable values from an execution state.
fn collect_outputs(
    state: &ExecutionState,
    output_vars: &HashSet<String>,
) -> BTreeMap<String, TestValue> {
    let mut result = BTreeMap::new();

    for var in output_vars {
        if let Some(value) = state.get(var) {
            let tv = symbolic_to_test_value(value);
            if tv != TestValue::Unknown {
                result.insert(var.clone(), tv);
            }
        }
    }

    result
}

/// Convert a SymbolicValue to a TestValue.
fn symbolic_to_test_value(value: &SymbolicValue) -> TestValue {
    match value {
        SymbolicValue::Concrete(n) => TestValue::Int(*n),
        SymbolicValue::ConcreteBool(b) => TestValue::Bool(*b),
        SymbolicValue::ConcreteStr(s) => TestValue::Str(s.clone()),
        _ => TestValue::Unknown,
    }
}

/// Check if a path condition is compatible with a set of concrete inputs.
fn is_path_compatible(
    solver: &SymbolicSolver,
    path_condition: &PathCondition,
    inputs: &BTreeMap<String, TestValue>,
) -> bool {
    let mut pc = path_condition.clone();

    for (name, value) in inputs {
        let concrete = match value {
            TestValue::Int(n) => SymbolicValue::int(*n),
            TestValue::Bool(b) => SymbolicValue::bool(*b),
            _ => continue,
        };
        let eq_constraint = SymbolicValue::sym_int(name).eq(concrete);
        pc.add(crate::path::Constraint {
            condition: eq_constraint,
            source: open_mainframe_lang_core::Span::default(),
            description: None,
        });
    }

    match solver.check(&pc) {
        CheckResult::Sat { .. } => true,
        _ => false,
    }
}

/// A decision boundary extracted from a branch condition.
#[derive(Debug, Clone)]
struct Boundary {
    variable: String,
    threshold: i64,
    op: ExprOp,
}

impl Boundary {
    fn op_str(&self) -> &'static str {
        match self.op {
            ExprOp::Gt => ">",
            ExprOp::Ge => ">=",
            ExprOp::Lt => "<",
            ExprOp::Le => "<=",
            ExprOp::Eq => "=",
            ExprOp::Ne => "<>",
            _ => "?",
        }
    }
}

/// Recursively extract comparison boundaries from a symbolic condition.
fn extract_boundaries(condition: &SymbolicValue, out: &mut Vec<Boundary>) {
    match condition {
        SymbolicValue::Expression { op, args } => {
            match op {
                ExprOp::Gt | ExprOp::Ge | ExprOp::Lt | ExprOp::Le | ExprOp::Eq | ExprOp::Ne => {
                    // Look for patterns like VAR op CONST or CONST op VAR.
                    if let (Some(var_name), Some(threshold)) =
                        (extract_var_name(&args[0]), extract_concrete(&args[1]))
                    {
                        out.push(Boundary {
                            variable: var_name,
                            threshold,
                            op: op.clone(),
                        });
                    } else if let (Some(threshold), Some(var_name)) =
                        (extract_concrete(&args[0]), extract_var_name(&args[1]))
                    {
                        // Flip the operator for CONST op VAR.
                        let flipped = match op {
                            ExprOp::Gt => ExprOp::Lt,
                            ExprOp::Ge => ExprOp::Le,
                            ExprOp::Lt => ExprOp::Gt,
                            ExprOp::Le => ExprOp::Ge,
                            other => other.clone(),
                        };
                        out.push(Boundary {
                            variable: var_name,
                            threshold,
                            op: flipped,
                        });
                    }
                }
                ExprOp::And | ExprOp::Or | ExprOp::Not => {
                    for arg in args {
                        extract_boundaries(arg, out);
                    }
                }
                _ => {}
            }
        }
        _ => {}
    }
}

/// Extract a variable name from a symbolic value (if it's a simple variable reference).
fn extract_var_name(value: &SymbolicValue) -> Option<String> {
    match value {
        SymbolicValue::Symbolic { name, .. } => Some(name.clone()),
        _ => None,
    }
}

/// Extract a concrete integer from a symbolic value.
fn extract_concrete(value: &SymbolicValue) -> Option<i64> {
    value.as_concrete_int()
}

/// Format a symbolic condition as a human-readable string.
fn format_condition(value: &SymbolicValue) -> String {
    match value {
        SymbolicValue::Concrete(n) => format!("{n}"),
        SymbolicValue::ConcreteBool(b) => format!("{b}"),
        SymbolicValue::ConcreteStr(s) => format!("\"{s}\""),
        SymbolicValue::Symbolic { name, .. } => name.clone(),
        SymbolicValue::Expression { op, args } => {
            match op {
                ExprOp::Not if args.len() == 1 => {
                    format!("NOT({})", format_condition(&args[0]))
                }
                ExprOp::And if args.len() == 2 => {
                    format!(
                        "{} AND {}",
                        format_condition(&args[0]),
                        format_condition(&args[1])
                    )
                }
                ExprOp::Or if args.len() == 2 => {
                    format!(
                        "{} OR {}",
                        format_condition(&args[0]),
                        format_condition(&args[1])
                    )
                }
                _ if args.len() == 2 => {
                    let op_str = match op {
                        ExprOp::Eq => "=",
                        ExprOp::Ne => "<>",
                        ExprOp::Gt => ">",
                        ExprOp::Ge => ">=",
                        ExprOp::Lt => "<",
                        ExprOp::Le => "<=",
                        ExprOp::Add => "+",
                        ExprOp::Sub => "-",
                        ExprOp::Mul => "*",
                        ExprOp::Div => "/",
                        ExprOp::Mod => "MOD",
                        _ => "?",
                    };
                    format!(
                        "{} {} {}",
                        format_condition(&args[0]),
                        op_str,
                        format_condition(&args[1])
                    )
                }
                _ => format!("{value}"),
            }
        }
        SymbolicValue::Unknown => "?".to_string(),
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interpreter::{FlatProgramBuilder, FlatStatement, InterpreterConfig, SymbolicInterpreter};
    use crate::state::ExecutionState;

    fn span() -> open_mainframe_lang_core::Span {
        open_mainframe_lang_core::Span::default()
    }

    #[test]
    fn classify_input_vs_output() {
        let mut state = ExecutionState::new();
        state.set("INPUT_BALANCE", SymbolicValue::sym_int("INPUT_BALANCE"));
        state.set("TIER", SymbolicValue::str("PREMIUM"));
        state.set("RATE", SymbolicValue::int(45));

        let stmts = vec![
            FlatStatement::Assign {
                target: "TIER".into(),
                value: SymbolicValue::str("PREMIUM"),
                span: span(),
            },
            FlatStatement::Assign {
                target: "RATE".into(),
                value: SymbolicValue::int(45),
                span: span(),
            },
        ];

        let (inputs, outputs) = classify_variables(&state, &stmts);
        assert!(inputs.contains("INPUT_BALANCE"));
        assert!(outputs.contains("TIER"));
        assert!(outputs.contains("RATE"));
        assert!(!inputs.contains("TIER"));
    }

    #[test]
    fn generate_path_tests_simple() {
        // IF FLAG THEN X = 1 ELSE X = 2; STOP
        let stmts = vec![
            FlatStatement::Branch {
                condition: SymbolicValue::sym_bool("FLAG"),
                then_pc: 1,
                else_pc: 3,
                span: span(),
            },
            FlatStatement::Assign {
                target: "X".into(),
                value: SymbolicValue::int(1),
                span: span(),
            },
            FlatStatement::Branch {
                condition: SymbolicValue::bool(true),
                then_pc: 4,
                else_pc: 4,
                span: span(),
            },
            FlatStatement::Assign {
                target: "X".into(),
                value: SymbolicValue::int(2),
                span: span(),
            },
            FlatStatement::Stop,
        ];

        let interp = SymbolicInterpreter::new(stmts.clone(), InterpreterConfig::default());
        let result = interp.execute(ExecutionState::new());

        let tests = generate_path_tests(&result, &stmts, "TEST-PROG");
        assert_eq!(tests.len(), 2);
        assert_eq!(tests[0].category, TestCategory::PathCoverage);
        assert!(!tests[0].source_program.is_empty());
    }

    #[test]
    fn generate_boundary_tests_simple() {
        // IF BALANCE > 10000 THEN ... ELSE ...
        let stmts = vec![
            FlatStatement::Branch {
                condition: SymbolicValue::Expression {
                    op: ExprOp::Gt,
                    args: vec![
                        SymbolicValue::sym_int("BALANCE"),
                        SymbolicValue::int(10000),
                    ],
                },
                then_pc: 1,
                else_pc: 2,
                span: span(),
            },
            FlatStatement::Stop,
            FlatStatement::Stop,
        ];

        let tests = generate_boundary_tests(&stmts, "TEST-PROG");
        assert_eq!(tests.len(), 3); // below, at, above
        assert_eq!(tests[0].category, TestCategory::BoundaryValue);

        // Check values: 9999, 10000, 10001
        let vals: Vec<i64> = tests
            .iter()
            .filter_map(|t| match t.inputs.get("BALANCE")? {
                TestValue::Int(n) => Some(*n),
                _ => None,
            })
            .collect();
        assert_eq!(vals, vec![9999, 10000, 10001]);
    }

    #[test]
    fn format_condition_display() {
        let cond = SymbolicValue::Expression {
            op: ExprOp::Gt,
            args: vec![
                SymbolicValue::sym_int("BALANCE"),
                SymbolicValue::int(10000),
            ],
        };
        assert_eq!(format_condition(&cond), "BALANCE > 10000");
    }

    #[test]
    fn test_value_serialization() {
        let tv = TestValue::Int(42);
        let json = serde_json::to_string(&tv).unwrap();
        assert_eq!(json, "42");

        let tv2 = TestValue::Str("PREMIUM".into());
        let json2 = serde_json::to_string(&tv2).unwrap();
        assert_eq!(json2, "\"PREMIUM\"");
    }

    #[test]
    fn full_suite_generation() {
        let stmts = vec![
            FlatStatement::Branch {
                condition: SymbolicValue::Expression {
                    op: ExprOp::Gt,
                    args: vec![
                        SymbolicValue::sym_int("BALANCE"),
                        SymbolicValue::int(10000),
                    ],
                },
                then_pc: 1,
                else_pc: 3,
                span: span(),
            },
            FlatStatement::Assign {
                target: "TIER".into(),
                value: SymbolicValue::str("PREMIUM"),
                span: span(),
            },
            FlatStatement::Branch {
                condition: SymbolicValue::bool(true),
                then_pc: 4,
                else_pc: 4,
                span: span(),
            },
            FlatStatement::Assign {
                target: "TIER".into(),
                value: SymbolicValue::str("STANDARD"),
                span: span(),
            },
            FlatStatement::Stop,
        ];

        let interp = SymbolicInterpreter::new(stmts.clone(), InterpreterConfig::default());
        let result = interp.execute(ExecutionState::new());

        let lowering = crate::lowering::LoweringResult {
            program_name: "CARDDEMO".into(),
            statements: vec![],
            paragraph_map: Default::default(),
            stats: Default::default(),
            errors: vec![],
        };

        let suite = generate_test_suite(&lowering, &result, &stmts);
        assert!(suite.stats.total_tests > 0);
        assert!(suite.stats.path_tests_generated >= 2);
        assert!(suite.stats.boundary_tests_generated >= 3);

        // Verify YAML output is non-empty.
        let yaml = to_yaml(&suite);
        assert!(yaml.contains("CARDDEMO"));
        assert!(yaml.contains("test_cases:"));

        // Verify JSON output parses.
        let json = to_json(&suite).unwrap();
        let _: serde_json::Value = serde_json::from_str(&json).unwrap();
    }

    #[test]
    fn equivalence_identical_programs() {
        let stmts = vec![
            FlatStatement::Assign {
                target: "X".into(),
                value: SymbolicValue::int(42),
                span: span(),
            },
            FlatStatement::Stop,
        ];

        let interp = SymbolicInterpreter::new(stmts.clone(), InterpreterConfig::default());
        let result = interp.execute(ExecutionState::new());

        let equiv = generate_equivalence_tests(
            &result, &stmts, "PROG-A",
            &result, &stmts, "PROG-B",
        );

        assert!(equiv.equivalent);
        assert!(equiv.counterexamples.is_empty());
    }

    #[test]
    fn equivalence_different_programs() {
        let stmts_a = vec![
            FlatStatement::Assign {
                target: "X".into(),
                value: SymbolicValue::int(42),
                span: span(),
            },
            FlatStatement::Stop,
        ];
        let stmts_b = vec![
            FlatStatement::Assign {
                target: "X".into(),
                value: SymbolicValue::int(99),
                span: span(),
            },
            FlatStatement::Stop,
        ];

        let interp_a = SymbolicInterpreter::new(stmts_a.clone(), InterpreterConfig::default());
        let result_a = interp_a.execute(ExecutionState::new());
        let interp_b = SymbolicInterpreter::new(stmts_b.clone(), InterpreterConfig::default());
        let result_b = interp_b.execute(ExecutionState::new());

        let equiv = generate_equivalence_tests(
            &result_a, &stmts_a, "PROG-A",
            &result_b, &stmts_b, "PROG-B",
        );

        assert!(!equiv.equivalent);
        assert!(!equiv.counterexamples.is_empty());
        assert!(equiv.counterexamples[0].differing_variables.contains(&"X".to_string()));
    }
}

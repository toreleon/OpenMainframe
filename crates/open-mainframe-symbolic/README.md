# open-mainframe-symbolic

`open-mainframe-symbolic` is a symbolic execution and formal verification engine for COBOL programs in the OpenMainframe workspace. It uses the Z3 SMT solver to perform automated path exploration, formal property verification, test suite generation, counterexample discovery, and semantic equivalence checking.

## Purpose

Traditional testing of mainframe legacy code relies on concrete test inputs which may miss edge-case branches and boundary conditions. This crate lowers COBOL programs into a symbolic control-flow representation, tracking symbolic variables and branch conditions along all feasible paths. It enables:
1. Proving program invariants, preconditions, and postconditions.
2. Generating concrete inputs that achieve high branch and statement coverage.
3. Formally proving equivalence between refactored or migrated COBOL programs.
4. Generating golden-master test suites via differential testing with GnuCOBOL.

## Capabilities

- **COBOL AST Lowering**: Translates parsed `open_mainframe_cobol::ast::Program` ASTs into a flattened symbolic control flow graph (`FlatStatement` / `FlatProgramBuilder`) covering data division structures, arithmetic expressions, condition evaluations, branching, `PERFORM` loops (varying, times, through), `GOTO`, string manipulation (`STRING`, `UNSTRING`), and basic file/CICS statements.
- **Symbolic Interpretation**: Explores symbolic execution states (`ExecutionState`) using configurable strategies:
  - Depth-First Search (`ExplorationStrategy::Dfs`)
  - Breadth-First Search (`ExplorationStrategy::Bfs`)
  - Coverage-Guided Exploration (`ExplorationStrategy::CoverageGuided`)
- **Z3 SMT Solver Integration**: Encodes symbolic expressions (`SymbolicValue`, `ExprOp`) and path constraints (`Constraint`, `PathCondition`) into Z3 SMT formulas to check path feasibility (`CheckResult::Sat`, `CheckResult::Unsat`, `CheckResult::Unknown`) and extract satisfying concrete assignments.
- **Formal Verification & Property Checking**: Validates program assertions, invariants, preconditions, and postconditions defined in external YAML files or embedded inline annotations (`*> @requires`, `*> @ensures`, `*> @invariant`, `*> @assert`).
- **Counterexample Synthesis**: Produces precise, concrete variable bindings for any failing execution path violating a declared specification.
- **Automated Test Generation**: Generates structured test suites (`TestSuite`, `TestCase`) partitioned into `BranchCoverage`, `BoundaryValue`, `ErrorPath`, and `PropertyVerification` categories.
- **Differential Testing**: Interfaces with external GnuCOBOL (`cobc`) compilers via `gnucobol.rs` to generate and run golden-master regression suites.
- **Command-Line Interface (`openmf-symbolic`)**: Multi-command CLI tool for verification, batch analysis, test generation, and equivalence verification.

## Architecture

```text
       COBOL Source Code (*.cbl)
                   │
                   ▼
       open_mainframe_cobol (parser)
                   │
                   ▼
             lowering.rs
       (AST → FlatProgram CFG)
                   │
                   ▼
            interpreter.rs
       (SymbolicInterpreter)
        │                  ▲
        │ explore          │ backtrack / branch
        ▼                  │
    state.rs ◄──────► solver.rs ◄──────► Z3 SMT Solver
 (ExecutionState)   (SymbolicSolver)
        │
   ┌────┴───────────────────────────┐
   ▼                                ▼
model_checker.rs                testgen.rs
(Property Verification)         (Test Suite Generation)
   │                                │
   ▼                                ▼
VerificationResult /            TestSuite /
PropertyViolation               gnucobol.rs (differential testing)
```

### Module Structure

| Module | Description |
|---|---|
| `lib` | Library root re-exporting core interpreter, solver, model checker, and testgen types. |
| `lowering` | Lowers parsed COBOL ASTs into symbolic execution CFGs (`lower_cobol_file`, `lower_cobol_source`, `FlatStatement`). |
| `interpreter` | Core symbolic engine (`SymbolicInterpreter`, `InterpreterConfig`, `ExplorationStrategy`, `ExecutionResult`). |
| `state` | Execution state container (`ExecutionState`, `CallFrame`) tracking symbolic variables, memory mappings, and call stacks. |
| `value` | Symbolic value representation (`SymbolicValue`) and algebraic expression operations (`ExprOp`). |
| `sort` | Typing system for symbolic values (`Sort`: Integer, Decimal, String, Boolean, BitVec, Array, Custom). |
| `path` | Path condition representation (`PathCondition`, `Constraint`, `BranchDecision`). |
| `solver` | Z3 SMT solver bridge (`SymbolicSolver`, `CheckResult`) managing Z3 contexts and constraint satisfiability. |
| `spec` | Formal property specifications (`Property`, `PropertyLocation`) and parser for YAML specs and inline COBOL annotations. |
| `model_checker` | Formal verification engine (`ModelChecker`, `VerificationResult`, `PropertyViolation`, `CoverageMetrics`). |
| `testgen` | Test case generation from symbolic constraints (`generate_test_suite`, `TestSuite`, `TestCase`, `EquivalenceResult`). |
| `gnucobol` | GnuCOBOL (`cobc`) compilation and test execution wrapper for differential verification. |
| `cli` | Hand-rolled CLI argument parser and command dispatcher for `openmf-symbolic`. |
| `main` | Executable entry point for `openmf-symbolic`. |

## Public API and Binaries

### Binary: `openmf-symbolic`

```text
openmf-symbolic <COMMAND> [OPTIONS]

Commands:
  verify       Verify a COBOL program against properties
  batch        Run verification against all .cbl files in a directory
  testgen      Generate test cases from symbolic execution
  equivalence  Check formal equivalence between two COBOL programs
  carddemo-testgen  Generate tests for a single CardDemo program
  golden-master     Run test suite through GnuCOBOL to produce golden master
  help         Display usage information
```

### Core Library Types

```rust
use open_mainframe_symbolic::{
    lower_cobol_source, InterpreterConfig, ModelChecker,
    SymbolicInterpreter, SymbolicSolver, VerificationResult,
};

// 1. Lower COBOL source to a flattened symbolic program
let lowering_result = lower_cobol_source(cobol_source, "PROGRAM-ID")?;

// 2. Configure interpreter bounds
let config = InterpreterConfig {
    max_depth: 500,
    max_paths: 100,
    max_loop_iterations: 10,
    timeout_ms: 5000,
    ..Default::default()
};

// 3. Initialize symbolic solver and interpreter
let solver = SymbolicSolver::new();
let interpreter = SymbolicInterpreter::new(config, solver);

// 4. Run verification with model checker
let model_checker = ModelChecker::new(interpreter);
let result = model_checker.verify(&lowering_result.program, &properties)?;
```

## Integration and Consumers

### Workspace Dependencies

- [`open-mainframe-cobol`](../open-mainframe-cobol/README.md) — COBOL lexer, parser, and AST structures.
- [`open-mainframe-lang-core`](../open-mainframe-lang-core/README.md) — Diagnostic spans and shared error infrastructure.

### External Dependencies

- `z3` / `z3-sys` — Native bindings to the Z3 SMT solver.
- `rust_decimal` — Exact decimal arithmetic modeling.
- `serde` / `serde_json` — Test suite and report serialization.

### Known Consumers

- `openmf-symbolic` (`src/main.rs`) — The standalone verification binary.
- Differential testing and migration test-generation pipelines.

## Examples

### Verifying a Program via CLI

```bash
cargo run --release -p open-mainframe-symbolic -- verify \
    -p programs/CALC.cbl \
    -I copybooks \
    --properties specs/calc_props.yaml \
    --max-paths 50 \
    -o text
```

### Generating Test Cases from Path Constraints

```bash
cargo run --release -p open-mainframe-symbolic -- testgen \
    -p programs/ACCOUNT.cbl \
    -I copybooks \
    -o tests/generated/
```

### Checking Equivalence Between Two Programs

```bash
cargo run --release -p open-mainframe-symbolic -- equivalence \
    --old legacy/ACCT01.cbl \
    --new modern/ACCT01.cbl \
    -I copybooks
```

## Testing

Run the test suite across all modules:

```bash
cargo test -p open-mainframe-symbolic
```

Test coverage includes:
- `interpreter.rs`: Path exploration, loop termination, and branching tests.
- `solver.rs`: Z3 constraint solving and model generation tests.
- `lowering.rs`: Statement flattening and loop exit fixup tests.
- `model_checker.rs`: Assertion violation detection and coverage metrics.
- `testgen.rs`: Test suite synthesis and categorization.
- `spec.rs`: Parsing of YAML specifications and `*> @requires` annotations.
- `gnucobol.rs`: GnuCOBOL command synthesis and test wrapper execution.

## Limitations

- **Native Dependency**: Requires the Z3 theorem prover library (`libz3`) installed on the host operating system.
- **Path Explosion**: Programs with unbounded loops or extensive branch permutations require explicit bounds (`--max-paths`, `--max-depth`, `--max-loop-iterations`).
- **GnuCOBOL Availability**: Differential testing and golden-master generation require `cobc` to be installed and available in the environment's `PATH`.

## Related Documentation

- [Crate Map](../../docs/architecture/crate-map.md)
- [COBOL Compiler (`open-mainframe-cobol`)](../open-mainframe-cobol/README.md)
- [Language Core (`open-mainframe-lang-core`)](../open-mainframe-lang-core/README.md)
- [Runtime Subsystem (`open-mainframe-runtime`)](../open-mainframe-runtime/README.md)
- [Code Assessment (`open-mainframe-assess`)](../open-mainframe-assess/README.md)

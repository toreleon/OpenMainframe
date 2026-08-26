# open-mainframe-precompilers

z/OS COBOL Precompilers — source-to-source transformation for DB2 (`EXEC SQL`) and CICS (`EXEC CICS`) statements for the OpenMainframe project.

## Purpose

Mainframe COBOL applications frequently embed subsystem commands (`EXEC SQL ... END-EXEC` for DB2 database operations and `EXEC CICS ... END-EXEC` for CICS transaction processing) that cannot be compiled directly by standard COBOL compilers. The `open-mainframe-precompilers` crate implements source-to-source transformation pipelines that parse embedded blocks, replace them with standard COBOL `CALL` statements and parameter structures, generate required linkage data structures (DFHEIBLK, DFHCOMMAREA, SQLCA), extract Database Request Modules (DBRMs), and process BMS map copybooks.

## Capabilities

- **DB2 Precompilation (`precompile_db2`)**:
  - Scans and extracts `EXEC SQL` blocks using the DB2 scanner from `open-mainframe-db2`.
  - Parses host variables (`:VAR-NAME`) and null indicator variables (`:VAR-NAME :IND-VAR`).
  - Converts COBOL `PIC` clauses to SQL data types (`pic_to_sql_type` for `CHAR`, `VARCHAR`, `INTEGER`, `SMALLINT`, `DECIMAL`).
  - Replaces SQL statements (`SELECT`, `INSERT`, `UPDATE`, `DELETE`, `DECLARE/OPEN/FETCH/CLOSE CURSOR`, `PREPARE`, `EXECUTE`) with `CALL 'DSNHLI'` statements.
  - Generates `SQLCA` copybook definitions and conditional `WHENEVER SQLERROR GO TO` error handling code.
  - Generates structured and binary Database Request Modules (`Dbrm`, `DbrmStatement`, `DbrmHostVar`, `serialize_dbrm`) for downstream BIND packaging.
- **CICS Precompilation (`precompile_cics`)**:
  - Scans and extracts `EXEC CICS` blocks across standard COBOL format lines (columns 8–72).
  - Classifies 30+ CICS verbs (`LINK`, `XCTL`, `RETURN`, `READ`, `WRITE`, `SEND MAP`, `RECEIVE MAP`, `ASKTIME`, `FORMATTIME`, etc.) and associated options.
  - Replaces CICS commands with `CALL 'DFHEI1'` invocations passing generated command parameter blocks and `DFHEIBLK`.
  - Generates `DFHEIBLK` (EXEC Interface Block) and `DFHCOMMAREA` data structures in the `LINKAGE SECTION`.
  - Automatically updates the `PROCEDURE DIVISION USING` header.
  - Generates BMS symbolic copybooks (`BmsSymbolicMap`, `BmsField`) for terminal map fields.
- **Mode Detection (`detect_mode`)**:
  - Inspects COBOL source text to detect subsystem requirements: `PrecompileMode::CicsOnly`, `PrecompileMode::Db2Only`, `PrecompileMode::Integrated` (both CICS and DB2), or `PrecompileMode::Standard` (neither).

## Architecture

```
         COBOL Source with Embedded Statements
        (EXEC SQL ... END-EXEC / EXEC CICS ... END-EXEC)
                           │
                           ▼
                 ┌───────────────────┐
                 │    detect_mode    │
                 └─────────┬─────────┘
                           │
         ┌─────────────────┼─────────────────┐
         │ (DB2 statements)│                 │ (CICS commands)
         ▼                 │                 ▼
  ┌──────────────┐         │          ┌──────────────┐
  │precompile_db2│         │          │precompile_cics│
  └──────┬───────┘         │          └──────┬───────┘
         │                 │                 │
         ├─────────────────┼─────────────────┤
         ▼                 ▼                 ▼
  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐
  │ CALL'DSNHLI' │  │     DBRM     │  │ CALL 'DFHEI1'│
  │ + SQLCA area │  │ (Binary/JSON)│  │ + DFHEIBLK   │
  └──────────────┘  └──────────────┘  └──────────────┘
```

### Module Structure

| Module | Purpose |
|--------|---------|
| `db2` | DB2 precompiler: statement parsing (`parse_exec_sql`), transformation (`transform_sql_blocks`), SQLCA generation (`generate_sqlca_include`), DBRM generation (`generate_dbrm`, `serialize_dbrm`), and full precompilation (`precompile_db2`) |
| `cics` | CICS precompiler: command parsing (`parse_exec_cics`), transformation (`transform_cics_blocks`), DFHEIBLK/DFHCOMMAREA generation (`generate_dfheiblk`, `generate_dfhcommarea`), BMS symbolic copybook mapping (`BmsSymbolicMap`), mode detection (`detect_mode`), and full precompilation (`precompile_cics`) |

## Public API

### Primary Types and Functions

- `detect_mode(source: &str) -> PrecompileMode`: Detects whether source code contains CICS, DB2, both, or standard COBOL.
- `precompile_db2(source: &str, program_name: &str) -> Result<Db2PrecompileResult, Db2PrecompileError>`: Executes full DB2 precompilation, returning transformed source, extracted SQL blocks, host variable metadata, and generated `Dbrm`.
- `precompile_cics(source: &str) -> Result<CicsPrecompileResult, CicsPrecompileError>`: Executes full CICS precompilation, returning transformed source, extracted CICS blocks, and transformed CALL specifications.
- `generate_dbrm(program_name: &str, blocks: &[ExecSqlBlock]) -> Dbrm`: Aggregates parsed SQL blocks into a DBRM structure.
- `serialize_dbrm(dbrm: &Dbrm) -> Vec<u8>`: Serializes a DBRM into binary format.
- `generate_dfheiblk() -> String`: Produces the standard COBOL `DFHEIBLK` copybook definition.
- `generate_dfhcommarea(length: u32) -> String`: Produces the standard `DFHCOMMAREA` structure for a given length.
- `generate_sqlca_include() -> String`: Produces the standard `SQLCA` structure.
- `BmsSymbolicMap`: Generates COBOL symbolic map copybooks from BMS field definitions.

## Integration

### Internal Workspace Dependencies

- `open-mainframe-db2`: Used for SQL block scanning and SQLCA structures.
- `open-mainframe-cics`: Used for CICS command tokenization and preprocessing structures.
- `thiserror`: Error derive macros.
- `miette`: Diagnostic error reporting.

### Workspace Consumers

- `open-mainframe`: Uses precompilation before program compilation in the CLI and CICS runner.
- `open-mainframe-assess`: Assesses migration readiness for precompiler-targeted statements.
- Root workspace member in `Cargo.toml`.

## Examples

### Precompiling COBOL with DB2 SQL

```rust
use open_mainframe_precompilers::db2::precompile_db2;

fn main() {
    let source = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DB2PROG.
       PROCEDURE DIVISION.
           EXEC SQL
               SELECT CUST_NAME INTO :WS-NAME
               FROM CUSTOMERS WHERE CUST_ID = :WS-ID
           END-EXEC.
           STOP RUN.
    "#;

    let result = precompile_db2(source, "DB2PROG").expect("precompilation failed");
    println!("Transformed source:\n{}", result.transformed_source);
    println!("Statements extracted: {}", result.dbrm.statements.len());
    assert!(result.transformed_source.contains("CALL 'DSNHLI'"));
}
```

### Precompiling COBOL with EXEC CICS

```rust
use open_mainframe_precompilers::cics::precompile_cics;

fn main() {
    let source = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CICSPROG.
       PROCEDURE DIVISION.
           EXEC CICS
               SEND MAP('INVMAP') MAPSET('INVSET') ERASE
           END-EXEC.
           EXEC CICS RETURN END-EXEC.
    "#;

    let result = precompile_cics(source).expect("precompilation failed");
    println!("Transformed source:\n{}", result.transformed_source);
    assert!(result.transformed_source.contains("CALL 'DFHEI1'"));
    assert_eq!(result.blocks.len(), 2);
}
```

## Testing

Run the test suite for this crate:

```sh
cargo test -p open-mainframe-precompilers
```

The crate contains 37 unit tests verifying:
- `db2`: Single/multi-line `EXEC SQL` parsing, host variable extraction with indicators, `pic_to_sql_type` conversions (`CHAR`, `VARCHAR`, `INTEGER`, `DECIMAL`), `SELECT`/`INSERT`/cursor/dynamic SQL transformations, `SQLCA` generation, `WHENEVER` handling, DBRM serialization round-trips, and full source-to-source pipelines.
- `cics`: Single/multi-line `EXEC CICS` parsing, command code mappings, `DFHEIBLK`/`DFHCOMMAREA` copybook generation, `PROCEDURE DIVISION USING` insertion, BMS symbolic map field generation, `READ FILE`/`SEND MAP` transformations, and full source-to-source pipelines.
- `mode_detection`: Accurate classification of `Db2Only`, `CicsOnly`, `Integrated`, and `Standard` source files.

## Limitations

- **Heuristic Type Mapping**: Host variable SQL type inference is based on regex pattern matching of `PIC` clauses (`pic_to_sql_type`) rather than a full semantic symbol table from COBOL AST lowering.
- **DBRM Format**: The generated DBRM uses custom JSON/binary serialization rather than exact IBM MVS 80-byte fixed-blocked DBRM partitioned dataset member binary layouts.
- **Fixed Format Assumptions**: Precompiler scanners expect standard COBOL line layouts (area A/B between columns 8 and 72); free-format COBOL source files may require pre-formatting.

## Related Documentation

- [OpenMainframe Crate Map](../../docs/architecture/crate-map.md)
- [open-mainframe-cobol](../open-mainframe-cobol/README.md)
- [open-mainframe-db2](../open-mainframe-db2/README.md)
- [open-mainframe-cics](../open-mainframe-cics/README.md)
